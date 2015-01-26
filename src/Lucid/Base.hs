{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Base types and combinators.

module Lucid.Base
  (-- * Rendering
   renderText
  ,renderBS
  ,renderTextT
  ,renderBST
  ,renderToFile
   -- * Running
  ,execHtmlT
  ,evalHtmlT
  ,runHtmlT
  -- * Combinators
  ,makeElement
  ,makeElementNoEnd
  ,makeXmlElementNoEnd
  ,makeAttribute
   -- * Types
  ,Html
  ,HtmlT
  ,Attribute
   -- * Classes
  ,Term(..)
  ,TermRaw(..)
  ,ToHtml(..)
  ,With(..))
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Prim as BP
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as L
import           Data.Char                    (chr,ord)
import           Data.Functor.Identity
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as M
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.Encoding      as LT
import           Data.Word                    (Word8)

--------------------------------------------------------------------------------
-- Types

-- | A simple attribute.
newtype Attribute = Attribute (Text,Text)
  deriving (Show,Eq)

-- | Simple HTML builder type. Defined in terms of 'HtmlT'. Check out
-- that type for instance information.
--
-- Simple use-cases will just use this type. But if you want to
-- transformer over Reader or something, you can go and use 'HtmlT'.
type Html = HtmlT Identity

-- | A monad transformer that generates HTML. Use the simpler 'Html'
-- type if you don't want to transform over some other monad.
newtype HtmlT m a =
  HtmlT {runHtmlT :: m (HashMap Text Text -> Builder -> Builder,a)
         -- ^ This is the low-level way to run the HTML transformer,
         -- finally returning an element builder and a value. You can
         -- pass 'mempty' for both arguments for a top-level call. See
         -- 'evalHtmlT' and 'execHtmlT' for easier to use functions.
         }

-- | Monoid is right-associative, a la the 'Builder' in it.
instance (a ~ (),Monad m) => Monoid (HtmlT m a) where
  mempty = HtmlT (return (\_ _ -> mempty,mempty))
  mappend (HtmlT get_f_a) (HtmlT get_g_b) =
    HtmlT (do ~(f,a) <- get_f_a
              ~(g,b) <- get_g_b
              return (\attr inner ->
                        f attr inner <>
                        g attr inner
                     ,a <> b))

-- | Based on the monad instance.
instance Monad m => Applicative (HtmlT m) where
  pure = return
  (<*>) = ap

-- | Just re-uses Monad.
instance Monad m => Functor (HtmlT m) where
  fmap = liftM

-- | Basically acts like Writer.
instance Monad m => Monad (HtmlT m) where
  return a = HtmlT (return (\_ _ -> mempty,a))
  HtmlT get_g_a >>= f =
    HtmlT (do ~(g,a) <- get_g_a
              let HtmlT get_f'_b = f a
              ~(f',b) <- get_f'_b
              return (\attr inner ->
                        g attr inner <>
                        f' attr inner
                     ,b))

-- | Used for 'lift'.
instance MonadTrans HtmlT where
  lift m =
    HtmlT (do a <- m
              return (\_ _ -> mempty,a))

-- | If you want to use IO in your HTML generation.
instance MonadIO m => MonadIO (HtmlT m) where
  liftIO = lift . liftIO

-- | We pack it via string. Could possibly encode straight into a
-- builder. That might be faster.
instance (Monad m,a ~ ()) => IsString (HtmlT m a) where
  fromString m' =
    HtmlT (return (\_ _ -> encode (T.pack m'),()))

-- | Just calls 'renderText'.
instance (m ~ Identity) => Show (HtmlT m a) where
  show = LT.unpack . renderText

-- | Can be converted to HTML.
class ToHtml a where
  toHtml :: Monad m => a -> HtmlT m ()
  toHtmlRaw :: Monad m => a -> HtmlT m ()

instance ToHtml String where
  toHtml = fromString
  toHtmlRaw m = HtmlT (return ((\_ _ -> fromString m),()))

instance ToHtml Text where
  toHtml m = HtmlT (return ((\_ _ -> encode m),()))
  toHtmlRaw m = HtmlT (return ((\_ _ -> T.encodeUtf8Builder m),()))

instance ToHtml LT.Text where
  toHtml m = HtmlT (return ((\_ _ -> encodeLazy m),()))
  toHtmlRaw m = HtmlT (return ((\_ _ -> LT.encodeUtf8Builder m),()))

-- | Used to construct HTML terms.
--
-- Simplest use: p_ = term "p" yields 'Lucid.Html5.p_'.
--
-- Very overloaded for three cases:
--
-- * The first case is the basic @arg@ of @[(Text,Text)]@ which will
--   return a function that wants children.
-- * The second is an @arg@ which is @HtmlT m ()@, in which case the
--   term accepts no attributes and just the children are used for the
--   element.
-- * Finally, this is also used for overloaded attributes, like
--   `Lucid.Html5.style_` or `Lucid.Html5.title_`. If a return type of @(Text,Text)@ is inferred
--   then an attribute will be made.
--
-- The instances look intimidating but actually the constraints make
-- it very general so that type inference works well even in the
-- presence of things like @OverloadedLists@ and such.
class Term arg result | result -> arg where
  -- | Used for constructing elements e.g. @term "p"@ yields 'Lucid.Html5.p_'.
  term :: Text   -- ^ Name of the element or attribute.
       -> arg    -- ^ Either an attribute list or children.
       -> result -- ^ Result: either an element or an attribute.
  term = flip termWith []
  -- | Use this if you want to make an element which inserts some
  -- pre-prepared attributes into the element.
  termWith :: Text          -- ^ Name.
           -> [Attribute]   -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance (Monad m,f ~ HtmlT m a) => Term [Attribute] (f -> HtmlT m a) where
  termWith name f = with (makeElement name) . (<> f)

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m) => Term (HtmlT m a) (HtmlT m a) where
  termWith name f = with (makeElement name) f

-- | Some terms (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance Term Text Attribute where
  termWith key _ value = makeAttribute key value

-- | Same as the 'Term' class, but will not HTML escape its
-- children. Useful for elements like 'Lucid.Html5.style_' or
-- 'Lucid.Html5.script_'.
class TermRaw arg result | result -> arg where
  -- | Used for constructing elements e.g. @termRaw "p"@ yields 'Lucid.Html5.p_'.
  termRaw :: Text   -- ^ Name of the element or attribute.
       -> arg    -- ^ Either an attribute list or children.
       -> result -- ^ Result: either an element or an attribute.
  termRaw = flip termRawWith []
  -- | Use this if you want to make an element which inserts some
  -- pre-prepared attributes into the element.
  termRawWith :: Text          -- ^ Name.
           -> [Attribute]   -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.

-- | Given attributes, expect more child input.
instance (Monad m,ToHtml f, a ~ ()) => TermRaw [Attribute] (f -> HtmlT m a) where
  termRawWith name f attrs = with (makeElement name) (attrs <> f) . toHtmlRaw

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,a ~ ()) => TermRaw Text (HtmlT m a) where
  termRawWith name f = with (makeElement name) f . toHtmlRaw

-- | Some termRaws (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance TermRaw Text Attribute where
  termRawWith key _ value = makeAttribute key value

-- | With an element use these attributes. An overloaded way of adding
-- attributes either to an element accepting attributes-and-children
-- or one that just accepts attributes. See the two instances.
class With a  where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html a@ or @Html a -> Html a@.
       -> [Attribute]
       -> a

-- | For the contentless elements: 'Lucid.Html5.br_'
instance (Monad m) => With (HtmlT m a) where
  with f =
    \attr ->
      HtmlT (do ~(f',a) <- runHtmlT f
                return (\attr' m' ->
                          f' (unionArgs (M.fromListWith (<>) (map toPair attr)) attr') m'
                       ,a))
    where toPair (Attribute x) = x

-- | For the contentful elements: 'Lucid.Html5.div_'
instance (Monad m) => With (HtmlT m a -> HtmlT m a) where
  with f =
    \attr inner ->
      HtmlT (do ~(f',a) <- runHtmlT (f inner)
                return ((\attr' m' ->
                           f' (unionArgs (M.fromListWith (<>) (map toPair attr)) attr') m')
                       ,a))
    where toPair (Attribute x) = x

-- | Union two sets of arguments and append duplicate keys.
unionArgs :: HashMap Text Text -> HashMap Text Text -> HashMap Text Text
unionArgs = M.unionWith (<>)

--------------------------------------------------------------------------------
-- Running

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderToFile :: FilePath -> Html a -> IO ()
renderToFile fp = L.writeFile fp . toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderBS :: Html a -> ByteString
renderBS = toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'Text'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'toLazyByteString', and
-- 'LT.decodeUtf8'. Check the source if you're interested in the
-- lower-level behaviour.
--
renderText :: Html a -> LT.Text
renderText = LT.decodeUtf8 . toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'toLazyByteString'. Check the source if you're interested in
-- the lower-level behaviour.
--
renderBST :: Monad m => HtmlT m a -> m ByteString
renderBST = liftM toLazyByteString . execHtmlT

-- | Render the HTML to a lazy 'Text', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'toLazyByteString', and 'LT.decodeUtf8'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderTextT :: Monad m => HtmlT m a -> m LT.Text
renderTextT = liftM (LT.decodeUtf8 . toLazyByteString) . execHtmlT

--------------------------------------------------------------------------------
-- Running, transformer versions

-- | Build the HTML. Analogous to @execState@.
--
-- You might want to use this is if you want to do something with the
-- raw 'Builder'. Otherwise for simple cases you can just use
-- 'renderText' or 'renderBS'.
execHtmlT :: Monad m
          => HtmlT m a  -- ^ The HTML to generate.
          -> m Builder  -- ^ The @a@ is discarded.
execHtmlT m =
  do (f,_) <- runHtmlT m
     return (f mempty mempty)

-- | Evaluate the HTML to its return value. Analogous to @evalState@.
--
-- Use this if you want to ignore the HTML output of an action
-- completely and just get the result.
--
-- For using with the 'Html' type, you'll need 'runIdentity' e.g.
--
-- >>> runIdentity (evalHtmlT (p_ "Hello!"))
-- ()
--
evalHtmlT :: Monad m
          => HtmlT m a -- ^ HTML monad to evaluate.
          -> m a       -- ^ Ignore the HTML output and just return the value.
evalHtmlT m =
  do (_,a) <- runHtmlT m
     return a

--------------------------------------------------------------------------------
-- Combinators

-- | Make an attribute builder.
makeAttribute :: Text -- ^ Attribute name.
              -> Text -- ^ Attribute value.
              -> Attribute
makeAttribute x y = Attribute (x,y)

-- | Make an HTML builder.
makeElement :: Monad m
            => Text       -- ^ Name.
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m a -- ^ A parent element.
makeElement name =
  \m' ->
    HtmlT (do ~(f,a) <- runHtmlT m'
              return (\attr m -> s "<" <> T.encodeUtf8Builder name
                              <> foldlMapWithKey buildAttr attr <> s ">"
                              <> m <> f mempty mempty
                              <> s "</" <> T.encodeUtf8Builder name <> s ">",
                      a))

-- | Make an HTML builder for elements which have no ending tag.
makeElementNoEnd :: Monad m
                 => Text       -- ^ Name.
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name =
  HtmlT (return (\attr _ -> s "<" <> T.encodeUtf8Builder name
                            <> foldlMapWithKey buildAttr attr <> s ">",
                 ()))

-- | Make an XML builder for elements which have no ending tag.
makeXmlElementNoEnd :: Monad m
                    => Text       -- ^ Name.
                    -> HtmlT m () -- ^ A parent element.
makeXmlElementNoEnd name =
  HtmlT (return (\attr _ -> s "<" <> T.encodeUtf8Builder name
                            <> foldlMapWithKey buildAttr attr <> s "/>",
                 ()))

-- | Build and encode an attribute.
buildAttr :: Text -> Text -> Builder
buildAttr key val =
  s " " <>
  T.encodeUtf8Builder key <>
  if val == mempty
     then mempty
     else s "=\"" <> encode val <> s "\""

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldlMapWithKey f = M.foldlWithKey' (\m k v -> m <> f k v) mempty

-- | Convenience function for constructing builders.
s :: String -> Builder
s = fromString
{-# INLINE s #-}

--------------------------------------------------------------------------------
-- Encoding

-- | Encode the given strict plain text to an encoded HTML builder.
encode :: Text -> Builder
encode = T.encodeUtf8BuilderEscaped charUtf8HtmlEscaped

-- | Encode the given strict plain text to an encoded HTML builder.
encodeLazy :: LT.Text -> Builder
encodeLazy = LT.encodeUtf8BuilderEscaped charUtf8HtmlEscaped

{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: BP.BoundedPrim Word8
charUtf8HtmlEscaped =
    condB (== c2w '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    condB (== c2w '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    condB (== c2w '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    condB (== c2w '"' ) (fixed5 ('&',('#',('3',('4',';'))))) $  -- &#34;
    condB (== c2w '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
    (liftFixedToBounded BP.word8)  -- fallback for Chars smaller than '>'
  where
    {-# INLINE fixed4 #-}
    fixed4 x = liftFixedToBounded $ const x >$<
      BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7

    {-# INLINE fixed5 #-}
    fixed5 x = liftFixedToBounded $ const x >$<
      BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7

    c2w = fromIntegral . ord

