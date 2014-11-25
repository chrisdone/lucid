{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

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
instance Monoid a => Monoid (Html a) where
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
  toHtmlRaw m = HtmlT (return ((\_ _ -> Blaze.fromString m),()))

instance ToHtml Text where
  toHtml m = HtmlT (return ((\_ _ -> encode m),()))
  toHtmlRaw m = HtmlT (return ((\_ _ -> Blaze.fromText m),()))

-- | Used to represent HTML terms. Very overloaded for three cases:
--
-- * The first case is the basic @arg@ of @[(Text,Text)]@ which will
--   return a function that wants children.
-- * The second is an @arg@ which is @HtmlT m ()@, in which case the
--   term accepts no attributes and just the children are used for the
--   element.
-- * Finally, this is also used for overloaded attributes, like
--   `_style` or `_title`. If a return type of @(Text,Text)@ is inferred
--   then an attribute will be made.
--
-- The instances look intimidating but actually the constraints make
-- it very general so that type inference works very well even in the
-- presence of things like @OverloadedLists@ and such. The last thing
-- you want to do is add type annotations to your HTML templates.
class Term arg result where
  termWith :: Builder       -- ^ Name.
           -> [Attribute] -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.
  term :: Builder -> arg -> result
  term = flip termWith []

-- | Given attributes, expect more child input.
instance (Monad m,children ~ HtmlT m unit,unit ~ (),attribute ~ Attribute)
         => Term [attribute] (children -> HtmlT m unit) where
  termWith name f = with (makeElement name) . (<> f)

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,f ~ HtmlT m unit,unit ~ ()) => Term f (HtmlT m unit) where
  termWith name f = with (makeElement name) f

-- | Some terms (like style_, title_) can be used for attributes as
-- well as elements.
instance (a ~ Text) => Term a Attribute where
  termWith key _ value = makeAttribute (blazeToString key) value

class TermRaw arg result where
  termRawWith :: Builder    -- ^ Name.
           -> [Attribute] -- ^ Attribute transformer.
           -> arg           -- ^ Some argument.
           -> result        -- ^ Result: either an element or an attribute.
  termRaw :: Builder -> arg -> result
  termRaw = flip termRawWith []

-- | Given attributes, expect more child input.
instance (Monad m,ToHtml html,unit ~ (),attribute ~ Attribute)
         => TermRaw [attribute] (html -> HtmlT m unit) where
  termRawWith name f attrs = with (makeElement name) (attrs <> f) . toHtmlRaw

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,ToHtml html,unit ~ ()) => TermRaw html (HtmlT m unit) where
  termRawWith name f = with (makeElement name) f . toHtmlRaw

-- | Some terms (like style_, title_) can be used for attributes as
-- well as elements.
instance (a ~ Text) => TermRaw a Attribute where
  termRawWith key _ value = makeAttribute (blazeToString key) value

-- | With an element use these attributes. An overloaded way of adding
-- attributes either to an element accepting attributes-and-children
-- or one that just accepts attributes. See the two instances.
class With a  where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html ()@ or @Html () -> Html ()@.
       -> [Attribute]
       -> a

-- | For the contentless elements: 'br_'
instance (Monad m,a ~ ()) => With (HtmlT m a) where
  with f =
    \attr ->
      HtmlT (do ~(f',_) <- runHtmlT f
                return (\attr' m' ->
                          f' (unionArgs (M.fromListWith (<>) (map toPair attr)) attr') m'
                       ,()))
    where toPair (Attribute x) = x

-- | For the contentful elements: 'div_'
instance (Monad m,a ~ ()) => With (HtmlT m a -> HtmlT m a) where
  with f =
    \attr inner ->
      HtmlT (do ~(f',_) <- runHtmlT (f inner)
                return ((\attr' m' ->
                           f' (unionArgs (M.fromListWith (<>) (map toPair attr)) attr') m')
                       ,()))
    where toPair (Attribute x) = x

-- | Union two sets of arguments and append duplicate keys.
unionArgs :: HashMap Text Text -> HashMap Text Text -> HashMap Text Text
unionArgs = M.unionWith (<>)

--------------------------------------------------------------------------------
-- Running

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderToFile :: FilePath -> Html a -> IO ()
renderToFile fp = L.writeFile fp . Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderBS :: Html a -> ByteString
renderBS = Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'Text'.
--
-- This is a convenience function defined in terms of 'execHtmlT',
-- 'runIdentity' and 'Blaze.toLazyByteString', and
-- 'LT.decodeUtf8'. Check the source if you're interested in the
-- lower-level behaviour.
--
renderText :: Html a -> LT.Text
renderText = LT.decodeUtf8 . Blaze.toLazyByteString . runIdentity . execHtmlT

-- | Render the HTML to a lazy 'ByteString', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'Blaze.toLazyByteString'. Check the source if you're interested in
-- the lower-level behaviour.
--
renderBST :: Monad m => HtmlT m a -> m ByteString
renderBST = liftM Blaze.toLazyByteString . execHtmlT

-- | Render the HTML to a lazy 'Text', but in a monad.
--
-- This is a convenience function defined in terms of 'execHtmlT' and
-- 'Blaze.toLazyByteString', and 'LT.decodeUtf8'. Check the source if
-- you're interested in the lower-level behaviour.
--
renderTextT :: Monad m => HtmlT m a -> m LT.Text
renderTextT = liftM (LT.decodeUtf8 . Blaze.toLazyByteString) . execHtmlT

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
            => Builder -- ^ Name.
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m () -- ^ A parent element.
makeElement name =
  \m' ->
    HtmlT (do ~(f,_) <- runHtmlT m'
              return (\attr m -> s "<" <> name <> foldlMapWithKey buildAttr attr <> s ">"
                              <> m <> f mempty mempty
                              <> s "</" <> name <> s ">",
                      ()))

-- | Make an HTML builder for
makeElementNoEnd :: Monad m
                 => Builder -- ^ Name.
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name =
  HtmlT (return (\attr _ -> s "<" <> name <> foldlMapWithKey buildAttr attr <> s ">",
                 ()))

-- | Build and encode an attribute.
buildAttr :: Text -> Text -> Builder
buildAttr key val =
  s " " <>
  Blaze.fromText key <>
  if val == mempty
     then mempty
     else s "=\"" <> encode val <> s "\""

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldlMapWithKey f = M.foldlWithKey' (\m k v -> m <> f k v) mempty

-- | Convenience function for constructing builders.
s :: String -> Builder
s = Blaze.fromString
{-# INLINE s #-}

--------------------------------------------------------------------------------
-- Encoding

-- | Encode the given strict plain text to an encoded HTML builder.
encode :: Text -> Builder
encode = Blaze.fromHtmlEscapedText

-- | Helper to convert a builder to text.
blazeToString :: Builder -> Text
blazeToString = LT.toStrict . LT.decodeUtf8 . Blaze.toLazyByteString
