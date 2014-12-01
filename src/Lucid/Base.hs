{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base types and combinators.

module Lucid.Base
  (-- * Rendering
   renderText
  ,renderBS
  ,renderTextT
  ,renderBST
  ,renderToFile
   -- * Running
  ,execHtml
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
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans
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
data Attribute = Attribute !Text !Text
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
  HtmlT {runHtmlT :: m (HashMap Text Text -> Builder,a)
         -- ^ This is the low-level way to run the HTML transformer,
         -- finally returning an element builder and a value. You can
         -- pass 'mempty' as an argument for a top-level call. See
         -- 'evalHtmlT' and 'execHtmlT' for easier to use functions.
         }

instance NFData a => NFData (Html a) where
  rnf (HtmlT m) = let (f,a) = runIdentity m in f mempty `seq` rnf a

-- | Monoid is right-associative, a la the 'Builder' in it.
instance (Monad m,Monoid a) => Monoid (HtmlT m a) where
  mempty  = return mempty
  mappend = liftM2 mappend

-- | Just re-uses Monad.
instance Monad m => Functor (HtmlT m) where
  fmap = liftM

instance Monad m => Applicative (HtmlT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (HtmlT m) where
  return a = HtmlT (return (mempty,a))
  HtmlT m >>= f =
    HtmlT (do ~(g,a) <- m
              ~(h,b) <- runHtmlT (f a)
              return (g <> h,b))

instance MonadTrans HtmlT where
  lift m = HtmlT (m >>= \a -> return (mempty,a))

-- | If you want to use IO in your HTML generation.
instance MonadIO m => MonadIO (HtmlT m) where
  liftIO = lift . liftIO

-- | Overloaded HTML string literals.
--   ie. div_ "hello" :: Html ()
instance (Monad m,a ~ ()) => IsString (HtmlT m a) where
  fromString = toHtml

-- | Just calls 'renderText'.
instance (m ~ Identity) => Show (HtmlT m a) where
  show = LT.unpack . renderText

-- | Can be converted to HTML.
class ToHtml a where
  toHtml :: Monad m => a -> HtmlT m ()
  toHtmlRaw :: Monad m => a -> HtmlT m ()

instance ToHtml String where
  toHtml = build . Blaze.fromHtmlEscapedString
  toHtmlRaw = build . Blaze.fromString

instance ToHtml Text where
  toHtml = build . Blaze.fromHtmlEscapedText
  toHtmlRaw = build . Blaze.fromText

instance ToHtml LT.Text where
  toHtml = build . Blaze.fromHtmlEscapedLazyText
  toHtmlRaw = build . Blaze.fromLazyText

-- Create a 'HtmlT' directly from a 'Builder'.
build :: Monad m => Builder -> HtmlT m ()
build b = HtmlT (return (const b,()))
{-# INLINE build #-}

-- | Used to construct HTML terms.
--
-- Simplest use: p_ = term "p" yields 'Lucid.Html5.p_'.
--
-- Very overloaded for three cases:
--
-- * The first case is the basic @arg@ of @[Attribute]@ which will
--   return a function that wants children.
-- * The second is an @arg@ which is @HtmlT m ()@, in which case the
--   term accepts no attributes and just the children are used for the
--   element.
-- * Finally, this is also used for overloaded attributes, like
--   `Lucid.Html5.style_` or `Lucid.Html5.title_`. If a return type of @Attribute@ is inferred
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
  termWith name attrs = with (makeElement name) . (attrs ++)
  {-# SPECIALIZE termWith :: Text -> [Attribute] -> [Attribute] -> (Html () -> Html ()) #-}

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m) => Term (HtmlT m a) (HtmlT m a) where
  termWith = with . makeElement
  {-# SPECIALIZE termWith :: Text -> [Attribute] -> Html () -> Html () #-}

-- | Some terms (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance Term Text Attribute where
  termWith = const . makeAttribute

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
  termRawWith name f attrs = termWith name f attrs . toHtmlRaw

-- | Given children immediately, just use that and expect no
-- attributes.
instance (Monad m,a ~ ()) => TermRaw Text (HtmlT m a) where
  termRawWith name f = termWith name f . toHtmlRaw

-- | Some termRaws (like 'Lucid.Html5.style_', 'Lucid.Html5.title_') can be used for
-- attributes as well as elements.
instance TermRaw Text Attribute where
  termRawWith = termWith

-- | With an element use these attributes. An overloaded way of adding
-- attributes either to an element accepting attributes-and-children
-- or one that just accepts attributes. See the two instances.
class With a where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html a@ or @Html a -> Html a@.
       -> [Attribute]
       -> a

-- | For the contentless elements: 'Lucid.Html5.br_'
instance Monad m => With (HtmlT m a) where
  with m attr =
    HtmlT (runHtmlT m >>= \ ~(f,a) -> return (f . insertArgs attr,a))
  {-# SPECIALIZE with :: Html () -> [Attribute] -> Html () #-}

-- | For the contentful elements: 'Lucid.Html5.div_'
instance Monad m => With (HtmlT m a -> HtmlT m a) where
  with m attr child =
    HtmlT (runHtmlT (m child) >>= \ ~(f,a) -> return (f . insertArgs attr,a))
  {-# SPECIALIZE with :: (Html () -> Html ()) -> [Attribute] -> (Html () -> Html ()) #-}

-- | Insert a list of Attributes into a HashMap and append duplicate keys.
insertArgs :: [Attribute] -> HashMap Text Text -> HashMap Text Text
insertArgs = flip (foldr (\(Attribute k v) -> M.insertWith mappend k v))

--------------------------------------------------------------------------------
-- Running

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'renderBS',
-- Check the source if you're interested in the lower-level behaviour.
--
renderToFile :: FilePath -> Html a -> IO ()
renderToFile fp = L.writeFile fp . renderBS

-- | Render the HTML to a lazy 'ByteString'.
--
-- This is a convenience function defined in terms of 'execHtml',
-- and 'Blaze.toLazyByteString'. Check the source if you're
-- interested in the lower-level behaviour.
--
renderBS :: Html a -> ByteString
renderBS = Blaze.toLazyByteString . execHtml

-- | Render the HTML to a lazy 'Text'.
--
-- This is a convenience function defined in terms of 'renderBS',
-- and 'LT.decodeUtf8'. Check the source if you're interested in the
-- lower-level behaviour.
--
renderText :: Html a -> LT.Text
renderText = LT.decodeUtf8 . renderBS

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
execHtml :: Html a -> Builder
execHtml m = let (f,_) = runIdentity (runHtmlT m) in f mempty

-- | Build the HTML in a monad. Analogous to @execStateT@.
--
-- You might want to use this is if you want to do something with the
-- raw 'Builder'. Otherwise for simple cases you can just use
-- 'renderText' or 'renderBS'.
execHtmlT :: Monad m
          => HtmlT m a  -- ^ The HTML to generate.
          -> m Builder  -- ^ The @a@ is discarded.
execHtmlT m =
  do (f,_) <- runHtmlT m
     return (f mempty)

-- | Evaluate the HTML to its return value in a monad. Analogous to @evalStateT@.
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
makeAttribute = Attribute

-- | Make an HTML builder.
makeElement :: Monad m
            => Text       -- ^ Name.
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m a -- ^ A parent element.
makeElement name child =
  HtmlT (runHtmlT child >>= \ ~(f,a) ->
    return (\attrs -> open name attrs <> f mempty <> close name,a))
{-# INLINE makeElement #-}

-- | Make an HTML builder for elements which have no ending tag.
makeElementNoEnd :: Monad m
                 => Text       -- ^ Name.
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name = HtmlT (return (open name,()))
{-# INLINE makeElementNoEnd #-}

-- | Build and encode an attribute.
buildAttr :: Text -> Text -> Builder
buildAttr key val =
  c ' ' <>
  t key <>
  if T.null val
     then mempty
     else s "=\"" <> Blaze.fromHtmlEscapedText val <> c '"'

-- | Build an opening tag with attributes.
open :: Text -> HashMap Text Text -> Builder
open name attrs = c '<' <> t name <> foldlMapWithKey buildAttr attrs <> c '>'
{-# INLINE open #-}

-- | Build a closing tag.
close :: Text -> Builder
close name = s "</" <> t name <> c '>'
{-# INLINE close #-}

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldlMapWithKey f = M.foldlWithKey' (\m k v -> m <> f k v) mempty

-- | Convenience function for constructing 'Builder' from 'Char'.
c :: Char -> Builder
c = Blaze.fromChar

-- | Convenience function for constructing 'Builder' from 'String'.
s :: String -> Builder
s = Blaze.fromString

-- | Convenience function for constructing 'Builder' from 'Text'.
t :: Text -> Builder
t = Blaze.fromText
