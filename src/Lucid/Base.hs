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
   -- * Running
  ,execHtmlT
  ,evalHtmlT
  ,runHtmlT
  -- * Combinators
  ,with
  ,makeElement
  ,makeElementNoEnd
   -- * Types
  ,Html
  ,Attr(..)
  ,HtmlT
   -- * Classes
  ,ToText(..)
  ,ToHtml(..)
  ,Mixed(..)
  ,With)
  where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

--------------------------------------------------------------------------------
-- Types

-- | Simple HTML builder type. Defined in terms of 'HtmlT'. Check out
-- that type for instance information.
--
-- Simple use-cases will just use this type. But if you want to
-- transformer over Reader or something, you can go and use 'HtmlT'.
type Html = HtmlT Identity

-- | A monad transformer that generates HTML. Use the simpler 'Html'
-- type if you don't want to transform over some other monad.
newtype HtmlT m a =
  HtmlT {runHtmlT :: m (Builder -> Builder -> Builder,a)
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

-- | We pack it via string. Could possibly encode straight into a
-- builder. That might be faster.
instance (Monad m,a ~ ()) => IsString (HtmlT m a) where
  fromString m' =
    HtmlT (return (\_ _ -> encode (T.pack m'),()))

-- | Just calls 'renderText'.
instance (m ~ Identity) => Show (HtmlT m a) where
  show = LT.unpack . renderText

-- | An attribute.
data Attr =
  Attr {attrName :: !Builder -- ^ The attribute name.
       ,attrValue :: !Text   -- ^ The attribute value.
       }

-- | Used for attributes.
class ToText a where
  toText :: a -> Text

instance ToText String where
  toText = T.pack

instance ToText Text where
  toText = id

-- | Can be converted to HTML.
class ToHtml a where
  toHtml :: a -> Html ()
  toHtmlRaw :: a -> Html ()

instance ToHtml String where
  toHtml = fromString
  toHtmlRaw m = HtmlT (return ((\_ _ -> Blaze.fromString m),()))

instance ToHtml Int where
  toHtml i = HtmlT (return ((\_ _ -> Blaze.fromInthost i),()))
  toHtmlRaw = toHtml

instance ToHtml Text where
  toHtml m = HtmlT (return ((\_ _ -> encode m),()))
  toHtmlRaw m = HtmlT (return ((\_ _ -> Blaze.fromText m),()))

-- | Used for names that are mixed, e.g. 'style_'.
class Mixed a r where
  mixed :: Builder -> a -> r

-- | Attributes can be a mixed thing e.g. 'style_'.
instance (ToText a) => Mixed a Attr where
  mixed s = Attr s . toText

-- | HTML elements can be a mixed thing e.g. 'style_'.
instance (Monad m,a ~ HtmlT m r,r ~ ()) => Mixed a (HtmlT m r) where
  mixed = makeElement

-- | With an element use these attributes.
class With a where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html ()@ or @Html () -> Html ()@.
       -> [Attr] -> a

-- | For the contentless elements: 'br_'
instance (Monad m,a ~ ()) => With (HtmlT m a) where
  with f =
    \attr ->
      HtmlT (do ~(f',_) <- runHtmlT f
                return (\attr' m' -> f' (attr' <> mconcat (map buildAttr attr)) m',()))
    where buildAttr :: Attr -> Builder
          buildAttr (Attr key val) =
            Blaze.fromString " " <>
            key <>
            if val == mempty
               then mempty
               else Blaze.fromString "=\"" <>
                    Blaze.fromText val <>
                    Blaze.fromText "\""

-- | For the contentful elements: 'div_'
instance (Monad m,a ~ ()) => With (HtmlT m a -> HtmlT m a) where
  with f =
    \attr inner ->
      HtmlT (do ~(f',_) <- runHtmlT (f inner)
                return ((\attr' m' ->
                           f' (attr' <>
                               mconcat (map buildAttr attr))
                              m'),
                        ()) )
    where buildAttr :: Attr -> Builder
          buildAttr (Attr key val) =
            Blaze.fromString " " <>
            key <>
            if val == mempty
               then mempty
               else Blaze.fromString "=\"" <>
                    Blaze.fromText val <>
                    Blaze.fromText "\""

--------------------------------------------------------------------------------
-- Running

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

-- | Make an HTML builder.
makeElement :: Monad m
            => Builder -- ^ Name.
            -> HtmlT m a  -- ^ Children HTML.
            -> HtmlT m () -- ^ A parent element.
makeElement name =
  \m' ->
    HtmlT (do ~(f,_) <- runHtmlT m'
              return ((\attr m -> s "<" <> name <> attr <> s ">" <> m <> f mempty mempty <> s "</" <>
                                   name <> s ">"),
                      ()))
  where s = Blaze.fromString

-- | Make an HTML builder for
makeElementNoEnd :: Monad m
                 => Builder -- ^ Name.
                 -> HtmlT m () -- ^ A parent element.
makeElementNoEnd name =
  HtmlT (return ((\attr _ -> s "<" <> name <> attr <> s ">"),
                 ()))
  where s = Blaze.fromString

--------------------------------------------------------------------------------
-- Encoding

-- | Encode the given strict plain text to an encoded HTML builder.
encode :: Text -> Builder
encode = Blaze.fromHtmlEscapedText
