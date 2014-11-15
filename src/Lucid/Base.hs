{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Base types and combinators.

module Lucid.Base
  (-- * Rendering
   renderText
  ,renderBS
   -- * Running
  ,execHtml
  ,evalHtml
  -- * Combinators
  ,with
  ,makeElement
  ,makeElementNoEnd
   -- * Types
  ,Html
  ,Attr(..)
   -- * Classes
  ,ToText(..)
  ,ToHtml(..)
  ,Mixed(..))
  where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid
import           Data.String
import           Data.Text (Text,pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

--------------------------------------------------------------------------------
-- Types

-- | Simple Html builder type.
data Html a =
  Html {htmlBuilder :: !(Builder -> Builder -> Builder)
       ,htmlReturn :: a}

-- | Monoid is right-associative, a la the 'Builder' in it.
instance Monoid a => Monoid (Html a) where
  mempty = Html (\_ _ -> mempty) mempty
  Html f a `mappend` Html g b =
    Html (\attr inner ->
            (f attr inner <>
             g attr inner))
         (a <> b)

-- | Based on the monad instance.
instance Applicative Html where
  pure = return
  (<*>) = ap

-- | Only touches the return part.
instance Functor Html where
  fmap f html =
    html {htmlReturn = f (htmlReturn html)}

-- | Basically acts like Writer.
instance Monad Html where
  return a = Html (\_ _ -> mempty) a
  Html g a >>= f =
    let Html f' b = f a
    in Html (\attr inner ->
               g attr inner <>
               f' attr inner)
            b

-- | We pack it via string. Could possibly encode straight into a
-- builder. That might be faster.
instance (a ~ ()) => IsString (Html a) where
  fromString m' =
    Html (\_ _ -> encode (T.pack m')) ()

-- | Just calls 'renderText'.
instance Show (Html a) where
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
  toText = pack

instance ToText Text where
  toText = id

-- | Can be converted to HTML.
class ToHtml a where
  toHtml :: a -> Html ()
  toHtmlRaw :: a -> Html ()

instance ToHtml String where
  toHtml = fromString
  toHtmlRaw m = Html (\_ _ -> Blaze.fromString m) ()

instance ToHtml Int where
  toHtml i = Html (\_ _ -> Blaze.fromInthost i) ()
  toHtmlRaw = toHtml

instance ToHtml Text where
  toHtml m = Html (\_ _ -> encode m) ()
  toHtmlRaw m = Html (\_ _ -> Blaze.fromText m) ()

-- | Used for names that are mixed, e.g. 'style_'.
class Mixed a r where
  mixed :: Builder -> a -> r

-- | Attributes can be a mixed thing e.g. 'style_'.
instance (ToText a) => Mixed a Attr where
  mixed s = Attr s . toText

-- | HTML elements can be a mixed thing e.g. 'style_'.
instance (a ~ Html r,r ~ ()) => Mixed a (Html r) where
  mixed = makeElement

-- | With an element use these attributes.
class With a where
  -- | With the given element(s), use the given attributes.
  with :: a -- ^ Some element, either @Html ()@ or @Html () -> Html ()@.
       -> [Attr] -> a

-- | For the contentless elements: 'br_'
instance (a ~ ()) => With (Html a) where
  with f =
    \attr ->
      let (Html f' _) = f
      in (Html (\attr' m' ->
                  f' (attr' <>
                      mconcat (map buildAttr attr))
                     m')
               ())
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
instance (a ~ ()) => With (Html a -> Html a) where
  with f =
    \attr inner ->
      let (Html f' _) = f inner
      in (Html (\attr' m' ->
                  f' (attr' <>
                      mconcat (map buildAttr attr))
                     m')
               ())
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

-- | Build the HTML. Analogous to @execState@.
execHtml :: Html a  -- ^ The HTML to generate.
         -> Builder -- ^ The @a@ is discarded.
execHtml m = htmlBuilder m mempty mempty

-- | Evaluate the HTML to its return value. Analogous to @evalState@.
evalHtml :: Html a -- ^ HTML monad to evaluate.
         -> a      -- ^ Ignore the HTML output and just return the value.
evalHtml = htmlReturn

-- | Render the HTML to a lazy 'ByteString'.
renderBS :: Html a -> ByteString
renderBS = Blaze.toLazyByteString . execHtml

-- | Render the HTML to a lazy 'Text'.
renderText :: Html a -> LT.Text
renderText = LT.decodeUtf8 . Blaze.toLazyByteString . execHtml

--------------------------------------------------------------------------------
-- Combinators

-- | Make an HTML builder.
makeElement :: Builder -- ^ Name.
            -> Html a  -- ^ Children HTML.
            -> Html () -- ^ A parent element.
makeElement name =
  \m' ->
    Html (\attr m -> s "<" <> name <> attr <> s ">" <> m <> execHtml m' <> s "</" <>
                      name <> s ">")
         ()
  where s = Blaze.fromString

-- | Make an HTML builder for
makeElementNoEnd :: Builder -- ^ Name.
                 -> Html () -- ^ A parent element.
makeElementNoEnd name =
  Html (\attr _ -> s "<" <> name <> attr <> s ">")
       ()
  where s = Blaze.fromString

--------------------------------------------------------------------------------
-- Encoding

-- | Encode the given strict plain text to an encoded HTML builder.
encode :: Text -> Builder
encode = Blaze.fromHtmlEscapedText
