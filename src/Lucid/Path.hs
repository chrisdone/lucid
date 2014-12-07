{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lucid.Path where

import qualified Data.Text as T

import Control.Monad.Reader.Class

import Data.Monoid ((<>))
import Data.Functor.Identity

-- | @Url@ takes an input type @a@, and returns a modality @f@ around @T.Text@.
class Url a f where
  renderUrl :: a -> f T.Text

-- | Normal @T.Text@ support
instance Url T.Text Identity where
  renderUrl = Identity . id

-- | A GET parameter encoded in a Url
data GETParam = GETParam { key :: !T.Text
                         , val :: !T.Text
                         }
  deriving (Show, Eq)

-- | Render a GET parameter pair
renderGETParam :: GETParam
               -> T.Text
renderGETParam (GETParam k v) =
  "&" <> k <> "=" <> v

-- | A Relative Url string with a phantom data type denoting the default 
-- expansion scheme.
data UrlString mode = UrlString { target :: !T.Text
                                , params :: [GETParam]
                                }
  deriving (Show, Eq)

-- | Render a Url String /simply/ - this is equivalent to @expandRelative@.
renderUrlString :: UrlString mode
                -> T.Text
renderUrlString (UrlString t []) = t
renderUrlString (UrlString t [(GETParam k v)]) =
  t <> "?" <> k <> "=" <> v
renderUrlString (UrlString t ((GETParam k v):ps)) =
  t <> "?" <> k <> "=" <> v <>
    (foldr (\x acc -> acc <> renderGETParam x) "" ps)


-- | Lifts a target path with some GET parameter chunks into a @UrlString@.
(<?>) :: T.Text
      -> (T.Text, T.Text)
      -> UrlString mode
t <?> (k,v) = UrlString t [GETParam k v]

infixl 9 <?>

-- | Adds more GET parameters to a @UrlString@.
(<&>) :: UrlString mode
      -> (T.Text, T.Text)
      -> UrlString mode
old <&> (k,v) = UrlString (target old) $ params old ++ [GETParam k v]

infixl 8 <&>

-- | Phantom Constant for relative url rendering
data RelativeUrl = RelativeUrl
  deriving (Show, Eq)

-- | Phantom Constant for grounded url rendering
data GroundedUrl = GroundedUrl
  deriving (Show, Eq)

-- | Phantom Constant for absolute url rendering
data AbsoluteUrl = AbsoluteUrl
  deriving (Show, Eq)

-- | Render the Url String as relative
expandRelative :: UrlString RelativeUrl
               -> T.Text
expandRelative = renderUrlString

-- | Render the Url String as grounded
expandGrounded :: UrlString GroundedUrl
               -> T.Text
expandGrounded x = "/" <> renderUrlString x

-- | Render the Url String as absolute - getting the root from a MonadReader.
expandAbsolute :: (MonadReader T.Text m) =>
                  UrlString AbsoluteUrl
               -> m T.Text
expandAbsolute x = do
  root <- ask
  return $ root <> "/" <> renderUrlString x

-- | Render the Url String as absolute, but with your own configuration type.
expandAbsoluteWith :: (MonadReader a m) =>
                      UrlString AbsoluteUrl
                   -> (a -> T.Text)
                   -> m T.Text
expandAbsoluteWith x f = do
  root <- ask >>= return . f
  return $ root <> "/" <> renderUrlString x


instance Url (UrlString RelativeUrl) Identity where
  renderUrl = Identity . expandRelative

instance Url (UrlString GroundedUrl) Identity where
  renderUrl = Identity . expandGrounded

instance (MonadReader T.Text m) => Url (UrlString AbsoluteUrl) m where
  renderUrl = expandAbsolute

