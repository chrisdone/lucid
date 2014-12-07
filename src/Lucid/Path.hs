{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}


module Lucid.Path where

import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Reader.Class

import Data.Monoid ((<>))
import Control.Applicative
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

-- | For coercing a global default (relative, in this case) throughout an @HtmlT@.
newtype UrlRelativeT (m :: * -> *) a = UrlRelativeT { runUrlRelativeT :: m a }
  deriving (Show, Eq)

instance Functor f => Functor (UrlRelativeT f) where
  fmap g x = UrlRelativeT $ fmap g $ runUrlRelativeT x

instance Applicative f => Applicative (UrlRelativeT f) where
  (<*>) gs x = UrlRelativeT $ (<*>) (runUrlRelativeT gs) $ runUrlRelativeT x

instance Monad m => Monad (UrlRelativeT m) where
  return  = UrlRelativeT . return
  x >>= f = UrlRelativeT $
    runUrlRelativeT x >>= (runUrlRelativeT . f)

instance MonadTrans UrlRelativeT where
  lift = UrlRelativeT

instance Monad m => Url (UrlString RelativeUrl) (UrlRelativeT m) where
  renderUrl = UrlRelativeT . return . expandRelative

-- | For coercing a global default (grounded, in this case) throughout an 
-- @HtmlT@.
newtype UrlGroundedT (m :: * -> *) a = UrlGroundedT { runUrlGroundedT :: m a }
instance Monad m => Monad (UrlGroundedT m) where
  return  = UrlGroundedT . return
  x >>= f = UrlGroundedT $
    runUrlGroundedT x >>= (runUrlGroundedT . f)
    

instance MonadTrans UrlGroundedT where
  lift = UrlGroundedT

instance Monad m => Url (UrlString GroundedUrl) (UrlGroundedT m) where
  renderUrl = UrlGroundedT . return . expandGrounded

-- | For coercing a global default (absolute, in this case) throughout an 
-- @HtmlT@.
newtype UrlAbsoluteT (m :: * -> *) a = UrlAbsoluteT { runUrlAbsoluteT :: T.Text -> m a }
instance Monad m => Monad (UrlAbsoluteT m) where
  return  = lift . return
  x >>= f = UrlAbsoluteT $ \z -> do
    a <- runUrlAbsoluteT x z
    runUrlAbsoluteT (f a) z

instance MonadTrans UrlAbsoluteT where
  lift :: Monad m => m a -> UrlAbsoluteT m a
  lift m = UrlAbsoluteT (const m)

instance Monad m => MonadReader T.Text (UrlAbsoluteT m) where
--  ask :: Monad m => UrlAbsoluteT m T.Text
  ask = UrlAbsoluteT return

instance (Monad m, a ~ AbsoluteUrl) => Url (UrlString a) (UrlAbsoluteT m) where
  renderUrl = expandAbsolute

{-
src_u :: Url a m => a -> m Attribute
src_u x = do
  url <- renderUrl x

  return $ makeAttribute "src" url
-}
-- * Example
--
-- > script_ [src_u $ "foo.php" <?> ("key","bar")] ""
-- >
-- >
