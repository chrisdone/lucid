{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.Breadcrumbs where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


breadcrumbs_ :: T.Text
breadcrumbs_ = " breadcrumbs "

unavailable_ :: T.Text
unavailable_ = " unavailable "

current_ :: T.Text
current_ = " current "

