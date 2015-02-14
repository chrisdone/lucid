{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.Pagination where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


pagination_ :: Term arg result => arg -> result
pagination_ = termWith
  "ul" [class_ "pagination"]

arrow_ :: T.Text
arrow_ = " arrow "

