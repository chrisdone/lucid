{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.SubNav where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


sub_nav_ :: Term arg result => arg -> result
sub_nav_ =
  termWith "dd" [class_ "sub-nav"]
