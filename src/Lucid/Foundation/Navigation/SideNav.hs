{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.SideNav where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


side_nav_ :: Term arg result => arg -> result
side_nav_ =
  termWith "ul" [class_ "side-nav"]
