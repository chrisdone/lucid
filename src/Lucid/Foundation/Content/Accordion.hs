{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Content.Accordion where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_accordion_ :: T.Text -> Attribute
data_accordion_ = data_ "accordion"

accordion_ :: T.Text
accordion_ = " accordion "

accordion_navigation_ :: T.Text
accordion_navigation_ = " accordion-navigation "



