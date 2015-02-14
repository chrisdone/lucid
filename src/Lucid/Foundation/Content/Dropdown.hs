{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Content.Dropdown where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_dropdown_ :: T.Text -> Attribute
data_dropdown_ = data_ "dropdown"

f_dropdown_ :: T.Text
f_dropdown_ = " f-dropdown "

data_dropdown_content_ :: Attribute
data_dropdown_content_ = data_ "dropdown-content" ""



