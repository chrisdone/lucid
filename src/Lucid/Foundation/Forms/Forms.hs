{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Forms.Forms where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T

inline_ :: T.Text
inline_ = " inline "

prefix_ :: T.Text
prefix_ = " prefix "

postfix_ :: T.Text
postfix_ = " postfix "

error_ :: T.Text
error_ = " error "

aria_label_ :: T.Text -> Attribute
aria_label_ = makeAttribute "aria-label"

aria_describedby_ :: T.Text -> Attribute
aria_describedby_ = makeAttribute "aria-describedby"
