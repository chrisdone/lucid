{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Forms.Switch where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


switch_ :: T.Text
switch_ = " switch "

tabindex_ :: Int -> Attribute
tabindex_ = makeAttribute "tabindex" . T.pack . show
