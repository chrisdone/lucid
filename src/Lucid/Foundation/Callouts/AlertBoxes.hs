{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Callouts.AlertBoxes where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_alert_ :: Attribute
data_alert_ = data_ "alert" ""

alert_box_ :: T.Text
alert_box_ = " alert-box "

close_ :: T.Text
close_ = " close "



