{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Callouts.Reveal where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_reveal_id_ :: T.Text -> Attribute
data_reveal_id_ = data_ "reveal-id"

reveal_modal_ :: T.Text
reveal_modal_ = " reveal-modal "

close_reveal_modal_ :: T.Text
close_reveal_modal_ = " close-reveal-modal "

data_reveal_ :: Attribute
data_reveal_ = data_ "reveal" ""


