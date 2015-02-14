{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.Magellan where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


data_magellan_expedition_ :: Attribute
data_magellan_expedition_ = data_ "magellan-expedition" "fixed"

data_magellan_arrival_ :: T.Text -> Attribute
data_magellan_arrival_ dest = data_ "magellan-arrival" dest

data_magellan_destination_ :: T.Text -> Attribute
data_magellan_destination_ dest = data_ "magellan-expedition" dest
