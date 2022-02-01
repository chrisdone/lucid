{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.
--
-- This version is the same as Lucid.Html5 but with 'class_' changed.

module Lucid.Html5_2
  (module Lucid.Html5
  ,class_)
  where

import Lucid.Base
import Lucid.Html5 hiding (class_, style_)

import Data.Monoid
import Data.Text (Text)

-- | The @class@ attribute, outputting a space after the value.
class_ :: Text -> Attribute
class_ = makeAttribute "class" . (<> " ")
