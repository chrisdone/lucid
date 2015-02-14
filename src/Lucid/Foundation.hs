{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Foundation layout elements. See
-- <http://foundation.zurb.com/> for more information.

module Lucid.Foundation
  ( module Lucid.Foundation.Structure
  , module Lucid.Foundation.Navigation
  , module Lucid.Foundation.Media
  , module Lucid.Foundation.Forms
  , module Lucid.Foundation.Buttons
  , module Lucid.Foundation.Typography
  , module Lucid.Foundation.Callouts
  , module Lucid.Foundation.Content
  ) where


import Lucid.Foundation.Structure
import Lucid.Foundation.Navigation
import Lucid.Foundation.Media
import Lucid.Foundation.Forms
import Lucid.Foundation.Buttons
import Lucid.Foundation.Typography
import Lucid.Foundation.Callouts
import Lucid.Foundation.Content

import Lucid.Base
import Lucid.Html5

import qualified Data.Text as T
import Data.Monoid

