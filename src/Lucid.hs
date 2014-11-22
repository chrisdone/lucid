-- | Clear to write, read and edit DSL for writing HTML

module Lucid
  (-- * Intro
   -- $intro
   renderText
  ,renderBS
  ,renderTextT
  ,renderBST
   -- * Running
   -- $running
  ,execHtmlT
  ,evalHtmlT
  ,runHtmlT
  -- * Combinators
  ,with
   -- * Types
  ,Html
   -- * Classes
  ,ToHtml(..)
  ,Mixed(..)
  ,With
  -- * Re-exports
  ,module Lucid.Html5)
 where

import Lucid.Base
import Lucid.Html5

-- $intro
--
-- HTML terms in Lucid are written with a postfix ‘@_@’ to indicate data
-- rather than code. Some examples:
--
-- 'p_', 'class_', 'table_', 'style_'
--
-- See "Lucid.Html5" for a complete list of Html5 combinators.
--
-- Plain text is written using the @OverloadedStrings@ and
-- @ExtendedDefaultRules@ extensions, and is automatically escaped:
--
-- >>> "123 < 456" :: Html ()
-- 123 &lt; 456
--
-- Elements nest by function application:
--
-- >>> table_ (tr_ (td_ (p_ "Hello, World!")))
-- <table><tr><td><p>Hello, World!</p></td></tr></table>
--
-- Elements are juxtaposed via monoidal append:
--
-- >>> p_ "hello" <> p_ "sup"
-- <p>hello</p><p>sup</p>
--
-- Or monadic sequencing:
--
-- >>> div_ (do p_ "hello"; p_ "sup")
-- <div><p>hello</p><p>sup</p></div>
--
-- Attributes are set using the 'with' combinator:
--
-- >>> with p_ [class_ "brand"] "Lucid Inc"
-- <p class="brand">Lucid Inc</p>
--
-- Here is a fuller example of Lucid:
--
-- @
-- with table_ [rows_ "2"]
--      (tr_ (do with td_ [class_ "top",colspan_ "2"]
--                    (p_ "Hello, attributes!")
--               td_ "yay!"))
-- @
--
-- For proper rendering you can easily run some HTML immediately with:
--
-- >>> renderText (p_ "Hello!")
-- > "<p>Hello!</p>"
--
-- >>> renderBS (with p_ [style_ "color:red"] "Hello!")
-- "<p style=\"color:red\">Hello!</p>"
--
-- For ease of use in GHCi, there is a 'Show' instance, as
-- demonstrated above.

-- $running
--
-- If the above rendering functions aren't suited for your purpose,
-- you can run the monad directly and use the more low-level blaze
-- `Builder`, which has a plethora of output modes in
-- "Blaze.ByteString.Builder".
