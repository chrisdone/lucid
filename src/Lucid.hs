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
   -- * Types
  ,Html
   -- * Classes
  ,Term(..)
  ,ToHtml(..)
  ,With(..)
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
-- See "Lucid.Html5" for a complete list of Html5 combinators. That
-- module is re-exported from this module for your convenience.
--
-- Plain text is written using the @OverloadedStrings@ and
-- @ExtendedDefaultRules@ extensions, and is automatically escaped:
--
-- >>> "123 < 456" :: Html ()
-- 123 &lt; 456
--
-- Except some elements, like 'script_':
--
-- >>> script_ "alert('Hello!' > 12)" :: Html ()
-- <script>alert('Hello!' > 12)</script>
--
-- Elements nest by function application:
--
-- >>> table_ (tr_ (td_ (p_ "Hello, World!"))) :: Html ()
-- <table><tr><td><p>Hello, World!</p></td></tr></table>
--
-- Elements are juxtaposed via monoidal append:
--
-- >>> p_ "hello" <> p_ "sup" :: Html ()
-- <p>hello</p><p>sup</p>
--
-- Or monadic sequencing:
--
-- >>> div_ (do p_ "hello"; p_ "sup") :: Html ()
-- <div><p>hello</p><p>sup</p></div>
--
-- Attributes are set by providing an argument list:
--
-- >>> p_ [class_ "brand"] "Lucid Inc" :: Html ()
-- <p class="brand">Lucid Inc</p>
--
-- Here is a fuller example of Lucid:
--
-- @
-- table_ [rows_ "2"]
--        (tr_ (do td_ [class_ "top",colspan_ "2"]
--                     (p_ "Hello, attributes!")
--                 td_ "yay!"))
-- @
--
-- Elements (and some attributes) are variadic and overloaded, see the
-- 'Term' class for more explanation about that.
--
-- For proper rendering you can easily run some HTML immediately with:
--
-- >>> renderText (p_ "Hello!")
-- > "<p>Hello!</p>"
--
-- >>> renderBS (p_ [style_ "color:red"] "Hello!")
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
