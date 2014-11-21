lucid
=====

Clear to write, read and edit DSL for writing HTML

## Introduction

HTML terms in Lucid are written with a postfix ‘`_`’ to indicate data
rather than code. Some examples:

`p_`, `class_`, `table_`, `style_`

See `Lucid.Html5` for a complete list of Html5 combinators.

Plain text is written using the `OverloadedStrings` and
`ExtendedDefaultRules` extensions, and is automatically escaped:

``` haskell
λ> "123 < 456" :: Html ()
```

``` html
123 &lt; 456
```

Elements nest by function application:

``` haskell
λ> table_ (tr_ (td_ (p_ "Hello, World!")))
```

``` html
<table><tr><td><p>Hello, World!</p></td></tr></table>
```

Elements are juxtaposed via monoidal append:

``` haskell
λ> p_ "hello" <> p_ "sup"
```

``` html
<p>hello</p><p>sup</p>
```

Or monadic sequencing:

``` haskell
λ> div_ (do p_ "hello"; p_ "sup")
```

``` html
<div><p>hello</p><p>sup</p></div>
```

Attributes are set using the 'with' combinator:

``` haskell
λ> with p_ [class_ "brand"] "Lucid Inc"
```

``` html
<p class="brand">Lucid Inc</p>
```

Here is a fuller example of Lucid:

``` haskell
with table_ [rows_ "2"]
     (tr_ (do with td_ [class_ "top",colspan_ "2"]
                   (p_ "Hello, attributes!")
              td_ "yay!"))
```

``` html
<table rows="2">
  <tr>
     <td class="top" colspan="2">
       <p>Hello, attributes!</p>
     </td>
     <td>yay!</td>
  </tr>
</table>
```

## Rendering

For proper rendering you can easily run some HTML immediately with:

``` haskell
λ> renderText (p_ "Hello!")
```

``` html
"<p>Hello!</p>"
```

Or to bytes:

``` haskell
λ> renderBS (with p_ [style_ "color:red"] "Hello!")
```

``` html
"<p style=\"color:red\">Hello!</p>"
```

For ease of use in GHCi, there is a `Show` instance, as
demonstrated above.

If the above rendering functions aren't suited for your purpose, you
can run the monad directly via `execHtml` and use the more low-level
blaze `Builder`, which has a plethora of output modes in
Blaze.ByteString.Builder.

See the documentation for the `Lucid` module for information about
using it as a monad transformer.

## Transforming

You can use `lift` to call parent monads.

``` haskell
λ> runReader (renderTextT (html_ (body_ (do name <- lift ask; p_ (toHtml name)))))
             ("Chris" :: String)
"<html><body><p>Chris</p></body></html>"
```
