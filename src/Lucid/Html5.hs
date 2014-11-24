{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

import           Lucid.Base

import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Data.Monoid
import           Data.Text (Text)

-- | @DOCTYPE@ element
doctype_ :: Monad m => HtmlT m ()
doctype_ = makeElementNoEnd (Blaze.fromString "!DOCTYPE HTML")

-- | @DOCTYPE@ element + @html@ element
doctypehtml_ :: Monad m => HtmlT m () -> HtmlT m ()
doctypehtml_ m =
  do doctype_
     html_ m

-- | @a@ element
a_ :: Term arg result => arg -> result
a_ = term (Blaze.fromString "a")

-- | @abbr@ element
abbr_ :: Term arg result => arg -> result
abbr_ = term (Blaze.fromString "abbr")

-- | @address@ element
address_ :: Term arg result => arg -> result
address_ = term (Blaze.fromString "address")

-- | @area@ element
area_ :: Monad m => [(Text,Text)] -> HtmlT m ()
area_ = with (makeElementNoEnd (Blaze.fromString "area"))

-- | @article@ element
article_ :: Term arg result => arg -> result
article_ = term (Blaze.fromString "article")

-- | @aside@ element
aside_ :: Term arg result => arg -> result
aside_ = term (Blaze.fromString "aside")

-- | @audio@ element
audio_ :: Term arg result => arg -> result
audio_ = term (Blaze.fromString "audio")

-- | @b@ element
b_ :: Term arg result => arg -> result
b_ = term (Blaze.fromString "b")

-- | @base@ element
base_ :: Monad m => [(Text,Text)] -> HtmlT m ()
base_ = with (makeElementNoEnd (Blaze.fromString "base"))

-- | @bdo@ element
bdo_ :: Term arg result => arg -> result
bdo_ = term (Blaze.fromString "bdo")

-- | @blockquote@ element
blockquote_ :: Term arg result => arg -> result
blockquote_ = term (Blaze.fromString "blockquote")

-- | @body@ element
body_ :: Term arg result => arg -> result
body_ = term (Blaze.fromString "body")

-- | @br@ element
br_ :: Monad m => [(Text,Text)] -> HtmlT m ()
br_ = with (makeElementNoEnd (Blaze.fromString "br"))

-- | @button@ element
button_ :: Term arg result => arg -> result
button_ = term (Blaze.fromString "button")

-- | @canvas@ element
canvas_ :: Term arg result => arg -> result
canvas_ = term (Blaze.fromString "canvas")

-- | @caption@ element
caption_ :: Term arg result => arg -> result
caption_ = term (Blaze.fromString "caption")

-- | @cite@ element or @cite@ attribute.
cite_ :: Term arg result => arg -> result
cite_ = term (Blaze.fromString "cite")

-- | @code@ element
code_ :: Term arg result => arg -> result
code_ = term (Blaze.fromString "code")

-- | @col@ element
col_ :: Monad m => [(Text,Text)] -> HtmlT m ()
col_ = with (makeElementNoEnd (Blaze.fromString "col"))

-- | @colgroup@ element
colgroup_ :: Term arg result => arg -> result
colgroup_ = term (Blaze.fromString "colgroup")

-- | @command@ element
command_ :: Term arg result => arg -> result
command_ = term (Blaze.fromString "command")

-- | @datalist@ element
datalist_ :: Term arg result => arg -> result
datalist_ = term (Blaze.fromString "datalist")

-- | @dd@ element
dd_ :: Term arg result => arg -> result
dd_ = term (Blaze.fromString "dd")

-- | @del@ element
del_ :: Term arg result => arg -> result
del_ = term (Blaze.fromString "del")

-- | @details@ element
details_ :: Term arg result => arg -> result
details_ = term (Blaze.fromString "details")

-- | @dfn@ element
dfn_ :: Term arg result => arg -> result
dfn_ = term (Blaze.fromString "dfn")

-- | @div@ element
div_ :: Term arg result => arg -> result
div_ = term (Blaze.fromString "div")

-- | @dl@ element
dl_ :: Term arg result => arg -> result
dl_ = term (Blaze.fromString "dl")

-- | @dt@ element
dt_ :: Term arg result => arg -> result
dt_ = term (Blaze.fromString "dt")

-- | @em@ element
em_ :: Term arg result => arg -> result
em_ = term (Blaze.fromString "em")

-- | @embed@ element
embed_ :: Monad m => [(Text,Text)] -> HtmlT m ()
embed_ = with (makeElementNoEnd (Blaze.fromString "embed"))

-- | @fieldset@ element
fieldset_ :: Term arg result => arg -> result
fieldset_ = term (Blaze.fromString "fieldset")

-- | @figcaption@ element
figcaption_ :: Term arg result => arg -> result
figcaption_ = term (Blaze.fromString "figcaption")

-- | @figure@ element
figure_ :: Term arg result => arg -> result
figure_ = term (Blaze.fromString "figure")

-- | @footer@ element
footer_ :: Term arg result => arg -> result
footer_ = term (Blaze.fromString "footer")

-- | @form@ element or @form@ attribute
form_ :: Term arg result => arg -> result
form_ = term (Blaze.fromString "form")

-- | @h1@ element
h1_ :: Term arg result => arg -> result
h1_ = term (Blaze.fromString "h1")

-- | @h2@ element
h2_ :: Term arg result => arg -> result
h2_ = term (Blaze.fromString "h2")

-- | @h3@ element
h3_ :: Term arg result => arg -> result
h3_ = term (Blaze.fromString "h3")

-- | @h4@ element
h4_ :: Term arg result => arg -> result
h4_ = term (Blaze.fromString "h4")

-- | @h5@ element
h5_ :: Term arg result => arg -> result
h5_ = term (Blaze.fromString "h5")

-- | @h6@ element
h6_ :: Term arg result => arg -> result
h6_ = term (Blaze.fromString "h6")

-- | @head@ element
head_ :: Term arg result => arg -> result
head_ = term (Blaze.fromString "head")

-- | @header@ element
header_ :: Term arg result => arg -> result
header_ = term (Blaze.fromString "header")

-- | @hgroup@ element
hgroup_ :: Term arg result => arg -> result
hgroup_ = term (Blaze.fromString "hgroup")

-- | @hr@ element
hr_ :: Monad m => [(Text,Text)] -> HtmlT m ()
hr_ = with (makeElementNoEnd (Blaze.fromString "hr"))

-- | @html@ element
html_ :: Term arg result => arg -> result
html_ = term (Blaze.fromString "html")

-- | @i@ element
i_ :: Term arg result => arg -> result
i_ = term (Blaze.fromString "i")

-- | @iframe@ element
iframe_ :: Term arg result => arg -> result
iframe_ = term (Blaze.fromString "iframe")

-- | @img@ element
img_ :: Monad m => [(Text,Text)] -> HtmlT m ()
img_ = with (makeElementNoEnd (Blaze.fromString "img"))

-- | @input@ element
input_ :: Monad m => [(Text,Text)] -> HtmlT m ()
input_ = with (makeElementNoEnd (Blaze.fromString "input"))

-- | @ins@ element
ins_ :: Term arg result => arg -> result
ins_ = term (Blaze.fromString "ins")

-- | @kbd@ element
kbd_ :: Term arg result => arg -> result
kbd_ = term (Blaze.fromString "kbd")

-- | @keygen@ element
keygen_ :: Monad m => [(Text,Text)] -> HtmlT m ()
keygen_ = with (makeElementNoEnd (Blaze.fromString "keygen"))

-- | @label@ element or @label@ attribute
label_ :: Term arg result => arg -> result
label_ = term (Blaze.fromString "label")

-- | @legend@ element
legend_ :: Term arg result => arg -> result
legend_ = term (Blaze.fromString "legend")

-- | @li@ element
li_ :: Term arg result => arg -> result
li_ = term (Blaze.fromString "li")

-- | @link@ element
link_ :: Monad m => [(Text,Text)] -> HtmlT m ()
link_ = with (makeElementNoEnd (Blaze.fromString "link"))

-- | @map@ element
map_ :: Term arg result => arg -> result
map_ = term (Blaze.fromString "map")

-- | @mark@ element
mark_ :: Term arg result => arg -> result
mark_ = term (Blaze.fromString "mark")

-- | @menu@ element
menu_ :: Term arg result => arg -> result
menu_ = term (Blaze.fromString "menu")

-- | @menuitem@ element
menuitem_ :: Monad m => [(Text,Text)] -> HtmlT m ()
menuitem_ = with (makeElementNoEnd (Blaze.fromString "menuitem"))

-- | @meta@ element
meta_ :: Monad m => [(Text,Text)] -> HtmlT m ()
meta_ = with (makeElementNoEnd (Blaze.fromString "meta"))

-- | @meter@ element
meter_ :: Term arg result => arg -> result
meter_ = term (Blaze.fromString "meter")

-- | @nav@ element
nav_ :: Term arg result => arg -> result
nav_ = term (Blaze.fromString "nav")

-- | @noscript@ element
noscript_ :: Term arg result => arg -> result
noscript_ = term (Blaze.fromString "noscript")

-- | @object@ element
object_ :: Term arg result => arg -> result
object_ = term (Blaze.fromString "object")

-- | @ol@ element
ol_ :: Term arg result => arg -> result
ol_ = term (Blaze.fromString "ol")

-- | @optgroup@ element
optgroup_ :: Term arg result => arg -> result
optgroup_ = term (Blaze.fromString "optgroup")

-- | @option@ element
option_ :: Term arg result => arg -> result
option_ = term (Blaze.fromString "option")

-- | @output@ element
output_ :: Term arg result => arg -> result
output_ = term (Blaze.fromString "output")

-- | @p@ element
p_ :: Term arg result => arg -> result
p_ = term (Blaze.fromString "p")

-- | @param@ element
param_ :: Monad m => [(Text,Text)] -> HtmlT m ()
param_ = with (makeElementNoEnd (Blaze.fromString "param"))

-- | @pre@ element
pre_ :: Term arg result => arg -> result
pre_ = term (Blaze.fromString "pre")

-- | @progress@ element
progress_ :: Term arg result => arg -> result
progress_ = term (Blaze.fromString "progress")

-- | @q@ element
q_ :: Term arg result => arg -> result
q_ = term (Blaze.fromString "q")

-- | @rp@ element
rp_ :: Term arg result => arg -> result
rp_ = term (Blaze.fromString "rp")

-- | @rt@ element
rt_ :: Term arg result => arg -> result
rt_ = term (Blaze.fromString "rt")

-- | @ruby@ element
ruby_ :: Term arg result => arg -> result
ruby_ = term (Blaze.fromString "ruby")

-- | @samp@ element
samp_ :: Term arg result => arg -> result
samp_ = term (Blaze.fromString "samp")

-- | @script@ element
script_ :: TermRaw arg result => arg -> result
script_ = termRaw (Blaze.fromString "script")

-- | @section@ element
section_ :: Term arg result => arg -> result
section_ = term (Blaze.fromString "section")

-- | @select@ element
select_ :: Term arg result => arg -> result
select_ = term (Blaze.fromString "select")

-- | @small@ element
small_ :: Term arg result => arg -> result
small_ = term (Blaze.fromString "small")

-- | @source@ element
source_ :: Monad m => [(Text,Text)] -> HtmlT m ()
source_ = with (makeElementNoEnd (Blaze.fromString "source"))

-- | @span@ element or @span@ attribute
span_ :: Term arg result => arg -> result
span_ = term (Blaze.fromString "span")

-- | @strong@ element
strong_ :: Term arg result => arg -> result
strong_ = term (Blaze.fromString "strong")

-- | @style@ element or @style@ attribute
style_ :: TermRaw arg result => arg -> result
style_ = termRaw (Blaze.fromString "style")

-- | @sub@ element
sub_ :: Term arg result => arg -> result
sub_ = term (Blaze.fromString "sub")

-- | @summary@ element or @summary@ attribute
summary_ :: Term arg result => arg -> result
summary_ = term (Blaze.fromString "summary")

-- | @sup@ element
sup_ :: Term arg result => arg -> result
sup_ = term (Blaze.fromString "sup")

-- | @table@ element
table_ :: Term arg result => arg -> result
table_ = term (Blaze.fromString "table")

-- | @tbody@ element
tbody_ :: Term arg result => arg -> result
tbody_ = term (Blaze.fromString "tbody")

-- | @td@ element
td_ :: Term arg result => arg -> result
td_ = term (Blaze.fromString "td")

-- | @textarea@ element
textarea_ :: Term arg result => arg -> result
textarea_ = term (Blaze.fromString "textarea")

-- | @tfoot@ element
tfoot_ :: Term arg result => arg -> result
tfoot_ = term (Blaze.fromString "tfoot")

-- | @th@ element
th_ :: Term arg result => arg -> result
th_ = term (Blaze.fromString "th")

-- | @thead@ element
thead_ :: Term arg result => arg -> result
thead_ = term (Blaze.fromString "thead")

-- | @time@ element
time_ :: Term arg result => arg -> result
time_ = term (Blaze.fromString "time")

-- | @title@ element or @title@ attribute
title_ :: Term arg result => arg -> result
title_ = term (Blaze.fromString "title")

-- | @tr@ element
tr_ :: Term arg result => arg -> result
tr_ = term (Blaze.fromString "tr")

-- | @track@ element
track_ :: Monad m => [(Text,Text)] -> HtmlT m ()
track_ = with (makeElementNoEnd (Blaze.fromString "track"))

-- | @ul@ element
ul_ :: Term arg result => arg -> result
ul_ = term (Blaze.fromString "ul")

-- | @var@ element
var_ :: Term arg result => arg -> result
var_ = term (Blaze.fromString "var")

-- | @video@ element
video_ :: Term arg result => arg -> result
video_ = term (Blaze.fromString "video")

-- | @wbr@ element
wbr_ :: Monad m => [(Text,Text)] -> HtmlT m ()
wbr_ = with (makeElementNoEnd (Blaze.fromString "wbr"))

-- | The @accept@ attribute.
accept_ :: Text -> (Text,Text)
accept_ = (,) ("accept")

-- | The @acceptCharset@ attribute.
acceptCharset_ :: Text -> (Text,Text)
acceptCharset_ = (,) ("acceptCharset")

-- | The @accesskey@ attribute.
accesskey_ :: Text -> (Text,Text)
accesskey_ = (,) ("accesskey")

-- | The @action@ attribute.
action_ :: Text -> (Text,Text)
action_ = (,) ("action")

-- | The @alt@ attribute.
alt_ :: Text -> (Text,Text)
alt_ = (,) ("alt")

-- | The @async@ attribute.
async_ :: Text -> (Text,Text)
async_ = (,) ("async")

-- | The @autocomplete@ attribute.
autocomplete_ :: Text -> (Text,Text)
autocomplete_ = (,) ("autocomplete")

-- | The @autofocus@ attribute.
autofocus_ :: (Text,Text)
autofocus_ = (,) ("autofocus") mempty

-- | The @autoplay@ attribute.
autoplay_ :: Text -> (Text,Text)
autoplay_ = (,) ("autoplay")

-- | The @challenge@ attribute.
challenge_ :: Text -> (Text,Text)
challenge_ = (,) ("challenge")

-- | The @charset@ attribute.
charset_ :: Text -> (Text,Text)
charset_ = (,) ("charset")

-- | The @checked@ attribute.
checked_ :: (Text,Text)
checked_ = (,) ("checked") mempty

-- | The @class@ attribute.
class_ :: Text -> (Text,Text)
class_ = (,) ("class")

-- | The @cols@ attribute.
cols_ :: Text -> (Text,Text)
cols_ = (,) ("cols")

-- | The @colspan@ attribute.
colspan_ :: Text -> (Text,Text)
colspan_ = (,) ("colspan")

-- | The @content@ attribute.
content_ :: Text -> (Text,Text)
content_ = (,) ("content")

-- | The @contenteditable@ attribute.
contenteditable_ :: Text -> (Text,Text)
contenteditable_ = (,) ("contenteditable")

-- | The @contextmenu@ attribute.
contextmenu_ :: Text -> (Text,Text)
contextmenu_ = (,) ("contextmenu")

-- | The @controls@ attribute.
controls_ :: Text -> (Text,Text)
controls_ = (,) ("controls")

-- | The @coords@ attribute.
coords_ :: Text -> (Text,Text)
coords_ = (,) ("coords")

-- | The @data@ attribute.
data_ :: Text -> (Text,Text)
data_ = (,) ("data")

-- | The @datetime@ attribute.
datetime_ :: Text -> (Text,Text)
datetime_ = (,) ("datetime")

-- | The @defer@ attribute.
defer_ :: Text -> (Text,Text)
defer_ = (,) ("defer")

-- | The @dir@ attribute.
dir_ :: Text -> (Text,Text)
dir_ = (,) ("dir")

-- | The @disabled@ attribute.
disabled_ :: Text -> (Text,Text)
disabled_ = (,) ("disabled")

-- | The @draggable@ attribute.
draggable_ :: Text -> (Text,Text)
draggable_ = (,) ("draggable")

-- | The @enctype@ attribute.
enctype_ :: Text -> (Text,Text)
enctype_ = (,) ("enctype")

-- | The @for@ attribute.
for_ :: Text -> (Text,Text)
for_ = (,) ("for")

-- | The @formaction@ attribute.
formaction_ :: Text -> (Text,Text)
formaction_ = (,) ("formaction")

-- | The @formenctype@ attribute.
formenctype_ :: Text -> (Text,Text)
formenctype_ = (,) ("formenctype")

-- | The @formmethod@ attribute.
formmethod_ :: Text -> (Text,Text)
formmethod_ = (,) ("formmethod")

-- | The @formnovalidate@ attribute.
formnovalidate_ :: Text -> (Text,Text)
formnovalidate_ = (,) ("formnovalidate")

-- | The @formtarget@ attribute.
formtarget_ :: Text -> (Text,Text)
formtarget_ = (,) ("formtarget")

-- | The @headers@ attribute.
headers_ :: Text -> (Text,Text)
headers_ = (,) ("headers")

-- | The @height@ attribute.
height_ :: Text -> (Text,Text)
height_ = (,) ("height")

-- | The @hidden@ attribute.
hidden_ :: Text -> (Text,Text)
hidden_ = (,) ("hidden")

-- | The @high@ attribute.
high_ :: Text -> (Text,Text)
high_ = (,) ("high")

-- | The @href@ attribute.
href_ :: Text -> (Text,Text)
href_ = (,) ("href")

-- | The @hreflang@ attribute.
hreflang_ :: Text -> (Text,Text)
hreflang_ = (,) ("hreflang")

-- | The @httpEquiv@ attribute.
httpEquiv_ :: Text -> (Text,Text)
httpEquiv_ = (,) ("httpEquiv")

-- | The @icon@ attribute.
icon_ :: Text -> (Text,Text)
icon_ = (,) ("icon")

-- | The @id@ attribute.
id_ :: Text -> (Text,Text)
id_ = (,) ("id")

-- | The @ismap@ attribute.
ismap_ :: Text -> (Text,Text)
ismap_ = (,) ("ismap")

-- | The @item@ attribute.
item_ :: Text -> (Text,Text)
item_ = (,) ("item")

-- | The @itemprop@ attribute.
itemprop_ :: Text -> (Text,Text)
itemprop_ = (,) ("itemprop")

-- | The @keytype@ attribute.
keytype_ :: Text -> (Text,Text)
keytype_ = (,) ("keytype")

-- | The @lang@ attribute.
lang_ :: Text -> (Text,Text)
lang_ = (,) ("lang")

-- | The @list@ attribute.
list_ :: Text -> (Text,Text)
list_ = (,) ("list")

-- | The @loop@ attribute.
loop_ :: Text -> (Text,Text)
loop_ = (,) ("loop")

-- | The @low@ attribute.
low_ :: Text -> (Text,Text)
low_ = (,) ("low")

-- | The @manifest@ attribute.
manifest_ :: Text -> (Text,Text)
manifest_ = (,) ("manifest")

-- | The @max@ attribute.
max_ :: Text -> (Text,Text)
max_ = (,) ("max")

-- | The @maxlength@ attribute.
maxlength_ :: Text -> (Text,Text)
maxlength_ = (,) ("maxlength")

-- | The @media@ attribute.
media_ :: Text -> (Text,Text)
media_ = (,) ("media")

-- | The @method@ attribute.
method_ :: Text -> (Text,Text)
method_ = (,) ("method")

-- | The @min@ attribute.
min_ :: Text -> (Text,Text)
min_ = (,) ("min")

-- | The @multiple@ attribute.
multiple_ :: Text -> (Text,Text)
multiple_ = (,) ("multiple")

-- | The @name@ attribute.
name_ :: Text -> (Text,Text)
name_ = (,) ("name")

-- | The @novalidate@ attribute.
novalidate_ :: Text -> (Text,Text)
novalidate_ = (,) ("novalidate")

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: Text -> (Text,Text)
onbeforeonload_ = (,) ("onbeforeonload")

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: Text -> (Text,Text)
onbeforeprint_ = (,) ("onbeforeprint")

-- | The @onblur@ attribute.
onblur_ :: Text -> (Text,Text)
onblur_ = (,) ("onblur")

-- | The @oncanplay@ attribute.
oncanplay_ :: Text -> (Text,Text)
oncanplay_ = (,) ("oncanplay")

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: Text -> (Text,Text)
oncanplaythrough_ = (,) ("oncanplaythrough")

-- | The @onchange@ attribute.
onchange_ :: Text -> (Text,Text)
onchange_ = (,) ("onchange")

-- | The @onclick@ attribute.
onclick_ :: Text -> (Text,Text)
onclick_ = (,) ("onclick")

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: Text -> (Text,Text)
oncontextmenu_ = (,) ("oncontextmenu")

-- | The @ondblclick@ attribute.
ondblclick_ :: Text -> (Text,Text)
ondblclick_ = (,) ("ondblclick")

-- | The @ondrag@ attribute.
ondrag_ :: Text -> (Text,Text)
ondrag_ = (,) ("ondrag")

-- | The @ondragend@ attribute.
ondragend_ :: Text -> (Text,Text)
ondragend_ = (,) ("ondragend")

-- | The @ondragenter@ attribute.
ondragenter_ :: Text -> (Text,Text)
ondragenter_ = (,) ("ondragenter")

-- | The @ondragleave@ attribute.
ondragleave_ :: Text -> (Text,Text)
ondragleave_ = (,) ("ondragleave")

-- | The @ondragover@ attribute.
ondragover_ :: Text -> (Text,Text)
ondragover_ = (,) ("ondragover")

-- | The @ondragstart@ attribute.
ondragstart_ :: Text -> (Text,Text)
ondragstart_ = (,) ("ondragstart")

-- | The @ondrop@ attribute.
ondrop_ :: Text -> (Text,Text)
ondrop_ = (,) ("ondrop")

-- | The @ondurationchange@ attribute.
ondurationchange_ :: Text -> (Text,Text)
ondurationchange_ = (,) ("ondurationchange")

-- | The @onemptied@ attribute.
onemptied_ :: Text -> (Text,Text)
onemptied_ = (,) ("onemptied")

-- | The @onended@ attribute.
onended_ :: Text -> (Text,Text)
onended_ = (,) ("onended")

-- | The @onerror@ attribute.
onerror_ :: Text -> (Text,Text)
onerror_ = (,) ("onerror")

-- | The @onfocus@ attribute.
onfocus_ :: Text -> (Text,Text)
onfocus_ = (,) ("onfocus")

-- | The @onformchange@ attribute.
onformchange_ :: Text -> (Text,Text)
onformchange_ = (,) ("onformchange")

-- | The @onforminput@ attribute.
onforminput_ :: Text -> (Text,Text)
onforminput_ = (,) ("onforminput")

-- | The @onhaschange@ attribute.
onhaschange_ :: Text -> (Text,Text)
onhaschange_ = (,) ("onhaschange")

-- | The @oninput@ attribute.
oninput_ :: Text -> (Text,Text)
oninput_ = (,) ("oninput")

-- | The @oninvalid@ attribute.
oninvalid_ :: Text -> (Text,Text)
oninvalid_ = (,) ("oninvalid")

-- | The @onkeydown@ attribute.
onkeydown_ :: Text -> (Text,Text)
onkeydown_ = (,) ("onkeydown")

-- | The @onkeyup@ attribute.
onkeyup_ :: Text -> (Text,Text)
onkeyup_ = (,) ("onkeyup")

-- | The @onload@ attribute.
onload_ :: Text -> (Text,Text)
onload_ = (,) ("onload")

-- | The @onloadeddata@ attribute.
onloadeddata_ :: Text -> (Text,Text)
onloadeddata_ = (,) ("onloadeddata")

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: Text -> (Text,Text)
onloadedmetadata_ = (,) ("onloadedmetadata")

-- | The @onloadstart@ attribute.
onloadstart_ :: Text -> (Text,Text)
onloadstart_ = (,) ("onloadstart")

-- | The @onmessage@ attribute.
onmessage_ :: Text -> (Text,Text)
onmessage_ = (,) ("onmessage")

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> (Text,Text)
onmousedown_ = (,) ("onmousedown")

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> (Text,Text)
onmousemove_ = (,) ("onmousemove")

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> (Text,Text)
onmouseout_ = (,) ("onmouseout")

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> (Text,Text)
onmouseover_ = (,) ("onmouseover")

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> (Text,Text)
onmouseup_ = (,) ("onmouseup")

-- | The @onmousewheel@ attribute.
onmousewheel_ :: Text -> (Text,Text)
onmousewheel_ = (,) ("onmousewheel")

-- | The @ononline@ attribute.
ononline_ :: Text -> (Text,Text)
ononline_ = (,) ("ononline")

-- | The @onpagehide@ attribute.
onpagehide_ :: Text -> (Text,Text)
onpagehide_ = (,) ("onpagehide")

-- | The @onpageshow@ attribute.
onpageshow_ :: Text -> (Text,Text)
onpageshow_ = (,) ("onpageshow")

-- | The @onpause@ attribute.
onpause_ :: Text -> (Text,Text)
onpause_ = (,) ("onpause")

-- | The @onplay@ attribute.
onplay_ :: Text -> (Text,Text)
onplay_ = (,) ("onplay")

-- | The @onplaying@ attribute.
onplaying_ :: Text -> (Text,Text)
onplaying_ = (,) ("onplaying")

-- | The @onprogress@ attribute.
onprogress_ :: Text -> (Text,Text)
onprogress_ = (,) ("onprogress")

-- | The @onpropstate@ attribute.
onpropstate_ :: Text -> (Text,Text)
onpropstate_ = (,) ("onpropstate")

-- | The @onratechange@ attribute.
onratechange_ :: Text -> (Text,Text)
onratechange_ = (,) ("onratechange")

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: Text -> (Text,Text)
onreadystatechange_ = (,) ("onreadystatechange")

-- | The @onredo@ attribute.
onredo_ :: Text -> (Text,Text)
onredo_ = (,) ("onredo")

-- | The @onresize@ attribute.
onresize_ :: Text -> (Text,Text)
onresize_ = (,) ("onresize")

-- | The @onscroll@ attribute.
onscroll_ :: Text -> (Text,Text)
onscroll_ = (,) ("onscroll")

-- | The @onseeked@ attribute.
onseeked_ :: Text -> (Text,Text)
onseeked_ = (,) ("onseeked")

-- | The @onseeking@ attribute.
onseeking_ :: Text -> (Text,Text)
onseeking_ = (,) ("onseeking")

-- | The @onselect@ attribute.
onselect_ :: Text -> (Text,Text)
onselect_ = (,) ("onselect")

-- | The @onstalled@ attribute.
onstalled_ :: Text -> (Text,Text)
onstalled_ = (,) ("onstalled")

-- | The @onstorage@ attribute.
onstorage_ :: Text -> (Text,Text)
onstorage_ = (,) ("onstorage")

-- | The @onsubmit@ attribute.
onsubmit_ :: Text -> (Text,Text)
onsubmit_ = (,) ("onsubmit")

-- | The @onsuspend@ attribute.
onsuspend_ :: Text -> (Text,Text)
onsuspend_ = (,) ("onsuspend")

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: Text -> (Text,Text)
ontimeupdate_ = (,) ("ontimeupdate")

-- | The @onundo@ attribute.
onundo_ :: Text -> (Text,Text)
onundo_ = (,) ("onundo")

-- | The @onunload@ attribute.
onunload_ :: Text -> (Text,Text)
onunload_ = (,) ("onunload")

-- | The @onvolumechange@ attribute.
onvolumechange_ :: Text -> (Text,Text)
onvolumechange_ = (,) ("onvolumechange")

-- | The @onwaiting@ attribute.
onwaiting_ :: Text -> (Text,Text)
onwaiting_ = (,) ("onwaiting")

-- | The @open@ attribute.
open_ :: Text -> (Text,Text)
open_ = (,) ("open")

-- | The @optimum@ attribute.
optimum_ :: Text -> (Text,Text)
optimum_ = (,) ("optimum")

-- | The @pattern@ attribute.
pattern_ :: Text -> (Text,Text)
pattern_ = (,) ("pattern")

-- | The @ping@ attribute.
ping_ :: Text -> (Text,Text)
ping_ = (,) ("ping")

-- | The @placeholder@ attribute.
placeholder_ :: Text -> (Text,Text)
placeholder_ = (,) ("placeholder")

-- | The @preload@ attribute.
preload_ :: Text -> (Text,Text)
preload_ = (,) ("preload")

-- | The @pubdate@ attribute.
pubdate_ :: Text -> (Text,Text)
pubdate_ = (,) ("pubdate")

-- | The @radiogroup@ attribute.
radiogroup_ :: Text -> (Text,Text)
radiogroup_ = (,) ("radiogroup")

-- | The @readonly@ attribute.
readonly_ :: Text -> (Text,Text)
readonly_ = (,) ("readonly")

-- | The @rel@ attribute.
rel_ :: Text -> (Text,Text)
rel_ = (,) ("rel")

-- | The @required@ attribute.
required_ :: Text -> (Text,Text)
required_ = (,) ("required")

-- | The @reversed@ attribute.
reversed_ :: Text -> (Text,Text)
reversed_ = (,) ("reversed")

-- | The @rows@ attribute.
rows_ :: Text -> (Text,Text)
rows_ = (,) ("rows")

-- | The @rowspan@ attribute.
rowspan_ :: Text -> (Text,Text)
rowspan_ = (,) ("rowspan")

-- | The @sandbox@ attribute.
sandbox_ :: Text -> (Text,Text)
sandbox_ = (,) ("sandbox")

-- | The @scope@ attribute.
scope_ :: Text -> (Text,Text)
scope_ = (,) ("scope")

-- | The @scoped@ attribute.
scoped_ :: Text -> (Text,Text)
scoped_ = (,) ("scoped")

-- | The @seamless@ attribute.
seamless_ :: Text -> (Text,Text)
seamless_ = (,) ("seamless")

-- | The @selected@ attribute.
selected_ :: Text -> (Text,Text)
selected_ = (,) ("selected")

-- | The @shape@ attribute.
shape_ :: Text -> (Text,Text)
shape_ = (,) ("shape")

-- | The @size@ attribute.
size_ :: Text -> (Text,Text)
size_ = (,) ("size")

-- | The @sizes@ attribute.
sizes_ :: Text -> (Text,Text)
sizes_ = (,) ("sizes")

-- | The @spellcheck@ attribute.
spellcheck_ :: Text -> (Text,Text)
spellcheck_ = (,) ("spellcheck")

-- | The @src@ attribute.
src_ :: Text -> (Text,Text)
src_ = (,) ("src")

-- | The @srcdoc@ attribute.
srcdoc_ :: Text -> (Text,Text)
srcdoc_ = (,) ("srcdoc")

-- | The @start@ attribute.
start_ :: Text -> (Text,Text)
start_ = (,) ("start")

-- | The @step@ attribute.
step_ :: Text -> (Text,Text)
step_ = (,) ("step")

-- | The @subject@ attribute.
subject_ :: Text -> (Text,Text)
subject_ = (,) ("subject")

-- | The @tabindex@ attribute.
tabindex_ :: Text -> (Text,Text)
tabindex_ = (,) ("tabindex")

-- | The @target@ attribute.
target_ :: Text -> (Text,Text)
target_ = (,) ("target")

-- | The @type@ attribute.
type_ :: Text -> (Text,Text)
type_ = (,) ("type")

-- | The @usemap@ attribute.
usemap_ :: Text -> (Text,Text)
usemap_ = (,) ("usemap")

-- | The @value@ attribute.
value_ :: Text -> (Text,Text)
value_ = (,) ("value")

-- | The @width@ attribute.
width_ :: Text -> (Text,Text)
width_ = (,) ("width")

-- | The @wrap@ attribute.
wrap_ :: Text -> (Text,Text)
wrap_ = (,) ("wrap")

-- | The @xmlns@ attribute.
xmlns_ :: Text -> (Text,Text)
xmlns_ = (,) ("xmlns")
