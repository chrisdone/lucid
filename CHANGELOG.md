## 2.9.8.1

* Improve performance by adding `INLINE` pragmas to `Monad` etc. combinators.

## 2.9.8

* Add `integrity_`, `crossorigin_` attributes
* Add `classes_` smart attribute constructor
* Add `ToHtml (HtmlT m a)` instance

## 2.9.6

* Fix compilation of benchmarks
* Add @athanclark's version of relaxHtmlT
* Add a utility to generalize the underlying monad from Identity

## 2.9.5

* Add ToHtml instance for ByteString (both)

## 2.9.1

* Small performance tweaks.
* Make svg_ an element.

## 2.6

* Restrict monoid instance's a to ~ () (means you can use mempty
  without inference errors)

## 2.2

* Export renderToFile from top-level Lucid module.

## 2.1

* Add some extra HTML tags.

## 2.0

* Use variadic HTML terms.
* Add lazy Text instance for ToHtml.

## 1.0

* Initial version.
