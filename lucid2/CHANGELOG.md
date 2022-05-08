## 0.0.x

TBA

## New in lucid2

Changed:

* All on* attributes and style_ do not escape their values anymore. Be
  careful. Though you were probably being careful with these anyway,
  as they are inherently dangerous for XXS.

Renamed:

* makeAttribute is renamed to makeAttributes.
* Added makeAttributesRaw.

Dropped:

* Eq/Ord/Show instances for Attribute.
* Drop the `Lucid.Bootstrap` module entirely.
* Dropped the mmorph dependency (breaking changes often, not
  reliable), instead we provide a simple `hoist` function of the right
  type.
* Drop MonadError.
* Drop MonadWriter.
* Drop hashable.
* Drop XML elements.

Dependencies:

* We now only depend on blaze-builder, and the rest are libraries that
  come with GHC, which are held to a slightly higher standard of not
  breaking changes.
