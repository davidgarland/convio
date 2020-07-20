# ConvIO

This module exposes a simple class `ConvIO` that comprises IO and ST, for
when you want to write abstractions over either. This is unsafe, obviously.
That being said, this module does value safety; `unsafeConvIfDet` and
`unsafeConvWhenDet` allow you to do things like only zeroing arrays in the ST
monad, so as to preserve determinism only where it's needed. Additionally,
this library is smaller than its main competitor, which would be the
primitive package with PrimMonad, which has a broader scope.
