
# Hedgehog

### Changes from Haskell

- All integral operations operate on `LargeInt.int`
  * Because SML doesn't have typeclasses, this saves a lot of code duplication
