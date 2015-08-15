1. Prelude useful
2. Prelude type-class method, not exported (Foldable#fold *appears to be exceptional*)
3. Implemented better in base
  * more general e.g. `concat`, `(.)`
  * safe e.g. `listToMaybe` opposed to `head`
4. Implemented better elsewhere (e.g. semigroupoids)
  * more general e.g. `(<*>)`, `sequenceA`
  * safe e.g. `foldl1`, `maximum` 
5. Unsafe (eliminate)
  * `head`, `fromJust`, `tail`, `!!`, `init`, `last`, `read`

----

Packages that combine the above in various useful ways

* `prelim-core`: `[1, 2]`
* `prelim-base`: `[1, 2, 3]`
* `prelim-extra-semigroupoids`
* `prelim-extra-lens`
* `prelim-extra` *all of the above*

