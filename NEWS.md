# shapviz 0.1.1

## Major changes

- Added "shapr" wrapper.
- `sv_waterfall()` now uses a more stringend argument `order_fun = function(s) order(abs(s))` instead of the original `sort_fun = function(shap) abs(shap)` that was then passed to `order()`.

## Minor changes

- Added `dim()` method for "shapviz" object, implying `nrow()` and `ncol()`.
- More unit tests.
- Improved documentation.
- Added H2O section to vignette.
- Fixed github installation instruction in README and vignette.

# shapviz 0.1.0

This is the initial CRAN release.
