# shapviz 0.1.1

## Major changes

- Added "shapr" wrapper.
- `sv_waterfall()` now uses a more stringend argument `order_fun = function(s) order(abs(s))` instead of the original `sort_fun = function(shap) abs(shap)` that was then passed to `order()`.

## Minor changes

- Added `dim()` method for "shapviz" object, implying `nrow()` and `ncol()`.
- Added argument `viridis_args = getOption("shapviz.viridis_args")` to `sv_dependence()` and `sv_importance()` to control the viridis color scale options. The default global option equals `list(begin = 0.25, end = 0.85, option = "inferno")`. For example, to switch to a reverted standard viridis scale, you can adapt the global option or set `viridis_args = list(option = "viridis", direction = -1)`.
- More unit tests.
- Improved documentation.
- Added H2O section to vignette.
- Fixed github installation instruction in README and vignette.

# shapviz 0.1.0

This is the initial CRAN release.
