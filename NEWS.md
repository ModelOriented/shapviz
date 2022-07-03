# shapviz 0.2.0

## Major change: new default

- `sv_importance()` now shows a bar plot by default, not a beeswarm plot. Use `kind = "beeswarm"` to get a beeswarm plot.

## Other major changes

- Added H2O wrapper.
- Added shapr wrapper.
- Added an optional `collapse` argument in `shapviz()`. This is named list specifying which columns in the SHAP matrix are to be collapsed by rowwise summation. A typical application will be to combine the SHAP values of one-hot-encoded dummies and explain them together.

## Minor changes

- Added `dim()` method for "shapviz" object, implying `nrow()` and `ncol()`.
- To allow more flexible formatting, the `format_fun` argument of `sv_waterfall()` and `sv_force()` has been replaced by `format_shap` to format SHAP values and `format_feat` to format numeric feature values. By default, they use the new global options "shapviz.format_shap" and "shapviz.format_feat", both with default `function(z) prettyNum(z, digits = 3, scientific = FALSE)`.
- To be consistent with above change, the argument `format_fun` in `sv_importance()` was renamed to `format_shap` and the default also points to the global option `shapviz.format_shap`.
- `sv_waterfall()` now uses the more consistent argument `order_fun = function(s) order(abs(s))` instead of the original `sort_fun = function(shap) abs(shap)` that was then passed to `order()`.
- Added argument `viridis_args = getOption("shapviz.viridis_args")` to `sv_dependence()` and `sv_importance()` to control the viridis color scale options. The default global option equals `list(begin = 0.25, end = 0.85, option = "inferno")`. For example, to switch to a standard viridis scale, you can either change the default with `options(shapviz.viridis_args = NULL)` 
or set `viridis_args = NULL`.
- Deprecated helper functions `shapviz_from_lgb_predict()` and `shapviz_from_xgb_predict` in favour of the collapsing logic (see above). The functions will be removed in version 0.3.0.
- Added 'lightgbm' as "Enhances" dependency.
- Added 'h2o' as "Enhances" dependency.
- Anticipated changes in `predict()` arguments of LightGBM (data -> newdata, predcontrib = TRUE -> type = "contrib").
- More unit tests.
- Improved documentation.
- Fixed github installation instruction in README and vignette.

# shapviz 0.1.0

This is the initial CRAN release.
