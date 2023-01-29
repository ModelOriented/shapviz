# shapviz 0.4.2

## Compatibility with "ggplot2"

- "ggplot2" 3.4 has replaced the "size" aesthetic in line-based geoms by "linewidth". This has been adapted. "shapviz" now depends on ggplot2 >= 3.4.

# shapviz 0.4.1

## New functionality

- Hide "other": `sv_importance()` has received a new argument `show_others = TRUE`. Set to `FALSE` to hide the "other" bar/beeswarm.

# shapviz 0.4.0

## Removed dependencies

The following dependencies have been removed:

- "ggbeeswarm"
- "vipor"
- "beeswarm"

## Changes in `sv_importance()`

- New argument `bee_width`: Relative width of the beeswarms. The default is 0.4. It replaces the `width` argument passed via `...`.
- New argument `bee_adjust`: Relative adjustment factor of the bandwidth used in estimating the density of the beeswarms. Default is 0.5.
- In case a beeswarm is shown: the `...` arguments are now passed to `geom_point()`.

## Improvement with Plotly

- `plotly::ggplotly()` now works for most functionalities of `sv_importance()`, including beeswarms.

# shapviz 0.3.0

## Less picky interface

- The argument `X` of the constructor of `shapviz()` is now less picky. If it contains columns not present in the SHAP matrix, they are silently dropped. Furthermore, the column order of the SHAP matrix and `X` is now determined by the SHAP matrix.

## Removed (according to depreciation cycle)

- Functions `shapviz_from_lgb_predict()` and `shapviz_from_xgb_predict()`
- `format_fun` argument in `sv_force()` and `sv_waterfall()`
- `sort_fun` argument in `sv_waterfall()`

## Minor changes

- `collapse_shap()` is not anymore an S3 method. It is just a normal function that can be applied to a matrix.

# shapviz 0.2.2

## Bug fix

- For R versions < 4.1, `sv_importance()` would return an error.

## Minor improvements

- kernelshap wrapper now also can deal with multioutput models.

# shapviz 0.2.1

## Major improvements

- Added kernelshap wrapper.

## Minor changes

- Removed unnecessary conversion of `X_pred` from `matrix` to `xgb.DMatrix` in `shapviz.xgb.Booster()`.
- Vignette: Added a CatBoost wrapper to the vignette and changed the `treeshap()` example to a `ranger()` model.

## Maintainance

- Fixed CRAN notes on html5.

# shapviz 0.2.0

## Major improvements

- Added H2O wrapper.
- Added shapr wrapper.
- Added an optional `collapse` argument in `shapviz()`. This is named list specifying which columns in the SHAP matrix are to be collapsed by rowwise summation. A typical application will be to combine the SHAP values of one-hot-encoded dummies and explain them by the corrsponding factor variable.
- Major rework of `sv_importance()`, see next section.

## Major rework of `sv_importance()`

The calculations behind `sv_importance()` are unchanged, but defaults and some plot aspects have been reworked.

- Instead of a beeswarm plot, `sv_importance()` now shows a bar plot by default. Use `kind = "beeswarm"` to get a beeswarm plot.
- The bar plot of `sv_importance()` does not show SHAP feature importances as text anymore. Use `show_numbers = TRUE` to get them back. Furthermore, the numbers are now printed on top of the bars instead on their bottom.
- The new argument `show_numbers` can be used to to add SHAP feature importance values for all plot types.
- The default of `max_display` has been increased from 10 to 15.
- The bar width has been reduced from 0.9 to 2/3 relative width. It can be controlled by the new argument `bar_width`.
- The color bar title of the beeswarm plot can now be manually chosen by the new argument `color_bar_title`. Set to `NULL` to remove the color bar altogether.
- The argument `format_fun` now uses a right-aligned number formatter with aligned decimal separator by default.

## Minor changes

- Added `dim()` method for "shapviz" object, implying `nrow()` and `ncol()`.
- To allow more flexible formatting, the `format_fun` argument of `sv_waterfall()` and `sv_force()` has been replaced by `format_shap` to format SHAP values and `format_feat` to format numeric feature values. By default, they use the new global options "shapviz.format_shap" and "shapviz.format_feat", both with default `function(z) prettyNum(z, digits = 3, scientific = FALSE)`.
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
