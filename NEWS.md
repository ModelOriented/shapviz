# shapviz 0.9.4

## API improvements

- Support both XGBoost 1.x.x as well as XGBoost 2.x.x, implemented in #144.

## Improvements

- New argument `sort_features = TRUE` in `sv_importance()` and `sv_interaction()`. Set to `FALSE` to show the features as they appear in your SHAP matrix. In that case, the plots will show the *first* `max_display` features, not the *most important* features. Implements #136.

### Bug fixes

- `shapviz.xgboost()` would fail if a single row is passed. This has been fixed in #142. Thanks @sebsilas for reporting.

# shapviz 0.9.3

## `sv_dependence()`: Control over automatic color feature selection

### How is the color feature selected, anyway?

If no SHAP interaction values are available, by default, the color feature `v'` is selected by the heuristic `potential_interaction()`, which works as follows:

1. If the feature `v` (the on the x-axis) is numeric, it is binned into `nbins` bins.
2. Per bin, the SHAP values of `v` are regressed onto `v'` and the R-squared is calculated. Rows with missing `v'` are discarded.
3. The R-squared are averaged over bins, weighted by the number of non-missing `v'` values.

This measures how much variability in the SHAP values of `v` is explained by `v'`, after accounting for `v`.

We have introduced four parameters to control the heuristic. Their defaults are in line with the old behaviour.

- `nbin = NULL`: Into how many quantile bins should a numeric `v` be binned? The default `NULL` equals the smaller of $n/20$ and $\sqrt n$ (rounded up), where $n$ is the sample size.
- `color_num` Should color features be converted to numeric, even if they are factors/characters? Default is `TRUE`.
- `scale = FALSE`: Should R-squared be multiplied with the sample variance of
within-bin SHAP values? If `TRUE`, bins with stronger vertical scatter will get higher weight. The default is `FALSE`.
- `adjusted = FALSE`: Should *adjusted* R-squared be calculated?

If SHAP interaction values are available, these parameters have no effect. In `sv_dependence()` they are called `ih_nbin` etc.

This partly implements the ideas in [#119](https://github.com/ModelOriented/shapviz/issues/119) of Roel Verbelen, thanks a lot for your patient explanations!

### Further plans?

We will continue to experiment with the defaults, which might change in the future. A good alternative to the current (naive) defaults could be:

- `nbins = 7`: Smaller than now to not overfit too strongly with factor/character color features.
- `color_num = FALSE`: To not naively integer encode factors/characters. 
- `scale = TRUE`: To account for non-equal spread in bins.
- `adjusted = TRUE`: To not put too much weight on factors with many categories.

## Other user-visible changes

- `sv_dependence()`: If `color_var = "auto"` (default) and no color feature seems to be relevant (SHAP interaction is `NULL`, or heuristic returns no positive value), there won't be any color scale. Furthermore, in some edge cases, a different
color feature might be selected.
- `mshapviz()` objects can now be rowbinded via `rbind()` or `+`. Implemented by [@jmaspons](https://github.com/jmaspons) in [#110](https://github.com/ModelOriented/shapviz/pull/110).
- `mshapviz()` is more strict when combining multiple "shapviz" objects. These now need to have identical column names, see [#114](https://github.com/ModelOriented/shapviz/pull/114).

## Small changes

- The README is shorter and easier.
- Updated vignettes.
- `print.shapviz()` now shows top two rows of SHAP matrix.
- Re-activate all unit tests.
- Setting `nthread = 1` in all calls to `xgb.DMatrix()` as suggested by [@jmaspons](https://github.com/jmaspons) in [#109](https://github.com/ModelOriented/shapviz/issues/109).
- Added "How to contribute" to README.
- `permshap()` connector is now part of {kerneshap} [#122](https://github.com/ModelOriented/shapviz/pull/122).

## Bug fixes

- `sv_dependence2D()`: In case `add_vars` are passed, `x` and/or `y` are removed from it in order to not use any variable twice. [#116](https://github.com/ModelOriented/shapviz/pull/116).
- `split.shapviz()` now drops empty levels. They launched an error because empty "shapviz" objects are currently not supported. [#117](https://github.com/ModelOriented/shapviz/pull/117), [#118](https://github.com/ModelOriented/shapviz/pull/118)

# shapviz 0.9.2

## User-visible changes

- `sv_importance()` of a "mshapviz" object now returns a dodged barplot instead of separate barplots via {patchwork}. Use the new argument `bar_type` to switch to a stacked barplot (`bar_type = "stack"`), to "facets" (via {ggplot2}), or "separate" for the old behaviour.

## New features

- Added connector to [permshap](https://github.com/mayer79/permshap), a package calculating permutation SHAP values for regression and (probabilistic) classification.

## Other changes

- Revised vignette on "mshapviz".
- Commenting out most unit tests as they would not pass timings measured on Debian.

# shapviz 0.9.1

## New features

- `dimnames.shapviz()` has received a replacement method. You can thus change the column names of SHAP matrix and feature data (as well as SHAP interactions) by `colnames(x) <- ...`, see https://github.com/ModelOriented/shapviz/issues/98

## Maintenance

- Fix for https://github.com/ModelOriented/shapviz/issues/100 (`package_version()` applied to numeric value will be deprecated in the future)

# shapviz 0.9.0

## New features

- New plot function `sv_dependence2D()`: x and y coordinates are two features, while their summed SHAP values are shown on the color scale. If `interaction = TRUE`, SHAP interaction values are shown on the color scale instead. The function is vectorized in `x` and/or `y`. This visualization is especially useful for models with geographic components.
- `split(x, f)` splits a "shapviz" object `x` into a "mshapviz" object.

## Documentation

- Slight improvements in help/docu.
- New vignette on models with geographic components.
- Added a fantastic house price dataset with about 14,000 houses sold in Miami-Date County, thanks Steven C. Bourassa.

## API improvements

- "mshapviz" object created from multioutput "kernelshap" object retains names.

# shapviz 0.8.0

## API improvement

- For (upcoming) {fastshap} version >0.0.7, `fastshap::explain()` offers the option `shap_only`. To conveniently construct the "shapviz" object, use `shapviz(fastshap::explain(..., shap_only = FALSE))`. This not only passes the SHAP matrix but also the feature data and the baseline. Thanks, Brandon Greenwell!

## Documentation

- Better help files
- Switched from "import ggplot2" to "ggplot2::function" code style
- Vignette "Multiple 'shapviz' objects": Fixed mistake in Random Forest + Kernel SHAP example

# shapviz 0.7.0

## Milestone: Working with multiple 'shapviz' objects

Sometimes, you will find it necessary to work with several "shapviz" objects at the same time:

- To visualize SHAP values of a multiclass or multi-output model.
- To compare SHAP plots of different models.
- To compare SHAP plots between subgroups.

To simplify the workflow, {shapviz} introduces the "mshapviz" object ("m" like "multi"). You can create it in different ways:

- Use `shapviz()` on multiclass XGBoost or LightGBM models.
- Use `shapviz()` on "kernelshap" objects created from multiclass/multioutput models.
- Use `c(Mod_1 = s1, Mod_2 = s2, ...)` on "shapviz" objects `s1`, `s2`, ...
- Or `mshapviz(list(Mod_1 = s1, Mod_2 = s2, ...))`

The `sv_*()` functions use the {patchwork} package to glue the individual plots together.

See the new vignette for more info and specific examples.

## Other new features

- `sv_dependence()` now allows multiple `v` and/or `color_var` to be plotted (glued via {patchwork}).
- {DALEX}: Support for "predict_parts" objects from {DALEX}, thanks to Adrian Stando.
- Aggregated SHAP values: The argument `row_id` of `sv_waterfall()` and `sv_force()` now also allows a vector of integers or a logical vector. If more than one row is selected, SHAP values and predictions are averaged before plotting (*aggregated SHAP values* in {DALEX}).
- Row bind: "shapviz" objects `x1`, `x2` can now be concatenated in rowwise manner using `x1 + x2` or `rbind(x1, x2)`, again thanks to Adrian.
- `colnames()`: "shapviz" objects `x` have received a `dimnames()` function, so you can now, e.g., use `colnames(x)` to see the feature names.
- Subsetting: "shapviz" `x` can now be subsetted using `x[cond, features]`.

## Maintenance

- We have a new contributor: Adrian Stando - welcome on the SHAP board.
- To be close to my sister package {kernelshap}, I have moved to https://github.com/ModelOriented/shapviz
- Webpage created with "pgkdown"
- New dependency: {patchwork}

## Other changes

- Color guides are closer to the plot area. This affects `sv_dependence()`, `sv_importance(kind="bee")`, and `sv_interaction()`.
- The lengthy y axis title "SHAP interaction value" in `sv_dependence()` has been shortened to "SHAP interaction".
- As announced, the argument `show_other` of `sv_importance()` has been removed.
- Slightly less picky checks on `S_inter`.
- `print.shapviz()` is much more compact, use `summary.shapviz()` for more info.

## Bug fixes

- `sv_waterfall()`: Using `order_fun()` would not work as expected with `max_display`. This has been fixed.
- `sv_dependence()`: Passing `viridis_args = NULL` would hide the color guide title. This has been fixed. But please pass `viridis_args = list()` instead.

# shapviz 0.6.0

## Change in defaults

- `sv_dependence()` now uses `color_var = "auto"` instead of `color_var = NULL`.
- `sv_dependence()` now uses "SHAP value" as y label (instead of the more verbose "SHAP value of [feature]").

# shapviz 0.5.0

## Major improvement: SHAP interaction values

- Introduced API for SHAP interaction values `S_inter` (3D array):
    - Matrix method: `shapviz(object, ..., S_inter = NULL)`
    - XGBoost method: `shapviz(object, ..., interactions = TRUE)`
    - treeshap method: `shapviz(object, ...)`
- `sv_interaction(x)` shows matrix of beeswarm plots.
- `sv_dependence(x, v = "x1", color_var = "x2", interactions = TRUE)` plots SHAP interaction values.
- `sv_dependence(x, v = "x1", interactions = TRUE)` plots pure main effects of "x1".
- If SHAP interaction values are available, `sv_dependence(..., color_var = "auto")` uses those to determine the most interacting color variable.
- `collapse_shap()` also works for SHAP interaction arrays.
- SHAP interaction values can be extracted by `get_shap_interactions()`.

## User visible changes

- `sv_importance()`: In case of too many features, `sv_importance()` used to collapse the remaining features into an additional bar/beeswarm. This logic has been removed, and the `show_other` argument has been deprecated.
- By default, `sv_dependence()` automatically adds horizontal jitter for discrete `v`. This now also works if `v` is numeric with at most seven unique values, not only for logicals, factors, and character `v`.

## Compatibility with "ggplot2"

- "ggplot2" 3.4 has replaced the "size" aesthetic in line-based geoms by "linewidth". This has been adapted. "shapviz" now depends on ggplot2 >= 3.4.

## Technical changes

- `sv_importance()` does not use a flipped coordinate system anymore.

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
