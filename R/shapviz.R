#' Initialize "shapviz" Object
#'
#' @description
#' This function creates an object of class "shapviz" from a matrix of SHAP values, or
#' from a fitted model of type
#' - XGBoost,
#' - LightGBM, or
#' - H2O (tree-based regression or binary classification model).
#'
#' Furthermore, [shapviz()] can digest the results of
#' - `fastshap::explain()`,
#' - `shapr::explain()`,
#' - `treeshap::treeshap()`,
#' - `DALEX::predict_parts()`,
#' - `kernelshap::kernelshap()`, and
#' - `kernelshap::permshap()`,
#'
#' check the vignettes for examples.
#'
#' @details
#' Together with the main input, a data set `X` of feature values is required,
#' used only for visualization. It can therefore contain character or factor
#' variables, even if the SHAP values were calculated from a purely numerical feature
#' matrix. In addition, to improve visualization, it can sometimes be useful to truncate
#' gross outliers, logarithmize certain columns, or replace missing values with an
#' explicit value.
#'
#' SHAP values of dummy variables can be combined using the convenient
#' `collapse` argument.
#' Multi-output models created from XGBoost, LightGBM, "kernelshap", or "permshap"
#' return a "mshapviz" object, containing a "shapviz" object per output.
#'
#' @inheritParams collapse_shap
#' @param object For XGBoost, LightGBM, and H2O, this is the fitted model used to
#'   calculate SHAP values from `X_pred`.
#'   In the other cases, it is the object containing the SHAP values.
#' @param X Matrix or data.frame of feature values used for visualization.
#'   Must contain at least the same column names as the SHAP matrix represented by
#'   `object`/`X_pred` (after optionally collapsing some of the SHAP columns).
#' @param X_pred Data set as expected by the `predict()` function of
#'   XGBoost, LightGBM, or H2O. For XGBoost, a matrix or `xgb.DMatrix`,
#'   for LightGBM a matrix, and for H2O a `data.frame` or an `H2OFrame`.
#'   Only used for XGBoost, LightGBM, or H2O objects.
#' @param baseline Optional baseline value, representing the average response at the
#'   scale of the SHAP values. It will be used for plot methods that explain single
#'   predictions.
#' @param which_class In case of a multiclass or multioutput setting,
#'   which class/output (>= 1) to explain. Currently relevant for XGBoost, LightGBM,
#'   kernelshap, and permshap.
#' @param interactions Should SHAP interactions be calculated (default is `FALSE`)?
#'   Only available for XGBoost.
#' @param S_inter Optional 3D array of SHAP interaction values.
#'   If `object` has shape n x p, then `S_inter` needs to be of
#'   shape n x p x p. Summation over the second (or third) dimension should yield the
#'   usual SHAP values. Furthermore, dimensions 2 and 3 are expected to be symmetric.
#'   Default is `NULL`.
#' @param ... Parameters passed to other methods (currently only used by
#'   the `predict()` functions of XGBoost, LightGBM, and H2O).
#' @returns
#'   An object of class "shapviz" with the following elements:
#'   - `S`: Numeric matrix of SHAP values.
#'   - `X`: `data.frame` containing the feature values corresponding to `S`.
#'   - `baseline`: Baseline value, representing the average prediction at the
#'     scale of the SHAP values.
#'   - `S_inter`: Numeric array of SHAP interaction values (or `NULL`).
#' @seealso
#'   [sv_importance()], [sv_dependence()], [sv_dependence2D()], [sv_interaction()],
#'   [sv_waterfall()], [sv_force()], [collapse_shap()]
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shapviz(S, X, baseline = 4)
#' @export
shapviz <- function(object, ...){
  UseMethod("shapviz")
}

#' @describeIn shapviz
#'   Default method to initialize a "shapviz" object.
#' @export
shapviz.default = function(object, ...) {
  stop("No default method available.")
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from a matrix of SHAP values.
#' @export
shapviz.matrix = function(object, X, baseline = 0, collapse = NULL,
                          S_inter = NULL, ...) {
  if (!is.null(collapse)) {
    object <- collapse_shap(object, collapse = collapse)
    if (!is.null(S_inter)) {
      S_inter <- collapse_shap(S_inter, collapse = collapse)
    }
  }
  .input_checks(object = object, X = X, baseline = baseline, S_inter = S_inter)

  # Select and align columns according to SHAP matrix ('object')
  nms <- colnames(object)
  X <- as.data.frame(X)[nms]
  if (!is.null(S_inter)) {
    S_inter <- S_inter[, nms, nms, drop = FALSE]
  }

  structure(
    list(S = object, X = X, baseline = baseline, S_inter = S_inter), class = "shapviz"
  )
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from an XGBoost model.
#' @export
#' @examples
#' # XGBoost models
#' X_pred <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_pred, label = iris[, 1], nthread = 1)
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#'
#' # Will use numeric matrix "X_pred" as feature matrix
#' x <- shapviz(fit, X_pred = X_pred)
#' x
#' sv_dependence(x, "Species")
#'
#' # Will use original values as feature matrix
#' x <- shapviz(fit, X_pred = X_pred, X = iris)
#' sv_dependence(x, "Species")
#'
#' # "X_pred" can also be passed as xgb.DMatrix, but only if X is passed as well!
#' x <- shapviz(fit, X_pred = dtrain, X = iris)
#'
#' # Multiclass setting
#' params <- list(objective = "multi:softprob", num_class = 3)
#' X_pred <- data.matrix(iris[, -5])
#' dtrain <- xgboost::xgb.DMatrix(
#'   X_pred, label = as.integer(iris[, 5]) - 1, nthread = 1
#' )
#' fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 10, nthread = 1)
#'
#' # Select specific class
#' x <- shapviz(fit, X_pred = X_pred, which_class = 3)
#' x
#'
#' # Or combine all classes to "mshapviz" object
#' x <- shapviz(fit, X_pred = X_pred)
#' x
#'
#' # What if we would have one-hot-encoded values and want to explain the original column?
#' X_pred <- stats::model.matrix(~ . -1, iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_pred, label = as.integer(iris[, 1]), nthread = 1)
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' x <- shapviz(
#'   fit,
#'   X_pred = X_pred,
#'   X = iris,
#'   collapse = list(Species = c("Speciessetosa", "Speciesversicolor", "Speciesvirginica"))
#' )
#' summary(x)
#'
#' # Similarly with LightGBM
#' if (requireNamespace("lightgbm", quietly = TRUE)) {
#'   fit <- lightgbm::lgb.train(
#'     params = list(objective = "regression", num_thread = 1),
#'     data = lightgbm::lgb.Dataset(X_pred, label = iris[, 1]),
#'     nrounds = 10,
#'     verbose = -2
#'   )
#'
#'   x <- shapviz(fit, X_pred = X_pred)
#'   x
#'
#'   # Multiclass
#'   params <- list(objective = "multiclass", num_class = 3, num_thread = 1)
#'   X_pred <- data.matrix(iris[, -5])
#'   dtrain <- lightgbm::lgb.Dataset(X_pred, label = as.integer(iris[, 5]) - 1)
#'   fit <- lightgbm::lgb.train(params = params, data = dtrain, nrounds = 10)
#'
#'   # Select specific class
#'   x <- shapviz(fit, X_pred = X_pred, which_class = 3)
#'   x
#'
#'   # Or combine all classes to a "mshapviz" object
#'   mx <- shapviz(fit, X_pred = X_pred)
#'   mx
#'   all.equal(mx[[3]], x)
#' }
shapviz.xgb.Booster = function(object, X_pred, X = X_pred, which_class = NULL,
                               collapse = NULL, interactions = FALSE, ...) {
  stopifnot(
    "X must be a matrix or data.frame. It can't be an object of class xgb.DMatrix" =
      is.matrix(X) || is.data.frame(X),
    "X_pred must be a matrix or a xgb.DMatrix" =
      is.matrix(X_pred) || inherits(X_pred, "xgb.DMatrix"),
    "X_pred must have column names" = !is.null(colnames(X_pred))
  )

  S <- stats::predict(object, newdata = X_pred, predcontrib = TRUE, ...)

  if (interactions) {
    S_inter <- stats::predict(object, newdata = X_pred, predinteraction = TRUE, ...)
  }

  # Multiclass
  if (is.list(S)) {
    if (is.null(which_class)) {
      nms <- setdiff(colnames(S[[1L]]), "BIAS")
      if (interactions) {
        S_inter <- lapply(S_inter, function(s) s[, nms, nms, drop = FALSE])
      } else {
        # mapply() does not want to see a length 0 object like NULL
        S_inter <- replicate(length(S), NULL)
      }
      shapviz_list <- mapply(
        FUN = shapviz.matrix,
        object = lapply(S, function(s) s[, nms, drop = FALSE]),
        baseline = lapply(S, function(s) unname(s[1L, "BIAS"])),
        S_inter = S_inter,
        MoreArgs = list(X = X, collapse = collapse),
        SIMPLIFY = FALSE
      )
      names(shapviz_list) <- .make_class_names(length(S))
      return(mshapviz(shapviz_list))
    }
    # Old way: select just one class
    S <- S[[which_class]]
    if (interactions) {
      S_inter <- S_inter[[which_class]]
    }
  }

  # Call matrix method
  nms <- setdiff(colnames(S), "BIAS")
  shapviz.matrix(
    object = S[, nms, drop = FALSE],
    X = X,
    baseline = unname(S[1L, "BIAS"]),
    S_inter = if (interactions) S_inter[, nms, nms, drop = FALSE],
    collapse = collapse
  )
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from a LightGBM model.
#' @export
shapviz.lgb.Booster = function(object, X_pred, X = X_pred,
                               which_class = NULL, collapse = NULL, ...) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("Package 'lightgbm' not installed")
  }
  stopifnot(
    "X_pred must be a matrix" = is.matrix(X_pred),
    "X_pred must have column names" = !is.null(colnames(X_pred))
  )

  # Switch for different versions of predict.lgb.Booster()
  is_v4 <- utils::packageVersion("lightgbm") >= "4"
  has_type <- "type" %in% names(formals(utils::getS3method("predict", "lgb.Booster")))
  if (is_v4 || has_type) {
    S <- stats::predict(object, newdata = X_pred, type = "contrib", ...)
  } else {
    S <- stats::predict(object, X_pred, predcontrib = TRUE, ...)
  }

  pp <- ncol(X_pred) + 1L
  stopifnot(ncol(S) %% pp == 0)

  # Reduce multiclass setting
  m <- ncol(S) %/% pp
  if (m >= 2L) {
    if (is.null(which_class)) {
      S_list <- lapply(1:m, function(j) S[, 1:(pp - 1L) + pp * (j - 1L), drop = FALSE])
      S_list <- lapply(S_list, function(s) {colnames(s) <- colnames(X_pred); s})
      shapviz_list <- mapply(
        FUN = shapviz.matrix,
        object = S_list,
        baseline = S[1L, pp * (1:m)],
        MoreArgs = list(X = X, collapse = collapse),
        SIMPLIFY = FALSE
      )
      names(shapviz_list) <- .make_class_names(m)
      return(mshapviz(shapviz_list))
    }
    # Old way: select just one class
    S <- S[, 1:pp + pp * (which_class - 1L), drop = FALSE]
  }

  # Call matrix method
  baseline <- S[1L, pp]
  S <- S[, -pp, drop = FALSE]
  colnames(S) <- colnames(X_pred)
  shapviz.matrix(object = S, X = X, baseline = baseline, collapse = collapse)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `fastshap::explain()`.
#' @export
shapviz.explain <- function(object, X = NULL, baseline = NULL, collapse = NULL, ...) {
  # Thanks @Brandon Greenwell
  if (inherits(object, "data.frame")) {  # packageVersion("fastshap") <= "0.0.7"
    object <- as.matrix(object)
  }
  if (is.matrix(object)) {    # fastshap::explain() called with shap_only = TRUE
    if (is.null(baseline)) {  # try to extract baseline attribute
      baseline <- attr(object, which = "baseline")
    }
    if (is.null(baseline)) {  # will still be NULL is missing, so check again
      warning("No baseline attribute found in 'object', setting baseline to zero.")
      baseline <- 0
    }
    if (is.null(X)) {
      stop("No feature values found. Pass feature values via 'X' or use ",
           "'fastshap::explain(..., shap_only = FALSE)'")
    }
    shapviz.matrix(object = object, X = X, baseline = baseline, collapse = collapse)
  } else {  # explain() called with shap_only = FALSE
    shapviz.matrix(
      object = object[["shapley_values"]],
      X = if (!is.null(X)) X else object[["feature_values"]],
      baseline = if (!is.null(baseline)) baseline else object[["baseline"]],
      collapse = collapse
    )
  }
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `treeshap::treeshap()`.
#' @export
shapviz.treeshap <- function(object, X = object[["observations"]],
                             baseline = 0, collapse = NULL, ...) {
  S_inter <- object[["interactions"]]
  if (!is.null(S_inter)) {
    S_inter <- aperm(S_inter, c(3L, 1:2))
  }
  shapviz.matrix(
    object = as.matrix(object[["shaps"]]),
    X = X,
    baseline = baseline,
    collapse = collapse,
    S_inter = S_inter
  )
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `DALEX::predict_parts()`.
#' @export
shapviz.predict_parts <- function(object, ...) {
  if (!inherits(object, c("shap", "shap_aggregated", "break_down"))) {
    stop("Incorrect object! It is neither 'shap', 'shap_aggregated' nor 'break_down'!")
  }
  if (inherits(object, "shap")) {
    agg <- as.data.frame(object[object$B == 0, ])
    baseline <- attr(object, "intercept")
  } else {
    if (inherits(object, "shap_aggregated")) {
      agg <- object$aggregated
    } else {  # break_down
      agg <- as.data.frame(object)  # to drop additional classes
    }
    baseline <- agg[1L, "contribution"]
    agg <- agg[2:(nrow(agg) - 1L), ]     # Drop intercept and prediction
  }

  ## Problem: agg might contain all feature values as strings
  ## -> excludes sv_dependence() and beeswarms
  X <- data.frame(t(agg[["variable_value"]]))
  S <- t(agg[["contribution"]])
  colnames(X) <- colnames(S) <- agg[["variable_name"]]

  shapviz(object = S, X = X, baseline = baseline, ...)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `shapr::explain()`.
#' @export
shapviz.shapr <- function(object, X = object[["x_test"]], collapse = NULL, ...) {
  dt <- as.matrix(object[["dt"]])
  shapviz.matrix(
    object = dt[, setdiff(colnames(dt), "none"), drop = FALSE],
    X = X,
    baseline = dt[1L, "none"],
    collapse = collapse
  )
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `kernelshap::kernelshap()`.
#' @export
shapviz.kernelshap <- function(object, X = object[["X"]],
                               which_class = NULL, collapse = NULL, ...) {
  S <- object[["S"]]
  b <- object[["baseline"]]

  # Multiclass/multioutput
  if (is.list(S)) {
    if (is.null(which_class)) {
      shapviz_list <- mapply(
        FUN = shapviz.matrix,
        object = S,
        baseline = b,
        MoreArgs = list(X = X, collapse = collapse),
        SIMPLIFY = FALSE
      )
      if (is.null(names(shapviz_list))) {
        names(shapviz_list) <- .make_class_names(length(S))
      }
      return(mshapviz(shapviz_list))
    }
    # Old way: select just one class
    S <- S[[which_class]]
    b <- b[which_class]
  }

  shapviz.matrix(object = S, X = X, baseline = b, collapse = collapse)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from `kernelshap::permshap()`.
#' @export
shapviz.permshap <- function(object, X = object[["X"]],
                             which_class = NULL, collapse = NULL, ...) {
  # The output structure of permshap is identical to kernelshap
  shapviz.kernelshap(object, X = X, which_class = which_class, collapse = collapse, ...)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from a (tree-based) H2O regression model.
#' @export
shapviz.H2ORegressionModel = function(object, X_pred, X = as.data.frame(X_pred),
                                      collapse = NULL, ...) {
  shapviz.H2OModel(object = object, X_pred = X_pred, X = X, collapse = collapse, ...)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from a (tree-based) H2O binary classification model.
#' @export
shapviz.H2OBinomialModel = function(object, X_pred, X = as.data.frame(X_pred),
                                    collapse = NULL, ...) {
  shapviz.H2OModel(object = object, X_pred = X_pred, X = X, collapse = collapse, ...)
}

#' @describeIn shapviz
#'   Creates a "shapviz" object from a (tree-based) H2O model (base class).
#' @export
shapviz.H2OModel = function(object, X_pred, X = as.data.frame(X_pred),
                            collapse = NULL, ...) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' not installed")
  }
  stopifnot(
    "X_pred must be a data.frame or an H2OFrame" =
      is.data.frame(X_pred) || inherits(X_pred, "H2OFrame"),
    "X_pred must have column names" = !is.null(colnames(X_pred))
  )
  if (!inherits(X_pred, "H2OFrame")) {
    X_pred <- h2o::as.h2o(X_pred)
  }
  S <- as.matrix(h2o::h2o.predict_contributions(object, newdata = X_pred, ...))
  shapviz.matrix(
    object = S[, setdiff(colnames(S), "BiasTerm"), drop = FALSE],
    X = X,
    baseline = unname(S[1L, "BiasTerm"]),
    collapse = collapse
  )
}

#' Combines compatible "shapviz" Objects
#'
#' This function combines a list of compatible "shapviz" objects to an object of class
#' "mshapviz". The elements can be named.
#'
#' @param object List of "shapviz" objects to be concatenated.
#' @param ... Not used.
#' @returns A "mshapviz" object.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1L]
#' s2 <- shapviz(S, X, baseline = 4)[2L]
#' s <- mshapviz(c(shp1 = s1, shp2 = s2))
#' s
mshapviz <- function(object, ...) {
  stopifnot("'object' must be a list" = is.list(object))
  if (!all(vapply(object, is.shapviz, FUN.VALUE = logical(1)))) {
    stop("Must pass list of 'shapviz' objects")
  }
  nms <- lapply(object, colnames)
  if (!all(vapply(nms, identical, y = nms[[1L]], FUN.VALUE = logical(1)))) {
    stop("'shapviz' objects need to have identical column names")
  }
  # Plot methods using interactions and do.call(rbind, ...) will fail, other plots are ok
  # inter <- vapply(
  #   object, function(x) is.null(get_shap_interactions(x)), FUN.VALUE = logical(1)
  # )
  # if (!(all(inter) || !any(inter))) {
  #   stop("Some 'shapviz' objects have SHAP interactions, some not.")
  # }
  structure(object, class = "mshapviz")
}


# Helper function
.make_class_names <- function(m) {
  paste("Class", seq_len(m), sep = "_")
}

.input_checks <- function(object, X, baseline = 0, S_inter = NULL) {
  stopifnot(
    "'X' must be a matrix or data.frame" = is.matrix(X) || is.data.frame(X),
    "'object' needs at least one row and one column" = dim(object) >= 1L,
    "'X' need at least one row and one column" = dim(X) >= 1L,
    "The number of rows of 'object' and 'X' differ" = nrow(object) == nrow(X),
    "'object' must have column names" = !is.null(colnames(object)),
    "'X' must have column names" = !is.null(colnames(X)),
    "'X' must contain all column names of 'object'" =
      all(colnames(object) %in% colnames(X)),
    "No missing SHAP values allowed" = !anyNA(object),
    "'baseline' has to be a single number" =
      length(baseline) == 1L && is.numeric(baseline),
    "'baseline' cannot be NA" = !is.na(baseline)
  )
  if (!is.null(S_inter)) {
    nms <- dimnames(S_inter)
    stopifnot(
      "'S_inter' must be an array" = is.array(S_inter),
      "'S_inter' must be 3-dimensional" = length(dim(S_inter)) == 3L,
      "Shape of 'S_inter' must be consistent with 'object'" =
        dim(S_inter) == c(dim(object), ncol(object)),
      "Dimensions 2 and 3 of 'S_inter' must have names" =
        !is.null(nms[[2L]]) && !is.null(nms[[3L]]),
      "Dimnames 2 and 3 of 'S_inter' must be equal" = nms[[2L]] == nms[[3L]],
      "Dimnames of 'S_inter' must be consistent with those of 'object'" =
        all(colnames(object) %in% nms[[2L]]),
      "No missing SHAP interaction values allowed" = !anyNA(S_inter)
      # "SHAP interactions must sum up to SHAP values" =
      #   max(abs(object - apply(S_inter, 1:2, FUN = sum))) <= 1e-4,
      # "SHAP interactions should be symmetric in dimensions 2 and 3" =
      #   max(abs(S_inter - aperm(S_inter, c(1L, 3:2)))) <= 1e-4
    )
  }
}
