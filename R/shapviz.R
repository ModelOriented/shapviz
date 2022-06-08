#' Initialize "shapviz" Object
#'
#' This function creates an object of class "shapviz" from one of the following inputs:
#' \itemize{
#'   \item A matrix of SHAP values.
#'   \item A fitted XGBoost model.
#'   \item A fitted LightGBM model.
#'   \item An "explain" object from the package "fastshap".
#'   \item The result of calling \code{treeshap()} from the "treeshap" package.
#' }
#' Along with the main input, a dataset \code{X} of feature values is required that
#' is used for visualization only.
#' It can thus contain character or factor variables even if
#' the SHAP values were calculated from a purely numeric feature matrix.
#' @param object Object to be converted to an object of type "shapviz".
#' @param X Corresponding matrix or data.frame of feature values used for visualization.
#' @param X_pred Feature matrix as expected by the \code{predict} function of
#' XGBoost/LightGBM. In case \code{object} is an XGBoost model, it can also be
#' an \code{xgb.DMatrix}.
#' @param baseline Optional baseline value, representing the average response at the
#' scale of the SHAP values. It will be used for plot methods that explain single
#' predictions.
#' @param which_class In case of a multiclass setting, which class
#' to explain (an integer between 1 and the \code{num_class} parameter of the model).
#' @param ... Parameters passed to other methods (currently only used by
#' the \code{predict} functions of XGBoost and LightGBM).
#' @return An object of class "shapviz" with the following three elements:
#' \itemize{
#'   \item \code{S}: A numeric \code{matrix} of SHAP values.
#'   \item \code{X}: A \code{data.frame} containing the feature values corresponding to \code{S}.
#'   \item \code{baseline}: Baseline value, representing the average prediction at the scale of the SHAP values.
#' }
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shapviz(S, X, baseline = 4)
#'
shapviz <- function(object, ...){
  UseMethod("shapviz")
}

#' @describeIn shapviz Default method to initialize a "shapviz" object.
#' @export
shapviz.default = function(object, ...) {
  stop("No default method available. shapviz() is available for objects
       of class 'matrix', 'xgb.Booster', 'lgb.Booster', 'treeshap',
       and 'explain' (from fastshap package).")
}

#' @describeIn shapviz Creates a "shapviz" object from a matrix of SHAP values.
#' @export
shapviz.matrix = function(object, X, baseline = 0, ...) {
  stopifnot(
    "X must be a matrix or data.frame" = is.matrix(X) || is.data.frame(X),
    "Dimensions of object and X are incompatible" = dim(object) == dim(X),
    "X and object need at least one row and one column" = dim(X) >= 1L,
    "SHAP matrix ('object') must have column names" = !is.null(colnames(object)),
    "X must have column names" = !is.null(colnames(X)),
    "object and X must have the same column names" =
      sort(colnames(object)) == sort(colnames(X)),
    "No missing SHAP values allowed" = !anyNA(object),
    "baseline has to be a single number" =
      length(baseline) == 1L && is.numeric(baseline),
    "baseline cannot be NA" = !is.na(baseline)
  )
  out <- list(
    S = object[, colnames(X), drop = FALSE],
    X = as.data.frame(X),
    baseline = baseline
  )
  class(out) <- "shapviz"
  out
}

#' @describeIn shapviz Creates a "shapviz" object from an XGBoost model.
#' @export
#' @examples
#' X_pred <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_pred, label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#'
#' # Will use numeric matrix "X_pred" as feature matrix
#' shapviz(fit, X_pred = X_pred)
#'
#' # Will use original values as feature matrix
#' x <- shapviz(fit, X_pred = X_pred, X = iris[, -1])
#' x
#'
#' # "X_pred" can also be passed as xgb.DMatrix, but only if X is passed as well!
#' shapviz(fit, X_pred = dtrain, X = iris[, -1])
#'
#' # In multiclass setting, we need to specify which_class (integer starting at 1)
#' # should be explained.
#' params <- list(objective="multi:softprob", num_class = 3)
#' X_pred <- data.matrix(iris[, -5])
#' dtrain <- xgboost::xgb.DMatrix(X_pred, label = as.integer(iris[, 5]) - 1L)
#' fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = X_pred, which_class = 3)
#' x
shapviz.xgb.Booster = function(object, X_pred, X = X_pred, which_class = NULL, ...) {
  stopifnot(
    "X_pred must be a matrix or a xgb.DMatrix" =
      is.matrix(X_pred) || inherits(X_pred, "xgb.DMatrix"),
    "X must be a matrix or data.frame" = is.matrix(X) || is.data.frame(X),
    "Dimensions of X_pred and X are incompatible" = dim(X_pred) == dim(X),
    "X_pred must have column names" = !is.null(colnames(X_pred)),
    "X must have column names" = !is.null(colnames(X)),
    "X_pred and X should have the same column names" =
      sort(colnames(X_pred)) == sort(colnames(X))
  )
  if (!inherits(X_pred, "xgb.DMatrix")) {
    X_pred <- xgboost::xgb.DMatrix(X_pred)
  }
  S <- stats::predict(object, newdata = X_pred, predcontrib = TRUE, ...)
  shapviz_from_xgb_predict(S, X = X, which_class = which_class)
}

#' @describeIn shapviz Creates a "shapviz" object from a LightGBM model.
#' @export
shapviz.lgb.Booster = function(object, X_pred, X = X_pred, which_class = NULL, ...) {
  stopifnot(
    "X_pred must be a matrix" = is.matrix(X_pred),
    "X must be a matrix or data.frame" = is.matrix(X) || is.data.frame(X),
    "Dimensions of X_pred and X are incompatible" = dim(X_pred) == dim(X),
    "X_pred must have column names" = !is.null(colnames(X_pred)),
    "X must have column names" = !is.null(colnames(X)),
    "X_pred and X should have the same column names" =
      sort(colnames(X_pred)) == sort(colnames(X))
  )
  S <- stats::predict(object, data = X_pred, predcontrib = TRUE, ...)

  # We need to be cautious here: S has no column names and can represent SHAP values
  # for more than one category. In order to not mess up the column order in the next
  # function call, we sort X in the same order as X_pred.
  shapviz_from_lgb_predict(
    S,
    X = X[, colnames(X_pred), drop = FALSE],
    which_class = which_class
  )
}

#' @describeIn shapviz Creates a "shapviz" object from fastshap's "explain()" method.
#' @export
shapviz.explain <- function(object, X, baseline = 0, ...) {
  shapviz.matrix(as.matrix(object), X = X, baseline = baseline, ...)
}

#' @describeIn shapviz Creates a "shapviz" object from treeshap's "treeshap()" method.
#' @export
shapviz.treeshap <- function(object, X = object[["observations"]], baseline = 0, ...) {
  shapviz.matrix(
    object = as.matrix(object[["shaps"]]),
    X = X,
    baseline = baseline,
    ...
  )
}

#' Initialize "shapviz" Object from XGBoost/LightGBM Predict
#'
#' These functions create an object of class "shapviz" by taking the output of
#' \code{predict(..., predcontrib = TRUE)} of an XGBoost or LightGBM model,
#' together with a matrix/data.frame \code{X} representing the corresponding
#' feature values. These functions are mainly written for internal use
#' but they can be useful if SHAP values have already been computed.
#' Note that the SHAP matrix returned by LightGBM does not provide column names.
#' Thus, you need to be absolutely sure that the column names of \code{X}
#' correspond to the column names originally passed to \code{predict()}.
#'
#' @param S Output of calling \code{predict(..., predcontrib = TRUE)}.
#' @param X Matrix or data.frame of feature values corresponding to \code{S}.
#' @param which_class In case of a multiclass setting, which class >= 1 to be explained.
#' @param ... Other parameters passed (currently unused).
#' @return An object of class "shapviz".
#' @name from_xgb_or_lgb
NULL

#' @rdname from_xgb_or_lgb
#' @export
#' @examples
#' X_pred <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_pred, label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' shap_values <- predict(fit, dtrain, predcontrib = TRUE)
#' x <- shapviz_from_xgb_predict(shap_values, iris[, -1])
shapviz_from_xgb_predict <- function(S, X, which_class = NULL, ...) {
  # Reduce multiclass setting
  if (is.list(S)) {
    m <- length(S)
    stopifnot(
      !is.null(which_class),
      which_class <= m
    )
    S <- S[[which_class]]
  }
  stopifnot(
    is.matrix(S),
    is.matrix(X) || is.data.frame(X),
    nrow(S) == nrow(X),
    ncol(S) == ncol(X) + 1L,
    !is.null(colnames(S)),
    !is.null(colnames(X)),
    sort(colnames(S)) == sort(c(colnames(X), "BIAS"))
  )
  shapviz.matrix(
    S[, colnames(X), drop = FALSE],
    X = X,
    baseline = unname(S[1L, "BIAS"])
  )
}

#' @rdname from_xgb_or_lgb
#' @export
shapviz_from_lgb_predict <- function(S, X, which_class = NULL, ...) {
  pp <- ncol(X) + 1L
  stopifnot(
    is.matrix(S),
    is.matrix(X) || is.data.frame(X),
    nrow(S) == nrow(X),
    ncol(S) %% pp == 0,
    !is.null(colnames(X))
  )
  # Reduce multiclass setting
  m <- ncol(S) %/% pp
  if (m >= 2L) {
    stopifnot(
      !is.null(which_class),
      which_class <= m
    )
    S <- S[, 1:pp + pp * (which_class - 1), drop = FALSE]
  }
  baseline <- S[1L, pp]
  S <- S[, -pp, drop = FALSE]
  colnames(S) <- colnames(X)
  shapviz.matrix(S, X = X, baseline = baseline)
}
