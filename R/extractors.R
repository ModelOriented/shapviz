#' Extractor Functions
#'
#' Functions to extract SHAP values, feature values, the baseline, or SHAP interactions from a "(m)shapviz" object.
#'
#' @name extractors
#' @param object Object to extract something.
#' @param ... Currently unused.
#' @return `get_shap_values()` returns the matrix of SHAP values,
#' `get_feature_values()` the \code{data.frame} of feature values,
#' `get_baseline()` the numeric baseline value,
#' and `get_shap_interactions()` the SHAP interactions of the input.
#' For objects of class "mshapviz", these functions return lists of those elements.
NULL

#' @rdname extractors
#' @export
get_shap_values <- function(object, ...) {
  UseMethod("get_shap_values")
}

#' @rdname extractors
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shp <- shapviz(S, X, baseline = 4)
#' get_shap_values(shp)
get_shap_values.shapviz = function(object, ...) {
  object[["S"]]
}

#' @rdname extractors
#' @export
get_shap_values.mshapviz = function(object, ...) {
  lapply(object, get_shap_values)
}

#' @rdname extractors
#' @export
get_shap_values.default = function(object, ...) {
  stop("No default method available.")
}

#' @rdname extractors
#' @export
get_feature_values <- function(object, ...) {
  UseMethod("get_feature_values")
}

#' @rdname extractors
#' @export
get_feature_values.shapviz <- function(object, ...) {
  object[["X"]]
}

#' @rdname extractors
#' @export
get_feature_values.mshapviz <- function(object, ...) {
  lapply(object, get_feature_values)
}

#' @rdname extractors
#' @export
get_feature_values.default <- function(object, ...) {
  stop("No default method available.")
}

#' @rdname extractors
#' @export
get_baseline <- function(object, ...) {
  UseMethod("get_baseline")
}

#' @rdname extractors
#' @export
get_baseline.shapviz = function(object, ...) {
  object[["baseline"]]
}

#' @rdname extractors
#' @export
get_baseline.mshapviz = function(object, ...) {
  lapply(object, get_baseline)
}

#' @rdname extractors
#' @export
get_baseline.default = function(object, ...) {
  stop("No default method available.")
}

#' @rdname extractors
#' @export
get_shap_interactions <- function(object, ...) {
  UseMethod("get_shap_interactions")
}

#' @rdname extractors
#' @export
get_shap_interactions.shapviz = function(object, ...) {
  object[["S_inter"]]
}

#' @rdname extractors
#' @export
get_shap_interactions.mshapviz = function(object, ...) {
  lapply(object, get_shap_interactions)
}

#' @rdname extractors
#' @export
get_shap_interactions.default = function(object, ...) {
  stop("No default method available.")
}
