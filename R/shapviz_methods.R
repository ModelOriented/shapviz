#' Prints "shapviz" Object
#'
#' @param x A on object of class "shapviz".
#' @param n Maximum number of rows of SHAP values and feature values to show.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shapviz(S, X, baseline = 4)
#' @seealso \code{\link{shapviz}}.
print.shapviz <- function(x, n = 2L, ...) {
  S <- get_shap_values(x)
  n <- min(n, nrow(S))
  cat(
    "'shapviz' object representing \n  - SHAP matrix of dimension",
    nrow(S), "x", ncol(S),
    "\n  - feature data.frame of dimension",  nrow(S), "x", ncol(S),
    "\n  - baseline value of", get_baseline(x)
  )
  cat("\n\n")
  cat("SHAP values of first", n, "observations:\n")
  print(utils::head(S, n))
  cat("\n Corresponding feature values:\n")
  print(utils::head(get_feature_values(x), n))
  cat("\n")
  invisible(x)
}

#' Check for shapviz
#'
#' Is object of class "shapviz"?
#'
#' @param object An R object.
#' @return Returns \code{TRUE} if \code{object} has "\code{shapviz}" among its classes, and \code{FALSE} otherwise.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shp <- shapviz(S, X)
#' is.shapviz(shp)
#' is.shapviz("a")
is.shapviz <- function(object){
  inherits(object, "shapviz")
}


#' Extractor Functions
#'
#' Functions to extract SHAP values, feature values, or the baseline from a "shapviz" object.
#'
#' @name extractors
#' @param object Object to extract something.
#' @param ... Currently unused.
#' @return `get_shap_values()` returns the matrix of SHAP values, `get_feature_values()` the \code{data.frame} of feature values, and `get_baseline()` the numeric baseline value of the input.
NULL

#' @rdname extractors
#' @export
get_shap_values <- function(object, ...){
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
get_shap_values.default = function(object, ...) {
  stop("No default method available.")
}

#' @rdname extractors
#' @export
get_feature_values <- function(object, ...){
  UseMethod("get_feature_values")
}

#' @rdname extractors
#' @export
get_feature_values.shapviz = function(object, ...) {
  object[["X"]]
}

#' @rdname extractors
#' @export
get_feature_values.default = function(object, ...) {
  stop("No default method available.")
}

#' @rdname extractors
#' @export
get_baseline <- function(object, ...){
  UseMethod("get_baseline")
}

#' @rdname extractors
#' @export
get_baseline.shapviz = function(object, ...) {
  object[["baseline"]]
}

#' @rdname extractors
#' @export
get_baseline.default = function(object, ...) {
  stop("No default method available.")
}
