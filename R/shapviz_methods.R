#' Prints "shapviz" Object
#'
#' @param x An object of class "shapviz".
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
  S_inter <- get_shap_interactions(x)
  n <- min(n, nrow(S))
  cat(
    "'shapviz' object representing \n  - SHAP matrix of dimension",
    nrow(S), "x", ncol(S),
    "\n  - feature data.frame of dimension",  nrow(S), "x", ncol(S),
    "\n  - baseline value of", get_baseline(x)
  )
  if (!is.null(S_inter)) {
    cat(
      "\n  - SHAP interaction array of dimension",
      paste(dim(S_inter), collapse = " x "))
  }
  cat("\n\n")
  cat("SHAP values of first", n, "observations:\n")
  print(utils::head(S, n))
  cat("\n Corresponding feature values:\n")
  print(utils::head(get_feature_values(x), n))
  cat("\n")
  invisible(x)
}

#' Dimensions of "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @return A numeric vector of length two providing the number of rows and columns
#' of the SHAP matrix (or the feature dataset) stored in \code{x}.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' dim(shapviz(S, X))
#' @seealso \code{\link{shapviz}}.
dim.shapviz <- function(x) {
  dim(get_shap_values(x))
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
#' Functions to extract SHAP values, feature values, the baseline, or SHAP interactions from a "shapviz" object.
#'
#' @name extractors
#' @param object Object to extract something.
#' @param ... Currently unused.
#' @return `get_shap_values()` returns the matrix of SHAP values,
#' `get_feature_values()` the \code{data.frame} of feature values,
#' `get_baseline()` the numeric baseline value,
#' and `get_shap_interactions()` the SHAP interactions of the input.
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
get_shap_interactions.default = function(object, ...) {
  stop("No default method available.")
}

#' Subsets "shapviz" Object
#'
#' Use standard square bracket subsetting to select rows and/or columns of
#' SHAP values, feature values, and SHAP interaction values of a "shapviz" object.
#'
#' @param x An object of class "shapviz".
#' @param i Row subsetting.
#' @param j Column subsetting.
#' @param ... Currently unused.
#' @return A new object of class "shapviz".
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' x[1, "x"]
#' @seealso \code{\link{shapviz}}.
#' @export
`[.shapviz` <- function(x, i, j, ...) {
  inter <- get_shap_interactions(x)
  shapviz(
    object = get_shap_values(x)[i, j, drop = FALSE],
    X = get_feature_values(x)[i, j, drop = FALSE],
    baseline = get_baseline(x),
    S_inter = if (!is.null(inter)) inter[i, j, j, drop = FALSE]
  )
}

#' Dimnames of "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @return Dimnames of the SHAP matrix.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' dimnames(x)
#'
#' # Implies colnames()
#' colnames(x)
#' @seealso \code{\link{shapviz}}.
#' @export
dimnames.shapviz <- function(x) {
  dimnames(get_shap_values(x))
}

#' Concatenates "shapviz" Object
#'
#' Use standard plus operator to concatenate two "shapviz" objects.
#'
#' @param e1 The first object of class "shapviz".
#' @param e2 The second object of class "shapviz".
#' @return A new object of class "shapviz".
#' @export
#' @examples
#' S1 <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' S2 <- matrix(c(-1, 1, 1, -1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X1 <- data.frame(x = c("a", "b"), y = c(100, 10))
#' X2 <- data.frame(x = c("b", "a"), y = c(100, 10))
#' x1 <- shapviz(S1, X1, baseline = 4)
#' x2 <- shapviz(S2, X2, baseline = 4)
#' x1 + x2
#' @seealso \code{\link{shapviz}}.
#' @export
`+.shapviz` <- function(e1, e2){
  # input checks
  stopifnot(
    is.shapviz(e1),
    is.shapviz(e2),
    ncol(e1) == ncol(e2),
    colnames(e1) == colnames(e2)
  )

  baseline <- get_baseline(e1)
  if (baseline != get_baseline(e2)) {
    warning("Baselines not identical! Will use the one from the first shapviz object.")
  }

  shapviz(
    object = rbind(get_shap_values(e1), get_shap_values(e2)),
    X = rbind(get_feature_values(e1), get_feature_values(e2)),
    b = baseline,
    S_inter = rbind_S_inter(get_shap_interactions(e1), get_shap_interactions(e2))
  )
}

#' Concatenates "shapviz" Objects
#'
#' It is based on the `+.shapviz` operator
#'
#' @param ... "shapviz" objects to be concatenated.
#' @return A new object of class "shapviz".
#' @export
#' @examples
#' S1 <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' S2 <- matrix(c(-1, 1, 1, -1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X1 <- data.frame(x = c("a", "b"), y = c(100, 10))
#' X2 <- data.frame(x = c("b", "a"), y = c(100, 10))
#' x1 <- shapviz(S1, X1, baseline = 4)
#' x2 <- shapviz(S2, X2, baseline = 4)
#' rbind(x1, x2)
#' @seealso \code{\link{shapviz}}.
#' @export
rbind.shapviz <- function(...) {
  Reduce(`+`, list(...))
}

# Helper functions

# Binds two compatible SHAP interaction arrays along the first dimension.
rbind_S_inter <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    stopifnot(is.null(x) && is.null(y))
    return(NULL)
  }

  # Could do many input checks, but consistency is given when called from `+.shapviz`()
  nx <- nrow(x)
  out <- array(
    dim = c(nx + nrow(y), dim(x)[-1L]),
    dimnames = c(list(NULL), dimnames(x)[-1L])
  )

  ix <- seq_len(nx)
  out[ix, , ] <- x
  out[-ix, , ] <- y
  out
}

