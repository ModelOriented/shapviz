#' Prints "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shapviz(S, X, baseline = 4)
#' @seealso \code{\link{shapviz}}.
print.shapviz <- function(x, ...) {
  cat("'shapviz' object representing", .print_dim(get_shap_values(x)), "SHAP matrix\n")
  invisible(x)
}

#' Prints "mshapviz" Object
#'
#' @param x An object of class "mshapviz".
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)
#' s2 <- shapviz(S, X, baseline = 4)
#' x <- c(s1 = s1, s2 = s2)
#' x
#' @seealso \code{\link{shapviz}}.
print.mshapviz <- function(x, ...) {
  nms <- names(x)
  S_list <- get_shap_values(x)

  cat("'mshapviz' object representing", length(x), "'shapviz' objects:")
  for (i in seq_along(x)) {
    nm <- if (is.null(nms)) i else paste0("'", nms[i], "'")
    cat("\n  ", paste0(nm, ":"), .print_dim(S_list[[i]]), "SHAP matrix")
  }
  cat("\n")
  invisible(x)
}

#' Summarizes "shapviz" Object
#'
#' @param object An object of class "shapviz".
#' @param n Maximum number of rows of SHAP values and feature values to show.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @export
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' object <- shapviz(S, X, baseline = 4)
#' summary(object)
#' @seealso \code{\link{shapviz}}.
summary.shapviz <- function(object, n = 2L, ...) {
  S <- get_shap_values(object)
  X <- get_feature_values(object)
  S_inter <- get_shap_interactions(object)
  n <- min(n, nrow(S))
  cat(
    "'shapviz' object representing \n  - SHAP matrix of dimension", .print_dim(S),
    "\n  - feature data.frame of dimension",  .print_dim(X),
    "\n  - baseline value of", get_baseline(object)
  )
  if (!is.null(S_inter)) {
    cat("\n  - SHAP interaction array of dimension", .print_dim(S_inter))
  }
  cat("\n\n")
  cat("SHAP values of first", n, "observations:\n")
  print(utils::head(S, n))
  cat("\n Corresponding feature values:\n")
  print(utils::head(X, n))
  cat("\n")
  invisible(object)
}

# TODO: summary.mshapviz()

#' Dimensions of "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @return A numeric vector of length two providing the number of rows and columns
#' of the SHAP matrix stored in \code{x}.
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

#' Check for mshapviz
#'
#' Is object of class "mshapviz"?
#'
#' @param object An R object.
#' @return Returns \code{TRUE} if \code{object} has "\code{mshapviz}" among its classes, and \code{FALSE} otherwise.
#' @export
#' @examples
#' S1 <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' S2 <- matrix(c(-1, 1, 1, -1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X1 <- data.frame(x = c("a", "b"), y = c(100, 10))
#' X2 <- data.frame(x = c("b", "a"), y = c(100, 10))
#' x1 <- shapviz(S1, X1, baseline = 4)
#' x2 <- shapviz(S2, X2, baseline = 4)
#' x <- c(Model_1 = x1, Model_2 = x2)
#' is.mshapviz(x)
#' is.mshapviz(x1)
is.mshapviz <- function(object){
  inherits(object, "mshapviz")
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

#' Rowbinds two "shapviz" Objects
#'
#' Rowbinds two "shapviz" objects using \code{+}.
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

#' Rowbinds Multiple "shapviz" Objects
#'
#' It is based on the \code{+} operator for "shapviz" objects.
#'
#' @param ... Any number of "shapviz" objects.
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

#' Concatenates "shapviz" Objects
#'
#' This function combines two or more "shapviz" objects to an object of class
#' "mshapviz". The objects can be named.
#'
#' @param ... Any number of (optionally named) "shapviz" objects.
#' @return A "mshapviz" object.
#' @export
#' @examples
#' S1 <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' S2 <- matrix(c(-1, 1, 1, -1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X1 <- data.frame(x = c("a", "b"), y = c(100, 10))
#' X2 <- data.frame(x = c("b", "a"), y = c(100, 10))
#' x1 <- shapviz(S1, X1, baseline = 4)
#' x2 <- shapviz(S2, X2, baseline = 4)
#' x <- c(Model_1 = x1, Model_2 = x2)
#' x
c.shapviz <- function(...) {
  mshapviz(list(...))
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

# Produces strings of the form "4 x 5"
.print_dim <- function(X, sep = " x ") {
  paste(dim(X), collapse = sep)
}
