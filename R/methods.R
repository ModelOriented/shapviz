#' Prints "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' x
#' @seealso [shapviz()]
#' @export
print.shapviz <- function(x, ...) {
  cat("'shapviz' object representing", .print_dim(get_shap_values(x)), "SHAP matrix\n")
  invisible(x)
}

#' Prints "mshapviz" Object
#'
#' @param x An object of class "mshapviz".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1]
#' s2 <- shapviz(S, X, baseline = 4)
#' x <- c(s1 = s1, s2 = s2)
#' x
#' @seealso [mshapviz()]
#' @export
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
#' @returns Invisibly, the input is returned.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' object <- shapviz(S, X, baseline = 4)
#' summary(object)
#' @seealso [shapviz()]
#' @export
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

# TODO? -> summary.mshapviz()

#' Dimensions of "shapviz" Object
#'
#' @param x An object of class "shapviz".
#' @returns
#'   A numeric vector of length two providing the number of rows and columns
#'   of the SHAP matrix stored in `x`.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X)
#' dim(x)
#' nrow(x)
#' ncol(x)
#' @seealso [shapviz()]
#' @export
dim.shapviz <- function(x) {
  dim(get_shap_values(x))
}

#' Check for shapviz
#'
#' Is object of class "shapviz"?
#'
#' @param object An R object.
#' @returns
#'   Returns `TRUE` if `object` has "shapviz" among its classes,
#'   and `FALSE` otherwise.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' shp <- shapviz(S, X)
#' is.shapviz(shp)
#' is.shapviz("a")
#' @seealso [shapviz()]
#' @export
is.shapviz <- function(object){
  inherits(object, "shapviz")
}

#' Check for mshapviz
#'
#' Is object of class "mshapviz"?
#'
#' @param object An R object.
#' @returns
#'   Returns `TRUE` if `object` has "mshapviz" among its classes,
#'   and `FALSE` otherwise.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1]
#' s2 <- shapviz(S, X, baseline = 4)
#' x <- c(s1 = s1, s2 = s2)
#' is.mshapviz(x)
#' is.mshapviz(s1)
#' @seealso [mshapviz()]
#' @export
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
#' @returns A new object of class "shapviz".
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' x[1, "x"]
#' x[1]
#' x[c(FALSE, TRUE), ]
#' x[, "x"]
#' @seealso [shapviz()]
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
#' This implies to use `colnames(x)` to get the column names of the SHAP and feature
#' matrix (and optional SHAP interaction values).
#'
#' @param x An object of class "shapviz".
#' @returns Dimnames of the SHAP matrix.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' dimnames(x)
#' colnames(x)
#' @seealso [shapviz()]
#' @export
dimnames.shapviz <- function(x) {
  dimnames(get_shap_values(x))
}

#' Dimnames (Replacement Method) of "shapviz" Object
#'
#' This implies `colnames(x) <- ...`.
#'
#' @param x An object of class "shapviz".
#' @param value A list with rownames and column names compliant with SHAP matrix.
#' @returns Like `x`, but with replaced dimnames.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' x <- shapviz(S, X, baseline = 4)
#' dimnames(x) <- list(1:2, c("a", "b"))
#' dimnames(x)
#' colnames(x) <- c("x", "y")
#' colnames(x)
#' @seealso [shapviz()]
#' @export
`dimnames<-.shapviz` <- function(x, value) {
  # Matrix
  dimnames(x[["S"]]) <- value

  # data.frame (can't have NULL rownames)
  if (!is.null(value[[1L]])) {
    dimnames(x[["X"]]) <- value
  } else {
    dimnames(x[["X"]]) <- list(seq_len(nrow(x[["X"]])), value[[2L]])
  }

  # 3D array
  if (!is.null(get_shap_interactions(x))) {
    dimnames(x[["S_inter"]]) <- list(value[[1L]], value[[2L]], value[[2L]])
  }
  x
}

#' Rowbinds two "shapviz" Objects
#'
#' Rowbinds two "shapviz" objects using `+`.
#'
#' @param e1 The first object of class "shapviz".
#' @param e2 The second object of class "shapviz".
#' @returns A new object of class "shapviz".
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1]
#' s2 <- shapviz(S, X, baseline = 4)[2]
#' s <- s1 + s2
#' s
#' @seealso [shapviz()], [rbind.shapviz()]
#' @export
`+.shapviz` <- function(e1, e2) {
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
#' Rowbinds multiple "shapviz" objects based on the `+` operator.
#'
#' @param ... Any number of "shapviz" objects.
#' @returns A new object of class "shapviz".
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1]
#' s2 <- shapviz(S, X, baseline = 4)[2]
#' s <- rbind(s1, s2)
#' s
#' @seealso [shapviz()]
#' @export
rbind.shapviz <- function(...) {
  Reduce(`+`, list(...))
}

#' Concatenates "shapviz" Objects
#'
#' This function combines two or more (usually named) "shapviz" objects
#' to an object of class "mshapviz".
#'
#' @param ... Any number of (optionally named) "shapviz" objects.
#' @returns A "mshapviz" object.
#' @examples
#' S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' X <- data.frame(x = c("a", "b"), y = c(100, 10))
#' s1 <- shapviz(S, X, baseline = 4)[1]
#' s2 <- shapviz(S, X, baseline = 4)[2]
#' s <- c(shp1 = s1, shp2 = s2)
#' s
#' @export
#' @seealso [mshapviz()]
c.shapviz <- function(...) {
  mshapviz(list(...))
}

#' Splits "shapviz" Object
#'
#' Splits "shapviz" object along a vector `f` into an object of class "mshapviz".
#'
#' @param x Object of class "shapviz".
#' @param f Vector used to split feature values and SHAP (interaction) values.
#' @param ... Arguments passed to `split()`.
#' @returns A "mshapviz" object.
#' @examples
#' \dontrun{
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' sv <- shapviz(fit, X_pred = dtrain, X = iris)
#' mx <- split(sv, f = iris$Species)
#' sv_dependence(mx, "Petal.Length")
#' }
#' @export
#' @seealso [shapviz()], [rbind.shapviz()]
split.shapviz <- function(x, f, ...) {
  ind <- split(seq_len(nrow(x)), f = f, ...)
  mshapviz(lapply(ind, function(i) x[i, ]))
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
