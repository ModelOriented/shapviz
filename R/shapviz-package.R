#' @aliases shapviz-package
"_PACKAGE"

#' @import ggplot2
NULL

globalVariables(c("from", "i", "id", "ind", "label", "to", "v", "values", "x", "shap"))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.shapviz <- list(
    shapviz.viridis_args = list(begin = 0.25, end = 0.85, option = "inferno")
  )
  toset <- !(names(op.shapviz) %in% names(op))
  if (any(toset)) {
    options(op.shapviz[toset])
  }
  invisible()
}
