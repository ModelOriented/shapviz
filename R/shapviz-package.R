#' @aliases shapviz-package
"_PACKAGE"

# Suppress R CMD check note
#' @importFrom xgboost xgb.train

globalVariables(c("from", "i", "id", "label", "to", "x", "shap", "SHAP",
                  "feature", "value", "color", "Var2", "Var3", "S"))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.shapviz <- list(
    shapviz.viridis_args = list(begin = 0.25, end = 0.85, option = "inferno"),
    shapviz.format_shap = function(z) prettyNum(z, digits = 3, scientific = FALSE),
    shapviz.format_feat = function(z) prettyNum(z, digits = 3, scientific = FALSE)
  )
  toset <- !(names(op.shapviz) %in% names(op))
  if (any(toset)) {
    options(op.shapviz[toset])
  }
  invisible()
}
