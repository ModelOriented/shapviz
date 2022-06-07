#' SHAP Importance Plots
#'
#' This function provides two types of SHAP importance plots: a beeswarm plot (the default)
#' and a bar plot. The beeswarm plot displays SHAP values per feature, using min-max
#' scaled feature values on the color scale (non-numeric features are transformed
#' to numeric by calling \code{data.matrix()} first).
#' The bar plot shows average absolute SHAP values. Both types can be combined.
#'
#' @param object An object of class "shapviz".
#' @param kind Should a "beeswarm" plot (the default), a "bar" plot or "both" be shown?
#' Set to "no" in order to suppress plotting. In that case, the sorted
#' average absolute SHAP values of all variables are returned.
#' @param max_display Maximum number of features (with largest mean absolute SHAP value)
#' should be plotted? If there are more, the most unimportant variables are collapsed:
#' their SHAP values are added and their min-max-scaled feature values are added as
#' well (and the resulting vector is min-max-scaled again). Set to \code{Inf} to show
#' all features.
#' @param fill Color used to fill the bars (only used if \code{kind = "bar"}).
#' @param format_fun Function used to format mean absolute SHAP values shown on the
#' bar plot. Use \code{format_fun = function(z) = ""} to suppress printing.
#' @param number_size Text size of the formatted numbers (only if \code{kind = "bar"}).
#' @param ... Arguments passed to \code{geom_bar()} (if \code{kind = "bar"}) or
#' to \code{ggbeeswarm::geom_quasirandom()} otherwise.
#' For instance, passing \code{alpha = 0.2} will produce semi-transparent beeswarms, or
#' setting \code{size = 3} will produce larger beeswarm dots.
#' @return A \code{ggplot} object or - if \code{kind = "no"} a named vector.
#' @export
#' @examples
#' X_train <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_train, label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = X_train)
#' sv_importance(x)
#' sv_importance(x, kind = "bar")
#' sv_importance(x, kind = "both")
#' sv_importance(x, kind = "no")
sv_importance <- function(object, ...) {
  UseMethod("sv_importance")
}

#' @describeIn sv_importance Default method.
#' @export
sv_importance.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_importance SHAP importance plot for an object of class "shapviz".
#' @export
sv_importance.shapviz <- function(object, kind = c("beeswarm", "bar", "both", "no"),
                                  max_display = 10L, fill = "#fca50a",
                                  format_fun = function(z)
                                    prettyNum(z, digits = 3, scientific = FALSE),
                                  number_size = 3.2, ...) {
  kind <- match.arg(kind)
  S <- get_shap_values(object)
  X <- get_feature_values(object)
  imp <- .get_imp(S)
  if (kind == "no") {
    return(imp)
  }
  X_scaled <- as.data.frame(apply(data.matrix(X), 2L, .min_max_scale))

  # Collapse unimportant features
  ok <- utils::head(names(imp), max_display - 1L)
  if (length(ok) < ncol(X) - 1L) {
    bad <- setdiff(names(X), ok)
    nn <- paste("Sum of", length(bad), "other")

    X_scaled_bad <- rowSums(X_scaled[bad])
    X_scaled <- X_scaled[ok]
    X_scaled[[nn]] <- .min_max_scale(X_scaled_bad)

    S <- cbind(S[, ok, drop = FALSE], rowSums(S[, bad, drop = FALSE]))
    colnames(S) <- c(ok, nn)
    imp <- .get_imp(S, sort_results = FALSE)
  }

  imp_df <- data.frame(ind = names(imp), values = imp)

  if (kind == "bar") {
    p <- ggplot(imp_df, aes(x = stats::reorder(ind, values), y = values)) +
      geom_bar(fill = fill, stat = "identity", ...) +
      geom_text(
        aes(y = 0, label = format_fun(values)), hjust = -0.2, size = number_size
      ) +
      coord_flip() +
      labs(x = element_blank(), y = "mean(|SHAP value|)")
    return(p)
  }

  # Transpose S and X_scaled
  S <- as.data.frame(S)
  stopifnot(colnames(S) == colnames(X_scaled)) # to be absolutely sure...
  shap_long <- utils::stack(S)
  shap_long$v <- utils::stack(X_scaled)$values

  viridis_args <- c(
    getOption("shapviz.viridis_args"),
    list(
      breaks = 0:1,
      labels = c("Low", "High"),
      guide = guide_colorbar(
        barwidth = 0.4,
        barheight = 8,
        title.theme = element_text(angle = 90, hjust = 0.5, vjust = 0),
        title.position = "left"
      )
    )
  )

  p <- ggplot(shap_long, aes(x = stats::reorder(ind, abs(values)), y = values))
  if (kind == "both") {
    p <- p + geom_bar(data = imp_df, fill = fill, stat = "identity")
  }
  p +
    geom_hline(yintercept = 0, color = "darkgray") +
    ggbeeswarm::geom_quasirandom(aes(color = v), ...) +
    coord_flip(clip = "off") +
    do.call(scale_color_viridis_c, viridis_args) +
    labs(x = element_blank(), y = "SHAP value", color = "Feature value")
}

# Helper functions
.min_max_scale <- function(z, na.rm = TRUE) {
  r <- range(z, na.rm = na.rm)
  d <- diff(r)
  if (is.na(d) || d == 0) {
    z[!is.na(z)] <- 0.5
    return(z)
  }
  return((z - r[1L]) /(r[2L] - r[1L]))
}

.get_imp <- function(z, sort_results = TRUE) {
  out <- colMeans(abs(z))
  if (sort_results) {
    out <- sort(out, decreasing = TRUE)
  }
  out
}

