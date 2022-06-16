#' SHAP Importance Plots
#'
#' This function provides two types of SHAP importance plots: a beeswarm plot (the default)
#' and a bar plot. The beeswarm plot displays SHAP values per feature, using min-max
#' scaled feature values on the color scale (non-numeric features are transformed
#' to numeric by calling \code{data.matrix()} first).
#' The bar plot shows average absolute SHAP values. Both types can be combined.
#'
#' The continuous viridis color scale of the beeswarm plot is determined by the
#' \code{shapviz.viridis_args} option with default
#' \code{list(begin = 0.25, end = 0.85, option = "inferno")}.
#' These values are passed to \code{ggplot2::scale_color_viridis_c()}}.
#' To switch to a reverted standard viridis scale, you would run
#' \code{options(shapviz.viridis_args = list(option = "viridis", direction = -1))}.
#' Check \code{?ggplot2::scale_color_viridis_c()} for all possible arguments.
#' Since a "ggplot" object is returned, you can also overwrite the default color scale
#' by adding another one.
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
#' For instance, passing \code{alpha = 0.2} will produce semi-transparent beeswarms,
#' setting \code{size = 3} will produce larger dots, or \code{width = 0.2} will
#' produce less wide swarms.
#' @return A \code{ggplot} object representing an importance plot or - if \code{kind = "no"} a named numeric vector of mean absolute SHAP values sorted in decreasing order.
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
  X_scaled <- X <- get_feature_values(object)
  imp <- .get_imp(S)
  if (kind == "no") {
    return(imp)
  }
  X_scaled[] <- apply(data.matrix(X), 2L, FUN = .min_max_scale, simplify = FALSE)

  # Collapse unimportant features (here, it is important that 'imp' is sorted)
  ok <- utils::head(names(imp), max_display - 1L)
  if (length(ok) < ncol(X) - 1L) {
    bad <- setdiff(colnames(X), ok)
    nn <- paste("Sum of", length(bad), "other")

    X_scaled_bad <- rowSums(X_scaled[bad])
    X_scaled <- X_scaled[ok]
    X_scaled[[nn]] <- .min_max_scale(X_scaled_bad)

    S <- cbind(S[, ok, drop = FALSE], rowSums(S[, bad, drop = FALSE]))
    colnames(S) <- c(ok, nn)
    imp <- .get_imp(S)
  }

  # Here, sort order of imp is irrelevant. stats::reorder will care about the ordering
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

  # Put together color scale and deal with special case of only one unique v value
  nv <- length(unique(shap_long$v))
  viridis_args <- c(
    getOption("shapviz.viridis_args"),
    list(
      breaks = if (nv >= 2L) 0:1 else 0.5,
      labels = if (nv >= 2L) c("Low", "High") else "Avg",
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

.get_imp <- function(z) {
  sort(colMeans(abs(z)), decreasing = TRUE)
}

