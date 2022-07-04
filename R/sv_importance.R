#' SHAP Importance Plots
#'
#' This function provides two types of SHAP importance plots: a bar plot
#' and a beeswarm plot (sometimes called "SHAP summary plot").
#' The bar plot shows SHAP feature importances, calculated as the average absolute SHAP
#' value per feature.
#' The beeswarm plot displays SHAP values per feature, using min-max
#' scaled feature values on the color axis. Non-numeric features are transformed
#' to numeric by calling \code{data.matrix()} first.
#' For both types of plots, the features are sorted in decreasing
#' order of importance. The two types of plots can also be combined.
#'
#' @param object An object of class "shapviz".
#' @param kind Should a "bar" plot (the default), a "beeswarm" plot, or "both" be shown?
#' Set to "no" in order to suppress plotting. In that case, the sorted
#' SHAP feature importances of all variables are returned.
#' @param max_display Maximum number of features (with highest importance)
#' should be plotted? If there are more, the least important variables are collapsed:
#' their SHAP values are added and their min-max-scaled feature values are added as
#' well (and the resulting vector is min-max-scaled again). Set to \code{Inf} to show
#' all features. Has no effect if \code{kind = "no"}.
#' @param fill Color used to fill the bars (only used if bar plots are shown).
#' @param viridis_args List of viridis color scale arguments used to control the
#' coloring of the beeswarm plot, see \code{?ggplot2::scale_color_viridis_c()}.
#' The default points to the global option \code{shapviz.viridis_args}, which
#' corresponds to \code{list(begin = 0.25, end = 0.85, option = "inferno")}.
#' These values are passed to \code{ggplot2::scale_color_viridis_c()}.
#' For example, to switch to a standard viridis scale, you can either change the default
#' with \code{options(shapviz.viridis_args = NULL)} or set \code{viridis_args = NULL}.
#' @param show_numbers Should SHAP feature importances be printed?
#' Default is \code{TRUE} for \code{kind = "bar"} and \code{FALSE} otherwise.
#' @param format_fun Function used to format SHAP feature importances
#' if \code{show_numbers = TRUE}.
#' @param number_size Text size of the formatted numbers.
#' @param ... Arguments passed to \code{geom_bar()} (if \code{kind = "bar"}) or
#' to \code{ggbeeswarm::geom_quasirandom()} otherwise.
#' For instance, passing \code{alpha = 0.2} will produce semi-transparent beeswarms,
#' setting \code{size = 3} will produce larger dots, or \code{width = 0.2} will
#' produce less wide swarms.
#' @return A \code{ggplot} object representing an importance plot, or - if
#' \code{kind = "no"} - a named numeric vector of sorted SHAP feature importances.
#' @export
#' @examples
#' X_train <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_train, label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = X_train)
#' sv_importance(x)
#' sv_importance(x, kind = "beeswarm", show_numbers = TRUE)
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
sv_importance.shapviz <- function(object, kind = c("bar", "beeswarm", "both", "no"),
                                  max_display = 10L, fill = "#fca50a",
                                  viridis_args = getOption("shapviz.viridis_args"),
                                  show_numbers = kind == "bar",
                                  format_fun = format_aligned, number_size = 3.2, ...) {
  stopifnot("format_fun must be a function" = is.function(format_fun))
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
      ylab("mean(|SHAP value|)")
  } else {
    # Transpose S and X_scaled
    S <- as.data.frame(S)
    stopifnot(colnames(S) == colnames(X_scaled)) # to be absolutely sure...
    shap_long <- utils::stack(S)
    shap_long$v <- utils::stack(X_scaled)$values

    # Put together color scale and deal with special case of only one unique v value
    nv <- length(unique(shap_long$v))
    viridis_args <- c(
      viridis_args,
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
    p <- p +
      geom_hline(yintercept = 0, color = "darkgray") +
      ggbeeswarm::geom_quasirandom(aes(color = v), ...) +
      scale_y_continuous(expand = expansion(mult = c(0.05 + 0.1 * show_numbers, 0.05))) +
      do.call(scale_color_viridis_c, viridis_args) +
      labs(y = "SHAP value", color = "Feature value")
  }
  if (show_numbers) {
    p <- p +
      geom_text(
        data = imp_df,
        aes(y = if (kind == "bar") 0 else -Inf, label = format_fun(values)),
        hjust = -0.2,
        size = number_size
      )
  }
  p +
    theme(axis.ticks.y = element_blank()) +
    xlab(element_blank()) +
    coord_flip(clip = "off")
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

#' Aligned Number Formatter
#'
#' Formats a vector of positive numbers in a way that the resulting strings are
#' all of the same length and the decimal points (if there are any) are aligned.
#'
#' @param x A numeric vector to be formatted.
#' @param digits Number of digits to be shown, including the zero in front of the decimal dot.
#' @param scientific Should scientific formatting be applied? Default is \code{FALSE}.
#' @param ... Further arguments passed to \code{format()}.
#' @return A character vector of formatted numbers.
#' @export
#' @examples
#' x <- c(10, 1, 0.1)
#' format_aligned(x)
format_aligned <- function(x, digits = 4, scientific = FALSE, ...) {
  stopifnot(all(x >= 0))
  mx <- trunc(log10(max(x))) + 1L
  format(round(x, pmax(0L, digits - mx)), scientific = scientific, ...)
}
