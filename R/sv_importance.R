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
#' should be plotted? If there are more and \code{show_other = TRUE},
#' the least important variables are collapsed:
#' their SHAP values are added and their min-max-scaled feature values are added as
#' well (and the resulting vector is min-max-scaled again). Set to \code{Inf} to show
#' all features. Has no effect if \code{kind = "no"}.
#' @param show_other If the number of features is larger than \code{max_display}:
#' Should the "other" group be shown (default) or not?
#' @param fill Color used to fill the bars (only used if bars are shown).
#' @param bar_width Relative width of the bars (only used if bars are shown).
#' @param bee_width Relative width of the beeswarms (only used if beeswarm shown).
#' @param bee_adjust Relative bandwidth adjustment factor used in
#' estimating the density of the beeswarms (only used if beeswarm shown).
#' @param viridis_args List of viridis color scale arguments used to control the
#' coloring of the beeswarm plot, see \code{?ggplot2::scale_color_viridis_c()}.
#' The default points to the global option \code{shapviz.viridis_args}, which
#' corresponds to \code{list(begin = 0.25, end = 0.85, option = "inferno")}.
#' These values are passed to \code{ggplot2::scale_color_viridis_c()}.
#' For example, to switch to a standard viridis scale, you can either change the default
#' with \code{options(shapviz.viridis_args = NULL)} or set \code{viridis_args = NULL}.
#' @param color_bar_title Title of color bar of the beeswarm plot.
#' Set to \code{NULL} to hide the color bar altogether.
#' @param show_numbers Should SHAP feature importances be printed?
#' Default is \code{FALSE}.
#' @param format_fun Function used to format SHAP feature importances
#' (only if \code{show_numbers = TRUE}). To change to scientific notation, use e.g.
#' \code{function(x) = prettyNum(x, scientific = TRUE)}.
#' @param number_size Text size of the numbers (if \code{show_numbers = TRUE}).
#' @param ... Arguments passed to \code{geom_bar()} (if \code{kind = "bar"}) or
#' to \code{geom_point()} otherwise.
#' For instance, passing \code{alpha = 0.2} will produce semi-transparent beeswarms,
#' and setting \code{size = 3} will produce larger dots.
#' @return A "ggplot" object representing an importance plot, or - if
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
#'
#' X <- data.frame(matrix(rnorm(1000), ncol = 20))
#' S <- as.matrix(X)
#' x2 <- shapviz(S, X)
#' sv_importance(x2)
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
                                  max_display = 15L, show_other = TRUE,
                                  fill = "#fca50a", bar_width = 2/3,
                                  bee_width = 0.4, bee_adjust = 0.5,
                                  viridis_args = getOption("shapviz.viridis_args"),
                                  color_bar_title = "Feature value",
                                  show_numbers = FALSE, format_fun = format_max,
                                  number_size = 3.2, ...) {
  stopifnot("format_fun must be a function" = is.function(format_fun))
  kind <- match.arg(kind)
  if ("width" %in% names(list(...))) {
    # To be removed in 0.5.0
    warning("Passing 'width' via ... is deprecated. Use 'bar_width' or 'bee_width'.")
  }

  S <- get_shap_values(object)
  X <- get_feature_values(object)
  imp <- .get_imp(S)
  if (kind == "no") {
    return(imp)
  }

  # The next two lines would be more elegant, but require R >= 4.1
  # X_scaled <- X
  # X_scaled[] <- apply(data.matrix(X), 2L, FUN = .min_max_scale, simplify = FALSE)
  X_tmp <- apply(data.matrix(X), 2L, FUN = .min_max_scale)
  X_scaled <- as.data.frame(if (nrow(X) >= 2L) X_tmp else t(X_tmp))

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

  imp_df <- data.frame(feature = stats::reorder(names(imp), imp), value = imp)

  is_bar <- kind == "bar"
  if (is_bar) {
    p <- ggplot(imp_df, aes(x = feature, y = value)) +
      geom_bar(fill = fill, width = bar_width, stat = "identity", ...) +
      labs(x = element_blank(), y = "mean(|SHAP value|)")
  } else {
    # Transpose S and X_scaled
    S <- as.data.frame(S)
    stopifnot(colnames(S) == colnames(X_scaled)) # to be absolutely sure...
    S_long <- utils::stack(S)
    df <- data.frame(
      feature = stats::reorder(S_long$ind, abs(S_long$values)),
      value = S_long$values,
      color = utils::stack(X_scaled)$values
    )

    # Put together color scale and deal with special case of only one unique v value
    if (!is.null(color_bar_title)) {
      nv <- length(unique(df$color))
      viridis_args_plus <-
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
    } else {
      viridis_args_plus <- list(guide = "none")
    }

    p <- ggplot(df, aes(x = feature, y = value))
    if (kind == "both") {
      p <- p +
        geom_bar(data = imp_df, fill = fill, width = bar_width, stat = "identity")
    }
    p <- p +
      geom_hline(yintercept = 0, color = "darkgray") +
      geom_point(
        aes(color = color),
        position = position_bee(width = bee_width, adjust = bee_adjust),
        ...
      ) +
      do.call(scale_color_viridis_c, c(viridis_args, viridis_args_plus)) +
      labs(x = element_blank(), y = "SHAP value", color = color_bar_title)
  }
  if (show_numbers) {
    p <- p +
      geom_text(
        data = imp_df,
        aes(
          y = if (is_bar) value + max(value) / 60 else
            min(df$value) - diff(range(df$value)) / 20,
          label = format_fun(value)
        ),
        hjust = !is_bar,
        size = number_size
      ) +
      scale_y_continuous(
        expand = expansion(mult = 0.05 + c(0.12 *!is_bar, 0.09 * is_bar))
      )
  }
  p + coord_flip(clip = "off")
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

#' Number Formatter
#'
#' Formats a numeric vector in a way that its largest absolute value determines
#' the number of digits after the decimal separator. This function is helpful in
#' perfectly aligning numbers on plots. Does not use scientific formatting.
#'
#' @param x A numeric vector to be formatted.
#' @param digits Number of significant digits of the largest absolute value.
#' @param ... Further arguments passed to \code{format()}, e.g., \code{big.mark = "'"}.
#' @return A character vector of formatted numbers.
#' @export
#' @examples
#' x <- c(100, 1, 0.1)
#' format_max(x)
#'
#' y <- c(100, 1.01)
#' format_max(y)
#' format_max(y, digits = 5)
format_max <- function(x, digits = 4L, ...) {
  mx <- trunc(log10(max(abs(x)))) + 1L
  x_rounded <- round(x, pmax(0L, digits - mx))
  format(x_rounded, scientific = FALSE, trim = TRUE, ...)
}
