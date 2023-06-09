#' SHAP Importance Plots
#'
#' This function provides two types of SHAP importance plots: a bar plot
#' and a beeswarm plot (sometimes called "SHAP summary plot").
#' The two types of plots can also be combined.
#'
#' The bar plot shows SHAP feature importances, calculated as the average absolute SHAP
#' value per feature. The beeswarm plot displays SHAP values per feature, using min-max
#' scaled feature values on the color axis. Non-numeric features are transformed
#' to numeric by calling [data.matrix()] first. For both types of plots, the features
#' are sorted in decreasing order of importance.
#'
#' @param object An object of class "(m)shapviz".
#' @param kind Should a "bar" plot (the default), a "beeswarm" plot, or "both" be shown?
#'   Set to "no" in order to suppress plotting. In that case, the sorted
#'   SHAP feature importances of all variables are returned.
#' @param max_display Maximum number of features (with highest importance) to plot.
#'   Set to `Inf` to show all features. Has no effect if `kind = "no"`.
#' @param fill Color used to fill the bars (only used if bars are shown).
#' @param bar_width Relative width of the bars (only used if bars are shown).
#' @param bee_width Relative width of the beeswarms.
#' @param bee_adjust Relative bandwidth adjustment factor used in
#'   estimating the density of the beeswarms.
#' @param viridis_args List of viridis color scale arguments. The default points to the
#'   global option `shapviz.viridis_args`, which corresponds to
#'   `list(begin = 0.25, end = 0.85, option = "inferno")`. These values are passed to
#'   [ggplot2::scale_color_viridis_c()]. For example, to switch to standard viridis,
#'   either change the default with `options(shapviz.viridis_args = list())` or set
#'   `viridis_args = list()`.
#' @param color_bar_title Title of color bar of the beeswarm plot. Set to `NULL`
#'   to hide the color bar altogether.
#' @param show_numbers Should SHAP feature importances be printed? Default is `FALSE`.
#' @param format_fun Function used to format SHAP feature importances
#'   (only if `show_numbers = TRUE`). To change to scientific notation, use
#'   `function(x) = prettyNum(x, scientific = TRUE)`.
#' @param number_size Text size of the numbers (if `show_numbers = TRUE`).
#' @param ... Arguments passed to [ggplot2::geom_bar()] (if `kind = "bar"`) or to
#'   [ggplot2::geom_point()] otherwise. For instance, passing `alpha = 0.2` will produce
#'   semi-transparent beeswarms, and setting `size = 3` will produce larger dots.
#' @returns
#'   A "ggplot" (or "patchwork") object representing an importance plot, or - if
#'   `kind = "no"` - a named numeric vector of sorted SHAP feature importances
#'   (or a list of such vectors in case of an object of class "mshapviz").
#' @examples
#' \dontrun{
#' X_train <- data.matrix(iris[, -1])
#' dtrain <- xgboost::xgb.DMatrix(X_train, label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 10, nthread = 1)
#' x <- shapviz(fit, X_pred = X_train)
#' sv_importance(x)
#' sv_importance(x, kind = "no")
#' sv_importance(x, kind = "beeswarm", show_numbers = TRUE)
#' }
#'
#' @seealso \code{\link{sv_interaction}}
#' @export
sv_importance <- function(object, ...) {
  UseMethod("sv_importance")
}

#' @describeIn sv_importance
#'   Default method.
#' @export
sv_importance.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_importance
#'   SHAP importance plot for an object of class "shapviz".
#' @export
sv_importance.shapviz <- function(object, kind = c("bar", "beeswarm", "both", "no"),
                                  max_display = 15L, fill = "#fca50a", bar_width = 2/3,
                                  bee_width = 0.4, bee_adjust = 0.5,
                                  viridis_args = getOption("shapviz.viridis_args"),
                                  color_bar_title = "Feature value",
                                  show_numbers = FALSE, format_fun = format_max,
                                  number_size = 3.2, ...) {
  stopifnot("format_fun must be a function" = is.function(format_fun))
  kind <- match.arg(kind)
  imp <- .get_imp(get_shap_values(object))

  if (kind == "no") {
    return(imp)
  }

  # Deal with too many features
  if (ncol(object) > max_display) {
    imp <- imp[seq_len(max_display)]
  }
  ord <- names(imp)
  object <- object[, ord]  # not required for kind = "bar"

  # ggplot will need to work with data.frame
  imp_df <- data.frame(feature = factor(ord, rev(ord)), value = imp)
  is_bar <- kind == "bar"
  if (is_bar) {
    p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = value, y = feature)) +
      ggplot2::geom_bar(fill = fill, width = bar_width, stat = "identity", ...) +
      ggplot2::labs(x = "mean(|SHAP value|)", y = ggplot2::element_blank())
  } else {
    # Prepare data.frame for beeswarm plot
    S <- get_shap_values(object)
    X <- .scale_X(get_feature_values(object))
    df <- transform(
      as.data.frame.table(S, responseName = "value"),
      feature = factor(Var2, levels = rev(ord)),
      color = as.data.frame.table(X)$Freq
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = feature))
    if (kind == "both") {
      p <- p +
        ggplot2::geom_bar(
          data = imp_df, fill = fill, width = bar_width, stat = "identity"
        )
    }
    p <- p +
      ggplot2::geom_vline(xintercept = 0, color = "darkgray") +
      ggplot2::geom_point(
        ggplot2::aes(color = color),
        position = position_bee(width = bee_width, adjust = bee_adjust),
        ...
      ) +
      .get_color_scale(
        viridis_args = viridis_args,
        bar = !is.null(color_bar_title),
        ncol = length(unique(df$color))   # Special case of constant feature values
      ) +
      ggplot2::labs(
        x = "SHAP value", y = ggplot2::element_blank(), color = color_bar_title
      ) +
      ggplot2::theme(legend.box.spacing = grid::unit(0, "pt"))
  }
  if (show_numbers) {
    p <- p +
      ggplot2::geom_text(
        data = imp_df,
        ggplot2::aes(
          x = if (is_bar) value + max(value) / 60 else
            min(df$value) - diff(range(df$value)) / 20,
          label = format_fun(value)
        ),
        hjust = !is_bar,
        size = number_size
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = 0.05 + c(0.12 *!is_bar, 0.09 * is_bar))
      )
  }
  p
}

#' @describeIn sv_importance
#'   SHAP importance plot for an object of class "mshapviz".
#' @export
sv_importance.mshapviz <- function(object, kind = c("bar", "beeswarm", "both", "no"),
                                   max_display = 15L, fill = "#fca50a", bar_width = 2/3,
                                   bee_width = 0.4, bee_adjust = 0.5,
                                   viridis_args = getOption("shapviz.viridis_args"),
                                   color_bar_title = "Feature value",
                                   show_numbers = FALSE, format_fun = format_max,
                                   number_size = 3.2, ...) {
  kind <- match.arg(kind)

  plot_list <- lapply(
    object,
    FUN = sv_importance,
    # Argument list (simplify via match.call() or some rlang magic?)
    kind = kind,
    max_display = max_display,
    fill = fill,
    bar_width = bar_width,
    bee_width = bee_width,
    bee_adjust = bee_adjust,
    viridis_args = viridis_args,
    color_bar_title = color_bar_title,
    show_numbers = show_numbers,
    format_fun = format_fun,
    number_size = number_size,
    ...
  )
  if (kind == "no") {
    return(plot_list)
  }
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list)
}

# Helper functions
.min_max_scale <- function(z, na.rm = TRUE) {
  r <- range(z, na.rm = na.rm)
  d <- diff(r)
  if (is.na(d) || d == 0) {
    z[!is.na(z)] <- 0.5
    return(z)
  }
  (z - r[1L]) /(r[2L] - r[1L])
}

.get_imp <- function(z) {
  sort(colMeans(abs(z)), decreasing = TRUE)
}

.scale_X <- function(X) {
  X_scaled <- apply(data.matrix(X), 2L, FUN = .min_max_scale)
  if (nrow(X) == 1L) t(X_scaled) else X_scaled
}

# ncol < 2 treats the special case of constant feature values (e.g., if n = 1)
.get_color_scale <- function(viridis_args, bar = TRUE, ncol = 2L) {
  if (bar) {
    viridis_args_plus <-
      list(
        breaks = if (ncol >= 2L) 0:1 else 0.5,
        labels = if (ncol >= 2L) c("Low", "High") else "Avg",
        guide = ggplot2::guide_colorbar(
          barwidth = 0.4,
          barheight = 8,
          title.theme = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0),
          title.position = "left"
        )
      )
  } else {
    viridis_args_plus <- list(guide = "none")
  }
  return(do.call(ggplot2::scale_color_viridis_c, c(viridis_args, viridis_args_plus)))
}


#' Number Formatter
#'
#' Formats a numeric vector in a way that its largest absolute value determines
#' the number of digits after the decimal separator. This function is helpful in
#' perfectly aligning numbers on plots. Does not use scientific formatting.
#'
#' @param x A numeric vector to be formatted.
#' @param digits Number of significant digits of the largest absolute value.
#' @param ... Further arguments passed to [format()], e.g., `big.mark = "'"`.
#' @returns A character vector of formatted numbers.
#' @export
#' @examples
#' x <- c(100, 1, 0.1)
#' format_max(x)
#'
#' y <- c(100, 1.01)
#' format_max(y)
#' format_max(y, digits = 5)
format_max <- function(x, digits = 4L, ...) {
  mx <- trunc(log10(max(abs(x), na.rm = TRUE))) + 1L
  x_rounded <- round(x, pmax(0L, digits - mx))
  format(x_rounded, scientific = FALSE, trim = TRUE, ...)
}
