#' SHAP Waterfall Plot
#'
#' Creates a waterfall plot of SHAP values of one observation. If multiple
#' observations are selected, their SHAP values and predictions are averaged.
#'
#' f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the
#' baseline SHAP value.
#'
#' @param object An object of class "(m)shapviz".
#' @param row_id Subset of observations to plot, typically a single row number.
#'   If more than one row is selected, SHAP values are averaged, and feature values
#'   are shown only when they are unique.
#' @param max_display Maximum number of features (with largest absolute SHAP values)
#'   should be plotted? If there are more features, they will be collapsed to one
#'   feature. Set to `Inf` to show all features.
#' @param order_fun Function specifying the order of the variables/SHAP values.
#'   It maps the vector `s` of SHAP values to sort indices from 1 to `length(s)`.
#'   The default is `function(s) order(abs(s))`. To plot without sorting, use
#'   `function(s) 1:length(s)` or `function(s) length(s):1`.
#' @param fill_colors A vector of exactly two fill colors: the first for positive
#'   SHAP values, the other for negative ones.
#' @param format_shap Function used to format SHAP values. The default uses the
#'   global option `shapviz.format_shap`, which equals to
#'   `function(z) prettyNum(z, digits = 3, scientific = FALSE)` by default.
#' @param format_feat Function used to format numeric feature values. The default uses
#'   the global option `shapviz.format_feat`, which equals to
#'   `function(z) prettyNum(z, digits = 3, scientific = FALSE)` by default.
#' @param contrast Logical flag that detemines whether to use white text in dark arrows.
#'   Default is `TRUE`.
#' @param show_connection Should connecting lines be shown? Default is `TRUE`.
#' @param show_annotation Should "f(x)" and "E(f(x))" be plotted? Default is `TRUE`.
#' @param annotation_size Size of the annotation text (f(x)=... and E(f(x))=...).
#' @param ... Arguments passed to [ggfittext::geom_fit_text()].
#'   For example, `size = 9` will use fixed text size in the bars and `size = 0`
#'   will altogether suppress adding text to the bars.
#' @returns An object of class "ggplot" (or "patchwork") representing a waterfall plot.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 20, nthread = 1)
#' x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
#' sv_waterfall(x)
#' sv_waterfall(x, row_id = 123, max_display = 2, size = 9, fill_colors = 4:5)
#'
#' \dontrun{
#' # Ordered by colnames(x), combined with max_display
#' sv_waterfall(
#'   x[, sort(colnames(x))], order_fun = function(s) length(s):1, max_display = 3
#' )
#'
#' # Aggregate over all observations with Petal.Length == 1.4
#' sv_waterfall(x, row_id = x$X$Petal.Length == 1.4)
#' }
#' @export
#' @seealso [sv_force()]
sv_waterfall <- function(object, ...) {
  UseMethod("sv_waterfall")
}

#' @describeIn sv_waterfall
#'   Default method.
#' @export
sv_waterfall.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_waterfall
#'   SHAP waterfall plot for an object of class "shapviz".
#' @export
sv_waterfall.shapviz <- function(object, row_id = 1L, max_display = 10L,
                                 order_fun = function(s) order(abs(s)),
                                 fill_colors = c("#f7d13d", "#a52c60"),
                                 format_shap = getOption("shapviz.format_shap"),
                                 format_feat = getOption("shapviz.format_feat"),
                                 contrast = TRUE, show_connection = TRUE,
                                 show_annotation = TRUE, annotation_size = 3.2, ...) {
  stopifnot(
    "Exactly two fill colors must be passed" = length(fill_colors) == 2L,
    "format_shap must be a function" = is.function(format_shap),
    "format_feat must be a function" = is.function(format_feat),
    "order_fun must be a function" = is.function(order_fun)
  )
  object <- object[row_id, ]
  b <- get_baseline(object)
  dat <- .make_dat(object, format_feat = format_feat, sep = " = ")
  if (ncol(object) > max_display) {
    dat <- .collapse(dat, max_display = max_display)
  }
  m <- nrow(dat)

  # Add order dependent columns
  dat <- dat[order_fun(dat$S), ]
  dat$i <- seq_len(m)
  dat$to <- cumsum(dat$S) + b
  dat$from <- .lag(dat$to, default = b)

  # Make a waterfall plot
  height <- grid::unit(1 / (1 + 2 * m), "npc")

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      xmin = from,
      xmax = to,
      y = stats::reorder(label, i),
      fill = factor(to < from, levels = c(FALSE, TRUE))
    )
  ) +
    gggenes::geom_gene_arrow(
      show.legend = FALSE,
      arrowhead_width = grid::unit(2, "mm"),
      arrowhead_height = height,
      arrow_body_height = height
    ) +
    ggfittext::geom_fit_text(
      ggplot2::aes(label = paste0(ifelse(S > 0, "+", ""), format_shap(S))),
      show.legend = FALSE,
      contrast = contrast,
      ...
    ) +
    ggplot2::scale_fill_manual(values = fill_colors, drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(y = ggplot2::element_blank(), x = "SHAP value")

  if (show_connection) {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(x = to, xend = to, y = i, yend = .lag(i, lead = TRUE, default = m)),
        linewidth = 0.3,
        linetype = 2
      )
  }

  if (show_annotation) {
    full_range <- c(dat[m, "to"], dat[1L, "from"])
    p <- p +
      ggplot2::annotate(
        "segment",
        x = full_range,
        xend = full_range,
        y = c(m, 1),
        yend = c(m, 1) + m * c(0.075, -0.075) + 0.13 * c(1, -1),
        linewidth = 0.3,
        linetype = 2
      ) +
      ggplot2::annotate(
        "text",
        x = full_range,
        y = c(m, 1) + m * c(0.1, -0.1) + 0.15 * c(1, -1),
        label = paste0(c("f(x)=", "E[f(x)]="), format_shap(full_range)),
        size = annotation_size
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.3, mult = 0.2)) +
      ggplot2::coord_cartesian(clip = "off")
  }
  p
}

#' @describeIn sv_waterfall
#'   SHAP waterfall plot for an object of class "mshapviz".
#' @export
sv_waterfall.mshapviz <- function(object, row_id = 1L, max_display = 10L,
                                  order_fun = function(s) order(abs(s)),
                                  fill_colors = c("#f7d13d", "#a52c60"),
                                  format_shap = getOption("shapviz.format_shap"),
                                  format_feat = getOption("shapviz.format_feat"),
                                  contrast = TRUE, show_connection = TRUE,
                                  show_annotation = TRUE, annotation_size = 3.2, ...) {
  plot_list <- lapply(
    object,
    FUN = sv_waterfall,
    # Argument list (simplify via match.call() or some rlang magic?)
    row_id = row_id,
    max_display = max_display,
    order_fun = order_fun,
    fill_colors = fill_colors,
    format_shap = format_shap,
    format_feat = format_feat,
    contrast = contrast,
    show_connection = show_connection,
    show_annotation = show_annotation,
    annotation_size = annotation_size,
    ...
  )
  plot_list <- add_titles(plot_list, nms = names(object))
  patchwork::wrap_plots(plot_list)
}

# Helper functions for sv_waterfall() and sv_force()
.lag <- function(z, default = NA, lead = FALSE) {
  n <- length(z)
  if (n < 2L) {
    return(rep(default, times = n))
  }
  if (isTRUE(lead)) {
    return(c(z[2L:n], default))
  }
  c(default, z[1L:(n - 1L)])
}

# Turns "shapviz" object into a two-column data.frame
.make_dat <- function(object, format_feat, sep = " = ") {
  X <- get_feature_values(object)
  S <- get_shap_values(object)
  if (nrow(object) == 1L) {
    S <- drop(S)
    label <- paste(colnames(X), format_feat(X), sep = sep)
  } else {
    message("Aggregating SHAP values over ", nrow(object), " observations")
    S <- colMeans(S)
    J <- vapply(X, function(z) length(unique(z)) <= 1L, FUN.VALUE = TRUE)
    label <- colnames(X)
    if (any(J)) {
      label[J] <- paste(label[J], format_feat(X[1L, J]), sep = sep)
    }
  }
  data.frame(S = S, label = label)
}

# Used to combine unimportant rows in dat. dat has two columns: S and label
# Note: rownames(dat) = colnames(object)
.collapse <- function(dat, max_display) {
  m_drop <- nrow(dat) - max_display + 1L
  drop_cols <- rownames(dat)[order(abs(dat$S))[seq_len(m_drop)]]
  keep_cols <- setdiff(rownames(dat), drop_cols)
  rbind(
    dat[keep_cols, ],
    data.frame(
      S = sum(dat[drop_cols, "S"]),
      label = paste(length(drop_cols), "other features"),
      row.names = "other"
    )
  )
}

# Adds non-null titles "nms" to list of ggplots
add_titles <- function(a_list, nms = NULL) {
  if (is.null(nms)) {
    return(a_list)
  }
  mapply(function(p, nm) p + ggplot2::ggtitle(nm), a_list, nms, SIMPLIFY = FALSE)
}
