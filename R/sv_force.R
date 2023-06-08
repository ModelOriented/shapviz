#' SHAP Force Plot
#'
#' Creates a force plot of SHAP values of one observation. If multiple
#' observations are selected, their SHAP values and predictions are averaged.
#'
#' f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the
#' baseline SHAP value.
#'
#' @inheritParams sv_waterfall
#' @param bar_label_size Size of text used to describe bars
#'   (via [ggrepel::geom_text_repel()]).
#' @returns An object of class "ggplot" (or "patchwork") representing a force plot.
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 20, nthread = 1)
#' x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
#' sv_force(x)
#'
#' \dontrun{
#' sv_force(x, row_id = 65, max_display = 3, size = 9, fill_colors = 4:5)
#'
#' # Aggregate over all observations with Petal.Length == 1.4
#' sv_force(x, row_id = x$X$Petal.Length == 1.4)
#' }
#' @export
#' @seealso [sv_waterfall()]
sv_force <- function(object, ...) {
  UseMethod("sv_force")
}

#' @describeIn sv_force
#'   Default method.
#' @export
sv_force.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_force
#'   SHAP force plot for object of class "shapviz".
#' @export
sv_force.shapviz <- function(object, row_id = 1L, max_display = 6L,
                             fill_colors = c("#f7d13d", "#a52c60"),
                             format_shap = getOption("shapviz.format_shap"),
                             format_feat = getOption("shapviz.format_feat"),
                             contrast = TRUE, bar_label_size = 3.2,
                             show_annotation = TRUE, annotation_size = 3.2, ...) {
  stopifnot(
    "Exactly two fill colors must be passed" = length(fill_colors) == 2L,
    "format_shap must be a function" = is.function(format_shap),
    "format_feat must be a function" = is.function(format_feat)
  )
  object <- object[row_id, ]
  b <- get_baseline(object)
  dat <- .make_dat(object, format_feat = format_feat, sep = "=")
  if (ncol(object) > max_display) {
    dat <- .collapse(dat, max_display = max_display)
  }

  # Reorder rows and calculate order dependent columns
  .sorter <- function(y, p) {
    y <- y[order(abs(y$S)), ]
    y$to <- cumsum(y$S)
    y$from <- .lag(y$to, default = 0)
    hook <- y[nrow(y), "to"]
    vars <- c("to", "from")
    y[, vars] <- y[, vars] + p - hook
    y
  }
  dat$id <- "1"
  pred <- b + sum(dat$S)
  dat <- do.call(rbind, lapply(split(dat, dat$S >= 0), .sorter, p = pred))

  # Make a force plot
  b_pred <- c(b, pred)
  height <- grid::unit(0.17, "npc")

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      xmin = from, xmax = to, y = id, fill = factor(S < 0, levels = c(FALSE, TRUE))
    )
  ) +
    gggenes::geom_gene_arrow(
      show.legend = FALSE,
      arrowhead_width = grid::unit(2, "mm"),
      arrow_body_height = height,
      arrowhead_height = height
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(x = (from + to) / 2, y = as.numeric(id) + 0.08, label = label),
      size = bar_label_size,
      nudge_y = 0.3,
      segment.size = 0.1,
      segment.alpha = 0.5,
      direction = "both"
    ) +
    ggfittext::geom_fit_text(
      ggplot2::aes(label = paste0(ifelse(S > 0, "+", ""), format_shap(S))),
      show.legend = FALSE,
      contrast = contrast,
      ...
    ) +
    ggplot2::coord_cartesian(ylim = c(0.8, 1.2), clip = "off") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.13)) +
    # scale_y_discrete(expand = expansion(add = c(0.1 + 0.5 * show_annotation, 0.6))) +
    ggplot2::scale_fill_manual(values = fill_colors, drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      aspect.ratio = 1 / 4,
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(y = ggplot2::element_blank(), x = "SHAP value")

  if (show_annotation) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = b_pred,
        xend = b_pred,
        y = c(0.5, 0.75),
        yend = c(0.92, 1),
        linewidth = 0.3,
        linetype = 2
      ) +
      ggplot2::annotate(
        "text",
        x = b_pred,
        y = c(0.4, 0.65),
        label = paste0(c("E[f(x)]=", "f(x)="), format_shap(b_pred)),
        size = annotation_size
      )
  }
  p
}

#' @describeIn sv_force
#'   SHAP force plot for object of class "mshapviz".
#' @export
sv_force.mshapviz <- function(object, row_id = 1L, max_display = 6L,
                              fill_colors = c("#f7d13d", "#a52c60"),
                              format_shap = getOption("shapviz.format_shap"),
                              format_feat = getOption("shapviz.format_feat"),
                              contrast = TRUE, bar_label_size = 3.2,
                              show_annotation = TRUE, annotation_size = 3.2, ...) {
  plot_list <- lapply(
    object,
    FUN = sv_force,
    # Argument list (simplify via match.call() or some rlang magic?)
    row_id = row_id,
    max_display = max_display,
    fill_colors = fill_colors,
    format_shap = format_shap,
    format_feat = format_feat,
    contrast = contrast,
    bar_label_size = bar_label_size,
    show_annotation = show_annotation,
    annotation_size = annotation_size,
    ...
  )
  plot_list <- add_titles(plot_list, nms = names(object))  # see sv_waterfall()
  patchwork::wrap_plots(plot_list) +
    patchwork::plot_layout(ncol = 1L)
}
