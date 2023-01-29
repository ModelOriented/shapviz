#' SHAP Force Plot
#'
#' Creates a force plot of SHAP values of one single observation. The value of
#' f(x) denotes the prediction on the SHAP scale, while E(f(x)) refers to the baseline
#' SHAP value.
#'
#' @param object An object of class "shapviz".
#' @param row_id A single row number to plot.
#' @param max_display Maximum number of features (with largest absolute SHAP values)
#' should be plotted? If there are more features, they will be collapsed to one feature.
#' Set to \code{Inf} to show all features.
#' @param fill_colors A vector of exactly two fill colors: the first for positive
#' SHAP values, the other for negative ones.
#' @param format_shap Function used to format SHAP values. The default uses the
#' global option \code{shapviz.format_shap}, which equals to
#' \code{function(z) prettyNum(z, digits = 3, scientific = FALSE)} by default.
#' @param format_feat Function used to format numeric feature values. The default uses
#' the global option \code{shapviz.format_feat}, which equals to
#' \code{function(z) prettyNum(z, digits = 3, scientific = FALSE)} by default.
#' @param contrast Logical flag that detemines whether to use white text in dark arrows.
#' Default is \code{TRUE}.
#' @param bar_label_size Size of text used to describe bars.
#' (via \code{ggrepel::geom_text_repel()}).
#' @param show_annotation Should "f(x)" and "E(f(x))" be plotted? Default is \code{TRUE}.
#' @param annotation_size Size of the annotation text (f(x)=... and E(f(x))=...).
#' @param ... Arguments passed to \code{ggfittext::geom_fit_text()}.
#' For example, \code{size = 9} will use fixed text size in the bars and \code{size = 0}
#' will altogether suppress adding text to the bars.
#' @return An object of class "ggplot" representing a force plot.
#' @export
#' @seealso \code{\link{sv_waterfall}}
#' @examples
#' dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
#' fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
#' x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])
#' sv_force(x)
#' sv_force(x, row_id = 65, max_display = 3, size = 9, fill_colors = 4:5)
sv_force <- function(object, ...) {
  UseMethod("sv_force")
}

#' @describeIn sv_force Default method.
#' @export
sv_force.default <- function(object, ...) {
  stop("No default method available.")
}

#' @describeIn sv_force SHAP force plot for object of class "shapviz".
#' @export
sv_force.shapviz <- function(object, row_id = 1L, max_display = 6L,
                             fill_colors = c("#f7d13d", "#a52c60"),
                             format_shap = getOption("shapviz.format_shap"),
                             format_feat = getOption("shapviz.format_feat"),
                             contrast = TRUE, bar_label_size = 3.2,
                             show_annotation = TRUE, annotation_size = 3.2, ...) {
  stopifnot(
    "Only one row number can be passed" = length(row_id) == 1L,
    "Exactly two fill colors must be passed" = length(fill_colors) == 2L,
    "format_shap must be a function" = is.function(format_shap),
    "format_feat must be a function" = is.function(format_feat)
  )

  X <- get_feature_values(object)[row_id, ]
  S <- get_shap_values(object)[row_id, ]
  b <- get_baseline(object)
  dat <- data.frame(S = S, label = paste(names(X), format_feat(X), sep = "="))

  # Collapse unimportant features
  dat <- .collapse(dat, S, max_display = max_display)

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

  p <- ggplot(
    dat,
    aes(xmin = from, xmax = to, y = id, fill = factor(S < 0, levels = c(FALSE, TRUE)))
  ) +
    gggenes::geom_gene_arrow(
      show.legend = FALSE,
      arrowhead_width = grid::unit(2, "mm"),
      arrow_body_height = height,
      arrowhead_height = height
    ) +
    ggrepel::geom_text_repel(
      aes(x = (from + to) / 2, y = as.numeric(id) + 0.08, label = label),
      size = bar_label_size,
      nudge_y = 0.3,
      segment.size = 0.1,
      segment.alpha = 0.5,
      direction = "both"
    ) +
    ggfittext::geom_fit_text(
      aes(label = paste0(ifelse(S > 0, "+", ""), format_shap(S))),
      show.legend = FALSE,
      contrast = contrast,
      ...
    ) +
    coord_cartesian(ylim = c(0.8, 1.2), clip = "off") +
    scale_x_continuous(expand = expansion(mult = 0.13)) +
    # scale_y_discrete(expand = expansion(add = c(0.1 + 0.5 * show_annotation, 0.6))) +
    scale_fill_manual(values = fill_colors, drop = FALSE) +
    theme_bw() +
    theme(
      aspect.ratio = 1 / 4,
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(y = element_blank(), x = "SHAP value")

  if (show_annotation) {
    p <- p +
      annotate(
        "segment",
        x = b_pred,
        xend = b_pred,
        y = c(0.5, 0.75),
        yend = c(0.92, 1),
        linewidth = 0.3,
        linetype = 2
      ) +
      annotate(
        "text",
        x = b_pred,
        y = c(0.4, 0.65),
        label = paste0(c("E[f(x)]=", "f(x)="), format_shap(b_pred)),
        size = annotation_size
      )
  }
  p
}
