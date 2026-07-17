# Build a horizontal forest (dot-and-whisker) plot used across rfriend.
#
# Several functions draw the same figure: one labelled row per item, a point
# at the estimate, a horizontal whisker for its confidence interval, a dashed
# vertical reference line at zero, and significance shown by colour. Two uses
# exist today and more are planned:
#   * coefficient forest plots (f_lmer, f_glm, later f_lm): rows are model
#     coefficients, zero means "no difference from the reference level / no
#     association".
#   * contrast forest plots (f_lmer, and planned for f_aov, f_glm,
#     f_kruskal_test): rows are pairwise differences between levels, zero
#     means "the two levels are equal".
# These are the same artifact with different data semantics, so the *drawing*
# is centralised here while the data preparation and the explanatory caption
# stay with each caller (their wording differs and should not be forced into a
# single template).
#
# The caller passes a tidy data frame with a fixed contract; this function
# only styles and returns the ggplot. It does NOT compute statistics, build
# captions, or save files.
#
# @param df A data frame with one row per item and these columns:
#   \code{label} (character; the row label), \code{est} (numeric; point
#   estimate), \code{lower}, \code{upper} (numeric; CI bounds), and
#   \code{sig} (character; "significant" / "not significant", used for
#   colour). Rows with missing est/lower/upper are dropped.
# @param title Plot title (character, length 1).
# @param x_label x-axis label (character, length 1). The two canonical values
#   are "Estimate (95% CI)" for coefficient plots and
#   "Estimated difference between levels" for contrast plots, but any string
#   is accepted.
# @param order_by How to order rows top-to-bottom. \code{"model"} keeps the
#   incoming row order (used for coefficient plots, which mirror the model
#   summary); \code{"estimate"} sorts by \code{est} so the largest effects
#   sit at the top (used for contrast plots). Default \code{"model"}.
# @param base_size Base font size passed to \code{f_theme_pub()}. Default 14.
# @return A \pkg{ggplot2} object, or \code{NULL} if there is nothing to plot
#   (empty / malformed input). Returning NULL rather than erroring lets
#   callers degrade visibly (skip the figure, keep the report) instead of
#   aborting the whole analysis.
build_forest_plot <- function(df,
                              title,
                              x_label,
                              order_by = c("model", "estimate"),
                              base_size = 14) {

  order_by <- match.arg(order_by)

  # Fail visibly via NULL (caller decides what to show) rather than throwing:
  # a malformed contrast table should not kill an otherwise valid report.
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(NULL)
  required <- c("label", "est", "lower", "upper", "sig")
  if (!all(required %in% names(df))) return(NULL)

  d <- data.frame(
    label = as.character(df$label),
    est   = df$est,
    lower = df$lower,
    upper = df$upper,
    sig   = as.character(df$sig),
    stringsAsFactors = FALSE
  )
  d <- d[stats::complete.cases(d[, c("est", "lower", "upper")]), , drop = FALSE]
  if (nrow(d) == 0L) return(NULL)

  # Row order. ggplot draws the y axis bottom-up, so to get the desired
  # top-to-bottom order we set factor levels in reverse.
  if (order_by == "estimate") {
    d <- d[order(d$est), , drop = FALSE]
    d$label <- factor(d$label, levels = d$label)        # largest on top
  } else {
    d$label <- factor(d$label, levels = rev(d$label))   # model order on top
  }

  # Significance colours. drop = FALSE keeps both legend keys present even
  # when every row is the same category, so the legend is stable across plots.
  sig_cols <- c("significant"     = f_pub_palette(2)[1],
                "not significant" = "grey60")

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data[["est"]], y = .data[["label"]],
                 colour = .data[["sig"]])) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey40") +
    # geom_errorbarh() was deprecated in ggplot2 4.0.0; geom_errorbar() with
    # orientation = "y" draws horizontal bars from xmin/xmax and is supported
    # back to ggplot2 3.3.0, so this is warning-free on both old and new
    # ggplot2. Note the deprecated 'height' argument becomes 'width' here.
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[["lower"]], xmax = .data[["upper"]]),
      orientation = "y", width = 0.2, linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = sig_cols, name = NULL,
                                 drop = FALSE) +
    ggplot2::labs(title = title, x = x_label, y = NULL) +
    f_theme_pub(base_size = base_size)
}
