# Build a contrast forest plot shared across f_aov(), f_glm(), f_lmer() and
# f_kruskal_test().
#
# A contrast forest plot shows one row per pairwise comparison between two
# factor levels: a point at the estimated difference, a horizontal whisker for
# its confidence interval, and a dashed reference line at zero. A CI that does
# not cross zero is a significant difference. This is the same "excludes zero"
# information the compact-letter display encodes, but it also shows the sign
# (which level is larger) and the magnitude of every difference.
#
# The drawing itself is delegated to build_forest_plot() (helper_forest_plot.R)
# so the look (theme f_theme_pub(), palette f_pub_palette(), zero line, colour
# by significance) is identical to the coefficient forest plots. This helper
# only turns a tidy contrast table into the data contract build_forest_plot()
# expects and attaches an explanatory caption (attr "int_caption").
#
# Two entry points are provided:
#   make_contrast_forest()       - from an emmeans contrast table, i.e.
#                                  as.data.frame(confint(pairs(emm))). Used by
#                                  f_aov(), f_glm() and f_lmer().
#   make_dunn_contrast_forest()  - from an rstatix::dunn_test() result, which
#                                  reports a z statistic and adjusted p-value
#                                  but no effect-size CI. Used by
#                                  f_kruskal_test().
#
# Both return a ggplot2 object (caption carried in attr "int_caption") or NULL
# when the figure cannot be built, so callers degrade visibly (skip the figure,
# keep the report) instead of aborting the analysis.

# ---------------------------------------------------------------------------
# emmeans-based contrast forest (f_aov, f_glm, f_lmer).
#
#   contrast_tbl : a data frame with columns "contrast", "estimate", and a
#                  lower/upper CI pair (lower.CL/upper.CL, or the asymptotic
#                  variants asymp.LCL/asymp.UCL, or LCL/UCL). Typically
#                  as.data.frame(confint(pairs(emm, adjust = adjust))).
#   plot_title   : the figure title (character, length 1).
#   alpha        : significance level, used only for the caption wording and
#                  the significance test (CI excludes zero). Default 0.05.
#   adjust       : p-value adjustment method, used only for the caption
#                  wording. Default "none".
make_contrast_forest <- function(contrast_tbl, plot_title,
                                 alpha = 0.05, adjust = "none") {
  if (is.null(contrast_tbl) || !is.data.frame(contrast_tbl) ||
      nrow(contrast_tbl) == 0L) return(NULL)
  if (!all(c("contrast", "estimate") %in% names(contrast_tbl)))
    return(NULL)
  lo_col <- intersect(c("lower.CL", "asymp.LCL", "LCL"),
                      names(contrast_tbl))[1]
  hi_col <- intersect(c("upper.CL", "asymp.UCL", "UCL"),
                      names(contrast_tbl))[1]
  if (is.na(lo_col) || is.na(hi_col)) return(NULL)

  cf_df <- data.frame(
    label = as.character(contrast_tbl$contrast),
    est   = contrast_tbl$estimate,
    lower = contrast_tbl[[lo_col]],
    upper = contrast_tbl[[hi_col]],
    stringsAsFactors = FALSE
  )
  # A difference is significant when its CI does not cross zero. Because the
  # CI was built with the same adjust as the post hoc p-values, the colouring
  # agrees with the pairwise table.
  cf_df$sig <- ifelse(!is.na(cf_df$lower) & !is.na(cf_df$upper) &
                        (cf_df$lower > 0 | cf_df$upper < 0),
                      "significant", "not significant")

  p <- build_forest_plot(
    cf_df,
    title    = plot_title,
    x_label  = "Estimated difference between levels",
    order_by = "estimate")          # largest differences at the top
  if (is.null(p)) return(NULL)

  attr(p, "int_caption") <- paste0(
    "**Pairwise differences:** each row is one comparison between two ",
    "levels, with its ", 100 * (1 - alpha), "% CI (", adjust,
    "-adjusted). A CI that does not cross the dashed zero line is a ",
    "significant difference (\u03b1 = ", alpha, "); one that crosses zero ",
    "is not. The sign shows which level is larger and the width shows the ",
    "precision.")
  p
}

# ---------------------------------------------------------------------------
# Dunn's-test-based contrast forest (f_kruskal_test).
#
# Dunn's test (rstatix::dunn_test) reports, for each pair, a standardized z
# statistic and an adjusted p-value, but no effect-size confidence interval.
# A forest plot therefore shows the z statistic per comparison with a dashed
# zero reference line; significance is taken from the adjusted p-value rather
# than from a CI (there is none). The sign of z shows which group has the
# higher mean rank.
#
#   dunn_tbl   : an rstatix::dunn_test() result with columns "group1",
#                "group2", "statistic" and "p.adj".
#   plot_title : the figure title (character, length 1).
#   alpha      : significance level for the colour split and caption. Default
#                0.05.
#   adjust     : p-value adjustment method, used only for the caption wording.
#                Default "none".
make_dunn_contrast_forest <- function(dunn_tbl, plot_title,
                                      alpha = 0.05, adjust = "none") {
  if (is.null(dunn_tbl) || !is.data.frame(dunn_tbl) ||
      nrow(dunn_tbl) == 0L) return(NULL)
  if (!all(c("group1", "group2", "statistic", "p.adj") %in% names(dunn_tbl)))
    return(NULL)

  z   <- as.numeric(dunn_tbl$statistic)
  padj <- as.numeric(dunn_tbl$p.adj)

  cf_df <- data.frame(
    label = paste0(as.character(dunn_tbl$group1), " - ",
                   as.character(dunn_tbl$group2)),
    est   = z,
    # No CI from Dunn's test: draw the point only (zero-width whisker). The
    # caption makes clear that significance comes from the adjusted p-value.
    lower = z,
    upper = z,
    sig   = ifelse(!is.na(padj) & padj < alpha,
                   "significant", "not significant"),
    stringsAsFactors = FALSE
  )

  p <- build_forest_plot(
    cf_df,
    title    = plot_title,
    x_label  = "Dunn's z statistic (difference in mean ranks)",
    order_by = "estimate")          # largest z at the top
  if (is.null(p)) return(NULL)

  attr(p, "int_caption") <- paste0(
    "**Pairwise comparisons:** each row is a Dunn's comparison; the point ",
    "is the z statistic (standardized difference in mean ranks) and the ",
    "dashed line marks zero (no difference). Dunn's test gives no ",
    "effect-size CI, so significance is taken from the ", adjust,
    "-adjusted p-value (\u03b1 = ", alpha, ") and shown by colour, not by a ",
    "whisker. A positive z means the first group has the higher mean rank; ",
    "a negative z means the second does.")
  p
}
