# =============================================================================
# Publication-ready theme and colour palette (shared helper)
# =============================================================================
#
# A single home for the plot "look" used across f_glm(), f_aov() and
# f_lmer(), so the three functions cannot drift apart. Nothing here adds a
# package dependency: the theme is reproduced from ggplot2 primitives only,
# and the palette is a plain character vector of hex codes.
#
# Why reproduce rather than import ggpubr::theme_pubr(): ggpubr is a heavy
# package (it pulls in cowplot, ggsci, ggsignif, ggrepel, rstatix and their
# trees). Carrying all of that just for one theme function would bloat the
# dependency tree and give CRAN reviewers and users an unnecessary
# maintenance surface. theme_pubr() itself is only theme_bw() with a handful
# of theme() overrides, so it is cheap to mirror exactly.
#
# Contents:
#   .fstat_pub_palette  - 8 NPG (Nature Publishing Group) hex colours, the
#                         de-facto "publication" palette exposed by ggpubr via
#                         ggsci. Tuned as an ensemble (muted, balanced
#                         saturation) so the colours sit together on a page.
#   .fstat_pub_primary  - the single primary colour for one-series plots
#                         (replaces the previous hard-coded "steelblue").
#   f_pub_palette(n)    - first n palette colours, recycling/interpolating if
#                         n exceeds the base set.
#   f_theme_pub()       - theme_pubr() + labs_pubr() reproduced together:
#                         theme_bw base, no panel border, no grid, black axis
#                         lines, bold black axis text/titles, legend on top.
#
# These objects are internal (dot-prefixed where not called as functions) and
# are not exported.
# =============================================================================

# NPG palette (ggsci pal_npg("nrc"), alpha removed). First 8 of the 9 colours.
.fstat_pub_palette <- c(
  "#E64B35", # coral red
  "#4DBBD5", # sky blue
  "#E7B800", # golden yellow
  "#00A087", # teal green
  "#3C5488", # navy blue
  "#F39B7F", # salmon
  "#8491B4", # slate blue
  "#91D1C2"  # mint
)

# Primary single-series colour. Navy reads as a calm, professional default for
# error bars / points in one-group plots; swap to .fstat_pub_palette[1] if a
# warmer accent is preferred.
.fstat_pub_primary <- "#3C5488"

# Return n colours from the publication palette. For n within the base set the
# colours are returned verbatim (preserving the ensemble); beyond it they are
# interpolated with grDevices::colorRampPalette so any number of groups still
# gets distinct colours.
f_pub_palette <- function(n) {
  if (is.null(n) || !is.numeric(n) || length(n) != 1L || n < 1L)
    return(.fstat_pub_palette)
  n <- as.integer(n)
  base_n <- length(.fstat_pub_palette)
  if (n <= base_n)
    return(.fstat_pub_palette[seq_len(n)])
  grDevices::colorRampPalette(.fstat_pub_palette)(n)
}

# Publication-ready theme: theme_pubr() + labs_pubr() reproduced exactly,
# using ggplot2 only. base_size 14 matches the size previously passed to
# theme_bw() throughout the package.
#
#   base_size    base font size (default 14)
#   base_family  base font family (default "")
#   legend       legend position: "top" (default), "bottom", "left",
#                "right" or "none"
#   border       if TRUE, draw a panel border instead of axis lines
#                (mirrors theme_pubr(border = TRUE))
f_theme_pub <- function(base_size = 14, base_family = "",
                        legend = "top", border = TRUE) {
  half_line <- base_size / 2

  if (isTRUE(border)) {
    panel_border <- ggplot2::element_rect(fill = NA, colour = "black",
                                          linewidth = 0.7)
    axis_line <- ggplot2::element_blank()
  } else {
    panel_border <- ggplot2::element_blank()
    axis_line <- ggplot2::element_line(colour = "black", linewidth = 0.5)
  }

  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # theme_pubr(): strip frame, kill the grid, draw axis lines
      panel.border     = panel_border,
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = axis_line,
      legend.key       = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#F2F2F2",
                                               colour = "black",
                                               linewidth = 0.7),
      plot.margin      = ggplot2::margin(half_line, half_line,
                                         half_line, half_line),
      legend.position  = legend,
      # Give the legend a little vertical room. With legend.position = "top"
      # and bold legend text, a too-tight text box clips the tops of the
      # glyphs (the bold ascenders); a small top margin on the text elements
      # plus legend.margin fixes the clipping.
      legend.margin    = ggplot2::margin(t = half_line, b = half_line / 2),

      # labs_pubr(): bold black axis text and titles
      axis.text.x  = ggplot2::element_text(size = ggplot2::rel(1),
                                           colour = "black", face = "bold"),
      axis.text.y  = ggplot2::element_text(size = ggplot2::rel(1),
                                           colour = "black", face = "bold"),
      axis.title   = ggplot2::element_text(size = ggplot2::rel(1),
                                           colour = "black", face = "bold"),
      plot.title   = ggplot2::element_text(size = ggplot2::rel(1),
                                           colour = "black", lineheight = 1.0,
                                           face = "bold"),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.8),
                                           colour = "black", face = "bold",
                                           margin = ggplot2::margin(t = 2,
                                                                    r = 10)),
      legend.text  = ggplot2::element_text(size = ggplot2::rel(0.8),
                                           colour = "black", face = "plain",
                                           margin = ggplot2::margin(t = 2))

    )
}
