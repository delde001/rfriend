#' Compact letter display for an emmeans grid (dependency-free)
#'
#' Internal replacement for the \code{cld()} method on emmeans grids, as it is
#' used throughout this package. It reproduces the part of that method's output
#' that the package consumes: the \code{emmeans::summary()} data frame with an
#' added \code{.group} column of compact letters, rows ordered by estimate.
#'
#' Letters come from pairwise contrasts: two means that are NOT significantly
#' different (adjusted p >= \code{alpha}) share a letter. This matches the usual
#' compact-letter semantics. Letter assignment uses \code{compact_letters()}, so
#' no external compact-letter package is needed.
#'
#' @param object An \code{emmGrid} (output of \code{emmeans::emmeans()}).
#' @param alpha Significance level for declaring two means different.
#' @param Letters Character pool for the letters.
#' @param adjust P-value adjustment passed to \code{emmeans::contrast()} /
#'   \code{summary()} (e.g. "tukey", "bonferroni", "none").
#' @param decreasing If \code{TRUE} (default here, matching the package's calls),
#'   the group with the highest estimate receives the earliest letter ("a") and
#'   rows are sorted by descending estimate.
#'
#' @return A data frame: the emmeans summary columns plus a \code{.group}
#'   character column, matching what the emmeans \code{cld()} method returns.
#' @keywords internal
#' @noRd
cld_emmeans <- function(object,
                        alpha = 0.05,
                        Letters = c(letters, LETTERS),
                        adjust = "tukey",
                        decreasing = TRUE) {

  # ---- Estimate column and per-row group labels -----------------------------
  emm_sum <- as.data.frame(summary(object))

  # emmeans names the estimate "emmean" (lm/lmer) or "rate"/"prob"/"response"
  # for transformed GLMs. Detect it the way emmeans' own print does.
  est_col <- intersect(
    c("emmean", "response", "prob", "rate", "ratio", "estimate"),
    names(emm_sum)
  )[1]
  if (is.na(est_col))
    stop("cld_emmeans: could not locate the estimate column in the emmeans ",
         "summary (looked for emmean/response/prob/rate/ratio/estimate).")

  # The grid's primary factor(s): every column that is not a standard emmeans
  # statistic. These identify each row uniquely.
  stat_cols <- c("emmean", "response", "prob", "rate", "ratio", "estimate",
                 "SE", "df", "lower.CL", "upper.CL",
                 "asymp.LCL", "asymp.UCL", "t.ratio", "z.ratio", "p.value")
  factor_cols <- setdiff(names(emm_sum), stat_cols)
  if (length(factor_cols) == 0)
    stop("cld_emmeans: no grouping factor columns found in emmeans summary.")

  # Row label = the factor level combination, in the summary's row order.
  row_label <- apply(emm_sum[factor_cols], 1, paste, collapse = " ")
  row_label <- trimws(row_label)
  if (anyDuplicated(row_label))
    stop("cld_emmeans: emmeans grid rows are not uniquely labelled by their ",
         "factor columns; cannot assign letters unambiguously.")

  n <- nrow(emm_sum)

  # ---- Single group: trivial -----------------------------------------------
  if (n == 1L) {
    out <- emm_sum
    out$.group <- Letters[1]
    return(out)
  }

  # ---- Pairwise p-values from emmeans ---------------------------------------
  ct  <- emmeans::contrast(object, method = "pairwise", adjust = adjust)
  prs <- as.data.frame(summary(ct))
  pcol <- intersect(c("p.value", "p value"), names(prs))[1]
  if (is.na(pcol))
    stop("cld_emmeans: no p-value column in pairwise contrasts.")

  # Map each contrast to the two grid rows it compares using the contrast
  # coefficient matrix (rows = contrasts, cols = grid rows). This avoids parsing
  # contrast label strings, which is fragile when factor names are prefixed
  # ("gear3 - gear4") or levels contain spaces. The two non-zero coefficients in
  # each row identify the compared grid positions; their order in con.coef
  # matches the emmeans summary (= row_label) order.
  con_coef <- ct@misc$con.coef
  if (is.null(con_coef) || nrow(con_coef) != nrow(prs) ||
      ncol(con_coef) != n)
    stop("cld_emmeans: contrast coefficient matrix unavailable or ",
         "inconsistent with the grid; cannot map comparisons to groups.")

  diff <- as.numeric(prs[[pcol]]) < alpha
  diff[is.na(diff)] <- FALSE

  sig_mat <- matrix(FALSE, n, n, dimnames = list(row_label, row_label))
  for (k in seq_len(nrow(con_coef))) {
    idx <- which(con_coef[k, ] != 0)
    if (length(idx) != 2L) next   # skip non-pairwise rows defensively
    sig_mat[idx[1], idx[2]] <- diff[k]
    sig_mat[idx[2], idx[1]] <- diff[k]
  }

  # ---- Order rows by estimate so letters run high-to-low (decreasing) -------
  ord <- order(emm_sum[[est_col]], decreasing = decreasing)
  ordered_label <- row_label[ord]
  sig_mat <- sig_mat[ordered_label, ordered_label, drop = FALSE]

  letters_out <- compact_letters(sig_mat, Letters = Letters)$Letters

  # ---- Assemble output frame in the ordered row order -----------------------
  out <- emm_sum[ord, , drop = FALSE]
  out$.group <- unname(letters_out[ordered_label])
  rownames(out) <- NULL
  out
}
