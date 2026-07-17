#' Compact letter display from a significance structure (no dependencies)
#'
#' Internal, dependency-free compact-letter-display generator. Implements the
#' standard
#' insert-and-absorb (sweep) algorithm: start with one group per column, mark
#' the letters each group is connected to, then drop any column whose set of
#' groups is wholly contained in another column. Two groups that are NOT
#' significantly different share at least one letter; groups that ARE
#' significantly different share none.
#'
#' Accepts either of the two input shapes used across this package:
#' \itemize{
#'   \item A logical (or 0/1) square matrix where \code{TRUE} means the row/column
#'         pair is significantly different (diagonal ignored). Used by
#'         \code{f_friedman} and \code{f_kruskal_test}.
#'   \item A named numeric vector of p-values for pairwise comparisons, with names
#'         like \code{"a-b"} or \code{"a:b"}, thresholded at \code{threshold}.
#'         Used by \code{f_glm}.
#' }
#'
#' The return value is a list with a named character vector
#' \code{$Letters} (names = group labels, in the order they first appear in the
#' input).
#'
#' @param x A logical/0-1 significance matrix, or a named numeric p-value vector.
#' @param threshold Significance cutoff applied only when \code{x} is a numeric
#'   vector. Pairs with p < \code{threshold} are treated as different.
#' @param Letters Character pool to draw letters from.
#' @param reversed If \code{TRUE}, reverse the order in which letters are
#'   assigned (reverses the letter order).
#' @param sep Separator used to split p-value vector names into the two group
#'   labels. Defaults to matching either \code{"-"} or \code{":"}.
#'
#' @return A list with element \code{Letters}: a named character vector.
#' @keywords internal
#' @noRd
compact_letters <- function(x,
                            threshold = 0.05,
                            Letters = c(letters, LETTERS),
                            reversed = FALSE,
                            sep = "[-:]") {

  # ---- Normalise input into a logical "different" matrix --------------------
  if (is.matrix(x)) {
    diff_mat <- matrix(as.logical(x), nrow = nrow(x), ncol = ncol(x),
                       dimnames = dimnames(x))
    grp <- rownames(diff_mat)
    if (is.null(grp))
      stop("compact_letters: matrix input must have row/column names.")
  } else {
    # Named p-value vector -> reconstruct group labels and matrix.
    nm <- names(x)
    if (is.null(nm))
      stop("compact_letters: p-value vector input must be named.")

    parts <- strsplit(nm, sep)
    if (any(vapply(parts, length, integer(1)) != 2))
      stop("compact_letters: every comparison name must split into exactly ",
           "two group labels using separator '", sep, "'.")

    g1 <- trimws(vapply(parts, `[`, character(1), 1))
    g2 <- trimws(vapply(parts, `[`, character(1), 2))
    grp <- unique(c(rbind(g1, g2)))   # first-appearance order

    n   <- length(grp)
    diff_mat <- matrix(FALSE, n, n, dimnames = list(grp, grp))
    is_diff <- as.numeric(x) < threshold
    is_diff[is.na(is_diff)] <- FALSE  # NA p-values: treat as "not different"
    for (k in seq_along(nm)) {
      diff_mat[g1[k], g2[k]] <- is_diff[k]
      diff_mat[g2[k], g1[k]] <- is_diff[k]
    }
  }

  diag(diff_mat) <- FALSE
  n <- length(grp)

  # Single group: trivially one letter.
  if (n <= 1) {
    out <- stats::setNames(rep(Letters[1], n), grp)
    return(list(Letters = out))
  }

  # ---- Absorb: remove any column whose group-set is a subset of another -----
  # Drops a column when its groups are wholly contained in another column
  # (and, for identical columns, keeps only the earliest). Factored out so it
  # can be applied AFTER EVERY split inside the sweep, not just once at the end.
  absorb_cols <- function(cols) {
    if (ncol(cols) <= 1L) return(cols)
    keep <- rep(TRUE, ncol(cols))
    for (a in seq_len(ncol(cols))) {
      if (!keep[a]) next
      for (b in seq_len(ncol(cols))) {
        if (a == b || !keep[b]) next
        # if column a's groups are all contained in column b, drop a
        if (all(cols[, a] <= cols[, b]) && any(cols[, b] & !cols[, a])) {
          keep[a] <- FALSE
          break
        }
        # identical columns: keep the earlier one only
        if (identical(cols[, a], cols[, b]) && b < a) {
          keep[a] <- FALSE
          break
        }
      }
    }
    cols[, keep, drop = FALSE]
  }

  # ---- Insert-and-absorb sweep ----------------------------------------------
  # Each column of `cols` is a candidate letter: a logical vector over groups
  # marking which groups carry that letter. Start with one column = all groups
  # share letter 1, then for every significant pair split the columns that
  # currently join those two groups.
  #
  # CRITICAL: absorb redundant columns AFTER EACH PAIR. Without this, the column
  # set grows multiplicatively (every split can double the columns joining i and
  # j) and explodes exponentially for many groups (e.g. a 32-cell interaction
  # reaches millions of columns, exhausting memory or the letter pool before the
  # final absorb runs). Absorbing inside the loop keeps the column count bounded
  # by the number of maximal "not different" cliques, which is what the letters
  # actually need.
  cols <- matrix(TRUE, nrow = n, ncol = 1, dimnames = list(grp, NULL))

  diff_pairs <- which(diff_mat & upper.tri(diff_mat), arr.ind = TRUE)
  if (reversed) diff_pairs <- diff_pairs[rev(seq_len(nrow(diff_pairs))), , drop = FALSE]

  for (p in seq_len(nrow(diff_pairs))) {
    i <- diff_pairs[p, 1]
    j <- diff_pairs[p, 2]
    # Columns that currently contain BOTH i and j must be split.
    joined <- which(cols[i, ] & cols[j, ])
    if (length(joined) > 0L) {
      for (col in joined) {
        new_a <- cols[, col]; new_a[i] <- FALSE  # drop i
        new_b <- cols[, col]; new_b[j] <- FALSE  # drop j
        cols[, col] <- new_a
        cols <- cbind(cols, new_b)
      }
      cols <- absorb_cols(cols)                   # keep the set minimal each step
    }
  }

  # Final absorb (no-op if the in-loop absorb already minimised the set).
  cols <- absorb_cols(cols)

  # ---- Order columns so letters read left-to-right by first group ----------
  # Order letters by the first group each connects to.
  first_grp <- apply(cols, 2, function(v) which(v)[1])
  cols <- cols[, order(first_grp), drop = FALSE]
  if (reversed) cols <- cols[, rev(seq_len(ncol(cols))), drop = FALSE]

  if (ncol(cols) > length(Letters))
    stop("compact_letters: not enough letters in the pool for ",
         ncol(cols), " groups.")

  # ---- Build the per-group letter strings -----------------------------------
  letter_strings <- vapply(seq_len(n), function(g) {
    cols_g <- which(cols[g, ])
    paste(Letters[cols_g], collapse = "")
  }, character(1))

  out <- stats::setNames(letter_strings, grp)
  list(Letters = out)
}
