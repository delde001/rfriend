# Helper function (internal) to format an Analysis of Variance (ANOVA) table
# with bold p-values, using Type II Sums of Squares via car::Anova().
#
# Why Type II (and not Type I)? For unbalanced multi-way designs the default
# Type I table is order-dependent: `drug * dose` and `dose * drug` give
# different sums of squares for the main effects, so swapping the order in
# the formula changes the omnibus p-values. The post-hoc tests in f_aov use
# emmeans, which is model-based and order-invariant; pairing emmeans with a
# Type I omnibus table can therefore tell mismatched stories. Type II sums
# of squares are order-invariant for main effects (each main effect is
# tested after all other main effects, ignoring interactions that contain
# it), aligning the omnibus table with the post-hoc tests. Type II also
# does not require sum / effect contrast coding (unlike Type III), so it
# is safe with R's default treatment contrasts.
#
# Accepts an aov / lm object and returns a pander-rendered markdown table.
#
# `type` selects the SS type passed to car::Anova(); defaults to 2.
# `alpha` is the significance threshold for bolding p-values; defaults
# to 0.05 so the helper is still usable when called outside of f_aov().
# When called from f_aov() the caller's alpha is forwarded so that the
# omnibus table and every other test in the same report agree on what
# counts as significant.
rmd_anova_summary <- function(aov_object, type = 2, alpha = 0.05) {

  # Set pander options for a fancy output table
  panderOptions("table.alignment.default", "left")
  panderOptions("table.alignment.rownames", "left")
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("knitr.auto.asis", FALSE)
  panderOptions("table.split.table", 300)
  panderOptions("table.caption.prefix", "")  # Remove "Table" prefix in caption

  # Compute the ANOVA table. car::Anova() returns an object of class
  # c("anova", "data.frame") with columns: Sum Sq | Df | F value | Pr(>F),
  # one row per model term plus a final "Residuals" row (with NA F-value
  # and NA p-value).
  anova_table <- as.data.frame(car::Anova(aov_object, type = type))

  # Trim any incidental whitespace from row names (defensive; car does not
  # pad rownames, but summary.aov used to, and downstream code is more
  # robust with clean labels).
  rownames(anova_table) <- trimws(rownames(anova_table))

  # Blank out NA F-values (the Residuals row has no F-statistic) rather
  # than printing literal "NA", which is visually noisy in publication
  # tables. formatC() coerces the column to character, which is fine for
  # pander rendering.
  if ("F value" %in% colnames(anova_table)) {
    fvals <- anova_table[["F value"]]
    anova_table[["F value"]] <- ifelse(
      is.na(fvals),
      "",
      formatC(fvals)
    )
  }

  # Bold p-values below `alpha`; blank NA cells (Residuals row).
  if ("Pr(>F)" %in% colnames(anova_table)) {
    pvals <- anova_table[["Pr(>F)"]]
    anova_table[["Pr(>F)"]] <- ifelse(
      is.na(pvals),
      "",
      ifelse(
        pvals < alpha,
        paste0("**", formatC(pvals), "**"),
        formatC(pvals)
      )
    )
  }

  # Caption tells the reader which SS type was used (transparency: e.g.
  # "Type II" is shown literally so a reviewer knows the table is not Type I).
  type_label <- paste0("Type ", as.character(as.roman(type)),
                       " ANOVA (car::Anova)")
  pander(anova_table, caption = type_label)
}
