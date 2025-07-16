# Helper function (internal) to format Analysis of Variance (ANOVA) table with bold p-values
rmd_anova_summary <- function(aov_object) {

  #Set Pander options for a fancy output tabel
  panderOptions('table.alignment.default', 'left')
  panderOptions('table.alignment.rownames', 'left')
  panderOptions('keep.trailing.zeros', TRUE)
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('table.split.table', 300)
  panderOptions('table.caption.prefix', '')  # Remove prefix Table in caption
  # Extract summary as a list
  summary_list <- summary(aov_object)

  # Extract the ANOVA table
  anova_table <- as.data.frame(summary_list[[1]])

  # Check if "Pr(>F)" column exists (to handle cases without p-values)
  if ("Pr(>F)" %in% colnames(anova_table)) {
    # Format p-values: Bold those less than 0.05
    anova_table[["Pr(>F)"]] <- ifelse(
      anova_table[["Pr(>F)"]] < 0.05,
      paste0("**", formatC(anova_table[["Pr(>F)"]]), "**"),
      formatC(anova_table[["Pr(>F)"]])
    )
  }

  # Use pander to render the table
  pander(anova_table)
}
