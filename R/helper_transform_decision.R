# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# helper_transform_decision.R
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Shared by f_aov(), f_lm() and f_t_test(), which transform the response
# automatically (Box-Cox or bestNormalize) when the residual checks fail.
#
# 1. keep_transformation(): a transformation is KEPT only when it improves the
#    check that triggered it. Otherwise the untransformed analysis is reported
#    and the attempt is disclosed (a transformation that does not help only
#    makes the result harder to interpret).
#      * triggered by non-normality      -> the normality p-value must rise
#        (Shapiro-Wilk; Anderson-Darling when Shapiro is unavailable)
#      * triggered by unequal variance only -> the variance-test p-value must
#        rise (Levene for f_aov, Breusch-Pagan for f_lm)
#      * forced by the user (force_transformation) -> always kept
# 2. transformation_label() / scale_heading(): the wording that names the
#    scale of a reported table, e.g.
#      "ANOVA on Box-Cox transformed height_cm (lambda = -0.131)"
#      "ANOVA on height_cm (untransformed)"
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# p-value of the residual normality check: Shapiro-Wilk, or Anderson-Darling
# when Shapiro-Wilk was skipped (n > 5000) or unavailable.
norm_check_p <- function(shapiro, adt = NULL) {
  p_sw <- if (!is.null(shapiro)) suppressWarnings(as.numeric(shapiro$p.value)) else NA_real_
  if (length(p_sw) == 1L && !is.na(p_sw)) return(list(p = p_sw, test = "Shapiro-Wilk"))
  p_ad <- if (!is.null(adt)) suppressWarnings(as.numeric(adt$p.value)) else NA_real_
  if (length(p_ad) == 1L && !is.na(p_ad)) return(list(p = p_ad, test = "Anderson-Darling"))
  list(p = NA_real_, test = NA_character_)
}

# Decide whether to keep a transformation. Returns list(keep, reason) where
# reason is a one-sentence explanation (used in the report when not kept).
keep_transformation <- function(trigger_normality, trigger_variance,
                                trigger_forced,
                                norm_before, norm_after, norm_test,
                                var_before = NA_real_, var_after = NA_real_,
                                var_test = "variance test",
                                method_label = "The transformation") {
  if (isTRUE(trigger_forced))
    return(list(keep = TRUE, reason = "forced with force_transformation"))
  fmt <- function(p) if (is.na(p)) "NA" else format(signif(p, 3))
  if (isTRUE(trigger_normality)) {
    improved <- !is.na(norm_before) && !is.na(norm_after) && norm_after > norm_before
    return(list(
      keep = improved,
      reason = paste0(method_label, " ",
                      if (improved) "improved" else "did not improve",
                      " the normality of the residuals (", norm_test,
                      " p = ", fmt(norm_before), " before, ", fmt(norm_after),
                      " after).")))
  }
  if (isTRUE(trigger_variance)) {
    improved <- !is.na(var_before) && !is.na(var_after) && var_after > var_before
    return(list(
      keep = improved,
      reason = paste0(method_label, " ",
                      if (improved) "improved" else "did not improve",
                      " the equality of the residual variances (", var_test,
                      " p = ", fmt(var_before), " before, ", fmt(var_after),
                      " after).")))
  }
  list(keep = TRUE, reason = "")
}

# "Box-Cox transformed (lambda = -0.131)" / "bestNormalize (arcsinh(x))
# transformed". method is "boxcox" or "bestnormalize".
transformation_label <- function(method, lambda = NULL, bn_name = NULL) {
  if (identical(method, "bestnormalize")) {
    paste0("bestNormalize", if (!is.null(bn_name) && nzchar(bn_name))
      paste0(" (", trimws(bn_name), ")") else "", " transformed")
  } else {
    paste0("Box-Cox transformed", if (!is.null(lambda) && !is.na(lambda))
      paste0(" (lambda = ", format(round(lambda, 3), nsmall = 3), ")") else "")
  }
}

# Heading that names the scale of the reported table.
#   scale_heading("ANOVA", "height_cm", "Box-Cox transformed (lambda = -0.131)")
#     -> "ANOVA on Box-Cox transformed height_cm (lambda = -0.131)"
#   scale_heading("ANOVA", "height_cm", NULL)
#     -> "ANOVA on height_cm (untransformed)"
scale_heading <- function(what, response, label = NULL) {
  if (is.null(label) || !nzchar(label))
    return(paste0(what, " on ", response, " (untransformed)"))
  # "Box-Cox transformed (lambda = x)" -> "Box-Cox transformed <resp> (lambda = x)"
  m <- regmatches(label, regexec("^(.*transformed)(.*)$", label))[[1]]
  if (length(m) == 3L)
    paste0(what, " on ", m[2], " ", response, m[3])
  else paste0(what, " on ", label, " ", response)
}

# Markdown note printed when a transformation is not kept.
transformation_rejected_note <- function(reason) {
  paste0("   \n  \n**Transformation not kept.** ", reason,
         " The untransformed analysis is reported below; see the residual ",
         "checks above and consider a test without the normality assumption ",
         "(e.g. `f_kruskal_test()`, `f_wilcox_test()`) or a GLM ",
         "(`f_glm()`).  \n  \n")
}
