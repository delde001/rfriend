# Build a caption describing the reference level of each factor in a fitted
# model, for use under a coefficient forest plot.
#
# A regression coefficient forest plot (f_lmer, f_glm, later f_lm) shows each
# factor level as a contrast against that factor's *reference* level. Under R's
# default treatment contrasts the reference is simply the first level of the
# factor, which is whatever sorts first alphabetically unless the user set it
# deliberately with stats::relevel() or factor(levels = ...). Because that
# choice silently determines what every coefficient means, the reference level
# of each factor must be stated explicitly. With several factors each has its
# own reference, so this belongs in the caption rather than the axis label.
#
# The helper reads the model frame of the fitted object, so it reports the
# reference level that was actually used in the fit (respecting relevel() and
# any user-supplied factor ordering), not a guess.
#
# @param fit A fitted model object accepted by stats::model.frame() and
#   stats::formula() (e.g. an lmerMod / lmerModLmerTest or a glm).
# @param fixed_only Logical. If TRUE, use formula(fit, fixed.only = TRUE) to
#   drop random-effects grouping factors (needed for lme4 models). For glm /
#   lm pass FALSE. Default FALSE.
# @return A list with three character components, each possibly empty (\"\"):
#   $reference  e.g. \"Reference levels: treatment = control; time_f = t0.\"
#   $continuous e.g. \"Continuous terms (dose) are per one-unit increase.\"
#   $how_to     a short note on how to change the reference level.
#   Components that do not apply are returned as \"\" so callers can paste()
#   them unconditionally.
build_coef_ref_caption <- function(fit, fixed_only = FALSE) {

  empty <- list(reference = "", continuous = "", how_to = "")

  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) return(empty)

  # Fixed-effect predictor variables. We iterate over the MODEL FRAME columns
  # directly rather than over all.vars(formula): a term written as factor(block)
  # or I(x^2) appears in the model frame under the literal name "factor(block)"
  # / "I(x^2)", whereas all.vars() returns the bare symbol ("block", "x"). The
  # old intersect(all.vars, names(mf)) therefore silently dropped any
  # factor()-wrapped predictor, so its reference level was never reported. The
  # model frame already carries the correct column class (a factor() wrapper
  # yields a factor column), which is exactly what we need here.
  fe_form <- tryCatch(
    if (isTRUE(fixed_only)) stats::formula(fit, fixed.only = TRUE)
    else stats::formula(fit),
    error = function(e) NULL)
  if (is.null(fe_form)) return(empty)

  # Response column name (first model-frame column) -- drop it from predictors.
  resp_nm <- tryCatch(names(mf)[1L], error = function(e) NA_character_)

  # Names of the fixed-effect terms actually in the (fixed) formula, used to
  # keep only genuine predictors and drop lme4 grouping columns / weights / etc.
  fe_term_vars <- tryCatch(all.vars(fe_form), error = function(e) character(0))

  # Helper: strip a leading transform wrapper for display only, so the caption
  # reads "block" rather than "factor(block)". The class check still uses the
  # real column. Only the common factor()/as.factor()/ordered() wrappers are
  # unwrapped; anything else (e.g. I(x^2)) is shown verbatim.
  pretty_name <- function(nm) {
    m <- regmatches(nm, regexec("^(?:factor|as\\.factor|ordered)\\(([^)]+)\\)$", nm))[[1]]
    if (length(m) == 2L) m[2] else nm
  }
  # Does this model-frame column correspond to a real fixed-effect predictor?
  is_predictor_col <- function(nm) {
    if (!is.na(resp_nm) && identical(nm, resp_nm)) return(FALSE)
    # Match the bare variable inside the column name against formula vars, so
    # "factor(block)" is kept because "block" is a formula variable. Fixed
    # substring matching avoids regex-metacharacter issues from names like
    # "Sepal.Width".
    bare <- pretty_name(nm)
    any(vapply(fe_term_vars, function(v)
      v == bare || grepl(v, nm, fixed = TRUE), logical(1)))
  }

  ref_bits  <- character(0)
  cont_bits <- character(0)
  for (nm in names(mf)) {
    if (!is_predictor_col(nm)) next
    col <- mf[[nm]]
    disp <- pretty_name(nm)
    if (is.factor(col)) {
      # Reference level under treatment contrasts is the first level, read from
      # the fitted model frame so relevel()/custom orderings are reflected.
      ref_bits <- c(ref_bits, paste0(disp, " = ", levels(col)[1L]))
    } else if (is.logical(col)) {
      ref_bits <- c(ref_bits, paste0(disp, " = FALSE"))
    } else if (is.numeric(col)) {
      cont_bits <- c(cont_bits, disp)
    }
  }

  out <- empty

  if (length(ref_bits) > 0L) {
    out$reference <- paste0(
      "**Reference level",
      if (length(ref_bits) > 1L) "s" else "", ":** ",
      paste(ref_bits, collapse = "; "), ". ",
      "Each factor row is that level compared with its reference level above."
    )
    # Only worth telling people how to change it when there is a factor.
    # Use the first reported factor and its current reference level so the
    # example is copy-paste safe (the old hard-coded ref = "drug" did not
    # match any real level and errored when run). Fall back to a generic
    # placeholder only if no factor name/level could be read.
    first_ref   <- ref_bits[1]
    first_var   <- sub(" = .*$", "", first_ref)
    first_level <- sub("^.* = ", "", first_ref)
    if (!nzchar(first_var))   first_var   <- "treatment"
    if (!nzchar(first_level)) first_level <- "control"
    out$how_to <- paste0(
      "To change a reference level, relevel the factor before fitting, ",
      "e.g. data\\$", first_var, " <- relevel(data\\$", first_var,
      ", ref = \"", first_level, "\"), ",
      "or with f_factors(data, select = \"", first_var,
      "\", ref = \"", first_level, "\")."
    )
  }

  if (length(cont_bits) > 0L) {
    cont_bits <- unique(cont_bits)
    plural <- length(cont_bits) > 1L
    out$continuous <- paste0(
      "Continuous term", if (plural) "s" else "", " (",
      paste(cont_bits, collapse = ", "), ") ",
      if (plural) "have" else "has",
      " no reference level; the estimate is the change per one-unit ",
      "increase (on the scale stated above), so zero means no association."
    )
  }

  out
}
