# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# helper_post_hoc.R
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Shared post hoc logic for f_aov() and f_lmer(): which tables to build
# (the `post_hoc` / `by` arguments), and how to build one table.
#
# One table = one question:
#   * a main effect   "treatment"          -> emmeans(model, ~ treatment)
#   * an interaction  "treatment:week"     -> emmeans(model, ~ treatment * week)
#   * a simple effect "treatment | week"   -> emmeans(model, ~ treatment | week)
# Letters and the p-value adjustment are computed WITHIN each table (for a
# simple effect: within each level of `by`). Every table carries its own
# footer with its own estimate and test counts.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# "did you mean" for a name that is not one of the candidates.
ph_suggest <- function(x, candidates) {
  if (!length(candidates)) return("")
  d <- utils::adist(x, candidates, ignore.case = TRUE)[1, ]
  best <- which.min(d)
  if (length(best) == 1L && d[best] <= max(2L, ceiling(0.3 * nchar(x))))
    paste0(" (did you mean '", candidates[best], "'?)") else ""
}

# Split an interaction label "a:b" into its parts.
ph_parts <- function(term) trimws(strsplit(term, ":", fixed = TRUE)[[1]])

# Resolve and validate post_hoc / by.
#
# post_hoc: NULL or TRUE  -> default per-term tables
#           FALSE         -> no post hoc
#           character     -> only the named terms (factor names and/or "a:b")
# by:       NULL or one categorical predictor (only with character post_hoc,
#           main-effect names only)
#
# Returns list(mode = "default" | "none" | "named", mains, ints, by).
# For mode "default", mains = every categorical main effect in the model and
# ints = NULL (the caller adds the significant categorical interactions once
# p-values are known). Errors are fail-loud, with a "did you mean" hint.
resolve_post_hoc_spec <- function(post_hoc, by, cat_predictors,
                                  numeric_predictors, model_terms, fun_name) {
  main_terms <- model_terms[!grepl(":", model_terms, fixed = TRUE)]
  int_terms  <- model_terms[grepl(":", model_terms, fixed = TRUE)]
  cat_mains  <- intersect(main_terms, cat_predictors)
  cat_ints   <- int_terms[vapply(int_terms, function(it)
    all(ph_parts(it) %in% cat_predictors), logical(1))]

  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by))
      stop(fun_name, "(): 'by' must be the name of one categorical predictor, ",
           "e.g. by = \"week\".", call. = FALSE)
    if (!is.character(post_hoc))
      stop(fun_name, "(): 'by' needs 'post_hoc' naming the factor(s) to ",
           "compare within each level of '", by, "', e.g. ",
           "post_hoc = \"treatment\", by = \"", by, "\".", call. = FALSE)
  }

  if (is.null(post_hoc) || isTRUE(post_hoc)) {
    return(list(mode = "default", mains = cat_mains, ints = NULL, by = NULL))
  }
  if (identical(post_hoc, FALSE)) {
    return(list(mode = "none", mains = character(0), ints = character(0),
                by = NULL))
  }
  if (!is.character(post_hoc) || !length(post_hoc) || anyNA(post_hoc))
    stop(fun_name, "(): 'post_hoc' must be NULL, TRUE, FALSE, or the names of ",
         "model terms, e.g. post_hoc = \"treatment\" or ",
         "post_hoc = \"treatment:week\".", call. = FALSE)

  post_hoc <- unique(trimws(post_hoc))
  mains <- character(0)
  ints  <- character(0)
  for (p in post_hoc) {
    if (grepl(":", p, fixed = TRUE)) {
      parts <- ph_parts(p)
      # Match the interaction in any order of its parts ("b:a" == "a:b").
      hit <- cat_ints[vapply(cat_ints, function(it)
        setequal(ph_parts(it), parts) && length(ph_parts(it)) == length(parts),
        logical(1))]
      if (!length(hit)) {
        num_parts <- intersect(parts, numeric_predictors)
        if (length(num_parts))
          stop(fun_name, "(): post_hoc = \"", p, "\" involves the numeric ",
               "covariate(s) ", paste0("'", num_parts, "'", collapse = ", "),
               "; cell means are only computed between categorical factors.",
               call. = FALSE)
        stop(fun_name, "(): post_hoc = \"", p, "\" is not a categorical ",
             "interaction in the model", ph_suggest(p, cat_ints),
             ". Interactions in the model: ",
             if (length(cat_ints)) paste0("'", cat_ints, "'", collapse = ", ")
             else "none", ".", call. = FALSE)
      }
      ints <- c(ints, hit[1])
    } else {
      if (p %in% numeric_predictors)
        stop(fun_name, "(): post_hoc = \"", p, "\" is a numeric covariate. ",
             "Pairwise comparisons are not meaningful for a slope; it is ",
             "reported in the coefficient / ANOVA table.", call. = FALSE)
      if (!p %in% cat_mains)
        stop(fun_name, "(): post_hoc = \"", p, "\" is not a categorical ",
             "predictor in the model", ph_suggest(p, c(cat_mains, cat_ints)),
             ". Categorical predictors: ",
             if (length(cat_mains)) paste0("'", cat_mains, "'", collapse = ", ")
             else "none", ".", call. = FALSE)
      mains <- c(mains, p)
    }
  }

  if (!is.null(by)) {
    if (length(ints))
      stop(fun_name, "(): 'by' cannot be combined with an interaction in ",
           "'post_hoc' (", paste0("'", ints, "'", collapse = ", "), "). Use ",
           "either post_hoc = \"a:b\" for all cells, or post_hoc = \"a\", ",
           "by = \"b\" for comparisons of a within each level of b.",
           call. = FALSE)
    if (!by %in% cat_predictors) {
      if (by %in% numeric_predictors)
        stop(fun_name, "(): by = \"", by, "\" is a numeric covariate; 'by' ",
             "must be a categorical predictor.", call. = FALSE)
      stop(fun_name, "(): by = \"", by, "\" is not a categorical predictor ",
           "in the model", ph_suggest(by, cat_predictors), ".", call. = FALSE)
    }
    if (by %in% mains)
      stop(fun_name, "(): '", by, "' cannot be both in 'post_hoc' and the ",
           "'by' factor.", call. = FALSE)
  }

  list(mode = "named", mains = mains, ints = ints, by = by)
}

# Footer for one table: confidence level, alpha, adjustment with this
# table's own estimate and test counts.
ph_footer <- function(adjust, alpha, n_est, n_tests, by = NULL,
                      unit = "Groups") {
  per <- if (!is.null(by)) paste0(" per level of ", by) else ""
  paste0(
    "Confidence level used: ", 1 - alpha, "  \n",
    "Significance level used: \u03b1 = ", alpha, "  \n",
    "P-value and CI adjustment: ", adjust, " method for ", n_est,
    " estimates", if (!is.na(n_tests)) paste0(" / ", n_tests, " tests") else "",
    per, ".  \n\n",
    "*Note: ", unit, " in the \"Letters\" column sharing the same letter are ",
    "**not** significantly different (\u03b1 = ", alpha, "). ", unit, " with ",
    "different letters are significantly different. Sharing a letter ",
    "indicates insufficient evidence to claim a difference; it does not ",
    "prove they are identical.*\n\n")
}

# Caption printed directly under each table heading.
ph_caption <- function(by = NULL) {
  paste0("Letters are compared within this table only",
         if (!is.null(by)) paste0(", separately for each level of ", by) else "",
         ".")
}

# Build one post hoc table.
#   model     fitted model (aov / lm / lmerMod)
#   term      main effect "a" or interaction "a:b"
#   by        NULL or one factor name (simple effects, main effects only)
#   term_ns   TRUE -> letters collapsed to "ns"
#   emm_args  extra arguments for emmeans::emmeans() (e.g. lmer.df)
# Returns list(emm, emmeans, pairs, cld, n_est, n_tests, by_levels) where
# `cld` has a "Letter" column; for `by` tables all three data frames carry a
# column named after `by`.
compute_post_hoc_table <- function(model, term, by = NULL, adjust, alpha,
                                   term_ns = FALSE, emm_args = list()) {
  specs <- ph_parts(term)
  emm_call <- function(...) do.call(emmeans::emmeans,
                                    c(list(model, ...), emm_args))
  one_grid <- function(emm) {
    emm_df  <- as.data.frame(emm)
    pairs_d <- tryCatch(as.data.frame(pairs(emm, adjust = adjust)),
                        error = function(e) NULL)
    cld_d <- tryCatch(as.data.frame(cld_emmeans(emm, alpha = alpha,
                                                Letters = letters,
                                                adjust = adjust,
                                                decreasing = TRUE)),
                      error = function(e) NULL)
    if (!is.null(cld_d) && ".group" %in% names(cld_d)) {
      names(cld_d)[names(cld_d) == ".group"] <- "Letter"
      cld_d$Letter <- trimws(cld_d$Letter)
      if (isTRUE(term_ns)) cld_d$Letter <- "ns"
    }
    rownames(emm_df) <- NULL
    if (!is.null(pairs_d)) rownames(pairs_d) <- NULL
    if (!is.null(cld_d)) rownames(cld_d) <- NULL
    list(emm = emm, emmeans = emm_df, pairs = pairs_d, cld = cld_d)
  }

  if (is.null(by)) {
    g <- one_grid(emm_call(specs = specs))
    g$n_est   <- nrow(g$emmeans)
    g$n_tests <- if (!is.null(g$pairs)) nrow(g$pairs) else NA_integer_
    g$by_levels <- NULL
    return(g)
  }

  # Simple effects: the same grid as emmeans(model, ~ term | by), with
  # letters and adjustment computed separately within each level of `by`.
  emm_by    <- emm_call(specs = specs, by = by)
  by_levels <- unique(as.character(as.data.frame(emm_by)[[by]]))
  parts <- lapply(by_levels, function(lv) {
    at <- stats::setNames(list(lv), by)
    g  <- one_grid(emm_call(specs = specs, at = at))
    add_by <- function(df) {
      if (is.null(df)) return(NULL)
      df[[by]] <- lv
      df[, c(by, setdiff(names(df), by)), drop = FALSE]
    }
    list(emmeans = add_by(g$emmeans), pairs = add_by(g$pairs),
         cld = add_by(g$cld), n_pairs = if (is.null(g$pairs)) NA_integer_
         else nrow(g$pairs), n_est = nrow(g$emmeans))
  })
  bind <- function(key) {
    dfs <- Filter(Negate(is.null), lapply(parts, `[[`, key))
    if (!length(dfs)) return(NULL)
    out <- do.call(rbind, dfs)
    rownames(out) <- NULL
    out
  }
  list(emm = emm_by,
       emmeans = bind("emmeans"), pairs = bind("pairs"), cld = bind("cld"),
       n_est = parts[[1]]$n_est, n_tests = parts[[1]]$n_pairs,
       by_levels = by_levels)
}

# Caution printed above a main-effect table when the factor takes part in a
# significant interaction (same wording as f_lmer()).
ph_caution <- function(trm, trm_sig_ints, cat_ints_shown = trm_sig_ints) {
  pointer <- if (length(cat_ints_shown))
    paste0("Read these letters alongside the interaction cell-means table ",
           "and plot(s) for ", paste0("`", cat_ints_shown, "`", collapse = ", "),
           ", not on their own.") else ""
  paste0("**[!] Interpret with caution:** `", trm, "` takes part in the ",
         "significant interaction(s) ",
         paste0("`", trm_sig_ints, "`", collapse = ", "), ". The ",
         "estimated marginal means below average over the other ",
         "term(s) in that interaction, so they can hide or even ",
         "reverse the real pattern - the effect of `", trm, "` depends ",
         "on the level/value of the interacting term(s). ", pointer)
}

# Display version of a post hoc table: key columns, emmeans columns, Letter,
# and the observed n per group (counted in data_complete), row order kept.
ph_display_table <- function(cld_df, data_complete, transformed) {
  if (is.null(cld_df)) return(NULL)
  key_cols <- setdiff(names(cld_df), c("emmean", "median (BT)", "SE", "df",
                                       "lower.CL", "upper.CL", "Letter",
                                       "scale"))
  keep <- c(key_cols, intersect(c("emmean", "median (BT)", "SE",
                                  "lower.CL", "upper.CL", "Letter"),
                                names(cld_df)))
  out <- cld_df[, keep, drop = FALSE]
  ncnt <- tryCatch(
    stats::aggregate(
      stats::setNames(list(rep(1L, nrow(data_complete))), "n"),
      by = lapply(stats::setNames(key_cols, key_cols),
                  function(k) as.character(data_complete[[k]])),
      FUN = sum),
    error = function(e) NULL)
  if (!is.null(ncnt)) {
    for (k in key_cols) out[[k]] <- as.character(out[[k]])
    out$.ord <- seq_len(nrow(out))
    out <- merge(out, ncnt, by = key_cols, all.x = TRUE, sort = FALSE)
    out <- out[order(out$.ord), , drop = FALSE]
    out$.ord <- NULL
  }
  if (!isTRUE(transformed))
    names(out)[names(out) == "emmean"] <- "emmean.."
  rownames(out) <- NULL
  out
}

# Build all post hoc tables for one response (used by f_aov() and f_lm()).
#   mains / ints / by  resolved terms (see resolve_post_hoc_spec)
#   sig_interactions   all significant interaction terms (for the caution)
#   cat_sig_ints       the significant all-categorical interactions
#   p_of               function(term) -> p-value from the ANOVA table
#   term_names         terms that have a p-value
#   bt_fn, transformed back-transformation of estimates / CIs
# Returns a named list of tables, each list(emmeans, pairs, cld, cld_text,
# caution) with attributes "kind" (cells / marginal / simple), "term_ns",
# "emm" and "display" (the table as shown in the report and print()).
build_post_hoc_tables <- function(model, mains, ints, by, sig_interactions,
                                  cat_sig_ints, p_of, term_names, adjust,
                                  alpha, conf_level, bt_fn, transformed,
                                  data_complete, fun_name) {
  if (is.null(bt_fn)) bt_fn <- identity
  # Back-transformed means are medians on the original scale, and the SE
  # (transformed scale) is not valid there, so it is dropped.
  bt_table <- function(df) {
    if (is.null(df) || !isTRUE(transformed)) return(df)
    for (cc in intersect(c("emmean", "lower.CL", "upper.CL"), names(df)))
      df[[cc]] <- bt_fn(df[[cc]])
    names(df)[names(df) == "emmean"] <- "median (BT)"
    df$SE <- NULL
    df
  }
  # Reporting order: interactions first (the reference grouping when
  # present), then main effects.
  jobs <- c(
    lapply(ints, function(it) list(term = it, by = NULL, kind = "cells")),
    lapply(mains, function(mn) list(term = mn, by = by,
                                    kind = if (is.null(by)) "marginal" else "simple")))
  res <- list()
  for (job in jobs) {
    trm <- job$term
    if (is.null(job$by)) {
      term_ns <- is.na(p_of(trm)) || p_of(trm) >= alpha
    } else {
      # Simple effects: "ns" only when the factor shows no effect at all,
      # i.e. neither its main effect nor its interaction with `by`.
      int_by <- intersect(c(paste(trm, job$by, sep = ":"),
                            paste(job$by, trm, sep = ":")), term_names)
      term_ns <- (is.na(p_of(trm)) || p_of(trm) >= alpha) &&
        (!length(int_by) || all(p_of(int_by) >= alpha))
    }
    tab <- tryCatch(
      compute_post_hoc_table(model, trm, by = job$by, adjust = adjust,
                             alpha = alpha, term_ns = term_ns,
                             emm_args = list(level = conf_level)),
      error = function(e) {
        warning(fun_name, ": post hoc for '", trm, "' could not be computed: ",
                conditionMessage(e), call. = FALSE, immediate. = TRUE)
        NULL
      })
    if (is.null(tab)) next
    trm_sig_ints <- if (identical(job$kind, "marginal"))
      sig_interactions[vapply(sig_interactions, function(it)
        trm %in% ph_parts(it), logical(1))] else character(0)
    caution <- if (length(trm_sig_ints))
      ph_caution(trm, trm_sig_ints, intersect(trm_sig_ints, cat_sig_ints)) else NULL
    key <- if (is.null(job$by)) trm else paste0(trm, " | ", job$by)
    cld_bt <- bt_table(tab$cld)
    res[[key]] <- list(
      emmeans  = bt_table(tab$emmeans),
      pairs    = tab$pairs,
      cld      = cld_bt,
      cld_text = ph_footer(adjust, alpha, tab$n_est, tab$n_tests, by = job$by,
                           unit = if (job$kind == "cells") "Cells" else "Groups"),
      caution  = caution)
    attr(res[[key]], "kind")    <- job$kind
    attr(res[[key]], "term_ns") <- term_ns
    attr(res[[key]], "emm")     <- tab$emm
    attr(res[[key]], "display") <- ph_display_table(cld_bt, data_complete, transformed)
  }
  res
}
