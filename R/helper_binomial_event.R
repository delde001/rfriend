# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# helper_binomial_event.R
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Recode a categorical (factor / character / logical) response to 0/1 for a
# binomial GLM, with the user choosing the "event" (coded 1) explicitly via
# f_glm(..., event = ).
#
# Why explicit: base glm() silently codes the FIRST factor level as 0 and ALL
# other levels as 1. Levels sort alphabetically, so whether a model estimates
# P(diseased) or P(healthy) depends on the spelling of the labels, and the sign
# of every coefficient flips with it. With three or more levels glm() pools
# levels 2..k as "success" without saying so. rfriend never guesses: it stops
# with an explanation until the user names the event, then states the coding
# in the console, the report and the returned object.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Pick the event specification that applies to one response.
#   event = NULL                    -> NULL
#   event = "yes" / c("a", "b")     -> applies to every categorical response
#   event = c(infected = "yes")     -> per response (named vector)
#   event = list(stage = c("a","b"))-> per response, several levels allowed
event_for_response <- function(event, response) {
  if (is.null(event)) return(NULL)
  nms <- names(event)
  if (!is.null(nms) && all(nzchar(nms))) {
    if (!response %in% nms) return(NULL)
    return(event[[response]])
  }
  if (is.list(event)) {
    stop("f_glm(): 'event' given as a list must be named by response, e.g. ",
         "event = list(infected = \"yes\").", call. = FALSE)
  }
  event
}

# Quote and count levels for messages: "no" (n = 45), "yes" (n = 15)
fmt_levels_n <- function(lv, y) {
  paste0("\"", lv, "\" (n = ", vapply(lv, function(l) sum(y == l, na.rm = TRUE),
                                         integer(1)), ")", collapse = ", ")
}
fmt_q <- function(x) paste0("\"", x, "\"", collapse = ", ")

# Validate the event choice and recode. Returns list(y = 0/1 integer vector,
# coding = list(...)). Stops with a teaching message when the choice is
# missing or invalid.
recode_binomial_event <- function(y, response, event, call_example) {

  # Observed levels, in factor order when the response is a factor.
  if (is.logical(y)) {
    y  <- ifelse(is.na(y), NA_character_, ifelse(y, "TRUE", "FALSE"))
    lv <- intersect(c("FALSE", "TRUE"), unique(y[!is.na(y)]))
    if (is.null(event)) event <- "TRUE"   # TRUE = 1 is unambiguous
    if (is.logical(event)) event <- as.character(event)
  } else if (is.factor(y)) {
    lv <- levels(droplevels(y))
    y  <- as.character(y)
  } else {
    lv <- sort(unique(y[!is.na(y)]))
  }

  if (length(lv) < 2) {
    stop("f_glm(): the response '", response, "' has only one observed value (",
         fmt_q(lv), "). A binomial model needs two outcomes to compare.",
         call. = FALSE)
  }

  # ---- No event given: explain and show how -------------------------------
  if (is.null(event)) {
    if (length(lv) == 2) {
      stop(
        "f_glm(): the response '", response, "' is categorical with levels ",
        fmt_levels_n(lv, y), ".\n",
        "A binomial GLM models the probability of ONE outcome, the event ",
        "(coded 1), against the other (coded 0). rfriend does not guess which ",
        "one you mean: the choice flips the sign of every effect.\n\n",
        "Tell f_glm() which level is the event, for example:\n",
        "  ", sub("EVENT", lv[2], call_example, fixed = TRUE), "\n",
        "This models P(", response, " = \"", lv[2], "\"). Use event = \"", lv[1],
        "\" to model P(", response, " = \"", lv[1], "\") instead.",
        call. = FALSE)
    }
    stop(
      "f_glm(): the response '", response, "' is categorical with ", length(lv),
      " levels: ", fmt_levels_n(lv, y), ".\n",
      "A binomial GLM compares exactly TWO outcomes, so choose which level(s) ",
      "count as the event (coded 1). All other levels are pooled as the ",
      "non-event (coded 0).\n\n",
      "  One level:      event = \"", lv[length(lv)], "\"   -> P(", lv[length(lv)],
      ") vs ", paste(lv[-length(lv)], collapse = " + "), "\n",
      "  Several levels: event = c(", fmt_q(lv[-1]), ")   -> P(",
      paste(lv[-1], collapse = " or "), ") vs ", lv[1], "\n\n",
      "Example:\n  ", sub("EVENT", lv[length(lv)], call_example, fixed = TRUE), "\n\n",
      "If the levels must stay separate categories, a binomial model is not ",
      "the right tool: use a multinomial model (nnet::multinom()) or, for ",
      "ordered levels, an ordinal model (MASS::polr()).",
      call. = FALSE)
  }

  # ---- Event given: validate ----------------------------------------------
  event <- unique(as.character(event))
  if (anyNA(event) || !length(event)) {
    stop("f_glm(): 'event' for '", response, "' must be one or more level ",
         "names, not NA.", call. = FALSE)
  }
  unknown <- setdiff(event, lv)
  if (length(unknown)) {
    stop("f_glm(): event = ", fmt_q(unknown), " is not an observed level of ",
         "the response '", response, "'. Its levels are: ", fmt_levels_n(lv, y),
         ". Level names are case-sensitive.", call. = FALSE)
  }
  non_event <- setdiff(lv, event)
  if (!length(non_event)) {
    stop("f_glm(): every level of '", response, "' was named as an event (",
         fmt_q(event), "), so nothing is left to compare against. Name only ",
         "the level(s) whose probability you want to model.", call. = FALSE)
  }

  y01 <- ifelse(is.na(y), NA_integer_, as.integer(y %in% event))

  coding <- list(
    response    = response,
    event       = event,
    non_event   = non_event,
    n_event     = sum(y01 == 1L, na.rm = TRUE),
    n_non_event = sum(y01 == 0L, na.rm = TRUE),
    n_na        = sum(is.na(y01)),
    pooled      = length(lv) > 2,
    label       = paste0("P(", response, " = ",
                         paste(event, collapse = " or "), ")")
  )
  coding$text <- event_coding_text(coding)
  list(y = y01, coding = coding)
}

# One plain-text sentence describing the coding (console, report, object).
event_coding_text <- function(cd) {
  txt <- paste0(
    "Response '", cd$response, "' coded as 1 = ", fmt_q(cd$event),
    " (event, n = ", cd$n_event, "), 0 = ", fmt_q(cd$non_event),
    if (length(cd$non_event) > 1) " (pooled, " else " (",
    "n = ", cd$n_non_event, ")",
    if (cd$n_na > 0) paste0(", ", cd$n_na, " missing left as NA") else "",
    ". Probabilities in the output are ", cd$label, ".")
  if (isTRUE(cd$pooled)) {
    txt <- paste0(txt, " The response has more than two levels; the levels ",
                  "coded 0 are analysed as one group. If they must stay ",
                  "separate, use nnet::multinom() or, for ordered levels, ",
                  "MASS::polr().")
  }
  txt
}
