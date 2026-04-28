# =============================================================================
# Save and restore session state (par, options, panderOptions)
# =============================================================================
#
# Internal helpers used by every f_* function in the package that
# temporarily mutates graphical parameters or global options. The pair
# is meant to be used together at the top of a function, like this:
#
#   my_function <- function(...) {
#     .session_state <- save_session_state()
#     on.exit(restore_session_state(.session_state), add = TRUE)
#     # ... function body that freely mutates par() / options() ...
#   }
#
# save_session_state() captures a snapshot of everything worth
# restoring. restore_session_state() puts it all back, tolerating any
# individual restore error so that one bad value can't abort the entire
# on.exit() cleanup.
#
# NOTE: on.exit() must be called from the function whose state we
# want to restore, NOT from inside a helper. That is why this module
# exposes two separate functions instead of a single
# "setup_exit_handler()" wrapper: on.exit() inside a helper would
# register its handler on the helper itself, which exits immediately,
# and the real caller would never get the cleanup.
# =============================================================================


# -----------------------------------------------------------------------------
# save_session_state
# -----------------------------------------------------------------------------
# Captures the current graphical parameters, global options, and
# panderOptions into a single list suitable for later restoration.
#
# Returns a list with three elements:
#   $par     - named list from par(no.readonly = TRUE), with unsafe keys
#              stripped (see below)
#   $options - named list from options()
#   $pander  - named list from pander::panderOptions(), or NULL if the
#              pander package is not available
#
# Why some par() keys are stripped:
#
# par(no.readonly = TRUE) returns every readable graphical parameter,
# but several of them describe the CURRENT plot region on the CURRENT
# device and become meaningless (or invalid) once the device state has
# changed. Restoring them after the function has drawn into a new
# device, changed the layout, or closed and reopened a graphics device
# raises errors like
#
#     invalid value specified for graphical parameter "fig"
#
# which is a perennial CRAN example-check failure. The stripped keys:
#
#   $new  - write-only trigger; saving / restoring it is meaningless
#   $fig  - figure region in normalised device coordinates; tied to
#           the device that was open at snapshot time
#   $plt  - plot region in normalised figure coordinates; same issue
#   $fin  - figure region dimensions in inches
#   $pin  - plot region dimensions in inches
#   $usr  - user coordinate ranges for the current plot; undefined
#           before any high-level plot has been drawn
#
# Everything else ("mar", "mfrow", "mfcol", "bg", "col", "cex", ...)
# is safe to save and restore across arbitrary device activity.
# -----------------------------------------------------------------------------

save_session_state <- function() {

  old_par <- graphics::par(no.readonly = TRUE)

  # Strip parameters that cannot be safely restored. See the block
  # comment above for the rationale.
  old_par$new <- NULL
  old_par$fig <- NULL
  old_par$plt <- NULL
  old_par$fin <- NULL
  old_par$pin <- NULL
  old_par$usr <- NULL

  old_options <- options()

  old_options$warn <- NULL
  old_options$warning.length <- NULL
  old_options$nwarnings <- NULL

  # pander is a soft dependency. Only snapshot its options if the
  # package is actually available and its API is what we expect.
  old_pander <- if (requireNamespace("pander", quietly = TRUE) &&
                    is.function(pander::panderOptions)) {
    pander::panderOptions()
  } else {
    NULL
  }

  list(
    par     = old_par,
    options = old_options,
    pander  = old_pander
  )
}


# -----------------------------------------------------------------------------
# restore_session_state
# -----------------------------------------------------------------------------
# Restores a snapshot produced by save_session_state(). Each of the
# three sub-restores is wrapped in tryCatch() so that a failure in one
# cannot prevent the others from running. After stripping the known-
# bad par() keys in save_session_state() the par() restore should
# always succeed in practice, but the guard is kept as
# belt-and-suspenders: on.exit() handlers run during function exit
# (including error exits), and raising a new error there would mask
# the original problem.
#
# Arguments:
#   state - a list produced by save_session_state(). Must contain
#           elements named "par", "options", and "pander" (the last
#           may be NULL).
#
# Returns NULL invisibly.
# -----------------------------------------------------------------------------

restore_session_state <- function(state) {

  # Guard against being called with a malformed or NULL snapshot
  # (e.g. if a caller forgot to assign the return value of
  # save_session_state() before registering on.exit()).
  if (is.null(state) || !is.list(state)) {
    return(invisible(NULL))
  }

  # Reset the layout matrix first. layout() is not captured by
  # par(no.readonly = TRUE), so a layout set during the function
  # body would otherwise persist and affect subsequent plots.
  # layout(1) is cheap and idempotent, so we always run it.
  tryCatch(
    graphics::layout(1),
    error = function(e) invisible(NULL)
  )


  # ---- Restore graphical parameters ---------------------------------------
  if (!is.null(state$par) && length(state$par) > 0) {
    tryCatch(
      graphics::par(state$par),
      error   = function(e) invisible(NULL)
      # ,warning = function(w) invisible(NULL)
    )
  }

  # ---- Restore global options ---------------------------------------------
  if (!is.null(state$options)) {
    tryCatch(
      options(state$options),
      error   = function(e) invisible(NULL)
      # ,warning = function(w) invisible(NULL)
    )
  }

  # ---- Restore panderOptions ----------------------------------------------
  # panderOptions() does not accept a whole-list form; each option
  # must be restored individually. If pander was not available at
  # snapshot time, state$pander is NULL and this block is skipped.
  if (!is.null(state$pander) &&
      requireNamespace("pander", quietly = TRUE) &&
      is.function(pander::panderOptions)) {

    for (opt in names(state$pander)) {
      tryCatch(
        pander::panderOptions(opt, state$pander[[opt]]),
        error   = function(e) invisible(NULL)
        # ,warning = function(w) invisible(NULL)
      )
    }
  }

  invisible(NULL)
}
