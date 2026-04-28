#' Correlation Plots with Factor Detection and Multiple Correlation Coefficients
#'
#' Creates correlation plots for numeric variables in a data frame. The upper
#' triangle displays Pearson \eqn{r}{r}, Spearman \eqn{\rho}{rho}, and Kendall
#' \eqn{\tau}{tau} simultaneously for each pair. Factor variables are automatically detected and
#' used for grouping, i.e. point colouring and shaping. Ordinal variables are supported via
#' \code{ordinal_vars}: their diagonal labels are italicised and Pearson \eqn{r}{r}
#' is greyed and bracketed for any pair that involves them.A separate legend file documents both
#' the grouping factors and the meaning of all three correlation symbols.
#'
#' @param data A \code{data.frame} containing the dataset. Must include at least
#'   two numeric variables.
#' @param detect_factors Logical. If \code{TRUE}, factor variables are
#'   automatically detected for colouring and shaping points. Default \code{TRUE}.
#' @param factor_table Logical. If \code{TRUE}, prints a detailed table of
#'   converted factors to the console. Default \code{FALSE}.
#' @param factor_select A character vector specifying the names of the columns to convert into factors. If \code{NULL}, the function automatically detects columns that should be factors based on their data type and unique value count. Default is \code{NULL}.
#' @param factor_exclude A character vector specifying the names of the columns NOT to convert into factors. If \code{NULL}, no columns are excluded. Default is \code{NULL}.
#' @param unique_num_treshold  Numeric. A threshold of the amount of unique numbers a numeric column should have to keep it numeric, i.e. omit factor conversion. Default \code{8}.
#' @param repeats_threshold  Numeric. A threshold of the minimal number of repeats a numeric column should have to convert it to a factor. Default \code{2}.
#' @param color_factor Character. Name of the factor variable used for point
#'   colours; \code{"auto"} selects automatically. Default \code{"auto"}.
#' @param shape_factor Character. Name of the factor variable used for point
#'   shapes; \code{"auto"} selects automatically. Default \code{"auto"}.
#' @param print_legend Logical. If \code{TRUE}, a separate legend file is
#'   created. Default \code{TRUE}.
#' @param fancy_names Named character vector or \code{NULL}. Maps column names
#'   to display names used in the plot and legend.
#' @param ordinal_vars Character vector or \code{NULL}. Names of variables to
#'   treat as ordinal. Ordered factors are coerced to integer ranks; other
#'   non-numeric types are coerced similarly. Their diagonal labels are
#'   italicised and Pearson \eqn{r}{r} is greyed and bracketed for any pair that
#'   involves them. Ordinal variables are included in the correlation panels but
#'   excluded from aesthetic factor detection. Default \code{NULL}.
#' @param width Numeric. Plot width in centimetres. Default 15.
#' @param height Numeric. Plot height in centimetres. Default 15.
#' @param res Numeric. Resolution in DPI. Default 600.
#' @param pointsize Numeric. Base font size. Default 8.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param output_type Character. One of \code{"pdf"}, \code{"word"},
#'   \code{"png"}, or \code{"rmd"}. Default \code{"word"}.
#' @param save_as Character or \code{NULL}. Output file path without extension.
#'   A full path, a filename, or a directory (with trailing slash) are all
#'   accepted. Providing an extension overrides \code{output_type}.
#'   Default saves to \code{tempdir()}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves to the working directory.
#'   Overridden by \code{save_as}. Default \code{FALSE}.
#'
#' @details
#' \itemize{
#'   \item \strong{Three correlations per panel:} Every upper-triangle panel
#'     shows \eqn{r}{r} (Pearson), \eqn{\rho}{rho} (Spearman), and \eqn{\tau}{tau}
#'     (Kendall) stacked vertically, so the reader can choose the most
#'     appropriate coefficient for each variable pair.
#'   \item \strong{Ordinal variables:} Specify column names with
#'     \code{ordinal_vars}. Those variables appear in italic on the diagonal.
#'     For any pair where at least one variable is ordinal, Pearson \eqn{r}{r} is
#'     shown greyed and in parentheses to signal it is technically inappropriate;
#'     Spearman and Kendall remain prominent.
#'   \item \strong{Factor detection:} Only unordered factors are used for
#'     colour/shape aesthetics. Ordered factors (\code{is.ordered()}) are
#'     treated as ordinal data, not as grouping variables.
#'   \item \strong{Legend:} The legend file documents the grouping factor levels
#'     (when present) and always includes an explanation of all three
#'     correlation symbols whenever a legend is generated.
#'   \item \strong{Constant columns:} Zero-variance columns produce \code{NA}
#'     in all correlation panels.
#' }
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @return No value is returned to the R environment. Output files are saved
#'   and opened automatically.
#'
#' @author Sander H. van Delden \email{plantmind@proton.me}
#'
#' @examples
#' data(mtcars)
#' mtcars_sub <- subset(mtcars, select = -c(am, qsec, vs))
#' f_corplot(mtcars_sub,
#'           color_factor = "gear",
#'           shape_factor = "cyl",
#'           output_type  = "png"
#'           )
#'
#' # With ordinal variables
#' data(iris)
#' fancy_names <- c(Sepal.Length = "Sepal Length (cm)",
#'                  Sepal.Width  = "Sepal Width (cm)")
#' f_corplot(iris,
#'           fancy_names  = fancy_names,
#'           ordinal_vars = "Petal.Width",
#'           output_type  = "png",
#'           open_generated_files = FALSE)
#'
#' @export
f_corplot <- function(data,
                      detect_factors        = TRUE,
                      factor_table          = FALSE,
                      factor_exclude        = NULL,
                      factor_select         = NULL,
                      unique_num_treshold   = 8,
                      repeats_threshold     = 2,
                      color_factor          = "auto",
                      shape_factor          = "auto",
                      print_legend          = TRUE,
                      fancy_names           = NULL,
                      ordinal_vars          = NULL,
                      width                 = 15,
                      height                = 15,
                      res                   = 600,
                      pointsize             = 10,
                      close_generated_files = FALSE,
                      open_generated_files  = interactive(),
                      output_type           = "word",
                      save_as               = NULL,
                      save_in_wdir          = FALSE) {

  ##########################################################################
  # Save and restore settings on exit
  ##########################################################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state


  ##########################################################################
  # Validate output_type
  ##########################################################################
  if (!(output_type %in% c("pdf", "word", "png", "rmd")))
    stop('output_type must be one of: "pdf", "word", "png" or "rmd"')

  ##########################################################################
  # Extract data name
  ##########################################################################
  data_name      <- deparse(substitute(data))
  file_extension <- NULL   # must come before the save_as resolution block

  ##########################################################################
  # Resolve output path
  ##########################################################################
  save_dir <- if (isTRUE(save_in_wdir)) getwd() else tempdir()

  output_type_map <- c(pdf = ".pdf", word = ".docx", png = ".png")

  if (!is.null(save_as) || isTRUE(save_in_wdir)) {

    if (!is.null(save_as)) {
      save_as                <- gsub("\\\\", "/", save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if (!isFALSE(file_extension_save_as[1]))
        file_extension <- file_extension_save_as
    }

    if (is.null(file_extension) && output_type == "word") {
      output_path <- get_save_path(
        save_as      = save_as,
        default_name = paste(data_name, "CorPlot", sep = "_"),
        default_dir  = save_dir,
        file.ext     = ".docx")
      output_type <- "word"

    } else if (is.null(file_extension)) {
      file.ext    <- unname(output_type_map[output_type])
      output_path <- get_save_path(
        save_as      = save_as,
        default_name = paste(data_name, "CorPlot", sep = "_"),
        default_dir  = save_dir,
        file.ext     = file.ext)

    } else {
      output_path <- get_save_path(
        save_as      = save_as,
        default_name = paste(data_name, "CorPlot", sep = "_"),
        default_dir  = save_dir,
        file.ext     = file_extension[1])
      output_type <- file_extension[2]
    }

  } else {
    file.ext    <- unname(output_type_map[output_type])
    output_path <- get_save_path(
      save_as      = save_as,
      default_name = paste(data_name, "CorPlot", sep = "_"),
      default_dir  = save_dir,
      file.ext     = file.ext)
  }

  if (output_type == "rmd") close_generated_files <- FALSE

  # Cross-platform close_generated_files (was Windows-only taskkill)
  if (output_type != "rmd" && isTRUE(close_generated_files)) {
    close_app <- function(win_proc, mac_name, linux_name) {
      sysname <- Sys.info()[["sysname"]]
      if (.Platform$OS.type == "windows") {
        system(paste0("taskkill /im ", win_proc, " /f"),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      } else if (sysname == "Darwin") {
        system(paste0("pkill -f '", mac_name, "'"),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      } else {
        system(paste0("pkill -f ", linux_name),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      }
    }
    if (output_type == "word")  close_app("WINWORD.EXE", "Microsoft Word",  "soffice")
  }

  # guard dev.off() -- only close if a non-null device is active.
   if (grDevices::dev.cur() > 1L) grDevices::dev.off()

  ##########################################################################
  # Prepare temporary Rmd output file
  ##########################################################################
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")
  file.create(temp_output_file)

  ##########################################################################
  # Apply fancy_names (renames columns, factors, and ordinal_vars)
  ##########################################################################
  if (!is.null(fancy_names)) {
    data         <- f_rename_columns(data, fancy_names)
    color_factor <- f_rename_vector(color_factor, fancy_names)
    shape_factor <- f_rename_vector(shape_factor, fancy_names)
    if (!is.null(ordinal_vars) && ordinal_vars %in% names(fancy_names)) {
      ordinal_vars <- f_rename_vector(ordinal_vars, fancy_names)
    }
  }

  ##########################################################################
  # Coerce ordinal_vars to integer so they appear in the numeric matrix
  ##########################################################################
  if (!is.null(ordinal_vars)) {
    for (ov in ordinal_vars) {
      if (!ov %in% names(data)) {
        warning("ordinal_vars: '", ov, "' not found in data \u2014 ignored.")
        ordinal_vars <- setdiff(ordinal_vars, ov)
        next
      }
      if (is.ordered(data[[ov]])) {
        data[[ov]] <- as.integer(data[[ov]])
        message("Ordinal variable '", ov, "' coerced to integer ranks.")
      } else if (!is.numeric(data[[ov]])) {
        data[[ov]] <- as.integer(factor(data[[ov]]))
        message("Ordinal variable '", ov, "' coerced to integer.")
      }
      # Already numeric: keep as-is; annotation only
    }
    if (length(ordinal_vars) == 0L) ordinal_vars <- NULL
  }

  ##########################################################################
  # Factor detection and assignment
  ##########################################################################
  factor_count <- 0L

  if (color_factor == "auto" && shape_factor == "auto") {
    if (isTRUE(detect_factors)) {
      data <- f_factors(
        data,
        properties          = factor_table,
        exclude             = factor_exclude,
        select              = factor_select,
        unique_num_treshold = unique_num_treshold,
        repeats_threshold   = repeats_threshold
      )
    }


    # Exclude ordered factors -- those are ordinal data, not grouping aesthetics
    factor_vars  <- vapply(data,
                           function(x) is.factor(x) && !is.ordered(x),
                           logical(1))
    factor_count <- sum(factor_vars)

    if (factor_count > 2){
      warning("Data contains more than 2 factors; only the first 2 will be used: ",
              names(factor_vars[factor_vars])[1], " and ",
              names(factor_vars[factor_vars])[2],
              "\nFactors can be selected manually with ",
              "'color_factor =' and 'shape_factor ='.")
    }

    if (factor_count >= 2) {
      color_factor <- names(factor_vars[factor_vars])[1]
      shape_factor <- names(factor_vars[factor_vars])[2]
    } else if (factor_count == 1) {
      color_factor <- names(factor_vars[factor_vars])[1]
      shape_factor <- "auto"
    }

  } else {
    # Count specified factors so the legend is not
    # silently suppressed when the user sets color_factor/shape_factor by name.
    if (color_factor != "auto" && color_factor %in% names(data))
      factor_count <- factor_count + 1L
    if (shape_factor != "auto" && shape_factor %in% names(data))
      factor_count <- factor_count + 1L
  }

  ##########################################################################
  # Validate factor names and auto-convert if needed
  ##########################################################################
  if (color_factor != "auto" && !color_factor %in% names(data))
    stop("The defined color factor '", color_factor,
         "' was not found in the data.")
  if (shape_factor != "auto" && !shape_factor %in% names(data))
    stop("The defined shape factor '", shape_factor,
         "' was not found in the data.")

  if (color_factor != "auto" && !is.factor(data[[color_factor]])) {
    data[[color_factor]] <- factor(data[[color_factor]])
    message("\nVariable '", color_factor, "' was converted to a factor.")
  }
  if (shape_factor != "auto" && !is.factor(data[[shape_factor]])) {
    data[[shape_factor]] <- factor(data[[shape_factor]])
    message("Variable '", shape_factor, "' was converted to a factor.\n")
  }

  if (factor_count == 0L)
    message("No factors in data. ",
            "Factors can be added via 'color_factor =' and 'shape_factor ='.")

  ##########################################################################
  # Identify numeric columns (ordinal_vars are now integer, so included)
  ##########################################################################
  numeric_vars  <- vapply(data, is.numeric, logical(1))
  numeric_count <- sum(numeric_vars)
  if (numeric_count < 2L)
    stop("At least two numeric variables are required in data.")


  # calculate a dynamic cex for panel text
  # Scales from ~1.2 (2-3 vars, large panels) down to ~0.6 (10+ vars, small panels)
  panel_cex <- max(0.55, min(1.2, 2.8 / sqrt(numeric_count)))

  ##########################################################################
  # Colour and shape mappings
  ###########################################################################
  .make_shapes <- function(n) {
    shapes <- if (n <= 5L) {
      seq(21L, 21L + n - 1L)
    } else {
      c(21L:25L, seq(3L, 3L + n - 6L))
    }
    if (length(shapes) == 3L) shapes[shapes == 23L] <- 24L
    if (length(shapes) >  3L) shapes[c(1,2,3,4)]    <- shapes[c(1,2,4,3)]
    shapes
  }

  if (factor_count >= 2L) {
    color_levels       <- levels(data[[color_factor]])
    shape_levels       <- levels(data[[shape_factor]])
    color_levels_count <- length(color_levels)
    shape_levels_count <- length(shape_levels)

    colors                <- rainbow(color_levels_count, alpha = 0.07)
    color_mapping         <- colors[unclass(data[[color_factor]])]
    border_colors         <- rainbow(color_levels_count, v = 0.7)
    border_colors_mapping <- border_colors[unclass(data[[color_factor]])]

    shapes        <- .make_shapes(shape_levels_count)
    shape_mapping <- shapes[unclass(data[[shape_factor]])]

    legend_colors        <- rainbow(color_levels_count, alpha = 0.1)
    legend_border_colors <- rainbow(color_levels_count, v = 0.7)
    legend_              <- as.vector(outer(color_levels, shape_levels,
                                            FUN = function(c, s) paste(c, "-", s)))
    legend_title         <- paste("Levels of:", color_factor, "-", shape_factor)
    legend_col   <- rep(legend_border_colors, times = shape_levels_count)
    legend_pch   <- rep(shapes, each = color_levels_count)
    legend_pt.bg <- rep(legend_colors, times = shape_levels_count)
    legend_ncol           <- 4L

  } else if (factor_count == 1L) {

    if (shape_factor == "auto") {
      # Color factor only
      color_levels          <- levels(data[[color_factor]])
      color_levels_count    <- length(color_levels)
      colors                <- rainbow(color_levels_count, alpha = 0.07)
      color_mapping         <- colors[unclass(data[[color_factor]])]
      shape_mapping         <- 21L
      border_colors         <- rainbow(color_levels_count, v = 0.7)
      border_colors_mapping <- border_colors[unclass(data[[color_factor]])]
      legend_colors         <- rainbow(color_levels_count, alpha = 0.1)
      legend_border_colors  <- rainbow(color_levels_count, v = 0.7)
      legend_               <- as.vector(color_levels)
      legend_title          <- paste("Levels of:", color_factor)
      legend_col            <- legend_border_colors
      legend_pch            <- 21L
      legend_pt.bg          <- legend_colors
      legend_ncol           <- 4L

    } else if (color_factor == "auto") {
      # Shape factor only
      shape_levels       <- levels(data[[shape_factor]])
      shape_levels_count <- length(shape_levels)
      shapes             <- .make_shapes(shape_levels_count)
      shape_mapping      <- shapes[unclass(data[[shape_factor]])]
      color_mapping      <- "white"
      border_colors_mapping <- "black"
      legend_            <- as.vector(shape_levels)
      legend_title       <- paste("Levels of:", shape_factor)
      legend_col         <- "black"
      legend_pch         <- shapes
      legend_pt.bg       <- "white"
      legend_ncol        <- 4L
    }

  } else {
    color_mapping         <- "white"
    border_colors_mapping <- "black"
    shape_mapping         <- 16L
  }

  ##########################################################################
  # Precompute all three correlation matrices
  ##########################################################################
  data_num  <- data[, numeric_vars, drop = FALSE]
  num_names <- names(data_num)

  cor_p <- suppressWarnings(cor(data_num, method = "pearson",  use = "na.or.complete"))
  cor_s <- suppressWarnings(cor(data_num, method = "spearman", use = "na.or.complete"))
  cor_k <- suppressWarnings(cor(data_num, method = "kendall",  use = "na.or.complete"))

  cor_p[!is.finite(cor_p)] <- NA_real_
  cor_s[!is.finite(cor_s)] <- NA_real_
  cor_k[!is.finite(cor_k)] <- NA_real_

  fmt_r <- function(x)
    if (is.na(x)) "NA" else formatC(round(x, 2L), format = "f", digits = 2L)

  ##########################################################################
  # Panel functions
  ##########################################################################

  # Upper triangle: r, rho, tau stacked.
  # Uses a closure over data_num, num_names, cor_p/s/k, ordinal_vars.
  # Matching x/y back to column names by identity is reliable because
  # pairs() passes the original data columns without copying.
  panel.triple.cor <- function(x, y, ...) {
    usr <- par("usr")
    cx  <- mean(usr[1:2])
    cy  <- mean(usr[3:4])
    dy  <- diff(usr[3:4]) * 0.19   # vertical spacing between the three lines

    xi     <- which(vapply(data_num, identical, logical(1L), x))[1L]
    yi     <- which(vapply(data_num, identical, logical(1L), y))[1L]
    x_name <- if (!is.na(xi)) num_names[xi] else ""
    y_name <- if (!is.na(yi)) num_names[yi] else ""

    pair_has_ordinal <- !is.null(ordinal_vars) &&
                        (x_name %in% ordinal_vars || y_name %in% ordinal_vars)

    r_p <- if (!is.na(xi) && !is.na(yi)) cor_p[xi, yi] else NA_real_
    r_s <- if (!is.na(xi) && !is.na(yi)) cor_s[xi, yi] else NA_real_
    r_k <- if (!is.na(xi) && !is.na(yi)) cor_k[xi, yi] else NA_real_

    # Pearson: greyed and bracketed when either variable is ordinal
    p_col <- if (pair_has_ordinal) "grey60" else "black"
    p_lab <- if (pair_has_ordinal) paste0("(", fmt_r(r_p), ")") else fmt_r(r_p)

    text(cx, cy + dy, bquote(italic(r)   == .(p_lab)), cex = panel_cex, col = p_col)
    text(cx,
         cy,
         bquote(italic(rho) == .(fmt_r(r_s))),
         cex = panel_cex,
         col = "black")
    text(cx,
         cy - dy,
         bquote(italic(tau) == .(fmt_r(r_k))),
         cex = panel_cex,
         col = "black")
  }

  # Diagonal text: italic label for ordinal variables.
  # Overrides pairs()'s text.panel; receives the column label as 'txt'.
  text.panel.custom <- function(x = 0.5, y = 0.5, txt, cex, font, ...) {
    use_font <- if (!is.null(ordinal_vars) && txt %in% ordinal_vars) 3L else font
    text(x, y, txt, cex = cex, font = use_font)
  }

  ##########################################################################
  # PNG file paths
  ##########################################################################
  if (output_type != "png") {
    png_path_corplot <- file.path(temp_output_dir, "temp_CorPlot.png")
    png_path_legend  <- file.path(temp_output_dir, "temp_Legend.png")
  } else {
    png_path_corplot <- output_path
    png_path_legend  <- if (grepl("_CorPlot\\.png$", png_path_corplot)) {
      sub("_CorPlot\\.png$", "_Legend.png", png_path_corplot)
    } else {
      sub("\\.png$", "_Legend.png", png_path_corplot)
    }
    message("Saving output in: ", png_path_corplot,
            " and ", png_path_legend)
  }
  png_path_corplot <- gsub("\\\\", "/", png_path_corplot)
  png_path_legend  <- gsub("\\\\", "/", png_path_legend)

  ##########################################################################
  # Draw the correlation plot
  ##########################################################################
  png(png_path_corplot,
      width     = width,
      height    = height,
      units     = "cm",
      res       = res,
      pointsize = pointsize)

  pairs(
    data_num,
    bg          = color_mapping,
    col         = border_colors_mapping,
    pch         = shape_mapping,
    lwd         = 0.5,
    font.labels = 1L,
    cex.labels  = 1,
    cex.axis    = 0.9,
    upper.panel = panel.triple.cor,
    text.panel  = text.panel.custom
  )

  grDevices::dev.off()

  if (output_type == "png" && isTRUE(open_generated_files))
    f_open_file(png_path_corplot)

  ##########################################################################
  #  Legend logic
  #
  #  draw_group_legend : factors are present AND user wants the legend
  #  draw_legend_file  : either of the above OR ordinal_vars are specified
  #                      (the coefficient explanation block is always included
  #                      in the legend file when ordinal_vars are used)
  ##########################################################################
  draw_group_legend <- factor_count > 0L && isTRUE(print_legend)
  draw_legend_file  <- draw_group_legend || !is.null(ordinal_vars)

  if (draw_legend_file) {

    png(png_path_legend, width = 8, height = 8, units = "in", res = res)

    plot(1L, type = "n", xlab = "", ylab = "",
         xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)

    y_cursor <- par("usr")[4L] - 0.05  # running y-position (top to bottom)

    # ---- Group legend (colour / shape levels) ----------------------------
    if (draw_group_legend) {
      text(-0.05, y_cursor, labels = legend_title,
           cex = 1, pos = 4L, font = 2L)
      y_cursor <- y_cursor - 0.06

      leg_params <- list(
        x          = -0.15,
        y          = y_cursor + 0.05,
        legend     = legend_,
        col        = legend_col,
        pch        = legend_pch,
        pt.bg      = legend_pt.bg,
        bty        = "n",
        cex        = 0.8,
        ncol       = legend_ncol,
        xpd        = TRUE,
        text.width = max(strwidth(legend_)) + 0.02
      )
      # Measure actual rendered height before drawing -- no more fixed guesses
      leg_dims <- do.call(legend, c(leg_params, list(plot = FALSE)))
      # Draw the real legend
      do.call(legend, leg_params)
      # Advance by the actual height plus a small breathing gap
      y_cursor <- y_cursor - leg_dims$rect$h - 0.04
    }

    # ---- Helper: draw one symbol + wrapped description -------------------------
    draw_cor_entry <- function(sym, desc, y_start,
                               sym_x   = -0.05,
                               text_x  =  0.03,
                               line_dy =  0.042,   # tighter line spacing
                               gap_dy  =  0.025,   # tighter gap between entries
                               wrap_w  =  85,
                               cex     =  0.80) {
      wrapped <- strwrap(desc, width = wrap_w)
      text(sym_x,  y_start, sym,        cex = cex, pos = 4L, xpd = TRUE)
      text(text_x, y_start, wrapped[1], cex = cex, pos = 4L, xpd = TRUE)
      if (length(wrapped) > 1L)
        for (i in seq(2L, length(wrapped)))
          text(text_x, y_start - (i - 1L) * line_dy,
               wrapped[i], cex = cex, pos = 4L, xpd = TRUE)
      y_start - length(wrapped) * line_dy - gap_dy
    }

    # ---- Helper: draw a plain wrapped text block --------------------------------
    draw_wrapped_text <- function(txt, y_start,
                                  text_x  = -0.05,
                                  line_dy =  0.042,
                                  gap_dy  =  0.025,
                                  wrap_w  =  92,
                                  cex     =  0.80,
                                  font    =  1L,
                                  col     =  "black") {
      wrapped <- strwrap(txt, width = wrap_w)
      for (i in seq_along(wrapped))
        text(text_x, y_start - (i - 1L) * line_dy,
             wrapped[i], cex = cex, pos = 4L, xpd = TRUE,
             font = font, col = col)
      y_start - length(wrapped) * line_dy - gap_dy
    }

    # ---- Correlation coefficient explanation block ------------------------------

    # Rule-of-thumb: split into two short lines manually so the break falls
    # at the "|" divider rather than mid-phrase
    y_cursor <- draw_wrapped_text(
      txt     = paste0("Rule of thumb:  ",
                       "continuous + normal \u2192 r",
                       "   |   ",
                       "ordinal or skewed \u2192 \u03c1 or \u03c4"),
      y_start = y_cursor,
      wrap_w  = 72,      # narrow enough to break at the | divider naturally
      font    = 3L,      # italic
      cex     = 0.78
    )

    # Section title
    text(-0.05, y_cursor, "Correlation coefficients:",
         cex = 0.85, pos = 4L, font = 2L, xpd = TRUE)
    y_cursor <- y_cursor - 0.055

    cor_entries <- list(
      list(sym  = expression(italic(r)),
           desc = paste0("= Pearson: measures the strength of a LINEAR relationship. ",
                         "Assumes both variables are continuous and approximately normally ",
                         "distributed. Sensitive to outliers.")),
      list(sym  = expression(italic(rho)),
           desc = paste0("= Spearman: measures the strength of a MONOTONIC relationship ",
                         "(one variable tends to increase as the other does, but not necessarily ",
                         "at a constant rate). Use instead of Pearson when data are ordinal, ",
                         "skewed, or contain outliers. Works on ranks, not raw values.")),
      list(sym  = expression(italic(tau)),
           desc = paste0("= Kendall: also rank-based like Spearman, but counts the proportion of CONCORDANT pairs (both variables increase together) minus DISCORDANT pairs (one increases, the other decreases). More robust than Spearman for small samples or many tied ranks (identical values that get the same rank). Preferred for strictly ordinal data.")
           )
    )

    for (entry in cor_entries)
      y_cursor <- draw_cor_entry(sym     = entry$sym,
                                 desc    = entry$desc,
                                 y_start = y_cursor)

    # ---- Ordinal annotation (if applicable) ------------------------------------
    if (!is.null(ordinal_vars)) {
      y_cursor <- y_cursor - 0.015   # small extra breath before this section
      y_cursor <- draw_wrapped_text(
        txt     = paste0("Italic variable names indicate ordinal variables: ",
                         paste(ordinal_vars, collapse = ", ")),
        y_start = y_cursor,
        font    = 3L
      )
      y_cursor <- draw_wrapped_text(
        txt     = paste0("(r) = Pearson is greyed and bracketed for pairs involving ",
                         "ordinal variables, as it is not appropriate for ordinal data. ",
                         "Use \u03c1 or \u03c4 instead."),
        y_start = y_cursor,
        col     = "grey50"
      )
    }

    grDevices::dev.off()

    if (output_type == "png" && isTRUE(open_generated_files))
      f_open_file(png_path_legend)
  }

  ##########################################################################
  # Generate Word / PDF via R Markdown, or emit Rmd code
  ##########################################################################
  legend_chunk <- if (draw_legend_file) {
    sprintf(paste0(
      "\n```{r, results='asis', fig.align = 'center', echo=FALSE}\n",
      "cat(paste0('![](%s)'), '   \\n  \\n')\n",
      "```\n"),
      png_path_legend)
  } else ""

  if (output_type %in% c("word", "pdf")) {

    cat(sprintf(
"---
title: \"Correlation Plots\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-2.5cm}
---
```{r, results='asis', fig.align = 'center', echo=FALSE}
cat(paste0('![](%s)'), '   \\n  \\n')
cat('   \\n    \\n&nbsp;  \\n  \\n')
```
%s",
      png_path_corplot, legend_chunk),
    file = temp_output_file)

  } else if (output_type == "rmd") {

    cat(sprintf(paste0(
      "\n```{r, results='asis', fig.align = 'center', echo=FALSE}\n",
      "cat(paste0('![](%s)'), '    \\n  \\n')\n",
      "cat('    \\n    \\n&nbsp;  \\n \\n')\n",
      "```\n%s"),
      png_path_corplot, legend_chunk))
  }

  if (output_type %in% c("pdf", "word")) {
    message("Saving output in: ", output_path)
    rmarkdown::render(
      temp_output_file,
      output_file       = output_path,
      intermediates_dir = temp_output_dir,
      knit_root_dir     = temp_output_dir,
      quiet             = TRUE,
      output_format     = paste0(output_type, "_document"))

    if (isTRUE(open_generated_files)) f_open_file(output_path)
    invisible(suppressWarnings(file.remove(temp_output_file)))
    invisible(suppressWarnings(file.remove(png_path_corplot)))
    invisible(suppressWarnings(file.remove(png_path_legend)))
  }
}
