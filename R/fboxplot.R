#' Generate a Boxplot Report of a data.frame
#'
#' Generates boxplots for all numeric variables in a given dataset, grouped by factor variables. The function automatically detects numeric and factor variables. It allows two output formats ('pdf', 'Word') and includes an option to add a general explanation about interpreting boxplots.
#' @param x A data.frame, formula, or numeric/integer vector (dispatches to the correct method). When a single numeric or integer vector is supplied, it is treated as a single response variable, plotted on the y-axis with the variable name as label, and grouped by a single dummy factor (one box). When several unnamed numeric vectors are supplied (as in base R's \code{boxplot()}, e.g. \code{f_boxplot(x1, x2)}), each becomes its own box side by side, labelled with the original variable name on the x-axis.
#' @param formula A formula specifying the factor to be plotted. More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to generate multiple boxplots. If the formula is omitted and only \code{data} is provided all data will be used for creating boxplots.
#' @param data A \code{data.frame} containing the data to be used for creating boxplots.
#' @param fancy_names An optional named vector mapping column names in \code{data} to more readable names for display in plots (name map). Defaults to \code{NULL}.
#' @param output_type Character string, specifying the output format: \code{"pdf"}, \code{"word"}, \code{"rmd"} or \code{"png"}. The option \code{"rmd"} saves rmd code in the output object not in a file. Default is \code{"pdf"}.
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_BoxPlot" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_BoxPlot.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param boxplot_explanation A logical value indicating whether to include an explanation of how to interpret boxplots in the report. Defaults to \code{TRUE}.
#' @param detect_factors A logical value indicating whether to automatically detect factor variables in the dataset. Defaults to \code{TRUE}.
#' @param jitter A logical value, if \code{TRUE} all data per boxplot is shown, if \code{FALSE} (default) individual data points (except for outliers) are omitted.
#' @param width Numeric, png figure width default \code{8} inch
#' @param height Numeric, png figure height default \code{7} inch
#' @param units Character string, png figure units default \code{"in"} = inch, other options are: \code{"px"} = Pixels, \code{"cm"} = centimeters, \code{"mm"} = millimeters.
#' @param res Numeric, png figure resolution default 300 dpi
#' @param las An integer (\code{0} t/m \code{3}), \code{las = 0}: Axis labels are parallel to the axis. \code{las = 1}: Axis labels are always horizontal. \code{las = 2}: Axis labels are perpendicular to the axis. (default setting). \code{las = 3}: Axis labels are always vertical.
#' @param outliers Logical. If \code{TRUE}, scans for outliers using Tukey's fences and if they exist, adds them to the report using \code{f_outliers}. Default \code{TRUE}.
#' @param coef Numeric. The multiplier for the Interquartile Range (IQR) used for outlier detection. Default \code{1.5}.
#' @param limit_columns Integer or \code{NULL}. Defines the number of columns shown in the outlier table. Default = \code{7}. \code{NULL} = all columns are shown.
#' @param color Colour scheme for the boxes. One of: \code{"rainbow"} (default; one hue per group), \code{"bw"} (white fill with black lines, outliers and mean marker, suitable for publication), a single R colour name or hex string applied to all boxes (with a transparent fill and a darker outline derived from it), or a vector of colours which is recycled to the number of groups for a custom per-group palette.
#' @param boxwidth Numeric or \code{NULL}. Relative width of each box, passed as \code{boxwex} to \code{boxplot()}. When \code{NULL} (default) the width is computed automatically as \code{num.bars/18}, keeping boxes roughly comparable across plots with different numbers of groups. Supply a numeric value (for example \code{0.5}) to override. The numeric/integer vector method uses \code{0.4} by default to avoid an overly thin single box.
#' @param ... Further arguments forwarded to \code{f_boxplot_worker},
#'   such as \code{fancy_names}, \code{title}, \code{fill}, etc.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Detects numeric and factor variables in the dataset.
#' \item Generates boxplots for each numeric variable grouped by each factor variable.
#' \item Outputs the report in the specified format ('pdf', 'Word' or 'Rmd').
#'}
#'
#'If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
#'
#' If no factor variables are detected, the function stops with an error message since factors are required for creating boxplots.
#'
#' This function will plot all numeric and factor candidates, use the function \code{subset()} to prepare a selection of columns before submitting to \code{f_boxplot()}.
#'
#' Note that there is an optional \code{jitter} option to plot all individual data points over the boxplots.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'
#' \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#'
#' \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#'
#' \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' If Pandoc is not found, this function may not work as intended.
#'
#'
#' @return The return value depends on \code{output_type}:
#' \itemize{
#'   \item \code{"pdf"} and \code{"word"}: Writes a report file to \code{save_as} (or \code{tempdir()} by default) and returns \code{NULL} invisibly. The file can optionally be opened with \code{open_generated_files = TRUE}.
#'   \item \code{"png"}: Writes one PNG file per response x factor combination into the directory given by \code{save_as} and returns \code{NULL} invisibly.
#'   \item \code{"rmd"}: Returns the generated R Markdown content as a single character string (invisibly). No file is written and nothing is printed to the console. The caller can \code{cat()} the string, assign it to a variable, or embed it in a larger report (see Examples).
#' }
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' \donttest{
#' # Example usage:
#' data(iris)
#'
#' new_names = c(
#'   "Sepal.Length" = "Sepal length (cm)" ,
#'   "Sepal.Width" = "Sepal width (cm)",
#'   "Petal.Length" = "Petal length (cm)",
#'   "Petal.Width" = "Petal width (cm)",
#'   "Species" = "Cultivar"
#' )
#'
#' # Use the whole data.frame to generate an MS Word report and don't open it.
#' f_boxplot(iris,
#'            fancy_names = new_names,
#'            output_type = "word"
#'            )
#'
#' # Use a formula to plot several response parameters (response 1 + response 2 etc)
#' # and generate a rmd output without boxplot_explanation.
#' data(mtcars)
#' f_boxplot(hp + disp ~ gear*cyl,
#'            data=mtcars,
#'            boxplot_explanation = FALSE,
#'            output_type = "word"
#'            )
#'
#' # Pass a bare numeric vector. Its name is used as the y-axis label
#' # and as the data_name in the output filename.
#' set.seed(1)
#' my_vec <- rnorm(50, mean = 10)
#' f_boxplot(my_vec, output_type = "png")
#'
#' # Formula with bare vectors (no data.frame): group hp by cyl.
#' hp1  <- mtcars$hp
#' cyl1 <- mtcars$cyl
#' f_boxplot(hp1 ~ cyl1, output_type = "png")
#'
#' # Multiple unnamed numeric vectors, base R's boxplot() convention:
#' # each vector becomes its own box, labelled on the x-axis with its
#' # original variable name. Use the formula syntax above when you
#' # instead want to group one response by a factor.
#' f_boxplot(hp1, cyl1, output_type = "png")
#'
#' # Capture the R Markdown output as a string and render it inline.
#' # Use output_type = "rmd" to get the markdown back as a character value
#' # instead of writing a file. Useful for embedding in a larger knitr document.
#' rmd <- f_boxplot(iris,
#'                  output_type         = "rmd",
#'                  boxplot_explanation = FALSE,
#'                  outliers            = FALSE
#'                  )
#'
#' # Display it in the console
#' cat(rmd)
#'
#' # ...or splice it into a knitr child chunk with results = "asis":
#' #   ```{r, echo=FALSE, results='asis'}
#' #   cat(rmd)
#' #   ```
#'
#'
#' }
#'
#' @export
# Public generic -first argument drives dispatch
f_boxplot <- function(x, ...) {
  UseMethod("f_boxplot")
}

#' @export
#' @rdname f_boxplot
# Dispatch when first argument is a formula
f_boxplot.formula <- function(formula, data = NULL, ...) {
  # Capture a sensible data_name from the original call and pass it
  # explicitly via .data_name. This is set BEFORE we touch do.call()
  # because do.call replaces the original 'data' expression with the
  # data.frame value itself; the worker's fallback path
  # `deparse(substitute(data))` would then deparse the whole data.frame
  # into a multi-line character vector, breaking downstream paste/grepl
  # calls with "the condition has length > 1".
  #
  # Two cases:
  #   data supplied    -> use the original expression for 'data' (typically
  #                       a symbol like 'mtcars') captured via match.call().
  #   data omitted     -> derive from the formula's variables, so
  #                       f_boxplot(hp1 ~ cyl1) yields 'hp1_cyl1'.
  args <- list(...)
  if (is.null(args$.data_name)) {
    if (!is.null(data)) {
      data_expr <- tryCatch(match.call()$data,
                            error = function(e) NULL)
      if (!is.null(data_expr)) {
        # paste with collapse = "" flattens any multi-line deparse (rare,
        # but possible for complex expressions) into a single string.
        args$.data_name <- paste(deparse(data_expr), collapse = "")
      } else {
        args$.data_name <- "data"
      }
    } else {
      vars <- all.vars(formula)
      if (length(vars) > 0L) {
        args$.data_name <- paste(vars, collapse = "_")
      }
    }
  }
  do.call(
    f_boxplot_worker,
    c(list(formula = formula, data = data), args)
  )
}

#' @export
#' @rdname f_boxplot
# Dispatch when first argument is a data.frame
f_boxplot.data.frame <- function(x, ...) {
  data <- x  # alias for readability inside the body
  f_boxplot_worker(formula = NULL, data = data, ...)
}

#' @export
#' @rdname f_boxplot
# Dispatch when first argument is a numeric vector. Handles two shapes:
#   single vector  -> one box, vector name on y-axis
#   N vectors      -> N boxes side by side (base R's boxplot() convention),
#                     vector names on x-axis, shared y-axis labelled "value"
f_boxplot.numeric <- function(x, ...) {

  # Capture x's original symbol name for labelling
  sx <- substitute(x)
  x_name <- if (is.name(sx)) as.character(sx) else "value"

  # Inspect ... to separate "extra response vectors" (unnamed numeric)
  # from "worker options" (named, or non-numeric).
  dots <- list(...)
  dot_names <- if (is.null(names(dots))) {
    rep("", length(dots))
  } else {
    names(dots)
  }

  is_extra_vec <- vapply(seq_along(dots), function(i) {
    dot_names[i] == "" &&
      is.numeric(dots[[i]]) &&
      length(dots[[i]]) > 0L
  }, logical(1))

  if (any(is_extra_vec)) {

    ## ---- Multi-vector path: base R boxplot(x1, x2, ...) convention ----

    extra_idx <- which(is_extra_vec)

    # Recover original symbols for the extra vectors from the unevaluated
    # call. match.call(expand.dots = FALSE) gives us mc$`...` as a
    # pairlist of the original argument expressions; we map back through
    # the same indices we used into 'dots'. Falls back to V2, V3, ... if
    # an entry is a literal call rather than a bare symbol.
    extra_names <- character(length(extra_idx))
    mc <- tryCatch(match.call(expand.dots = FALSE), error = function(e) NULL)
    dot_exprs <- if (!is.null(mc)) as.list(mc$`...`) else list()

    for (k in seq_along(extra_idx)) {
      i <- extra_idx[k]
      e <- if (length(dot_exprs) >= i) dot_exprs[[i]] else NULL
      if (!is.null(e) && is.symbol(e)) {
        extra_names[k] <- as.character(e)
      } else {
        # k + 1 because position 1 is x; first extra is V2
        extra_names[k] <- paste0("V", k + 1L)
      }
    }

    # Assemble all vectors and their labels in plot order
    all_vecs  <- c(list(unname(x)), lapply(dots[extra_idx], unname))
    all_names <- c(x_name, extra_names)

    # Basic validation: at least the first vector must have non-NA data
    if (length(x) == 0L) {
      stop("Cannot create a boxplot from a length-0 vector.")
    }
    if (all(is.na(x))) {
      stop("Cannot create a boxplot: vector contains only NA values.")
    }

    # Long-format data.frame: one row per observation, with the original
    # variable name carried as a factor level on the .group column.
    # The response column is named 'value' (the package's data.frame is
    # synthetic, so there is no collision risk with user columns).
    lengths_vec <- vapply(all_vecs, length, integer(1))
    df <- data.frame(
      value  = unlist(all_vecs, use.names = FALSE),
      .group = factor(
        rep(all_names, times = lengths_vec),
        levels = all_names
      ),
      stringsAsFactors = FALSE
    )

    fml       <- as.formula("value ~ `.group`")
    data_name <- paste(all_names, collapse = "_")

    # Everything else in ... goes to the worker as options
    worker_args <- dots[!is_extra_vec]

    do.call(
      f_boxplot_worker,
      c(list(formula = fml, data = df, .data_name = data_name),
        worker_args)
    )

  } else {

    ## ---- Single-vector path (unchanged) ----

    if (length(x) == 0L) {
      stop("Cannot create a boxplot from a length-0 vector.")
    }
    if (all(is.na(x))) {
      stop("Cannot create a boxplot: vector contains only NA values.")
    }

    # Drop names so the data.frame is clean.
    x <- unname(x)

    # Build a one-column data.frame with the vector under its original name
    # and add a single-level dummy factor so the worker (which currently
    # requires at least one factor) can process it. The empty-string level
    # produces a clean, unlabelled x-axis tick.
    df <- data.frame(x, stringsAsFactors = FALSE)
    names(df)[1] <- x_name
    df[[".group"]] <- factor(rep("", length(x)), levels = "")

    # Construct the formula vec_name ~ .group, backticking both sides so
    # non-syntactic vector names are handled safely.
    fml <- as.formula(
      paste0("`", x_name, "` ~ `.group`")
    )

    # Default boxwidth to 0.4 so a single box looks reasonable, but only
    # when the caller has not supplied boxwidth via ... .
    args <- list(...)
    if (is.null(args$boxwidth)) {
      args$boxwidth <- 0.4
    }

    do.call(
      f_boxplot_worker,
      c(list(formula = fml, data = df, .data_name = x_name), args)
    )
  }
}

#' @export
#' @rdname f_boxplot
# Integer vectors behave the same as numerics for plotting purposes
f_boxplot.integer <- f_boxplot.numeric


# Internal helper. Resolves the user-facing 'color' argument into the three
# palettes (c1, c2, c3) the boxplot code uses internally:
#   c1 = whiskers, points, jitter, outlier marks
#   c2 = box fill (transparent in coloured modes)
#   c3 = median line, box outline, staples, mean marker
#
# Accepted shapes for 'color':
#   "rainbow"           default; one hue per group
#   "bw"                white fill, black lines/outliers/mean
#   single colour name  all boxes the same hue, with a transparent fill and
#                       a darkened outline derived from it
#   colour vector       custom palette, recycled to num.bars; each entry
#                       gets its own transparent fill and darkened outline
#
# Not exported; lives below the public S3 methods so the roxygen doc block
# at the top of the file stays attached to the f_boxplot generic.
resolve_boxplot_colors <- function(color, num.bars) {

  # Pure black or pure white as a single colour would collapse the plot
  # to one tone (lines, whiskers and outliers vanish into the background
  # or the fill). Route both to the dedicated 'bw' mode so the user gets
  # a proper publication-style plot regardless of which extreme they pass.
  if (length(color) == 1L &&
      (identical(color, "black") || identical(color, "white"))) {
    color <- "bw"
  }

  # Default rainbow palette: keeps existing behaviour byte-for-byte
  if (is.null(color) ||
      (length(color) == 1L && identical(color, "rainbow"))) {
    return(list(
      c1 = rainbow(num.bars),
      c2 = rainbow(num.bars, alpha = 0.2),
      c3 = rainbow(num.bars, v = 0.7)
    ))
  }

  # Pure black-and-white mode for publication-style plots
  if (length(color) == 1L && identical(color, "bw")) {
    return(list(
      c1 = rep("black", num.bars),
      c2 = rep("white", num.bars),
      c3 = rep("black", num.bars)
    ))
  }

  # Otherwise treat 'color' as one or more R colours. Validate up front
  # so the user gets a clear message rather than a cryptic col2rgb error.
  tryCatch(
    col2rgb(color),
    error = function(e) {
      stop(
        "Invalid value for 'color': ", conditionMessage(e),
        ". 'color' should be 'rainbow', 'bw', a valid R colour name or ",
        "hex string, or a vector of such colours.",
        call. = FALSE
      )
    }
  )

  # Recycle the user's colour(s) to one entry per box
  cols <- rep_len(color, num.bars)

  # Decompose each colour into HSV once; both the fill and the darker
  # outline are derived from these components.
  rgb_mat <- col2rgb(cols) / 255
  hsv_mat <- rgb2hsv(rgb_mat[1, ], rgb_mat[2, ], rgb_mat[3, ])

  # Light-tint fill derived in HSV space: keep the hue, knock saturation
  # down, pin value high. Alpha-blending pure RGB blue onto a white
  # background drifts perceptually into purple/lavender, which is the
  # wrong hue family. Reducing saturation in HSV stays in the same hue
  # family, so 'blue' produces a blue-tinted fill rather than a purple
  # one. The same recipe also gives sensible pinks for 'red', light
  # greens for 'green', etc.
  fill <- hsv(hsv_mat["h", ],
              hsv_mat["s", ] * 0.3,
              pmax(hsv_mat["v", ], 0.95))

  # Darker outline derived in HSV space, mirroring rainbow(..., v = 0.7)
  darker <- hsv(hsv_mat["h", ],
                hsv_mat["s", ],
                hsv_mat["v", ] * 0.7)

  # Per-entry override: pure black or pure white inside a vector palette
  # gets the same publication-style treatment as the dedicated 'bw' mode
  # (white fill, black lines/outliers/mean). The standard derivation
  # collapses these extremes (transparent black renders as grey fill;
  # transparent white plus a grey outline is nearly invisible on a white
  # background), so we substitute the bw triplet for those positions.
  bw_idx <- cols %in% c("black", "white")
  if (any(bw_idx)) {
    cols[bw_idx]   <- "black"
    fill[bw_idx]   <- "white"
    darker[bw_idx] <- "black"
  }

  list(
    c1 = unname(cols),
    c2 = unname(fill),
    c3 = unname(darker)
  )
}

#' @export
#' @rdname f_boxplot
# Private worker - all the real logic lives here
f_boxplot_worker <- function(formula = NULL, data, fancy_names = NULL,
                             output_type = "pdf", outliers = TRUE,
                             coef = 1.5,
                      limit_columns = 7,
                      save_as = NULL,
                      # Specify the name of the output dir and file (name and type).
                      save_in_wdir = FALSE,
                      # Save file output in the working directory.
                      close_generated_files = FALSE,
                      # Closes either open word files depending on the output format.
                      open_generated_files = interactive(),
                      # Open files after creation
                      boxplot_explanation = TRUE,
                      # This text reminds the user on how to read a boxplot.
                      detect_factors  = TRUE,
                      # Detect factors automatically (TRUE) or not (FALSE)
                      jitter = FALSE,
                      # show individual data points
                      #ouput png settings
                      width = 8,
                      height = 7,
                      units = "in",
                      res = 300,
                      las = 2,
                      # Colour scheme: 'rainbow', 'bw', a single colour, or
                      # a vector of colours (recycled to num.bars).
                      color = "rainbow",
                      # Relative width of each box (boxwex). NULL = auto.
                      boxwidth = NULL,
                      # Internal: explicit data_name override used by
                      # f_boxplot.numeric so the output filename and
                      # headings reflect the original vector name rather
                      # than the synthetic data.frame name.
                      ...

)
{

    # Internal backchannel set by f_boxplot.formula and f_boxplot.numeric.
    # Not part of the public API; deliberately read from `...` so it does
    # not appear in the function signature or the Rd usage section.
    .dots      <- list(...)
    .data_name <- if (".data_name" %in% names(.dots)) .dots[[".data_name"]] else NULL

    ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state


  if( !(output_type %in% c("pdf", "word", "rmd", "png")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'rmd', 'png' ")
  }

  # Generate a temporary file path for "output.Rmd"
  temp_output_dir <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)
  file_extension <- NULL

  ####### Save dataframe name and Handle input from vectors (dataframe column) #####

  # Resolve data_name. Priority: explicit .data_name (supplied by the
  # vector method), then deparse(substitute(data)), then a name derived
  # from the formula when data is NULL.
  if (!is.null(.data_name)) {
    data_name <- .data_name
  } else if (!is.null(data)) {
    data_name <- deparse(substitute(data))
  }

  if (is.null(data)) {

    if (is.null(.data_name)) {
      if (length(formula_extract_df_names(formula)) == 0) {
        data_name <- "data"
      } else if (length(formula_extract_df_names(formula)) == 1) {
        data_name <- formula_extract_df_names(formula)
      } else if (length(formula_extract_df_names(formula)) > 1) {
        data_name <- paste(formula_extract_df_names(formula), collapse = "_")
      }
    }

    # Make a data.frame based on the formula
    data <- formula_to_dataframe(formula)


    # Rewrite formula without data frame prefixes
    formula <- clean_formula(formula)

  }

  #### Handle option "save_as = " ###
  if (output_type != "rmd"){
    if(save_in_wdir == TRUE){
      save_dir <- getwd()
    }else{
      save_dir <- tempdir()
    }


  #map the output type to extensions
  output_type_map <- c(
    "pdf"  = ".pdf",
    "word" = ".docx",
    "excel"= ".xlsx",
    "png"  = ".png"
  )

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R



  # If the user specifies a path, filename or save_in_wdir == TRUE an output_file should be created
  if (!is.null(save_as) || save_in_wdir == TRUE) {

    if (!is.null(save_as)) {
      #Remove backslash in save_as if needed
      save_as <- gsub(pattern = "\\\\", replacement = "/", x = save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if(file_extension_save_as[1] != FALSE){
        file_extension <- file_extension_save_as
      }
    }

    if(is.null(file_extension) && output_type == "word"){
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".docx"
      )

      #set output_type to default
      output_type <- "word"

    }
    else if(is.null(file_extension) && output_type %in% c("pdf", "word", "excel", "rmd", "png")){

      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file.ext
      )



    }
    else if(!is.null(file_extension) && length(file_extension) >= 2) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file_extension[1]
      )

      # Only override output_type if we actually got a valid type back
      if (!is.na(file_extension[2]) && nchar(file_extension[2]) > 0) {
        output_type <- file_extension[2]
      }
    }
  } else {

    #create extension based on input_type
    file.ext <- unname(output_type_map[output_type])

    # use helper get_save_path() to create output_path
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(data_name, "BoxPlot", sep = "_"),
                                 default_dir  = save_dir,
                                 file.ext     = file.ext
    )

  }

  # Do not run explanation code when output is png
  if(output_type == "png"){
  boxplot_explanation <- FALSE
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
}

  # Wrap lines in rmd output document
  f_wrap_lines()

  # Create a counter for the number of factors and set it to zero
  factor_count <- 0



  # Automatically detect factor variables
  if (detect_factors == TRUE) {
    data <- f_factors(data)
  }

  # Rename the data.frame with fancy names if fancy_names are provided.
  if(!is.null(fancy_names)){
    data <- f_rename_columns(data, fancy_names)
  }


  # Count the number of factor variables
  factor_vars  <- vapply(data, is.factor, logical(1))
  factor_count <- sum(factor_vars)
  factor_vars  <- names(factor_vars)[factor_vars]

  if(factor_count == 0){
    stop("The data contains NO factor variable candidates, factors are required to make boxplots")
  }

  # Automatically detect numeric variables
  numeric_vars  <- vapply(data, is.numeric, logical(1)) # Identify numeric columns
  numeric_count <- sum(numeric_vars)
  numeric_vars  <- names(numeric_vars)[numeric_vars]


  if(!is.null(formula)){
    if(!is.null(fancy_names) ){
    formula <-  rename_formula_terms(formula, fancy_names)
    }
    # Extract and backtick-wrap RHS variable names that need it
    rhs_vars <- all.vars(formula[[3]])
    rhs_vars_backt <- ifelse(
      rhs_vars != make.names(rhs_vars),
      paste0("`", rhs_vars, "`"),
      rhs_vars
    )
    # Rebuild RHS string replacing bare names with backtick versions
    RHS <- deparse(formula[[3]])
    for (i in seq_along(rhs_vars)) {
      if (rhs_vars[i] != make.names(rhs_vars[i])) {  # only non-syntactic names
        backticked <- paste0("`", rhs_vars[i], "`")
        if (!grepl(backticked, RHS, fixed = TRUE)) {  # only if NOT already wrapped
          RHS <- gsub(rhs_vars[i], backticked, RHS, fixed = TRUE)
        }
      }
    }
    factor_vars <- all.vars(formula[[3]])  # bare names for data frame indexing

    # Restrict the plotting loop to the responses listed on the LHS of
    # the formula. Without this, the main loop below iterates over every
    # numeric column in 'data', so a call like f_boxplot(hp ~ cyl, mtcars)
    # would generate plots for mpg, disp, hp, drat, wt and qsec instead
    # of just hp.
    #
    # Behaviour preserved:
    #   hp ~ cyl              -> only hp
    #   hp + mpg ~ cyl        -> hp and mpg (in formula order)
    #   ~ cyl   (no LHS)      -> all numeric columns (length(response_vars)==0
    #                            leaves numeric_vars untouched)
    #   data only, no formula -> all numeric columns (this branch never runs)
    #
    # Extraction happens AFTER fancy_names renaming so the names match
    # numeric_vars, which was computed from the (possibly renamed) data.
    response_vars <- all.vars(formula[[2]])
    if (length(response_vars) > 0L) {
      keep <- response_vars[response_vars %in% numeric_vars]
      if (length(keep) == 0L) {
        stop(
          "None of the LHS variables (",
          paste(response_vars, collapse = ", "),
          ") match a numeric column in 'data'.",
          call. = FALSE
        )
      }
      numeric_vars <- keep
    }
  }


generate_report <- function() {
    # This text reminds the user on how to read a boxplot.
    if(boxplot_explanation == TRUE){

      cat("  \n  \n
# Understanding Boxplots: A Visual Guide

A **boxplot** (or box-and-whisker plot) is a statistical tool for visualizing the distribution of a dataset. It offers a quick snapshot of the dataset's central tendency, spread, and potential outliers. Below is a guide to help you read and interpret a boxplot effectively.

## Anatomy of a Boxplot

1. **The Five-Number Summary**:
   - **Minimum**: The smallest value in the dataset (excluding outliers).
   - **First Quartile (Q1)**: The value below which 25% of the data falls.
   - **Median (Q2)**: The middle value of the dataset, splitting it into two halves.
   - **Third Quartile (Q3)**: The value below which 75% of the data lies.
   - **Maximum**: The largest value in the dataset (excluding outliers).


 A note on quartiles: Q1, Q2, and Q3 are cut points, not the four parts of the data themselves. Like slicing a cake into four pieces with three cuts, three quartiles divide the data into four equal quarters.

&nbsp;  \n   \n

2. **The Box**:
   - The box represents the **interquartile range (IQR)**, spanning from Q1 to Q3, which contains the middle 50% of the data.
   - A vertical line inside the box marks the **median**, indicating the central value of the dataset.

&nbsp;  \n   \n

3. **The Whiskers**:
   - Whiskers extend from the box to the smallest and largest data points that still lie within **1.5 times the IQR** of the quartiles; that is, above Q1 - 1.5 x IQR (lower) and below Q3 + 1.5 x IQR (upper). These limits are called the fences.
   - Values outside this range are considered **outliers**.

&nbsp;  \n   \n

4. **Outliers**:
   - These are individual points plotted beyond the whiskers. They highlight extreme values that may warrant further investigation.

&nbsp;  \n   \n

5. **Mean**:
   - Some boxplots include the **mean** (average) as a distinct marker, often represented by a cross or dot.

&nbsp;  \n   \n")

invisible(image_path <- system.file('extdata', 'boxplot_explained_rfriend.png',
                                    package = 'rfriend'))
# Include the saved plots in R Markdown
cat(paste0("![](", image_path, ")"), "   \n  \n")

cat("
## How to Interpret a Boxplot

- **Center**: The **median** represents the central tendency of the data and serves as the most reliable summary measure, especially in the presence of outliers or skewed distributions.
- **Spread**:
  - The **IQR** (length of the box) captures the variability of the middle 50% of the data.
  - The whiskers show the overall spread of values within the dataset.
- **Skewness**:
  - If the median is closer to one end of the box or if one whisker is longer, the dataset is skewed in that direction. A symmetric distribution is balanced around the median, with the median and mean close to each other.
- **Outliers**: Points outside the whiskers indicate unusual or extreme values that could influence the dataset's overall analysis.


## Why and When to Use Boxplots?

Boxplots are particularly valuable when comparing distributions across multiple groups or datasets. They allow you to quickly assess differences in central tendency, variability, and the presence of outliers, making them a powerful tool for exploratory data analysis. If data is not normally distributed and the mean is not a good summary measure, scientists resort to boxplots instead of barplot in their publications.")

    if(output_type != "rmd"){
      # Pagebreak
      cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
    }
    }


  #Main loop starts here
  for (response_name in numeric_vars) {

    if (response_name != make.names(response_name)) {
      response_name_backt <- paste0("`", response_name, "`")
    } else {
      response_name_backt <- response_name
    }

    if(output_type != "png"){
      cat("  \n  \n#  Boxplots of: ", response_name, "  \n")
    }

    if(!is.null(formula)){
        if(output_type != "png"){
        cat("##  Boxplot of: ", response_name, " as function of ", RHS, "  \n")
        }

        current_formula <- as.formula(paste(response_name_backt, "~", RHS))

        level_count <- NULL

        for(i in factor_vars){
          level_count[i] <- length(levels(data[[i]]))
        }

        factor_cols <- names(level_count)

        # Get the number of bars per boxplot and store in num.bars
        num.bars <- prod(level_count, na.rm = FALSE)

        # Resolve colours via the helper so 'color' (rainbow/bw/single/vector)
        # is honoured consistently across all boxplot elements.
        .pal <- resolve_boxplot_colors(color, num.bars)
        c1 <- .pal$c1
        c2 <- .pal$c2
        c3 <- .pal$c3

        # Determine width of the boxes. When boxwidth is supplied, use it
        # as-is. Otherwise scale by num.bars/18 so boxes stay roughly
        # comparable across plots with different numbers of groups
        # (max bars = 9 -> 0.5).
        if (is.null(boxwidth)) {
          box.width <- num.bars / 18
        } else {
          box.width <- boxwidth
        }


        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = width, height = height, units = units, res = res)

        # Set margins where c(1=bottom, 2=left, 3=top and 4=right).
        # par(mar = c(5,7,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
        par(mar = c(8,5,4,2))
        par(cex.lab=0.9)  # is text size for y-axis
        par(cex.axis=0.9) # is text size for x-axis

        #Draw Boxplot (note gsub("(^')|('$)", "", resp$name[[i]]) removes the leading and tailing single quotes)
        boxp <- boxplot(current_formula,
                        data = data,
                        xlab = "",
                        ylab = response_name,
                        las  = las,
                        #Create nice colors
                        col=c2, medcol=c3, whiskcol=c1, staplecol=c3, boxcol=c3, outcol=c3,
                        #Set box width using relative number of boxes to keep them more or less equal
                        boxwex = box.width
                        #Let the width of the boxes depend on relative number of replicas: varwidth = TRUE
                        # varwidth = TRUE,

        )
        # Add jittered points
        if(jitter == TRUE){
          stripchart(current_formula,
                     data = data,
                     method = "jitter", # Adds random noise
                     pch = 19,          # Point style
                     vertical = TRUE,   # Align points vertically or not
                     cex = 0.7,         # Reduce point size
                     col = c1,          # Point color
                     add = TRUE)        # Overlay on the boxplot
        }

        # Calculate means for each group
        means <- tapply(data[[response_name]],
                        interaction(data[factor_cols]),
                        function(x) mean(x, na.rm = TRUE)
                        )

        # Add mean points to the plot
        points(seq_along(means), means, col = c3, pch = 10, cex = 1.5)


        # Allow plotting outside the plot region
        par(xpd = TRUE)

        # Get current plot limits
        usr <- par("usr")  # usr contains c(xmin, xmax, ymin, ymax)

        # Calculate a proportional position above the top of the plot
        y_position <- usr[4] + 0.05 * (usr[4] - usr[3])  # 5% above ymax

        #Get n
        x.pos.n <- rep(seq_along(boxp$n))
        text(x= x.pos.n, y = y_position, labels= paste0("n=", boxp$n))
        if(output_type != "png"){
        cat("   \n")
        }
        # Close the png file
        dev.off()

        if (output_type != "png"){
          # Include the saved plots in R Markdown
          cat(paste0("![](", temp_file, ")"), "   \n  \n")

          if (outliers == TRUE){
            suppressMessages(
              out <- f_outliers(
                data,
                columns    = response_name,
                group_vars = factor_cols,
                coef       = coef
              )
            )

            # f_outliers() returns a list of class "f_outliers".
            # For a single response column the data.frame lives in out$output_df.
            out_df <- NULL
            out_df <- extract_outlier_df(out)

            if (!is.null(out_df) && nrow(out_df) > 0 && ncol(out_df) > 0) {
              cat("##  Outlier Table of: ",
                  response_name,
                  " as function of ",
                  RHS,
                  "  \n")
              cat(f_pander(out_df, limit_columns = limit_columns))
            } else {
              cat("*No outliers found in:* ",
                  response_name,
                  " as function of ",
                  RHS,
                  "  \n  \n")
            }
          }
        }

        if (output_type == "png"){

          # Define the new file name
          new_file_name <- paste0(dirname(output_path),"/",response_name,".png")

          # Rename the temporary file
          file.rename(temp_file, new_file_name)

          if (open_generated_files == TRUE){
            f_open_file(new_file_name)
          }
        }

    }
    else {
      for (factor_name in factor_vars) {

        if (factor_name != make.names(factor_name)) {
          factor_name_backt  <- paste0("`", factor_name, "`")

        } else {
          factor_name_backt   <- factor_name
          }

      # Create a new formula for each response
      current_formula <- as.formula(paste0(response_name_backt, "~", factor_name_backt))



      if(output_type != "png"){
      cat("##  Boxplot of: ", response_name, " as function of ", factor_name, "  \n")
      }
      # Get the number of bars per boxplot and store in num.bars
      num.bars <- length(levels(data[[factor_name]]))

      # Resolve colours via the helper so 'color' (rainbow/bw/single/vector)
      # is honoured consistently across all boxplot elements.
      .pal <- resolve_boxplot_colors(color, num.bars)
      c1 <- .pal$c1
      c2 <- .pal$c2
      c3 <- .pal$c3

      # Determine width of the boxes. When boxwidth is supplied, use it
      # as-is. Otherwise scale by num.bars/18 so boxes stay roughly
      # comparable across plots with different numbers of groups
      # (max bars = 9 -> 0.5).
      if (is.null(boxwidth)) {
        box.width <- num.bars / 18
      } else {
        box.width <- boxwidth
      }



      temp_file <- tempfile(fileext = ".png")
      png(
        temp_file,
        width = width,
        height = height,
        units = units,
        res = res
      )

      # Set margins where c(1=bottom, 2=left, 3=top and 4=right).
      # par(mar = c(5,7,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      par(mar = c(8,5,4,2))
      par(cex.lab=0.9) # is text size for y-axis
      par(cex.axis=0.9) # is text size for x-axis

      # Draw Boxplot (note gsub("(^')|('$)", "",
      # resp$name[[i]]) removes the leading and tailing single quotes)
      boxp <- boxplot(
        current_formula,
        data = data,
        xlab = "",
        ylab = response_name,
        las  = las,
        #Create nice colors
        col = c2,
        medcol = c3,
        whiskcol = c1,
        staplecol = c3,
        boxcol = c3,
        outcol = c3,
        #Set box width using relative number of boxes to keep them more or less equal
        boxwex = box.width
        #Let the width of the boxes depend on relative number of replicas: varwidth = TRUE
        # varwidth = TRUE,
      )
      # Add jittered points
      if (jitter == TRUE) {
        stripchart(
          current_formula,
          data = data,
          method = "jitter",
          # Adds random noise
          pch = 19,
          # Point style
          vertical = TRUE,
          # Align points vertically or not
          cex = 0.7,
          # Reduce point size
          col = c1,
          # Point color
          add = TRUE
        )        # Overlay on the boxplot
      }


      means <- tapply(data[[response_name]],
                      data[[factor_name]],
                      function(x) mean(x, na.rm = TRUE)
                      )

      # Add mean points to the plot
      points(seq_along(means), means, col = c3, pch = 10, cex = 1.5)


      # Allow plotting outside the plot region
      par(xpd = TRUE)

      # Get current plot limits
      usr <- par("usr")  # usr contains c(xmin, xmax, ymin, ymax)

      # Calculate a proportional position above the top of the plot
      y_position <- usr[4] + 0.05 * (usr[4] - usr[3])  # 5% above ymax

      #Get n
      x.pos.n <- rep(seq_along(boxp$n))
      text(x= x.pos.n, y = y_position, labels= paste0("n=", boxp$n))
      if(output_type != "png"){
        cat("   \n")
      }

      # Close the png file
      dev.off()

      if (output_type != "png"){
        # Include the saved plots in R Markdown
        cat(paste0("![](", temp_file, ")"), "   \n  \n")


        if (outliers == TRUE) {
          suppressMessages(
            out <- f_outliers(
              data,
              columns    = response_name,
              group_vars = factor_name,
              coef       = coef
            )
          )

          # f_outliers() returns a list of class "f_outliers".
          # For a single column the data.frame lives in out$output_df.
          out_df <- NULL
          out_df <- extract_outlier_df(out)

          if (!is.null(out_df) && nrow(out_df) > 0 && ncol(out_df) > 0) {
            cat("##  Outlier Table of: ",
                response_name,
                " as function of ",
                factor_name,
                "  \n")
            cat(f_pander(out_df, limit_columns = limit_columns))
          } else {
            cat("*No outliers found in:* ",
                response_name,
                " as function of ",
                factor_name,
                "  \n  \n")
          }
        }


      }

      if (output_type == "png"){
      # Define the new file name
      new_file_name <- paste0(dirname(output_path),"/",response_name,"_", factor_name, ".png")

      # Rename the temporary file
      file.rename(temp_file, new_file_name)

        if (open_generated_files == TRUE){
        f_open_file(new_file_name)
        }
      }

      } # End loop Factor variables
    } # End if statement
  } # End Loop Response variables
} # End generate_report function



# Here the documents are constructed.
if (output_type %in% c("word", "pdf")) {


  # Create a temporary R Markdown file
  word_pdf_preamble <- function(){ paste0( # Create a temporary R Markdown file
"
---
title: \"Boxplot Report\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
---
")}

  # Prevent ## before printed output
  knitr::opts_chunk$set(comment = "")

  # show the location were the file is saved
  message(paste0("Saving output in: ", output_path))

  # re-run generate_report, but this time capture its output to a string
  generated_markdown <- capture.output(generate_report())

  # Combine the preamble, assumptions, and the captured report into one string
  rmd_content <- paste(
    word_pdf_preamble(),
    # The captured output already contains the necessary markdown formatting and image links
    paste(generated_markdown, collapse = "\n"),
    sep = "\n"
  )

  # Write the complete Rmd content to the temp file
  writeLines(rmd_content, temp_output_file)

  # Create the RMarkdown file
  rmarkdown::render(
    temp_output_file,
    output_file = output_path,
    intermediates_dir = temp_output_dir,
    knit_root_dir = temp_output_dir,
    quiet = TRUE,
    output_format = paste0(output_type, "_document")
  )

  if(open_generated_files == TRUE){
  # Open the file with default program
    f_open_file(output_path)
  }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))
  return(invisible(NULL))
  }
  else if (output_type == "rmd"){

    if (is.null(knitr::opts_knit$get("output.dir"))) {
      knitr::opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report())

    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    # Return the markdown as a character string (consistent with f_aov,
    # f_glm, f_summary, f_kruskal_test, etc., which all return their
    # rmd content as part of the output_list). Wrapping in invisible()
    # keeps the console quiet when the user calls f_boxplot(...) directly:
    # to display it they do `cat(result)` or `knitr::asis_output(result)`.
    return(invisible(clean_rmd_output))

  }
  else if (output_type == "png"){

    # Capture generate_report()'s cat() output so it does not leak to the
    # console. generate_report() does its real work (saving PNG files) as
    # a side effect, so we can discard the captured markdown string.
    invisible(capture.output(generate_report()))

    message(paste0("PNG files saved in: ", dirname(output_path), "\n   \n"))

    return(invisible(NULL))
  }
}
