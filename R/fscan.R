#' Perform a visual check on your data
#'
#' @description
#' Creates a 3-panel diagnostic dashboard to check data distribution and assumptions. It can also output a data summary table and identify outliers.
#'
#'
#' @param x A data.frame or formula (dispatches to the right method).
#' @param formula A formula specifying the columns (right hand side) to be summarized by maximal 3 groups (left hand side). More columns or groups can be added using \code{-} or \code{+} (e.g., \code{col1 + col2 ~ group1 + group2}) to do a sequential summary for each column parameter.
#' @param data A 'data.frame', 'data.table', or 'tibble'.
#' @param columns The numerical column(s) to summarize if no formula is used. Can be entered as a single character string (e.g., \code{"weight"}) or as a character vector \code{c("weight", "length"}).
#' @param group_vars Character vector of up to 3 grouping variables (e.g., \code{c("species", "fertilizer")}).
#' @param summary Logical. Show a summary table of the data. Default is \code{TRUE}.
#' @param outliers Logical. If \code{TRUE}, scans for outliers using Tukey's fences and if they exist, adds them to the result object. Default \code{TRUE}.
#' @param coef Numeric. The multiplier for the Interquartile Range (IQR) used for outlier detection. Default \code{1.5}.
#' @param limit_columns Integer or \code{NULL}. Defines the number of columns shown in the outlier table. Default = \code{7}. \code{NULL} = all columns are shown.
#' @param fancy_names Named character vector or \code{NULL}. Optional mapping of column names to more readable names for display in plots and legends.
#' @param advice Logical. If \code{TRUE}, runs \code{f_stat_wizard()} on each response
#'   column and appends the recommendation to the result. The advice is accessible
#'   via \code{result[["column_name"]]$advice} and is printed automatically.
#'   Default \code{FALSE}.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param output_type Character string specifying the output format. Default is \code{"default"}.
#'   \itemize{
#'     \item \code{"default"}: Returns the object and lets R decide whether
#'       to print; auto-prints if unassigned, silent if assigned to a variable.
#'       Use \code{print(result)} or \code{plot(result)} to display the
#'       returned object.
#'     \item \code{"console"}: Forces immediate printing to the console
#'       regardless of object assignment.
#'     \item \code{"pdf"}, \code{"word"}, \code{"excel"}: Saves results to a
#'       file of the corresponding format. See \code{save_as},
#'       \code{save_in_wdir}, and \code{open_generated_files} for file
#'       path and opening behavior.
#'     \item \code{"rmd"}: Stores the raw markdown string inside the returned
#'       object for use in R Markdown documents.
#'   }
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_fscan" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_fscan.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param digits Integer. Decimal places for printed tables in 'pdf' and 'Word' output files. Default \code{3}.
#' @param ... Further arguments forwarded to \code{f_scan.data.frame}.
#'
#' @return A list of class \code{f_scan} containing plots, the summary table, and the outlier table. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print, summary and plot methods for 'f_scan' objects.
#'
#' @details
#' \code{f_scan} automatically adapts the visualization based on the number of grouping variables provided:
#' \itemize{
#'   \item \strong{0 groups:} Univariate analysis (Single density/boxplot).
#'   \item \strong{1 group :} Main grouping variable (X-axis and Color).
#'   \item \strong{2 groups:} Adds Facet Wrapping.
#'   \item \strong{3 groups:} Adds Facet Grid (Row vs Column).
#' }
#'
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder.
#' \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @examples
#' # 1. Non-formula | No groups | Default output (default)
#' result <- f_scan(iris, columns = "Sepal.Length")
#' print(result)
#'
#' # 2. Non-formula | 1 group | Console output
#' result <- f_scan(
#'   mtcars,
#'   columns   = "mpg",
#'   group_vars = "cyl",
#'   output_type = "console"
#' )
#'
#' \donttest{
#' # 3. Non-formula | 2 groups | Multiple columns | Excel output
#'  result <- f_scan(
#'    mtcars,
#'    columns    = c("mpg", "hp"),
#'    group_vars = c("cyl", "am"),
#'    outliers   = TRUE,
#'    coef       = 1.5,
#'    output_type = "excel",
#'    save_as    = "mtcars_scan"
#'  )
#'
#' # 4. Formula | 1 group | Strict outlier detection | Word output
#' result <- f_scan(
#'   Sepal.Width ~ Species,
#'   data        = iris,
#'   outliers    = TRUE,
#'   coef        = 3.0,
#'   output_type = "word",
#'   save_as     = "iris_scan"
#'  )
#'
#' # 5. Formula | 2 groups | Multiple columns | Fancy names
#' result <- f_scan(
#'  mpg + hp + wt ~ vs + am,
#'  data        = mtcars,
#'  fancy_names = c(mpg = "Fuel Efficiency", hp = "Horsepower",
#'                  wt  = "Weight",          vs = "Engine Type",
#'                  am  = "Transmission"),
#'  summary     = TRUE
#' )
#' print(result)
#'
#'
#' #Create a small reproducible dataset with 3 grouping variables
#' set.seed(42)
#' plant_data <- data.frame(
#'   weight    = c(rnorm(60, 10, 2), rnorm(60, 14, 2)),
#'   species   = rep(c("A", "B"), each = 60),
#'   treatment = rep(rep(c("control", "treated"), each = 30), 2),
#'   batch     = factor(rep(c("1", "2", "3"), 40))
#'  )
#'
#' # 6. Formula | 3 groups | Facet Grid
#' result <- f_scan(
#'   weight ~ species + treatment + batch,
#'   data         = plant_data,
#'   coef         = 2.0,
#'   digits       = 2,
#'   output_type  = "word"
#' )
#' print(result)
#'
#' # 7. With statistical advice
#' result <- f_scan(
#'   Sepal.Length ~ Species,
#'   data    = iris,
#'   advice  = TRUE
#' )
#' #' print(result)
#' result[["Sepal.Length"]]$advice$y_type
#' }
#'
#' @export
f_scan <- function(x, ...) {
  # Use match.call() to check for 'data' WITHOUT evaluating the arguments.
  # This prevents the "object x not found" error.
  mc <- match.call()

  if (missing(x)) {
    # Check if 'data' was provided in the call (e.g. f_summary(data = mtcars))
    if (!is.null(mc$data)) {
      # Manually retrieve the data object
      x_val <- eval(mc$data, envir = parent.frame())
      # Dispatch manually to the data.frame method
      return(f_scan.data.frame(x_val, ...))
    } else {
      stop("Argument 'x' (or 'data') is missing.")
    }
  }

  # Standard S3 Dispatch
  UseMethod("f_scan")
}

#' @export
#' @rdname f_scan
f_scan.formula <- function(formula, data, ...) {
  # x is the formula (e.g., y ~ B0 + B1)
  x <- formula

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(x) #use helper_check_lhs.R

  # Parse LHS (Response Variable)
  lhs_vars <- all.vars(x[[2]])


  # Parse RHS (Grouping Variables)
  rhs_vars  <- all.vars(x[[3]])
  if (length(rhs_vars) == 0)
    rhs_vars <- NULL

  # Capture the name of the data object passed to the formula method
  data_name_str <- deparse(substitute(data))
  column_name_str <- deparse(substitute(lhs_vars))

  if (length(data_name_str) > 1)
    data_name_str <- "data"

  # Pass it to the data.frame method
  f_scan.data.frame(
    x = data,
    columns = lhs_vars,
    group_vars = rhs_vars,
    internal_data_name = data_name_str,
    internal_column_name = column_name_str,
    ...
  )
}
#'
#' @export
#' @rdname f_scan
f_scan.data.frame <- function(x,
                              columns,
                              group_vars = NULL,
                              summary = TRUE,
                              outliers = TRUE,
                              coef = 1.5,
                              limit_columns = 7,
                              fancy_names = NULL,
                              advice = FALSE,
                              close_generated_files = FALSE,
                              open_generated_files = interactive(),
                              output_type = "default",
                              save_as = NULL,
                              save_in_wdir = FALSE,
                              digits = NULL,
                              ...) {
  # Map 'x' (S3 standard) back to 'data' (Internal logic)
  data <- x

  # Input Validation & Setup
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Parameter validation
  if (!(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "default"))) {
    stop(
      "Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'default'"
    )
  }

  # Safe filename logic
  # Default safety fallback
  data_name <- "data"

  # Check for the hidden internal name (passed from formula method)
  dots <- list(...)
  if ("internal_data_name" %in% names(dots)) {
    data_name <- dots[["internal_data_name"]]
  } else {
    # If no internal name, try to grab it from 'x' using:
    try_name <- try(deparse(substitute(x)), silent = TRUE)
    if (!inherits(try_name, "try-error") &&
        length(try_name) == 1 && nchar(try_name) < 50) {
      data_name <- try_name
    }
  }
  if ("internal_column_name" %in% names(dots)) {
    target_col_name <- dots[["internal_column_name"]]
  } else {
    target_col_name <- deparse(substitute(column))
  }

  # Add a safety check. If x was a complex expression (like filtered data),
  # deparse might still return multiple lines. Force it to length 1.
  if (length(data_name) > 1) {
    data_name <- "data"
  }


  if (target_col_name %in% names(data)) {
    target_cols <- target_col_name
  } else {
    target_cols <- columns
  }


  # Ensure response and predictors are in the data
  for (target_col in target_cols) {

    if (!(target_col %in% names(data))) {
      stop(paste("Column '", target_col_var, "' not found in the data."))
    }
    # Ensure the response variable is numeric
    target_col_var <- data[[target_col]]

    if (!is.numeric(target_col_var)) {
      stop(paste0("Column '", target_col_var, "' must be numeric."))
    }
  }

  for (group_var in group_vars) {
    if (!(group_var %in% names(data))) {
      stop(paste("Grouping variable", group_var, "not found in the data."))
    }
  }

  # Rename the data.frame with fancy names if fancy_names are provided.
  if (!is.null(fancy_names)) {
    data        <- f_rename_columns(data, fancy_names)
    target_cols <- f_rename_vector(target_cols, fancy_names)
    group_vars  <- f_rename_vector(group_vars, fancy_names)
  }

  # Handle Grouping Variables
  # Define roles based on how many groups are provided
  main_cat <- NULL # The X-axis / Color variable
  facet_1  <- NULL # The Wrap variable
  facet_2  <- NULL # The Grid variable (Row)

  # Check existence of groups
  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(data))) {
      missing <- group_vars[!group_vars %in% names(data)]
      stop(paste("Group columns not found:", paste(missing, collapse = ", ")))
    }

    # Convert groups to factors
    for (col in group_vars) {
      if (!is.factor(data[[col]])) {
        data[[col]] <- as.factor(data[[col]])
      }
    }

    # Assign roles dynamically
    n_groups <- length(group_vars)
    main_cat <- group_vars[1]
    if (n_groups > 1)
      facet_1 <- group_vars[2]
    if (n_groups > 2)
      facet_2 <- group_vars[3]
    if (n_groups > 3)
      warning("Only the first 3 grouping variables are used for plotting.")
  }

  #### Handle option "save_as = " ###
  if (save_in_wdir == TRUE) {
    save_dir <- getwd()
  } else{
    save_dir <- tempdir()
  }

  ### Prepare doc output
  # Generate a temporary file path for "output.Rmd"
  temp_output_dir <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")
  output_list <- list()

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)
  # Create a file_extension switch
  file_extension <- NULL

  # Wrap lines in rmd output document
  f_wrap_lines()

  #map the output type to extensions
  output_type_map <- c(
    "pdf"  = ".pdf",
    "word" = ".docx",
    "excel" = ".xlsx",
    "rmd"  = ".rmd"
  )

  # If the user specifies a path, filename or save_in_wdir == TRUE an output file should be created
  if (!is.null(save_as) || save_in_wdir == TRUE) {
    if (!is.null(save_as)) {
      #Remove backslash in save_as if needed
      save_as <- gsub(pattern = "\\\\",
                      replacement = "/",
                      x = save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if (file_extension_save_as[1] != FALSE) {
        file_extension <- file_extension_save_as
      }
    }

    if (is.null(file_extension) &&
        output_type %in% c("console", "default")) {
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "fscan_output", sep = "_"),
        default_dir = save_dir,
        file.ext = ".pdf"
      )
      #set output_type to default
      output_type <- "pdf"

    }
    else if (is.null(file_extension) &&
             output_type %in% c("pdf", "word", "excel", "rmd")) {
      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "fscan_output", sep = "_"),
        default_dir = save_dir,
        file.ext = file.ext
      )


    }
    else if (!is.null(file_extension)) {
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "fscan_output", sep = "_"),
        default_dir = save_dir,
        file.ext = file_extension[1]
      )
      # reset the output type to match the user input extention in save_as
      output_type <- file_extension[2]
    }
  } else {
    #create extension based on input_type
    file.ext <- unname(output_type_map[output_type])

    # use helper get_save_path() to create output_path
    output_path <- get_save_path(
      save_as = save_as,
      default_name = paste(data_name, "fscan_output", sep = "_"),
      default_dir = save_dir,
      file.ext = file.ext
    )
  }


  # Prevent output to console and keep files open when output is "rmd" format
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
    if (output_type == "excel") close_app("EXCEL.EXE",   "Microsoft Excel", "soffice")
  }

  # This is the main function that generates the content for all the output types
  # render = TRUE: also produces ggsave PNGs + markdown (needed for word/pdf/rmd)
  # render = FALSE: only builds the R objects (plots, tables) -- much faster
  generate_report <- function(render = FALSE) {
    for (target_col in target_cols) {
      # Prepare Plot Data
      # If no groups provided, create a dummy "All Data" column for plotting consistency
      plot_data <- data
      if (is.null(main_cat)) {
        main_cat <- "All Data"
        plot_data[[main_cat]] <- factor("All Data")
      }

      # Faceting Logic
      my_facet <- NULL
      if (!is.null(facet_1) && is.null(facet_2)) {
        # 2 Groups: Facet Wrap
        my_facet <- facet_wrap(as.formula(paste("~", facet_1)), labeller = label_both)
      } else if (!is.null(facet_1) && !is.null(facet_2)) {
        # 3 Groups: Facet Grid
        my_facet <- facet_grid(as.formula(paste(facet_1, "~", facet_2)), labeller = label_both)
      }

      # Common Theme
      my_theme <- theme_bw(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(
            face = "bold",
            size = 10,
            hjust = 0
          ),
          axis.title = element_text(size = 9),
          plot.margin = margin(5, 5, 5, 5),
          strip.background = element_rect(fill = "grey95")
        )

      # Helper: Legend Extractor
      get_legend <- function(myggplot) {
        tmp <- ggplot_gtable(ggplot_build(myggplot))
        leg <- which(sapply(tmp$grobs, function(x)
          x$name) == "guide-box")
        if (length(leg) > 0)
          legend <- tmp$grobs[[leg]]
        else
          legend <- NULL
        return(legend)
      }

      # Define the trendline layer conditionally -- NULL skips it in ggplot2
      trendline_layer <- if (main_cat != "All Data") {
        stat_summary(
          fun      = mean,
          geom     = "line",
          aes(group = 1),
          color    = "darkblue",
          linewidth = 0.8,
          linetype = "dashed",
          alpha    = 0.8,
          na.rm    = TRUE
        )
      } else {
        NULL  # ggplot2 silently ignores NULL layers
      }

      # Combined Boxplot + Means
      p1_base <- ggplot(plot_data, aes(
        x = .data[[main_cat]],
        y = .data[[target_col]],
        fill = .data[[main_cat]]
      )) +
        geom_boxplot(
          alpha = 0.4,
          outlier.shape = NA,
          width = 0.5,
          na.rm = TRUE
        ) +
        geom_jitter(
          width = 0.1,
          size = 1.2,
          alpha = 0.3,
          color = "grey30",
          na.rm = TRUE
        ) +
        stat_summary(
          fun.data = mean_se,
          geom = "errorbar",
          width = 0.1,
          color = "darkblue",
          linewidth = 0.8,
          alpha = 0.8,
          na.rm = TRUE
        ) +
        trendline_layer +
        stat_summary(
          fun = mean,
          geom = "point",
          size = 3,
          color = "darkblue",
          shape = 18,
          na.rm = TRUE
        ) +
        labs(title = "A. Overview: Data Spread, Mean (SE) & Trendline", y = target_col, x = "") +
        my_theme +
        theme(legend.position = "top", legend.title = element_blank()) +
        #use this to get the legend in one row
        guides(fill = guide_legend(nrow = 1)) +
        my_facet

      # Extract the legend
      shared_legend <- get_legend(p1_base)

      # Remove legend from p1 for the final plot
      p1_combined <- p1_base + theme(legend.position = "none")

      # Histogram + Density
      p2_hist <- ggplot(plot_data, aes(x = .data[[target_col]], fill = .data[[main_cat]])) +
        geom_histogram(
          aes(y = after_stat(density)),
          color = "white",
          alpha = 0.4,
          bins = 20,
          position = "identity",
          na.rm = TRUE
        ) +
        geom_density(alpha = 0.2,
                     linewidth = 0.8,
                     na.rm = TRUE) +
        labs(title = "B. Distribution Shape", x = target_col, y = "Density") +
        my_theme +
        theme(legend.position = "none") +
        my_facet

      # QQ-Plot
      p3_qq <- ggplot(plot_data, aes(sample = .data[[target_col]], color = .data[[main_cat]])) +
        stat_qq(size = 1.5,
                alpha = 0.6,
                na.rm = TRUE) +
        stat_qq_line(linetype = "dashed",
                     linewidth = 0.8,
                     na.rm = TRUE) +
        labs(title = "C. Normality Check (QQ)", x = "Theoretical", y = "Sample") +
        my_theme +
        theme(legend.position = "none") +
        my_facet

      # Layout Strategy
      bottom_row <- gridExtra::arrangeGrob(p2_hist, p3_qq, ncol = 2)

      # Construct Title
      title_str <- paste("Data scan:", target_col)
      if (!is.null(group_vars))
        title_str <- paste(title_str, "by", paste(group_vars, collapse = " | "))

      # Combine
      if (is.null(shared_legend)) {
        final_plot <- gridExtra::arrangeGrob(
          p1_combined,
          bottom_row,
          ncol = 1,
          heights = c(10, 8),
          top = title_str
        )
      } else {
        final_plot <- gridExtra::arrangeGrob(
          shared_legend,
          p1_combined,
          bottom_row,
          ncol = 1,
          heights = c(1, 10, 8),
          top = title_str
        )
      }

      output_list[[target_col]][["boxplot"]]   <- p1_base
      output_list[[target_col]][["histogram"]] <- p2_hist
      output_list[[target_col]][["qqplot"]]    <- p3_qq
      output_list[[target_col]][["main_plot"]] <- final_plot


      if (summary == TRUE) {
        sum_table <- f_summary(
          data = data,
          columns = target_col,
          group_vars = group_vars,
          show_name = FALSE,
          digits = NULL
        )$output_df
        output_list[[target_col]][["f_summary"]] <- sum_table
      }

      if (outliers == TRUE) {
        # Outlier Table Option
        # Suppress message "No outliers found" inside the scan to keep console clean
        suppressMessages({
          out_table <- f_outliers(
            data = data,
            column = target_col,
            group_vars = group_vars,
            coef = coef
          )$output_df
        })
        # Safely add Outlier Table
        # Check if 'out_table' is not NULL
        if (!is.null(out_table)) {
          output_list[[target_col]][["f_outliers"]] <- out_table
        } else {
          # If NULL (no outliers), add a placeholder sheet so the user knows
          output_list[[target_col]][["f_outliers"]] <- data.frame(Message = "No outliers detected")
        }
       }


      # --- Markdown rendering (ggsave + cat) only when needed for word/pdf/rmd ---
      if (render) {

      cat("
# Diagnostic dashboard of:", target_col , "from", data_name, "  \n")

      temp_file_final_plot <- tempfile(fileext = ".png")
      ggsave(
        temp_file_final_plot,
        plot = final_plot,
        width = 7.8,
        height = 7.8,
        units = "in",
        dpi = 300
      )

      cat(paste0("![](", temp_file_final_plot, ")"), "   \n  \n")
      cat("&nbsp;\n   \n")

      if (output_type != "rmd") {
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
      }


      if (summary == TRUE) {
        cat("
## Summary table of variable: ",
            target_col,
            "from",
            data_name,
            "  \n")
        f_pander(f_conditional_round(sum_table, digits = digits))
      }


      if (outliers == TRUE) {
        cat("
## Outliers table of variable: ",
            target_col,
            "from",
            data_name,
            "  \n")
        # Check if out_table is NULL before passing to f_pander
        if (!is.null(out_table)) {
          f_pander(out_table, digits = digits)
        } else {
          cat("No outliers detected.\n")
        }
      }

      if (output_type != "rmd") {
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
      }


      cat("
## Boxplots of:", target_col ,"from", data_name,"  \n")
      temp_file_p1_base <- tempfile(fileext = ".png")
      ggsave(
        temp_file_p1_base,
        plot = p1_base + theme(legend.position = "top"),
        width = 7.8,
        height = 5.8,
        units = "in",
        dpi = 300
      )

      cat(paste0("![](", temp_file_p1_base, ")"), "   \n  \n")

      cat("
## Histograms of:", target_col , "from", data_name,"  \n")
      temp_file_p2_hist <- tempfile(fileext = ".png")
      ggsave(
        temp_file_p2_hist,
        plot = p2_hist + theme(legend.position = "top"),
        width = 7.8,
        height = 5.8,
        units = "in",
        dpi = 300
      )

      cat(paste0("![](", temp_file_p2_hist, ")"), "   \n  \n")

      cat("
## QQ-plots of:", target_col , "from", data_name,"  \n")
      temp_file_p3_qq <- tempfile(fileext = ".png")
      ggsave(
        temp_file_p3_qq,
        plot = p3_qq + theme(legend.position = "top"),
        width = 7.8,
        height = 5.8,
        units = "in",
        dpi = 300
      )

      cat(paste0("![](", temp_file_p3_qq, ")"), "   \n  \n")

      } # End render block

    } #Main loop end
    return(output_list)
  } # End generate report function.



  # --- Execute generate_report() exactly ONCE ---
  # For word/pdf/rmd: render = TRUE  (builds R objects + ggsave/markdown)
  # For everything else: render = FALSE (builds R objects only -- much faster)
  needs_render <- output_type %in% c("word", "pdf", "rmd")

  if (needs_render) {
    # Single call: capture markdown AND collect output_list simultaneously
    suppressMessages(
      generated_markdown <- utils::capture.output(
        output_list <- generate_report(render = TRUE)
      )
    )
  } else {
    # Single call: only build R objects (no ggsave, no cat)
    suppressMessages(
      utils::capture.output(
        output_list <- generate_report(render = FALSE),
        file = nullfile()
      )
    )
  }
  class(output_list) <- "f_scan"

  # --- STATISTICAL ADVICE (optional) ---
  if (advice) {
    for (col in target_cols) {
      tryCatch({
        # Build formula: column ~ group_vars (or column ~ 1 if no groups)
        if (!is.null(group_vars) && length(group_vars) > 0) {
          wizard_formula <- as.formula(paste(col, "~", paste(group_vars, collapse = " + ")))
        } else {
          wizard_formula <- as.formula(paste(col, "~ 1"))
        }
        wizard_result <- f_stat_wizard(
          formula     = wizard_formula,
          data        = data,
          data_name   = data_name,      # <- pass user's name (was being overridden post-hoc)
          output_type = output_type,    # <- forward f_scan's output_type
          interactive = FALSE
        )
        output_list[[col]][["advice"]] <- wizard_result
      }, error = function(e) {
        output_list[[col]][["advice"]] <<- paste("Wizard could not analyse", col, ":", conditionMessage(e))
      })
    }
    class(output_list) <- "f_scan"
  }



  # Here the documents are constructed.
  if (output_type %in% c("word", "pdf")) {
    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))

    # Create a temporary R Markdown file
    word_pdf_preamble <- function() {
      paste0(
        "
---
title: \"f_Scan Data Analysis Report\"
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
  - \\setlength{\\droptitle}{-2.5cm} % Adjust vertical spacing
---
"
      )
    }

    # Prevent ## before printed output
    knitr::opts_chunk$set(comment = "")

    # Reuse the markdown already captured above -- no second generate_report() call
    rmd_content <- paste(
      word_pdf_preamble(),
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

    # Open files after creation
    if (open_generated_files == TRUE) {
      # Open the file with default program
      f_open_file(output_path)
    }

    return(invisible(output_list))


  } else if (output_type == "excel") {


    # Extract all f_summary_tables and keep their names
    f_summary_tables <- lapply(output_list, function(obj)
      obj$f_summary)

    # Assign names to the list for Excel sheet names based on response names
    names(f_summary_tables) <- paste0("sum_", target_cols)

    if (outliers == TRUE) {
    # Extract all f_outlier_tables and keep their names
    f_outliers_tables <- lapply(output_list, function(obj)
      obj$f_outliers)

    # Assign names to the list for Excel sheet names based on response names
    names(f_outliers_tables) <- paste0("out_", target_cols)

      output_excel <- c(f_summary_tables, f_outliers_tables)
    } else {
      output_excel <- f_summary_tables
    }

    # show the location were the file is saved
    message(paste0("Saving output in: ", output_path))

    # Write the safe list
    writexl::write_xlsx(output_excel, path = output_path)

    if (open_generated_files == TRUE) {
      f_open_file(output_path)
    }

    return(invisible(output_list))

  } else if (output_type == "rmd") {
    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Reuse the markdown already captured above -- no extra generate_report() call
    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if (output_type == "default") {
    #Default R behavior only show when not stored in an new object
    return(output_list)

  } else if (output_type == "console") {
    #Print output list to the console (forced)
    print(output_list)

    return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")

  }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))

} # end of f_scan function


# -------------------------------------------------------------------------
# S3 METHODS (Print, Summary, Plot)
# -------------------------------------------------------------------------

#' Print method for f_scan objects
#'
#' @param x An \code{f_scan} object.
#' @param summary Logical. Print summary statistics table? Default \code{TRUE}.
#' @param outliers Logical. Print outlier table? Default \code{TRUE}.
#' @param boxplot,histogram,qqplot,main_plot Logical. Which plots to print? All default \code{TRUE}.
#' @param advice Logical. Print statistical test recommendations? Default \code{TRUE} (shown only if \code{advice=TRUE} was used during \code{f_scan}).
#' @param digits Integer. Decimal places for printed tables. Default \code{3}.
#' @param ... Further arguments passed to or from other methods. Currently
#'   unused by the \code{f_scan} methods themselves, but accepted so the
#'   methods remain consistent with the base generics \code{print},
#'   \code{summary}, and \code{plot}.

#' @export
#' @method print f_scan
print.f_scan <- function(x,
                         summary = TRUE,
                         outliers = TRUE,
                         boxplot = TRUE,
                         histogram = TRUE,
                         qqplot = TRUE,
                         main_plot = TRUE,
                         advice = TRUE,
                         digits = 3,
                         ...
                         ) {
  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Print Summary Table
    if (summary && !is.null(sublist[["f_summary"]])) {
      cat("\n--- Summary Statistics ---\n")
      f_pander(sublist[["f_summary"]], digits = digits)
      cat("\n")
    }

    # Print Outlier Table
    if (outliers && "f_outliers" %in% names(sublist)) {
      cat("\n--- Outlier Detection ---\n")
      out_data <- sublist[["f_outliers"]]

      if (is.null(out_data) || ncol(out_data) == 1) {
        cat("No outliers detected.\n")
      } else {
        # If there are many outliers, print the top 10 to avoid spamming the console
        n_out <- nrow(out_data)
        cat(paste0("\nFound ", n_out, " outliers:\n"))
        f_pander(
          head(out_data, 10),
          col_width = 6,
          table_width = 90,
          limit_columns = 8,
          digits = digits
        )
        if (n_out > 15)
          cat(paste0("... and ", n_out - 15, " more rows.\n"))
      }
      cat("\n")
    }

    # Print Plots
    if (boxplot && !is.null(sublist[["boxplot"]]))
      print(sublist[["boxplot"]])
    if (histogram &&
        !is.null(sublist[["histogram"]]))
      print(sublist[["histogram"]])
    if (qqplot && !is.null(sublist[["qqplot"]]))
      print(sublist[["qqplot"]])

    if (main_plot && !is.null(sublist[["main_plot"]])) {
      if (inherits(sublist[["main_plot"]], "grob")) {
        if (!isTRUE(getOption("knitr.in.progress"))) {
          grid::grid.newpage()
        }
        grid::grid.draw(sublist[["main_plot"]])
      } else {
        print(sublist[["main_plot"]])
      }
    }

    # Print Statistical Advice (from f_stat_wizard)
    if (advice && !is.null(sublist[["advice"]])) {
      advice_obj <- sublist[["advice"]]
      if (inherits(advice_obj, "f_stat_wizard")) {
        cat("\n")
        print(advice_obj)
        cat("\n")
      } else if (is.character(advice_obj)) {
        cat("\n--- Statistical Advice ---\n")
        cat(advice_obj, "\n")
      }
    }
  }
  invisible(x)
}

#' Summary method for f_scan objects
#' @rdname print.f_scan
#' @param object f_scan object to make a summary table from.
#'
#' @export
#' @method summary f_scan
summary.f_scan <- function(object,
                           digits = 3,
                           ...) {
  # Loop over each category (a, b, etc.)
  for (category in names(object)) {
    # Get the sublist for this category
    sublist <- object[[category]]
    if (!is.list(sublist)) next

    if ("f_summary" %in% names(sublist)) {
      cat("\n--- Summary Statistics ---\n")
      f_pander(sublist[["f_summary"]],
               digits = digits
               )
      cat("\n")
    }
  }
  invisible(object)
}

#' Plot method for f_scan objects
#' @rdname print.f_scan
#' @param boxplot,histogram,qqplot,main_plot Logical. Which plots to render?
#' @export
#' @method plot f_scan
plot.f_scan <- function(x,
                        boxplot = TRUE,
                        histogram = TRUE,
                        qqplot = TRUE,
                        main_plot = TRUE,
                        ...
                        ) {
  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Logic matches print method but focuses on visuals
    if (boxplot && !is.null(sublist[["boxplot"]]))
      print(sublist[["boxplot"]])
    if (histogram &&
        !is.null(sublist[["histogram"]]))
      print(sublist[["histogram"]])
    if (qqplot && !is.null(sublist[["qqplot"]]))
      print(sublist[["qqplot"]])

    if (main_plot && !is.null(sublist[["main_plot"]])) {
      if (inherits(sublist[["main_plot"]], "grob")) {
        grid::grid.newpage()
        grid::grid.draw(sublist[["main_plot"]])
      } else {
        print(sublist[["main_plot"]])
      }
    }
  }
  invisible(x)
}
