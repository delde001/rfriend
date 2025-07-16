is_nested <- function(model1, model2) {
  # Initialize result
  result <- list(
    nested = FALSE,
    reason = "Models could not be compared",
    direction = NA
  )

  # reason_message <- "Automatically determined if models were nested and found:\n"

  # Try to extract formulas
  f1 <- tryCatch(formula(model1), error = function(e) NULL)
  f2 <- tryCatch(formula(model2), error = function(e) NULL)

  if (is.null(f1) || is.null(f2)) {
    result$reason <- "that the formula could not be extracted from one or both models."
    return(result)
  }

  # Extract response variables
  resp1 <- as.character(f1)[2]
  resp2 <- as.character(f2)[2]

  # Check if response variables are the same
  if (resp1 != resp2) {
    result$reason <- "that there is a different number of response variables."
    return(result)
  }


  get_model_data <- function(model) {
    # Try various common ways models store data
    data <- NULL

    # Try model.frame first
    data <- tryCatch(model.frame(model), error = function(e) NULL)
    if (!is.null(data)) return(data)

    # Try model$model
    if (!is.null(model$model)) return(model$model)

    # Try model$data
    if (!is.null(model$data)) {
      # Check if model$data is a data frame or a character string
      if (is.data.frame(model$data)) {
        return(model$data)
      } else if (is.character(model$data)) {
        # If it's a character string, try to get the data from global environment
        data_obj <- tryCatch(get(model$data, envir = .GlobalEnv), error = function(e) NULL)
        if (!is.null(data_obj) && is.data.frame(data_obj)) return(data_obj)
      }
    }

    # Try model$call$data
    if (!is.null(model$call$data)) {
      data_name <- tryCatch(as.character(model$call$data), error = function(e) NULL)
      if (!is.null(data_name)) {
        data_obj <- tryCatch(get(data_name, envir = .GlobalEnv), error = function(e) NULL)
        if (!is.null(data_obj) && is.data.frame(data_obj)) return(data_obj)
      }
    }

    # Try environment(formula(model))
    env_data <- tryCatch({
      env <- environment(formula(model))
      if (exists("data", env)) {
        data_obj <- get("data", env)
        if (is.data.frame(data_obj)) {
          return(data_obj)
        } else if (is.character(data_obj)) {
          # If it's a character string, try to get the data from global environment
          data_frame <- tryCatch(get(data_obj, envir = .GlobalEnv), error = function(e) NULL)
          if (!is.null(data_frame) && is.data.frame(data_frame)) return(data_frame)
        }
      }
      NULL
    }, error = function(e) NULL)
    if (!is.null(env_data)) return(env_data)

    # Try to extract data name from call and get it from global environment
    data_name <- tryCatch({
      call_obj <- model$call
      if (is.null(call_obj)) call_obj <- attr(model, "call")
      if (!is.null(call_obj) && "data" %in% names(call_obj)) {
        as.character(call_obj$data)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(data_name)) {
      data_obj <- tryCatch(get(data_name, envir = .GlobalEnv), error = function(e) NULL)
      if (!is.null(data_obj) && is.data.frame(data_obj)) return(data_obj)
    }

    return(NULL)
  }


  data1 <- get_model_data(model1)
  data2 <- get_model_data(model2)


  if (is.null(is.null(data1)) || is.null(data2)) {
    result$reason <- "the data could not be extracted from one or both models."
    return(result)
  } else if(nrow(data1) != nrow(data2)) {

    # Check if data dimensions match
    result$reason <- "that the number of observations differs between models."
    return(result)
  }

  # Extract terms using multiple approaches
  get_model_terms <- function(model) {
    # Try terms() first
    terms_labels <- tryCatch(attr(terms(model), "term.labels"),
                             error = function(e) NULL)
    if (!is.null(terms_labels)) return(terms_labels)

    # Try extracting from formula
    f <- tryCatch(formula(model), error = function(e) NULL)
    if (!is.null(f)) {
      rhs <- as.character(f)[3]
      if (rhs == "1" || rhs == "0") return(character(0))
      terms_str <- unlist(strsplit(rhs, " \\+ "))
      return(terms_str)
    }

    return(NULL)
  }

  terms1 <- get_model_terms(model1)
  terms2 <- get_model_terms(model2)

  if (is.null(terms1) || is.null(terms2)) {
    result$reason <- "that the predictors could not be extracted from one or both models."
    return(result)
  }

  # Check if one model's terms are a subset of the other
  if (all(terms1 %in% terms2)) {
    result$nested <- TRUE
    result$reason <- "Model 1 is nested within Model 2"
    result$direction <- "1 in 2"
    return(result)
  } else if (all(terms2 %in% terms1)) {
    result$nested <- TRUE
    result$reason <- "Model 2 is nested within Model 1"
    result$direction <- "2 in 1"
    return(result)
  } else {
    result$reason <- "that neither model is nested within the other"
    return(result)
  }
}
