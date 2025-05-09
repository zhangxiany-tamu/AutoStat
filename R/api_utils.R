#' Create a generic LLM connection object
#'
#' @description
#' Creates a connection object for interacting with a specified LLM provider. This is an S3
#' generic function with specific implementations for each supported provider (Gemini, OpenAI,
#' and Anthropic).
#'
#' @param provider The LLM provider (e.g., "gemini", "openai", "anthropic").
#' @param api_key Your API key for the specified provider.
#' @param model The model to use for the specified provider.
#' @param ... Additional arguments specific to the provider.
#'
#' @return An LLM connection object of a class specific to the provider (e.g., "gemini_connection",
#'         "openai_connection", "anthropic_connection").
#'
#' @examples
#' \dontrun{
#' # Create a connection to Gemini
#' gemini_conn <- create_llm_connection(
#'   provider = "gemini",
#'   api_key = "your_gemini_api_key",
#'   model = "gemini-1.0-pro"
#' )
#'
#' # Create a connection to OpenAI with organization ID
#' openai_conn <- create_llm_connection(
#'   provider = "openai",
#'   api_key = "your_openai_api_key",
#'   model = "gpt-4",
#'   organization_id = "your_org_id"
#' )
#'
#' # Create a connection to Anthropic's Claude
#' claude_conn <- create_llm_connection(
#'   provider = "anthropic",
#'   api_key = "your_anthropic_api_key",
#'   model = "claude-3-opus-20240229"
#' )
#' }
#' @export
create_llm_connection <- function(provider, api_key, model, ...) {
  # --- S3 Dispatch Fix ---
  # Create a simple object and assign the provider name as its class.
  # This allows S3 dispatch to work based on the provider string value.
  provider_obj <- structure(list(), class = provider)

  # Call UseMethod on this dummy object. R will look for create_llm_connection.<provider>
  # We pass the original arguments along.
  UseMethod("create_llm_connection", provider_obj)
  # --- End S3 Dispatch Fix ---
}

#' Send a prompt to a connected LLM
#'
#' @description
#' Sends a text prompt to a connected LLM and retrieves the response. This is an S3 generic function
#' with specific implementations for each supported LLM provider. It can optionally include a dataset
#' in the prompt if it's small enough.
#'
#' @param connection An LLM connection object (e.g., from `create_llm_connection`).
#' @param prompt The text prompt to send to the LLM.
#' @param data Optional dataset to include in the prompt (if supported and small).
#' @param max_rows Max rows for including full data.
#' @param max_cols Max columns for including full data.
#' @param llm_params A list of provider-specific parameters (e.g., temperature, max_tokens).
#' @param ... Additional arguments specific to the provider's send method.
#'
#' @return The LLM's response text.
#'
#' @examples
#' \dontrun{
#' # Create a connection
#' conn <- create_llm_connection(
#'   provider = "gemini",
#'   api_key = "your_gemini_api_key",
#'   model = "gemini-1.0-pro"
#' )
#'
#' # Send a simple prompt
#' response <- send_to_llm(
#'   connection = conn,
#'   prompt = "Suggest three statistical methods for time series analysis."
#' )
#'
#' # Send a prompt with a dataset
#' response <- send_to_llm(
#'   connection = conn,
#'   prompt = "How would you analyze this dataset to understand the relationship between horsepower
#'   and mpg?",
#'   data = mtcars,
#'   llm_params = list(temperature = 0.2, maxOutputTokens = 2000)
#' )
#' }
#' @export
send_to_llm <- function(connection, prompt, data = NULL, max_rows = 50, max_cols = 20, llm_params = list(), ...) {
  # Dispatch happens correctly on the class of the 'connection' object
  UseMethod("send_to_llm")
}

# --- Gemini Specific Implementation ---

#' Create LLM connection for Gemini
#' @param provider An object whose class is "gemini" (used for S3 dispatch).
#' @param api_key Your Gemini API key.
#' @param model Gemini model name (e.g., "gemini-1.0-pro").
#' @param ... Additional arguments (currently ignored for Gemini).
#' @return A "gemini_connection" object.
#' @export
create_llm_connection.gemini <- function(provider, api_key, model, ...) {
  # Retrieve arguments from parent call frame if needed
  call_generic <- sys.call(sys.parent(1)) # Get the call to create_llm_connection

  # Evaluate arguments in the calling environment
  # Check if api_key was passed directly to the method (unlikely with this dispatch)
  # or retrieve it from the generic call environment
  if (missing(api_key) || is.null(api_key)) {
    api_key_arg <- call_generic$api_key
    if(is.null(api_key_arg)) stop("Gemini API key is required (missing from call).")
    api_key <- eval(api_key_arg, envir = parent.frame(2)) # Evaluate in the environment where create_llm_connection was called
    if (is.null(api_key) || api_key == "") stop("Gemini API key is required (evaluated to NULL/empty).")
  }
  if (missing(model) || is.null(model)) {
    model_arg <- call_generic$model
    if(is.null(model_arg)) stop("Gemini model name is required (missing from call).")
    model <- eval(model_arg, envir = parent.frame(2))
    if (is.null(model) || model == "") stop("Gemini model name is required (evaluated to NULL/empty).")
  }

  connection <- list(
    api_key = api_key,
    model = model,
    provider_name = "gemini"
  )
  class(connection) <- c("gemini_connection", "llm_connection")
  return(connection)
}

#' Send prompt to Gemini API
#' @param connection A "gemini_connection" object.
#' @param prompt Text prompt to send.
#' @param data Optional dataset to include.
#' @param max_rows Max rows for including full data.
#' @param max_cols Max columns for including full data.
#' @param llm_params A list of parameters for Gemini's generationConfig.
#' @param ... Additional arguments (currently ignored).
#' @return Gemini response text.
#' @export
send_to_llm.gemini_connection <- function(connection, prompt, data = NULL, max_rows = 50, max_cols = 20,
                                          llm_params = list(), ...) {
  full_prompt <- prompt
  if (!is.null(data) && nrow(data) <= max_rows && ncol(data) <= max_cols) {
    data_text_lines <- utils::capture.output(print(as.data.frame(data), print.gap = 2, row.names = FALSE))
    max_data_lines <- 200
    if(length(data_text_lines) > max_data_lines) {
      data_text_lines <- c(data_text_lines[1:max_data_lines], "...", paste("Dataset truncated after", max_data_lines, "lines for brevity."))
    }
    full_data_text <- paste(data_text_lines, collapse = "\n")
    full_prompt <- paste0(prompt, "\n\nFull dataset for reference (or first ", max_data_lines," lines if large):\n\n```\n", full_data_text, "\n```")
  }

  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", connection$model, ":generateContent?key=", connection$api_key)

  default_gemini_params <- list(
    temperature = 0.3,
    maxOutputTokens = 4096,
    topP = 0.95,
    topK = 40
  )
  final_gemini_params <- utils::modifyList(default_gemini_params, llm_params)
  final_gemini_params$maxOutputTokens <- as.integer(final_gemini_params$maxOutputTokens)
  final_gemini_params$topK <- as.integer(final_gemini_params$topK)

  body <- list(
    contents = list(list(parts = list(list(text = full_prompt)))),
    generationConfig = final_gemini_params
  )

  response <- httr::POST(
    url = api_url,
    httr::add_headers("Content-Type" = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, pretty = FALSE),
    encode = "json"
  )

  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste("Gemini API request failed (", connection$model, ") with status ", httr::status_code(response), ": ", error_content, sep=""))
  }

  content <- httr::content(response, "parsed", encoding = "UTF-8")

  result_text <- tryCatch({
    if (!is.null(content$candidates) && length(content$candidates) > 0 &&
        !is.null(content$candidates[[1]]$content) &&
        !is.null(content$candidates[[1]]$content$parts) && length(content$candidates[[1]]$content$parts) > 0 &&
        !is.null(content$candidates[[1]]$content$parts[[1]]$text)) {
      content$candidates[[1]]$content$parts[[1]]$text
    } else {
      if (!is.null(content$promptFeedback) && !is.null(content$promptFeedback$blockReason)) {
        stop(paste0("Gemini API blocked the prompt. Reason: ", content$promptFeedback$blockReason,
                    ". Details: ", content$promptFeedback$blockReasonMessage %||% "N/A"))
      } else if (!is.null(content$candidates[[1]]$finishReason) && content$candidates[[1]]$finishReason != "STOP") {
        stop(paste0("Gemini API finished with reason: ", content$candidates[[1]]$finishReason,
                    ". Safety ratings: ", jsonlite::toJSON(content$candidates[[1]]$safetyRatings %||% "N/A")))
      }
      stop("Failed to extract text from Gemini API response: No valid text part found or content blocked.")
    }
  }, error = function(e) {
    stop(paste("Error parsing Gemini API response (", connection$model, "): ", e$message, sep=""))
  })
  return(result_text)
}

# --- OpenAI Specific Implementation ---
#' Create LLM connection for OpenAI
#' @param provider An object whose class is "openai".
#' @param api_key Your OpenAI API key.
#' @param model OpenAI model name.
#' @param organization_id Optional OpenAI organization ID.
#' @param ... Additional arguments.
#' @return An "openai_connection" object.
#' @export
create_llm_connection.openai <- function(provider, api_key, model, organization_id = NULL, ...) {
  # Retrieve arguments from parent call frame if needed
  call_generic <- sys.call(sys.parent(1))
  if (missing(api_key) || is.null(api_key)) {
    api_key_arg <- call_generic$api_key
    if(is.null(api_key_arg)) stop("OpenAI API key is required.")
    api_key <- eval(api_key_arg, envir = parent.frame(2))
    if (is.null(api_key) || api_key == "") stop("OpenAI API key is required.")
  }
  if (missing(model) || is.null(model)) {
    model_arg <- call_generic$model
    if(is.null(model_arg)) stop("OpenAI model name is required.")
    model <- eval(model_arg, envir = parent.frame(2))
    if (is.null(model) || model == "") stop("OpenAI model name is required.")
  }
  if (missing(organization_id) || is.null(organization_id)) {
    org_arg <- call_generic$organization_id
    if (!is.null(org_arg)) {
      organization_id <- eval(org_arg, envir = parent.frame(2))
    } else {
      organization_id <- NULL
    }
  }

  connection <- list(
    api_key = api_key, model = model, organization_id = organization_id,
    provider_name = "openai", api_base_url = "https://api.openai.com/v1"
  )
  dots <- list(...)
  if("api_base_url" %in% names(dots)) connection$api_base_url <- dots$api_base_url
  class(connection) <- c("openai_connection", "llm_connection")
  return(connection)
}

#' Send prompt to OpenAI API
#' @param connection An "openai_connection" object.
#' @param prompt Text prompt to send.
#' @param data Optional dataset.
#' @param max_rows Max rows for including full data.
#' @param max_cols Max columns for including full data.
#' @param llm_params A list of parameters for OpenAI's API.
#' @param ... Additional arguments.
#' @return OpenAI response text.
#' @export
send_to_llm.openai_connection <- function(connection, prompt, data = NULL, max_rows = 50, max_cols = 20,
                                          llm_params = list(), ...) {
  default_openai_params <- list(
    temperature = 0.3,
    max_tokens = 4000,
    top_p = 1.0,
    frequency_penalty = 0,
    presence_penalty = 0,
    system_message = "You are an expert R statistician and programmer. Provide complete and accurate R code and interpretations as requested."
  )
  final_openai_params <- utils::modifyList(default_openai_params, llm_params)

  # Prepare messages
  messages <- list()
  if (!is.null(final_openai_params$system_message) && nzchar(final_openai_params$system_message)) {
    messages <- c(messages, list(list(role = "system", content = final_openai_params$system_message)))
  }

  # Add data to prompt if appropriate
  full_user_prompt <- prompt
  if (!is.null(data) && nrow(data) <= max_rows && ncol(data) <= max_cols) {
    data_text_lines <- utils::capture.output(print(as.data.frame(data), print.gap = 2, row.names = FALSE))
    max_data_lines <- 200
    if(length(data_text_lines) > max_data_lines) {
      data_text_lines <- c(data_text_lines[1:max_data_lines], "...", paste("Dataset truncated after", max_data_lines, "lines for brevity."))
    }
    full_data_text <- paste(data_text_lines, collapse = "\n")
    full_user_prompt <- paste0(prompt, "\n\nDataset for reference (or first ", max_data_lines," lines if large):\n\n```\n", full_data_text, "\n```")
  }
  messages <- c(messages, list(list(role = "user", content = full_user_prompt)))

  # Prepare request body
  request_body <- list(
    model = connection$model,
    messages = messages,
    temperature = final_openai_params$temperature,
    max_tokens = final_openai_params$max_tokens,
    top_p = final_openai_params$top_p,
    frequency_penalty = final_openai_params$frequency_penalty,
    presence_penalty = final_openai_params$presence_penalty
  )

  # Set up headers
  headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", connection$api_key)
  )
  if (!is.null(connection$organization_id) && connection$organization_id != "") {
    headers <- c(headers, "OpenAI-Organization" = connection$organization_id)
  }

  # Make API call
  api_url <- paste0(connection$api_base_url, "/chat/completions")

  response <- tryCatch({
    httr::POST(
      url = api_url,
      httr::add_headers(.headers = headers),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    stop(paste("Error connecting to OpenAI API:", e$message))
  })

  # Process response
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste("OpenAI API request failed:", httr::status_code(response), ":", error_content))
  }

  content <- httr::content(response, "parsed", encoding = "UTF-8")

  # Extract response text
  result_text <- tryCatch({
    content$choices[[1]]$message$content
  }, error = function(e) {
    stop(paste("Error parsing OpenAI API response:", e$message))
  })

  return(result_text)
}

# --- Anthropic (Claude) Specific Implementation ---
#' Create LLM connection for Anthropic (Claude)
#' @param provider An object whose class is "anthropic".
#' @param api_key Your Anthropic API key.
#' @param model Anthropic model name.
#' @param anthropic_version API version (default: "2023-06-01")
#' @param api_base_url Base URL for the Anthropic API (default: "https://api.anthropic.com/v1")
#' @param ... Additional arguments.
#' @return An "anthropic_connection" object.
#' @export
create_llm_connection.anthropic <- function(provider, api_key, model, anthropic_version = "2023-06-01", api_base_url = "https://api.anthropic.com/v1", ...) {
  # Retrieve arguments from parent call frame if needed
  call_generic <- sys.call(sys.parent(1))
  if (missing(api_key) || is.null(api_key)) {
    api_key_arg <- call_generic$api_key
    if(is.null(api_key_arg)) stop("Anthropic API key is required.")
    api_key <- eval(api_key_arg, envir = parent.frame(2))
    if (is.null(api_key) || api_key == "") stop("Anthropic API key is required.")
  }
  if (missing(model) || is.null(model)) {
    model_arg <- call_generic$model
    if(is.null(model_arg)) stop("Anthropic model name is required.")
    model <- eval(model_arg, envir = parent.frame(2))
    if (is.null(model) || model == "") stop("Anthropic model name is required.")
  }

  # Parse additional arguments
  dots <- list(...)

  # If anthropic_version or api_base_url were passed in dots (from the old version),
  # they would override the defaults
  if ("anthropic_version" %in% names(dots)) anthropic_version <- dots$anthropic_version
  if ("api_base_url" %in% names(dots)) api_base_url <- dots$api_base_url

  connection <- list(
    api_key = api_key,
    model = model,
    provider_name = "anthropic",
    anthropic_version = anthropic_version,
    api_base_url = api_base_url
  )
  class(connection) <- c("anthropic_connection", "llm_connection")
  return(connection)
}

#' Send prompt to Anthropic (Claude) API
#' @param connection An "anthropic_connection" object.
#' @param prompt Text prompt to send.
#' @param data Optional dataset.
#' @param max_rows Max rows for including full data.
#' @param max_cols Max columns for including full data.
#' @param llm_params A list of parameters for Anthropic's API.
#' @param ... Additional arguments.
#' @return Anthropic response text.
#' @export
send_to_llm.anthropic_connection <- function(connection, prompt, data = NULL, max_rows = 50, max_cols = 20,
                                             llm_params = list(), ...) {
  # Default parameters for Anthropic
  default_anthropic_params <- list(
    temperature = 0.3,
    max_tokens = 4000,
    top_p = 0.95,
    system_message = "You are an expert R statistician and programmer. Provide complete and accurate R code and interpretations as requested."
  )
  final_anthropic_params <- utils::modifyList(default_anthropic_params, llm_params)

  # Prepare the prompt with data if appropriate
  full_user_prompt <- prompt
  if (!is.null(data) && nrow(data) <= max_rows && ncol(data) <= max_cols) {
    data_text_lines <- utils::capture.output(print(as.data.frame(data), print.gap = 2, row.names = FALSE))
    max_data_lines <- 200
    if(length(data_text_lines) > max_data_lines) {
      data_text_lines <- c(data_text_lines[1:max_data_lines], "...", paste("Dataset truncated after", max_data_lines, "lines for brevity."))
    }
    full_data_text <- paste(data_text_lines, collapse = "\n")
    full_user_prompt <- paste0(prompt, "\n\nDataset for reference (or first ", max_data_lines," lines if large):\n\n```\n", full_data_text, "\n```")
  }

  # Determine which API version to use
  is_messages_api <- grepl("-", connection$anthropic_version) && as.numeric(gsub("-.*", "", connection$anthropic_version)) >= 2023

  if (is_messages_api) {
    # Use the newer Messages API (v2023-06-01 and later)
    api_url <- paste0(connection$api_base_url, "/messages")

    request_body <- list(
      model = connection$model,
      messages = list(
        list(role = "user", content = full_user_prompt)
      ),
      temperature = final_anthropic_params$temperature,
      max_tokens = as.integer(final_anthropic_params$max_tokens)
    )

    # Add system prompt if provided
    if (!is.null(final_anthropic_params$system_message) && nzchar(final_anthropic_params$system_message)) {
      request_body$system <- final_anthropic_params$system_message
    }

  } else {
    # Use the legacy Completions API (pre-2023 versions)
    api_url <- paste0(connection$api_base_url, "/complete")

    # Format prompt in the way Anthropic expects
    formatted_prompt <- paste0("\n\nHuman: ", full_user_prompt, "\n\nAssistant:")

    request_body <- list(
      model = connection$model,
      prompt = formatted_prompt,
      temperature = final_anthropic_params$temperature,
      max_tokens_to_sample = as.integer(final_anthropic_params$max_tokens),
      top_p = final_anthropic_params$top_p
    )
  }

  # Set up headers
  headers <- c(
    "Content-Type" = "application/json",
    "x-api-key" = connection$api_key,
    "anthropic-version" = connection$anthropic_version
  )

  # Make API call
  response <- tryCatch({
    httr::POST(
      url = api_url,
      httr::add_headers(.headers = headers),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "json"
    )
  }, error = function(e) {
    stop(paste("Error connecting to Anthropic API:", e$message))
  })

  # Process response
  if (httr::status_code(response) != 200) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste("Anthropic API request failed:", httr::status_code(response), ":", error_content))
  }

  content <- httr::content(response, "parsed", encoding = "UTF-8")

  # Extract response text
  result_text <- tryCatch({
    if (is_messages_api) {
      content$content[[1]]$text  # For Messages API
    } else {
      content$completion  # For Completions API
    }
  }, error = function(e) {
    stop(paste("Error parsing Anthropic API response:", e$message))
  })

  return(result_text)
}

# --- Default Methods ---
#' Default method for creating LLM connections
#' @param provider An object whose class matches the provider string.
#' @param ... Additional arguments captured from the original call.
#' @export
create_llm_connection.default <- function(provider, ...) {
  # Extract the actual provider string from the class of the dummy object
  provider_name <- class(provider)[1]
  stop(paste("LLM provider '", provider_name, "' is not supported or a specific create_llm_connection.", provider_name, " method is missing.", sep=""))
}

#' Default method for sending prompts to LLMs
#' @param connection An LLM connection object.
#' @param prompt Text prompt to send to the LLM.
#' @param ... Additional arguments.
#' @export
send_to_llm.default <- function(connection, prompt, ...) {
  stop(paste("No specific send_to_llm method implemented for connection class:", class(connection)[1]))
}

#' Extract code blocks from LLM response text
#'
#' @description
#' Extracts R code blocks from text responses provided by an LLM.
#' The function looks for code blocks surrounded by triple backticks (```).
#'
#' @param text The text response from an LLM containing code blocks.
#'
#' @return A character string containing the extracted code, or an empty string if no code blocks are found.
#'
#' @examples
#' \dontrun{
#' # Extract code blocks from an LLM response
#' response_text <- send_to_llm(connection, "Generate R code to create a scatter plot of iris data")
#' extracted_code <- extract_code_blocks(response_text)
#' }
#' @keywords internal
extract_code_blocks <- function(text) {
  if (is.null(text) || nchar(trimws(text)) == 0) return("")
  pattern_r_explicit <- "```(?:[rR]|\\{[rR]\\})[\\s\\S]*?\\n([\\s\\S]*?)```"
  pattern_generic <- "```[\\s\\S]*?\\n([\\s\\S]*?)```"
  extracted_code_list <- character(0)
  matches_r <- gregexpr(pattern_r_explicit, text, perl = TRUE)
  if (matches_r[[1]][1] != -1) {
    code_segments <- regmatches(text, matches_r)[[1]]
    for (segment in code_segments) {
      actual_code <- sub(pattern_r_explicit, "\\1", segment, perl = TRUE)
      extracted_code_list <- c(extracted_code_list, actual_code)
    }
  }
  if (length(extracted_code_list) == 0) {
    matches_generic <- gregexpr(pattern_generic, text, perl = TRUE)
    if (matches_generic[[1]][1] != -1) {
      code_segments <- regmatches(text, matches_generic)[[1]]
      for (segment in code_segments) {
        actual_code <- sub(pattern_generic, "\\1", segment, perl = TRUE)
        extracted_code_list <- c(extracted_code_list, actual_code)
      }
    }
  }
  if (length(extracted_code_list) > 0) {
    return(paste(extracted_code_list, collapse = "\n\n"))
  }
  return("")
}

#' Process code extracted from LLM response
#'
#' @description
#' Processes code extracted from an LLM response to prepare it for execution.
#' This function cleans up the code by removing unnecessary lines and ensuring
#' it starts with relevant R statements.
#'
#' @param code The code string extracted from an LLM response.
#'
#' @return A processed code string ready for execution.
#'
#' @examples
#' \dontrun{
#' # Process code from an LLM response
#' raw_code <- extract_code_blocks(llm_response)
#' executable_code <- process_code(raw_code)
#' }
#' @keywords internal
process_code <- function(code) {
  if (is.null(code) || nchar(trimws(code)) == 0) return("")
  lines <- strsplit(code, "\n")[[1]]
  non_empty_indices <- which(trimws(lines) != "")
  if (length(non_empty_indices) == 0) return("")
  lines <- lines[min(non_empty_indices):max(non_empty_indices)]
  r_start_patterns <- c(
    "^\\s*#", "^\\s*library\\(", "^\\s*require\\(", "^\\s*install\\.packages\\(",
    "^\\s*[a-zA-Z0-9_\\.]+\\s*<-", "^\\s*[a-zA-Z0-9_\\.]+\\s*=",
    "^\\s*if\\s*\\(", "^\\s*for\\s*\\(", "^\\s*while\\s*\\(", "^\\s*function\\s*\\(",
    "^\\s*source\\s*\\(", "^\\s*data\\s*\\(", "^\\s*ggplot\\(", "^\\s*plot\\(", "^\\s*par\\("
  )
  start_idx <- 1
  for (i in 1:length(lines)) {
    if (any(sapply(r_start_patterns, function(p) grepl(p, lines[i])))) {
      start_idx <- i; break
    }
  }
  code_lines <- if (start_idx <= length(lines)) lines[start_idx:length(lines)] else character(0)
  if (length(code_lines) == 0) return("")
  return(paste(code_lines, collapse = "\n"))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Detect the LLM provider based on the model name
#'
#' @description
#' Identifies the appropriate provider (Gemini, OpenAI, Anthropic)
#' based on the model name's pattern.
#'
#' @param model The model name string (e.g., "gpt-4", "gemini-1.0-pro", "claude-3-opus-20240229")
#'
#' @return A string with the detected provider name ("gemini", "openai", or "anthropic"),
#'         or NULL if the provider couldn't be detected.
#'
#' @examples
#' \dontrun{
#' # Detect provider from model name
#' provider <- detect_provider_from_model("gemini-1.0-pro")  # Returns "gemini"
#' provider <- detect_provider_from_model("gpt-4")           # Returns "openai"
#' provider <- detect_provider_from_model("claude-3-opus")   # Returns "anthropic"
#' }
#' @keywords internal
detect_provider_from_model <- function(model) {
  if (is.null(model) || !is.character(model) || nchar(trimws(model)) == 0) {
    return(NULL)
  }

  model <- tolower(trimws(model))

  # OpenAI models
  if (grepl("^gpt-", model) ||
      grepl("^text-", model) ||
      model %in% c("davinci", "curie", "babbage", "ada")) {
    return("openai")
  }

  # Gemini models
  if (grepl("^gemini-", model) ||
      grepl("^palm-", model) ||
      grepl("^bison-", model) ||
      grepl("^text-bison", model)) {
    return("gemini")
  }

  # Anthropic models
  if (grepl("^claude-", model) ||
      grepl("^anthropic\\.", model)) {
    return("anthropic")
  }

  # Could not determine provider
  return(NULL)
}
