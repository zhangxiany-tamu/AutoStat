#' Send prompt to LLM with retry logic
#'
#' @description
#' Sends a prompt to an LLM with automatic retry on failure, exponential backoff,
#' and proper error handling for different types of failures.
#'
#' @param connection LLM connection object
#' @param prompt The prompt to send
#' @param max_retries Maximum number of retry attempts (default: 3)
#' @param base_delay Base delay in seconds for exponential backoff (default: 1)
#' @param max_delay Maximum delay between retries in seconds (default: 60)
#' @param retry_on_errors Character vector of error patterns that should trigger retry
#' @param ... Additional arguments passed to send_to_llm
#'
#' @return The LLM response text
#'
#' @examples
#' \dontrun{
#' conn <- create_llm_connection("gemini", "your_api_key", "gemini-1.0-pro")
#' response <- send_to_llm_with_retry(
#'   conn,
#'   "Explain linear regression",
#'   max_retries = 3
#' )
#' }
#' @export
send_to_llm_with_retry <- function(connection,
                                   prompt,
                                   max_retries = 3,
                                   base_delay = 1,
                                   max_delay = 60,
                                   retry_on_errors = c(
                                     "rate limit",
                                     "timeout",
                                     "connection",
                                     "503",
                                     "502",
                                     "429",
                                     "500"
                                   ),
                                   ...) {

  attempt <- 0
  last_error <- NULL

  while (attempt <= max_retries) {
    attempt <- attempt + 1

    # Log attempt if verbose
    if (getOption("auto_stat.verbose", FALSE)) {
      if (attempt > 1) {
        message(sprintf("Retry attempt %d/%d for LLM call...", attempt - 1, max_retries))
      }
    }

    # Try to send the request
    result <- tryCatch({
      send_to_llm(connection, prompt, ...)
    }, error = function(e) {
      last_error <<- e

      # Check if this is a retryable error
      error_message <- tolower(as.character(e))
      is_retryable <- any(sapply(retry_on_errors, function(pattern) {
        grepl(pattern, error_message, ignore.case = TRUE)
      }))

      # If not retryable or last attempt, throw the error
      if (!is_retryable || attempt > max_retries) {
        stop(e)
      }

      # Calculate delay with exponential backoff
      delay <- min(base_delay * (2 ^ (attempt - 1)), max_delay)

      # Add jitter to prevent thundering herd
      jitter <- runif(1, 0.5, 1.5)
      actual_delay <- delay * jitter

      if (getOption("auto_stat.verbose", FALSE)) {
        message(sprintf("Retryable error detected: %s. Waiting %.1f seconds before retry...",
                        substring(as.character(e), 1, 100), actual_delay))
      }

      Sys.sleep(actual_delay)

      return(NULL)  # Signal to retry
    })

    # If we got a result, return it
    if (!is.null(result)) {
      if (attempt > 1 && getOption("auto_stat.verbose", FALSE)) {
        message("LLM call succeeded after retry.")
      }
      return(result)
    }
  }

  # If we get here, all retries failed
  stop(sprintf("Failed after %d retries. Last error: %s",
               max_retries,
               as.character(last_error)))
}

#' Check if an error is retryable
#'
#' @param error The error object or message
#' @param patterns Character vector of patterns indicating retryable errors
#' @return Logical indicating if the error should trigger a retry
#' @keywords internal
is_retryable_error <- function(error, patterns = c("rate limit", "timeout", "connection",
                                                   "503", "502", "429", "500")) {
  error_text <- tolower(as.character(error))
  any(sapply(patterns, function(p) grepl(p, error_text, ignore.case = TRUE)))
}

#' Set auto_stat verbosity
#'
#' @param verbose Logical indicating whether to show detailed messages
#' @export
set_auto_stat_verbose <- function(verbose = TRUE) {
  options(auto_stat.verbose = verbose)
}
