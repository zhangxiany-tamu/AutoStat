% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_utils.R
\name{detect_provider_from_model}
\alias{detect_provider_from_model}
\title{Detect the LLM provider based on the model name}
\usage{
detect_provider_from_model(model)
}
\arguments{
\item{model}{The model name string (e.g., "gpt-4", "gemini-1.0-pro", "claude-3-opus-20240229")}
}
\value{
A string with the detected provider name ("gemini", "openai", or "anthropic"),
or NULL if the provider couldn't be detected.
}
\description{
Identifies the appropriate provider (Gemini, OpenAI, Anthropic)
based on the model name's pattern.
}
\examples{
\dontrun{
# Detect provider from model name
provider <- detect_provider_from_model("gemini-1.0-pro")  # Returns "gemini"
provider <- detect_provider_from_model("gpt-4")           # Returns "openai"
provider <- detect_provider_from_model("claude-3-opus")   # Returns "anthropic"
}
}
\keyword{internal}
