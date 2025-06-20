% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute.R
\name{execute_code}
\alias{execute_code}
\title{Execute R code from LLM with enhanced safety and error handling}
\usage{
execute_code(data, code_input)
}
\arguments{
\item{data}{The dataset to be available in the execution environment.}

\item{code_input}{A single string of R code, or a list/vector of R code strings (chunks).
It's assumed this code has already been processed by \code{process_gemini_code}.}
}
\value{
A list containing the results of execution, including any created objects,
printed output, errors, and warnings for each block.
}
\description{
Executes R code safely and captures the results, including any objects created,
printed output, errors, and warnings. This function is used internally by \code{auto_stat()}
but can also be used directly.
}
\examples{
\dontrun{
# Execute a single block of code
results <- execute_code(mtcars, "
  model <- lm(mpg ~ wt + hp, data = data)
  summary(model)
  plot(model)
")

# Execute multiple code chunks
results <- execute_code(iris, c(
  "# Exploratory Analysis\nlibrary(ggplot2)\nstr(data)",
  "# Create scatter plot\nggplot(data, aes(x = Sepal.Length, y = Petal.Length, color = Species))
  + geom_point()"
))
}
}
\keyword{internal}
