#' Print Method for Age-Length Key
#'
#' @param x An age-length key data frame
#' @param ... Additional arguments (not used)
#' @export
print.age_length_key <- function(x, ...) {
  cat("*** AGE-LENGTH KEY SUMMARY ***\n")
  cat("Rows:", nrow(x), "\n")
  cat("Columns:", ncol(x), "\n")
  invisible(x)
}
