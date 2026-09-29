# Add sex-specific smooths without changing parametric terms or explicit by terms.
alk_sex_term <- function(term, by_sex) {
  if (by_sex && grepl("^\\s*(s|te|ti|t2)\\s*\\(", term) && !grepl("by\\s*=", term)) {
    term <- sub("\\)\\s*$", ", by = sex)", term)
  }
  term
}

validate_alk_plus_group <- function(plus_group) {
  if (!is.null(plus_group) && (!is.numeric(plus_group) || length(plus_group) != 1L ||
      !is.finite(plus_group) || plus_group < 1 || plus_group != floor(plus_group))) {
    stop("plus_group must be NULL or one positive integer.")
  }
  invisible(plus_group)
}
