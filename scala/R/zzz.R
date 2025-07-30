.onAttach <- function(libname, pkgname) {
  # Get package version from DESCRIPTION
  version <- utils::packageVersion(pkgname)

  packageStartupMessage(
    paste0(
      "scala package version ", version, " loaded\n",
      "Warning - this package is under active development. Check for updates."
    )
  )
}
