.colrr_pak_packages <- c(
  brathering = "close-your-eyes/brathering"
)

.colrr_cran_packages <- c(
  "farver",
  "ggplot2",
  "ggtext",
  "igraph",
  "pak",
  "RColorBrewer",
  "reticulate",
  "TSP"
)

.colrr_optional_packages <- c(
  names(.colrr_pak_packages),
  .colrr_cran_packages
)

.ensure_package <- function(package) {
  if (!package %in% .colrr_optional_packages) {
    stop("Unknown optional package: ", package, call. = FALSE)
  }
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible(TRUE))
  }

  message("Installing optional package '", package, "'.")
  if (package %in% names(.colrr_pak_packages)) {
    if (!requireNamespace("pak", quietly = TRUE)) {
      utils::install.packages("pak")
    }
    pak::pak(unname(.colrr_pak_packages[[package]]))
  } else {
    utils::install.packages(package)
  }

  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' is required for this function but could not be installed.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.ensure_packages <- function(packages) {
  invisible(lapply(unique(packages), colrr:::.ensure_package))
}
