.onLoad <- function(libname, pkgname) {
  # To ensure backward compatibility
  pkg <- getNamespace(pkgname)
  assign(
    "strrep",
    function (x, times) {
      x <- as.character(x)
      if (length(x) == 0L) { return(x) }
      unlist(.mapply(function(x, times) {
        if (is.na(x) || is.na(times)) { return(NA_character_) }
        if (times <= 0L) { return("") }
        paste0(replicate(times, x), collapse = "")
      }, list(x = x, times = times), MoreArgs = list()), use.names = FALSE)
    },
    envir = pkg
  )

  # Package options
  op <- options()
  op.rdbnomics <- list(
    rdbnomics.mask_compatible_providers = c(
      "BIS", "ECB", "Eurostat", "FED", "ILO", "IMF", "INSEE", "OECD", "WTO"
    ),
    rdbnomics.use_readLines = FALSE,
    rdbnomics.sleep_run = 2L,
    rdbnomics.try_run = 2L,
    rdbnomics.verbose_warning = TRUE,
    rdbnomics.api_base_url = "https://api.db.nomics.world",
    rdbnomics.api_version = 22,
    rdbnomics.authorized_api_version = c(21, 22),
    rdbnomics.verbose_warning_readLines = FALSE,
    rdbnomics.timestamp_tz = "GMT"
  )
  toset <- !(names(op.rdbnomics) %in% names(op))
  if(any(toset)) options(op.rdbnomics[toset])
  invisible()
}
