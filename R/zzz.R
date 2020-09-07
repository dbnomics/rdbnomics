.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Visit <https://db.nomics.world>.")
}

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
  opts <- list(
    rdbnomics.use_readLines = FALSE,
    rdbnomics.sleep_run = 1L,
    rdbnomics.try_run = 2L,
    rdbnomics.verbose_warning = TRUE,
    rdbnomics.api_base_url = "https://api.db.nomics.world",
    rdbnomics.editor_base_url = "https://editor.nomics.world",
    rdbnomics.api_version = 22,
    rdbnomics.editor_version = 1,
    rdbnomics.authorized_api_version = c(21, 22),
    rdbnomics.verbose_warning_readLines = FALSE,
    rdbnomics.timestamp_tz = "GMT",
    rdbnomics.http_ok = "200([[:blank:]]+OK)?[[:blank:]]*$",
    rdbnomics.curl_config = NULL,
    rdbnomics.rdb_no_arg = TRUE,
    rdbnomics.metadata = TRUE,
    rdbnomics.filters = NULL,
    rdbnomics.progress_bar = TRUE,
    rdbnomics.translate_codes = TRUE,
    rdbnomics.progress_bar_datasets = FALSE,
    rdbnomics.progress_bar_dimensions = FALSE,
    rdbnomics.progress_bar_series = FALSE
  )
  opts <- append(
    opts,
    list(rdbnomics = sort(gsub("rdbnomics\\.", "", names(opts)))),
    0
  )

  op <- options()
  op.rdbnomics <- opts
  toset <- !(names(op.rdbnomics) %in% names(op))
  if (any(toset)) options(op.rdbnomics[toset])
  invisible()
}
