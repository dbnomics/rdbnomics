# `%notin%`
`%notin%` <- function (x, table) {
  !(x %in% table)
}

#-------------------------------------------------------------------------------
# read_lines
read_lines <- function(x, y, run = 0) {
  if (run > 0) {
    Sys.sleep(getOption("rdbnomics.sleep_run"))
  }
  
  tryCatch({
    if (x) {
      if (
        as.numeric(R.Version()$major) >= 3 &
        as.numeric(R.Version()$minor) < 2
      ) {
        suppressMessages(suppressWarnings(utils::setInternet2(TRUE)))
      }

      y <- suppressWarnings(readLines(y))
    }
    jsonlite::fromJSON(y)
  }, error = function(e) {
    if (run < getOption("rdbnomics.try_run")) {
      read_lines(x, y, run = run + 1)
    } else {
      stop(e)
    }
  })
}

#-------------------------------------------------------------------------------
# deploy
deploy <- function(DT, columns = NULL) {
  stopifnot(is.data.table(DT))

  if (nrow(DT) <= 0) {
    return(DT)
  }

  if (ncol(DT) <= 0) {
    return(DT)
  }

  has_list <- sapply(1:ncol(DT), function(x) {
    is.list(DT[[x]])
  }, USE.NAMES = FALSE)
  has_list <- sum(has_list, na.rm = TRUE)

  if (has_list) {
    DT[, dotI := .I]
    DT <- split(DT, DT$dotI)
    DT <- lapply(DT, function(y) {
      y <- as.list(y)
      if (is.null(columns)) {
        y <- lapply(y, function(v) {
          if (is.list(v)) {
            v <- unlist(v)
          }
          v
        })
      } else {
        for (i in columns) {
          y[[i]] <- unlist(y[[i]])
        }
      }
      data.table::as.data.table(y)
    })
    DT <- data.table::rbindlist(DT, use.names = TRUE, fill = TRUE)
    DT[, dotI := NULL]
  }

  DT[]
}
