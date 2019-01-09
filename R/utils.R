# `%notin%`
`%notin%` <- function (x, table) {
  !(x %in% table)
}

#-------------------------------------------------------------------------------
# read_lines
read_lines <- function(x, y, run = 0) {
  if (run > 0) { Sys.sleep(getOption("rdbnomics.sleep_run")) }
  
  tryCatch({
    if (x) {
      if (as.numeric(R.Version()$major) >= 3) {
        if (as.numeric(R.Version()$minor) < 2) {
          suppressMessages(suppressWarnings(utils::setInternet2(TRUE)))
        }
      }
      
      if (getOption("rdbnomics.verbose_warning_readLines")) {
        y <- readLines(y)
      } else {
        y <- suppressWarnings(readLines(y))
      }
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
deploy <- function(DT, columns = NULL, reference_column = "value") {
  if (!data.table::is.data.table(DT)) {
    stop("DT is not a data.table.")
  }

  if (nrow(DT) <= 0) { return(DT) }
  if (ncol(DT) <= 0) { return(DT) }

  has_list <- sapply(1:ncol(DT), function(x) {
    inherits(DT[[x]], "list")
  }, USE.NAMES = FALSE)
  has_list <- sum(has_list, na.rm = TRUE)
  has_list <- (has_list > 0)

  if (has_list) {
    DT[, dotI := .I]
    DT <- split(DT, DT$dotI)
    DT <- lapply(DT, function(y) {
      y <- as.list(y)

      # Reference length
      to_list_length <- length(y[[reference_column]][[1]])

      # Transform lists into vectors
      if (is.null(columns)) {
        y <- lapply(y, function(v) {
          if (inherits(v, "list")) {
            v <- unlist(v)
            if (length(v) != to_list_length) {
              v <- paste(v, collapse = ",")
            }
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

#-------------------------------------------------------------------------------
# list_has_dataframe
list_has_dataframe <- function(x) {
  check_for_dataframe <- lapply(x, function(y) {
    y <- sapply(y, inherits, what = "data.frame", simplify = FALSE)
    unlist(y)
  })
  check_for_dataframe <- unlist(check_for_dataframe)
  check_for_dataframe <- sum(check_for_dataframe, na.rm = TRUE)
  (check_for_dataframe > 0)
}

#-------------------------------------------------------------------------------
# no_empty_char
no_empty_char <- function(x) {
  if (inherits(x, "character") & length(x) > 1) {
    x <- x[x != "" & !is.na(x)]
  }
  x
}

#-------------------------------------------------------------------------------
# dataframe_to_columns
dataframe_to_columns <- function(x) {
  has_dataframe <- sapply(x, function(y) {
    if (inherits(y, "data.frame")) { return(NULL) }
    ""
  }, simplify = FALSE)
  has_dataframe <- Filter(is.null, has_dataframe)

  if (length(has_dataframe) <= 0) { return(x) }

  for (i in names(has_dataframe)) {
    # cols <- colnames(x[[i]])
    x <- cbind(x, x[[i]])
    # names(x)[(ncol(x) - length(cols) + 1):ncol(x)] <- paste(
    #   i, cols,
    #   sep = "_"
    # )
    x[[i]] <- NULL
  }

  x
}

#-------------------------------------------------------------------------------
# get_version
get_version <- function(x) {
  if ("python_project_version" %in% names(x$`_meta`)) {
    api_version <- numeric_version(x$`_meta`$python_project_version)
  } else if ("version" %in% names(x$`_meta`)) {
    api_version <- numeric_version(x$`_meta`$version)
  } else {
    stop("Can't find the version.")
  }
  api_version <- unlist(api_version)
  api_version <- api_version[api_version != 0]
  api_version <- utils::head(api_version, 1)
  authorized_version(api_version)
  api_version
}

#-------------------------------------------------------------------------------
# authorized_version
authorized_version <- function(x) {
  versions <- getOption("rdbnomics.authorized_api_version")

  name <- deparse(substitute(versions))
  if (is.null(versions)) {
    stop(paste0(name, " cannot be NULL."))
  }
  if (!inherits(versions, c("numeric", "integer"))) {
    stop(
      paste0(name, " must be of class 'integer' or 'numeric'.")
    )
  }
  if (length(versions) <= 0) {
    stop(paste0(name, " must be of length greater than 0."))
  }
  
  if (x %notin% versions) {
    stop(
      paste0(
        "Only versions ", paste0(versions, collapse = ", "), " are supported."
      )
    )
  }
  invisible()
}

#-------------------------------------------------------------------------------
# date_format
date_format <- function(x) {
  sum(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) == length(x)
}

#-------------------------------------------------------------------------------
# timestamp_format
timestamp_format <- function(x) {
  sum(
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      x
    )
  ) == length(x)
}

#-------------------------------------------------------------------------------
# timestamp_format2
timestamp_format2 <- function(x) {
  sum(
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]+Z$",
      x
    )
  ) == length(x)
}

#-------------------------------------------------------------------------------
# stopifnot_logical
check_argument <- function(x, type, len = TRUE, n = 1, not_null = TRUE) {
  name <- deparse(substitute(x))
  if (not_null) {
    if (is.null(x)) { stop(paste0(name, " cannot be NULL.")) }
  }
  if (!inherits(x, type)) {
    stop(
      paste0(name, " must be of class '", paste0(type, collapse = "', '"), "'.")
    )
  }
  if (len) {
    if (length(x) != n) { stop(paste0(name, " must be of length ", n, ".")) }
  }
  invisible()
}

#-------------------------------------------------------------------------------
# to_json_if_list
to_json_if_list <- function(x) {
  if (inherits(x, "list")) {
    if (is.null(names(x))) { stop("The list 'dimensions' must be named.") }
    if (length(x) <= 0) { stop("The list 'dimensions' is empty.") }
    nm <- names(x)
    nm <- no_empty_char(nm)
    if (length(x) != length(nm)) {
      stop("All elements of 'dimensions' must be named.")
    }
    return(jsonlite::toJSON(x))
  }
  x
}

#-------------------------------------------------------------------------------
# transform_date_timestamp
transform_date_timestamp <- function(DT) {
  timezone <- getOption("rdbnomics.timestamp_tz")
  check_argument(timezone, "character")

  DT[
    ,
    (colnames(DT)) := lapply(.SD, function(x) {
      if (date_format(x)) {
        return(as.Date(x))
      }
      if (timestamp_format(x)) {
        return(as.POSIXct(x, tz = timezone, format = "%Y-%m-%dT%H:%M:%SZ"))
      }
      if (timestamp_format2(x)) {
        return(as.POSIXct(x, tz = timezone, format = "%Y-%m-%dT%H:%M:%OSZ"))
      }
      x
    }),
    .SDcols = colnames(DT)
  ]
  invisible()
}
