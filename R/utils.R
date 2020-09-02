# `%notin%`
`%notin%` <- function (x, table) {
  !(x %in% table)
}

#-------------------------------------------------------------------------------
# get_data
get_data <- function(x, userl, curl_args, headers = NULL, opt = NULL, run = 0) {
  if (run > 0) {
    sys_sleep <- getOption("rdbnomics.sleep_run")
    check_argument(sys_sleep, c("integer", "numeric"))
    Sys.sleep(sys_sleep)
  }

  tryCatch({
    if (userl) {
      # Only readLines
      if (as.numeric(R.Version()$major) == 3) {
        if (as.numeric(R.Version()$minor) < 2) {
          try(
            suppressMessages(suppressWarnings(utils::setInternet2(TRUE))),
            silent = TRUE
          )
        }
      }

      verb_warn_rl <- getOption("rdbnomics.verbose_warning_readLines")
      check_argument(verb_warn_rl, "logical")
      if (verb_warn_rl) {
        response <- try(readLines(x), silent = TRUE)
      } else {
        response <- try(suppressWarnings(readLines(x)), silent = TRUE)
      }

      if (inherits(response, "try-error")) {
        stop("BAD REQUEST", call. = FALSE)
      } else {
        jsonlite::fromJSON(response)
      }
    } else {
      # With curl
      if (!is.null(curl_args)) {
        if (inherits(curl_args, "curl_handle")) {
          curl_args <- list(handle = curl_args)
        }
        if (!inherits(curl_args, "list")) {
          stop(
            paste0(
              "Argument 'curl_config' or option 'rdbnomics.curl_config' can ",
              "only be of class 'curl_handle' or 'list'."
            ),
            call. = FALSE
          )
        }
        if (inherits(curl_args, "list")) {
          if (is.null(names(curl_args))) {
            stop("The list 'curl_config' must be named.", call. = FALSE)
          }
          if (length(curl_args) <= 0) {
            stop("The list 'curl_config' is empty.", call. = FALSE)
          }
          nm <- names(curl_args)
          nm <- no_empty_char(nm)
          if (length(curl_args) != length(nm)) {
            stop("All elements of 'curl_config' must be named.", call. = FALSE)
          }
        }
      }

      try(curl::handle_reset(tmp_curl), silent = TRUE)
      if (!list_has_curl_handle(curl_args)) {
        tmp_curl <- list(handle = curl::new_handle())
        if (!is.null(curl_args)) {
          curl::handle_setopt(tmp_curl$handle, .list = curl_args)
        }
      } else {
        tmp_curl <- curl_args
      }

      if (!is.null(headers) & !is.null(opt)) {
        curl::handle_setheaders(tmp_curl$handle, .list = headers)
        curl::handle_setopt(tmp_curl$handle, .list = opt)
      }

      response <- do.call(curl::curl_fetch_memory, c(list(url = x), tmp_curl))
      check_x <- curl::parse_headers(response$headers)
      check_x <- utils::head(check_x, 1)

      http_ok <- getOption("rdbnomics.http_ok")
      check_argument(http_ok, "character")
      if (grepl(http_ok, toupper(check_x))) {
        response <- rawToChar(response$content)

        if (!is.null(jsonlite::fromJSON(response)$errors)) {
          cat("\n")
          for (ie in 1:nrow(jsonlite::fromJSON(response)$errors)) {
            cat(
              jsonlite::fromJSON(response)$errors[ie,]$message, " : ",
              jsonlite::fromJSON(response)$errors[ie,]$provider_code, "/",
              jsonlite::fromJSON(response)$errors[ie,]$dataset_code, "/",
              jsonlite::fromJSON(response)$errors[ie,]$series_code,
              "\n"
            )
          }
        }

        jsonnum <- try(
          jsonlite::fromJSON(response)$series$num_found,
          silent = TRUE
        )
        if (!inherits(jsonnum, "try-error")) {
          if (!is.null(jsonnum)) {
            if (jsonlite::fromJSON(response)$series$num_found <= 0) {
              run <- 100
              stop(
                "Error when fetching the data.",
                call. = FALSE
              )
            }
          }
        }

        jsonlite::fromJSON(response)
      } else {
        errormessage <- try(
          paste0(
            "\n", check_x,
            "\n", jsonlite::fromJSON(rawToChar(response$content))$error
          ),
          silent = TRUE
        )
        if (inherits(errormessage, "try-error")) {
          errormessage <- paste0("\n", check_x)
        }
        stop(errormessage, call. = FALSE)
      }
    }
  }, error = function(e) {
    try_run <- getOption("rdbnomics.try_run")
    check_argument(try_run, c("integer", "numeric"))

    myerror <- try(
      grepl("'curl_config'", e$message) |
      grepl("BAD[[:blank:]]+REQUEST", toupper(e$message)),
      silent = TRUE
    )
    if (!inherits(myerror, "try-error")) {
      if (myerror) {
        try_run <- -1L
      }
    }

    if (run < try_run) {
      get_data(x, userl, curl_args, headers = headers, opt = opt, run = run + 1)
    } else {
      stop(e)
    }
  })
}

#-------------------------------------------------------------------------------
# deploy
deploy <- function(DT, columns = NULL, reference_column = "value") {
  if (!data.table::is.data.table(DT)) {
    stop("DT is not a data.table.", call. = FALSE)
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
        for (iv in names(y)) {
          v <- y[[iv]]
          if (inherits(v, "list")) {
            v <- unlist(v)
            if (length(v) == 1 & to_list_length != 1) {
              # New col
              y[[iv]] <- paste0(trim(v), ",")
            } else if (
              (length(v) == to_list_length + 1) |
              (length(v) == 2 & to_list_length != 2)
            ) {
              # New col
              y[[iv]] <- paste0(trim(v[1]), ",", utils::tail(v, -1))
            } else if (length(v) != to_list_length) {
              y[[iv]] <- paste(unique(v), collapse = ",")
            } else {
              y[[iv]] <- v
            }
          }
        }
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
  if (inherits(x, "character")) {
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
    stop("Can't find the version.", call. = FALSE)
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
  check_argument(versions, c("integer", "numeric"), len = FALSE)

  name <- deparse(substitute(versions))
  if (is.null(versions)) {
    stop(paste0(name, " cannot be NULL."), call. = FALSE)
  }
  if (!inherits(versions, c("numeric", "integer"))) {
    stop(
      paste0(name, " must be of class 'integer' or 'numeric'."),
      call. = FALSE
    )
  }
  if (length(versions) <= 0) {
    stop(paste0(name, " must be of length greater than 0."), call. = FALSE)
  }

  if (x %notin% versions) {
    stop(
      paste0(
        "Only versions ", paste0(versions, collapse = ", "), " are supported."
      ),
      call. = FALSE
    )
  }
  invisible()
}

#-------------------------------------------------------------------------------
# trim
trim <- function(x) {
  gsub("^[[:blank:]]+|[[:blank:]]+$", "", x)
}

#-------------------------------------------------------------------------------
# date_format
date_format <- function(x) {
  x <- no_empty_char(x)
  if (length(x) <= 0) {
    return(FALSE)
  }
  sum(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trim(x)), na.rm = TRUE) == length(x)
}

#-------------------------------------------------------------------------------
# timestamp_format
timestamp_format <- function(x, y) {
  x <- no_empty_char(x)
  if (length(x) <= 0) {
    return(FALSE)
  }
  sum(grepl(y, trim(x)), na.rm = TRUE) == length(x)
}

#-------------------------------------------------------------------------------
# check_argument
check_argument <- function(x, type, len = TRUE, n = 1, not_null = TRUE) {
  name <- deparse(substitute(x))
  if (not_null) {
    if (is.null(x)) { stop(paste0(name, " cannot be NULL."), call. = FALSE) }
  }
  if (!inherits(x, type)) {
    stop(
      paste0(
        name, " must be of class '", paste0(type, collapse = "', '"), "'."
      ),
      call. = FALSE
    )
  }
  if (len) {
    if (length(x) != n) {
      stop(paste0(name, " must be of length ", n, "."), call. = FALSE)
    }
  }
  invisible()
}

#-------------------------------------------------------------------------------
# to_json_if_list
to_json_if_list <- function(x) {
  if (inherits(x, "list")) {
    if (is.null(names(x))) {
      stop("The list 'dimensions' must be named.", call. = FALSE)
    }
    if (length(x) <= 0) {
      stop("The list 'dimensions' is empty.", call. = FALSE)
    }
    nm <- names(x)
    nm <- no_empty_char(nm)
    if (length(x) != length(nm)) {
      stop("All elements of 'dimensions' must be named.", call. = FALSE)
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

  from_timestamp <- c(
    # "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.?[0-9]*Z$",
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[[:blank:]]+[0-9]{2}:[0-9]{2}:[0-9]{2}$"
  )

  to_timestamp <- c(
    # "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d %H:%M:%S"
  )

  cols <- copy(colnames(DT))
  cols <- cols[
    (tolower(cols) != "observations_attributes") &
    !grepl("name$", tolower(cols))
  ]

  if (length(cols) > 0) {
    DT[
      ,
      (cols) := lapply(.SD, function(x) {
        if (inherits(x, "character")) {
          if (date_format(x)) {
            return(suppressWarnings(as.Date(x)))
          }
          for (i in seq_along(from_timestamp)) {
            if (timestamp_format(x, from_timestamp[i])) {
              return(
                suppressWarnings(
                  as.POSIXct(x, tz = timezone, format = to_timestamp[i])
                )
              )
            }
          }
        }
        x
      }),
      .SDcols = cols
    ]
  }

  invisible(DT)
}

#-------------------------------------------------------------------------------
# avoid_partial_argument
avoid_partial_argument <- function(x) {
  x <- as.list(x)
  x <- names(x)
  x <- no_empty_char(x)
  if (is.null(x)) {
    return(invisible())
  }
  if (length(x) <= 0) {
    return(invisible())
  }

  args_ok <- c(names(formals(rdb)), names(formals(rdb_by_api_link)))
  args_ok <- args_ok[args_ok %notin% c("...", "api_link")]
  if (length(setdiff(x, args_ok)) > 0) {
    stop("Please avoid partial argument matching.", call. = FALSE)
  }
  invisible()
}

#-------------------------------------------------------------------------------
# correct_argument
correct_argument <- function() {
  args_ok <- c(names(formals(rdb)), names(formals(rdb_by_api_link)))
  args_ok[args_ok %notin% c("...", "api_link")]
}

#-------------------------------------------------------------------------------
# call_ok
call_ok <- function(x) {
  x <- as.list(x)
  x <- names(x)

  modif_arg <- FALSE
  if (is.null(x)) {
    modif_arg <- TRUE
  }
  if (!is.null(x)) {
    x <- no_empty_char(x)
    if (length(x) <= 0) {
      modif_arg <- TRUE
    } else {
      x <- pmatch(x, correct_argument(), duplicates.ok = TRUE)
      if (sum(is.na(x)) <= 0) {
        modif_arg <- TRUE
      }
    }
  }

  modif_arg
}

#-------------------------------------------------------------------------------
# remove_columns
remove_columns <- function(DT, x, expr = FALSE) {
  if (expr) {
    cols <- grep(x, colnames(DT), value = TRUE)
  } else {
    cols <- intersect(x, colnames(DT))
  }
  if (length(cols) > 0) {
    DT[, (cols) := NULL]
  }
  invisible(DT)
}

#-------------------------------------------------------------------------------
# reduce_to_one
reduce_to_one <- function(DT) {
  x <- DT[
    ,
    lapply(.SD, function(y) {
      length(unique(y))
    })
  ]
  x <- as.list(x)
  x <- Filter(function(y){ y > 1 }, x)
  x <- names(x)
  DT[, (x) := NULL]
  invisible(DT)
}

#-------------------------------------------------------------------------------
# filter_type
filter_type <- function(x) {
  test <- tryCatch({
    res <- "ko"
    n <- length(x)
    y <- sapply(x, filter_ok)
    y <- sum(y, na.rm = TRUE)
    if (y == n) {
      res <- "list"
    }
    res
  }, error = function(e) {
    "ko"
  })

  if (test == "ko") {
    test <- tryCatch({
      res <- filter_ok(x)
      if (res) {
        res <- "notlist"
      } else {
        res <- "ko"
      }
      res
    }, error = function(e) {
      "ko"
    })
  }

  test
}

#-------------------------------------------------------------------------------
# filter_ok
filter_ok <- function(x) {
  tryCatch({
    res <- FALSE
    nm1 <- names(x)
    nm2 <- names(x$parameters)
    if (identical(nm1, c("code", "parameters"))) {
      if (is.null(nm2)) {
        res <- TRUE
      }
      if (identical(nm2, c("frequency", "method"))) {
        res <- TRUE
      }
    }
    res
  }, error = function(e) {
    FALSE
  })
}

#-------------------------------------------------------------------------------
# get_geo_colname
get_geo_colname <- function(x) {
  y <- try(
    {
      expr <- "dimensions_label[s]*\\."

      elt <- grep(expr, names(unlist(x)), value = TRUE)
      if (is.null(x$dataset$code)) {
        codes <- gsub(".*/|\\..*", "", elt)
      } else {
        codes <- x$dataset$code
        if (length(codes) == 1) {
          codes <- rep(codes, length(elt))
        }
      }

      lapply(seq_along(elt), function(i) {
        z <- elt[i]

        ref_elt <- gsub(".*\\.", "", z)
        elt_ <- gsub('\\.', '"]][["', z)
        elt_ <- paste0('[["', elt_, '"]]')
        
        res <- c(codes[i], ref_elt, eval(parse(text = paste0("x", elt_))))
        if (length(res) > 0) {
          if (res[2] == res[3]) {
            if (res[3] == capital_first(res[3])) {
              res[3] <- toupper(res[3])
            } else {
              res[3] <- capital_first(res[3])
            }
          }
          res
        } else {
          res
        }
      })
    },
    silent = TRUE
  )
  if (inherits(y, "try-error")) {
    return(NULL)
  }
  y
}

#-------------------------------------------------------------------------------
# get_geo_names
get_geo_names <- function(x, colname) {
  y <- try(
    {
      codes <- sapply(colname, `[[`, 1)
      nm <- sapply(colname, `[[`, 2)

      DTs <- lapply(seq_along(codes), function(i) {
        expr <- paste0(
          "(", paste0(codes[i], collapse = "|"), ")*",
          "\\.dimensions_value[s]*_label[s]*\\.(",
          paste0(nm[i], collapse = "|"),
          "){1}\\."
        )

        elt <- grep(expr, names(unlist(x)), value = TRUE)
        for (y in nm) {
          elt <- gsub(paste0(y, '\\..*'), y, elt)
        }
        elt <- unique(elt)

        if (length(elt) > 0) {
          elt_ <- gsub('\\.', '"]][["', elt)
          elt_ <- paste0('[["', elt_, '"]]')
          y <- eval(parse(text = paste0("x", elt_)))
          suppressWarnings(
            setnames(
              data.table(X1 = codes[i], X2 = names(y), X3 = unname(unlist(y))),
              c("dataset_code", colname[[i]][2:3])
            )[]
          )
        } else {
          expr <- paste0(
            "(", paste0(codes[i], collapse = "|"), ")*",
            "\\.dimensions_value[s]*_label[s]*\\.(",
            paste0(nm[i], collapse = "|"),
            "){1}[0-9]*"
          )

          elt <- grep(expr, names(unlist(x)), value = TRUE)
          for (y in nm) {
            elt <- gsub(paste0(y, '\\..*'), y, elt)
          }
          elt <- gsub("[0-9]*$", "", elt)
          elt <- unique(elt)

          if (length(elt) > 0) {
            elt_ <- gsub('\\.', '"]][["', elt)
            elt_ <- paste0('[["', elt_, '"]]')
            y <- eval(parse(text = paste0("x", elt_)))
            suppressWarnings(
              setnames(
                data.table(X1 = codes[i], X2 = y[, 1], X3 = y[, 2]),
                c("dataset_code", colname[[i]][2:3])
              )[]
            )
          } else {
            NULL
          }
        }
      })
      DTs <- Filter(Negate(is.null), DTs)
      if (length(DTs) <= 0) {
        NULL
      } else {
        DTs
      }
    },
    silent = TRUE
  )
  if (inherits(y, "try-error")) {
    return(NULL)
  }
  y
}

#-------------------------------------------------------------------------------
# list_has_curl_handle
list_has_curl_handle <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (!inherits(x, "list")) {
    return(FALSE)
  }
  y <- sapply(x, inherits, what = "curl_handle")
  y <- sum(y, na.rm = TRUE)
  y <- y > 0
  if (y) {
    TRUE
  } else {
    FALSE
  }
}

#-------------------------------------------------------------------------------
# unpack
unpack <- function(DT) {
  if (is.null(DT)) {
    return(NULL)
  }
  if (is.list(DT) & !data.table::is.data.table(DT)) {
    if (length(DT) <= 0) {
      return(NULL)
    }
  }
  if (nrow(DT) <= 0) {
    return(NULL)
  }
  data.table::setDT(DT)
  DT[, k := .I]
  DT <- split(DT, by = "k")
  lapply(DT, function(DT_) {
    if (!("children" %in% colnames(DT_))) {
      DT_[, k := NULL]
      DT_[]
    } else {
      if (is.null(DT_$children[[1]])) {
        DT_[, k := NULL]
        DT_[, .SD, .SDcols = setdiff(colnames(DT_), "children")]
      } else {
        lapply(DT_$children, unpack)
      }
    }
  })
}

#-------------------------------------------------------------------------------
# rbindlist_recursive
rbindlist_recursive <- function(l) {
  if (is.null(l)) {
    return(NULL)
  }
  if (data.table::is.data.table(l)) {
    return(l)
  }
  if (
    sum(sapply(l, is.data.table), na.rm = TRUE) != length(l)
  ) {
    l <- lapply(l, rbindlist_recursive)
  }
  data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
}

#-------------------------------------------------------------------------------
# check_datasets
check_datasets <- function(l, run = 1) {
  if (is.null(l)) {
    return(l)
  }

  if (run == 0) {
    return(l)
  }

  for (i1 in names(l)) {
    if (is.null(l[[i1]])) {
      l[[i1]] <- NULL
    } else {
      if (length(l[[i1]]) <= 0) {
        l[[i1]] <- NULL
      } else {
        if (nrow(l[[i1]]) <= 0) {
          l[[i1]] <- NULL
        }
      }
    }
  }

  check_datasets(l, run = run - 1)
}

#-------------------------------------------------------------------------------
# check_dimensions
check_dimensions <- function(l, run = 1) {
  if (is.null(l)) {
    return(l)
  }
  
  if (run == 0) {
    return(l)
  }

  for (i1 in names(l)) {
    if (length(l[[i1]]) <= 0) {
      l[[i1]] <- NULL
    } else {
      for (i2 in names(l[[i1]])) {
        if (is.null(l[[i1]][[i2]])) {
          l[[i1]][[i2]] <- NULL
        } else {
          if (length(l[[i1]][[i2]]) <= 0) {
            l[[i1]][[i2]] <- NULL
          }
        }
      }
    }
  }

  check_dimensions(l, run = run - 1)
}

#-------------------------------------------------------------------------------
# capital_first
capital_first <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (length(x) <= 0) {
    return(x)
  }

  paste0(
    toupper(substr(x, 1, 1)),
    tolower(substr(x, 2, nchar(x)))
  )
}

#-------------------------------------------------------------------------------
# new_title
new_title <- function(x) {
  if (is.null(x)) {
    return("unknown")
  }

  if (length(x) <= 0) {
    return("unknown")
  }

  if (x != capital_first(x)) {
    return(capital_first(x))
  }

  toupper(x)
}

#-------------------------------------------------------------------------------
# get_dimensions_values
get_dimensions_values <- function(DT, ...) {
  tryCatch({
    tmpDT <- DT[, .SD, .SDcols = c("provider_code", "dataset_code")]
    tmpDT <- unique(tmpDT)
    tmpDT[, k := .I]
    tmpDT <- lapply(tmpDT$k, function(x) {
      tmpx <- rdb_dimensions(
        provider_code = tmpDT[k == x]$provider_code,
        dataset_code = tmpDT[k == x]$dataset_code,
        ...
      )
      tmpx <- tmpx[[1]][[1]]
      lapply(tmpx, function(y) {  
        DT2 <- data.table::copy(y)
        DT2[, dataset_code := tmpDT[k == x]$dataset_code]
        data.table::setcolorder(
          DT2,
          c("dataset_code", setdiff(colnames(DT2), "dataset_code"))
        )
        DT2
      })
    })
    tmpDT <- unlist(tmpDT, recursive = FALSE)  
    unname(tmpDT)
  }, error = function(e) {
    NULL
  })
}

#-------------------------------------------------------------------------------
# get_dimensions_names
get_dimensions_names <- function(DT = NULL, dimensions_values = NULL, ...) {
  tryCatch({
    if (is.null(dimensions_values)) {
      tmpDT <- get_dimensions_values(DT, ...)
    } else {
      tmpDT <- dimensions_values
    }
    lapply(tmpDT, function(x) {
      c(
        unique(x$dataset_code),
        utils::tail(colnames(x), -1)
      )
    })
  }, error = function(e) {
    NULL
  })
}

#-------------------------------------------------------------------------------
# JUST A TRY, DO NOT USE !
# get_geo_colname2
get_geo_colname2 <- function(x, y) {
  u <- try(
    {
      nm <- unlist(x)
      nm <- names(nm)
      nm <- grep("\\.dimensions_value[s]*_label[s]\\.", nm, value = TRUE)
      nm <- gsub("\\.dimensions_value[s]*_label[s]", "", nm)
      nm <- gsub("dataset[s]*\\.*", "", nm)
      nm <- gsub("\\.[^.]*$", "", nm)
      nm <- unique(nm)

      nm <- lapply(nm, function(u) {
        pc <- gsub("/.*", "", u)

        dc <- gsub(paste0(pc, "/"), "", u)
        dc <- gsub("\\..*", "", dc)
        
        code <- gsub(paste0(pc, "/", dc, "\\."), "", u)
        c(pc, dc, code, new_title(code))
      })
      nm <- lapply(nm, `[`, 2:4)

      if (!is.null(y)) {
        for (i in seq_along(nm)) {
          for (j in seq_along(y)) {
            if (nm[[i]][1] == y[[j]][1] & nm[[i]][2] == y[[j]][2]) {
              nm[[i]][3] <- y[[j]][3]
            }
          }
        }
      }
      nm
    },
    silent = TRUE
  )
  if (inherits(u, "try-error")) {
    return(y)
  }
  u
}

#-------------------------------------------------------------------------------
# ellipsis_default
ellipsis_default <- function(name, x, default) {
  if (length(x) <= 0) {
    return(default)
  }
  tmp <- x[[name]]
  if (is.null(tmp)) {
    return(default)
  }
  tmp
}

#-------------------------------------------------------------------------------
# additional_info
additional_info <- function(x) {
  # Additional informations to translate geo, freq, ...
  if (!getOption("rdbnomics.translate_codes")) {
    additional_geo_column <- additional_geo_mapping <- NULL
  } else {
    additional_geo_column <- get_geo_colname(x)
    additional_geo_mapping <- get_geo_names(x, additional_geo_column)
    # Check coherence
    if (is.null(additional_geo_column) | is.null(additional_geo_mapping)) {
      additional_geo_column <- additional_geo_mapping <- NULL
    }
    if (!is.null(additional_geo_column) & !is.null(additional_geo_mapping)) {
      if (length(additional_geo_column) != length(additional_geo_mapping)) {
        if (
          length(additional_geo_column) == 0 |
          length(additional_geo_mapping) == 0
        ) {
          additional_geo_column <- additional_geo_mapping <- NULL
        } else {
          check_agc <- sapply(additional_geo_column, paste0, collapse = "|")
          additional_geo_column <- stats::setNames(additional_geo_column, check_agc)

          check_agm <- sapply(additional_geo_mapping, function(u) {
            u1 <- u$dataset_code[1]
            u2 <- colnames(u)[2:3]
            u2 <- paste0(u2, collapse = "|")
            paste0(u1, "|", u2)
          })
          additional_geo_mapping <- stats::setNames(additional_geo_mapping, check_agm)

          keep <- intersect(check_agc, check_agm)

          if (length(keep) == 0) {
            additional_geo_column <- additional_geo_mapping <- NULL
          } else {
            additional_geo_column <- additional_geo_column[sort(keep)]
            additional_geo_mapping <- additional_geo_mapping[sort(keep)]
          }
        }
      }
    }
  }

  list(additional_geo_column, additional_geo_mapping)
}
