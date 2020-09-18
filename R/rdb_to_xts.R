#' Transform the data.table object into a xts object
#'
#' For some analysis, it is more convenient to have a \code{xts} object
#' instead of a \code{data.table} object.
#' 
#' @param x \code{data.table}. The \code{data.table} returned by the \code{rdb}
#' function.
#' @param needed_columns Vector of character strings (default
#' \code{c("period", "series_code", "series_name", "value")}). Vector of column
#' names which are needed to transform the \code{data.table} into a \code{xts}
#' object.
#' @param series_columns Vector of character strings (default
#' \code{c("series_code", "series_name")}). Vector of series column
#' names.
#' @return A \code{xts} object.
#' @examples
#' \dontrun{
#' library(xts)
#' library(data.table)
#' library(rdbnomics)
#' 
#' df <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
#' rdb_to_xts(df)
#' }
#' @seealso \code{\link{rdb}}, \code{\link{rdb_rename_xts}}
#' @author Sebastien Galais
#' @export
rdb_to_xts <- function(
  x,
  needed_columns = c("period", "series_code", "series_name", "value"),
  series_columns = c("series_code", "series_name")
) {
  code <- paste(
    "  xts_ok <- try(utils::packageVersion('xts'), silent = TRUE)",
    "  if (inherits(xts_ok, 'try-error')) {",
    "    stop(",
    "      'Please install the package xts to use rdb_to_xts().',",
    "      call. = FALSE",
    "    )",
    "  }",
    "  if (is.null(x)) {",
    "    return(NULL)",
    "  }",
    "  all_cols <- length(setdiff(needed_columns, colnames(x))) != 0",
    "  if (all_cols) {",
    "    stop(",
    "      paste0(",
    "        'To export as a xts object, some columns are missing. Needed columns ',",
    "        'are \u0022', paste0(needed_columns, collapse = '\u0022, \u0022'),",
    "        '\u0022'",
    "      ),",
    "      call. = FALSE",
    "    )",
    "  }",
    "  x <- x[, .SD, .SDcols = needed_columns]",
    "  data.table::setcolorder(x, needed_columns)",
    "  attr_names <- NULL",
    "  if (!is.null(series_columns)) {",
    "    attr_names <- unique(x[, .SD, .SDcols = series_columns])",
    "  }",
    "  if (nrow(x) > 0) {",
    "    x <- data.table::dcast.data.table(",
    "      x, period ~ series_code,",
    "      value.var = 'value'",
    "    )",
    "  } else {",
    "    orig <- Sys.Date() - as.numeric(Sys.Date())",
    "    x <- data.table(",
    "      period = as.Date(numeric(), origin = orig),",
    "      no_code = numeric()",
    "    )",
    "  }",
    "  if (!inherits(x[[1]], 'Date')) {",
    "    stop(",
    "      paste0(",
    "        'The first needed column ', needed_columns[1], ' is not of class ',",
    "        'Date which is a problem for data.table::as.xts.data.table().'",
    "      ),",
    "      '\n',",
    "      paste0(",
    "        'Please check with packageVersion(\u0022rdbnomics\u0022) that your version is ',",
    "        'greater or equal to 0.5.2. Otherwise update rdbnomics or contact ',",
    "        's915.stem@gmail.com.'",
    "      ),",
    "      call. = FALSE",
    "    )",
    "  }",
    "  x <- data.table::as.xts.data.table(x)",
    "  xts::xtsAttributes(x) <- list(codename = attr_names)",
    "  x",
    sep = "\n"
  )
  eval(parse(text = code))
}
