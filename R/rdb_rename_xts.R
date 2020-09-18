#' Rename the xts object columns
#'
#' In the \code{xts} object returned by the function \code{rdb_to_xts}, the
#' series codes are used as column names. If you prefer the series names
#' (or apply a function to them), the function \code{rdb_rename_xts} is here for
#' that.
#' 
#' @param x \code{xts} object. The \code{xts} object returned by the function
#' \code{rdb_to_xts}.
#' @param fun function (default NULL). The function to apply to the column names.
#' @param ... Arguments for the function \code{fun}.
#' @return A \code{xts} object.
#' @examples
#' \dontrun{
#' library(xts)
#' library(data.table)
#' library(rdbnomics)
#' 
#' df <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
#' df <- rdb_to_xts(df)
#' rdb_rename_xts(df)
#' }
#' @seealso \code{\link{rdb}}, \code{\link{rdb_to_xts}}
#' @author Sebastien Galais
#' @export
rdb_rename_xts <- function(x, fun = NULL, ...) {
  code <- paste(
    "  xts_ok <- try(utils::packageVersion('xts'), silent = TRUE)",
    "  if (inherits(xts_ok, 'try-error')) {",
    "    stop(",
    "      'Please install the package xts to use rdb_to_xts().',",
    "      call. = FALSE",
    "    )",
    "  }",
    "  nm <- xts::xtsAttributes(x)$codename",
    "  cols <- nm$series_name[match(names(x), nm$series_code)]",
    "  if (is.null(fun)) {",
    "    names(x) <- cols",
    "  } else {",
    "    names(x) <- sapply(X = cols, FUN = fun, ..., USE.NAMES = FALSE)",
    "  }",
    "  x",
    sep = "\n"
  )
  eval(parse(text = code))
}
