#' DBnomics ggplot2 theme
#'
#' \code{dbnomics} is a simple ggplot2 theme for drawing nicer graphs. We do not
#' recommend to use it. It has been included in the package to avoid errors
#' when reproducing the vignette examples.
#'
#' @param ... Arguments to be passed to \code{theme}.
#' 
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(ggplot2)
#' 
#' rdb("IMF", "WEO", query = "France current account balance percent") %>%
#'   ggplot(aes(x = period, y = value, color = series_name)) +
#'   geom_line(size = 1.2) +
#'   geom_point(size = 2) +
#'   dbnomics()
#' }
#' @export
dbnomics <- function(...) {
  list(
    ggplot2::scale_x_date(expand = c(0, 0)),
    ggplot2::scale_y_continuous(labels = function(x) { format(x, big.mark = " ") }),
    ggplot2::xlab(""),
    ggplot2::ylab(""),
    ggplot2::theme_bw(),
    ggplot2::theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.title = ggplot2::element_blank()
    ),
    ggplot2::theme(...),
    ggplot2::annotate(
      geom = "text", label = "DBnomics <https://db.nomics.world>", 
      x = structure(Inf, class = "Date"), y = -Inf,
      hjust = 1.1, vjust = -0.4, col = "grey", 
      fontface = "italic"
    )
  )
}