#' Calculate the 'spokes' of the spider plot
#'
#' @param num The number of spokes
#'
#' @return A data frame with num rows and columns x, y, xend, and yend representing the x-y
#' start and end coordinates of the spokes
#'
#' @examples
#' \dontrun{
#' calc_spokes(5)
#' }
calc_spokes <- function(num) {
  if (missing(num)) {
    stop("Argument 'num' is required",
      call. = FALSE
    )
  }
  if ((class(num) != "numeric" && class(num) != "integer") || length(num) > 1 || is.na(num) || num < 1) {
    stop("Argument 'num' must be a single positive integer")
  }
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / num)
  data.frame(x = 0, y = 0, xend = sin(angles[-1]), yend = cos(angles[-1]))
}
