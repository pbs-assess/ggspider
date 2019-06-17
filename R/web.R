#' Title
#'
#' @param mydf A data frame. The first column holds the names for the 'web' data.
#' The remaining columns are the names of the 'spokes'.
#'
#' @return A data.frame in 'long' format with columns 'group', 'x', and 'y' where group
#'  holds the names of the 'web' data groups, with 'x' and 'y' representing points on each
#'  spoke, with each group having number of rows = number of spokes plus one (the first one
#'  repeats at the end so that plot lines will connect).
#'
#' @examples
#' df <- data.frame(nm = c("A", "B", "C",
#'   "A", "B", "C",
#'   "A", "B", "C",
#'   "A", "B", "C",
#'   "A", "B", "C"),
#' spk = c("P1", "P1", "P1",
#'         "P2", "P2", "P2",
#'         "P3", "P3", "P3",
#'         "P4", "P4", "P4",
#'         "P5", "P5", "P5"),
#' value = c(.1, .2, .3,
#'           .4, .5, .6,
#'           .7, .8, .9,
#'           .10, .11, .12,
#'           .13, .14, .15))
#'ds <- tidyr::spread(df, key = "spk", value = "value")
#'web <- calc_web(ds)
calc_web <- function(mydf){
  if(missing(mydf)){
    stop("Argument 'mydf' is required",
         call. = FALSE)
  }
  if(is.na(mydf) || class(mydf) != "data.frame" || length(mydf) < 2 || nrow(mydf) < 1){
    stop("Argument 'mydf' must be a data frame with at least one row and two columns")
  }
  df <- cbind(mydf[, -1], mydf[,2])
  myvec <- c(t(df))
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / (ncol(df) - 1))
  xx <- myvec * sin(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  yy <- myvec * cos(rep(c(angles[-ncol(df)], angles[1]), nrow(df)))
  data.frame(group = rep(as.character(mydf[, 1, drop = TRUE]),
                         each = ncol(mydf)),
             x = xx,
             y = yy)
}