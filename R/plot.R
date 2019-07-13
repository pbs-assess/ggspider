#' Make a spider web plot
#'
#' @param df A data.frame representing groups and spokes for the plot (see examples)
#' @param grp_col The name of the column used for groups
#' @param spk_col The name of the column used for spokes
#' @param value_col The name of the column used for the values of each group along the spokes
#' @param spoke_color Single value for color of the spokes
#' @param spoke_lty Single integer value for the line type of the spokes
#' @param ref_lines_val A vector of reference line values to plot
#' @param ref_lines_color A vector of reference line colors
#' @param ref_lines_type vector of reference line types
#' @param ref_label_color A vector of reference label colors
#' @param ref_lines_label_spoke A vector of reference label locations (spoke number). A value of
#'  zero means that the label will not be printed
#' @param diff_lty_by_name This is an optional string which will be used to [grep()] the names
#'  of the values given in column grp_col. If they match, the diff_lty value will be used for
#'  line type for those lines
#' @param diff_lty Line type to use for grp_col values that diff_lty_by_name matches with [grep()]
#' @param palette As defined in [ggplot2::scale_color_brewer()]
#' @param show_legend Show legend?
#' @param leg_main_title Main legend title
#' @param leg_lty_title Linetype legend title
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_segment geom_text coord_equal
#' @importFrom ggplot2 labs theme scale_color_brewer element_blank
#'
#' @importFrom tidyr spread
#' @importFrom dplyr mutate select left_join
#' @importFrom magrittr "%>%"
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' df <- data.frame(
#'   nm = c(
#'     "A", "B", "C",
#'     "A", "B", "C",
#'     "A", "B", "C",
#'     "A", "B", "C",
#'     "A", "B", "C"
#'   ),
#'   spk = c(
#'     "P1", "P1", "P1",
#'     "P2", "P2", "P2",
#'     "P3", "P3", "P3",
#'     "P4", "P4", "P4",
#'     "P5", "P5", "P5"
#'   ),
#'   value = c(
#'     .1, .2, .3,
#'     .4, .5, .6,
#'     .7, .8, .9,
#'     .10, .11, .12,
#'     .13, .14, .15
#'   )
#' )
#' spider_web(df,
#'   ref_lines_label_spoke = c(4, 3, 0),
#'   ref_lines_type = c(2, 3, 4),
#'   ref_lines_color = c("black", "royalblue", "salmon")
#' )
spider_web <- function(df,
                       grp_col = "nm",
                       spk_col = "spk",
                       value_col = "value",
                       spoke_color = "grey75",
                       spoke_lty = 1,
                       ref_lines_val = c(0.5, 0.75, 1),
                       ref_lines_color = c("grey75", "grey75", "grey75"),
                       ref_lines_type = c(1, 1, 1),
                       ref_label_color = c("grey50", "grey50", "grey50"),
                       ref_lines_label_spoke = c(
                         length(unique(df[, spk_col, drop = TRUE])),
                         length(unique(df[, spk_col, drop = TRUE])),
                         0
                       ),
                       diff_lty_by_name = "ref",
                       diff_lty = 2,
                       palette = "Set2",
                       show_legend = TRUE,
                       leg_main_title = "Legend",
                       leg_lty_title = "Linetype") {
  if (!any("data.frame" %in% class(df)) || length(df) < 3 || nrow(df) < 1) {
    stop("Argument 'df' must be a data frame with at least one row and three columns")
  }
  if (class(grp_col) != "character" | length(grp_col) != 1 | !grp_col %in% names(df)) {
    stop("grp_col is either of the wrong type or is not a column of df",
      call. = FALSE
    )
  }
  if (class(spk_col) != "character" | length(spk_col) != 1 | !spk_col %in% names(df)) {
    stop("spk_col is either of the wrong type or is not a column of df",
      call. = FALSE
    )
  }
  if (class(value_col) != "character" | length(value_col) != 1 | !value_col %in% names(df)) {
    stop("value_col is either of the wrong type or is not a column of df",
      call. = FALSE
    )
  }
  df <- df %>% select(grp_col, spk_col, value_col)

  if (class(spoke_color) != "character" & class(spoke_color) != "numeric") {
    stop("spoke_color must be a vector of character values (color names) or numeric values representing colors",
      call. = FALSE
    )
  }
  if ((class(spoke_lty) != "integer" & class(spoke_lty) != "numeric") | length(spoke_lty) != 1) {
    stop("spoke_lty must be a single numeric value",
      call. = FALSE
    )
  }
  if (class(ref_lines_val) != "numeric") {
    stop("ref_lines_val must be a vector of numeric values",
      call. = FALSE
    )
  }
  if (class(ref_lines_color) != "character" & class(ref_lines_color) != "numeric") {
    stop("ref_lines_color must be a vector of character values (color names) or numeric values representing colors",
      call. = FALSE
    )
  }
  if (class(ref_lines_type) != "numeric") {
    stop("ref_lines_type must be a vector of numeric values",
      call. = FALSE
    )
  }
  if (class(ref_label_color) != "character" & class(ref_label_color) != "numeric") {
    stop("ref_label_color must be a vector of character values (color names) or numeric values representing colors",
      call. = FALSE
    )
  }
  if (class(ref_lines_label_spoke) != "integer" & class(ref_lines_label_spoke) != "numeric") {
    stop("ref_lines_label_spoke must be a vector of numeric values",
      call. = FALSE
    )
  }
  if (class(show_legend) != "logical" | length(show_legend) > 1) {
    stop("show_legend must be a single TRUE or FALSE value",
      call. = FALSE
    )
  }

  if (length(ref_lines_val) != length(ref_lines_color) |
    length(ref_lines_color) != length(ref_lines_type) |
    length(ref_lines_type) != length(ref_label_color) |
    length(ref_label_color) != length(ref_lines_label_spoke)) {
    stop("ref_lines_val, ref_lines_color, ref_lines_type, ref_label_color, and ref_lines_label_spoke must all be the same length",
      call. = FALSE
    )
  }

  grp_nms <- as.character(unique(df[, grp_col, drop = TRUE]))
  spk_nms <- as.character(unique(df[, spk_col, drop = TRUE]))
  spokes <- calc_spokes(length(spk_nms))
  if (any(ref_lines_label_spoke > nrow(spokes))) {
    stop("ref_lines_label_spoke cannot have a value greater than the number of spokes, ", nrow(spokes),
      call. = FALSE
    )
  }
  spokes$spk_nms <- spk_nms
  ds <- tidyr::spread(df, key = spk_col, value = value_col)
  spider_data <- calc_web(ds)
  ref_lines_attr <- data.frame(
    line_num = 1:length(ref_lines_val),
    x = ref_lines_val,
    color = ref_lines_color,
    type = ref_lines_type
  )

  # ref lines calculations
  ref_lines_data <- data.frame()
  for (i in seq_along(ref_lines_val)) {
    k <- rbind(spokes, spokes[1, ]) %>%
      dplyr::select(-c(x, y, spk_nms)) %>%
      dplyr::mutate(
        line_num = ref_lines_attr[i, ]$line_num,
        x = xend * ref_lines_attr[i, ]$x,
        y = yend * ref_lines_attr[i, ]$x,
        color = ref_lines_attr[i, ]$color,
        lty = ref_lines_attr[i, ]$type
      )
    ref_lines_data <- rbind(ref_lines_data, k)
  }

  ref_lines_to_label <- ref_lines_attr$x[as.logical(ref_lines_label_spoke)]
  ref_label_color <- ref_label_color[as.logical(ref_lines_label_spoke)]
  if (length(ref_lines_to_label)) {
    label_data <- data.frame(x = 0, y = ref_lines_to_label)
    label_data <- label_data %>% dplyr::mutate(
      label = y,
      color = ref_label_color
    )
    label_spokes <- spokes[ref_lines_label_spoke, ]
    theta <- atan2(label_spokes$yend, label_spokes$xend)
    label_data$x <- label_data$y * cos(theta)
    label_data$y <- label_data$y * sin(theta)
  }

  # diff_lty and diff_lty_by_name calculations
  if (diff_lty_by_name != "") {
    lty <- rep("default", length(grp_nms))
    # lty <- rep(1, length(grp_nms))
    diff_mtch <- grep(diff_lty_by_name, grp_nms)
    lty[diff_mtch] <- diff_lty_by_name
    # lty[diff_mtch] <- diff_lty
    lty_df <- data.frame(group = grp_nms, lty = lty)
    spider_data <- spider_data %>%
      left_join(lty_df, by = "group")
  }

  # spokes are off by one
  spokes$spk_nms <- c(tail(spokes$spk_nms, -1), head(spokes$spk_nms, 1))

  g <- spider_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = spokes,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      color = spoke_color,
      lty = spoke_lty
    ) +
    geom_path(
      data = ref_lines_data,
      aes(x,
        y,
        group = line_num
      ),
      color = ref_lines_data$color,
      lty = ref_lines_data$lty,
      inherit.aes = FALSE
    ) +
    geom_path(aes(
      color = as.factor(group),
      linetype = as.factor(lty)
    ),
    lwd = 0.8
    ) +
    coord_equal() +
    geom_text(
      data = spokes,
      aes(
        x = xend * 1.1,
        y = yend * 1.1,
        label = spk_nms
      ),
      colour = "grey30"
    ) +
    gfplot::theme_pbs() +
    scale_color_brewer(name = leg_main_title, palette = palette, guide = "legend") +
    scale_linetype_discrete(guide = FALSE) +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    ) +
    theme(legend.position = "right")

  if (length(ref_lines_to_label)) {
    g <- g + geom_text(
      data = label_data,
      aes(x = x, y = y, label = label), color = ref_label_color,
      nudge_y = 0.04, hjust = 0, nudge_x = 0.01
    )
  }
  if (!show_legend) {
    g <- g + theme(legend.position = "none")
  }
  g <- g + labs(linetype = leg_lty_title)

  g
}
