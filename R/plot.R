#' Title
#'
#' @param df
#' @param grp_col
#' @param spk_col
#' @param value_col
#'
#' @return
#' @importFrom ggplot2 ggplot aes geom_path geom_segment geom_text coord_equal labs theme scale_color_brewer element_blank
#' @importFrom tidyr spread
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%``
#' @export
#'
#' @examples
spider_web <- function(df,
                       grp_col = "nm",
                       spk_col = "spk",
                       value_col = "value",
                       spoke_color = "grey75",
                       spoke_lty = 1,
                       ref_lines_val = c(0.5, 0.75, 1),
                       ref_lines_color = c("grey75", "grey75", "grey75"),
                       ref_lines_type = c(2, 2, 2),
                       ref_label_color = c("red", "green", "blue"),
                       ref_lines_label_spoke = c(1, 1, 1),
                       show_legend = TRUE){

  if(is.na(df) || class(df) != "data.frame" || length(df) < 3 || nrow(df) < 2){
    stop("Argument 'df' must be a data frame with at least one row and three columns")
  }

  if(length(ref_lines_val) != length(ref_lines_color) |
     length(ref_lines_color) != length(ref_lines_type) |
     length(ref_lines_type) != length(ref_label_color) |
     length(ref_label_color) != length(ref_lines_label_spoke)){
    stop("ref_lines_val, ref_lines_color, ref_lines_type, ref_label_color, and ref_lines_label_spoke must all be the same length",
         call. = FALSE)
  }

  grp_nms <- as.character(unique(df[,grp_col, drop = TRUE]))
  spk_nms <- as.character(unique(df[,spk_col, drop = TRUE]))
  spokes <- calc_spokes(length(spk_nms))
  spokes$spk_nms <- spk_nms
  ds <- tidyr::spread(df, key = spk_col, value = value_col)
  spider_data <- calc_web(ds)
  browser()
  ref_lines_attr <- data.frame(grp_nms = grp_nms,
                               x = ref_lines_val,
                               color = ref_lines_color,
                               type = ref_lines_type)
  ref_lines_data <- data.frame()
  for(i in seq_along(ref_lines_val)){
     k <- rbind(spokes, spokes[1,]) %>%
       dplyr::select(-c(x, y, spk_nms)) %>%
       dplyr::mutate(grp_nms = ref_lines_attr[i,]$grp_nms,
                     x = xend * ref_lines_attr[i,]$x,
                     y = yend * ref_lines_attr[i,]$x,
                     color = ref_lines_attr[i,]$color,
                     lty = ref_lines_attr[i,]$type)
     ref_lines_data <- rbind(ref_lines_data, k)
  }

  ref_lines_to_label <- ref_lines_attr$x[as.logical(ref_lines_label_spoke)]
  ref_label_color <- ref_label_color[as.logical(ref_lines_label_spoke)]
  if(length(ref_lines_to_label)){
    label_data <- data.frame(x = 0, y = ref_lines_to_label)
    label_data <- label_data %>% dplyr::mutate(label = y,
                                               color = ref_label_color)
    label_spokes <- spokes[ref_lines_label_spoke,]
    theta <- atan2(label_spokes$yend, label_spokes$xend)
    label_data$x <- label_data$y * cos(theta)
    label_data$y <- label_data$y * sin(theta)
  }

  g <- spider_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = spokes,
      aes(x = x, y = y, xend = xend, yend = yend), color = spoke_color, lty = spoke_lty) +
    geom_path(data = ref_lines_data,
              aes(x, y, group = grp_nms), color = ref_lines_data$color, lty = ref_lines_data$lty,
              inherit.aes = FALSE ) +
     geom_path(aes(colour = as.factor(group)), lwd = 0.8) +
     coord_equal() +
     geom_text(data = spokes, aes(
       x = xend * 1.1, y = yend * 1.1,
       label = spk_nms
     ), colour = "grey30") +
    gfplot::theme_pbs() +
    labs(colour = "MP") +
    # scale_color_viridis_d() +
    scale_color_brewer(palette = "Set2") +
    # guides(colour = FALSE) +
    # ggrepel::geom_text_repel(data = labs,
    #   aes(x = xx, y = yy, label = group, colour = group),
    #   nudge_y = 0.1, nudge_x = -0.1) +
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
    theme(legend.position="right")

  if(length(ref_lines_to_label)){
    g <- g + geom_text(data = label_data,
                       aes(x = x, y = y, label = label), color = ref_label_color,
                       nudge_y = 0.04, hjust = 0, nudge_x = 0.01)
  }
  if(!show_legend){
    g <- g + theme(legend.position = "none")
  }
  g
}