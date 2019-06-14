spider_web <- function(df,
                       grp_col = "nm",
                       spk_col = "spk",
                       value_col = "value"){
  grp_nms <- as.character(unique(df[,grp_col, drop = TRUE]))
  spokes <- calc_spokes(length(grp_nms))
  spokes$grp_nms <- grp_nms
  ds <- tidyr::spread(df, key = spk_col, value = value_col)
  spider_data <- calc_web(ds)
  #label_data <- data.frame(x = 0, y = c(0.5, 0.75))

  ## for ggrepel labels:
  # radar_data$pm <- rep(c(names(dat[,-1]), ""), length(unique(dat[,1])))
  # labs <- filter(radar_data, pm %in% c("AAVY", "PNOF")) %>%
  #   group_by(group) %>%
  #   summarize(xx = approx(x = x, y = y, n = 7)$x[6],
  #     yy = approx(x = x, y = y, n = 7)$y[6])

  browser()
  spider_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_segment(
      data = spokes,
      aes(x = x, y = y, xend = xend, yend = yend), colour = "grey75", lty = 1
    ) +
    # FIXME: do this more elegantly in the data:
    geom_path(
      data = rbind(spokes, spokes[1, ]),
      aes(x = xend * 0.5, y = yend * 0.5), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes, spokes[1, ]),
      aes(x = xend * 0.75, y = yend * 0.75), colour = "grey75", lty = 2
    ) +
    geom_path(
      data = rbind(spokes, spokes[1, ]),
      aes(x = xend * 1, y = yend * 1), colour = "grey75", lty = 2
    ) +
    geom_path(aes(colour = as.factor(group)), lwd = 0.8) +
    coord_equal() +
    geom_text(data = spokes, aes(
      x = xend * 1.1, y = yend * 1.1,
      label = nms
    ), colour = "grey30") +
    geom_text(
      data = label_data,
      aes(x = x, y = y, label = y), colour = "grey50",
      nudge_y = 0.04, hjust = 0, nudge_x = 0.01
    ) +
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

}