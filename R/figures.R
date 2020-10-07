##' Projections plot
##'
##'
##' @title projections plot with median and various quantiles
##' @param obs data frame with columns dates and deaths
##' @param pred data frame with columns dates, val, .lower and .upper.
##' This is an output of ggdist::median_qi function.
##' @return ggplot object
##' @author Sangeeta Bhatia
##' @export
##' @import ggdist ggplot2
plot_projections <- function(obs, pred) {

  ## Make sure dates columns are actually dates
  obs$dates <- as.Date(obs$dates)
  pred$dates <- as.Date(pred$dates)
  ##xintercept <- as.numeric(as.Date(week_ending)) - 0.5
  p <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
    ggdist::geom_lineribbon(
      data = pred,
      aes(
        x = dates, y = val, ymin = .lower, ymax = .upper
      )
    ) +
    scale_fill_brewer(palette = "Greens") +
    scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +
    ##ggplot2::geom_vline(xintercept = xintercept, linetype = "dashed") +
    theme_minimal() +
    xlab("") + ylab("Deaths") +
    theme(legend.position = "none")

  p
}

##' @export
theme_manuscript <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size/22,
                             base_rect_size = base_size/22) {

  theme_minimal() %+replace%
    theme(
      axis.title.x = element_text(size = base_size, angle = 90),
      axis.title.y = element_text(size = base_size),
      axis.text.y = element_text(size = base_size),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size)
    )
}

##' @export
save_multiple <- function(plot, filename, one_col = TRUE, two_col = TRUE) {

  if (one_col) {
    name <- glue("1col_{filename}")
    ggsave(
      filename = name, plot = plot, width = 5.2, height = 6.1,
      unit = "in", dpi = 300, compression = "lzw"
    )
  }
  if (two_col) {
    name <- glue("2col_{filename}")
    ggsave(
      filename = name, plot = plot, width = 7.45, height = 8.7,
      unit = "in", dpi = 300, compression = "lzw"
    )
    name <- glue("2col_wider_{filename}")
    ggsave(
      filename = name, plot = plot, width = 7.45, height = 4.7,
      unit = "in", dpi = 300, compression = "lzw"
    )
  }
}

##' @export
continent_colorscale <- function() {

  c(
    "Africa" = "#000000", "Asia" = "#E69F00", "Europe" = "#56B4E9",
    "North America" = "#009E73", "South America" = "#0072B2",
    "Oceania" = "#D55E00"
  )
}

##' @export
alternating_palette <- function(x, col1 = "#3d2115", col2 = "#8e4d31") {

  palette <- rep(c(col1, col2), 2 * length(x))
  palette <- setNames(palette[1:length(x)], x)
  palette

}
