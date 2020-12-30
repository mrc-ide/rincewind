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

  dir <- dirname(filename)
  filename <- basename(filename)
  if (one_col) {
    name <- glue("{dir}/1col_{filename}")
    ggsave(
      filename = name, plot = plot, width = 5.2, height = 6.1,
      unit = "in", dpi = 300, compression = "lzw"
    )
  }
  if (two_col) {
    name <- glue("{dir}/2col_{filename}")
    ggsave(
      filename = name, plot = plot, width = 7.45, height = 8.7,
      unit = "in", dpi = 300, compression = "lzw"
    )
    name <- glue("{dir}/2col_wider_{filename}")
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

##' Defines a date scale to be used for all figures.
##' @param date_breaks breaks for x-axis, Defaults to 1 month
##' @param date_labels labels, default is day-month
##' @export
scale_date_manuscript <- function(date_breaks, date_labels, xmin) {
  scale_x_date(
    date_breaks = date_breaks, date_labels = date_labels,
    limits = c(as.Date(xmin), NA)
  )
}


##' @export
##' @examples restimates_linegraph(df, forecast_date)
restimates_linegraph <- function(df, group_var,
                                 date_breaks = "1 month",
                                 date_labels = "%d - %b",
                                 xmin = "2020-03-01") {
  group_var <- enquo(group_var)
  p <- ggplot() +
    geom_ribbon(
      data = df,
      aes(x = dates, ymin = `2.5%`, ymax = `97.5%`,
          group = !! group_var, fill = "black"),
      alpha = 0.3
    ) +
    geom_line(
      data = df,
      aes(dates, `50%`, group = !! group_var, linetype = "solid")
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
    expand_limits(y = 0) +
    scale_date_manuscript(date_breaks, date_labels, xmin) +
    scale_fill_identity(
      breaks = "black", labels = "95% CrI", guide = "legend"
    ) +
    scale_linetype_identity(
      breaks = "solid", labels = "Median", guide = "legend"
    )
  p
}


##' @export
all_forecasts_calendar <- function(obs, pred, date_breaks,
                                   date_labels, group_var) {

  group_var <- enquo(group_var)
  ggplot() +
    geom_point(
      data = obs, aes(dates, deaths, shape = 16), alpha = 0.5
    ) +
    geom_line(
      data = pred,
      aes(x = date, `50%`, group = !! group_var, col = "#4a8c6f")
    ) +
    geom_ribbon(
      data = pred,
      aes(
        x = date, ymin = `2.5%`, ymax = `97.5%`, group = !! group_var,
        fill = "#4a8c6f"
      ),
      alpha = 0.4
    ) +
    scale_date_manuscript(date_breaks, date_labels) +
    scale_shape_identity(
      breaks = 16, labels = "Obs deaths",
      guide = guide_legend(order = 1)
    ) +
    scale_color_identity(
      breaks =  "#4a8c6f", labels = "Median",
      guide = guide_legend(order = 2)
    ) +
    scale_fill_identity(
      breaks = "#4a8c6f", labels = "95% CrI",
      guide = guide_legend(order = 3)
    ) +
    ggtitle(label = nice_country_name(obs$country[1]))

}
