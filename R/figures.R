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
##' @import ggdist
plot_projections <- function(obs, pred) {

  ## Make sure dates columns are actually dates
  obs$dates <- as.Date(obs$dates)
  pred$dates <- as.Date(pred$dates)
  week_ending <- min(pred$dates)
  xintercept <- as.numeric(as.Date(week_ending)) - 0.5
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = obs, aes(dates, deaths)) +
    ggdist::geom_lineribbon(
      data = pred,
      aes(
        x = dates, y = val, ymin = .lower, ymax = .upper
      )
    ) +
    ggplot2::scale_fill_brewer(palette = "Greens") +
    ggplot2::scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +
    ggplot2::geom_vline(xintercept = xintercept, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::xlab("") + ggplot2::ylab("Deaths") +
    ggplot2::theme(legend.position = "none")

  p
}
