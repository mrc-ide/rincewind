##' @author Sangeeta Bhatia
##' @export
pool_predictions_weighted <- function(outputs, weights, nsim = 10000) {
  models <- names(weights)
  ## Sample model with weights
  n_1 <- sample(
    x = names(weights), nsim, replace = TRUE, prob = weights
  )
  n_1 <- table(n_1)
  if (! all(models %in% names(n_1))) {
    idx <- which(! models %in% names(n_1))
    n_1[[models[idx]]] <- 0
  }
  message("Number of times models picked ")
  message(paste(n_1, collapse = "\n"))

  out <- purrr::imap(
    outputs,
    function(output, model) {
      apply(output, 2, function(y) sample(y, size = n_1[[model]]))
    }
  )
  out <- Reduce("rbind", out)
  out
}

######################################################################
## Extract model quantiles
## Each model output is a list with several elements,
## the key one is Predictions.
## This is a list of data.frames with one data.frame for each country
## For each country, we have a list of 2 components, corresponding
## to the 2 serial intervals being considered.
## it is this last list (country, 2 components) that is passed to
## this function.
##' @export
extract_predictions_qntls <- function(y, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  names(y) <- paste0("si_", seq_along(y))
  out <- purrr::map_dfr(
    y,
    function(y_si) {
      out2 <- t(
        apply(y_si, 2, stats::quantile, prob = prob, na.rm = TRUE)
      )
      out2 <- as.data.frame(out2)
      out2 <- tibble::rownames_to_column(out2, var = "date")
      out2
    },
    .id = "si"
  )
  out
}

##' @export
daily_to_weekly <- function(y, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  names(y) <- paste0("si_", seq_along(y))
  out <- purrr::map_dfr(
    y,
    function(y_si) {
      weekly <- rowSums(y_si)
      weekly <- stats::quantile(
        weekly,
        prob = prob,
        na.rm = TRUE
      )
      weekly_df <- as.data.frame(weekly)
      ## This is not the last date for which predictions are
      ## available, but the last date for which observations are
      ## available.
      weekly_df$week_ending <- as.Date(colnames(y_si)[1]) - 1

      weekly_df <- tibble::rownames_to_column(
        weekly_df,
        var = "quantile"
      )

      weekly_df <- tidyr::spread(
        weekly_df, key = quantile, value = weekly
      )
      weekly_df
    },
    .id = "si"
  )
  out
}

##' @export
assign_epidemic_phase <- function(rt) {
  rt$phase <- dplyr::case_when(
    rt$`97.5%` < 1 ~ "decline",
    (rt$`97.5%` - rt$`2.5%` > 1)  ~ "unclear",
    (rt$`2.5%` > 1 &
     ((rt$`97.5%` - rt$`2.5%`) < 1))  ~ "growing",
    (rt$`2.5%` < 1 &
     ((rt$`97.5%` - rt$`2.5%`) < 1))  ~ "stable/growing slowly"
  )
  rt
}
##' Assign epidemic phase
##'
##' Use Epinow definitions
##'
##' @param rt a vector of samples from the
##' posterior distribution
##' @return phase
##' @author Sangeeta Bhatia
##' @export
assign_epidemic_phase2 <- function(rt) {
  less_than_1 <- 100 * (length(which(rt < 1)) / length(rt))
  phase <- NA
  if (less_than_1 < 5) phase <- "definitely growing"
  if (5 <= less_than_1 & less_than_1 < 45) phase <- "likely growing"
  if (45 <= less_than_1 & less_than_1 < 55) phase <- "likely stable"
  if (55 <= less_than_1 & less_than_1 < 60) phase <- "indeterminate"
  if (60 <= less_than_1 & less_than_1 < 80) phase <- "likely decreasing"
  if (80 <= less_than_1) phase <- "definitely decreasing"
  phase
}

##' Check if two intervals overlap
##'
##'
##'
##' @param x1 a numeric vector of length 2
##' @param x2 a numeric vector of length 2
##' @param digits integer indicating the number of decimal places
##' to be used
##' @return TRUE if x1 and x2 overlap
##' @author Sangeeta Bhatia
##' @export
overlaps <- function(x1, x2, digits) {

  low1 <- round(min(x1), digits = digits)
  high1 <- round(max(x1), digits = digits)
  interval1 <- seq(low1, high1, by = 10^(-digits))

  low2 <- round(min(x2), digits = digits)
  high2 <- round(max(x2), digits = digits)
  interval2 <- seq(low2, high2, by = 10^(-digits))

  common <- intersect(interval1, interval2)
  ## if the two intervals only overlap on the edge, we want to say
  ## they don't overlap.
  overlap <- length(common) > 1

  overlap
}



##' Convert time-series to incidence object
##'
##' incidence package accepts a linelist like object (list of dates)
##' and converts them into an incid object, which is needed by
##' projections package. To use projections package with a time-series
##' we convert time-series object into incid object.
##'
##' @param ts
##' @param date_col
##' @param case_col
##' @return
##' @author Sangeeta Bhatia
##' @export
ts_to_incid <- function(ts, date_col, case_col) {

  first_date <- min(ts[[date_col]])
  last_date <- max(ts[[date_col]])
  x <- tidyr::uncount(ts, weights = ts[[case_col]])
  out <- incidence::incidence(
    x[[date_col]],
    first_date = first_date,
    last_date = last_date
  )
  out
}

##' Cap predictions to twice the observed for visualisation
##'
##'
##'
##' @param pred
##' @return data.frame with capped predictions
##' @author Sangeeta Bhatia
##' @export
cap_predictions <- function(pred) {

  x <- split(pred, pred$country)
  purrr::map_dfr(x, function(y) {
    ymax <- 2 * ceiling(max(y$deaths, na.rm = TRUE) / 10) * 10
    y$`50%`[y$`50%` > ymax] <- NA
    dplyr::mutate_if(y, is.numeric, ~ ifelse(.x > ymax, ymax, .x))
  }
 )
}


##' @export
##' @importFrom snakecase to_title_case
nice_country_name <- function(x) {
  out <- snakecase::to_title_case(as.character(x))
  out[out == "Bosnia and Herzegovina"] <- "Bosnia-Herz'"
  out[out == "Dominican Republic"] <- "D Republic"
  out[out == "North Macedonia"] <- "N Macedonia"
  out[out == "South Africa"] <- "S Africa"
  out[out == "South Korea"] <- "S Korea"
  out[out == "United States of America"] <- "USA"
  out[out == "United Kingdom"] <- "UK"
  out
}

##' @export
round_and_format <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}

##' @export
deaths_threshold <- function(ts,
                             Threshold_criterion_7days = 10,
                             Threshold_criterion_prev7days = 10) {
  th1 <- sum(
    ts$Deaths[ts$DateRep >= max(ts$DateRep) - 7],
    na.rm = TRUE
  ) >= Threshold_criterion_7days

  th2 <- sum(
    ts$Deaths[ts$DateRep >= max(ts$DateRep) - 14 &
      ts$DateRep < max(ts$DateRep) - 7]
  ) >= Threshold_criterion_prev7days

  th3 <- sum(ts$Deaths) >= 100

  th1 & th2 & th3
}

##' @export
##' @param data data.frame that has a column listing countries
##' @param country_col name of the column that has country names
country_to_continent <- function(data, country_col) {

  merge(
    data, country_continent_mapping, by.x = country_col,
    by.y = "Countries and territories"
  )
}
