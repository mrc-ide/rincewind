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
