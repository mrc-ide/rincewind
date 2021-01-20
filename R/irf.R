##' @importFrom rlang .data
population_age_pyramid <- function(x) {
  x <- dplyr::mutate_at(x, vars(`0-4`:`100+`), as.numeric)
  out <- data.frame(
    location = x$Location,
    age_group = c("[0-15)", "[15-44)", "[45-64)", "[65-74)", "75+"),
    population = c(
      rowSums(x[1, c("0-4", "5-9", "10-14")]),
      rowSums(
        x[1, c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")]
      ),
      rowSums(x[1, c("45-49", "50-54", "55-59")]),
      rowSums(x[1, c("65-69", "70-74")]),
      rowSums(
        x[1, c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")]
      )
    ),
    check.names = FALSE
  )
  out$prop <- out$pop / sum(out$population)
  out
}


clean_pop_by_age <- function(pop_by_age, locations) {
  colnames(pop_by_age) <- pop_by_age[1, ]
  pop_by_age <- pop_by_age[-1, ]
  ## First re-organise the population in the desired age brakets
  ## Fix names so that names in ECDC match with those in population
  ## estimates excel sheet
  pop_by_age$Location[pop_by_age$Location == "Bolivia (Plurinational State of)"] <- "Bolivia"
  pop_by_age$Location[pop_by_age$Location == "Côte d'Ivoire"] <- "Cote_dIvoire"
  pop_by_age$Location[pop_by_age$Location == "Iran (Islamic Republic of)"] <- "Iran"
  pop_by_age$Location[pop_by_age$Location == "Lao People's Democratic Republic"] <- "Laos"
  pop_by_age$Location[pop_by_age$Location == "Republic of Moldova"] <- "Moldova"
  pop_by_age$Location[pop_by_age$Location == "State of Palestine"] <- "Palestine"
  pop_by_age$Location[pop_by_age$Location == "Russian Federation"] <- "Russia"
  pop_by_age$Location[pop_by_age$Location == "Republic of Korea"] <- "South Korea"
  pop_by_age$Location[pop_by_age$Location == "Syrian Arab Republic"] <- "Syria"
  pop_by_age$Location[pop_by_age$Location == "China, Taiwan Province of China"] <- "Taiwan"
  pop_by_age$Location[pop_by_age$Location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
  pop_by_age$Location[pop_by_age$Location == "Viet Nam"] <- "Vietnam"
  ## Kosovo is not present in the population estimates
  ## We use the age distribution of its nearest neighbor
  df <- pop_by_age[pop_by_age$Location == "Serbia", ]
  df$Location <- "Kosovo"
  pop_by_age <- rbind(pop_by_age, df)
  pop_by_age[pop_by_age$Location %in% locations, ]
}


### Age-disaggregated IFR
### https://www.medrxiv.org/content/10.1101/2020.08.12.20173690v2.full.pdf
### Table 3, page 19
ifr_by_age <- function(nsim = 1e4) {

  infections <- data.frame(
    age_group = c("[15-44)", "[45-64)", "[65-74)", "75+"),
    mu = c(1536000, 895000, 181000, 166000),
    sigma = 1000 * c(
      (1635 - 1437) / (2 * 1.96),
      (953 - 837) / (2 * 1.96),
      (209 - 153) / (2 * 1.96),
      (201 - 131) / (2 * 1.96)
    )
  )
  c19_deaths <- data.frame(
    age_group = c("[15-44)", "[45-64)", "[65-74)", "75+"),
    deaths = c(524, 4657, 5663, 19330)
  )
  ifr_distr <- sapply(
    infections$age_group,
    FUN = function(age_group) {
      mu <- infections$mu[infections$age_group == age_group]
      sigma <- infections$sigma[infections$age_group == age_group]
      x <- rnorm(nsim, mean = mu, sd = sigma)
      while (any(x < 0)) x <- rnorm(nsim, mean = mu, sd = sigma)
      c19_deaths$deaths[c19_deaths$age_group == age_group] / x
    },
    simplify = FALSE
  )
  names(ifr_distr) <- infections$age_group
  ifr_distr
}

##' COVID-19 IFR adjusted for age compoisition of a country
##'
##'
##' @title Age-weighted IFR of COVID-19
##' @param locations list of country names. Note that no checks have
##' been implemented to test that the country names are valid.
##'
##'
##' @return a named list with samples from age-weighted IFR for each
##' location
##' @author Sangeeta Bhatia
##' @export
age_wtd_ifr <- function(locations) {
  ifr_distr <- ifr_by_age()
  pop_by_age <- clean_pop_by_age(population_by_age, locations)
  locations <- setNames(locations, locations)
  pop_pyramid <- sapply(
    locations,
    function(location) {
      x <- pop_by_age[pop_by_age$Location == location, ]
      population_age_pyramid(x)
    },
    simplify = FALSE
  )

  out <- sapply(
    pop_pyramid,
    function(pop) {
      out <- slider::slide(
        pop, ~ ifr_distr[[as.character(.x$age_group)]] * .x$prop
      )
      out[[5]] + out[[2]] + out[[3]] + out[[4]]
    },
    simplify = FALSE
  )
  out
}
