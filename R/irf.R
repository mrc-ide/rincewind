##' @importFrom rlang .data
population_age_pyramid <- function(x) {
  x <- mutate_at(x, vars(`0-4`:`100+`), as.numeric)
  out <- data.frame(
    location = location,
    "[0-15)" = rowSums(select(x, .data$`0-4`:.data$`10-14`)),
    "[15-44)" = rowSums(select(x, .data$`15-19`:.data$`40-44`)),
    "[45-64)" = rowSums(select(x, .data$`45-49`:.data$`60-64`)),
    "[65-74)" = rowSums(select(x, .data$`65-69`:.data$`70-74`)),
    "75+" = rowSums(select(x, .data$`75-79`:.data$`100+`)),
    ## total = rowSums(select(x, `0-4`:`100+`)),
    check.names = FALSE
  )
  out <- gather(out, .data$age_group, .data$pop, -.data$location)
  out$prop <- out$pop / sum(out$pop)
  out
}


clean_pop_by_age <- function(pop_by_age) {
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

  pop_by_age
}


### Age-disaggregated IFR
### https://www.medrxiv.org/content/10.1101/2020.08.12.20173690v2.full.pdf
### Table 3, page 19
ifr_by_age <- function() {

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
  ifr_distr <- map(
    infections$age_group,
    function(age_group) {
      mu <- infections$mu[infections$age_group == age_group]
      sigma <- infections$sigma[infections$age_group == age_group]
      x <- rnorm(1e4, mean = mu, sd = sigma)
      while (any(x < 0)) x <- rnorm(1e4, mean = mu, sd = sigma)
      c19_deaths$deaths[c19_deaths$age_group == age_group] / x
    }
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
##' Use names as in ECDC COVID-19 data, so United_Kingdom instead of
##' any other variation.
##' @return a named list with samples from age-weighted IFR for each
##' location
##' @author Sangeeta Bhatia
##' @export
age_wtd_ifr <- function(locations) {
  ifr_distr <- ifr_by_age()
  pop_by_age <- clean_pop_by_age(population_by_age)
  locations <- setNames(locations, locations)
  pop_pyramid <- map(
    locations,
    function(location) {
      x <- pop_by_age[pop_by_age$Location == location, ]
      population_age_pyramid(x)
    }
  )
  out <- map(
    pop_pyramid,
    function(pop) {
      out <- slide(
        pop, ~ ifr_distr[[as.character(.x$age_group)]] * .x$prop
      )
      out[[5]] + out[[2]] + out[[3]] + out[[4]]
    }
  )
  out
}
