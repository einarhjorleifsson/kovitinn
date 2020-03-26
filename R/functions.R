#' Import covid data
#'
#' @param variable Character, either "confirmed" or "deaths"
#'
#' @return A tibble
#' @export
#'
read_covid <- function(variable = "confirmed") {
  url <-
    paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
           "time_series_covid19_",
           variable,
           "_global.csv")

  readr::read_csv(url)

}

#' Tidy the covid data
#'
#' Turns the wide covid table to a long format and adds ISO country
#' code (iso3c).
#'
#' @param d A tibble obtained via read_covid
#'
#' @return A tibble with the following variables:
#' @export
#'
covid_tidy <- function(d) {
  d %>%
    janitor::clean_names() %>%
    dplyr::rename(province = province_state,
                  country = country_region) %>%
    dplyr::filter(!country %in% c("Cruise Ship", "Diamond Princess")) %>%
    tidyr::pivot_longer(cols = -c(province:long), names_to = "date", values_to = "n") %>%
    dplyr::mutate(date = lubridate::mdy(stringr::str_sub(date, 2))) %>%
    dplyr::mutate(country = dplyr::case_when(country %in% c("Denmark",
                                                            "France",
                                                            "Netherlands",
                                                            "United Kingdom") &
                                               !is.na(province) &
                                               province != "Channel Islands" ~ province,
                                             TRUE ~ country),
                  province = ifelse(province == country, NA_character_, province),
                  country = ifelse(country == "St Martin", "Sint Maarten", country),
                  iso3c = countrycode::countrycode(country,
                                                   origin = "country.name",
                                                   destination = "iso3c")) %>%
    dplyr::select(country, iso3c, province, lon = long, lat, date, n)
}



#' Calculate doubling time
#'
#' @param d Tidy covid data, e.g. read_covid() %>% covid_tidy
#' @param DATE Not really, ....
#'
#' @return A tibble, ...
#' @export
#'
covid_doubling_time <- function(d, DAY = 0L, variable = confirmed) {
  d %>%
    arrange(country, desc(date)) %>%
    group_by(country) %>%
    mutate(DAYS = (1:n()) - 1) %>%
    filter(DAYS >= DAY) %>%
    select(country, date, n = {{ variable }} ) %>%
    mutate(days = (1:n()) - 1,
           r = n / max(n),
           r.unique = length(unique(r))) %>%
    filter(r.unique > 1) %>%
    summarise(z = approx(r, days, xout = 0.5)$y) %>%
    ungroup()
}

