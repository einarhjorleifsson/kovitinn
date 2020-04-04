#' Title
#'
#' @param source either "ecdc" or "jhu"
#'
#' @return dataframe
#' @export
#'
get_covid <- function(source = c("ecdc", "jhu", "us")) {

  if(source == "ecdc") d <- get_covid_ecdc()
  if(source == "jhu")  d <- get_covid_jhu()
  if(source == "us")   d <- get_covid_us()

  return(d)

}


#' covid data
#'
#' European Centre for Disease Prevention and Control
#'
#' @return a tibble
#' @export
#'
get_covid_ecdc <- function() {

  httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
            httr::authenticate(":", ":", type="ntlm"),
            httr::write_disk(tf <- tempfile(fileext = ".csv")))

  d <-
    readr::read_csv(tf) %>%
    dplyr::select(country = countriesAndTerritories,
                  iso3c = countryterritoryCode,
                  date = dateRep,
                  confirmed = cases,
                  deaths,
                  pop = popData2018) %>%
    tidyr::gather(variable, n, c(confirmed, deaths)) %>%
    dplyr::mutate(date = lubridate::dmy(date),
                  source = "ecdc",
                  country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country =
                    ifelse(country == "Cases on an international conveyance Japan",
                           "Cases on an international conveyance JPN",
                           country)) %>%
    standardize_country_name() %>%
    add_missing_iso3c() %>%
    tidyr::complete(date, tidyr::nesting(country, iso3c, variable, pop, source), fill = list(n = 0)) %>%
    dplyr::arrange(country, variable, date) %>%
    dplyr::group_by(country, variable) %>%
    dplyr::mutate(cn = cumsum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(iso3c)) %>%
    dplyr::select(country, iso3c, date, type = variable, n, cn)

  return(d)

}

#' get covid data from JHU
#'
#' John Hopkins University
#'
#' @return a tibble
#' @export
#'
get_covid_jhu <- function() {

  # TODO: Fix province "Channel Islands"

  lh <- function(what) {

    url <-
      paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
             "time_series_covid19_",
             what,
             "_global.csv")
    readr::read_csv(url) %>%
      dplyr::mutate(variable = what)
  }

  d <-
    purrr::map_df(c("confirmed", "deaths", "recovered"), lh)
  d %>%
    dplyr::rename(province = `Province/State`,
                  country = `Country/Region`) %>%
    tidyr::pivot_longer(cols = -c(province:Long, variable), names_to = "date", values_to = "cn") %>%
    dplyr::mutate(date = lubridate::mdy(date),
                  country = stringr::str_squish(country),
                  country = ifelse(country == "St Martin", "Sint Maarten", country),
                  country = dplyr::case_when(country %in% c("Denmark",
                                                            "France",
                                                            "Netherlands",
                                                            "United Kingdom") &
                                               !is.na(province) &
                                               province != "Channel Islands" ~ province,
                                             TRUE ~ country),
                  province = ifelse(province == country, NA_character_, province),
                  province = stringr::str_squish(province)) %>%
    standardize_country_name() %>%
    add_iso3c() %>%
    dplyr::mutate(source = "jhu") %>%
    dplyr::mutate(cn = dplyr::case_when(country == "Iceland" &
                                          date == lubridate::ymd("2020-03-15") &
                                          variable == "deaths" ~ 0,
                                        country == "Iceland" &
                                          date == lubridate::ymd("2020-03-20") &
                                          variable == "deaths" ~ 1,
                                        country == "Iceland" &
                                          dplyr::between(date, lubridate::ymd("2020-03-15"), lubridate::ymd("2020-03-17")) &
                                          variable == "recovered" ~ 1,
                                        TRUE ~ cn)) %>%
    dplyr::arrange(country, variable, date) %>%
    dplyr::group_by(country, variable) %>%
    dplyr::mutate(n = dplyr::lead(cn) - cn,
                  n = tidyr::replace_na(n, 0)) %>%
    dplyr::ungroup()
}

#' get US covid data from JHU
#'
#' John Hopkins University
#'
#' @return a tibble
#' @export
#'
get_covid_us <- function() {

  lh <- function(what) {

    url <-
             "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
      paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/",
             "time_series_covid19_",
             what,
             "_US.csv")
    readr::read_csv(url) %>%
      dplyr::mutate(variable = what)
  }

  d <-
    purrr::map_df(c("confirmed", "deaths"), lh)
  d %>%
    tidyr::pivot_longer(cols = -c(UID:Combined_Key, variable), names_to = "date", values_to = "cn") %>%
    janitor::clean_names() %>%
    dplyr::mutate(country = stringr::str_squish(country_region),
                  province = stringr::str_squish(province_state),
                  date = lubridate::mdy(date),
                  country = "United States") %>%
    dplyr::mutate(source = "jhu") %>%
    dplyr::arrange(country, province, variable, date) %>%
    dplyr::group_by(country, province, variable) %>%
    dplyr::mutate(n = dplyr::lead(cn) - cn,
                  n = tidyr::replace_na(n, 0)) %>%
    dplyr::ungroup()


}

standardize_country_name <- function(d) {
  d %>%
    dplyr::mutate(country2 = countrycode::countrycode(country,
                                                      "country.name",
                                                      "country.name",
                                                      warn = FALSE),
                  country = ifelse(!is.na(country2), country2, country)) %>%
    dplyr::select(-country2)
}
add_missing_iso3c <- function(d) {

  d %>%
    dplyr::mutate(iso3c.2 = countrycode::countrycode(country,
                                                     "country.name",
                                                     "iso3c",
                                                     warn = FALSE),
                  iso3c = ifelse(!is.na(iso3c), iso3c, iso3c.2)) %>%
    dplyr::select(-iso3c.2) %>%
    dplyr::mutate(iso3c = ifelse(country == "Kosovo", "XKX", iso3c),
                  iso3c = ifelse(iso3c == "N/A", NA_character_, iso3c))

}

add_iso3c <- function(d) {
  d %>%
    dplyr::mutate(iso3c = countrycode::countrycode(country,
                                                   "country.name",
                                                   "iso3c",
                                                   warn = FALSE))
}
