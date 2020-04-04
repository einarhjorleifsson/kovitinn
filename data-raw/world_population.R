# get world populations --------------------------------------------------------

# wikipedia
tb <-
  httr::GET("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population") %>%
  xml2::read_html() %>%
  rvest::html_nodes("table")

replace_everything_within_square_brackets <- function(x) {
  stringr::str_replace_all(x, "\\[.*?\\]", "")
}
replace_everything_within_brackets <- function(x) {
  stringr::str_replace_all(x, "\\(.*?\\)", "")
}

wiki <-
  rvest::html_table(tb[1]) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  tibble::as_tibble() %>%
  dplyr::filter(country_or_dependent_territory != "World") %>%
  dplyr::select(country = country_or_dependent_territory,
                pop = population) %>%
  dplyr::mutate(pop = stringr::str_replace_all(pop,",", ""),
                pop = as.integer(pop),
                country = replace_everything_within_square_brackets(country),
                country = replace_everything_within_brackets(country)) %>%
  dplyr::filter(country != "Northern Cyprus") %>%
  dplyr::mutate(iso3c = countrycode::countrycode(country,
                                                 "country.name",
                                                 "iso3c",
                                                 warn = FALSE),
                country = countrycode::countrycode(iso3c,
                                                   "iso3c",
                                                   "country.name",
                                                   warn = FALSE)) %>%
  filter(!is.na(country))

library(wpp2019)
data(pop)
wpp2019 <-
  pop %>%
  select(iso3n = country_code, country = name, pop = `2020`) %>%
  as_tibble() %>%
  mutate(country = as.character(country),
         iso3c = countrycode::countrycode(iso3n,
                                          origin = "iso3n",
                                          destination = "iso3c",
                                          warn = FALSE),
         country = countrycode::countrycode(iso3c,
                                            origin = "iso3c",
                                            destination = "country.name",
                                            warn = FALSE)) %>%
  dplyr::filter(!is.na(iso3c)) %>%
  dplyr::select(iso3c, country, pop) %>%
  filter(!is.na(country))

world_population <-
  bind_rows(wiki %>% mutate(source = "wikipedia"),
            wpp2019 %>% mutate(source = "wpp2019"))  %>%
  spread(source, pop) %>%
  mutate(source = ifelse(!is.na(wikipedia), "wikipedia", "wpp2019"),
         pop = ifelse(!is.na(wikipedia), wikipedia, wpp2019)) %>%
  select(country, iso3c, pop, source) %>%
  add_row(country = c("Caribbean Netherlands", "Kosovo"),
          iso3c = c("BES", "XKX"),
          pop = c(25157, 1845300),
          source = c("wikipedia", "wikipedia"))
usethis::use_data(world_population)

