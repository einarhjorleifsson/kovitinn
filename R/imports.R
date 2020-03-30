# Population and other statistics ----------------------------------------------

library(wpp2019)
data(pop)
POP <-
  pop %>%
  select(iso3n = country_code, country = name, pop = `2020`) %>%
  as_tibble() %>%
  mutate(country = as.character(country),
         iso3c = countrycode::countrycode(iso3n,
                                          origin = "iso3n",
                                          destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, country, pop)
rm(pop, popF, popFT, popM, popMT)



# vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")
wbstats::wb(indicator = c("SP.POP.TOTL", "NY.GDP.PCAP.KD", "SP.DYN.LE00.IN"),
            mrv = 1) %>%
  tidyr::as_tibble() %>%
  dplyr::rename(var = indicatorID) %>%
  dplyr::mutate(var = dplyr::case_when(var == "AG.LND.TOTL.K2" ~ "land_area_skm",
                                       var == "EN.POP.DNST" ~ "pop_density",
                                       var == "NY.GDP.PCAP.KD" ~ "gdp_capita",
                                       var == "SP.DYN.LE00.IN" ~ "life_expectancy",
                                       var == "SP.POP.TOTL" ~ "population",
                                       TRUE ~ NA_character_)) %>%
  dplyr::select(iso3c, country, var, value) %>%
  tidyr::spread(var, value) %>%
  dplyr::full_join(POP %>%
                     dplyr::select(iso3c, pop) %>%
                     dplyr::mutate(pop = pop * 1e3)) %>%
  dplyr::mutate(pop.source = ifelse(is.na(population) & !is.na(pop),
                                    "wpp2019",
                                    "wb"),
                population = ifelse(pop.source == "wpp2019",
                                    pop,
                                    population)) %>%
  dplyr::select(-pop) %>%
  # Manual stuff, country in covid but no population number in abov
  dplyr::bind_rows(tidyr::tribble(~country, ~iso3c, ~population, ~pop.source,
                                  "Saint Barthelemy", "BLM", 9793, "wikipedia",
                                  "Holy See",         "VAT", 1000, "wikipedia",
                                  "Montserrat",       "MSR", 4649, "wikipedia")) %>%
  readr::write_rds("data/wbstats01.rds")

c("Iceland",
    "Greenland",
    "Faroe Islands",
    "Norway",
    "Andorra",
    "Azerbaijan",
    "Belarus",
    "Georgia",
    "Liechtenstein",
    "Moldova",
    "Monaco",
    "Russia",
    "San Marino",
    "Switzerland",
    "Ukraine",
    "United Kingdom",
    # EU countries below
    "Austria",
    "Belgium",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czechia",
    "Denmark",
    "Estonia",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Italy",
    "Latvia",
    "Lithuania",
    "Luxembourg",
    "Malta",
    "Netherlands",
    "Poland",
    "Portugal",
    "Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Sweden") %>%
  sort() %>%
  write_rds("data/countries_europa.rds")

