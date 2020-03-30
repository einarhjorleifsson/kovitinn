plot_covid <- function(d,
                       countries = c("Iceland", "Denmark"),
                       CASE.MIN = 1,
                       relative = TRUE) {

  d2 <-
    d %>%
    filter(country %in% countries) %>%
    mutate(relative = relative,
           case.min = CASE.MIN) %>%
    filter(confirmed > case.min) %>%
    mutate(n = if_else(relative == TRUE, confirmed / pop * 1e6, confirmed, NA_real_)) %>%
    arrange(country, date) %>%
    group_by(country, iso3c) %>%
    mutate(day = as.integer(date - min(date[n >= case.min]))) %>%
    filter(day >= 0)

  x.max <- d2  %>% pull(day) %>% max()
  y.max <- max(d2$n)
  d.highlight <-
    d2 %>%
    filter(country %in% countries) %>%
    rename(fcountry = country)

  d.median <-
    d2 %>%
    filter(country %in% countries) %>%
    group_by(day) %>%
    summarise(n = median(n)) %>%
    ungroup()

  CASE.MIN <-
    dplyr::case_when(CASE.MIN  ==  1 ~ paste0(CASE.MIN, "st"),
                     TRUE ~ paste0(CASE.MIN, "th"))

  ggplot() +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_line(colour = "white"),
          strip.text = element_text(hjust = 0),
          axis.ticks.length.y = unit(.0001, "cm"),
          axis.ticks.length.x = unit(.0001, "cm"),
          panel.spacing = unit(0.3, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 11, hjust = 0, colour = "red4",
                                      margin = margin(0,0,0,0, "cm"))) +
    geom_line(data = d2 %>% filter(country %in% countries),
              aes(day, n, group = country),
              colour = "grey") +
    geom_smooth(data = d.median,
              aes(day, n),
              colour = "yellow",
              se = FALSE,
              lwd = 2) +
    geom_line(data = d.highlight,
              aes(day, n),
              colour = "red4") +
    scale_x_continuous(limits = c(0, x.max),
                       expand = c(0, 0)) +
    facet_wrap(~ fcountry) +
    labs(x = NULL, y = NULL,
         subtitle = paste0("Cumulative number of confirmed cases per million by number of days since ",
                           CASE.MIN, " case"))

}
