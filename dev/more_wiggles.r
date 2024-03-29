library(tidyverse)
library(lubridate)
theme_set(theme_light())

source("functions.r")

linesinemodel <- function(tbl) {
    nls(
        data = tbl,
        trace = TRUE,
        value ~ a0 * sin((time - a1) / a2) + b0 - b1 * time,
        start = list(a0 = 1500, a1 = 42, a2 = 16, b0 = 32500, b1 = 4),
        control = nls.control(maxiter = 100)
    )
}

index <- c("SP500", "WILL5000PR", "DJIA")

stockindex_raw <- collect_online_data(index)

cuttoff_date <- ymd(20220401) + days(14)

stockindex <- stockindex_raw %>%
    filter(date > cuttoff_date) %>%
    mutate(time = as.numeric(date - cuttoff_date))

linesine <- stockindex %>%
    group_by(index) %>%
    nest() %>%
    mutate(sinmod = map(data, ~ linesinemodel(.)))

linesine %>%
    mutate(params = map(sinmod, broom::tidy)) %>%
    unnest(params) %>%
    select(index, term, estimate) %>%
    pivot_wider(names_from = "term", values_from = "estimate") %>%
    ungroup() %>%
    mutate(period = 2 * pi / a2 * 365)

linesine %>%
    mutate(dats = map(
        sinmod,
        ~ broom::augment(.x, newdata = tibble(time = 0:365))
    )) %>%
    unnest(dats) %>%
    mutate(date = time + cuttoff_date) %>%
    ggplot() +
    aes(date, .fitted) +
    geom_line() +
    facet_wrap(~index, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "2 month", date_label = "%b\n%Y") +
    geom_point(data = stockindex, aes(y = value), alpha = .5) +
    theme(legend.position = "none") +
    labs(y = NULL, x = "Date") +
    theme(panel.grid.minor.y = element_blank())

ggsave("dev/wiggles-model.png", height = 12, width = 6)

linesine %>%
    mutate(params = map(sinmod, broom::tidy)) %>%
    unnest(params) %>%
    ungroup() %>%
    filter(term == "a2") %>%
    select(index, term, estimate, std.error) %>%
    mutate(
        est_lo = estimate + 2 * std.error,
        est_hi = estimate - 2 * std.error
    ) %>%
    mutate(across(starts_with("est"), ~ 2 * pi / .x * 365)) %>%
    select(index, starts_with("est"))

linesine %>%
    mutate(dats = map(
        sinmod,
        ~ broom::augment(.x, newdata = tibble(time = 0:365))
    )) %>%
    unnest(dats) %>%
    mutate(date = time + cuttoff_date) %>%
    inner_join(stockindex, by = c("index", "date")) %>%
    select(index, date, value, .fitted) %>%
    nest() %>%
    mutate(lineal = map(data, ~ lm(.fitted ~ value, data = .x))) %>%
    mutate(dataq = map(lineal, broom::glance)) %>%
    unnest(dataq) %>%
    select(index, r.squared, adj.r.squared)

ggsave("graphs/wiggle-by-index.png", width = 4, height = 8)