# the great wiggle
library(tidyverse)
library(lubridate)
theme_set(theme_light())
source("functions.r")

index <- c("SP500", "WILL5000PR", "DJIA")

stockindex_raw <- collect_online_data(index)

cuttoff_date <- ymd(20220401) + days(14)


stockindex <- stockindex_raw %>%
    filter(date > cuttoff_date)

stockmod <-
    stockindex %>%
    group_by(index) %>%
    nest() %>%
    mutate(stockmod = map(data, ~ lm(value ~ date, data = .)))

predictions <-
    stockmod %>%
    mutate(pred = map(stockmod, broom::augment)) %>%
    unnest(pred) %>%
    mutate(resid = value - .fitted) %>%
    group_by(index) %>%
    mutate(
        rel_residue = resid / max(abs(resid)),
        time = as.numeric(date - cuttoff_date)
    ) %>%
    ungroup()

# works
sinemodel <- function(tbl) {
    nls(
        data = tbl,
        trace = TRUE,
        rel_residue ~ a0 * sin((time - a1) / a2),
        start = list(a0 = 1, a1 = 50, a2 = 6)
    )
}

wigglemod <- predictions %>%
    mutate(time = as.numeric(date - cuttoff_date)) %>%
    nest(data = everything()) %>%
    mutate(sinemod = map(data, ~ sinemodel(.)))

wigglepred <- wigglemod %>%
    mutate(sinemodpred = map(sinemod, broom::augment)) %>%
    unnest(sinemodpred) %>%
    mutate(date = cuttoff_date - days(1) + days(time))

wigglepred <- wigglemod %>%
    mutate(sinemodpred = map(sinemod, ~ broom::augment(.x,
        newdata = tibble(time = 1:150)
    ))) %>%
    unnest(sinemodpred) %>%
    mutate(date = cuttoff_date - days(1) + days(time))


wiggleparam <- wigglemod %>%
    mutate(sinemodparam = map(sinemod, broom::tidy)) %>%
    unnest(sinemodparam)

predictions %>%
    mutate(month = month(date)) %>%
    ggplot() +
    aes(date, rel_residue, color = factor(month)) +
    geom_point() +
    geom_line(
        data = wigglepred, aes(y = .fitted, color = NULL),
        lty = 2,
        color = "gray50"
    ) +
    scale_x_date(date_breaks = "2 weeks", date_label = "%b %d") +
    scale_y_continuous(
        name = "Relative deviation from predicted amount",
        breaks = 0.25 * -10:10
    )

ggsave("graphs/wiggle-analysis.png", width = 8, height = 6)