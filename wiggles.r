# the great wiggle
library(tidyverse)
library(lubridate)
library(patchwork)
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
        start = list(a0 = 1, a1 = 50, a2 = 16), 
        control = nls.control(maxiter = 300)
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
        newdata = tibble(time = 1:350)
    ))) %>%
    unnest(sinemodpred) %>%
    mutate(date = cuttoff_date - days(1) + days(time)) %>%
    mutate(month = month(date))


wiggleparam <- wigglemod %>%
    mutate(sinemodparam = map(sinemod, broom::tidy)) %>%
    unnest(sinemodparam)

predictions %>%
    mutate(month = month(date)) %>%
    ggplot() +
    aes(date, rel_residue, color = factor(month)) +
    geom_point() +
    geom_line() +
    geom_line(
        data = wigglepred, aes(y = 1.5*.fitted, color = factor(month)),
        lty = 2,
        # color = "gray50"
    ) +
    scale_x_date(date_breaks = "2 month", date_label = "%b %d") +
    scale_y_continuous(
        name = "Relative deviation from predicted amount",
        breaks = 0.25 * -10:10
    ) +
    labs(color = "Month")

ggsave("graphs/wiggle-analysis.png", width = 8, height = 6)

inner_join(wigglepred %>% rename(.fittedwiggle = .fitted), predictions %>% rename(.fittedpred = .fitted), by = "date") %>%
    ggplot + aes(rel_residue, 1.5 * .fittedwiggle, color = factor(month)) + geom_point() + geom_abline() +
    scale_x_continuous(limits = c(-1,1)) + 
    scale_y_continuous(limits = c(-1,1)) 


inner_join(wigglepred %>% rename(.fittedwiggle = .fitted), predictions %>% rename(.fittedpred = .fitted), by = "date") %>%
    ggplot + aes(date, .fittedpred) + geom_line() + facet_wrap(~index, scale = "free_y") +
    geom_point(aes(y = value)) +
    geom_line(aes(y = -.fittedwiggle * resid + .fittedpred))

source("dev/explaining_wiggles.r")