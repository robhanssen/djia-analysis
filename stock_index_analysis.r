library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("functions.r")

index <- c("DJIA", "SP500", "NASDAQCOM", "WILL5000PR")

stockindex <- collect_online_data(index)

save(stockindex, file = "Rdata/stockindex.Rdata")

train_1 <- ymd("2013-06-18") %--% ymd("2015-03-06")
model_1 <- predict_index(stockindex, train_1)

train_2 <- ymd("2017-10-01") %--% ymd("2019-10-10")
model_2 <- predict_index(stockindex, train_2)

train_3 <- ymd("2022-01-21") %--% ymd("2022-07-01")
model_3 <- predict_index_choice(stockindex, train_3, regtype = "linear")

color <- c(
    "DJIA" = "darkblue",
    "SP500" = "darkgreen",
    "NASDAQCOM" = "darkred",
    "WILL5000PR" = "purple",
    "GDP" = "black"
)

plot <-
    stockindex %>%
    ggplot() +
    aes(date, value, color = index) +
    geom_point(alpha = .2, size = .2) +
    geom_line(
        data = model_1,
        aes(y = .fitted, color = index),
        lty = 1,
        linewidth = .25
    ) +
    geom_line(
        data = model_2,
        aes(y = .fitted, color = index),
        lty = 1,
        linewidth = .25
    ) +
    geom_line(
        data = model_3,
        aes(y = .fitted, color = index),
        lty = 1,
        linewidth = .25
    ) +
    geom_ribbon(
        data = model_1,
        aes(
            y = NULL,
            ymin = .lower,
            ymax = .upper,
            fill = index,
            color = NULL
        ),
        alpha = .1,
    ) +
    geom_ribbon(
        data = model_2,
        aes(
            y = NULL,
            ymin = .lower,
            ymax = .upper,
            color = NULL,
            fill = index
        ),
        alpha = .1,
    ) +
    scale_x_date(
        date_breaks = "2 year",
        date_labels = "%Y",
        minor_breaks = "1 year"
    ) +
    scale_y_continuous(
        breaks = c(
            seq(0, 10e3, 5e2),
            seq(10e3, 50e3, 5e3)
        ),
        label = scales::comma_format()
    ) +
    labs(
        x = "Date",
        y = "Stock Index"
    ) +
    theme(legend.position = "none") +
    facet_wrap(~index, scales = "free_y", ncol = 2) +
    scale_color_manual(values = color) +
    scale_fill_manual(values = color)

images <- length(index)

wdt <- 12
hgth <- (images + 1) %/% 2 * 6

ggsave("graphs/stockindex_modelpredictions.png",
    width = wdt,
    height = hgth,
    plot = plot
)

no_output <- purrr::map(
    index,
    ~ indiv_graph(
        .x,
        stockindex %>% filter(date > ymd(20171231)),
        train_2
    )
)

plot +
    scale_x_date(
        date_breaks = "3 months",
        date_labels = "%b\n%Y",
        minor_breaks = "1 month",
        limits = c(ymd(20210101), NA)
    )

ggsave("graphs/stockindex_modelpredictions_zoomed.png",
    width = wdt,
    height = hgth
)