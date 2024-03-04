library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

source("functions.r")

index <- c("DJIA", "SP500", "NASDAQCOM", "WILL5000PR")

stockindex <- collect_online_data(index)

save(stockindex, file = "Rdata/stockindex.Rdata")

predict_index_2 <- function(tbl, daterange, yrs = 0) {
    tbl %>%
        filter(date %within% daterange) %>%
        group_by(index) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(value ~ date, data = .))) %>%
        mutate(modeldata = map(
            model,
            ~ broom::augment(.x,
                interval = "prediction",
                newdata = period_to_dates(daterange, yrs)
            )
        )) %>%
        unnest(modeldata) # %>%
    # mutate(
    #     .fitted = 10^.fitted,
    #     .lower = 10^.lower,
    #     .upper = 10^.upper
    # )
}



train_1 <- ymd("2019-10-01") %--% ymd("2020-02-01")
model_1 <- predict_index_2(stockindex, train_1)

train_2 <- ymd("2020-06-01") %--% ymd("2021-03-01")
model_2 <- predict_index_2(stockindex, train_2)

train_3 <- ymd("2022-07-01") %--% today()
model_3 <- predict_index_2(stockindex, train_3, yrs = 1L)


color <- c(
    "DJIA" = "darkblue",
    "SP500" = "darkgreen",
    "NASDAQCOM" = "darkred",
    "WILL5000PR" = "purple",
    "GDP" = "black"
)

plot <-
    stockindex %>%
    filter(date > ymd(20180101)) %>%
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
        date_breaks = "6 months",
        date_labels = "%b\n%Y",
        minor_breaks = NULL
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

ggsave("graphs/stockindex_two_increases.png",
    width = wdt,
    height = hgth,
    plot = plot
)

# no_output <- purrr::map(
#     index,
#     ~ indiv_graph(
#         .x,
#         stockindex %>% filter(date > ymd(20171231)),
#         train_2
#     )
# )
