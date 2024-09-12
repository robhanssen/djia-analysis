library(tidyverse)
library(patchwork)
theme_set(theme_light())

get_index_yahoo <- function(index, src = "yahoo") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    as_tibble(t) %>%
        mutate(
            date = zoo::index(t)
        ) %>%
        relocate(date)
}

djt <- get_index_yahoo("DJT")


mod <-
    djt %>%
    filter(date > "2024-07-21") %>%
    lm(DJT.Close ~ date, data = .)

line_fit <- broom::augment(mod, newdata = tibble(date = seq(ymd("2024-04-01"), today(), by = "1 week")))

param <- broom::tidy(mod)

average_daily_loss <- paste(scales::dollar(param$estimate[2]), "per day")

price_g <-
    djt %>%
    filter(date > "2024-03-21") %>%
    ggplot(aes(x = date)) +
    geom_step(
        aes(y = DJT.Close),
        lty = 1
    ) +
    geom_step(
        aes(y = DJT.Open),
        lty = 3
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_currency()
    ) +
    labs(
        x = "",
        y = "$DJT stock value (Close and Open)"
    ) +
    geom_vline(xintercept = ymd(20240721), alpha = .2) +
    geom_line(
        data = line_fit,
        aes(y = .fitted), color = "gray70"
    ) +
    annotate("text",
        x = ymd(20240815), y = 28,
        label = average_daily_loss, hjust = 0
    )

vol_g <-
    djt %>%
    filter(date > "2024-03-21") %>%
    ggplot(aes(x = date)) +
    geom_line(
        aes(y = DJT.Volume),
        lty = 1
    ) +
    scale_y_log10(
        # limits = c(1, NA),
        labels = scales::label_number()
    ) +
    labs(
        x = "",
        y = "$DJT stock value (Close and Open)"
    ) +
    geom_vline(xintercept = ymd(20240721), alpha = .2)

ggsave("graphs/DJT_tracking.png", height = 8, width = 8, plot = price_g / vol_g)
