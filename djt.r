library(tidyverse)
library(patchwork)
theme_set(theme_light())


# event info
events <-
    tribble(
        ~date, ~event,
        "2024-03-22", "Truth Social IPO",
        "2024-04-15", "Announcement additional stock",
        "2024-07-15", "GOP Convention",
        "2024-07-23", "Joe Biden steps down",
        "2024-07-26", "First Trump shooting",
        "2024-08-19", "DNC Convention",
        "2024-09-16", "Second Trump shooting",
        "2024-09-19", "Open sale Truth Social",
        "2024-11-05", "Election day",
        "2025-01-20", "Inauguration day"
    ) %>%
    mutate(date = ymd(date)) %>%
    arrange(date)

# add in dates for important up and down events for fitting
fit_line_dates <-
    tribble(
        ~date, ~name,
        "2024-07-21", "first_down",
        "2024-09-26", "up_after_free_window"
    ) %>%
    mutate(across(starts_with("date"), ymd))

get_index_yahoo <- function(index, src = "yahoo") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    as_tibble(t) %>%
        mutate(
            date = zoo::index(t)
        ) %>%
        relocate(date)
}

djt <- get_index_yahoo("DJT")

events <- events %>% left_join(djt, by = "date")

fitted_models <- full_join(fit_line_dates, djt) %>%
    arrange(date) %>%
    fill(name, .direction = "down") %>%
    drop_na(name) %>%
    nest(data = !name) %>%
    mutate(
        mod = map(data, ~ lm(DJT.Close ~ date, data = .x)),
        param = map(mod, broom::tidy),
        fitted = map(mod, broom::augment)
    )

param_estimate <-
    fitted_models %>%
    unnest(param) %>%
    filter(term == "date") %>%
    mutate(across(c("estimate", "std.error"), ~ round(.x, digits = 4))) %>%
    mutate(comment = glue::glue("Estimate of change: {estimate - std.error}-{estimate - std.error} $/day")) %>%
    mutate(positive_estimate = (estimate > 0) & (estimate - std.error > 0)) %>%
    select(name, comment, positive_estimate)

data_estimate <-
    fitted_models %>%
    unnest(fitted) %>%
    select(name, date, .fitted)

price_g <-
    djt %>%
    filter(date > "2024-03-21") %>%
    ggplot(aes(x = date)) +
    geom_vline(xintercept = events$date, alpha = .05, linewidth = 2) +
    coord_cartesian(xlim = ymd(c(20240321), today() + weeks(2))) +
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
    geom_line(
        data = data_estimate,
        aes(y = .fitted, group = name), color = "gray60"
    )

vol_g <-
    djt %>%
    filter(date > "2024-03-21") %>%
    ggplot(aes(x = date)) +
    geom_vline(xintercept = events$date, alpha = .05, linewidth = 2) +
    geom_line(
        aes(y = DJT.Volume),
        lty = 1
    ) +
    scale_y_continuous(
        # limits = c(1, NA),
        labels = scales::label_number(scale = 1e-6, suffix = " M")
    ) +
    coord_cartesian(xlim = ymd(c(20240321), today() + weeks(2))) +
    labs(
        x = "",
        y = "$DJT sales volume"
    ) +
    # geom_vline(xintercept = ymd(20240721), alpha = .2)
    geom_point(
        data = events,
        aes(
            x = date,
            y = DJT.Volume
        ), size = 3
    ) +
    ggrepel::geom_label_repel(
        data = events,
        aes(
            x = date,
            y = DJT.Volume,
            label = event,
        ), point.padding = 1, nudge_y = 30e6, segment.linetype = 2
    )

ggsave("graphs/DJT_tracking.png", height = 8, width = 8, plot = price_g / vol_g)