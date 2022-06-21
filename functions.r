options("getSymbols.warning4.0" = FALSE)

period_to_dates <- function(pd, yrs = 1) {
    daterange <- floor_date(
        seq(
            as.Date(int_start(pd)),
            today() + years(yrs),
            by = "month"
        ),
        unit = "month"
    )
    tibble::tibble(date = daterange)
}

# function obseleted in favor of collect_online_data()
collect_data <- function() {
    map_df(
        list.files(path = "source/", pattern = "*.csv", full.names = TRUE),
        function(x) {
            readr::read_csv(x, na = ".")
        }
    ) %>%
        pivot_longer(!date, names_to = "index", values_to = "value") %>%
        arrange(date, index) %>%
        filter(!is.na(value))
}


get_index <- function(index, src = "FRED") {
    t <- quantmod::getSymbols(index, src = src, auto.assign = FALSE)
    tibble::tibble(
        date = zoo::index(t),
        value = as.numeric(t),
        index = index
    )
}

collect_online_data <- function(indices = c("SP500", "DJIA", "NASDAQCOM")) {
    purrr::map_df(indices, ~ get_index(.x)) %>%
        arrange(date) %>%
        filter(date > ymd(20130101))
}

predict_index_choice <- function(tbl, daterange, regtype = "exp") {
    tbl %>%
        filter(date %within% daterange) %>%
        group_by(index) %>%
        nest() %>%
        mutate(model = case_when(regtype == "exp"  ~ map(data, ~ lm(log10(value) ~ date, data = .)),
                                 regtype == "linear"  ~ map(data, ~ lm(value ~ date, data = .)),
                                 TRUE  ~ map(data, ~ lm(log10(value) ~ date, data = .))
        )) %>%
        mutate(modeldata = map(
            model,
            ~ broom::augment(.x,
                interval = "prediction",
                newdata = period_to_dates(daterange)
            )
        )) %>%
        unnest(modeldata) %>%
        mutate(
            .fitted = ifelse(regtype == "exp", 10^.fitted, .fitted),
            .lower = ifelse(regtype == "exp", 10^.lower, .lower),
            .upper = ifelse(regtype == "exp", 10^.upper, .upper)
        )
}

predict_index <- function(tbl, daterange) {
    tbl %>%
        filter(date %within% daterange) %>%
        group_by(index) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(log10(value) ~ date, data = .))) %>%
        mutate(modeldata = map(
            model,
            ~ broom::augment(.x,
                interval = "prediction",
                newdata = period_to_dates(daterange)
            )
        )) %>%
        unnest(modeldata) %>%
        mutate(
            .fitted = 10^.fitted,
            .lower = 10^.lower,
            .upper = 10^.upper
        )
}

predict_index_linear <- function(tbl, daterange) {
    tbl %>%
        filter(date %within% daterange) %>%
        group_by(index) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(value ~ date, data = .))) %>%
        mutate(modeldata = map(
            model,
            ~ broom::augment(.x,
                interval = "prediction",
                newdata = period_to_dates(daterange)
            )
        )) %>%
        unnest(modeldata)
}

indiv_graph <- function(idx, tbl, training) {
    stock <-
        tbl %>% filter(index == idx)

    predict <-
        predict_index(stock, training)

    index_max <-
        max(stock$value, na.rm = TRUE)

    g <-
        stock %>%
        ggplot() +
        aes(date, value, color = idx) +
        geom_point(alpha = .2, size = .2) +
        geom_line(
            data = predict,
            aes(y = .fitted, color = index),
            lty = 1,
            size = .25
        ) +
        geom_ribbon(
            data = predict,
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
            date_breaks = "1 year",
            date_labels = "%Y",
            minor_breaks = NULL
        ) +
        scale_y_continuous(
            label = scales::comma_format(),
            sec.axis = sec_axis(~ . / index_max,
                    labels = scales::percent_format())
        ) +
        labs(
            title = idx,
            x = "Date",
            y = "Stock Index"
        ) +
        theme(legend.position = "none") +
        scale_color_manual(values = color) +
        scale_fill_manual(values = color)

    fname <- paste0("graphs/", idx, ".png")

    ggplot2::ggsave(fname, width = 6, height = 6, plot = g)
}
