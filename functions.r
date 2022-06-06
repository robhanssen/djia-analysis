
period_to_dates <- function(pd) {
    daterange <- floor_date(
        seq(
            as.Date(int_start(pd)),
            today() + years(1),
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