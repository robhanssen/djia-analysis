
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

collect_online_data <- function() {
    quantmod::getSymbols("SP500", src = "FRED")
    quantmod::getSymbols("DJIA", src = "FRED")
    quantmod::getSymbols("NASDAQCOM", src = "FRED")

    bind_rows(
        tibble(
            date = index(SP500),
            value = as.numeric(SP500),
            index = "sp500"
        ),
        tibble(
            date = index(DJIA),
            value = as.numeric(DJIA),
            index = "djia"
        ),
        tibble(
            date = index(NASDAQCOM),
            value = as.numeric(NASDAQCOM),
            index = "nasdaq"
        )
    ) %>%
        arrange(date) %>%
        filter(date > ymd(20130101))
}

predict_index <- function(tbl, daterange) {
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