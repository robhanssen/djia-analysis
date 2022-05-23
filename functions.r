
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