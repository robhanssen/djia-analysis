
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