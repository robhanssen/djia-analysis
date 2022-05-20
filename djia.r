library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

djia <-
    read_csv("djia.csv") %>%
    janitor::clean_names() %>%
    mutate(djia = as.numeric(djia)) %>%
    filter(!is.na(djia))

train_djia_1 <- ymd("2013-06-18") %--% ymd("2015-03-06")

trainingperiod_djia_1 <- tibble(early = int_start(train_djia_1), late = int_end(train_djia_1)) %>%
    mutate(across(everything(), ~as.Date(.x)))

data_1 <- floor_date(
    seq(
        ymd("2013-06-18"),
        today() + years(1),
        by = "month"
    ),
    unit = "month"
)

train_djia_2 <- ymd("2017-10-01") %--% ymd("2019-10-10")

trainingperiod_djia_2 <- tibble(early = int_start(train_djia_2), late = int_end(train_djia_2)) %>%
    mutate(across(everything(), ~as.Date(.x)))

data_2 <- floor_date(
    seq(
        ymd("2017-10-01"),
        today() + years(1),
        by = "month"
    ),
    unit = "month"
)

model_djia_1 <- djia %>%
    filter(date %within% train_djia_1) %>%
    lm(djia ~ date, data = .)

extra_djia_1 <-
    broom::augment(model_djia_1, newdata = tibble(date = data_1), interval = "prediction")

model_djia_2 <- djia %>%
    filter(date %within% train_djia_2) %>%
    lm(djia ~ date, data = .)

extra_djia_2 <-
    broom::augment(model_djia_2, newdata = tibble(date = data_2), interval = "prediction")

max_djia <- max(djia$djia)



djia_graph <-
    djia %>%
    ggplot() +
    aes(date, djia) +
    geom_point(alpha = .1, size = .2) +
    geom_line(data = extra_djia_1, aes(y = .fitted), color = "red", lty = 3, size = .8) +
    geom_line(data = extra_djia_2, aes(y = .fitted), color = "darkgreen", lty = 3, size = .8) +
    geom_ribbon(data = extra_djia_1, aes(y= NULL, ymin = .lower, ymax = .upper), alpha = .1, fill = "red") +
    geom_ribbon(data = extra_djia_2, aes(y= NULL, ymin = .lower, ymax = .upper), alpha = .1, fill = "darkgreen") +
    scale_x_date(
        date_breaks = "2 year",
        date_labels = "%Y",
        minor_breaks = NULL
    ) +
    scale_y_continuous(
        breaks = seq(0, 1e5, 5e3), label = scales::comma_format(),
        sec.axis = sec_axis(~ . / max_djia, name = "Relative to peak value (%)", label = scales::percent_format())
    ) +
    labs(
        x = "Date",
        y = "Dow Jones Industrial Index"
    ) + 
    geom_errorbar(data = trainingperiod_djia_1, aes(y = 12500, x = NULL, xmin = early, xmax = late), width = 1000, color = "red")  +
    geom_errorbar(data = trainingperiod_djia_2, aes(y = 12500, x = NULL, xmin = early, xmax = late), width = 1000, color = "darkgreen")

ggsave("djia-prediction.png", width = 6, height = 6, plot = djia_graph)

###
#
#
#
###

sp500 <-
    read_csv("sp500.csv") %>%
    janitor::clean_names() %>%
    mutate(sp500 = as.numeric(sp500)) %>%
    filter(!is.na(sp500))

train_snp500_1 <- ymd("2013-06-18") %--% ymd("2015-03-06")
data_1 <- floor_date(
    seq(
        ymd("2013-06-18"),
        today() + years(1),
        by = "month"
    ),
    unit = "month"
)

train_snp500_2 <- ymd("2017-10-01") %--% ymd("2019-10-10")
data_2 <- floor_date(
    seq(
        ymd("2017-10-01"),
        today() + years(1),
        by = "month"
    ),
    unit = "month"
)

model_snp500_1 <- sp500 %>%
    filter(date %within% train_snp500_1) %>%
    lm(sp500 ~ date, data = .)

extra_snp500_1 <-
    broom::augment(model_snp500_1, newdata = tibble(date = data_1), interval = "prediction")

model_snp500_2 <- sp500 %>%
    filter(date %within% train_snp500_2) %>%
    lm(sp500 ~ date, data = .)

extra_snp500_2 <-
    broom::augment(model_snp500_2, newdata = tibble(date = data_2), interval = "prediction")

max_sp500 <- max(sp500$sp500)

sp500_graph <-
    sp500 %>%
    ggplot() +
    aes(date, sp500) +
    geom_point(alpha = .1, size = .2) +
    geom_line(data = extra_snp500_1, aes(y = .fitted), color = "red", lty = 3, size = .8) +
    geom_line(data = extra_snp500_2, aes(y = .fitted), color = "darkgreen", lty = 3, size = .8) +
    geom_ribbon(data = extra_snp500_1, aes(y= NULL, ymin = .lower, ymax = .upper), alpha = .1, fill = "red") +
    geom_ribbon(data = extra_snp500_2, aes(y= NULL, ymin = .lower, ymax = .upper), alpha = .1, fill = "darkgreen") +    
    scale_x_date(
        date_breaks = "2 year",
        date_labels = "%Y",
        minor_breaks = NULL
    ) +
    scale_y_continuous(
        breaks = seq(0, 1e5, 5e2), label = scales::comma_format(),
        sec.axis = sec_axis(~ . / max_sp500, name = "Relative to peak value (%)", label = scales::percent_format())
    ) +
    labs(
        x = "Date",
        y = "S&P 500 Index"
    ) + 
    geom_errorbar(data = trainingperiod_djia_1, aes(y = 1250, x = NULL, xmin = early, xmax = late), width = 100, color = "red") +
    geom_errorbar(data = trainingperiod_djia_2, aes(y = 1250, x = NULL, xmin = early, xmax = late), width = 100, color = "darkgreen")

ggsave("sp500-prediction.png", width = 6, height = 6, plot = sp500_graph)




ggsave("combined_prediction_djiasp500.png", width = 12, height = 6, plot = djia_graph + sp500_graph)


#
# variant graph based on level at 12/21/2021
#
#

djia_rob_max <- djia %>% filter(date == "2021-12-17") %>% pull(djia)
sp500_rob_max <- sp500 %>% filter(date == "2021-12-17") %>% pull(sp500)


djia_graph_rob <-
    djia %>%
    ggplot() +
    aes(date, djia) +
    geom_point(alpha = .1, size = .2) +
    geom_line(data = extra_djia_1, aes(y = .fitted), color = "red", lty = 3, size = .8) +
    geom_line(data = extra_djia_2, aes(y = .fitted), color = "darkgreen", lty = 3, size = .8) +
    scale_x_date(
        date_breaks = "2 year",
        date_labels = "%Y",
        minor_breaks = NULL
    ) +
    scale_y_continuous(
        breaks = seq(0, 1e5, 5e3), label = scales::comma_format(),
        sec.axis = sec_axis(~ . / djia_rob_max, 
                        name = "Relative to peak value (%)", 
                        breaks = .1 * 0:10,
                        label = scales::percent_format())
    ) +
    labs(
        x = "Date",
        y = "Dow Jones Industrial Index"
    )

sp500_graph_rob <-
    sp500 %>%
    ggplot() +
    aes(date, sp500) +
    geom_point(alpha = .1, size = .2) +
    geom_line(data = extra_snp500_1, aes(y = .fitted), color = "red", lty = 3, size = .8) +
    geom_line(data = extra_snp500_2, aes(y = .fitted), color = "darkgreen", lty = 3, size = .8) +
    scale_x_date(
        date_breaks = "2 year",
        date_labels = "%Y",
        minor_breaks = NULL
    ) +
    scale_y_continuous(
        breaks = seq(0, 1e5, 5e2), label = scales::comma_format(),
        sec.axis = sec_axis(~ . / sp500_rob_max, 
                            name = "Relative to peak value (%)", 
                            breaks = .1 * 0:10,
                            label = scales::percent_format())
    ) +
    labs(
        x = "Date",
        y = "S&P 500 Index"
    )

p <- djia_graph_rob + sp500_graph_rob

ggsave("combined-graph-robs-account.png", width = 12, height = 6, plot = p)