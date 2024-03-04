line_graph <- predictions %>%
    ggplot() +
    aes(date, .fitted, color = index) +
    facet_wrap(~index, scales = "free_y") +
    geom_line(, show.legend = FALSE) +
    geom_line(data = stockindex, aes(y = value), show.legend = FALSE) +
    labs(y = NULL, title = "Stock index values with linear model")


rel_graph <- predictions %>%
    ggplot() +
    aes(date, rel_residue, color = index) +
    geom_line(, show.legend = FALSE) +
    labs(y = NULL, title = "Relative deviation from linear model")

wigglemod_byindex <-
    predictions %>%
    mutate(time = as.numeric(date - cuttoff_date)) %>%
    group_by(index) %>%
    nest() %>%
    mutate(sinemod = map(data, ~ sinemodel(.)))

wigglemod_byindex %>%
    mutate(params = map(sinemod, broom::tidy)) %>%
    unnest(params) %>%
    select(index, term, estimate) %>%
    pivot_wider(names_from = "term", values_from = "estimate")

wigglemod_byindex %>%
    mutate(params = map(
        sinemod,
        ~ broom::augment(.x, newdata = tibble(time = 1:365))
    )) %>%
    unnest(params) %>%
    mutate(date = cuttoff_date + days(time) - 1) %>%
    ggplot() +
    aes(date, .fitted, color = index) +
    facet_wrap(~index, scales = "free_y") +
    geom_line(, show.legend = FALSE) +
    geom_line(data = predictions, aes(y = rel_residue), show.legend = FALSE)



wigglemod_all <-
    predictions %>%
    mutate(time = as.numeric(date - cuttoff_date)) %>%
    # group_by(index) %>%
    nest(data = everything()) %>%
    mutate(sinemod = map(data, ~ sinemodel(.)))

wigglemod_all %>%
    mutate(params = map(sinemod, broom::tidy)) %>%
    unnest(params) %>%
    select(term, estimate) %>%
    pivot_wider(names_from = "term", values_from = "estimate")



sine_graph <- wigglemod_all %>%
    mutate(params = map(
        sinemod,
        ~ broom::augment(.x, newdata = tibble(time = 1:365))
    )) %>%
    unnest(params) %>%
    mutate(date = cuttoff_date + days(time) - 1) %>%
    ggplot() +
    aes(date, .fitted) +
    geom_line(, show.legend = FALSE) +
    geom_line(data = predictions, aes(y = rel_residue), show.legend = FALSE) +
    labs(y = NULL, title = "Relative deviation periodic model")

p_all <- (line_graph) / (rel_graph + sine_graph) +
    plot_annotation(title = "Modeling stock indices")

ggsave("graphs/wiggle_model_explanation.png",
    width = 12, height = 6, plot = p_all
)
