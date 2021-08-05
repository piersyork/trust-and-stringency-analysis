
## function to get coefficients over specified time period across the year. Outputs a plot and df 
## showing how the coefficient changes over time.
#' @export
get_coefs = function(data, .formula, start = "2020-04-01", n_days = 4, 
                     method = "lmer", coef_position = 2, .labs = NULL,
                     print_plot = TRUE, ...) {
  box::use(lubridate[date],
           lme4[lmer],
           broom[tidy],
           lmtest[coeftest],
           ggplot2[...],
           magrittr[use_series, extract],
           sandwich[vcovCL, vcovHC],
           miceadds[lm.cluster],
           dplyr[...],
           stats[lm, median, na.omit])
  if (!plyr::is.formula(.formula)) {
    stop("not formula")
  }
  weekly_dates <- seq(date(x = start), by = n_days, len = 1000) 
  weekly_dates <- weekly_dates[weekly_dates <= date("2021-04-01")]
  
  estimates <- list()
  
  for (i in 1:length(weekly_dates)) {
    
    if (method == "lmer") {
      model_sum <- data %>% 
        filter(date %in% c(weekly_dates[[i]] + 0:(n_days-1))) %>% 
        lmer(formula = .formula, data = ., ...) %>% 
        summary()  
        
      coefficients <- model_sum$coefficients
      nobs <- model_sum$ngrps
      
      estimate <- coefficients[coef_position, 1]
      print(paste(estimate, " | ", weekly_dates[[i]]))
      std_err <- coefficients[coef_position, 2]
      estimates[[i]] <- tibble(estimate = estimate, std_err = std_err, nobs = nobs)
    }
    
    if (method == "lm") {
      
      coefficients <- data %>% 
        filter(date %in% c(weekly_dates[[i]] + 0:(n_days-1))) %>%
        lm(.formula, ., ...) %>% 
        coeftest(vcovCL, cluster = data$location[data$date %in% c(weekly_dates[[i]] + 0:(n_days-1))])
      
      estimate <- coefficients[coef_position, 1]
      print(paste(estimate, " | ", weekly_dates[[i]]))
      std_err <- coefficients[coef_position, 2]
      estimates[[i]] <- tibble(estimate = estimate, std_err = std_err)
    }

  }
  
  df_estimates <- bind_rows(estimates)
  df_estimates$date_start <- weekly_dates
  
  plot <- df_estimates %>% 
    ggplot(aes(date_start, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_ribbon(aes(ymin = estimate - std_err * 1.96,
                    ymax = estimate + std_err * 1.96), 
                alpha = 0.3, fill = "blue") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          plot.caption = element_text(hjust = 0))
  if (print_plot) print(plot) 
  
  if (is.null(.labs)) {
    plot <- plot + 
      labs(title = paste("The estimated effect of", all.vars(.formula)[coef_position], "on",
                         all.vars(.formula)[1], "\n", "for every", 
                         ifelse(n_days == 1, "day", paste(n_days, "days"))),
           caption = paste("Method:", 
                           ifelse(method == "lm", "linear regression", "random effects regression")))
  }
  
  out <- list(df_estimates, plot)
  
  names(out) <- c("df", "plot")
  
  return(out)
}


## function to import data for project
#' @export
load_project_data = function() {
  box::use(dplyr[...],
           readr[read_csv])
  load("Data/panel_data.Rdata")
  
  data <<- data %>% 
    filter(!is.na(distrust_people)) %>% 
    group_by(location) %>%
    arrange(date) %>%
    mutate(log_gdp = log(gdp_per_capita),
           log_conflict = log(conflict_index),
           deaths_per_mil_lag_5 = lag(new_deaths_per_million, 5),
           deaths_per_mil_lag_6 = lag(new_deaths_per_million, 6),
           deaths_per_mil_lag_7 = lag(new_deaths_per_million, 7),
           deaths_per_mil_lag_10 = lag(new_deaths_per_million, 10),
           deaths_per_mil_lag_14 = lag(new_deaths_per_million, 14),
           deaths_per_mil_lag_20 = lag(new_deaths_per_million, 20),
           deaths_per_mil_lag_28 = lag(new_deaths_per_million, 28),
           res_chng_lag_5 = lag(res_pct_chng, 5),
           res_chng_lag_6 = lag(res_pct_chng, 6),
           res_chng_lag_7 = lag(res_pct_chng, 7),
           res_chng_lag_10 = lag(res_pct_chng, 10),
           res_chng_lag_14 = lag(res_pct_chng, 14),
           res_chng_lag_20 = lag(res_pct_chng, 20),
           res_chng_lag_24 = lag(res_pct_chng, 24),
           res_chng_lag_28 = lag(res_pct_chng, 28),
           res_chng_lag_34 = lag(res_pct_chng, 34),
           res_chng_lag_40 = lag(res_pct_chng, 40),
           trans_chng_lag_5 = lag(trans_pct_chng, 5),
           trans_chng_lag_6 = lag(trans_pct_chng, 6),
           trans_chng_lag_7 = lag(trans_pct_chng, 7),
           trans_chng_lag_10 = lag(trans_pct_chng, 10),
           trans_chng_lag_14 = lag(trans_pct_chng, 14),
           trans_chng_lag_20 = lag(trans_pct_chng, 20),
           trans_chng_lag_24 = lag(trans_pct_chng, 24),
           trans_chng_lag_28 = lag(trans_pct_chng, 28),
           trans_chng_lag_34 = lag(trans_pct_chng, 34),
           trans_chng_lag_40 = lag(trans_pct_chng, 40),
           transport_reduction_lag_34 = trans_chng_lag_34*-1,
           stringency_index_lag_1 = lag(stringency_index, 1),
           stringency_index_lag_2 = lag(stringency_index, 2),
           stringency_index_lag_4 = lag(stringency_index, 4),
           stringency_index_lag_7 = lag(stringency_index, 7),
           max_stringency = max(stringency_index, na.rm = TRUE),
           democracy_index_2 = democracy_index^2) %>% 
    ungroup() %>%  
    mutate(trusting = ifelse(distrust_people < stats::median(distrust_people), "High trust", "Low trust"),
           distrust_people = distrust_people * 100,
           conf_govt = conf_govt * 100) 
  
  
  weekly_data <<- read_csv("Data/weekly_data.csv") %>% 
    group_by(location) %>% 
    arrange(date) %>% 
    mutate(excess_lag_1 = lag(excess_deaths_per_100k, 1),
           excess_lag_2 = lag(excess_deaths_per_100k, 2),
           excess_lag_3 = lag(excess_deaths_per_100k, 3),
           excess_lag_4 = lag(excess_deaths_per_100k, 4),
           res_chng_lag_1 = lag(res_pct_chng, 1),
           res_chng_lag_2 = lag(res_pct_chng, 2),
           res_chng_lag_3 = lag(res_pct_chng, 3),
           res_chng_lag_4 = lag(res_pct_chng, 4),
           stringency_index_lag_1 = lag(stringency_index, 1),
           stringency_index_lag_2 = lag(stringency_index, 2),
           stringency_index_lag_3 = lag(stringency_index, 3),
           stringency_index_lag_4 = lag(stringency_index, 4),
           deaths_per_100_lag_1 = lag(daily_deaths_100k, 1),
           deaths_per_100_lag_2 = lag(daily_deaths_100k, 2),
           deaths_per_100_lag_3 = lag(daily_deaths_100k, 3),
           deaths_per_100_lag_4 = lag(daily_deaths_100k, 4)) %>% 
    ungroup()
}

## function to test effect of variable at various time lags
#' @export
test_lag = function(data, .formula, n_lag = 30, lag_var,
                    method = "lmer", ...) {
  box::use(lubridate[date],
           lme4[lmer],
           broom[tidy],
           lmtest[coeftest],
           ggplot2[...],
           magrittr[use_series, extract],
           sandwich[vcovCL, vcovHC],
           dplyr[...],
           stats[lm, median, na.omit, update])
  lag_var = enquo(lag_var)
  estimates <- list()
  for (i in 1:n_lag) {
    coefficients <- data %>% 
      group_by(location) %>%
      arrange(date) %>% 
      mutate(main_lag = lag(!!lag_var, i)) %>% 
      lmer(formula = update(.formula, ~ main_lag + .), data = ., ...) %>% 
      summary() %>% 
      use_series(coefficients) 
    
    estimate <- coefficients[2, 1]
    print(paste(estimate, " | ", "lag", i))
    std_err <- coefficients[2, 2]
    estimates[[i]] <- tibble(estimate = estimate, std_err = std_err)
  }
  
  
  df_estimates <- bind_rows(estimates)
  df_estimates$lag <- 1:n_lag
  
  plot <- df_estimates %>% 
    ggplot(aes(lag, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_ribbon(aes(ymin = estimate - std_err * 1.96,
                    ymax = estimate + std_err * 1.96), 
                alpha = 0.3, fill = "blue") +
    labs(title = paste0("Effect of ", as_label(lag_var), " on ", all.vars(.formula)[1], "\n",
                        "for increasing levels of lag")) +
    theme_minimal() +
    theme(axis.title = element_blank())
  print(plot)
  output <- list(df_estimates, plot)
  names(output) <- c("df", "plot")
  return(output)
}

## function to plot relationship between two variables using averaged level for timeseries variables
#' @export
plot_summarised = function(data, x, y, .poly = 1) {
  box::use(dplyr[...],
           ggplot2[...],
           stats[na.omit, lm, poly])
  x <- enquo(x)
  y <- enquo(y)
  data %>% 
    select(!!x, !!y, location) %>% 
    na.omit() %>% 
    group_by(location) %>% 
    summarise(var_x = mean(!!x), 
              var_y = mean(!!y)) %>% 
    ggplot(aes(var_x, var_y, label = location)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ poly(x, .poly, raw = TRUE)) +
    labs(x = as_label(x), y = as_label(y))
}
# data %>% 
#   plot_summarised(conf_govt, stringency_index)

#' @export
plot_sample_map = function(data, map_tidy, .formula) {
  box::use(dplyr[...],
           stats[...],
           ggplot2[...],
           functions/ts[effective_sample])
  
  df <- effective_sample(data, .formula)

  
  map_tidy %>% 
    left_join(df, by = c("id" = "location")) %>% 
    mutate(weights = ifelse(!is.na(weights), weights, 0)) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = weights)) +
    geom_polygon(show.legend = FALSE, color = "grey", size = 0.1) + #
    scale_fill_continuous(high = "black", low = "white", na.value = "white") + #3182bd
    #scale_fill_distiller(direction = 1) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.05), 
          plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))
  
}

#' @export
effective_sample = function(data, .formula, model = NULL) {
  box::use(dplyr[...],
           stats[...],
           ggplot2[...],
           tibble[rownames_to_column])
  ## Aronow & Samii (2015) method to get country weights
  # run regression model
  effc_reg <- lm(.formula, data)
  
  
  # subset to remove any omitted variable 
  all_vars <- effc_reg$model %>% colnames()
  
  data_sub <- data %>%
    select(all_of(all_vars), location) %>% 
    na.omit()
  
  
  # residuals
  d.tilde <- as.numeric(residuals(effc_reg))
  w <- d.tilde^2
  
  # weights
  w1 <- tapply(w, data_sub$location, mean)
  
  # data frame of countries and weights
  df <- as.data.frame(w1) %>% 
    mutate(weights_pct = round((w1/sum(w1))*100, 4),
           weights = (w1/max(w1))) %>% 
    arrange(desc(weights)) %>% 
    rownames_to_column(var = "location")
  return(df)
  
}

#' @export
table_continents <- function(data) {
  box::use(dplyr[...])
  
  continents <- data$continent %>% unique()
  countries_in <- list()
  for (i in 1:length(continents)) {
    countries_in[[i]] <- data %>% 
      filter(continent == continents[i]) %>% 
      pull(location)
  }
  names(countries_in) <- continents
  
  big_length <- 0
  for (i in 1:length(countries_in)) {
    test_length <- length(countries_in[[i]])
    if (test_length > big_length) {
      big_length <- test_length
    }
  }
  
  for (i in 1:length(countries_in)) {
    while (length(countries_in[[i]]) < big_length) {
      countries_in[[i]] <- c(countries_in[[i]], "")
    }
  }
  
  return(as_tibble(countries_in))
}











