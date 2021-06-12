options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         readr[read_csv],
         # import own project functions
         functions/ts[get_coefs, load_project_data, test_lag])

load_project_data()

lag_df <- data %>% 
  test_lag(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
             education_index + ghs + pop.km2 + democracy_index + 
             (1 | location), lag_var = res_pct_chng, n_lag = 50)

lag_df_deaths <- data %>% 
  test_lag(stringency_index ~ distrust_people + log(gdp_per_capita) + res_chng_lag_34 +
             education_index + ghs + pop.km2 + democracy_index + 
             (1 | location), lag_var = new_deaths_per_million, n_lag = 30)

deaths_simple <- data %>% 
  test_lag(stringency_index ~ distrust_people + (1 | location), 
           lag_var = new_deaths_per_million, n_lag = 50)

res_simple <- data %>% 
  test_lag(stringency_index ~ distrust_people + deaths_per_mil_lag_7 + 
             (1 | location), 
           lag_var = res_pct_chng, n_lag = 50)
res2 <- data %>% 
  test_lag(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
             education_index + ghs + pop.km2 + democracy_index +
             (1 | location), lag_var = res_pct_chng)

deaths_with_str <- data %>% 
  test_lag(stringency_index ~ distrust_people + stringency_index_lag_7 + (1 | location), 
           lag_var = new_deaths_per_million, n_lag = 50)
res_with_str <- data %>% 
  test_lag(stringency_index ~ distrust_people + stringency_index_lag_7 + (1 | location), 
           lag_var = res_pct_chng, n_lag = 50)


deaths_outcome <- data %>% 
  test_lag(new_deaths_per_million ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
             ghs + pop.km2 + democracy_index + ethnic + education_index + conflict_index +
             deaths_per_mil_lag_5 +
             (1 | location), n_lag = 50, lag_var = res_pct_chng)

res_outcome <- data %>% 
  test_lag(res_pct_chng ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
             ghs + pop.km2 + democracy_index + ethnic + education_index + conflict_index +
             deaths_per_mil_lag_5 + res_chng_lag_10 +
             (1 | location), n_lag = 50, lag_var = stringency_index)


















