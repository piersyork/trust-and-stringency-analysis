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

.formula <- stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         ghs + ethnic + pop.km2 + polity2 + log_conflict + conf_govt + deaths_per_mil_lag_5 +
         (1 | location)


# how the effect of distrust changes over time (get_coefs is defined in functions/ts.r)
lmer_time <- data %>% 
  get_coefs(.formula, method = "lmer", n_days = 28, start = "2020-04-01")

lmer_time$df %>% 
  ggplot(aes(date_start, nobs)) +
  geom_line()

# same but with the coef on deaths lag. coef_position determines which coef to plot
lmer_time_deaths <- data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_34 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index +
              (1 | location), n_days = 14, coef_position = 4)

# coef on distrust in a OLS model over time
form <- stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
  conflict_index + continent + deaths_per_mil_lag_5 +
  log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index

lm_time <- data %>% 
  get_coefs(form, n_days = 1, method = "lm", coef_position = 2, start = "2020-04-01")

lm_time$df$estimate %>% mean()

data %>% 
  lmer(update(form, ~ . + (1 | location)), .) %>% 
  screenreg()

data %>% 
  lm(form, .) %>% 
  screenreg()

data %>% 
  plm(form, ., index = c("location", "date"), effect = "time") %>% 
  summary() %>% 
  use_series(coefficients) %>% 
  data.frame() %>% 
  use_series(Estimate) %>% 
  extract(1)
  

# same but for deaths coef
lm_time_deaths <- data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_34 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index,
            n_days = 1, method = "lm", coef_position = 4)

# playing around with more model specs
data %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + sch_enrol_per_cap +
         deaths_per_mil_lag_7 + res_chng_lag_7 + ghs + pop.km2 + polity2 + ethnic +
         (1 | location), .) %>% 
  screenreg()

data %>%
  lmer(rtl_pct_chng ~ distrust_people + stringency_index + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic +
         (1 | location), .) %>% 
  screenreg()

# modeling the effect of distrust on res_chng over time
rpc_time <- data %>% 
  get_coefs(res_pct_chng ~ distrust_people + stringency_index + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic +
              (1 | location), n_days = 7, coef_position = 2)

rpc_lm <- data %>% 
  get_coefs(res_pct_chng ~ distrust_people + stringency_index_lag_7 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic,
            n_days = 1, method = "lm", coef_position = 2, start = "2020-03-01")


# effect of distrust on deaths
deaths_lm <- data %>% 
  get_coefs(new_deaths_per_million ~ distrust_people + stringency_index + 
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic,
            n_days = 1, method = "lm", coef_position = 2, start = "2020-03-01")

# in random effects
ef_death <- data %>% 
  get_coefs(new_deaths_per_million ~ distrust_people + stringency_index_lag_7 + 
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic +
              (1 | location),
            n_days = 7, method = "lmer", coef_position = 2, start = "2020-03-17")
ef_death



weekly_times <- weekly_data %>% 
  get_coefs(stringency_index ~ distrust_people + excess_lag_1 +
              log(gdp_per_capita) + ghs + pop.km2 + 
              polity2 + ethnic + 
              (1 | location), n_days = 14)
weekly_times_lm <- weekly_data %>% 
  get_coefs(stringency_index ~ distrust_people + excess_lag_1 +
              log(gdp_per_capita) + ghs + pop.km2 + 
              polity2 + ethnic,
            n_days = 7, method = "lm")

weekly_data %>% 
  get_coefs(excess_deaths_per_100k ~ distrust_people + stringency_index + 
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic,
            n_days = 7, method = "lmer", coef_position = 2)

str_str <- data %>% 
  get_coefs(stringency_index ~ distrust_people + log(gdp_per_capita) + education_index +
              deaths_per_mil_lag_7 + res_chng_lag_7 + 
              ghs + pop.km2 + polity2 + ethnic +
              (1 | location), n_days = 7)



lm_test <- data %>% 
  lm(stringency_index ~ distrust_people, .)
rob <- coeftest(lm_test, vcovCL, cluster = ~location)
rob


## measuring effect of confidence in government
conf_form <- stringency_index ~ conf_govt + deaths_per_mil_lag_7 + 
  distrust_people + conflict_index + log(gdp_per_capita) + ghs + 
  pop.km2 + polity2 + ethnic + sch_enrol_per_cap


lmer_conf <- data %>% 
  get_coefs(update(conf_form, ~ . + (1 | location)), n_days = 4)

lm_conf <- data %>% 
  get_coefs(conf_form, n_days = 1, method = "lm", coef_position = 2)
data %>% 
  lm(stringency_index ~ conf_govt, .) %>% 
  coeftest(vcovCL, cluster = ~location) %>% 
  screenreg()

lm_conf_res <- data %>% 
  get_coefs(update(conf_form, res_pct_chng ~ .),
            n_days = 1, method = "lm")

## total deaths instead of new deaths
daily_coef_total <- data %>% 
  get_coefs(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
              log(conflict_index) + continent + deaths_per_mil_lag_5 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index,
            n_days = 1, method = "lm")
lmer_total_d <- data %>% 
  get_coefs(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
              log(conflict_index) + continent + deaths_per_mil_lag_5 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index +
              (1 | location),
            n_days = 7)


total_deaths <- data %>% 
  get_coefs(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
              log(conflict_index) + continent + deaths_per_mil_lag_5 +
              log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic + education_index,
            n_days = 1, method = "lm", coef_position = 4)
data %>% 
  get_coefs(stringency_index ~ total_deaths_per_million, method = "lm", n_days = 1)

data %>% 
  ggplot(aes(total_deaths_per_million, new_deaths_per_million)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm)






