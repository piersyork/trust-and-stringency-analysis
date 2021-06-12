options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         rgdal[readOGR],
         broom[tidy],
         lubridate[date, as_date],
         magrittr[use_series, extract],
         readr[read_csv],
         # import own project functions
         functions/ts[get_coefs, load_project_data, test_lag])

# imports data for the project (function defined in functions/ts.r)
load_project_data()


# Set theme
my_theme <- theme_minimal() +
  theme(legend.position = "top",
        axis.line.x = element_line(), axis.ticks.x = element_line(),
        plot.caption = element_text(hjust = 0))

theme_set(my_theme)

## Initial models
# pooled regression clustered SE on location
pooled1 <- data %>% 
  lm(formula = stringency_index ~ distrust_people) %>% 
  coeftest(., vcovCL, cluster = ~location)
screenreg(pooled1)

# regression with country averaged variables: aprox same as pooled model
data %>% 
  select(location, stringency_index, distrust_people) %>% 
  na.omit() %>% 
  group_by(location) %>% 
  summarise(distrust_people = mean(distrust_people), stringency_index = mean(stringency_index)) %>% 
  lm(stringency_index ~ distrust_people, .) %>% 
  screenreg()

# time fixed model
modelFixed <- data %>% plm(stringency_index ~ distrust_people + deaths_per_mil_lag_7, 
                           ., index = c("location", "date"), effect = "time") %>% 
  coeftest(., vcovHC, cluster = "group")
screenreg(modelFixed)

# simple random effects model
model1 <- data %>% 
  lmer(stringency_index ~ distrust_people +
         (1 | location), .)
screenreg(model1)

# adding deaths lag to random effects
model2 <- data %>% 
  lmer(stringency_index ~ distrust_people + deaths_per_mil_lag_7 + 
         (1 | location), .)
screenreg(model2)

# adding res_pct_chng with 28 days of lag
model3 <- data %>% 
  lmer(stringency_index ~ distrust_people + deaths_per_mil_lag_7 + res_chng_lag_28 +
         log(gdp_per_capita) +
         (1 | location), .)
screenreg(model3)

# adding other country fixed variables
model4 <- data %>% 
  lmer(stringency_index ~ distrust_people + res_chng_lag_7 + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + sch_enrol_per_cap +
         (1 | location), .)

screenreg(model4)

# multivariate pooled model with country clustered SE
pooled4 <- data %>% 
  lm(stringency_index ~ distrust_people + log(gdp_per_capita) + lag(new_deaths_per_million, 14) +
       lag(res_pct_chng, 7) + ghs + pop.km2 + democracy_index + ethnic + sch_enrol_per_cap, .) %>% 
  coeftest(vcovCL, cluster = ~location)
screenreg(list(pooled4, model4))

screenreg(list(model1, model2, model3, model4))

screenreg(list(pooled1, model1, pooled4, model4), 
          custom.gof.rows = list("Random Effects" = c("NO", "YES", "NO", "YES")),
          omit.coef = "Intercept")

## explore data
# countries in order of the highest peek stringency
data %>% 
  select(location, stringency_index) %>% 
  group_by(location) %>% 
  summarise(max_stringency = max(stringency_index, na.rm = TRUE)) %>% 
  arrange(max_stringency) %>% 
  print(n = 100)

# countries in order of level of distrust
data %>% 
  select(location, distrust_people, conf_govt) %>% 
  distinct() %>% 
  arrange(distrust_people) %>% 
  print(n = 100)
  

# how the effect of distrust changes over time (get_coefs is defined in functions/ts.r)
lmer_time <- data %>% 
  get_coefs(stringency_index ~ distrust_people + log(gdp_per_capita) + sch_enrol_per_cap +
              deaths_per_mil_lag_7 + res_chng_lag_7 + ghs + pop.km2 + democracy_index + ethnic +
              (1 | location), n_days = 7)
lmer_time$plot

# same but with the coef on deaths lag. coef_position determines which coef to plot
lmer_time_deaths <- data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_7 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
              (1 | location), n_days = 7, coef_position = 4)

# coef on distrust in a OLS model over time
lm_time <- data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_7 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + sch_enrol_per_cap,
            n_days = 7, method = "lm", coef_position = 2)

# same but for deaths coef
lm_time_deaths <- data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_14 + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic,
            n_days = 7, method = "lm", coef_position = 4)

# playing around with more model specs
data %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + sch_enrol_per_cap +
         deaths_per_mil_lag_7 + res_chng_lag_7 + ghs + pop.km2 + democracy_index + ethnic +
         (1 | location), .) %>% 
  screenreg()

data %>%
  lmer(res_pct_chng ~ distrust_people + stringency_index + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
         (1 | location), .) %>% 
  screenreg()

# modeling the effect of distrust on res_chng over time
rpc_time <- data %>% 
  get_coefs(res_pct_chng ~ distrust_people + stringency_index + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
              (1 | location), n_days = 7, coef_position = 4)

rpc_lm <- data %>% 
  get_coefs(res_pct_chng ~ distrust_people + stringency_index + deaths_per_mil_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic,
            n_days = 1, method = "lm", coef_position = 3)
ef_death <- data %>% 
  get_coefs(new_deaths_per_million ~ distrust_people + stringency_index + 
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
              (1 | location),
            n_days = 7, method = "lmer", coef_position = 3)


weekly_times <- weekly_data %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_2 + excess_lag_1 +
                            log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
                            (1 | location), n_days = 15)

weekly_excess <- weekly_data %>%
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(deaths_lag_7 = lag(new_deaths_per_million, 7)) %>% 
  get_coefs(res_pct_chng ~ distrust_people + deaths_lag_7 + sch_enrol_per_cap +
              log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
              (1 | location), n_days = 15)

weekly_data %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(deaths_lag_7 = lag(new_deaths_per_million, 7)) %>% 
  lmer(res_pct_chng ~ distrust_people + deaths_lag_7 + sch_enrol_per_cap +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
         (1 | location), .) %>% 
  screenreg()

lag_df <- data %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(res_chng_lag_80 = lag(res_pct_chng, 80),
         stringency_index_lag_1 = lag(stringency_index, 2)) %>% 
  test_lag(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
             sch_enrol_per_cap + ghs + pop.km2 + democracy_index + stringency_index_lag_1 +
             (1 | location), lag_var = res_pct_chng, n_lag = 50)

lag_df_deaths <- data %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(res_chng_lag_32 = lag(res_pct_chng, 32),
         stringency_index_lag_1 = lag(stringency_index, 1)) %>% 
  test_lag(stringency_index ~ distrust_people + log(gdp_per_capita) + res_chng_lag_32 +
             sch_enrol_per_cap + ghs + pop.km2 + democracy_index + stringency_index_lag_1 +
             (1 | location), lag_var = new_deaths_per_million, n_lag = 30)

data %>%
  get_coefs(stringency_index ~ distrust_people + log(gdp_per_capita) + sch_enrol_per_cap +
              deaths_per_mil_lag_7 + res_chng_lag_28 + ghs + pop.km2 + democracy_index + ethnic +
              lag(stringency_index, 1) +
              (1 | location), n_days = 7)
data %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + sch_enrol_per_cap +
         deaths_per_mil_lag_7 + res_chng_lag_28 + ghs + pop.km2 + democracy_index + ethnic +
         lag(stringency_index, 1) +
         (1 | location), .) %>% 
  screenreg()



data %>% 
  plm(stringency_index ~ distrust_people + res_chng_lag_14 + deaths_per_mil_lag_14 +
        log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic,
      ., effect = "time", index = c("location", "date")) %>% 
  coeftest(., vcov. = vcovHC(.), cluster = "group") %>% 
  screenreg()

 









