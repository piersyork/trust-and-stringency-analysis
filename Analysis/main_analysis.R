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
  lmer(stringency_index ~ distrust_people + deaths_per_mil_lag_7 + res_chng_lag_34 +
         log(gdp_per_capita) +
         (1 | location), .)
screenreg(model3)

# adding other country fixed variables
model4 <- data %>% 
  lmer(stringency_index ~ distrust_people + res_chng_lag_34 + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .)
screenreg(model4)

# add lagged stringency to right hand side of formula
model5 <- data %>% 
  lmer(stringency_index ~ distrust_people + continent + res_chng_lag_34 + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         stringency_index_lag_1 +
         (1 | location), .)
screenreg(model5)

# no res chng
model6 <- data %>% 
  lmer(stringency_index ~ distrust_people + continent + deaths_per_mil_lag_5 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .)
screenreg(model6)

# including almost all variables and playing around with logged vars. Exclude Nicaragua because outlier?
model7 <- data %>% 
  # filter(!location == "Nicaragua") %>%
  lmer(stringency_index ~ distrust_people + deaths_per_mil_lag_5 + conf_govt + 
         log(conflict_index) + continent +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .)
screenreg(model7)

# include total deaths
model8 <- data %>% 
  # filter(!location == "Nicaragua") %>%
  lmer(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
         log(conflict_index) + continent + deaths_per_mil_lag_5 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .)
screenreg(model8)

# multivariate pooled model with country clustered SE
pooled4 <- data %>% 
  lm(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
       res_chng_lag_34 + ghs + pop.km2 + democracy_index + ethnic + education_index, .) %>% 
  coeftest(vcovCL, cluster = ~location)
screenreg(list(pooled4, model4))

screenreg(list(model1, model2, model3, model4, model5))

screenreg(list(pooled1, model1, pooled4, model4), 
          custom.gof.rows = list("Random Effects" = c("NO", "YES", "NO", "YES")),
          omit.coef = "Intercept")




# what countries are in model
data %>% 
  select(location, stringency_index, distrust_people, gdp_per_capita, res_chng_lag_34, deaths_per_mil_lag_7,
         ghs, pop.km2, democracy_index, ethnic, education_index, continent, conflict_index, conf_govt) %>% 
  select(-res_chng_lag_34) %>%
  na.omit() %>% 
  distinct(location) %>% pull() %>% length()
# school enrollment is reducing num obs by 10, pop.km2 by 3, democracy by 2, ethnic by 1, ghs by 2
# res_chng_34 by 10 (nothing can be done about this), deaths by 1

## explore data
# countries in order of the highest peek stringency
data %>% 
  select(location, stringency_index, distrust_people) %>% 
  group_by(location) %>% 
  summarise(max_stringency = max(stringency_index, na.rm = TRUE), distrust_people) %>% 
  distinct() %>% 
  arrange(max_stringency) %>% 
  print(n = 100)

# countries in order of level of distrust
data %>% 
  select(location, distrust_people, conf_govt) %>% 
  distinct() %>% 
  arrange(distrust_people) %>% 
  print(n = 100)

## Weekly data models with excess deaths instead of recorded covid deaths
weekly_data %>% 
  lmer(stringency_index ~ distrust_people + excess_lag_1 + conf_govt + 
         log(conflict_index) + continent +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .) %>% 
  screenreg()

## based on lag_testing
data %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
         education_index + ghs + pop.km2 + democracy_index + 
         res_chng_lag_34 + 
         (1 | location), .) %>% 
  screenreg()



# figuring out res_chng
data %>% 
  lm(res_pct_chng ~ distrust_people, .) %>% 
  coeftest(vcovCL, cluster = ~location) %>% 
  screenreg()
data %>% 
  lmer(res_pct_chng ~ distrust_people + democracy_index + deaths_per_mil_lag_7 + log(gdp_per_capita) +
         education_index + ghs + ethnic + pop.km2 + stringency_index +
         (1 | location), .) %>% 
  screenreg()

form = res_pct_chng ~ distrust_people + stringency_index + 
  log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic +
  (1 | location)


countries <- data %>% 
  select(location, date, distrust_people, res_pct_chng, new_deaths_per_million, gdp_per_capita, ghs, pop.km2, 
         democracy_index, ethnic, education_index) %>% 
  na.omit() %>% 
  distinct(location) %>% 
  pull()

date_begin <- data %>% 
  select(location, date, distrust_people, res_pct_chng, new_deaths_per_million, gdp_per_capita, ghs, pop.km2, 
         democracy_index, ethnic, education_index) %>% 
  na.omit() %>% 
  group_by(location) %>% 
  summarise(min_date = min(date)) %>% pull(min_date) %>% max()

s = data %>% 
  filter(location %in% countries, date >= date_begin) %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_34 + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), start = date_begin, n_days = 7)


sum4 <- summary(model4)
sum4$ngrps 
sum4$coefficients
model4
data %>% 
  select(location, distrust_people) %>% 
  distinct() %>% 
  arrange(distrust_people) %>% 
  print(n = 101)


## without deaths and res_chng
data %>% 
  lmer(stringency_index ~ distrust_people + continent +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .) %>% 
  screenreg()
data %>% 
  plm(stringency_index ~ distrust_people + continent + deaths_per_mil_lag_7 + res_chng_lag_34 + 
        # stringency_index_lag_1 +
        log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index, .,
      index = c("location", "date"), effect = "time") %>% 
  summary(vcov = vcovHC)


## trust effect on res_chng 
data %>% 
  lmer(res_pct_chng ~ distrust_people + continent + deaths_per_mil_lag_7 + stringency_index +
         log(gdp_per_capita) + ghs + pop.km2 + democracy_index + ethnic + education_index +
         (1 | location), .) %>% 
  screenreg()

data %>% 
  plm(res_pct_chng ~ distrust_people + continent + deaths_per_mil_lag_7 + 
        stringency_index_lag_7 + 
        log(gdp_per_capita) + ghs + pop.km2 + democracy_index + 
        ethnic + education_index, .,
      index = c("location", "date"), effect = "time") %>% 
  summary(vcov = vcovHC)

  
## max stringency as outcome
# could I also control for max stringency
data %>% 
  lm(max_stringency ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
       ghs + pop.km2 + democracy_index + ethnic + education_index + conflict_index, .) %>% 
  coeftest(vcovCL, cluster = ~location) 


data_str <- data %>% 
  group_by(location) %>% 
  filter(stringency_index == max_stringency) %>% 
  select(location, max_stringency, date) %>% 
  filter(date == min(date)) %>% 
  select(location, date, max_stringency) %>% 
  rename(date_str = date) %>% 
  ungroup()
data_deaths_before <- data %>% 
  left_join(data_str) %>% 
  select(location, date, date_str, max_stringency, new_deaths_per_million, new_deaths) %>% 
  filter(date < date_str) %>% 
  group_by(location) %>% 
  mutate(deaths_before_max_per_mil = sum(new_deaths_per_million, na.rm = TRUE),
         deaths_before_max = sum(new_deaths, na.rm = TRUE)) %>% 
  select(location, max_stringency, deaths_before_max_per_mil, deaths_before_max) %>% 
  distinct()
  
data %>% 
  left_join(data_deaths_before) %>% 
  lm(max_stringency ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
       ghs + pop.km2 + democracy_index + ethnic + education_index + log(conflict_index) +
       deaths_before_max, .) %>% 
  coeftest(vcovCL, cluster = ~location) %>% 
  screenreg()
  
  
## deaths as outcome
data %>% 
  lmer(new_deaths_per_million ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
         ghs + pop.km2 + democracy_index + ethnic + education_index + log(conflict_index) +
         stringency_index_lag_7 + (1 | location), .) %>% 
  screenreg()

data %>% 
  plm(new_deaths_per_million ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
        ghs + pop.km2 + democracy_index + ethnic + education_index + log(conflict_index) +
        stringency_index_lag_7, .,
      index = c("location", "date"), effect = "time") %>% 
  coeftest(vcovHC) %>% 
  screenreg()

weekly_data %>% 
  lmer(excess_deaths_per_100k ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
         ghs + pop.km2 + democracy_index + ethnic + education_index + log(conflict_index) +
         stringency_index_lag_4 +
         (1 | location), .) %>% 
  screenreg()


data %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(stringency_lag = lag(stringency_index, 100)) %>% 
  plm(new_deaths_per_million ~ stringency_lag, .,
      index = c("location", "date"), effect = "individual") %>% 
  coeftest(vcovHC) %>% 
  screenreg()


resid(model8) %>% 
  as_tibble(rownames = "index") %>% 
  ggplot(aes(as.numeric(index), value)) +
  geom_point(alpha = 0.3)

resid(model8) %>% 
  as_tibble() %>% 
  ggplot(aes(value)) +
  geom_histogram()





