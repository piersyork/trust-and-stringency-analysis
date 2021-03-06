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

data %>% 
  select(location, date, stringency_index, distrust_people) %>% 
  na.omit() %>% 
  filter(is.infinite(stringency_index))
is.nan(data_omit$distrust_people)

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
  lmer(stringency_index ~ distrust_people + (1 | location), .)
screenreg(model1)

# adding economic indicators
model2 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index + 
         (1 | location), .)

# adding population over 65 and population density
model3 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + (1 | location), .)


# adding ghs
model4 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + (1 | location), .) 


# add polity
model5 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + polity2 + (1 | location), .)


# include conflict
model6 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + polity2 + log_conflict + (1 | location), .) 

model7 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + polity2 + log_conflict + pop.km2 + (1 | location), .) 

model8 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + polity2 + log_conflict + pop.km2 + conf_govt + (1 | location), .) 
screenreg(model8)
summary(model8)

screenreg(list(model1, model2, model3, model4, model5, model6, model7))


# include gini
model8 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         pop_65 + ghs + polity2 + log_conflict + pop.km2 + conf_govt + gini_disp + (1 | location), .) 
screenreg(model8)

model9 <- data %>% 
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         ghs + pop.km2 + polity2 + log_conflict + conf_govt + 
         deaths_per_mil_lag_5 + trans_chng_lag_28 + (1 | location), .) 
screenreg(model9)

methods(class = "merMod")

coefs <- summary(model10) %>% 
  use_series(coefficients) %>% 
  data.frame() %>% 
  as_tibble(rownames = "var") %>% 
  extract(-1,) %>% 
  print(n = 30)

distrust_df <- coefs[c(1, 21, 22, 23, 24, 25), ] %>% 
  t() %>% 
  as_tibble(rownames = "val") 

colnames(distrust_df) <- make.names(distrust_df[1, ])
distrust_df <- distrust_df[-1,]
distrust_df %>% 
  mutate(across(distrust_people:distrust_people.continentSouth.America, as.numeric),
         across(distrust_people.continentAsia:distrust_people.continentSouth.America,
                ~distrust_people + .x)) %>% 
  extract(1,) %>% 
  t() %>% 
  data.frame() %>% 
  as_tibble(rownames = "cont") %>% 
  extract(-1,) %>% 
  `colnames<-`(c("cont", "est")) %>% 
  mutate(est = as.numeric(est),
         cont = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")) %>% 
  ggplot(aes(forcats::fct_reorder(cont, est), est)) +
  geom_col() +
  theme() #axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)
  

data %>% 
  lmer(trans_pct_chng ~ distrust_people + log_gdp + gdp_growth + education_index +
         ghs + pop.km2 + polity2 + log_conflict + conf_govt +
         deaths_per_mil_lag_5 + (1 | location), .) %>% 
  screenreg()

coef(model10)
data$continent %>% unique()
data$regime_type %>% unique()

model11 <- data %>% 
  lmer(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
         log(conflict_index) + deaths_per_mil_lag_5 +
         log(gdp_per_capita) + ghs + pop.km2 + education_index +
         poly(polity2, 2, raw = TRUE) + 
         (1 | location), .)
model12 <- data %>% 
  lmer(stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
         log(conflict_index) + deaths_per_mil_lag_5 +
         log(gdp_per_capita) + ghs + pop.km2 + education_index +
         polity2 + 
         (1 | location), .)
screenreg(list(model11, model12))
methods(class = "merMod")
residuals(model12) 
# multivariate pooled model with country clustered SE
pooled4 <- data %>% 
  lm(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
       res_chng_lag_34 + ghs + pop.km2 + polity2 + education_index, .) %>% 
  coeftest(vcovCL, cluster = ~location)
screenreg(list(pooled4, model4))

screenreg(list(model1, model2, model3, model4, model5))

screenreg(list(pooled1, model1, pooled4, model4), 
          custom.gof.rows = list("Random Effects" = c("NO", "YES", "NO", "YES")),
          omit.coef = "Intercept")





# what countries are in model
data %>% 
  select(location, stringency_index, distrust_people, gdp_per_capita, res_chng_lag_34, deaths_per_mil_lag_7,
         ghs, pop.km2, polity2, ethnic, education_index, conflict_index, conf_govt) %>% 
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
  lmer(stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
         ghs + pop.km2 + polity2 + log_conflict + conf_govt + excess_lag_1 +
         (1 | location), .) %>% 
  screenreg()
weekly_data %>% 
  select(location, distrust_people)


## based on lag_testing
data %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + deaths_per_mil_lag_7 +
         education_index + ghs + pop.km2 + polity2 + 
         res_chng_lag_34 + 
         (1 | location), .) %>% 
  screenreg()



# figuring out res_chng
data %>% 
  lm(res_pct_chng ~ distrust_people, .) %>% 
  coeftest(vcovCL, cluster = ~location) %>% 
  screenreg()
data %>% 
  lmer(res_pct_chng ~ distrust_people + polity2 + deaths_per_mil_lag_7 + log(gdp_per_capita) +
         education_index + ghs + pop.km2 + stringency_index +
         (1 | location), .) %>% 
  screenreg()

form = res_pct_chng ~ distrust_people + stringency_index + 
  log(gdp_per_capita) + ghs + pop.km2 + polity2 + ethnic +
  (1 | location)


countries <- data %>% 
  select(location, date, distrust_people, res_pct_chng, new_deaths_per_million, gdp_per_capita, ghs, pop.km2, 
         polity2, ethnic, education_index) %>% 
  na.omit() %>% 
  distinct(location) %>% 
  pull()

date_begin <- data %>% 
  select(location, date, distrust_people, res_pct_chng, new_deaths_per_million, gdp_per_capita, ghs, pop.km2, 
         polity2, ethnic, education_index) %>% 
  na.omit() %>% 
  group_by(location) %>% 
  summarise(min_date = min(date)) %>% pull(min_date) %>% max()

s = data %>% 
  filter(location %in% countries, date >= date_begin) %>% 
  get_coefs(stringency_index ~ distrust_people + res_chng_lag_34 + deaths_per_mil_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + polity2 + education_index +
         (1 | location), start = date_begin, n_days = 7)

s$df$estimate %>% mean()
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
         log(gdp_per_capita) + ghs + pop.km2 + polity2 + education_index +
         (1 | location), .) %>% 
  screenreg()
data %>% 
  plm(stringency_index ~ distrust_people + continent + deaths_per_mil_lag_7 + res_chng_lag_34 + 
        # stringency_index_lag_1 +
        log(gdp_per_capita) + ghs + pop.km2 + polity2 + education_index, .,
      index = c("location", "date"), effect = "time") %>% 
  summary(vcov = vcovHC)


## trust effect on res_chng 
data %>% 
  lmer(trans_pct_chng ~ distrust_people + deaths_per_mil_lag_7 + stringency_index_lag_7 +
         log(gdp_per_capita) + ghs + pop.km2 + regime_type + education_index +
         (1 | location), .) %>% 
  screenreg()
gc_trans <- data %>% 
  get_coefs(trans_pct_chng ~ distrust_people + conf_govt + deaths_per_mil_lag_7 + stringency_index_lag_7 +
              log(gdp_per_capita) + ghs + pop.km2 + regime_type + education_index + conflict_index,
            n_days = 1, method = "lm")

data %>% 
  plm(res_pct_chng ~ distrust_people + continent + deaths_per_mil_lag_7 + 
        stringency_index_lag_7 + 
        log(gdp_per_capita) + ghs + pop.km2 + polity2 + 
        education_index, .,
      index = c("location", "date"), effect = "time") %>% 
  summary(vcov = vcovHC)

  
## max stringency as outcome
# could I also control for max stringency
data %>% 
  lm(max_stringency ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
       ghs + pop.km2 + polity2 + education_index + conflict_index, .) %>% 
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
       ghs + pop.km2 + polity2 + education_index + log(conflict_index) +
       deaths_before_max, .) %>% 
  coeftest(vcovCL, cluster = ~location) %>% 
  screenreg()
  
  
## deaths as outcome
data %>% 
  lmer(new_deaths_per_million ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
         ghs + pop.km2 + polity2 + education_index + log(conflict_index) +
         stringency_index_lag_7 + (1 | location), .) %>% 
  screenreg()

data %>% 
  plm(new_deaths_per_million ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
        ghs + pop.km2 + polity2 + education_index + log(conflict_index) +
        stringency_index_lag_7, .,
      index = c("location", "date"), effect = "time") %>% 
  coeftest(vcovHC) %>% 
  screenreg()

weekly_data %>% 
  lmer(excess_deaths_per_100k ~ distrust_people + conf_govt + continent + log(gdp_per_capita) +
         ghs + pop.km2 + polity2 + education_index + log(conflict_index) +
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


data %>% 
  select(location, distrust_people, stringency_index) %>% 
  na.omit() %>% 
  select(location, distrust_people) %>% 
  distinct() %>% 
  arrange(distrust_people) %>% 
  print(n = 100) %>% 
  use_series(distrust_people) %>% 
  range()


model = lmer(stringency_index ~ distrust_people + log_gdp + polity2 + (1 | location), data)

model_deaths <- data %>% 
  lmer(trans_pct_chng*-1 ~ stringency_index_lag_7 + deaths_per_mil_lag_7 + log_gdp + gdp_growth + education_index +
         ghs + pop.km2 + polity2 + log_conflict + conf_govt + trans_chng_lag_7 + (1 | location), .)
screenreg(model_deaths)
