options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         magrittr[use_series, extract],
         # import own project functions
         functions/ts[get_coefs, load_project_data, test_lag],
         functions/ap[...])

# imports data for the project (function defined in functions/ts.r)
load_project_data()

vars <- c("deaths_per_mil_lag_5", "conf_govt", "ghs", "pop.km2", "education_index",
          "polity2", "ethnic", "log_gdp", "log_conflict", "gdp_growth")
.formula <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic + regime_type + 
  pop.km2 + continent + education_index + log_gdp + log_conflict + 
  deaths_per_mil_lag_5 + (1 | location)

.formula <- stringency_index ~ distrust_people + log_gdp + gdp_growth + education_index +
  pop_65 + ghs + polity2 + log_conflict + pop.km2 + conf_govt + (1 | location)


## new comparison: basic model but with the countries that are omitted from the main model omitted from the 
## basic model
model_x <- lmer(stringency_index ~ distrust_people + (1 | location), data)
model_y <- lmer(.formula, data)
model_z <- data %>% 
  select(all_of(all.vars(.formula))) %>% 
  na.omit() %>% 
  lmer(stringency_index ~ distrust_people + (1 | location), .)
screenreg(list(model_x, model_z, model_y))

data$location %>% unique()

model_x <- lmer(.formula, data)
model_y <- lmer(update(.formula, ~ . + trans_chng_lag_5), data)

model_z <- data %>% 
  select(location, all_of(vars), stringency_index, trans_chng_lag_34, distrust_people) %>% 
  na.omit() %>% 
  lmer(.formula, .)

screenreg(list(model_x, model_y, model_z))

model <- lmer(stringency_index ~ distrust_people + conf_govt + ghs + ethnic + pop.km2 + gdp_growth + 
                education_index + log_gdp + log_conflict + deaths_per_mil_lag_5 + 
                polity2 + (1 | location), data)

# lmer(stringency_index ~ distrust_people + conf_govt + ghs + ethnic + 
# pop.km2 + gdp_growth + education_index + log_gdp + log_conflict + 
#   deaths_per_mil_lag_5 + trans_chng_lag_34 + stringency_index_lag_1 + 
#   polity2 + (1 | location), data)
screenreg(model)



resid(model_x) %>% 
  as_tibble(rownames = "index") %>% 
  mutate(index = as.numeric(index)) %>% 
  ggplot(aes(index, value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(span = 1)

plot_linearity(model_x)
plot(model_x, resid(., scaled=TRUE) ~ fitted(.), abline = 0,pch=16,xlab="Fitted values",ylab="Standardised residuals")
rstandard(model_y)

# countries that are still included when adding transport change
countries_omit <- data %>% 
  select(location, all_of(vars), stringency_index, trans_chng_lag_34, distrust_people) %>% 
  na.omit() %>% 
  distinct(location) %>% 
  pull()

# countries in sample when not including transport
countries <- data %>% 
  select(location, all_of(vars), stringency_index, distrust_people) %>% 
  na.omit() %>% 
  distinct(location) %>% 
  pull()

# countries that don't have data for transport - are omitted when adjusting for transport
countries_exc <- countries[!countries %in% countries_omit]



data %>% 
  filter(location %in% countries_exc) %>%
  # group_by(date) %>% 
  # summarise(across(stringency_index, mean)) %>% 
  ggplot(aes(date, stringency_index, colour = location)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_line()

data %>% 
  filter(location %in% countries_exc) %>%
  summarise(across(distrust_people, median))
data %>% 
  # filter(!location %in% countries_exc) %>%
  summarise(across(distrust_people, median))

data %>% 
  filter(location %in% countries_exc) %>%
  group_by(location) %>% 
  summarise(across(stringency_index, mean, na.rm = TRUE)) %>% 
  use_series(stringency_index) %>% 
  mean()
data %>% 
  # filter(!location %in% countries_exc) %>%
  group_by(location) %>% 
  summarise(across(stringency_index, mean, na.rm = TRUE)) %>% 
  use_series(stringency_index) %>% 
  mean()


data %>% 
  filter(location %in% countries_exc) %>% 
  group_by(location) %>% 
  summarise(across(distrust_people, mean)) %>% 
  ggplot(aes(distrust_people, forcats::fct_reorder(location, distrust_people))) +
  geom_col(fill = "#00468B") +
  geom_vline(xintercept = mean(unique(data$distrust_people[data$location %in% countries_omit])),
             linetype = "dashed") +
  coord_cartesian(expand = FALSE) +
  labs(title = "Distrust for countries excluded when including transport", x = "", y = "",
       subtitle = "Verticle line shows the mean distrust of the countries still included in the sample
when adjusting for transport") +
  theme(plot.title.position = "plot")

data %>% 
  filter(location %in% countries_exc) %>% 
  group_by(location) %>% 
  summarise(across(stringency_index, mean, na.rm = TRUE)) %>% 
  ggplot(aes(stringency_index, forcats::fct_reorder(location, stringency_index))) +
  geom_col(fill = "#00468B") +
  geom_vline(xintercept = mean(data$stringency_index[data$location %in% countries_omit], na.rm = TRUE),
             linetype = "dashed") +
  coord_cartesian(expand = FALSE) +
  labs(title = "Mean stringency for countries excluded when including transport", x = "", y = "",
       subtitle = "Verticle line shows the mean stringency of the countries still included in the sample
when adjusting for transport") +
  theme(plot.title.position = "plot")
  




