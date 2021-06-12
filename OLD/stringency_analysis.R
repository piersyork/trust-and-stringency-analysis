library(tidyverse)
library(texreg)
#rm(list = ls())

country_from <- read_csv("Data/country_data.csv") 
country <- country_from %>%  
  filter(!is.na(total_cases_per_million)) %>% 
  filter(!location %in% c("Malta")) %>% 
  mutate(across(state_of_health:V001, .fns = ~.x*gwght))

order <- country %>% 
  group_by(continent) %>% 
  summarise(median = median(avg_stringency, na.rm = TRUE)) %>% 
  arrange(desc(median)) %>% 
  pull(continent)
ggplot(country, aes(fct_relevel(continent, order), avg_stringency)) +  
  geom_jitter(width = 0.3, alpha = 0.6, colour = "blue") +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  geom_smooth(method = lm) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

trust_order <- country %>% 
  group_by(continent) %>% 
  summarise(median = median(distrust_people, na.rm = TRUE)) %>% 
  arrange(desc(median)) %>% 
  pull(continent)
ggplot(country, aes(fct_relevel(continent, order), distrust_people)) +  
  geom_jitter(width = 0.3, alpha = 0.6, colour = "blue") +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  geom_smooth(method = lm) +
  # scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()
plotly::ggplotly()

country %>% 
  lm(avg_stringency ~ distrust_people + positive_rate + gdp_per_capita + conf_govt + total_deaths_per_million +
      total_cases_per_million + gdp_growth + pop.km2 + ghs + democracy_index + 
       median_age, .) %>% 
  screenreg()



country %>% 
  # filter(continent == "Europe") %>% 
  select(location, avg_stringency) %>% 
  arrange(avg_stringency) 

regime_interact <- country %>% 
  lm(avg_stringency ~ distrust_people*regime_type + positive_rate + gdp_per_capita + conf_govt + total_deaths_per_million +
       gdp_growth + pop.km2 + ghs + democracy_index + regime_type + continent + median_age, .) 

regimeTypes <- country %>% 
  pull(regime_type) %>% 
  na.omit() %>% 
  unique() 

regime_pred <- predict(regime_interact, 
                       tibble(distrust_people = median(country$distrust_people, na.rm = TRUE),
                              regime_type = regimeTypes,
                              positive_rate = median(country$positive_rate, na.rm = TRUE),
                              gdp_per_capita = median(country$gdp_per_capita, na.rm = TRUE),
                              conf_govt = median(country$conf_govt, na.rm = TRUE),
                              total_deaths_per_million = median(country$total_deaths_per_million, na.rm = TRUE),
                              gdp_growth = median(country$gdp_growth, na.rm = TRUE),
                              pop.km2 = median(country$pop.km2, na.rm = TRUE),
                              ghs = median(country$ghs, na.rm = TRUE),
                              democracy_index = median(country$democracy_index, na.rm = TRUE),
                              continent = "Europe",
                              median_age = median(country$median_age, na.rm = TRUE)))

tibble(regime_type = regimeTypes,
       avg_stringency = regime_pred) %>% 
  ggplot(aes(fct_reorder(regime_type, avg_stringency), avg_stringency)) +
  geom_col()



country %>% 
  filter(continent == "Europe") %>% 
  select(location, avg_stringency) %>% 
  arrange(avg_stringency) 

country %>% 
  lm(avg_stringency ~ distrust_people, .) %>% 
  screenreg()

avg_stringency_model <- country %>% 
  lm(avg_stringency ~ distrust_people + log(gdp_per_capita) + total_deaths_per_million +
       gdp_growth + pop.km2 + ghs + democracy_index + 
       median_age + avg_res_chng_6, .)
avg_stringency_6_model <- country %>% 
  lm(avg_stringency_6 ~ distrust_people + log(gdp_per_capita) + total_deaths_per_million +
       gdp_growth + pop.km2 + ghs + democracy_index + 
       median_age + avg_res_chng_6, .)
avg_stringency_1_4_model <- country %>% 
  lm(avg_stringency_1_4 ~ distrust_people + log(gdp_per_capita) + total_deaths_per_million +
       gdp_growth + pop.km2 + ghs + democracy_index + 
       median_age + avg_res_chng_6, .)
screenreg(list(avg_stringency_model, avg_stringency_1_4_model, avg_stringency_6_model))

country %>% 
  lm(avg_res_chng_6 ~ distrust_people + positive_rate + log(gdp_per_capita) + total_deaths_per_million +
       total_cases_per_million + gdp_growth + pop.km2 + ghs + democracy_index + 
       median_age + avg_stringency_6, .) %>% 
  screenreg()










