library(Matching)
library(tidyverse)
library(texreg)
library(magrittr)

country <- read_csv("Data/country_data.csv") 
country %<>% 
  filter(!is.na(total_cases_per_million)) %>% 
  select(total_cases_per_million, distrust_people, democracy_index, gdp_per_capita, ghs, 
         pop.km2, total_tests_per_thousand, gdp_growth, total_deaths_per_million,
         health_spending_pct_gdp, aged_65_older) %>% 
  mutate(binary_trust = if_else(distrust_people >= mean(distrust_people, na.rm = TRUE), 0, 1)) %>% 
  na.omit()

country %>% 
  select(distrust_people, binary_trust)

country %>% 
  lm(total_cases_per_million ~ binary_trust + total_tests_per_thousand + gdp_per_capita + 
       gdp_growth + aged_65_older + pop.km2 + health_spending_pct_gdp + democracy_index, .) %>% 
  screenreg()

Y = country$total_cases_per_million
D = country$binary_trust
X = country %>% 
  select(democracy_index, gdp_per_capita, ghs, 
         pop.km2, total_tests_per_thousand, gdp_growth, total_deaths_per_million,
         health_spending_pct_gdp, aged_65_older)

matching <- Match(Y, D, X, 
                  estimand = "ATE", M = 1, 
                  exact = FALSE, replace = TRUE)
summary(matching)
