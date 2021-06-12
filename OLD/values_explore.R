library(tidyverse)
library(texreg)
library(york)
library(readxl)
library(lubridate)

load("Raw/WVSEVS_Joint_v1-0-0.rdata")

country <- read_csv("Data/country_data.csv")

values <- fixed %>% 
  as_tibble() %>% 
  select(cntry_AN, reg_nuts1, reg_nuts2, gwght, pwght, wght_eq1000, A009, A065, A068, A165,
         B008, G007_18_B, E069_04, 
         E069_07, E069_08, E069_11, E069_12, E115, F115, F116, H009, H010, H011, X001, 
         X003, G027A, V002, V001) %>% 
  as_tibble() %>% 
  rename(neighbour_distrust = G007_18_B, 
         state_of_health = A009,
         religious_member = A065,
         political_member = A068,
         distrust_people = A165,
         environment_vs_economy = B008,
         conf_press = E069_04,
         conf_parliament = E069_07,
         conf_civil_service = E069_08,
         conf_govt = E069_11,
         avoid_fare = F115,
         cheat_taxes = F116,
         govt_surveilence = H009,
         female = X001,
         age = X003) %>% 
  mutate(female = female - 1)

sel <- country %>% 
  select(location, riots, antgovt_dem, distrust_people, life_exp, govt_surveilence, conf_govt, conf_parliament, 
         conf_press, cheat_taxes, gdp_per_capita, democracy_index, ethnic, human_development_index)

sel %>% 
  lm(antgovt_dem ~ democracy_index + gdp_per_capita + conf_govt + distrust_people + human_development_index, .) %>% 
  screenreg()
values %>% 
  filter(cntry_AN == "GB") %>% 
  lm(distrust_people ~ factor(female), .) %>% 
  screenreg()
values$sex
