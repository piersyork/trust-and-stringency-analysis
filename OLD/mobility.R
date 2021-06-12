library(tidyverse)
library(texreg)
country <- read_csv("Data/country_data.csv")

country %>% 
  lm(avg_res_chng_6 ~ distrust_people + positive_rate + gdp_per_capita + conf_govt + total_deaths_per_million +
       total_cases_per_million + gdp_growth + pop.km2 + ghs + democracy_index + regime_type + continent + 
       median_age, .) %>% 
  screenreg()






# country %>% 
#   filter(location %in% c("United Kingdom", "France", "United States", "Sweden", "Germany", 
#                              "Spain", "Italy", "Canada", "New Zealand", "Australia")) %>%  
#   mutate(week = week(date),
#          year = year(date)) %>% 
#   group_by(location, year, week) %>% 
#   summarise(date, res_pct_chng = mean(res_pct_chng)) %>% 
#   ggplot(aes(date, res_pct_chng, colour = location)) +
#   geom_line()
