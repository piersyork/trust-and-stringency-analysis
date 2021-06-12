library(tidyverse)
library(readxl)
library(lubridate)
library(texreg)
library(magrittr)

response_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx", sheet = 2)
stringency_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx", sheet = 1)

country <- read_csv("Data/country_data.csv")
cases_raw <- read_csv("Raw/owid-covid-data.csv", col_types = cols(.default = "d", 
                                                                  date = "D",
                                                                  iso_code = "c",
                                                                  continent = "c",
                                                                  location = "c"))
cases <- cases_raw %>% 
  select(iso_code, date, total_cases, total_cases_per_million, total_tests_per_thousand) %>% 
  rename(alpha.3 = iso_code)

cases2 <- cases %>% 
  mutate(date = date - weeks(2)) %>% 
  select(alpha.3, date, total_cases_per_million) %>% 
  rename(cases_2_weeks = total_cases_per_million)


response <- response_raw %>% 
  gather("date", "response_index", -country_code, - country_name) %>% 
  mutate(date = dmy(date)) %>% 
  rename(location = country_name, 
         alpha.3 = country_code)

stringency <- stringency_raw %>% 
  gather("date", "stringency_index", -country_code, - country_name) %>% 
  mutate(date = dmy(date)) %>% 
  rename(location = country_name, 
         alpha.3 = country_code)

subed <- country %>% 
  select(alpha.3, median_age, aged_65_older, extreme_poverty, democracy_index, regime_type,
         gdp_per_capita, gdp_growth, pop.km2, sch_enrol_per_cap, geni, health_spending_pct_gdp,
         life_exp, positive_rate)

response %>% 
  filter(location %in% c("United Kingdom", "France")) %>% 
  ggplot(aes(date, response_index, colour = location)) +
  geom_line()

comb <- response %>% 
  left_join(country, by = "alpha.3") %>% 
  mutate(trusting = ifelse(distrust_people < median(distrust_people, na.rm = T), 1, 0))

  
comb %>% 
  group_by(date.x, trusting) %>% 
  summarise(response = mean(response_index, na.rm = TRUE)) %>% 
  ggplot(aes(date.x, response, colour = factor(trusting))) +
  geom_line()

# mobility <- read_csv("Raw/Global_Mobility_Report.csv")
# mobility %>% 
#   group_by(country_region, date) %>% 
#   summarise(res_pct_chnge = mean(residential_percent_change_from_baseline)) %>% 
#   write_csv("global_mobility_report_country_avg_res.csv")
mobility <- read_csv("Data/global_mobility_report_country_avg_res.csv")
mobility %<>% rename(location = country_region)

first_case <- cases %>% 
  filter(!total_cases == 0) %>% 
  group_by(alpha.3) %>% 
  filter(date == min(date)) %>% 
  select(alpha.3, date) %>% 
  rename(first_case = date)

sel <- country %>% 
  select(alpha.3, distrust_people, continent) %>% 
  mutate(trusting = ifelse(distrust_people < median(distrust_people, na.rm = T), 1, 0)) %>% 
  right_join(cases) %>% 
  right_join(response) %>% 
  left_join(subed) %>% 
  full_join(stringency) %>% 
  left_join(first_case) %>% 
  distinct() %>% 
  left_join(mobility) %>% 
  left_join(cases2)
  
sel %>%  
  na.omit() %>% 
  group_by(trusting, date) %>% 
  summarise(cases = mean(total_cases_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(date, cases, colour = factor(trusting))) +
  geom_line()

sel %>% 
  group_by(continent, date) %>% 
  summarise(cases = mean(total_cases_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(date, cases, colour = continent)) +
  geom_line()
sel %>% 
  group_by(continent, date) %>% 
  summarise(response = mean(stringency_index, na.rm = TRUE)) %>% 
  ggplot(aes(date, response, colour = continent)) +
  geom_line()
plotly::ggplotly()
sel %>% 
  group_by(regime_type, date) %>% 
  summarise(cases = mean(total_cases_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(date, cases, colour = regime_type)) +
  geom_line()
sel %>% 
  group_by(regime_type, date) %>% 
  summarise(response = mean(stringency_index, na.rm = TRUE)) %>% 
  ggplot(aes(date, response, colour = regime_type)) +
  geom_line()
summary(sel)

sel %>% 
  lm(total_cases_per_million ~ response_index + total_tests_per_thousand + distrust_people + gdp_per_capita +
      health_spending_pct_gdp + democracy_index + pop.km2 + aged_65_older + date, .) %>% 
  screenreg(omit.coef = "date|location") 

sel %>% 
  lm(total_cases_per_million ~ response_index + . - alpha.3 -trusting, .) %>% 
  screenreg(omit.coef = "date|location") 


sel %>% 
  filter(location %in% c("United Kingdom", "Argentina", "Indonesia")) %>% 
  group_by(location, date) %>% 
  summarise(response = mean(stringency_index, na.rm = TRUE)) %>% 
  ggplot(aes(date, response, colour = location)) +
  geom_line()


sel %>% 
  filter(continent == "Europe") %>% 
  group_by(location) %>% 
  summarise(max = max(stringency_index, na.rm = TRUE)) %>% 
  arrange(max)

sel %>% 
  filter(continent == "Europe") %>% 
  mutate(locked75 = if_else(stringency_index >= 90, 1, 0)) %>%
  # filter(stringency_index >= 90) %>% 
  group_by(location, locked75) %>% 
  summarise(days = n()) %>% 
  filter(locked75 == 1) %>%
  select(-locked75) %>%
  # ungroup() %>% 
  # complete(location, locked75, fill = list(locked75 = 1, days = 0)) %>% 
  arrange(location) 


sel2 <- sel %>% 
  group_by(alpha.3) %>% 
  filter(date > first_case) %>% 
  mutate(avg_stringency = mean(stringency_index, na.rm = TRUE)) %>% 
  select(alpha.3, location, median_age, aged_65_older, extreme_poverty, democracy_index, regime_type,
         gdp_per_capita, gdp_growth, pop.km2, sch_enrol_per_cap, geni, health_spending_pct_gdp,
         life_exp, positive_rate, avg_stringency, distrust_people, total_cases_per_million,
         total_tests_per_thousand, continent) %>% 
  mutate(total_cases_per_million = max(total_cases_per_million, na.rm = TRUE),
         total_tests_per_thousand = max(total_tests_per_thousand, na.rm = TRUE)) %>% 
  unique()

sel2 %>% 
  lm(total_cases_per_million ~ avg_stringency + gdp_per_capita + distrust_people + pop.km2 + democracy_index +
       health_spending_pct_gdp + regime_type + continent, .) %>% 
  screenreg()


sel %>% 
  lm(cases_2_weeks ~ res_pct_chnge + date + location, .) %>% 
  screenreg(omit.coef = "date|location")

sel %>% 
  select(location, total_cases_per_million, cases_2_weeks) 










