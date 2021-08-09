box::use(dplyr[...],
         ggplot2[...],
         readr[read_csv, write_csv],
         lubridate[dmy, date])

excess_raw <- read_csv("Raw/economist_estimate_excess_deaths.csv")

excess_raw %>% 
  group_by(date, iso3c) %>% 
  summarise(deaths = mean(estimated_daily_excess_deaths_per_100k, na.rm = TRUE)) %>% 
  filter(iso3c %in% c("ARG", "USA", "GBR")) %>% 
  mutate(date = dmy(date)) %>% 
  ggplot(aes(date, deaths, colour = iso3c)) +
  geom_line()

colnames(excess_raw)

excess <- excess_raw %>% 
  rename(alpha.3 = iso3c, excess_deaths_per_100k = estimated_daily_excess_deaths_per_100k) %>% 
  select(alpha.3, date, excess_deaths_per_100k) %>% 
  mutate(date = dmy(date))



weekly_data <- data %>% 
  mutate(weekno = as.numeric(date - data$date[1]) %/% 7) %>% 
  group_by(weekno) %>% 
  mutate(date = min(date) - 2) %>% 
  group_by(location, alpha.3, date) %>% 
  summarise(across(c(stringency_index, res_pct_chng, distrust_people, gdp_per_capita,
                     ghs, gdp_growth, health_spending_pct_gdp, pop.km2,
                     polity2, ethnic, conf_govt, conflict_index, education_index), mean),
            across(c(new_cases_per_million, new_deaths_per_million), ~mean(.x, na.rm = TRUE)/10)) %>%
  rename(daily_deaths_100k = new_deaths_per_million, daily_cases_100k = new_cases_per_million) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(excess) %>% 
  write_csv("Data/weekly_data.csv")

weekly_data %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(excess_lag_1 = lag(excess_deaths_per_100k, 1),
         excess_lag_2 = lag(excess_deaths_per_100k, 2),
         res_chng_lag1 = lag(res_pct_chng, 1)) %>% 
  lmer(stringency_index ~ distrust_people + log(gdp_per_capita) + excess_lag_1 + res_chng_lag1 +
         daily_cases_100k +
         ghs + pop.km2 + polity2 + ethnic + sch_enrol_per_cap +
         (1 | location), .) %>% 
  screenreg()

weekly_data %>% 
  group_by(location) %>% 
  summarise(distrust_people = mean(distrust_people),
            excess_deaths = mean(excess_deaths_per_100k, na.rm = TRUE)) %>% 
  ggplot(aes(distrust_people, excess_deaths)) +
  geom_point() +
  geom_smooth(method = lm)
data %>% 
  group_by(location) %>% 
  summarise(distrust_people = mean(distrust_people),
            deaths = mean(new_deaths_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(distrust_people, deaths)) +
  geom_point() +
  geom_smooth(method = lm)


















  



