
box::use(dplyr[...],
         readr[read_csv, cols, write_csv],
         readxl[read_xlsx, read_xls],
         tidyr[gather],
         lubridate[date, dmy],
         ggplot2[...])
not_all_na <- function(x) any(!is.na(x))

# covid_raw <- read_csv("Raw/owid-covid-data.csv", col_types = cols(.default = "d", 
#                                                                   date = "D",
#                                                                   iso_code = "c",
#                                                                   continent = "c",
#                                                                   location = "c"))
# covid_positive <- covid_raw %>% 
#   select(iso_code, date, positive_rate) %>% 
#   na.omit() %>% 
#   group_by(iso_code) %>% 
#   filter(date == max(date)) %>% 
#   rename(alpha.3 = iso_code) %>% 
#   select(-date)

country_iso <- read_csv("Raw/all.csv") %>% 
  select(location = name, alpha.2 = `alpha-2`, alpha.3 = `alpha-3`, 
         continent = region, sub_region = `sub-region`)

country_iso <- ISOcodes::ISO_3166_1 %>% 
  as_tibble() %>% 
  select(location = Name, alpha.2 = Alpha_2, alpha.3 = Alpha_3, official_name = Official_name) %>% 
  mutate(location = recode(location,
                           "Viet Nam" = "Vietnam",
                           "Iran, Islamic Republic of" = "Iran",
                           "Russian Federation" = "Russia",
                           "Taiwan, Province of China" = "Taiwan",
                           "Korea, Republic of" = "South Korea",
                           "Czechia" = "Czech Republic",
                           "Bolivia, Plurinational State of" = "Bolivia"))

load("Raw/WVSEVS_Joint_v1-0-0.rdata")

countries_raw <- read_xlsx("Raw/2020 Edition CNTSDATA.xlsx", skip = 1) 


countries_data <- countries_raw %>% 
  filter(year == 2019) %>% 
  select(where(not_all_na)) %>% 
  mutate(country = recode(country,
                          "Russian Federation" = "Russia",
                          "Slovak Republic" = "Slovakia",
                          "Korea, South" = "South Korea",
                          "Myanmar (Burma)" = "Myanmar",
                          "China PR" = "China"))

countries_educ <- countries_raw %>% 
  filter(year == 2015) %>% 
  select(Wbcode, country, contains("school")) %>% 
  select(country, school11) %>% 
  rename(sch_enrol_per_cap = school11) %>% 
  mutate(country = recode(country,
                          "Russian Federation" = "Russia",
                          "Slovak Republic" = "Slovakia",
                          "Korea, South" = "South Korea",
                          "Myanmar (Burma)" = "Myanmar",
                          "China PR" = "China"))

geni_raw <- read_csv("Raw/geni_index_worldbank.csv")
geni <- geni_raw %>% 
  select(where(not_all_na)) %>% 
  gather("year", "geni", -location, -country_code) %>% 
  arrange(location) %>% 
  group_by(location) %>% 
  na.omit() %>% 
  filter(year == max(year)) %>% 
  select(-year)

health_workers_raw <- read_csv("Raw/health_workers_per_1000.csv") 
health_workers <- health_workers_raw %>% 
  select(where(not_all_na)) %>% 
  gather("year", "health_workers_per_1000", -location) %>% 
  arrange(location) %>% 
  group_by(location) %>% 
  na.omit() %>% 
  filter(year == max(year))

health_spending_raw <- read_csv("Raw/health_spending_worldbank.csv")
health_spending <- health_spending_raw %>% 
  select(where(not_all_na)) %>% 
  gather("year", "health_spending_pct_gdp", -location, -country_code) %>% 
  arrange(location) %>% 
  group_by(location) %>% 
  na.omit() %>% 
  filter(year == "2018") %>% 
  select(-year)

life_exp_raw <- read_csv("Raw/life_expectancy.csv")
life_exp <- life_exp_raw %>% 
  select(where(not_all_na)) %>% 
  gather("year", "life_exp", -location) %>% 
  arrange(location) %>% 
  group_by(location) %>% 
  na.omit() %>% 
  filter(year == "2018")


gdp_growth <- read_csv("Raw/gdp_growth.csv", col_types = cols(location = "c",
                                                              gdp_growth = "d"))
gdp_growth <- gdp_growth %>% 
  mutate(location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location))

values <- fixed

values %>% 
  as_tibble() %>% 
  select(uniqid, A165, cntry_AN, year) %>% 
  count(year)

values %>% 
  group_by(cntry_AN) %>% 
  summarise(distrust = mean(A165, na.rm = TRUE)-1) %>% 
  na.omit() %>% 
  arrange() %>% 
  print(n = 100)
colnames(values) %>% grep("X001", ., value = TRUE)
female_distrust <- values %>% 
  as_tibble() %>% 
  select(cntry_AN, A165, X001) %>% 
  filter(X001 == 2) %>% 
  group_by(cntry_AN) %>% 
  summarise(female_distrust = mean(A165, na.rm = TRUE) - 1)
distrust <- values %>% 
  as_tibble() %>% 
  select(cntry_AN, A165, X001) %>% 
  filter(X001 == 1) %>% 
  group_by(cntry_AN) %>% 
  summarise(male_distrust = mean(A165, na.rm = TRUE) - 1) %>% 
  left_join(female_distrust)

distrust %>% 
  ggplot(aes(male_distrust, female_distrust)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "blue", size = 1)

ghs <- read_csv("Raw/ghs.csv") %>% 
  mutate(location = recode(location, "Kyrgyz Republic" = "Kyrgyzstan"))


democracy <- read_csv("Raw/democracy_index.csv")
democracy <- democracy %>% 
  mutate(location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location))

ethnic <- read_csv("Raw/ethnic_fractionalisation.csv") 
colnames(ethnic) <- tolower(colnames(ethnic))
ethnic <- ethnic %>% 
  mutate(location = gsub(pattern = "^\\s", replacement = "", x = location))

density <- read_csv("Raw/country density.csv")
density <- density %>% 
  mutate(location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = gsub(pattern = "^\\s", replacement = "", x = location),
         location = recode(location,
                           "Russia[note 10]" = "Russia",
                           "Ukraine [note 4]" = "Ukraine")) %>% 
  select(location, area_km2, pop.km2)

country_iso$location[!country_iso$location %in% ethnic$location]
# covid <- covid_raw %>% 
#   rename(alpha.3 = iso_code) %>% 
#   group_by(alpha.3) %>% 
#   filter(date == max(date)) %>% 
#   select(alpha.3, continent, location, total_cases_per_million, total_deaths_per_million,
#          aged_65_older, gdp_per_capita, extreme_poverty, human_development_index, 
#          median_age) %>% 
#   right_join(country_iso[,2:4]) %>% 
#   rename(cntry_AN = alpha.2)
# 
# covid_test <- covid_raw %>% 
#   rename(alpha.3 = iso_code) %>% 
#   group_by(alpha.3) %>% 
#   select(total_tests, total_tests_per_thousand, total_vaccinations) %>% 
#   summarise(across(everything(), max, na.rm = TRUE)) %>% 
#   mutate(across(total_tests:total_vaccinations, ~ ifelse(. == -Inf, NA, .)))
  

# first_case <- covid_raw %>% 
#   select(iso_code, date, total_cases, total_cases_per_million, total_tests_per_thousand) %>% 
#   rename(alpha.3 = iso_code) %>% 
#   filter(!total_cases == 0) %>% 
#   group_by(alpha.3) %>% 
#   filter(date == min(date)) %>% 
#   select(alpha.3, date) %>% 
#   rename(first_case = date) %>% 
#   ungroup()
# 
# stringency_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx")
# stringency <- stringency_raw %>% 
#   gather(key = "date", value = "stringency", -country_code, -country_name) %>% 
#   mutate(date = dmy(date)) %>% 
#   `colnames<-`(c("alpha.3", "location", "date", "stringency"))
# avg_stringency3 <- stringency %>% 
#   left_join(first_case) %>% 
#   group_by(alpha.3) %>% 
#   filter(date >= first_case) %>% 
#   summarise(avg_stringency = mean(stringency, na.rm = TRUE))
# avg_stringency2 <- stringency %>% 
#   left_join(first_case) %>% 
#   group_by(alpha.3) %>% 
#   filter((date >= first_case + months(1)) & date < first_case + months(4)) %>% 
#   summarise(avg_stringency_1_4 = mean(stringency, na.rm = TRUE))
# avg_stringency <- stringency %>% 
#   left_join(first_case) %>% 
#   group_by(alpha.3) %>% 
#   filter((date >= first_case + months(0)) & date < first_case + months(6)) %>% 
#   summarise(avg_stringency_6 = mean(stringency, na.rm = TRUE)) %>% 
#   full_join(avg_stringency2) %>% 
#   full_join(avg_stringency3)
# 
# 
# mobility_raw <- read_csv("Raw/Global_Mobility_Report.csv")
# data_alpha.3 <- country_iso %>% 
#   select(name, alpha.3) %>% 
#   rename(location = name)
# mobility <- mobility_raw %>% 
#   group_by(country_region, date) %>% 
#   summarise(country_region_code, res_pct_chng = mean(residential_percent_change_from_baseline, na.rm = TRUE)) %>% 
#   distinct() %>% 
#   rename(location = country_region) %>% 
#   select(-country_region_code)
# avg_mobility_6 <- mobility %>%
#   left_join(data_alpha.3) %>% 
#   left_join(first_case) %>% 
#   group_by(location) %>% 
#   filter((date >= first_case) & date < first_case + months(6)) %>% 
#   summarise(avg_res_chng_6 = mean(res_pct_chng, na.rm = TRUE))
#   



sub <- values %>% 
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
         sex = X001,
         age = X003) %>%
  # right_join(covid, by = "cntry_AN") %>% 
  mutate(distrust_people = distrust_people - 1) %>% 
  right_join(country_iso, by = c("cntry_AN" = "alpha.2")) %>% 
  # mutate_at(vars(state_of_health:V001), ~ . * gwght * wght_eq1000) %>%
  right_join(ghs, by = "location") %>%
  right_join(ethnic, by = "location") %>% 
  # right_join(democracy, by = "location") %>% 
  right_join(density, by = "location")
  
sub %>%
  select(location, pop.km2) %>% 
  distinct() %>% 
  na.omit()
  

group <- sub %>% 
  # filter(continent == "Europe") %>%
  group_by(location) %>% 
  summarise_all(mean, na.rm = T) %>% 
  # summarise(across(state_of_health:V001, ~ mean(.x, na.rm = TRUE))) %>% 
  unique()

gini <- read_csv("Raw/swiid9_1_summary.csv") %>% 
  filter(year %in% 2016:2019) %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  select(country, gini_disp, gini_mkt) %>% 
  mutate(country = recode(country,
                          "Egypt" = "Egypt, Arab Rep.",
                          "Korea" = "Korea, Rep.",
                          "Iran" = "Iran, Islamic Rep.",
                          "Russia" = "Russian Federation",
                          "Kyrgyzstan" = "Kyrgyz Republic",
                          "Slovakia" = "Slovak Republic",
                          "Venezuela" = "Venezuela, RB",
                          "São Tomé and Príncipe" = "Sao Tome and Principe"))

gini$country[!gini$country %in% world_bank$country]

world_bank <- read_csv("Raw/world_bank_indicators.csv") %>% 
  group_by(alpha.3) %>% 
  filter(year == 2019) %>% 
  rename(gdp_per_capita = gdp_per_cap) %>% 
  left_join(gini)

gini$country[!gini$country %in% world_bank$country]
grep("Macedonia", world_bank$country, value = TRUE)

world_bank %>% 
  select(country, gini_disp) %>% 
  na.omit()

country <- values %>% 
  select(cntry_AN, reg_nuts1, reg_nuts2, gwght, A009, A065, A068, A165,
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
         sex = X001,
         age = X003) %>% 
  mutate(distrust_people = ifelse(distrust_people < 0, NA,  distrust_people - 1),
         conf_govt = case_when(conf_govt %in% 1:2 ~ 1,
                               conf_govt %in% 3:4 ~ 0)) %>% 
  group_by(cntry_AN) %>%  
  summarise(across(state_of_health:V001, ~ mean(. * gwght, na.rm = TRUE))) %>% 
  left_join(country_iso, by = c("cntry_AN" = "alpha.2")) %>% 
  # right_join(covid, by = "cntry_AN") %>% 
  left_join(ghs, by = "location") %>% 
  left_join(ethnic, by = "location") %>% 
  # left_join(democracy, by = "location") %>% 
  left_join(density, by = "location") %>% 
  # left_join(covid_test, by = "alpha.3") %>% 
  left_join(countries_data, by = c("location" = "country")) %>% 
  left_join(countries_educ, by = c("location" = "country")) %>% 
  # left_join(gdp_growth, by = "location") %>% 
  # left_join(geni, by = "location") %>% 
  left_join(health_spending, by = "location") %>% 
  left_join(life_exp, by = "location") %>% 
  left_join(world_bank, by = "alpha.3") %>% 
  # full_join(health_workers, by = "location") %>% 
  # left_join(covid_positive) %>% 
  # left_join(avg_stringency, by = "alpha.3") %>% 
  # left_join(avg_mobility_6) %>% 
  distinct() 

countries_in <- country %>% 
  select(location, distrust_people) %>% 
  distinct() %>% 
  use_series(location)

countries_in[!countries_in %in% gini$country]

gini$country %>% grep("Tajik", ., value = TRUE)

alpha_in <- country$alpha.3 %>% unique()

country %>% 
  select(location, distrust_people, gini_disp) %>% 
  na.omit() %>% 
  distinct()

world_bank$alpha.3[!world_bank$alpha.3 %in% alpha_in]
alpha_in[!alpha_in %in% world_bank$alpha.3]

distrust_countries <- country %>% 
  select(location, distrust_people) %>% 
  distinct() %>% 
  pull(location)
country %>% 
  select(location, distrust_people, gdp_per_capita) %>% 
  na.omit()

distrust_countries[!distrust_countries %in% world_bank$country]
grep("Czech", ethnic$location, value = TRUE)

country %>% 
  select(location, distrust_people, gdp_per_capita) %>% 
  na.omit()

country %>% 
  select(location, distrust_people, conf_govt) %>% 
  na.omit() %>% 
  arrange(distrust_people) %>% 
  # print(n = 100)
  ggplot(aes(distrust_people, conf_govt)) +
  geom_point() +
  geom_smooth()
  


write_csv(country, "Data/country_data.csv")
# 
# 
# write_csv(sub, "Data/survey_with_country.csv")

















