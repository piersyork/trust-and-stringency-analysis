box::use(dplyr[...],
         readr[read_csv, cols],
         readxl[read_xlsx],
         tidyr[gather],
         lubridate[dmy])
covid_raw <- read_csv("Raw/owid-covid-data.csv", col_types = cols(.default = "d", 
                                                                  date = "D",
                                                                  iso_code = "c",
                                                                  continent = "c",
                                                                  location = "c"))
country_data_all <- read_csv("Data/country_data.csv")
covid_raw$location %>% unique()
countries_in <- country_data_all %>% 
  select(location, alpha.3, distrust_people, conf_govt) %>% 
  filter(!is.na(distrust_people)) %>% 
  # select(distrust_people, location, pop.km2) %>%
  na.omit() %>% 
  pull(location)

polity <- readxl::read_xls("Raw/p5v2018.xls") %>% 
  filter(year == 2017) %>% 
  select(location = country, polity2) %>% 
  mutate(location = recode(location,
                           "Korea South" = "South Korea",
                           "Slovak Republic" = "Slovakia",
                           "Bosnia" = "Bosnia and Herzegovina"))

countries_in[!countries_in %in% polity$location]
grep("Hong", polity$location, value = TRUE)

education <- read_csv("Raw/education_index.csv") %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  select(country, education_index) %>% 
  ungroup() %>% 
  mutate(country = recode(country,
                          "Czechia" = "Czech Republic",
                          "Viet Nam" = "Vietnam",
                          "Korea (Republic of)" = "South Korea",
                          "Bolivia (Plurinational State of)" = "Bolivia"))

conflict <- read_csv("Raw/conflict.csv") %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  select(country, conflict_index) %>% 
  mutate(country = recode(country,
                          "Korea, South" = "South Korea",
                          "Slovak Republic" = "Slovakia"))

conflict_countries <- conflict %>% 
  na.omit() %>% 
  pull(country)

countries_in[!countries_in %in% conflict_countries]
grep("Slovak", conflict_countries, value = TRUE)

country_data <- country_data_all %>% 
  select(location, alpha.3, distrust_people, conf_govt, gdp_per_capita,  
         ghs, ethnic, pop.km2, gdp_growth, health_spending_pct_gdp) %>% 
  distinct() %>% 
  left_join(education, by = c("location" = "country")) %>% 
  left_join(conflict, by = c("location" = "country")) %>% 
  left_join(polity)

country_data %>% 
  select(location, distrust_people, education_index, conflict_index, polity2) %>% 
  na.omit() %>% 
  print(n = 2000)


mobility_raw <- read_csv("Raw/Global_Mobility_Report.csv")
stringency_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx")

stringency <- stringency_raw %>% 
  gather("date", "stringency_index", -country_code, - country_name) %>% 
  mutate(date = dmy(date)) %>% 
  rename(location = country_name, 
         alpha.3 = country_code) %>% 
  mutate(location = recode(location,
                           "Kyrgyz Republic" = "Kyrgyzstan",
                           "Slovak Republic" = "Slovakia"))

stringency_countries <- stringency %>% 
  na.omit() %>% 
  pull(location) %>% 
  unique()

countries_in[!countries_in %in% stringency_countries]
grep("Slova", stringency_countries, value = TRUE)

mobility <- mobility_raw %>% 
  group_by(country_region, date) %>% 
  summarise(country_region_code, 
            res_pct_chng = mean(residential_percent_change_from_baseline, na.rm = TRUE),
            parks_pct_chng = mean(parks_percent_change_from_baseline, na.rm = TRUE),
            grcry_pct_chng = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
            trans_pct_chng = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
            rtl_pct_chng = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)) %>% 
  distinct() %>% 
  rename(location = country_region) %>% 
  select(-country_region_code) %>% 
  mutate(location = recode(location, "Czechia" = "Czech Republic"))
 
mobility_countries <- mobility %>% 
  na.omit() %>% 
  pull(location) %>% 
  unique()

covid <- covid_raw %>% 
  select(location, date, total_cases, new_cases, new_cases_per_million, total_deaths, new_deaths,
         new_deaths_per_million, total_deaths_per_million, total_cases_per_million) %>% 
  mutate(location = recode(location, "Czechia" = "Czech Republic"))
covid_countries <- covid$location %>% unique()
countries_in[!countries_in %in% covid_countries]


data <- stringency %>% 
  left_join(mobility, by = c("location", "date")) %>% 
  left_join(covid, by = c("location", "date")) %>% 
  left_join(country_data, by = c("alpha.3", "location"))

data %>% 
  select(location, education_index, conflict_index) %>% 
  na.omit() %>% 
  distinct()

save(data, file = "Data/panel_data.Rdata")

data %>% 
  select(location, distrust_people, polity2) %>% 
  distinct() %>% 
  na.omit()


