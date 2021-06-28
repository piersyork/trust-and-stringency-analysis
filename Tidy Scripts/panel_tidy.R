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

education <- read_csv("Raw/education_index.csv") %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  select(country, education_index) %>% 
  ungroup()

conflict <- read_csv("Raw/conflict.csv") %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  select(country, conflict_index) 

## not yet included
world_bank <- read_csv("Raw/world_bank_indicators.csv") %>% 
  group_by(alpha.3) %>% 
  filter(year == max(year) - 1)


country_data <- country_data_all %>% 
  select(location, distrust_people, conf_govt, gdp_per_capita,  
         ghs, ethnic, democracy_index, regime_type, pop.km2, sch_enrol_per_cap,
         gdp_growth, health_spending_pct_gdp, continent) %>% 
  distinct() %>% 
  left_join(education, by = c("location" = "country")) %>% 
  left_join(conflict, by = c("location" = "country"))

country_data %>% 
  select(location, distrust_people, education_index, conflict_index) %>% 
  print(n = 2000)


mobility_raw <- read_csv("Raw/Global_Mobility_Report.csv")
stringency_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx")

stringency <- stringency_raw %>% 
  gather("date", "stringency_index", -country_code, - country_name) %>% 
  mutate(date = dmy(date)) %>% 
  rename(location = country_name, 
         alpha.3 = country_code)

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
  select(-country_region_code)

covid <- covid_raw %>% 
  select(location, date, total_cases, new_cases, new_cases_per_million, total_deaths, new_deaths,
         new_deaths_per_million, total_deaths_per_million, total_cases_per_million)


data <- stringency %>% 
  left_join(mobility, by = c("location", "date")) %>% 
  left_join(covid, by = c("location", "date")) %>% 
  left_join(country_data, by = c("location")) 

save(data, file = "Data/panel_data.Rdata")




