box::use(caseMatch[case.match],
         dplyr[...],
         readr[read_csv])

country <- read_csv("Data/country_data.csv")

match_out <- country %>% 
  select(location, distrust_people, avg_stringency_6, gdp_per_capita, health_spending_pct_gdp,
         median_age, democracy_index, ethnic, pop.km2) %>% 
  distinct() %>% 
  na.omit() %>%
  as.data.frame() %>% 
  case.match(id.var = "location", case.N = 2, distance = "mahalanobis", 
             number.of.matches.to.return = 20, outcome.var = "avg_stringency_6", 
             max.variance.outcome = TRUE)
match_out$cases
country %>% 
  select(location, distrust_people, avg_stringency_6, gdp_per_capita, gdp_growth, health_spending_pct_gdp,
         health_workers_per_1000, median_age, democracy_index) %>% 
  distinct() %>% 
  filter(grepl("United", location)) %>% 
  distinct(location)
