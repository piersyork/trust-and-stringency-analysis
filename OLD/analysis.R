library(tidyverse)
library(texreg)
library(magrittr)
library(sandwich)
library(lmtest)

country_from <- read_csv("Data/country_data.csv") 
country <- country_from %>%  
  filter(!is.na(total_cases_per_million)) %>% 
  filter(!location %in% c("Malta")) %>% 
  mutate(across(state_of_health:V001, .fns = ~.x*gwght))

country %>% 
  lm(total_cases_per_million ~ distrust_people + democracy_index + gdp_per_capita + ghs + pop.km2 +
       human_development_index + total_tests_per_thousand + continent + state_of_health +
       avg_stringency + median_age + avg_res_chng_6, .) %>% 
  screenreg()

country %>% 
  ggplot(aes(sch_enrol_per_cap, total_cases_per_million)) +
  geom_point() +
  geom_smooth(method = lm, colour = "black") +
  scale_color_viridis_d() 
plotly::ggplotly()


country %>% 
  select(location, total_cases_per_million, distrust_people, democracy_index, gdp_per_capita, ghs, 
         pop.km2, total_tests_per_thousand,
         sch_enrol_per_cap) %>% 
  na.omit() %>% 
  # pull(location)
  lm(total_cases_per_million ~ . + log(gdp_per_capita) -location -gdp_per_capita, .) %>% 
  screenreg()


highcharter::hchart(country$distrust_people)

country %>% 
  lm(total_cases_per_million ~ distrust_people + democracy_index + gdp_per_capita +
       ghs + pop.km2 + aged_65_older + sch_enrol_per_cap, .) %>% 
  screenreg()
country %>% 
  lm(total_cases_per_million ~ sch_enrol_per_cap + distrust_people + gdp_per_capita + 
       ghs + pop.km2 + total_tests_per_thousand + democracy_index + gdp_growth, .) %>% 
  screenreg()
country %>% 
  ggplot() +
  geom_point(aes(distrust_people, total_cases_per_million, group = location)) +
  geom_smooth(method = lm, aes(distrust_people, total_cases_per_million, colour = continent),
              se = FALSE)
plotly::ggplotly()


country %>% 
  lm(total_cases_per_million ~ distrust_people + total_tests_per_thousand + gdp_per_capita + 
       gdp_growth + aged_65_older + pop.km2 + health_spending_pct_gdp + democracy_index, .) %>% 
  screenreg()

country %>% 
  filter(continent == "Europe") %>% 
  ggplot() +
  geom_point(aes(pop.km2, total_deaths_per_million, colour = continent, group = location)) +
  geom_smooth(aes(pop.km2, total_deaths_per_million), 
              method = lm, se = FALSE)

country$total_deaths_per_million %>% 
  mean(na.rm = T)

complete_means <- country %>% 
  select(total_cases_per_million, distrust_people, democracy_index, gdp_per_capita, ghs, 
         pop.km2, total_tests_per_thousand, gdp_growth, total_deaths_per_million) %>% 
  na.omit() %>% 
  summarise(across(everything(), mean, na.rm = TRUE), n = n()) %>% 
  gather("variable", "complete_obs")
  
country %>% 
  select(total_cases_per_million, distrust_people, democracy_index, gdp_per_capita, ghs, 
         pop.km2, total_tests_per_thousand, gdp_growth, total_deaths_per_million) %>% 
  summarise(across(everything(), mean, na.rm = TRUE), n = n()) %>% 
  gather("variable", "all_obs") %>% 
  right_join(complete_means, by = "variable") %>% 
  mutate(diff = complete_obs - all_obs)


country %>% 
  arrange(desc(total_cases_per_million)) %>% 
  select(location, total_cases_per_million)

summary(country$ghs)

country %>% pull(gwght)

linear <- country %>% 
  lm(total_cases_per_million ~ distrust_people + total_tests_per_thousand + gdp_per_capita + 
       gdp_growth + aged_65_older + pop.km2 + ghs + democracy_index + avg_stringency,
      family = "poisson", .)
poisson <- country %>% 
  glm(total_cases_per_million ~ distrust_people + total_tests_per_thousand + gdp_per_capita + 
        gdp_growth + aged_65_older + pop.km2 + ghs + democracy_index + avg_stringency,
     family = "poisson", .)
screenreg(list(linear, poisson))

exp(poisson$coefficients[1] + poisson$coefficients[2])

### main modelling
model1 <- country %>% 
  lm(total_cases_per_million ~ distrust_people + total_tests_per_thousand + gdp_per_capita + 
       gdp_growth + aged_65_older + pop.km2 + health_spending_pct_gdp + democracy_index +
       avg_stringency, .)
model1_robust <- coeftest(model1, vcov. = vcovHC(model1, "HC3"))

screenreg(list(model1, model1_robust))

poisson_robust <- coeftest(poisson, vcov. = vcovHC(poisson, "HC3"))

screenreg(list(poisson, poisson_robust))

# testing non-linear relationships
country_modeling <- country %>% 
  select(total_cases_per_million, distrust_people, democracy_index, gdp_per_capita, 
         pop.km2, total_tests_per_thousand, gdp_growth, health_spending_pct_gdp, aged_65_older) %>% 
  na.omit()

country %>% 
  ggplot(aes(aged_65_older, total_cases_per_million)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ poly(x, 3))
plotly::ggplotly()

country %>% 
  ggplot(aes(health_spending_pct_gdp, total_cases_per_million)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ poly(x, 1))

poly_model <- country %>% 
  lm(total_cases_per_million ~ distrust_people + total_tests_per_thousand + gdp_per_capita + 
       gdp_growth + poly(aged_65_older, 3) + pop.km2 + health_spending_pct_gdp + democracy_index, .) 

pred_poisson <- predict(poisson, 
                    tibble(distrust_people = seq(0, 1, by = 0.01),
                           total_tests_per_thousand = median(country$total_tests_per_thousand,
                                                                         na.rm = TRUE),
                           gdp_per_capita = median(country$gdp_per_capita, na.rm = T),
                           gdp_growth = median(country$gdp_growth, na.rm = T),
                           aged_65_older = median(country$aged_65_older, na.rm = T),
                           pop.km2 = median(country$pop.km2),
                           ghs = median(country$ghs, na.rm = T),
                           democracy_index = median(country$democracy_index, na.rm = T)))
pred_linear <- predict(model1, 
                        tibble(distrust_people = seq(0, 1, by = 0.01),
                               total_tests_per_thousand = median(country$total_tests_per_thousand,
                                                                 na.rm = TRUE),
                               gdp_per_capita = median(country$gdp_per_capita, na.rm = T),
                               gdp_growth = median(country$gdp_growth, na.rm = T),
                               aged_65_older = median(country$aged_65_older, na.rm = T),
                               pop.km2 = median(country$pop.km2),
                               ghs = median(country$ghs, na.rm = T),
                               democracy_index = median(country$democracy_index, na.rm = T)))
pred_poisson_mean <- predict(poisson, 
                        tibble(distrust_people = seq(0, 1, by = 0.01),
                               total_tests_per_thousand = mean(country$total_tests_per_thousand,
                                                                 na.rm = TRUE),
                               gdp_per_capita = mean(country$gdp_per_capita, na.rm = T),
                               gdp_growth = mean(country$gdp_growth, na.rm = T),
                               aged_65_older = mean(country$aged_65_older, na.rm = T),
                               pop.km2 = mean(country$pop.km2),
                               ghs = mean(country$health_spending_pct_gdp, na.rm = T),
                               democracy_index = mean(country$democracy_index, na.rm = T)))
pred_poisson_manual <- predict(poisson, 
                             tibble(distrust_people = seq(0, 1, by = 0.01),
                                    total_tests_per_thousand = 342,
                                    gdp_per_capita = 30000,
                                    gdp_growth = 5,
                                    aged_65_older = 10,
                                    pop.km2 = 200,
                                    ghs = 10,
                                    democracy_index = 8))

tibble(distrust = seq(0, 1, by = 0.01),
       cases = pred_poisson) %>% 
  ggplot() +
  geom_line(aes(distrust, exp(cases))) +
  geom_point(aes(distrust_people, total_cases_per_million), country_modeling)

tibble(distrust = seq(0, 1, by = 0.01),
       cases = pred_linear) %>% 
  ggplot() +
  geom_line(aes(distrust, cases)) +
  geom_point(aes(distrust_people, total_cases_per_million), country_modeling)

tibble(distrust = seq(0, 1, by = 0.01),
       median = pred_poisson,
       mean = pred_poisson_mean,
       manual = pred_poisson_manual) %>% 
  gather("key", "cases", median, mean, manual) %>% 
  ggplot() +
  geom_line(aes(distrust, exp(cases), colour = key)) +
  geom_point(aes(distrust_people, total_cases_per_million), country_modeling)


xwcountry_modeling$poisson_pred <- predict(poisson, type = "response")

country_modeling %>% 
  ggplot(aes(distrust_people, poisson_pred)) +
  geom_line()
















