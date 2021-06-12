box::use(dplyr[...],
         magrittr[use_series])

load("Raw/WVSEVS_Joint_v1-0-0.rdata")
values <- fixed %>% 
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
  mutate(distrust_people = ifelse(distrust_people < 0, NA,  distrust_people - 1),
         conf_govt = case_when(conf_govt %in% 1:2 ~ 1,
                               conf_govt %in% 3:4 ~ 0))

values %>% 
  filter(cntry_AN == "GB") %>% 
  use_series(conf_govt) %>% 
  table(useNA = "ifany") %>% 
  prop.table() %>% 
  magrittr::multiply_by(100)
  