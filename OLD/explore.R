library(tidyverse)
library(york)

load("Raw/WVSEVS_Joint_v1-0-0.rdata")
values <- `WVSEVS_Joint_v1-0-0`

values %>% 
  group_by(cntry_AN) %>% 
  summarise(mean = mean(A001, na.rm = T)) %>% 
  view()

gb <- values %>% 
  filter(cntry_AN == "GB")
write_csv(gb, "Data/gb_survey.csv")
gb %>% 
  group_by(reg_nuts1) %>% 
  summarise(mean = mean(A165, na.rm = T),
            n = n()) %>% 
  arrange(mean)



gb %>% 
  diff_means(A165, reg_nuts1, "UKI")

values %>% 
  group_by(cntry_AN) %>% 
  summarise(mean = mean(A165, na.rm = T),
            n = n()) %>% 
  arrange(mean)

mean(values$A165, na.rm = T)
table(values$G007_18_B, values$cntry_AN) %>% 
  prop.table(2) * 100
table(values$A165, values$cntry_AN) %>% 
  prop.table(2) * 100

values$cntry
