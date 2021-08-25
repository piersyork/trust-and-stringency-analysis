options(box.path = getwd())
box::use(dplyr[...],
         readr[read_csv],
         tidyr[gather, spread],
         functions/ts[load_project_data],
         ggplot2[...])
schools <- read_csv("~/Downloads/yougov-close-schools.csv")
# schools <- read_csv("~/Downloads/yougov-cancel-events.csv")

out <- vector()
for (i in 2:ncol(schools)) {
  mean_support <- schools[, i] %>% 
    pull() %>% 
    as.numeric() %>% 
    # na.omit() %>%
    mean(na.rm = TRUE)
  
  out <- c(out, mean_support)
}
names(out) <- colnames(schools)[-1]
close_schools <- data.frame(out) %>% 
  as_tibble(rownames = "country") %>% 
  rename(close_schools = out) %>% 
  mutate(country = recode(country,
                          "UK" = "United Kingdom",
                          "USA" = "United States"))
  
# index <- close_schools$country %in% unique(data$location)
# length(index)
# length(close_schools$country)
# close_schools$country[!index]

load_project_data()

distrust <-
  data %>% 
  distinct(location, distrust_people) 

close_schools %>% 
  left_join(distrust, by = c("country" = "location")) %>% 
  na.omit() %>% 
  ggplot(aes(distrust_people, close_schools)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)





