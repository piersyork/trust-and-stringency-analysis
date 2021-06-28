options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg, htmlreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         rgdal[readOGR],
         broom[tidy],
         magrittr[use_series, extract],
         ggsci[scale_fill_lancet],
         influence.ME[influence, cooks.distance.estex],
         # import own project functions
         functions/ts[...], functions/ap[...])

load_project_data()

form <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic +
  democracy_index + pop.km2 + log_gdp + log_conflict + deaths_per_mil_lag_5 +
  (1 | location)

model <- lmer(update(form, ~ .), data)


inf <- influence(model, group = "location")
cd_df <- cooks.distance.estex(inf) %>% 
  as_tibble(rownames = "location") %>% 
  `colnames<-`(c("location", "cooks_distance")) %>% 
  arrange(desc(cooks_distance))

most_locat <- head(cd_df, 10) %>% pull(location)

data2 <- data %>% 
  filter(!location %in% most_locat) 

model2 <- lmer(form, data2)
screenreg(list(model, model2))
inf2 <- influence(model2, group = "location")
cd_df <- cooks.distance.estex(inf2) %>% 
  as_tibble(rownames = "location") %>% 
  `colnames<-`(c("location", "cooks_distance")) %>% 
  arrange(desc(cooks_distance)) %>% 
  print(n=100)




# import map data
map_data <- readOGR("Map Data/World_Countries/", "World_Countries")

# remove Antarctica from map and tidy map data
map_tidy <- tidy(map_data, "COUNTRY") %>% 
  filter(!id == "Antarctica")

map_tidy %>% 
  left_join(cd_df, by = c("id" = "location")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cooks_distance, label = id)) +
  geom_polygon(show.legend = FALSE, color = "grey", size = 0.1) + #
  scale_fill_continuous(high = "#01468B", low = "white", na.value = "white") + #3182bd
  #scale_fill_distiller(direction = 1) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.05), 
        plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))

plotly::ggplotly()
tibble(leverage = hatvalues(model2), 
       std.resid = HLMdiag::pull_resid(model2, standardize = TRUE, type = "eb")) %>% 
  ggplot(aes(leverage, std.resid)) +
  geom_point()

plot_cooks_distance(model)

cd_df %>% 
  mutate(high = ifelse(cooks_distance >= mean(cd_df$cooks_distance)*3, "yes", "no")) %>% 
  ggplot(aes(forcats::fct_reorder(location, cooks_distance), cooks_distance, 
             fill = high)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  scale_fill_lancet()






summary(cd_df$cooks_distance)







