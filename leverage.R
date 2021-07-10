options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular, fixef],
         texreg[screenreg, htmlreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         forcats[fct_reorder],
         rgdal[readOGR],
         broom[tidy],
         magrittr[use_series, extract],
         ggsci[scale_fill_lancet],
         influence.ME[influence, cooks.distance.estex, dfbetas.estex, sigtest, plot.estex],
         # import own project functions
         functions/ts[...], functions/ap[...])

load_project_data()

form <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic + pop.km2 + gdp_growth + education_index + 
  log_gdp + log_conflict + deaths_per_mil_lag_5 + democracy_index_2 + 
  (1 | location)

model <- lmer(form, data)
screenreg(model)
model_les_nic <- lmer(form, data %>% filter(location != "Guatemala"))
screenreg(list(model, model_les_nic))

inf <- influence(model, group = "location")
cd_df <- cooks.distance.estex(inf) %>% 
  as_tibble(rownames = "location") %>% 
  `colnames<-`(c("location", "cooks_distance")) %>% 
  arrange(desc(cooks_distance)) %>% 
  mutate(., influence = ifelse(cooks_distance > 4/nrow(.), "high", "low")) 

dfb <- dfbetas.estex(inf) %>% 
  as_tibble(rownames = "location") %>% 
  select(location, distrust_people) %>% 
  mutate(. ,mod_distrust_people = Mod(distrust_people),
         influence = ifelse(mod_distrust_people > 2/sqrt(nrow(.)), "high", "low")) %>% 
  arrange(desc(mod_distrust_people)) %>% 
  select(-mod_distrust_people)
sigtest(inf)$distrust_people %>% 
  as_tibble(rownames = "location") %>% 
  arrange(Altered.Teststat)

plot(inf, "dfbetas", parameters = "distrust_people", sort = TRUE, to.sort = "distrust_people")
dfb %>% 
  ggplot(aes(distrust_people, 
             fct_reorder(location, distrust_people), 
             colour = factor(influence, levels = c("low", "high")))) +
  geom_point() +
  geom_vline(xintercept = 0) +
  ggsci::scale_colour_lancet() +
  labs(colour = "", y = "", x = "DFBETAS") +
  theme_minimal() +
  theme(legend.position = "top")
cd_df %>% 
  ggplot(aes(cooks_distance, 
             fct_reorder(location, cooks_distance), 
             colour = influence)) +
  geom_point()

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

# using ggplot maps instead
world <- map_data("world") %>%
  # select(lon = long, lat, group, region) %>%
  filter(region != "Antarctica")

# world$region %>% unique() %>% grep("congo", ., value = TRUE, ignore.case = TRUE)
# data$Entity %>% unique() %>% grep("congo", ., value = TRUE, ignore.case = TRUE)

world$region <- recode(world$region,
                       "USA" = "United States",
                       "UK" = "United Kingdom",
                       "Republic of Congo" = "Congo",
                       "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                       "Ivory Coast" = "Cote d'Ivoire")

world %>% 
  left_join(cd_df, by = c("region" = "location")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cooks_distance, label = region)) +
  geom_polygon(show.legend = FALSE, color = "grey", size = 0.1) + #
  scale_fill_continuous(high = "#01468B", low = "white", na.value = "white") + #3182bd
  # scale_fill_distiller(direction = 1, na.value = "white") +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.05), 
        plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))
  


tibble(leverage = hatvalues(model2), 
       std.resid = HLMdiag::pull_resid(model2, standardize = TRUE, type = "eb")) %>% 
  ggplot(aes(leverage, std.resid)) +
  geom_point()

plot_cooks_distance(model)

cd_df %>% 
  mutate(high = ifelse(cooks_distance >= mean(cd_df$cooks_distance)*3, "high", "low")) %>% 
  ggplot(aes(forcats::fct_reorder(location, cooks_distance), cooks_distance, 
             fill = factor(high, c("low", "high")))) +
  geom_col(width = 0.7, show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  scale_fill_lancet() +
  labs(x = "", fill = "", caption = "Red indicates a high cook's distance (three time the mean).")


cd_df %>% 
  ggplot(aes(cooks_distance)) +
  geom_histogram()

influence.ME::influence()

summary(cd_df$cooks_distance)







