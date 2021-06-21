options(box.path = getwd())
box::use(dplyr[...], 
         ggplot2[...],
         cowplot[...],
         purrr[reduce],
         rgdal[readOGR],
         broom[tidy],
         functions/ts[plot_sample_map, load_project_data, effective_sample],
         texreg[screenreg],
         lme4[lmer])

load("Data/panel_data.Rdata")

compare_sample <- function(var) {
  var <- enquo(var)
  load("Data/panel_data.Rdata")
  var_length <- data %>% 
    filter(location == "United States") %>% 
    distinct(!!var) %>% 
    nrow()
  if (var_length > 1) {
    out <- data %>% 
      mutate(sample = ifelse(is.na(distrust_people), 0, 1)) %>% 
      group_by(sample, date) %>% 
      summarise(variable = mean(!!var, na.rm = TRUE)) %>% 
      ggplot(aes(date, variable, colour = factor(sample))) +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "4 months") +
      geom_smooth(span = 0.1, se = FALSE) +
      labs(title = paste(as_label(var), "for countries in the sample \ncompared to those not"),
           x = "", y = "", colour = "")
  } else {
    out <- data %>% 
      mutate(sample = ifelse(is.na(distrust_people), 0, 1)) %>% 
      group_by(sample) %>% 
      summarise(variarble = mean(!!var, na.rm = TRUE)) 
    colnames(out)[2] <- as_label(var)
  }
  
  return(out)
}

theme_set(theme_minimal())

deaths <- compare_sample(new_deaths_per_million) 
stringency <- compare_sample(stringency_index)
trans_chng <- compare_sample(trans_pct_chng)

leg <- get_legend(deaths)

theme_set(theme_minimal() + 
            theme(title = element_blank(),
                  legend.position = "none",
                  plot.margin = margin(rep(0.5, 4), unit = "cm")))

title <- ggdraw() +
  draw_label("Comparing countries with trust data available to those without",
             fontface = "bold",
             x = 0,
             hjust = 0) +
  theme(plot.margin = margin(0.5, 0, 0, 0))

top_row <- plot_grid(deaths, stringency, 
                     labels = c("deaths", "stringency"),
                     label_size = 12, 
                     label_y = 1.05,
                     rel_widths = c(1, 1))

bot_row <- plot_grid(trans_chng, leg,
                     labels = c("transport change", ""),
                     label_size = 12, 
                     label_y = 1.05,
                     rel_widths = c(3, 1))

plot_grid(title, top_row, bot_row,
          ncol = 1,
          rel_heights = c(0.2, 1, 1))



gdp <- compare_sample(gdp_per_capita)
ethnic <- compare_sample(ethnic)
ghs <- compare_sample(ghs)
educ <- compare_sample(education_index)
pop <- compare_sample(pop.km2)
democ <- compare_sample(democracy_index)

list(gdp, ethnic, ghs, educ, pop, democ) %>% 
  reduce(left_join)



load_project_data()
### Draw map of counties in sample


# import map data
map_data <- readOGR("Map Data/World_Countries/", "World_Countries")

# remove Antarctica from map and tidy map data
map_tidy <- tidy(map_data, "COUNTRY") %>% 
  filter(!id == "Antarctica")

cntry_data <- data %>% 
  select(location, max_stringency, distrust_people, ) %>% 
  distinct()
  

map_tidy %>% 
  left_join(cntry_data, by = c("id" = "location")) %>%
  mutate(sample = ifelse(!is.na(distrust_people), 1, 0)) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = factor(sample))) +
  geom_polygon(show.legend = FALSE, colour = "grey", size = 0.05) + #color = "black", size = 0.1, 
  scale_fill_manual(values = c("white", "black")) + #"#deebf7", "#3182bd"
  #scale_fill_brewer(direction = 1) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.05),
        plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))


.formula <- stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
  log_conflict + deaths_per_mil_lag_5 +
  log_gdp + ghs + pop.km2 + ethnic + education_index +
  democracy_index + regime_type

data %>% 
  plot_sample_map(map_tidy, .formula)
df <- effective_sample(data, .formula)

df %>% 
  arrange(desc(weights_pct)) %>% 
  mutate(cum_sum = round(cumsum(weights_pct), 2)) %>% 
  arrange(weights_pct) %>% 
  mutate(cum_sum2 = round(cumsum(weights_pct), 2))

formula_lmer <- update(.formula, ~ . + (1 | location))
data %>% 
  filter(continent == "Asia") %>% 
  lmer(formula_lmer, .) %>% 
  screenreg()

data$continent %>% unique()

