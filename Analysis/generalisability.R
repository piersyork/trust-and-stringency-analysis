options(box.path = getwd())
box::use(dplyr[...], 
         ggplot2[...],
         cowplot[...],
         purrr[reduce],
         rgdal[readOGR],
         broom[tidy],
         functions/ts[plot_sample_map, load_project_data, effective_sample],
         texreg[screenreg],
         lme4[lmer],
         sf[...])

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

data %>% 
  mutate(sample = ifelse(is.na(distrust_people), 0, 1)) %>% 
  group_by(sample) %>% 
  summarise(variarble = mean(gdp_per_capita, na.rm = TRUE)) 

gdp <- compare_sample(gdp_per_capita)
ethnic <- compare_sample(ethnic)
ghs <- compare_sample(ghs)
educ <- compare_sample(education_index)
pop <- compare_sample(pop.km2)
polity <- compare_sample(polity2)

list(gdp, ethnic, ghs, educ, pop, polity) %>% 
  reduce(left_join)



load_project_data()
### Draw map of counties in sample

cntry_data <- data %>% 
  select(location, max_stringency, distrust_people, ) %>% 
  distinct()

# import map data
map_data <- readRDS("Map Data/map_data.rds") %>% 
  mutate(SOVEREIGNT = recode(SOVEREIGNT, 
                             "United States of America" = "United States",
                             "Czechia" = "Czech Republic",
                             "Republic of Serbia" = "Serbia")) %>% 
  left_join(cntry_data, by = c("SOVEREIGNT" = "location")) %>%
  mutate(sample = ifelse(!is.na(distrust_people), 1, 0))

map_data %>% 
  ggplot(aes(fill = factor(sample))) +
  geom_sf(show.legend = FALSE, colour = "white", size = 0.05) + #color = "black", size = 0.1, 
  scale_fill_manual(values = c("grey", "black")) + #"#deebf7", "#3182bd"
  # scale_fill_brewer(direction = 1) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.05),
        plot.tag.position = c(0.95, 1), plot.tag = element_text(size = 10))

library(leaflet)


pal <- leaflet::colorNumeric(
  palette = "Reds",
  domain = map_data$max_stringency
)
pal_leg <- leaflet::colorNumeric(
  palette = "Reds",
  domain = map_data$max_stringency,
  na.color = NA
)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  map_data$owid_name, map_data$max_stringency
) %>% lapply(htmltools::HTML)
map_data %>% 
  leaflet::leaflet() %>%
  leaflet:: addPolygons(
    fillColor = ~pal(max_stringency),
    weight = 0.2,
    opacity = 1,
    color = "black",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = leaflet::highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  leaflet::addLegend(pal = pal_leg, values = ~max_stringency, opacity = 0.7, title = NULL,
                     position = "bottomleft", labFormat = leaflet::labelFormat())

grep("United", unique(data$location), value = TRUE)
grep("United", unique(map_data$SOVEREIGNT), value = TRUE)

unique(data$location)[!unique(data$location) %in% map_data$SOVEREIGNT]

.formula <- stringency_index ~ distrust_people + conf_govt + total_deaths_per_million +
  log_conflict + deaths_per_mil_lag_5 +
  log_gdp + ghs + pop.km2 + ethnic + education_index +
  polity2 + regime_type

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

