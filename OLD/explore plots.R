box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg],
         plm[plm],
         ggplot2[...],
         rgdal[readOGR],
         broom[tidy],
         readxl[read_xlsx],
         tidyr[gather],
         lubridate[dmy],
         RColorBrewer[...],
         plotly[...])

load("Data/panel_data.Rdata")
stringency_raw <- read_xlsx("Raw/OxCGRT_timeseries_all.xlsx")

stringency <- stringency_raw %>% 
  gather("date", "stringency_index", -country_code, - country_name) %>% 
  mutate(date = dmy(date)) %>% 
  rename(location = country_name, 
         alpha.3 = country_code)

map_data <- readOGR("Map Data/World_Countries/", "World_Countries")

map_tidy <- tidy(map_data, "COUNTRY") %>% 
  mutate(id = if_else(id == "Greenland (Denmark)", "Greenland", id),
         id = if_else(id == "Democratic Republic of the Congo", "Democratic Republic of Congo", id)) %>% 
  filter(id != "Antarctica")

# 
countries <- map_tidy %>% 
  distinct(id)
grep(pattern = "French", countries$id, value = TRUE)


stringency_max <- stringency %>% 
  select(location, stringency_index) %>% 
  group_by(location) %>% 
  summarise(stringency = max(stringency_index, na.rm = TRUE)) 
grep(pattern = "Guiana", stringency_max$location, value = TRUE)

map_tidy %>% 
  left_join(stringency_max, by = c("id" = "location")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = stringency, text = paste0("Country: ", id, 
                                                                                "<br>",
                                                                                "Max Stringency: ", stringency))) +
  geom_polygon(color = "black", size = 0.1, show.legend = FALSE) + #, show.legend = FALSE
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  coord_equal() +
  theme_void() 
plotly::ggplotly(tooltip = "text") %>% 
  # plotly::style(marker.color = "red") %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = c("autoScale2d", "resetScale2d", "toggleSpikelines",
                                                         "hoverClosestCartesian", "hoverCompareCartesian"))

stringency_avg <- stringency %>% 
  select(location, stringency_index) %>% 
  group_by(location) %>% 
  summarise(stringency = round(mean(stringency_index, na.rm = TRUE), 2))

map_tidy %>% 
  left_join(stringency_avg, by = c("id" = "location")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = stringency, text = paste0("Country: ", id, 
                                                                                "<br>",
                                                                                "Avg. Stringency: ", stringency))) +
  geom_polygon(color = "gray", size = 0.05, show.legend = FALSE) + #, show.legend = FALSE
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  coord_equal() +
  theme_void() 
ggplotly(tooltip = "text") %>% 
  # plotly::style(marker.color = "red") %>%
  config(displaylogo = FALSE, modeBarButtonsToRemove = c("autoScale2d", "resetScale2d", "toggleSpikelines",
                                                         "hoverClosestCartesian", "hoverCompareCartesian"))




