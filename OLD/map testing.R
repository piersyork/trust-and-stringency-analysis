library(sparklyr)

country <- read_csv("Data/country_data.csv") %>% 
  select(location, gdp_per_capita)
sc <- spark_connect(master = "local", version = "2.3")
  
cars <- copy_to(sc, mtcars)

spark_web(sc)
count(cars)
select(cars, hp, mpg) %>%
  sample_n(100) %>%
  collect() %>%
  plot()

maps::map()

help(package = "maps")

world <- map_data("world", ) %>% 
  filter(region != "Antarctica") %>% 
  left_join(country, by = c("region" = "location"))

ggplot(world, aes(long, lat, group = group, fill = gdp_per_capita)) +
  geom_polygon() +
  coord_quickmap()

ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) + 
  coord_map("ortho", orientation = c(41, -74, 0))
