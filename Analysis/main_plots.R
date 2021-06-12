options(box.path = getwd())
box::use(dplyr[...],
         ggplot2[...],
         functions/ts[load_project_data, plot_summarised],
         lubridate[as_date, date])

load_project_data()

# Set theme
my_theme <- theme_minimal() +
  theme(legend.position = "top",
        axis.line.x = element_line(), axis.ticks.x = element_line(),
        plot.caption = element_text(hjust = 0))

theme_set(my_theme)


## Main Plots:
# plot of deaths: trusting vs untrusting
data %>% 
  select(location, date, trusting, new_deaths_per_million) %>% 
  na.omit() %>% 
  group_by(trusting, date) %>% 
  summarise(new_deaths_per_million = mean(new_deaths_per_million)) %>% 
  ggplot(aes(date, new_deaths_per_million, colour = factor(trusting))) +
  geom_smooth(span = 0.05, se = FALSE, size = 0.5) +
  # geom_line(alpha = 0.5) +
  scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
               limits = c(date("2020-02-01"), date("2021-03-01"))) +
  scale_y_continuous(limits = c(-0.5, 9)) +
  labs(title = "Daily deaths per million", x = "", y = "", colour = "",
       caption = "Source: Our World in Data") +
  ggsci::scale_color_lancet()

# excess deaths
weekly_data %>% 
  select(location, date, distrust_people, excess_deaths_per_100k) %>% 
  mutate(trusting = ifelse(distrust_people < median(distrust_people), "High trust", "Low trust"),
         excess_per_million = excess_deaths_per_100k * 10) %>% 
  group_by(trusting, date) %>% 
  summarise(excess_deaths = mean(excess_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(date, excess_deaths, colour = factor(trusting))) +
  geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
  # geom_line(alpha = 0.5) +
  scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
               limits = c(date("2020-02-01"), date("2021-03-01"))) +
  scale_y_continuous(limits = c(-0.5, 9)) +
  labs(title = "Estimated excess deaths per million", x = "", y = "", colour = "",
       caption = "Source: Our World in Data") +
  ggsci::scale_color_lancet()

# plot of total deaths
data %>% 
  filter(date > date("2020-03-01")) %>% 
  select(location, date, trusting, total_deaths_per_million) %>% 
  # na.omit() %>% 
  group_by(trusting, date) %>% 
  summarise(total_deaths_per_million = mean(total_deaths_per_million, na.rm = TRUE)) %>% 
  ggplot(aes(date, total_deaths_per_million, colour = factor(trusting))) +
  geom_smooth(span = 0.05, se = FALSE, size = 0.5) +
  geom_line(alpha = 0.5) +
  scale_x_date(date_breaks = "10 weeks", date_labels = "%b %Y",
               limits = c(date("2020-02-01"), date("2021-03-01"))) +
  # scale_y_continuous(limits = c(-0.5, 9)) +
  labs(title = "Total deaths per million", x = "", y = "", colour = "",
       caption = "Source: Our World in Data") +
  ggsci::scale_color_lancet()

# 

# plot of stringency: 
data %>% 
  select(location, date, trusting, stringency_index) %>% 
  na.omit() %>% 
  group_by(trusting, date) %>% 
  summarise(stringency_index = mean(stringency_index)) %>% 
  ggplot(aes(date, stringency_index, colour = factor(trusting))) +
  geom_smooth(span = 0.01, se = FALSE, size = 0.5) +
  # geom_line() +
  scale_x_date(limits = c(as_date("2020-01-01"), as_date("2021-03-01")), 
               date_breaks = "10 weeks", date_labels = "%b %Y") +
  labs(title = "Stringency Index",
       x = "", y = "", colour = "",
       caption = "Source: Our World in Data") +
  ggsci::scale_color_lancet()

# plot of residential
data %>% 
  select(location, date, trusting, res_pct_chng) %>% 
  na.omit() %>%
  group_by(trusting, date) %>% 
  summarise(res_pct_chng = mean(res_pct_chng)) %>% 
  ggplot(aes(date, res_pct_chng, colour = factor(trusting))) +
  geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
  scale_x_date(limits = c(as_date("2020-03-01"), as_date("2021-03-01")), 
               date_breaks = "8 weeks", date_labels = "%B") +
  labs(title = "Percent change in residential",
       x = "", y = "", colour = "",
       caption = "Source: Our World in Data") +
  ggsci::scale_color_lancet()




pool <- data %>% 
  filter(!location == "Nicaragua") %>%
  plot_summarised(distrust_people, stringency_index)

plotly::ggplotly()

data %>% 
  plot_summarised(conf_govt, stringency_index)

data %>% 
  plot_summarised(distrust_people, res_pct_chng)
data %>% 
  plot_summarised(stringency_index, res_pct_chng)


data %>% 
  select(location, distrust_people, conf_govt) %>% 
  distinct() %>% 
  arrange(conf_govt) %>% 
  print(n = 100)

data %>% 
  plot_summarised(conf_govt, distrust_people)

data %>% 
  lm(conf_govt ~ distrust_people, .) %>% 
  texreg::screenreg()


data %>% plot_summarised(new_deaths_per_million, stringency_index)

weekly_data %>% plot_summarised(excess_deaths_per_100k, stringency_index)


data %>% plot_summarised(distrust_people, max_stringency)

data %>% 
  mutate(log_conflict = (conflict_index)) %>% 
  plot_summarised(log_conflict, stringency_index)

data %>% 
  mutate(logged = log(ghs)) %>% 
  plot_summarised(logged, distrust_people)

data %>% 
  mutate(log_gdp = log(gdp_per_capita)) %>% 
  plot_summarised(log_gdp, stringency_index)















