options(box.path = getwd())
box::use(dplyr[...],
         ggplot2[...],
         functions/ts[load_project_data, plot_summarised],
         lubridate[as_date, date])

load_project_data()

# Set theme
my_theme <- theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.line.x = element_line(), axis.ticks.x = element_line(),
        plot.caption = element_text(hjust = 0))

theme_set(my_theme)

data %>% 
  select(distrust_people, log_gdp, gdp_growth, log_conflict, education_index) %>% 
  distinct() %>% 
  gtsummary::tbl_summary(
    type = gtsummary::all_continuous() ~ "continuous2",
    statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})")),
    missing_text = "Missing Data",
    label = list(distrust_people ~ "Distrust in People",
                 log_gdp ~ "Log of GDP per Capita",
                 gdp_growth ~ "GDP Growth",
                 log_conflict ~ "Log of Conflict Index",
                 education_index ~ "Education Index")
  ) %>%
  gtsummary::modify_header(list(label ~ "**Variable**")) %>%
  gtsummary::italicize_labels() %>%
  gtsummary::add_stat_label(label = list(gtsummary::all_continuous() ~ c("Mean (SD)"))) %>%
  gtsummary::as_gt() %>%
  gt::tab_source_note(source_note = "SD = Standard Deviation")

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
  group_by(trusting, date) %>% #trusting, 
  summarise(stringency_index = mean(stringency_index)) %>% 
  ggplot(aes(date, stringency_index, colour = factor(trusting))) + #, colour = factor(trusting)
  geom_smooth(span = 0.01, se = FALSE, size = 0.5) +
  # geom_line() +
  scale_x_date(limits = c(as_date("2020-01-01"), as_date("2021-03-01")), 
               date_breaks = "10 weeks", date_labels = "%b %Y") +
  labs(title = "Stringency Index",
       x = "", y = "", colour = "",
       caption = "Source: Oxford COVID-19 Government Response Tracker") +
  ggsci::scale_color_lancet()


median(data$distrust_people)

my_stats <- function(x) {
  tibble(mean = mean(x), sd = sd(x))
}

data %>% 
  select(location, date, stringency_index) %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(my_stats(stringency_index)) %>% 
  ggplot(aes(date, sd)) +
  geom_line() +
  # geom_line(aes(date, mean)) +
  scale_x_date(limits = c(as_date("2020-01-01"), as_date("2021-03-01")), 
               date_breaks = "10 weeks", date_labels = "%b %Y")



# plot of financial support: 
data %>% 
  select(location, date, trusting, support_index) %>% 
  na.omit() %>% 
  group_by(trusting, date) %>% # 
  summarise(support_index = mean(support_index)) %>% 
  ggplot(aes(date, support_index, colour = factor(trusting))) + #, colour = factor(trusting)
  geom_smooth(span = 0.01, se = FALSE, size = 0.5) +
  # geom_line() +
  scale_x_date(limits = c(as_date("2020-01-01"), as_date("2021-03-01")), 
               date_breaks = "10 weeks", date_labels = "%b %Y") +
  labs(title = "Economic Support Index",
       x = "", y = "", colour = "",
       caption = "Source: Oxford COVID-19 Government Response Tracker") +
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

# plot of change in transport
data %>% 
  select(location, date, trusting, trans_pct_chng) %>% 
  na.omit() %>%
  group_by(trusting, date) %>% 
  summarise(trans_pct_chng = mean(trans_pct_chng)) %>% 
  ggplot(aes(date, trans_pct_chng, colour = factor(trusting))) +
  geom_smooth(span = 0.1, se = FALSE, size = 0.5) +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  scale_x_date(limits = c(as_date("2020-03-01"), as_date("2021-03-01")), 
               date_breaks = "8 weeks", date_labels = "%B") +
  labs(title = "Percent change in transport",
       x = "", y = "", colour = "",
       caption = "Source: Google Mobility Report") +
  ggsci::scale_color_lancet()


pool <- data %>% 
  # filter(!location == "Nicaragua") %>%
  plot_summarised(distrust_people, stringency_index)

plotly::ggplotly()

data %>% 
  plot_summarised(conf_govt, stringency_index)
data %>% 
  plot_summarised(conf_govt, distrust_people)
data %>% 
  select(conf_govt, distrust_people) %>% 
  distinct() %>% 
  lm(distrust_people ~ conf_govt, .) %>% 
  screenreg()

data %>% 
  plot_summarised(education_index, distrust_people, .poly = 2)
data %>% 
  plot_summarised(log_gdp, stringency_index)
data %>% 
  plot_summarised(gdp_growth, distrust_people)
data %>% 
  plot_summarised(ghs, distrust_people)
data %>% 
  mutate(log_pop_km2 = log(pop.km2)) %>% 
  plot_summarised(log_pop_km2, distrust_people)
data %>% 
  plot_summarised(pop_65, stringency_index)
data %>% 
  plot_summarised(gini_disp, stringency_index)

data %>% 
  plot_summarised(new_deaths_per_million, distrust_people)

data %>% 
  plot_summarised(new_deaths_per_million, distrust_people)
data %>% 
  plot_summarised(polity2, log_conflict)

data %>%
  select(trans_pct_chng, date, location) %>% 
  na.omit() %>% 
  use_series(date) %>% 
  max()

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


data %>% 
  select(regime_type, location, distrust_people) %>% 
  na.omit() %>% 
  distinct() %>% 
  ggplot(aes(factor(regime_type, c("Authoritarian", "Hybrid regime",
                                   "Flawed democracy", "Full democracy")), distrust_people)) +
  geom_boxplot()
data %>% plot_summarised(polity2, distrust_people)

data %>% 
  select(regime_type, location, distrust_people, polity2) %>% 
  na.omit() %>% 
  distinct() %>% 
  ggplot(aes(polity2, distrust_people, colour = regime_type)) +
  geom_point() +
  geom_smooth(method = lm)


  
data %>% 
  filter(location %in% c("Denmark", "Albania")) %>% 
  ggplot(aes(date, stringency_index, colour = location)) +
  geom_line()







