box::use(dplyr[...], 
         ggplot2[...],
         cowplot[...],
         purrr[reduce])
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
res_chng <- compare_sample(res_pct_chng)

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

bot_row <- plot_grid(res_chng, leg,
                     labels = c("res", ""),
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











