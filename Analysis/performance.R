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
         lubridate[date, as_date],
         magrittr[use_series, extract],
         readr[read_csv],
         # import own project functions
         functions/ts[...],
         performance[check_model],
         ggsci[scale_fill_lancet],
         functions/ap[plot_vif])

load_project_data()

# Set theme
my_theme <- theme_minimal() +
  theme(legend.position = "top",
        axis.line.x = element_line(), axis.ticks.x = element_line(),
        plot.caption = element_text(hjust = 0))

theme_set(my_theme)
         
modelx <- lmer(stringency_index ~ distrust_people + conf_govt + ghs + ethnic + regime_type +
       democracy_index + pop.km2 + log_gdp + log_conflict + deaths_per_mil_lag_5 +  continent +
       (1 | location), data)
screenreg(modelx)

# model_check <- check_model(modelx)
# 
# model_check$INFLUENTIAL %>% 
#   # tibble() %>%   
#   filter(Influential == "Influential")
# 
methods(class = "merMod")
# model.frame(modelx) %>% 
#   as_tibble(rownames = "index") %>% 
#   filter(index == 10801) %>% 
#   select(location)
# 
# model_check$VIF$group
# colinearity <- tibble(location = model_check$VIF$x,
#                       vif = model_check$VIF$y,
#                       group = model_check$VIF$group)
# colinearity %>% 
#   ggplot(aes(location, vif, fill = group)) +
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


modelx2 <- lmer(stringency_index ~ distrust_people + conf_govt + ghs + ethnic + 
                 pop.km2 + log_gdp + log_conflict + deaths_per_mil_lag_5 + continent +
                  democracy_index +
                 (1 | location), data)
form <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic +  
  democracy_index + pop.km2 + log_gdp + log_conflict + deaths_per_mil_lag_5 +  
  (1 | location)

model <- lmer(form, data)


plot_vif(model) +
  theme(axis.text.x = element_text(size = 11))


std.res




