options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg, htmlreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lubridate[date],
         lmtest[coeftest],
         ggplot2[...],
         rgdal[readOGR],
         broom[tidy],
         magrittr[use_series, extract],
         performance[check_model],
         # import own project functions
         functions/ts[...], functions/ap[...])

load_project_data()

# Set theme
my_theme <- theme_minimal() +
  theme(legend.position = "top",
        axis.line.x = element_line(), axis.ticks.x = element_line(),
        plot.caption = element_text(hjust = 0))

theme_set(my_theme)

.formula <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic + pop.km2 + gdp_growth + 
  log_gdp + log_conflict + deaths_per_mil_lag_5 + polity2 + education_index +
  (1 | location)

modelx <- lmer(.formula, data)
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
                 democracy_index + (1 | location), data)


model <- lmer(update(.formula, ~ . ), data) #  - deaths_per_mil_lag_5
screenreg(model)

plot_vif(model) +
  theme(axis.text.x = element_text(size = 11))

plot_homogeneity(model)

box::use(dplyr[...],
         HLMdiag[pull_resid],
         ggplot2[...],
         stats[fitted])
tibble(fitted = fitted(model), 
       std.resid = pull_resid(model, standardize = TRUE, type = "ls")) %>% 
  ggplot(aes(fitted, std.resid)) + # sqrt(Mod(std.resid))
  geom_point(colour = "#01468B", alpha = 0.2) +
  geom_hline(yintercept = 0, colour = "#42B540", linetype = "dashed", size = 1) +
  # geom_smooth(colour = "#f7634c") +
  labs(x = "Fitted Values", y = "Standardised Residuals")

HLMdiag::hlm_resid(model, level = 1, standardize = TRUE, include.ls = TRUE) %>% 
  ggplot(aes(.ls.fitted, .std.ls.resid, colour = location)) + # sqrt(Mod(std.resid))
  geom_point(alpha = 0.2, show.legend = FALSE) + # colour = "#01468B", 
  geom_hline(yintercept = 0, colour = "#42B540", linetype = "dashed", size = 1) +
  # geom_smooth(colour = "#f7634c") +
  labs(x = "Fitted Values", y = "Standardised Residuals")

data %>% 
  select(location, stringency_index, distrust_people) %>% 
  na.omit() %>% 
  group_by(location) %>% 
  count() %>% 
  print(n = 100)

plot_dfbetas(model)

vignette("hlm_resid", package = "HLMdiag")

HLMdiag::hlm_resid(model, level = "1", standardize = FALSE, include.ls = FALSE) %>% 
  select(location, .ranef.intercept) %>% 
  ggplot(aes(location, .ranef.intercept)) +
  geom_col(width = 0.15, colour = "#01468B") +
  geom_point() +
  coord_flip()

lme_form <- update(.formula, ~ . - (1 | location))
lme_model <- data %>%
  select(all.vars(lme_form), location) %>%
  na.omit() %>%
  nlme::lme(lme_form, data =  ., random =  ~ 1 | location,
            weights = nlme::varFixed(~(deaths_per_mil_lag_5 + 1))) #form = ~ fitted(.)

screenreg(list(model, lme_model))

plot_homogeneity(lme_model)
plot(model)
plot(lme_model)


methods(class = "lme")

plot
plot.merMod(model)


data %>%
  ggplot(aes(new_deaths_per_million, stringency_index)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE))

model %>% class()
# lev <- HLMdiag::leverage(model, level = "location")


data_2 <- data %>% 
  filter(date > date("2020-06-01"))

model_2 <- lmer(.formula, data_2)
screenreg(model_2)


data_2 %>%
  ggplot(aes(new_deaths_per_million, stringency_index)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm)

plot_cooks_distance(model)








