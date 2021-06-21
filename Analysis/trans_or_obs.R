options(box.path = getwd())
box::use(dplyr[...],
         lme4[lmer, isSingular],
         texreg[screenreg],
         plm[plm],
         sandwich[vcovCL, vcovHC],
         lmtest[coeftest],
         ggplot2[...],
         magrittr[use_series, extract],
         # import own project functions
         functions/ts[get_coefs, load_project_data, test_lag])

# imports data for the project (function defined in functions/ts.r)
load_project_data()

.formula <- stringency_index ~ distrust_people + conf_govt + ghs + ethnic + regime_type + 
  pop.km2 + continent + education_index + log_gdp + log_conflict + 
  deaths_per_mil_lag_5 + (1 | location)

vars <- c("deaths_per_mil_lag_5", "conf_govt", "ghs", "pop.km2",
          "regime_type", "ethnic", "log_gdp", "log_conflict",
          "education_index", "continent")

data$trans_chng_lag_34

model_x <- lmer(.formula, data)
model_y <- lmer(update(.formula, ~ . + trans_chng_lag_34), data)

model_z <- data %>% 
  select(location, vars, stringency_index, trans_chng_lag_34, distrust_people) %>% 
  na.omit() %>% 
  lmer(.formula, .)

screenreg(list(model_x, model_y, model_z))

methods(class = "merMod")

model <- lmer(update(.formula, ~ . + trans_chng_lag_34 - deaths_per_mil_lag_5), data)

plot_homogeneity(model)

resid(model_x) %>% 
  as_tibble(rownames = "index") %>% 
  mutate(index = as.numeric(index)) %>% 
  ggplot(aes(index, value)) +
  geom_point(alpha = 0.2) +
  geom_smooth(span = 1)

plot_linearity(model_x)
plot(model_x, resid(., scaled=TRUE) ~ fitted(.), abline = 0,pch=16,xlab="Fitted values",ylab="Standardised residuals")
rstandard(model_y)






