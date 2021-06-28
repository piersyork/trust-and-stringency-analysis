
#' @export
plot_vif <- function(model) {
  box::use(dplyr[...],
           ggplot2[...],
           ggsci[scale_fill_lancet])
  rects <- tibble(ystart = c(0, 5, 10), 
                  yend = c(5, 10, Inf), 
                  group = c("low", "moderate", "high")) %>% 
    mutate(group = factor(group, c("moderate", "high", "low")))
  
  vif_df <- car::vif(model) %>% 
    data.frame() %>% 
    as_tibble(rownames = "location")
  
  if (length(colnames(vif_df)) == 2) {
    colnames(vif_df)[2] <- "VIF"
    vif_df %>% 
      mutate(group = case_when(VIF < 5 ~ "low",
                               VIF >= 5 & VIF < 10 ~ "moderate",
                               VIF >= 10 ~ "high"),
             group = factor(group, levels = c("moderate", "high", "low"))) %>% 
      ggplot() +
      geom_rect(aes(ymin = ystart, ymax = yend, xmin = -Inf, xmax = Inf, fill = group),
                data = rects, alpha = 0.3) +
      geom_col(aes(location, VIF, fill = group)) +
      labs(x = "", fill = "") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)) +
      scale_fill_lancet(breaks = c("low", "moderate", "high"))
  } else {
    vif_df %>% 
      mutate(group = case_when(GVIF < 5 ~ "low",
                               GVIF >= 5 & GVIF < 10 ~ "moderate",
                               GVIF >= 10 ~ "high"),
             group = factor(group, levels = c("moderate", "high", "low"))) %>% 
      ggplot() +
      geom_rect(aes(ymin = ystart, ymax = yend, xmin = -Inf, xmax = Inf, fill = group),
                data = rects, alpha = 0.3) +
      geom_col(aes(location, GVIF, fill = group)) +
      labs(x = "", fill = "",
           title = "Multicolinearity") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, size = 11)) +
      scale_fill_lancet(breaks = c("low", "moderate", "high"))
  }
}

#' @export
plot_linearity <- function(model) {
  box::use(dplyr[...],
           ggplot2[...],
           stats[fitted, residuals])
  tibble(fitted = fitted(model), resid = residuals(model)) %>% 
    ggplot(aes(fitted, resid)) +
    geom_point(colour = "#01468B") +
    geom_smooth(method = "gam") +
    geom_hline(yintercept = 0, colour = "#42B540", linetype = "dashed", size = 1) +
    labs(title = "Linearity")
}

#' @export
plot_homogeneity <- function(model) {
  box::use(dplyr[...],
           HLMdiag[pull_resid],
           ggplot2[...],
           stats[fitted])
  tibble(fitted = fitted(model), 
         std.resid = pull_resid(model, standardize = TRUE, type = "eb")) %>% 
    ggplot(aes(fitted, std.resid)) + # sqrt(Mod(std.resid))
    geom_point(colour = "#01468B", alpha = 0.3) +
    geom_hline(yintercept = 0, colour = "#42B540", linetype = "dashed", size = 1) +
    # geom_smooth(colour = "#42B540") +
    labs(x = "Fitted Values", y = "Standardised Residuals")
}

#' @export
plot_cooks_distance <- function(model) {
  box::use(dplyr[...],
           ggplot2[ggplot, aes, geom_point],
           HLMdiag[pull_resid],
           influence.ME[influence, cooks.distance.estex],
           stats[hatvalues])
  inf <- influence(model, group = "location")
  cd_df <- cooks.distance.estex(inf) %>% 
    as_tibble(rownames = "location") %>% 
    `colnames<-`(c("location", "cooks_distance")) %>% 
    arrange(desc(cooks_distance))
  tibble(leverage = hatvalues(model), 
         std.resid = pull_resid(model, standardize = TRUE, type = "eb")) %>% 
    ggplot(aes(leverage, std.resid)) +
    geom_point()
}

#' @export
plot.merMod = plot_homogeneity

#' @export
plot.lme = plot_homogeneity










