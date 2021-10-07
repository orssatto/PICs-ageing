
# Author: David N Borg
# Date: April 2021

# Libraries
library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(janitor)
library(cowplot)
library(broom.mixed)

# Load data
d = read.csv("df-data-30-04-21.csv") %>%
  clean_names() %>%
  mutate(
    muscle = as.factor(group_muscle),
    muscle = recode_factor(muscle, 'Soleus' = 'Soleus', 'Tibialis anterior' = 'Tibialis Anterior'),
    recruit_s = scale(recruit, center = T, scale = T),
    recruit_s = as.numeric(recruit_s),
    sex = as.factor(sex),
    group_age = recode_factor(group_age, 'Young adults' = 'Young Adults', 'Older adults' = 'Older Adults'),
    group_age = relevel(factor(group_age), ref = "Young Adults"),
    participant = as.factor(participant) 
  ) %>%
  select(
    -group_age_muscle, -group_muscle, -mu_n, -derecruit
  ) %>%
  drop_na(deltaf)

# Missing data
vis_miss(d)
vis_dat(d)

# Model: deltaf
fit_base <- lmer(deltaf ~ group_age*muscle + (1 + muscle | participant), data = d)
fit_recuit <- lmer(deltaf ~ group_age*muscle + recruit_s + (1 + recruit_s + muscle | participant), data = d)
fit_muscle_by_recruit <- lmer(deltaf ~ group_age*muscle + muscle*recruit_s + (1 + recruit_s + muscle | participant), data = d)
fit_age_by_others <- lmer(deltaf ~ group_age*muscle + group_age*recruit_s + (1 + recruit_s + muscle | participant), data = d, control = lmerControl(optimizer = "Nelder_Mead"))
fit_2ways <- lmer(deltaf ~ group_age*muscle + group_age*recruit_s + muscle*recruit_s + (1 + recruit_s + muscle | participant), data = d)
fit_3way <- lmer(deltaf ~ group_age*muscle*recruit_s + (1 + recruit_s + muscle | participant), data = d)

# Compare models
anova(fit_base,
      fit_recuit,
      fit_muscle_by_recruit,
      fit_age_by_others,
      fit_2ways,
      fit_3way)

# Chose best fitting model
fit = fit_recuit

# Model diagnostics
qqnorm(residuals(fit)); qqline(residuals(fit))
summary(fit)
tidy(fit, conf.int = T)
anova(fit)

## Fitted values
(refgrid <- list(group_age=c("Young Adults","Older Adults"), muscle=c("Soleus","Tibialis Anterior"), recruit_s=seq({min(d$recruit_s)},{max(d$recruit_s)}, by = 0.1)))
mar_ef <- emmip(fit, ~ group_age|muscle, at = refgrid, CIs = T, plotit = F)

# Group level
ggplot(data = d, aes(x = group_age, y = deltaf)) +
  geom_jitter(width = 0.2, alpha = 1, size = 1.5, aes(colour = participant)) +
  theme_bw(base_size = 14) +
  facet_grid(~muscle) +
  guides(fill = 'none', color = 'none') +
  geom_point(data = mar_ef, aes(x = group_age, y = yvar), 
           position = position_nudge(x = 0.3), size = 2.25) +
  geom_errorbar(data = mar_ef, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0) +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank()
  ) +
  scale_y_continuous(limits = c(-2,8.75), n.breaks = 7) +
  labs(y = "ΔF (pps)", x = "") -> plot_deltaf
plot_deltaf
#ggsave(file = "deltaf.png", units="in", width = 7, height = 4.5, dpi = 300)





# Model: fmax
fit_int <- lmer(fmax ~ group_age*muscle + (1|participant), data = d)
fit_int_and_slope <- lmer(fmax ~ group_age*muscle + (1+muscle|participant), data = d)

# Compare models
anova(fit_int, fit_int_and_slope)

# Select best fitting model
fit = fit_int_and_slope

# Model diagnostics
#plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
summary(fit)
#confint(fit)
anova(fit)

# Look at the influence of outliers on results
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>%
  dplyr::select(participant,group_age,muscle)
d_outlier
d_no_out <- d %>% anti_join(d_outlier, by = c("participant","group_age","muscle")) # keep rows without matching id

fit_refit <- lmer(fmax ~ group_age*muscle + (1|participant), data = d_no_out) # Refit model
qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))
summary(fit_refit)
anova(fit_refit)
# Note: conclusions remain the same

# Fitted values
(refgrid <- list(group_age=c("Young Adults","Older Adults"), muscle=c("Soleus","Tibialis Anterior")))
mar_ef <- emmip(fit, ~ group_age|muscle, at = refgrid, CIs = T, plotit = F)

# Group level
ggplot(data = d, aes(x = group_age, y = fmax)) +
  geom_jitter(width = 0.2, alpha = 1, size = 1.5, aes(colour = participant)) +
  theme_bw(base_size = 14) +
  facet_grid(~muscle) +
  guides(fill = 'none', color = 'none') +
  geom_point(data = mar_ef, aes(x = group_age, y = yvar), 
             position = position_nudge(x = 0.3), size = 2.25) +
  geom_errorbar(data = mar_ef, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0) +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank()
  ) +
  scale_y_continuous(limits = c(5,22.5), n.breaks = 4) +
  labs(x = "", y = "Peak Discharge Rate\n(pps)") -> plot_fmax
plot_fmax
#ggsave(file = "fmax.png", units="in", width = 7, height = 4.5, dpi = 300)








# Model: Recruitment threshold
fit_int_id <- lmer(recruit ~ group_age*muscle + (1|participant), data = d)
fit_int_slope_id <- lmer(recruit ~ group_age*muscle + (1 + muscle|participant), data = d)

# Compare models
anova(fit_int_id, fit_int_slope_id)

# Chose best fitting model
fit = fit_int_slope_id

# Model diagnostics
#plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
summary(fit)
#confint(fit)
anova(fit)

# Look at the influence of outliers on results
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>%
  dplyr::select(participant,group_age,muscle)
d_outlier
d_no_out <- d %>% anti_join(d_outlier, by = c("participant","group_age","muscle")) # keep rows without matching id

fit_refit <- lmer(recruit ~ group_age*muscle + (1 + muscle|participant), data = d_no_out) # Refit model
qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))
summary(fit_refit)
anova(fit_refit)
# Note: conclusions remain the same

# Fitted values
(refgrid <- list(group_age=c("Young Adults","Older Adults"), muscle=c("Soleus","Tibialis Anterior")))
mar_ef <- emmip(fit, ~ group_age|muscle, at = refgrid, CIs = T, plotit = F)

# Group level
ggplot(data = d, aes(x = group_age, y = recruit)) +
  geom_jitter(width = 0.2, alpha = 1, size = 1.5, aes(colour = participant)) +
  theme_bw(base_size = 14) +
  facet_grid(~muscle) +
  guides(fill = 'none', color = 'none') +
  geom_point(data = mar_ef, aes(x = group_age, y = yvar), 
             position = position_nudge(x = 0.3), size = 2.25) +
  geom_errorbar(data = mar_ef, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0) +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank()
  ) +
  labs(x = "", y = "Recruitment Threshold\n(% maximum torque)") -> plot_recruit
plot_recruit

# Panel figure fmax, deltaf, recruitment threshold
plot_grid(plot_deltaf, plot_fmax, plot_recruit,
          ncol = 1, nrow = 3,
          labels = c('(A)','(B)','(C)'),
          align = 'v',
          axis = "lr",
          label_size = 16,
          scale = 0.95)
#ggsave(file = "Figure2.png", units="in", width = 7, height = 10, dpi = 300)
ggsave(file = "Figure2.tiff", units="in", width = 7, height = 10, dpi = 300, compression = "lzw")



#### END
