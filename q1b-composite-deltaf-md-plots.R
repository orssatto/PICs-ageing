
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
d = read.csv("composite-data-04-06-21.csv") %>%
  clean_names() %>%
  mutate(
    muscle = recode_factor(muscle, 'soleus' = 'Soleus', 'ta' = 'Tibialis Anterior'),
    recruit_s = as.numeric(scale(rt, center = T, scale = T)),
    group_age = as.factor(group),
    group_age = recode_factor(group_age, '2' = 'Young Adults', '1' = 'Older Adults'),
    group_age = relevel(factor(group_age), ref = "Young Adults"),
    participant = as.factor(participant) 
  ) %>%
  select(-group, -mu) %>%
  drop_na(deltaf)

# Missing data
vis_miss(d)
vis_dat(d)

# Model: deltaf
fit <- lmer(deltaf ~ group_age*muscle + recruit_s + (1 + recruit_s + muscle| participant), data = d)

# Model diagnostics
#plot(fit)
qqnorm(residuals(fit)); qqline(residuals(fit))
summary(fit)
tidy(fit, conf.int = T)
anova(fit)


# Post hoc test for interaction
emm <- emmeans(fit, pairwise ~ muscle|group_age)
summary(emm)
confint(emm)
conditional_effect <- emmeans(fit, ~ muscle|group_age, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fit), edf = df.residual(fit))


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
             position = position_nudge(x = 0.3), size = 2.5) +
  geom_errorbar(data = mar_ef, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0) +
  theme(
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank()
  ) +
  scale_y_continuous(limits = c(0,8)) + 
  labs(y = "ΔF (pps)", x = "") -> plot_deltaf_composite
plot_deltaf_composite
#ggsave(file = "deltaf.png", units="in", width = 7, height = 4.5, dpi = 300)





# Mean difference plot
# MD
d = read.csv("composite-data-04-06-21.csv") %>%
  clean_names() %>%
  mutate(
    muscle = recode_factor(muscle, 'soleus' = 'Soleus', 'ta' = 'Tibialis Anterior'),
    recruit_s = as.numeric(scale(rt, center = T, scale = T)),
    group_age = as.factor(group),
    group_age = recode_factor(group_age, '2' = 'Young Adults', '1' = 'Older Adults'),
    group_age = relevel(factor(group_age), ref = "Older Adults"),
    participant = as.factor(participant) 
  ) %>%
  select(-group, -mu) %>%
  drop_na(deltaf)

# Missing data
vis_miss(d)
vis_dat(d)

# Model: deltaf
fit <- lmer(deltaf ~ group_age*muscle + recruit_s + (1 + recruit_s + muscle| participant), data = d)



emm <- emmeans(fit, pairwise ~ group_age|muscle)
summary(emm)
ci_95 <- confint(emm, level = .95)
ci_90 <- confint(emm, level = .90)

# MD Soleus
md  <- ci_95$contrasts$estimate[1]
lower_90 <- ci_90$contrasts$lower.CL[1]
upper_90 <- ci_90$contrasts$upper.CL[1]
lower_95 <- ci_95$contrasts$lower.CL[1]
upper_95 <- ci_95$contrasts$upper.CL[1]
muscle <- 'soleus'

df_soleus <- cbind(muscle, md,lower_90,upper_90,lower_95,upper_95)

# MD Tibialis anterior
md  <- ci_95$contrasts$estimate[2]
lower_90 <- ci_90$contrasts$lower.CL[2]
upper_90 <- ci_90$contrasts$upper.CL[2]
lower_95 <- ci_95$contrasts$lower.CL[2]
upper_95 <- ci_95$contrasts$upper.CL[2]
muscle <- 'tibialis_anterior'

df_tibialis_anterior <- cbind(muscle, md,lower_90,upper_90,lower_95,upper_95)

# Join
df <- rbind(df_soleus, df_tibialis_anterior) %>%
  as_tibble() %>%
  mutate(
    md = as.numeric(md),
    lower_90 = as.numeric(lower_90),
    lower_95 = as.numeric(lower_95),
    upper_90 = as.numeric(upper_90),
    upper_95 = as.numeric(upper_95),
    muscle = recode_factor(muscle, 'soleus' = 'Soleus', 'tibialis_anterior' = 'Tibialis Anterior')
  )

ggplot(data = df, aes(x = muscle, y = md)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), size = 0.4, width = 0) +
  geom_errorbar(aes(ymin = lower_90, ymax = upper_90), size = 1.2, width = 0) +
  theme_bw(base_size = 14) +
  labs(x = "", y = "Mean Difference\n(pps)") +
  geom_hline(yintercept=0, linetype="solid",color = "red", size=0.25) +
  ylim(-3,0.25) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(~"ΔF") -> plot_md_deltaf_composite
plot_md_deltaf_composite
#write.csv(df, file = "detlaf-composite-method.csv", row.names = F)


# Panel figure fmax, deltaf, recruitment threshold
plot_grid(plot_deltaf_composite, plot_md_deltaf_composite,
          ncol = 2, 
          nrow = 1,
          labels = c('(A)','(B)'),
          align = 'v',
          axis = "lr",
          label_size = 16,
          rel_widths = c(2, 1.15),
          scale = 0.95)
#ggsave(file = "composite-figure.png", units="in", width = 9.5, height = 3.5, dpi = 300)
ggsave(file = "Figure4.tiff", units="in", width = 9.5, height = 3.5, dpi = 300, compression = "lzw")




# Descriptive analysis of composite method
# Descriptive counts of deltaf by group and muscle
d %>%
  group_by(group_age, muscle) %>% # by group
  summarise(
    count_obs = n()
  )

d %>%
  group_by(group_age, muscle, participant) %>% # by person
  summarise(
    count = n()
  ) %>%
  group_by(group_age, muscle) %>%
  summarise(
    median = median(count, na.rm = T),
    quant_25 = quantile(count, probs = 0.25, na.rm = T),
    quant_75 = quantile(count, probs = 0.75, na.rm = T),
    mean = mean(count, na.rm = T),
    sd = sd(count, na.rm = T),
  )



#### END


