
# Author: David N Borg
# Date: April 2021

# Libraries
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(emmeans)
library(lmerTest)
library(janitor)
library(cowplot)


# Load data
d = read.csv("df-data-30-04-21.csv") %>%
  clean_names() %>%
  mutate(
    muscle = as.factor(group_muscle),
    recruit_s = scale(recruit, center = T, scale = T),
    recruit_s = as.numeric(recruit_s),
    sex = as.factor(sex),
    group_age = relevel(factor(group_age),ref = "Older adults"),
    participant = as.factor(participant) 
  ) %>%
  select(
    -group_age_muscle, -group_muscle, -mu_n, -derecruit
  ) %>%
  drop_na(deltaf)


# Deltaf: Mean difference
# Model
fit <- lmer(deltaf ~ group_age*muscle + muscle*recruit_s + (1 + recruit_s*muscle | participant), data = d)

# Mean difference
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
  theme_bw(base_size = 12) +
  labs(x = "Muscle", y = "Mean Difference\n(pps)") +
  geom_hline(yintercept=0, linetype="solid",color = "red", size=0.25) +
  ylim(-2.5,0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(~"ΔF") -> plot_md_deltaf
plot_md_deltaf
write.csv(df, file = "detlaf.csv", row.names = F)


# Fmax: Mean difference
# Model
fit <- lmer(fmax ~ group_age*muscle + (1+muscle|participant), data = d)

# Mean difference
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
  theme_bw(base_size = 12) +
  labs(x = "Muscle", y = "Mean Difference\n(pps)") +
  geom_hline(yintercept=0, linetype="solid",color = "red", size=0.25) +
  ylim(-4,1) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(~"Peak Discharge Rate") -> plot_md_fmax
write.csv(df, file = "fmax.csv", row.names = F)





# Recruitment threshold: Mean difference
# Model
fit <- lmer(recruit ~ group_age*muscle + (1 + muscle|participant), data = d)

# Mean difference
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
  theme_bw(base_size = 12) +
  labs(x = "Muscle", y = "Mean Difference\n(% maximum torque)") +
  geom_hline(yintercept=0, linetype="solid",color = "red", size=0.25) +
  ylim(-4,1) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  facet_grid(~"Recruitment Threshold") -> plot_md_rt
write.csv(df, file = "recrutiment-threshold.csv", row.names = F)





# Panel md plots
plot_grid(plot_md_deltaf, plot_md_fmax, plot_md_rt,
          ncol = 3, nrow = 1,
          labels = c('(A)','(B)','(C)'),
          align = 'v',
          axis = "lr",
          label_size = 12,
          scale = 0.95)
ggsave(file = "Figure3.png", units="in", width = 9.5, height = 3, dpi = 300)
ggsave(file = "Figure3.tiff", units="in", width = 9.5, height = 3, dpi = 300, compression = "lzw")

