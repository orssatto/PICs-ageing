

# Author: David N Borg
# Date: May 2021

# Libraries
library(janitor)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(merTools)

# Load data
d = read.csv("df-data-30-04-21.csv") %>%
  clean_names() %>%
  mutate(
    muscle = as.factor(group_muscle),
    recruit_s = scale(recruit, center = T, scale = T),
    recruit_s = as.numeric(recruit_s),
    sex = as.factor(sex),
    group_age = relevel(factor(group_age),ref = "Young adults"),
    participant = as.factor(participant) 
  ) %>%
  select(
    -group_age_muscle, -group_muscle, -mu_n, -derecruit
  ) %>%
  drop_na(deltaf)

# Standardise deltaf
dpred = d %>%
  mutate(
    deltaf_s = scale(deltaf, center = T, scale = T),
    deltaf_s = as.numeric(deltaf_s)
  )

# Model: took the same model as in the paper, included deltaf as a univariate effect
# Base models
fit_base1 <- lmer(fmax ~ group_age + muscle + (1+muscle|participant), data = dpred) # no interaction
fit_base2 <- lmer(fmax ~ group_age*muscle + (1+muscle|participant), data = dpred)

# Deltaf added
fit3 <- lmer(fmax ~ group_age + muscle + deltaf_s + (1+muscle|participant), data = dpred) # no interaction
fit4 <- lmer(fmax ~ group_age*muscle + deltaf_s + (1+muscle|participant), data = dpred)
fit5 <- lmer(fmax ~ group_age*muscle + deltaf_s*muscle + group_age*deltaf_s + (1+muscle|participant), data = dpred)
fit6 <- lmer(fmax ~ group_age*muscle*deltaf_s + (1+muscle|participant), data = dpred)

# Compare models
anova(fit_base1,
      fit_base2,
      fit3,
      fit4,
      fit5,
      fit6)

# Summary
summary(fit_base1) # Base model
summary(fit3) # Model with deltaf

# Compare R^2 from two most similiar
  # R^2m = marginal variance explained by the fixed effects
  # Reference for the R^2 statistic: Nakagawa, S., Johnson, P.C.D., Schielzeth, H. (2017) The coefficient of determination R² and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. J. R. Soc. Interface 14: 20170213.
Rsqr_fit_no_deltaf <- r.squaredGLMM(fit_base1)
Rsqr_fit_no_deltaf

Rsqr_fit_deltaf <- r.squaredGLMM(fit3)
Rsqr_fit_deltaf

# Difference in R^2 fixed effects
Rsqr_diff = Rsqr_fit_deltaf[1]-Rsqr_fit_no_deltaf[1]
print({Rsqr_diff}, digits = 3)

# Extract the Root Mean Squared Error (RMSE)
  # RMSE = SD of the residuals (prediction errors)
  # Smaller value = better
RMSE.merMod(fit_base1, scale = F) # FALSE = on origional scale, TRUE = on SD scale
RMSE.merMod(fit3, scale = F)

# Compare models
anova(fit_base1,fit3)



