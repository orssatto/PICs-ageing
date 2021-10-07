

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
library(broom.mixed)

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
  dplyr::select(
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
fit_inter_only <- lmer(fmax ~ deltaf_s + (1|participant), data = dpred) # no interaction
fit_inter_and_slope <- lmer(fmax ~ deltaf_s + (1+deltaf_s|participant), data = dpred)

# Compare models
anova(fit_inter_only,
      fit_inter_and_slope)

# Best fit
fit = fit_inter_and_slope

# Summary
summary(fit)
tidy(fit, conf.int = T)

# Calculate rsquared
  # R^2m = marginal variance explained by the fixed effects
  # Reference for the R^2 statistic: Nakagawa, S., Johnson, P.C.D., Schielzeth, H. (2017) The coefficient of determination R² and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. J. R. Soc. Interface 14: 20170213.
r.squaredGLMM(fit)



#### END


# Extract the Root Mean Squared Error (RMSE)
  # RMSE = SD of the residuals (prediction errors)
  # Smaller value = better
#RMSE.merMod(fit3, scale = F)



