
# Author: David N Borg
# Date: April 2021

# Libraries
library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(janitor)

# Load data
d = read_csv("data-participants.csv") %>%
  clean_names() %>%
  drop_na(participant) %>%
  mutate(group_age = relevel(factor(group_age), ref = "Young adults"))

# Peak torque - Soleus
fit = lm(pt_soleus_best~group_age, data = d)
plot(fit)
summary(fit)
confint(fit)

# Std. MD
effsize::cohen.d(d$pt_soleus_best~d$group_age, 
                 paired = F, 
                 noncentral = F, 
                 na.rm = T, 
                 conf.level = 0.95)

# Peak torque - TA
fit = lm(pt_ta_best~group_age, data = d)
plot(fit)
summary(fit)
confint(fit)

# Std. MD
effsize::cohen.d(d$pt_ta_best~d$group_age, 
                 paired = F, 
                 noncentral = F, 
                 na.rm = T, 
                 conf.level = 0.95)

# Physical activity levels
fit = lm(physical_activity_level~group_age, data = d)
plot(fit)
summary(fit)
confint(fit)

# Look at the influence of outliers on results
rs <- residuals(fit) 
rs_q <- quantile(rs, probs = c(0.05,0.95))
pot_out <- rs < rs_q[1] | rs > rs_q[2]
d_outlier <- as.data.frame(d)[pot_out,] %>%
  dplyr::select(participant,group_age)
d_outlier
d_no_out <- d %>% anti_join(d_outlier, by = c("participant","group_age")) # keep rows without matching id

fit_refit <- lm(physical_activity_level~group_age, data = d_no_out) # Refit model
qqnorm(residuals(fit_refit)); qqline(residuals(fit_refit))
summary(fit_refit)
confint(fit_refit)
# Note: conclusions remain the same


# Std. MD
effsize::cohen.d(d$physical_activity_level~d$group_age, 
                 paired = F, 
                 noncentral = F, 
                 na.rm = T, 
                 conf.level = 0.95)

effsize::cohen.d(d_no_out$physical_activity_level~d_no_out$group_age, 
                 paired = F, 
                 noncentral = F, 
                 na.rm = T, 
                 conf.level = 0.95)

