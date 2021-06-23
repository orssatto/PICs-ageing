
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
  filter(!participant == "MY4") # Attrition

# Summary
# Age
d %>%
  group_by(group_age) %>%
  summarise(median = median(age, na.rm = T),
            quant_25 = quantile(age, probs = 0.25, na.rm = T),
            quant_75 = quantile(age, probs = 0.75, na.rm = T),
            mean = mean(age, na.rm = T),
            sd = sd(age, na.rm = T))

# Sex
d %>%
  group_by(group_age,sex) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

# Body mass
d %>%
  group_by(group_age) %>%
  summarise(median = median(body_mass, na.rm = T),
            quant_25 = quantile(body_mass, probs = 0.25, na.rm = T),
            quant_75 = quantile(body_mass, probs = 0.75, na.rm = T),
            mean = mean(body_mass, na.rm = T),
            sd = sd(body_mass, na.rm = T))

# Height
d %>%
  group_by(group_age) %>%
  summarise(median = median(height, na.rm = T),
            quant_25 = quantile(height, probs = 0.25, na.rm = T),
            quant_75 = quantile(height, probs = 0.75, na.rm = T),
            mean = mean(height, na.rm = T),
            sd = sd(height, na.rm = T))

# Peak torque - Soleus
d %>%
  group_by(group_age) %>%
  summarise(median = median(pt_soleus_best, na.rm = T),
            quant_25 = quantile(pt_soleus_best, probs = 0.25, na.rm = T),
            quant_75 = quantile(pt_soleus_best, probs = 0.75, na.rm = T),
            mean = mean(pt_soleus_best, na.rm = T),
            sd = sd(pt_soleus_best, na.rm = T))

# Peak torque - TA
d %>%
  group_by(group_age) %>%
  summarise(median = median(pt_ta_best, na.rm = T),
            quant_25 = quantile(pt_ta_best, probs = 0.25, na.rm = T),
            quant_75 = quantile(pt_ta_best, probs = 0.75, na.rm = T),
            mean = mean(pt_ta_best, na.rm = T),
            sd = sd(pt_ta_best, na.rm = T))

# Peak torque - TA
d %>%
  group_by(group_age) %>%
  summarise(median = median(physical_activity_level, na.rm = T),
            quant_25 = quantile(physical_activity_level, probs = 0.25, na.rm = T),
            quant_75 = quantile(physical_activity_level, probs = 0.75, na.rm = T),
            mean = mean(physical_activity_level, na.rm = T),
            sd = sd(physical_activity_level, na.rm = T))




