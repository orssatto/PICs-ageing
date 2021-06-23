
# Author: DNB
# Date: April 2021

# Standardised difference between age group, by muscle
conditional_effect <- emmeans(fit, ~ group_age|muscle, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fit), edf = df.residual(fit))




# Comparison to Cohen's d (e.g., deltaf)
sub = d %>%
  group_by(participant, muscle, group_age) %>%
  summarise(deltaf_mean = mean(deltaf))

d_soleus = sub %>% filter(muscle == "Soleus")
effsize::cohen.d(d_soleus$deltaf_mean, d_soleus$group_age, paired = F)

d_ta = sub %>% filter(muscle == "Tibialis Anterior")
effsize::cohen.d(d_ta$deltaf_mean, d_ta$group_age, paired = F)
