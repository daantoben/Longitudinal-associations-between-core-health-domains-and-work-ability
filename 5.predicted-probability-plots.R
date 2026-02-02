# Predicted probability plots are too much for the Quarto document. Render them here
# and then load them into Quarto as pictures

# SET-UP ==========================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(finalfit)
library(viridisLite)
library(forcats)
library(gtsummary)
library(gt)
library(mice)
library(ggmice)
library(naniar)
library(purrr)

# Load data
analysis_df <- read_rds(here("1.data", "4.finalized", "analysis_df.rds"))
load(here("1.data", "4.finalized", "em_model1.RData"))
load(here("1.data", "4.finalized", "OLR_wai_rating_impx.RData"))




# First, create new data for prediction, making sure that we're letting the promis
# subdomains vary, while keeping the confounders constant, thereby controlling for them, 
# as the model does. Also, make stratified datasets for gender, as the effect modifier

# Generate promis data
newdata <- data.frame(
  promis_PH_tscore_m = seq(
    min(analysis_df$promis_PH_tscore_m, na.rm = T), max(analysis_df$promis_PH_tscore_m, na.rm = T),
    length.out = 750),
  promis_MH_tscore_m = seq(
    min(analysis_df$promis_MH_tscore_m, na.rm = T), max(analysis_df$promis_MH_tscore_m, na.rm = T),
    length.out = 750)
)

# Add confounders to new dataset
newdata <- newdata %>% mutate(
  age = rep(median(analysis_df$age, na.rm = T), 750),
  bmi = sample(
    (analysis_df$bodyweight_kg_all_m_1 / ((analysis_df$bodylength_cm_all_m_1/100)^2)), size = 750),
  comorbidity = rep("0", 750),
  degree_5 = rep("high", 750),
  partner_presence_adu_q_1 = rep("ja, ik ben getrouwd", 750),
  work_sector = rep("Professionals", 750),
  income_net_adu_q_1_v3 = rep("2000 - 2500 euro", 750),
  packyears_cumulative_adu_c_2 = rep(median(analysis_df$packyears_cumulative_adu_c_2, na.rm = T), 750),
  squash_perweek_adu_q_15_a = rep("5", 750),
  rand_generalhealth_adu_q_02 = rep("ongeveer hetzelfde als een jaar geleden", 750)
)

# Duplicate for gender
newdata <- bind_rows(
  newdata %>% mutate(gender = factor("FEMALE", levels(analysis_df$gender))),
  newdata %>% mutate(gender = factor("MALE", levels(analysis_df$gender)))
)

# First, average the probabilities across all imputations
preds <- lapply(model9$analyses, function(model) {
  probs <- predict(model, newdata = newdata, type = "probs")
  as_tibble(probs)
})

avg_probs <- reduce(preds, `+`) / length(preds)

pred_df <- avg_probs %>%
  bind_cols(newdata) %>%
  pivot_longer(cols = everything()[1:11], names_to = "Outcome", values_to = "Probability")

pred_df <- pred_df %>% 
  mutate(promis_PH_tscore = promis_PH_tscore_m + mean(analysis_df$promis_PH_tscore, na.rm = T),
         promis_MH_tscore = promis_MH_tscore_m + mean(analysis_df$promis_MH_tscore, na.rm = T))


# PH plot
pred_df %>% 
  ggplot(aes(x = promis_PH_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  facet_wrap(~ gender) +
  scale_colour_viridis_d() +
  labs(
    x = "PROMIS Physical Health",
    y = "Predicted probability",
    colour = "WAS",
    title = "Physical health",
    subtitle = "Predicted probability distributions by gender"
  ) +
  theme_bw()

# MH plot
pred_df %>% 
  ggplot(aes(x = promis_MH_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "magma") +
  labs(
    x = "PROMIS Physical Health",
    y = "Predicted probability",
    colour = "WAS",
    title = "Mental health",
    subtitle = "Predicted probability distributions by gender"
  ) +
  theme_bw()



# 2
# First, create new data for prediction, making sure that we're letting the promis
# subdomains vary, while keeping the confounders constant, thereby controlling for them, 
# as the model does. Also, make stratified datasets for gender, as the effect modifier

# Generate promis data
newdata <- data.frame(
  promis_pf_tscore_m = seq(
    min(analysis_df$promis_pf_tscore_m, na.rm = T), max(analysis_df$promis_pf_tscore_m, na.rm = T),
    length.out = 750),
  promis_anx_tscore_m = seq(
    min(analysis_df$promis_anx_tscore_m, na.rm = T), max(analysis_df$promis_anx_tscore_m, na.rm = T),
    length.out = 750),
  promis_dp_tscore_m = seq(
    min(analysis_df$promis_dp_tscore_m, na.rm = T), max(analysis_df$promis_dp_tscore_m, na.rm = T),
    length.out = 750),
  promis_fa_tscore_m = seq(
    min(analysis_df$promis_fa_tscore_m, na.rm = T), max(analysis_df$promis_fa_tscore_m, na.rm = T),
    length.out = 750),
  promis_sl_tscore_m = seq(
    min(analysis_df$promis_sl_tscore_m, na.rm = T), max(analysis_df$promis_sl_tscore_m, na.rm = T),
    length.out = 750),
  promis_so_tscore_m = seq(
    min(analysis_df$promis_so_tscore_m, na.rm = T), max(analysis_df$promis_so_tscore_m, na.rm = T),
    length.out = 750),
  promis_pif_tscore_m = seq(
    min(analysis_df$promis_pif_tscore_m, na.rm = T), max(analysis_df$promis_pif_tscore_m, na.rm = T),
    length.out = 750),
  promis_pain_adu_q_29_m = seq(
    min(analysis_df$promis_pain_adu_q_29_m, na.rm = T), 
    max(analysis_df$promis_pain_adu_q_29_m, na.rm = T), 
    length.out = 750)
)

# Add confounders to new dataset
newdata <- newdata %>% mutate(
  age = rep(median(analysis_df$age, na.rm = T), 750),
  bmi = sample(
    (analysis_df$bodyweight_kg_all_m_1 / ((analysis_df$bodylength_cm_all_m_1/100)^2)), size = 750),
  comorbidity = rep("0", 750),
  degree_5 = rep("high", 750),
  partner_presence_adu_q_1 = rep("ja, ik ben getrouwd", 750),
  work_sector = rep("Professionals", 750),
  income_net_adu_q_1_v3 = rep("2000 - 2500 euro", 750),
  packyears_cumulative_adu_c_2 = rep(median(analysis_df$packyears_cumulative_adu_c_2, na.rm = T), 750),
  squash_perweek_adu_q_15_a = rep("5", 750),
  rand_generalhealth_adu_q_02 = rep("ongeveer hetzelfde als een jaar geleden", 750)
)

# Duplicate for gender
newdata <- bind_rows(
  newdata %>% mutate(gender = factor("FEMALE", levels(analysis_df$gender))),
  newdata %>% mutate(gender = factor("MALE", levels(analysis_df$gender)))
)

# First, average the probabilities across all imputations
preds <- lapply(em_model1$analyses, function(model) {
  probs <- predict(model, newdata = newdata, type = "probs")
  as_tibble(probs)
})

avg_probs <- reduce(preds, `+`) / length(preds)

pred_df <- avg_probs %>%
  bind_cols(newdata) %>%
  pivot_longer(cols = everything()[1:11], names_to = "Outcome", values_to = "Probability")

pred_df <- pred_df %>% 
  mutate(
    promis_pf_tscore = promis_pf_tscore_m + mean(analysis_df$promis_pf_tscore, na.rm = T),
    promis_anx_tscore = promis_anx_tscore_m + mean(analysis_df$promis_anx_tscore, na.rm = T),
    promis_dp_tscore = promis_dp_tscore_m + mean(analysis_df$promis_dp_tscore, na.rm = T),
    promis_fa_tscore = promis_fa_tscore_m + mean(analysis_df$promis_fa_tscore, na.rm = T),
    promis_sl_tscore = promis_sl_tscore_m + mean(analysis_df$promis_sl_tscore, na.rm = T),
    promis_so_tscore = promis_so_tscore_m + mean(analysis_df$promis_so_tscore, na.rm = T),
    promis_pif_tscore = promis_pif_tscore_m + mean(analysis_df$promis_pif_tscore, na.rm = T),
    promis_pain_adu_q_29 = promis_pain_adu_q_29_m + mean(analysis_df$promis_pain_adu_q_29, na.rm = T))

# PF
pred_summary <- pred_df %>%
  group_by(promis_pf_tscore, gender, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_pf_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  facet_wrap(~ gender) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Physical Function",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions by gender"
  ) +
  theme_bw()

# ANX
pred_summary <- pred_df %>%
  group_by(promis_anx_tscore, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_anx_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Anxiety",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()


# DP
pred_summary <- pred_df %>%
  group_by(promis_dp_tscore, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_dp_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Depression",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()


# FA
pred_summary <- pred_df %>%
  group_by(promis_fa_tscore, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_fa_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Fatigue",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()


# SL
pred_summary <- pred_df %>%
  group_by(promis_sl_tscore, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_sl_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Sleep",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()


# SO
pred_summary <- pred_df %>%
  group_by(promis_so_tscore, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_so_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Social",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()


# PIF
pred_summary <- pred_df %>%
  group_by(promis_pif_tscore, gender, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_pif_tscore, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  facet_wrap(~ gender) +
  labs(
    x = "PROMIS Pain Interference",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions by gender"
  ) +
  theme_bw()



# PI
pred_summary <- pred_df %>%
  group_by(promis_pain_adu_q_29, Outcome) %>%
  summarize(Probability = mean(Probability), .groups = "drop")

pred_summary %>% 
  ggplot(aes(x = promis_pain_adu_q_29, y = Probability, colour = factor(
    Outcome, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
  geom_smooth(se = FALSE, method = "gam", span = 0.2) +
  scale_colour_viridis_d(option = "plasma") +
  labs(
    x = "PROMIS Pain intensity",
    y = "Predicted probability",
    colour = "WAS",
    subtitle = "Predicted probability distributions"
  ) +
  theme_bw()






