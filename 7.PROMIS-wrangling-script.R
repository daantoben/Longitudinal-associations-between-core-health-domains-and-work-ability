# SET-UP =======================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)

# Load data
results_2b <- read.csv(here("1.data", "1.Dataset_order_202507", "Results", "2b_q_1_results.csv"))



# PROMIS-29 --------------------------------------------------------------------
# All PROMIS wrangling is performed using the PROMIS profile scoring manual of 15-07-2025:
# https://www.healthmeasures.net/images/PROMIS/manuals/Scoring_Manual_Only/PROMIS_Adult_Profile_Scoring_Manual_15July2025.pdf

# First, how many missing data in PROMIS items?
promis_vars <- c(
  "promis_physical_adu_q_01", "promis_physical_adu_q_02", "promis_physical_adu_q_03", "promis_physical_adu_q_04",
  "promis_anxiety_adu_q_05", "promis_anxiety_adu_q_06", "promis_anxiety_adu_q_07", "promis_anxiety_adu_q_08",
  "promis_depression_adu_q_09", "promis_depression_adu_q_10", "promis_depression_adu_q_11", "promis_depression_adu_q_12",
  "promis_fatigue_adu_q_13", "promis_fatigue_adu_q_14", "promis_fatigue_adu_q_15", "promis_fatigue_adu_q_16",
  "promis_sleep_adu_q_17", "promis_sleep_adu_q_18", "promis_sleep_adu_q_19", "promis_sleep_adu_q_20",
  "promis_social_adu_q_21", "promis_social_adu_q_22", "promis_social_adu_q_23", "promis_social_adu_q_24",
  "promis_pain_adu_q_25", "promis_pain_adu_q_26", "promis_pain_adu_q_27", "promis_pain_adu_q_28", "promis_pain_adu_q_29"
)

results_2b %>% summarize(across(all_of(promis_vars), ~ sum(is.na(.)))) # None!

# Next, we turn all PROMIS values into numeric values, debugging as we go
sum(is.na(results_2b$promis_anxiety_adu_q_05))
unique(results_2b$promis_anxiety_adu_q_05)
results_2b$promis_anxiety_adu_q_05 <- as.numeric(results_2bb$promis_anxiety_adu_q_05)
sum(is.na(results_2b$promis_anxiety_adu_q_05))
unique(results_2b$promis_anxiety_adu_q_05) # works!


results_2b <- results_2b %>%
  mutate(across(all_of(promis_vars), as.numeric))

# Now compute total scores according to the scoring manual
# Create raw sum scores for each domain 
# Physical function - REVERSE
# First, reverse coding so that higher = better
physical_vars <- paste0("promis_physical_adu_q_0", 1:4)
results_2b[physical_vars] <- lapply(results_2b[physical_vars],
                                   function(x) ifelse(!is.na(x), 6 - x, NA))

# Now compute sumscores
# Physical function
results_2b <- results_2b %>%
  mutate(promis_physical_rawsum = rowSums(across(c(
    promis_physical_adu_q_01, promis_physical_adu_q_02, promis_physical_adu_q_03, promis_physical_adu_q_04
  )), na.rm = T))
results_2b$promis_physical_rawsum[results_2b$promis_physical_rawsum == 0] <- NA

# Anxiety
results_2b <- results_2b %>%
  mutate(promis_anxiety_rawsum = rowSums(across(c(
    promis_anxiety_adu_q_05, promis_anxiety_adu_q_06, promis_anxiety_adu_q_07, promis_anxiety_adu_q_08
  )), na.rm = T))
results_2b$promis_anxiety_rawsum[results_2b$promis_anxiety_rawsum == 0] <- NA

# Depression
results_2b <- results_2b %>%
  mutate(promis_depression_rawsum = rowSums(across(c(
    promis_depression_adu_q_09, promis_depression_adu_q_10, promis_depression_adu_q_11, promis_depression_adu_q_12
  )), na.rm = T))
results_2b$promis_depression_rawsum[results_2b$promis_depression_rawsum == 0] <- NA

# Fatigue
results_2b <- results_2b %>%
  mutate(promis_fatigue_rawsum = rowSums(across(c(
    promis_fatigue_adu_q_13, promis_fatigue_adu_q_14, promis_fatigue_adu_q_15, promis_fatigue_adu_q_16
  )), na.rm = T))
results_2b$promis_fatigue_rawsum[results_2b$promis_fatigue_rawsum == 0] <- NA

# Sleep - half is REVERSE
sleep_vars <- c("promis_sleep_adu_q_17", "promis_sleep_adu_q_18")
results_2b[sleep_vars] <- lapply(results_2b[sleep_vars],
                                function(x) ifelse(!is.na(x), 6 - x, NA))

results_2b <- results_2b %>%
  mutate(promis_sleep_rawsum = rowSums(across(c(
    promis_sleep_adu_q_17, promis_sleep_adu_q_18, promis_sleep_adu_q_19, promis_sleep_adu_q_20
  )), na.rm = T))
results_2b$promis_sleep_rawsum[results_2b$promis_sleep_rawsum == 0] <- NA

# Social - REVERSE
# First, flip the coding so that higher = better
social_vars <- paste0("promis_social_adu_q_", 21:24)
results_2b[social_vars] <- lapply(results_2b[social_vars],
                                 function(x) ifelse(!is.na(x), 6 - x, NA))

# Now compute sumscores
results_2b <- results_2b %>%
  mutate(promis_social_rawsum = rowSums(across(c(
    promis_social_adu_q_21, promis_social_adu_q_22, promis_social_adu_q_23, promis_social_adu_q_24
  )), na.rm = T))
results_2b$promis_social_rawsum[results_2b$promis_social_rawsum == 0] <- NA

# Pain
results_2b <- results_2b %>%
  mutate(promis_painif_rawsum = rowSums(across(c(
    promis_pain_adu_q_25, promis_pain_adu_q_26, promis_pain_adu_q_27, promis_pain_adu_q_28
  )), na.rm = T))
results_2b$promis_painif_rawsum[results_2b$promis_painif_rawsum == 0] <- NA

# Remove respondents with missing values; we can't deal with them using scoring manuals
# but we only want to delete those with some NA values; those valid PROMIS respondents who
# have missing data. There's loads of respondents with full NA values across PROMIS, we're
# not interested in those.
# So some PROMIS scores, but not all = problematic, get rid of them.
# Physical function
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_physical_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_physical_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2bb$promis_physical_rawsum, na.rm = T) # looks good!

# Anxiety 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_anxiety_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_anxiety_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2bb$promis_anxiety_rawsum, na.rm = T) # looks good!

# Depression 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_depression_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_depression_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2bb$promis_depression_rawsum, na.rm = T) # looks good!

# Fatigue 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_fatigue_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_fatigue_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2bb$promis_fatigue_rawsum, na.rm = T) # looks good!

# Sleep 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_sleep_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_sleep_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2bb <- results_2bb %>% filter(mixed_na == F)
range(results_2bb$promis_sleep_rawsum, na.rm = T) # looks good!

# Social 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_social_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_social_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2b$promis_social_rawsum, na.rm = T) # Looks good!

# Pain interference 
results_2b <- results_2b %>%
  mutate(
    na_count = rowSums(is.na(across(all_of(c("promis_pain_adu_q_25", "promis_pain_adu_q_26",
                                             "promis_pain_adu_q_27", "promis_pain_adu_q_28"))))),
    non_na_count = rowSums(!is.na(across(all_of(c("promis_pain_adu_q_25", "promis_pain_adu_q_26",
                                                  "promis_pain_adu_q_27", "promis_pain_adu_q_28"))))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2b <- results_2b %>% filter(mixed_na == F)
range(results_2b$promis_sleep_rawsum, na.rm = T) # Looks good!


# Turn raw sumscores into tscores using the manual's scoring tables
# We're using PROMIS-29 Profile v1.0 scoring tables, as per Lifelines' advice
# Physical function
results_2b <- results_2b %>%
  mutate(
    promis_pf_tscore = case_when(
      promis_physical_rawsum == 4 ~ 22.9,
      promis_physical_rawsum == 5 ~ 26.9,
      promis_physical_rawsum == 6 ~ 29.1,
      promis_physical_rawsum == 7 ~ 30.7,
      promis_physical_rawsum == 8 ~ 32.1,
      promis_physical_rawsum == 9 ~ 33.3,
      promis_physical_rawsum == 10 ~ 34.4,
      promis_physical_rawsum == 11 ~ 35.6,
      promis_physical_rawsum == 12 ~ 36.7,
      promis_physical_rawsum == 13 ~ 37.9,
      promis_physical_rawsum == 14 ~ 39.1,
      promis_physical_rawsum == 15 ~ 40.4,
      promis_physical_rawsum == 16 ~ 41.8,
      promis_physical_rawsum == 17 ~ 43.4,
      promis_physical_rawsum == 18 ~ 45.3,
      promis_physical_rawsum == 19 ~ 48.0,
      promis_physical_rawsum == 20 ~ 56.9
    )
  )

# Anxiety
results_2b <- results_2b %>%
  mutate(
    promis_anx_tscore = case_when(
      promis_anxiety_rawsum == 4 ~ 40.3,
      promis_anxiety_rawsum == 5 ~ 48.0,
      promis_anxiety_rawsum == 6 ~ 51.2,
      promis_anxiety_rawsum == 7 ~ 53.7,
      promis_anxiety_rawsum == 8 ~ 55.8,
      promis_anxiety_rawsum == 9 ~ 57.7,
      promis_anxiety_rawsum == 10 ~ 59.5,
      promis_anxiety_rawsum == 11 ~ 61.4,
      promis_anxiety_rawsum == 12 ~ 63.4,
      promis_anxiety_rawsum == 13 ~ 65.3,
      promis_anxiety_rawsum == 14 ~ 67.3,
      promis_anxiety_rawsum == 15 ~ 69.3,
      promis_anxiety_rawsum == 16 ~ 71.2,
      promis_anxiety_rawsum == 17 ~ 73.3,
      promis_anxiety_rawsum == 18 ~ 75.4,
      promis_anxiety_rawsum == 19 ~ 77.9,
      promis_anxiety_rawsum == 20 ~ 81.6
    )
  )

# Depression
results_2b <- results_2b %>%
  mutate(
    promis_dp_tscore = case_when(
      promis_depression_rawsum == 4 ~ 41.0,
      promis_depression_rawsum == 5 ~ 49.0,
      promis_depression_rawsum == 6 ~ 51.8,
      promis_depression_rawsum == 7 ~ 53.9,
      promis_depression_rawsum == 8 ~ 55.7,
      promis_depression_rawsum == 9 ~ 57.3,
      promis_depression_rawsum == 10 ~ 58.9,
      promis_depression_rawsum == 11 ~ 60.5,
      promis_depression_rawsum == 12 ~ 62.2,
      promis_depression_rawsum == 13 ~ 63.9,
      promis_depression_rawsum == 14 ~ 65.7,
      promis_depression_rawsum == 15 ~ 67.5,
      promis_depression_rawsum == 16 ~ 69.4,
      promis_depression_rawsum == 17 ~ 71.2,
      promis_depression_rawsum == 18 ~ 73.3,
      promis_depression_rawsum == 19 ~ 75.7,
      promis_depression_rawsum == 20 ~ 79.4
    )
  )

# Fatigue
results_2b <- results_2b %>%
  mutate(
    promis_fa_tscore = case_when(
      promis_fatigue_rawsum == 4 ~ 33.7,
      promis_fatigue_rawsum == 5 ~ 39.7,
      promis_fatigue_rawsum == 6 ~ 43.1,
      promis_fatigue_rawsum == 7 ~ 46.0,
      promis_fatigue_rawsum == 8 ~ 48.6,
      promis_fatigue_rawsum == 9 ~ 51.0,
      promis_fatigue_rawsum == 10 ~ 53.1,
      promis_fatigue_rawsum == 11 ~ 55.1,
      promis_fatigue_rawsum == 12 ~ 57.0,
      promis_fatigue_rawsum == 13 ~ 58.8,
      promis_fatigue_rawsum == 14 ~ 60.7,
      promis_fatigue_rawsum == 15 ~ 62.7,
      promis_fatigue_rawsum == 16 ~ 64.6,
      promis_fatigue_rawsum == 17 ~ 66.7,
      promis_fatigue_rawsum == 18 ~ 69.0,
      promis_fatigue_rawsum == 19 ~ 71.6,
      promis_fatigue_rawsum == 20 ~ 75.8
    )
  )

# Sleep
results_2b <- results_2b %>%
  mutate(
    promis_sl_tscore = case_when(
      promis_sleep_rawsum == 4 ~ 32.0,
      promis_sleep_rawsum == 5 ~ 37.5,
      promis_sleep_rawsum == 6 ~ 41.1,
      promis_sleep_rawsum == 7 ~ 43.8,
      promis_sleep_rawsum == 8 ~ 46.2,
      promis_sleep_rawsum == 9 ~ 48.4,
      promis_sleep_rawsum == 10 ~ 50.5,
      promis_sleep_rawsum == 11 ~ 52.4,
      promis_sleep_rawsum == 12 ~ 54.3,
      promis_sleep_rawsum == 13 ~ 56.1,
      promis_sleep_rawsum == 14 ~ 57.9,
      promis_sleep_rawsum == 15 ~ 59.8,
      promis_sleep_rawsum == 16 ~ 61.7,
      promis_sleep_rawsum == 17 ~ 63.8,
      promis_sleep_rawsum == 18 ~ 66.0,
      promis_sleep_rawsum == 19 ~ 68.8,
      promis_sleep_rawsum == 20 ~ 73.3
    )
  )

# Social
results_2b <- results_2b %>%
  mutate(
    promis_so_tscore = case_when(
      promis_social_rawsum == 4 ~ 29.0,
      promis_social_rawsum == 5 ~ 33.6,
      promis_social_rawsum == 6 ~ 35.7,
      promis_social_rawsum == 7 ~ 37.3,
      promis_social_rawsum == 8 ~ 38.8,
      promis_social_rawsum == 9 ~ 30.3,
      promis_social_rawsum == 10 ~ 41.7,
      promis_social_rawsum == 11 ~ 43.2,
      promis_social_rawsum == 12 ~ 44.8,
      promis_social_rawsum == 13 ~ 46.4,
      promis_social_rawsum == 14 ~ 48.1,
      promis_social_rawsum == 15 ~ 49.8,
      promis_social_rawsum == 16 ~ 51.6,
      promis_social_rawsum == 17 ~ 53.5,
      promis_social_rawsum == 18 ~ 55.6,
      promis_social_rawsum == 19 ~ 58.1,
      promis_social_rawsum == 20 ~ 64.1 
    )
  )

# Pain interference
results_2b <- results_2b %>%
  mutate(
    promis_pif_tscore = case_when(
      promis_painif_rawsum == 4 ~ 41.6,
      promis_painif_rawsum == 5 ~ 49.6,
      promis_painif_rawsum == 6 ~ 52.0,
      promis_painif_rawsum == 7 ~ 53.9,
      promis_painif_rawsum == 8 ~ 55.6,
      promis_painif_rawsum == 9 ~ 57.1,
      promis_painif_rawsum == 10 ~ 58.5,
      promis_painif_rawsum == 11 ~ 59.9,
      promis_painif_rawsum == 12 ~ 61.2,
      promis_painif_rawsum == 13 ~ 62.5,
      promis_painif_rawsum == 14 ~ 63.8,
      promis_painif_rawsum == 15 ~ 65.2,
      promis_painif_rawsum == 16 ~ 66.6,
      promis_painif_rawsum == 17 ~ 68.0,
      promis_painif_rawsum == 18 ~ 69.7,
      promis_painif_rawsum == 19 ~ 71.6,
      promis_painif_rawsum == 20 ~ 75.6 
    )
  )

# Generate Physical and Mental Health summary scores (that's as general as it gets)
# Step 1 was to create the tscores.
# Step 2 is to transform the tscores to zscores
results_2b$promis_pf_zscore <- (results_2b$promis_pf_tscore - 50)/10
results_2b$promis_anx_zscore <- (results_2b$promis_anx_tscore - 50)/10
results_2b$promis_dp_zscore <- (results_2b$promis_dp_tscore - 50)/10
results_2b$promis_fa_zscore <- (results_2b$promis_fa_tscore - 50)/10
results_2b$promis_sl_zscore <- (results_2b$promis_sl_tscore - 50)/10
results_2b$promis_so_zscore <- (results_2b$promis_so_tscore - 50)/10
results_2b$promis_pif_zscore <- (results_2b$promis_pif_tscore - 50)/10

# Step 3 is to create a pain intensity zscore
results_2b$promis_pin_zscore <- (results_2b$promis_pain_adu_q_29 - 2.31)/2.34

# Step 4 is to create a pain composite score
results_2b <- results_2b %>%
  mutate(promis_pain_comp = rowMeans(across(c(promis_pin_zscore, promis_pif_zscore)), na.rm = T))

# Step 5 is to create an emotional distress composite score
results_2b <- results_2b %>%
  mutate(promis_emo_comp = rowMeans(across(c(promis_anx_zscore, promis_dp_zscore)), na.rm = T))

# Step 6 is to multiply the z scores with the factor scoring coefficients from the manual
# for physical health
results_2b <- results_2b %>%
  mutate(
    promis_PH_sum_z = (promis_pf_zscore * 0.872) +
      (promis_pain_comp * -0.094) +
      (promis_so_zscore * 0.113) +
      (promis_fa_zscore * -0.009) +
      (promis_emo_comp * 0.003) +
      (promis_sl_zscore * 0.002)
  )
# for mental health           
results_2b <- results_2b %>%
  mutate(
    promis_MH_sum_z = (promis_fa_zscore * -0.351) +
      (promis_emo_comp * -0.257) +
      (promis_so_zscore * 0.252) +
      (promis_pain_comp * -0.154) +
      (promis_sl_zscore * -0.139) +
      (promis_pf_zscore * -0.015)
  )

# Convert to tscores
results_2b <- results_2b %>%
  mutate(
    promis_PH_tscore = (promis_PH_sum_z * 10) + 50,
    promis_MH_tscore = (promis_MH_sum_z * 10) + 50
  )

