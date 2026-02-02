# SET-UP =======================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(stringr)
library(zoo)

# Load data
results_2 <- read_rds(here("1.data", "2.processed", "results_2.rds"))
results_3 <- read_rds(here("1.data", "2.processed", "results_3.rds"))
covid <- read_rds(here("1.data", "2.processed", "covid.rds"))



# PRE-PROCESSING ===============================================================
# First, filter out measurements not including promis or wai
results_3a <- results_3 %>% filter(measurement == "3a")
results_2b <- results_2 %>% filter(measurement == "2b")
results_2a <- results_2 %>% filter(measurement == "2a")

# How many shared individuals?
sum(results_2b$project_pseudo_id %in% results_3a$project_pseudo_id)
sum(covid$project_pseudo_id %in% results_2b$project_pseudo_id)
sum(covid$project_pseudo_id %in% results_3a$project_pseudo_id)

# Select working population
# Hmmm.... why do 42724 out of 62478 respondents say they dont work?
table(results_2b$employment_current_adu_q_1)
table(results_2b$unemployment_current_adu_q_1)
table(results_2b$unemployment_current_adu_q_1_a)
summary(as.numeric(results_2b$workweek_hours_adu_q_1))

# And why are 98.5% of them NA on the subsequent " how many hours per week do you work"  item?
a <- results_2b %>% filter(employment_current_adu_q_1 == "ja") %>% pull(project_pseudo_id)
b <- results_2b %>% filter(is.na(as.numeric(workweek_hours_adu_q_1))) %>% pull(project_pseudo_id)
sum(a %in% b)/19446

# Seems like the item coding on the employment_current_adu_q_1 is inverted?
results_2b <- results_2b %>%
  mutate(employment_current_adu_q_1 = case_when(
    employment_current_adu_q_1 == "ja" ~ "nee",
    employment_current_adu_q_1 == "nee" ~ "ja"
  ))

# Transform educational attainment to a 5 class structure
results_2b <- results_2b %>%
  mutate(
    degree_5 = case_when(
      degree_highest_adu_q_1 == "geen opleiding (lager onderwijs niet afgemaakt)" ~ "none",
      degree_highest_adu_q_1 == "lager onderwijs (basisonderwijs, speciaal basisonderwijs)" ~ "low",
      degree_highest_adu_q_1 == "lager of voorbereidend beroepsonderwijs (zoals lts, leao, lhno, vmbo)" ~ "low",
      degree_highest_adu_q_1 == "middelbaar algemeen voortgezet onderwijs (zoals mavo, (m)ulo, mb)-kort, vmbo-t)" ~ "low",
      degree_highest_adu_q_1 == "hoger algemeen en voorbereidend wetenschappelijk onderwijs (zoals havo, vwo, atheneum, gymnasium, hbs, mms)" ~ "medium",
      degree_highest_adu_q_1 == "middelbaar beroepsonderwijs of beroepsbegeleidend onderwijs (zoals mbo-lang, mts, meao, bol, bbl, inas)" ~ "medium",
      degree_highest_adu_q_1 == "hoger beroepsonderwijs (zoals hbo, hts, heao, kandidaats wetenschappelijk onderwijs)" ~ "high",
      degree_highest_adu_q_1 == "wetenschappelijk onderwijs (universiteit)" ~ "high",
      degree_highest_adu_q_1 == "anders, nl." ~ "other"
    ),
    degree_5 = factor(degree_5)
  )

results_2b <- results_2b %>%
  mutate(
    degree_4 = case_when(
      degree_5 == "low" ~ "low",
      degree_5 == "medium" ~ "medium",
      degree_5 == "high" ~ "high",
      degree_5 == "other" ~ "other",
      degree_5 == "none" ~ "other"
    ),
    degree_4 = factor(degree_4)
  )

# Transform packyears to a numeric variable
results_2b$packyears_cumulative_adu_c_2 <- as.numeric(results_2b$packyears_cumulative_adu_c_2)

# Set packyears NA values for never smokers to 0, instead of NA so that these rows don't get
# deleted from the model
results_2b$packyears_cumulative_adu_c_2[results_2b$smoking_habit_adu_c_2 == "nooit gerookt"] <- 0

sum(is.na(results_2b$packyears_cumulative_adu_c_2)) # much better

# Now filter based on employment
employed_df_w2 <- results_2b %>% filter(employment_current_adu_q_1 == "ja")
employed_df_w3 <- results_3a %>% filter(project_pseudo_id %in% employed_df_w2$project_pseudo_id)

# Rename date columns to be able to add them to the merged analysis_df
# for computing average time between measurement 2b, and 3a
employed_df_w2$date2b <- as.yearmon(employed_df_w2$date)
employed_df_w3$date3a <- as.yearmon(employed_df_w3$date)
results_2a$date2a <- as.yearmon(results_2a$date)

# Merge dataframes into a single dataframe for analysis, taking w3 as the basis as that's the one that
# holds the outcome: WAI
analysis_df <- left_join(
  employed_df_w3 %>% dplyr::select(
    project_pseudo_id, date3a, wai_demands_adu_q_02_a, wai_demands_adu_q_02_b, wai_future_adu_q_03, wai_rating_adu_q_01), 
  employed_df_w2, 
  by = "project_pseudo_id")

# Add employment_type from the covid dataframe
analysis_df <- left_join(
  analysis_df, covid %>% dplyr::select(project_pseudo_id, employment_type),
  by = "project_pseudo_id")

# And include BMI variables that belong to measurement 2a. Little finnicky...
analysis_df <- left_join(
  analysis_df %>% dplyr::select(
    -bodylength_cm_all_m_1, -bodyweight_kg_all_m_1, -bodyweight_kg_all_m_2), 
  results_2a %>% dplyr::select(
    project_pseudo_id, date2a, bodylength_cm_all_m_1, bodyweight_kg_all_m_1, bodyweight_kg_all_m_2
  ), by = "project_pseudo_id"
)

# Factorize WAI rating
analysis_df$wai_rating_adu_q_01 <- factor(analysis_df$wai_rating_adu_q_01)

# Compute comorbidity dichotomy
analysis_df <- analysis_df %>%
  mutate(
    comorbidity = case_when(
      promas_chronicdisease_adu_q_01_a == "1" ~ 0, # No comorbidity
      promas_chronicdisease_adu_q_01_b == "1" ~ 1, # Cardiovascular disease
      promas_chronicdisease_adu_q_01_c == "1" ~ 1, # Diabetes
      promas_chronicdisease_adu_q_01_d == "1" ~ 1, # COPD
      promas_chronicdisease_adu_q_01_e == "1" ~ 1, # High blood pressure
      promas_chronicdisease_adu_q_01_f == "1" ~ 1  # Other
    ),
    comorbidity = factor(comorbidity)
  )

# Center PROMIS values
analysis_df$promis_PH_tscore_m <- analysis_df$promis_PH_tscore - mean(analysis_df$promis_PH_tscore, na.rm = T)
analysis_df$promis_MH_tscore_m <- analysis_df$promis_MH_tscore - mean(analysis_df$promis_MH_tscore, na.rm = T)

analysis_df$promis_pf_tscore_m <- analysis_df$promis_pf_tscore - mean(analysis_df$promis_pf_tscore, na.rm = T)
analysis_df$promis_anx_tscore_m <- analysis_df$promis_anx_tscore - mean(analysis_df$promis_anx_tscore, na.rm = T)
analysis_df$promis_dp_tscore_m <- analysis_df$promis_dp_tscore - mean(analysis_df$promis_dp_tscore, na.rm = T)
analysis_df$promis_fa_tscore_m <- analysis_df$promis_fa_tscore - mean(analysis_df$promis_fa_tscore, na.rm = T)
analysis_df$promis_sl_tscore_m <- analysis_df$promis_sl_tscore - mean(analysis_df$promis_sl_tscore, na.rm = T)
analysis_df$promis_so_tscore_m <- analysis_df$promis_so_tscore - mean(analysis_df$promis_so_tscore, na.rm = T)
analysis_df$promis_pif_tscore_m <- analysis_df$promis_pif_tscore - mean(analysis_df$promis_pif_tscore, na.rm = T)
analysis_df$promis_pain_adu_q_29_m <- analysis_df$promis_pain_adu_q_29 - mean(analysis_df$promis_pain_adu_q_29, na.rm = T)

# Compute work sectors from ISO codes using: https://www.ilostats.ilo.org/methods/concepts-and-definitions/classification-occupation/
analysis_df <- analysis_df %>%
  mutate(
    work_sector = case_when(
      str_starts(jobcode2_currentjob_adu_c_2, "0") ~ "Armed forces",
      str_starts(jobcode2_currentjob_adu_c_2, "1") ~ "Managers",
      str_starts(jobcode2_currentjob_adu_c_2, "2") ~ "Professionals",
      str_starts(jobcode2_currentjob_adu_c_2, "3") ~ "Technicians and Associate Professionals",
      str_starts(jobcode2_currentjob_adu_c_2, "4") ~ "Clerical Support Workers",
      str_starts(jobcode2_currentjob_adu_c_2, "5") ~ "Service and Sales Workers",
      str_starts(jobcode2_currentjob_adu_c_2, "6") ~ "Skilled Agricultural, Forestry and Fishery Workers",
      str_starts(jobcode2_currentjob_adu_c_2, "7") ~ "Craft and Related Trades Workers",
      str_starts(jobcode2_currentjob_adu_c_2, "8") ~ "Plant and Machine Operators, and Assemblers",
      str_starts(jobcode2_currentjob_adu_c_2, "9") ~ "Elementary Occupations",
      str_starts(jobcode2_currentjob_adu_c_2, "$") ~ NA
    )
  )

# Compute time between measurements, in months (dates were given in YYYY-MM format, no days available)
analysis_df$difftime_3a2a <- 12 * (as.numeric(analysis_df$date3a) - as.numeric(analysis_df$date2a))
analysis_df$difftime_2b2a <- 12 * (as.numeric(analysis_df$date2b) - as.numeric(analysis_df$date2a))
analysis_df$difftime_3a2b <- 12 * (as.numeric(analysis_df$date3a) - as.numeric(analysis_df$date2b))

mean(analysis_df$difftime_3a2b, na.rm = TRUE)

# SAVE ANALYSIS DATAFRAME ======================================================
write_rds(analysis_df, file = here("1.data", "4.finalized", "analysis_df.rds"))



