# SET-UP =======================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)


# Findings
results_2a <- read.csv(here("1.data", "1.Dataset_order_202507", "Results", "2a_q_1_results.csv"))
results_2aII <- read.csv(here("1.data", "1.Dataset_order_202510", "Results", "2a_v_1_results.csv"))
results_2b <- read.csv(here("1.data", "1.Dataset_order_202507", "Results", "2b_q_1_results.csv"))
results_2bII <- read.csv(here("1.data", "1.Dataset_order_202510", "Results", "2b_q_1_results.csv"))
results_3a <- read.csv(here("1.data", "1.Dataset_order_202507", "Results", "3a_q_1_results.csv"))
results_3b <- read.csv(here("1.data", "1.Dataset_order_202507", "Results", "3b_q_1_results.csv"))

covid <- read.csv(here("1.data", "1.Dataset_order_20251020", "Results", "covq_q_t02_results.csv"))

# Add BMI variables to their datasets
results_2a <- left_join(results_2a, results_2aII %>% dplyr::select(
  project_pseudo_id, bodylength_cm_all_m_1, bodyweight_kg_all_m_1, bodyweight_kg_all_m_2),
  by = "project_pseudo_id")

results_2b <- left_join(results_2b, results_2bII %>% dplyr::select(
  project_pseudo_id, bodyweight_current_adu_q_1),
  by = "project_pseudo_id")

rm(results_2aII, results_2bII)

# Merge datasets into long format
# Create a measurement variable denoting the measurement wave
results_2a$measurement <- "2a"
results_2b$measurement <- "2b"
results_3a$measurement <- "3a"
results_3b$measurement <- "3b"
covid$measurement <- "covid"

# Bind the dataframes of each wave into single, long dataframes
results_2 <- bind_rows(results_2a, results_2b)
results_3 <- bind_rows(results_3a, results_3b)

# Order them by id and then by the submeasurement
results_2 <- results_2 %>% arrange(project_pseudo_id, measurement)
results_3 <- results_3 %>% arrange(project_pseudo_id, measurement)

# Remove obsolete data
rm(results_2a, results_2b, results_3a, results_3b)

# Reorder the columns so that measurement sits next to id
results_2 <- results_2 %>% dplyr::select(project_pseudo_id, measurement, everything())
results_3 <- results_3 %>% dplyr::select(project_pseudo_id, measurement, everything())
covid <- covid %>% dplyr::select(project_pseudo_id, measurement, everything())


# FEATURE ENGINEERING ==========================================================
# The assumption here - still have to verify it - is that the ubiquitous $7 or $6 values 
# across each dataframe denote different flavours of missing values

# (LIKERT) LEVELS & LABELS
likert_levels_4 <- c("1", "2", "3", "4")
likert_levels_5 <- c("1", "2", "3", "4", "5")
likert_levels_6 <- c("1", "2", "3", "4", "5", "6")

likert_labels_quality <- c("zeer goed", "goed", "matig", "slecht", "zeer slecht")
likert_labels_frequency <- c("altijd", "vaak", "soms", "zelden", "(bijna) nooit")
likert_labels_frequency2 <- c("altijd", "vaak", "soms", "zelden", "(bijna) nooit", "n.v.t.")
likert_labels_satisfaction <- c("zeer tevreden", "tevreden", "ontevreden", "zeer ontevreden")
likert_labels_quantity <- c(
  "in zeer hoge mate", "in hoge mate", "enigszins", "in geringe mate", "in zeer geringe mate"
)
likert_labels_smoking <- c(
  "ja, gemiddeld aantal per dag", "ja, maar minder dan 1 per dag", "nee", "ja, maar minder dan 1 sigaret en/of shagje per dag"
)
likert_labels_intensity <- c("helemaal niet", "een klein beetje", "nogal", "veel", "heel erg veel")


dich_levels1 <- c("1", "2")
dich_labels1 <- c("ja", "nee")

dich_levels2 <- c("0", "1")
dich_labels2 <- c("nee", "ja")



# 2 ----------------------------------------------------------------------------
# Measurement
results_2$measurement <- factor(
  results_2$measurement
)

# Gender
results_2$gender <- factor(
  results_2$gender
)

# BMI
results_2$bodyweight_current_adu_q_1 <- as.numeric(results_2$bodyweight_current_adu_q_1)
results_2$bodyweight_kg_all_m_1 <- as.numeric(results_2$bodyweight_kg_all_m_1)
results_2$bodyweight_kg_all_m_2 <- as.numeric(results_2$bodyweight_kg_all_m_2)

# Arthritis follow-up
results_2$arthritis_followup_adu_q_1 <- factor(
  results_2$arthritis_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# Canccer follow-up
results_2$cancer_followup_adu_q_1 <- factor(
  results_2$cancer_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# copd follow-up
results_2$copd_followup_adu_q_1 <- factor(
  results_2$copd_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# heart attack follow-up
results_2$heartattack_followup_adu_q_1 <- factor(
  results_2$heartattack_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# osteoarthritis follow-up
results_2$osteoarthritis_followup_adu_q_1 <- factor(
  results_2$osteoarthritis_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# stroke follow-up
results_2$stroke_followup_adu_q_1 <- factor(
  results_2$stroke_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# In house children
results_2$inhouse_children_adu_q_1 <- factor(
  results_2$inhouse_children_adu_q_1, levels = likert_levels_4, 
  labels = c("ja", "nee", "heb ik niet", "nee/nvt")
)

# Squash 
results_2$squash_workactivity_adu_q_04_e <- factor(
  results_2$squash_workactivity_adu_q_04_e, 
  levels = "1", 
  labels = "niet van toepassing want ik heb geen werk en ga niet naar school"
)

# Smoking behaviour
results_2$cigarettes_frequency_adu_q_1_a <- factor(
  results_2$cigarettes_frequency_adu_q_1_a, 
  levels = "1", 
  labels = "ik rook wel maar minder dan 1 per dag"
)

results_2$cigarettes_smoking_adu_q_1 <- factor(
  results_2$cigarettes_smoking_adu_q_1, levels = likert_levels_4, labels = likert_labels_smoking
)

results_2$cigarillos_frequency_adu_q_1_a <- factor(
  results_2$cigarillos_frequency_adu_q_1_a, 
  levels = "1", 
  labels = "ik rook wel maar minder dan 1 per dag"
)

results_2$cigarillos_smoking_adu_q_1 <- factor(
  results_2$cigarillos_smoking_adu_q_1, levels = likert_levels_4, labels = likert_labels_smoking
)

results_2$cigars_frequency_adu_q_1_a <- factor(
  results_2$cigars_frequency_adu_q_1_a, 
  levels = "1", 
  labels = "ik rook wel maar minder dan 1 per dag"
)

results_2$cigars_smoking_adu_q_1 <- factor(
  results_2$cigars_smoking_adu_q_1, levels = likert_levels_4, labels = likert_labels_smoking
)

results_2$pipetobacco_frequency_adu_q_1_a <- factor(
  results_2$pipetobacco_frequency_adu_q_1_a, 
  levels = "1", 
  labels = "ik rook wel maar minder dan 1 per dag"
)

results_2$pipetobacco_smoking_adu_q_1 <- factor(
  results_2$pipetobacco_smoking_adu_q_1, levels = likert_levels_4, labels = likert_labels_smoking
)

results_2$ecigarettes_frequency_adu_q_1_a <- factor(
  results_2$ecigarettes_frequency_adu_q_1_a, 
  levels = "1", 
  labels = "ik rook wel maar minder dan 1 per dag"
)

results_2$ecigarettes_smoking_adu_q_1 <- factor(
  results_2$ecigarettes_smoking_adu_q_1, levels = likert_levels_4, labels = likert_labels_smoking
)

results_2$current_smoker_adu_c_2 <- factor(
  results_2$current_smoker_adu_c_2, levels = dich_levels2, labels = dich_labels2
)

results_2$ever_smoker_adu_c_2 <- factor(
  results_2$ever_smoker_adu_c_2, levels = dich_levels2, labels = dich_labels2
)

results_2$ex_smoker_adu_c_2 <- factor(
  results_2$ex_smoker_adu_c_2, levels = dich_levels2, labels = dich_labels2
)

results_2$never_smoker_adu_c_1 <- factor(
  results_2$never_smoker_adu_c_1, levels = dich_levels2, labels = dich_labels2
)

results_2$recent_starter_adu_c_2 <- factor(
  results_2$ex_smoker_adu_c_2, levels = dich_levels2, labels = dich_labels2
)

results_2$smoking_habit_adu_c_2 <- factor(
  results_2$smoking_habit_adu_c_2, 
  levels = c("0", "1", "2", "3"), 
  labels = c("nooit gerookt", "ex-roker", "huidige roker", "recente starter")
)

# Education
results_2$degree_highest_adu_q_1 <- factor(
  results_2$degree_highest_adu_q_1, 
  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
  labels = c("geen opleiding (lager onderwijs niet afgemaakt)",
             "lager onderwijs (basisonderwijs, speciaal basisonderwijs)",
             "lager of voorbereidend beroepsonderwijs (zoals lts, leao, lhno, vmbo)",
             "middelbaar algemeen voortgezet onderwijs (zoals mavo, (m)ulo, mb)-kort, vmbo-t)",
             "hoger algemeen en voorbereidend wetenschappelijk onderwijs (zoals havo, vwo, atheneum, gymnasium, hbs, mms)",
             "middelbaar beroepsonderwijs of beroepsbegeleidend onderwijs (zoals mbo-lang, mts, meao, bol, bbl, inas)",
             "hoger beroepsonderwijs (zoals hbo, hts, heao, kandidaats wetenschappelijk onderwijs)",
             "wetenschappelijk onderwijs (universiteit)",
             "anders, nl."
             )
)


# (Un)Employment
results_2$employment_current_adu_q_1 <- factor(
  results_2$employment_current_adu_q_1, levels = dich_levels1, labels = dich_labels2
)

# We have to collapse the employment situation variables: each variable is just a level
# So instead, we make them 1 variable with g through n levels denoting the employment situation
results_2 <- results_2 %>%
  mutate(
    employment_situation_adu_q_1 = case_when(
      employment_situation_adu_q_1_g == 1 ~ "ik ben arbeidsongeschikt",
      employment_situation_adu_q_1_h == 1 ~ "ik heb een bijstandsuitkering",
      employment_situation_adu_q_1_i == 1 ~ "ik ben fulltime huisvrouw",
      employment_situation_adu_q_1_j == 1 ~ "ik volg onderwijs/ik studeer",
      employment_situation_adu_q_1_k == 1 ~ "ik ben met pensioen",
      employment_situation_adu_q_1_l == 1 ~ "ik ben met vervroegd pensioen",
      employment_situation_adu_q_1_m == 1 ~ "geen van onderstaande",
      employment_situation_adu_q_1_n == 1 ~ "ik ben werkloos/werkzoekend (geregistreerd bij het uitzendbureau"
    ),
    employment_situation_adu_q_1 = factor(employment_situation_adu_q_1)
  )

results_2$unemployment_current_adu_q_1 <- factor(
  results_2$unemployment_current_adu_q_1, 
  levels = likert_levels_4, 
  labels = c("minder dan 6 maanden", "6 maanden of meer, maar minder dan 1 jaar",
             "1 jaar of meer, maar minder dan 3 jaar", "meer dan 3 jaar")
)

results_2$unemployment_current_adu_q_1_a <- factor(
  results_2$unemployment_current_adu_q_1_a, 
  levels = "1",
  labels = "ik ben niet werkloos/werkzoekend"
)

results_2$incapacity_current_adu_q_1_a <- factor(
  results_2$incapacity_current_adu_q_1_a, 
  levels = "1",
  labels = "ik ben niet arbeidsongeschikt"
)

# Chronic diseases
results_2 <- results_2 %>%
  mutate(
    promas_chronicdisease_adu_q_01 = case_when(
      promas_chronicdisease_adu_q_01_a == 1 ~ "geen chronische aandoening",
      promas_chronicdisease_adu_q_01_b == 1 ~ "hart- en/of vaatziekte",
      promas_chronicdisease_adu_q_01_c == 1 ~ "diabetes",
      promas_chronicdisease_adu_q_01_d == 1 ~ "copd",
      promas_chronicdisease_adu_q_01_e == 1 ~ "hoge bloeddruk",
      promas_chronicdisease_adu_q_01_f == 1 ~ "andere chronische aandoening",
    ),
    promas_chronicdisease_adu_q_01 = factor(promas_chronicdisease_adu_q_01)
  )

results_2$promas_chronicdisease_adu_q_02 <- factor(
  results_2$promas_chronicdisease_adu_q_02, levels = dich_levels1, labels = dich_labels1
)

# Partner
results_2$partner_presence_adu_q_1 <- factor(
  results_2$partner_presence_adu_q_1, levels = likert_levels_5,
  labels = c(
    "ja, ik ben getrouwd",
    "ja, ik woon samen en/of ik heb een geregistreerd partnerschap",
    "ja, ik heb een lat-relatie/verkering",
    "nee, ik ben alleenstaand/ik heb geen partner",
    "anders, nl."
    )
)

# Income
results_2$income_net_adu_q_1_a <- factor(
  results_2$income_net_adu_q_1_a,
  levels = c("1", "2", "3", "4", "5", "6", "7"),
  labels = c("1", "2", "3", "4", "5", "6", "meer dan 6")
)

results_2$income_net_adu_q_1_v3 <- factor(
  results_2$income_net_adu_q_1_v3, 
  levels = c("7", "8", "9", "10", "11", "12", "13", "14", "15", "17", "18", "19", "20"), # Doesn't hjave a 16 value...
  labels = c(
    "ik weet het niet",
    "ik wil hier geen antwoord op geven",
    "minder dan 750 euro",
    "750 - 1000 euro",
    "1000 - 1500 euro",
    "1500 - 2000 euro",
    "2000 - 2500 euro",
    "2500 - 3000 euro",
    "3000 - 3500 euro",
    "3500 - 4000 euro",
    "4000 - 4500 euro",
    "4500 - 5000 euro",
    "meer dan 5000 euro"
  )
)

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

results_2 %>% filter(measurement == "2b") %>% summarize(across(all_of(promis_vars), ~ sum(is.na(.)))) # None!

# Next, we turn all PROMIS values into numeric values, debugging as we go
sum(is.na(results_2$promis_anxiety_adu_q_05))
unique(results_2$promis_anxiety_adu_q_05)
results_2$promis_anxiety_adu_q_05 <- as.numeric(results_2$promis_anxiety_adu_q_05)
sum(is.na(results_2$promis_anxiety_adu_q_05))
unique(results_2$promis_anxiety_adu_q_05) # works!


results_2 <- results_2 %>%
  mutate(across(all_of(promis_vars), as.numeric))

# Now compute total scores according to the scoring manual
# Create raw sum scores for each domain 
# Physical function - REVERSE
# First, reverse coding so that higher = better
physical_vars <- paste0("promis_physical_adu_q_0", 1:4)
results_2[physical_vars] <- lapply(results_2[physical_vars],
                                   function(x) ifelse(!is.na(x), 6 - x, NA))

# Now compute sumscores
# Physical function
results_2 <- results_2 %>%
  mutate(promis_physical_rawsum = rowSums(across(c(
    promis_physical_adu_q_01, promis_physical_adu_q_02, promis_physical_adu_q_03, promis_physical_adu_q_04
  )), na.rm = T))
results_2$promis_physical_rawsum[results_2$promis_physical_rawsum == 0] <- NA

# Anxiety
results_2 <- results_2 %>%
  mutate(promis_anxiety_rawsum = rowSums(across(c(
    promis_anxiety_adu_q_05, promis_anxiety_adu_q_06, promis_anxiety_adu_q_07, promis_anxiety_adu_q_08
  )), na.rm = T))
results_2$promis_anxiety_rawsum[results_2$promis_anxiety_rawsum == 0] <- NA

# Depression
results_2 <- results_2 %>%
  mutate(promis_depression_rawsum = rowSums(across(c(
    promis_depression_adu_q_09, promis_depression_adu_q_10, promis_depression_adu_q_11, promis_depression_adu_q_12
  )), na.rm = T))
results_2$promis_depression_rawsum[results_2$promis_depression_rawsum == 0] <- NA

# Fatigue
results_2 <- results_2 %>%
  mutate(promis_fatigue_rawsum = rowSums(across(c(
    promis_fatigue_adu_q_13, promis_fatigue_adu_q_14, promis_fatigue_adu_q_15, promis_fatigue_adu_q_16
  )), na.rm = T))
results_2$promis_fatigue_rawsum[results_2$promis_fatigue_rawsum == 0] <- NA

# Sleep - half is REVERSE
sleep_vars <- c("promis_sleep_adu_q_17", "promis_sleep_adu_q_18")
results_2[sleep_vars] <- lapply(results_2[sleep_vars],
                                   function(x) ifelse(!is.na(x), 6 - x, NA))

results_2 <- results_2 %>%
  mutate(promis_sleep_rawsum = rowSums(across(c(
    promis_sleep_adu_q_17, promis_sleep_adu_q_18, promis_sleep_adu_q_19, promis_sleep_adu_q_20
  )), na.rm = T))
results_2$promis_sleep_rawsum[results_2$promis_sleep_rawsum == 0] <- NA

# Social - REVERSE
# First, flip the coding so that higher = better
social_vars <- paste0("promis_social_adu_q_", 21:24)
results_2[social_vars] <- lapply(results_2[social_vars],
                                function(x) ifelse(!is.na(x), 6 - x, NA))

# Now compute sumscores
results_2 <- results_2 %>%
  mutate(promis_social_rawsum = rowSums(across(c(
    promis_social_adu_q_21, promis_social_adu_q_22, promis_social_adu_q_23, promis_social_adu_q_24
  )), na.rm = T))
results_2$promis_social_rawsum[results_2$promis_social_rawsum == 0] <- NA

# Pain
results_2 <- results_2 %>%
  mutate(promis_painif_rawsum = rowSums(across(c(
    promis_pain_adu_q_25, promis_pain_adu_q_26, promis_pain_adu_q_27, promis_pain_adu_q_28
  )), na.rm = T))
results_2$promis_painif_rawsum[results_2$promis_painif_rawsum == 0] <- NA

# Remove respondents with missing values; we can't deal with them using scoring manuals
# but we only want to delete those with some NA values; those valid PROMIS respondents who
# have missing data. There's loads of respondents with full NA values across PROMIS, we're
# not interested in those.
# So some PROMIS scores, but not all = problematic, get rid of them.
# Physical function
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_physical_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_physical_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_physical_rawsum, na.rm = T) # looks good!

# Anxiety 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_anxiety_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_anxiety_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_anxiety_rawsum, na.rm = T) # looks good!

# Depression 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_depression_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_depression_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_depression_rawsum, na.rm = T) # looks good!

# Fatigue 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_fatigue_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_fatigue_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_fatigue_rawsum, na.rm = T) # looks good!

# Sleep 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_sleep_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_sleep_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_sleep_rawsum, na.rm = T) # looks good!

# Social 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(starts_with("promis_social_adu_q_")))),
    non_na_count = rowSums(!is.na(across(starts_with("promis_social_adu_q_")))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_social_rawsum, na.rm = T) # Looks good!

# Pain interference 
results_2 <- results_2 %>%
  mutate(
    na_count = rowSums(is.na(across(all_of(c("promis_pain_adu_q_25", "promis_pain_adu_q_26",
                                             "promis_pain_adu_q_27", "promis_pain_adu_q_28"))))),
    non_na_count = rowSums(!is.na(across(all_of(c("promis_pain_adu_q_25", "promis_pain_adu_q_26",
                                                  "promis_pain_adu_q_27", "promis_pain_adu_q_28"))))),
    mixed_na = na_count > 0 & non_na_count > 0
  )

results_2 <- results_2 %>% filter(mixed_na == F)
range(results_2$promis_sleep_rawsum, na.rm = T) # Looks good!


# Turn raw sumscores into tscores using the manual's scoring tables
# We're using PROMIS-29 Profile v1.0 scoring tables, as per Lifelines' advice
# Physical function
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2 <- results_2 %>%
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
results_2$promis_pf_zscore <- (results_2$promis_pf_tscore - 50)/10
results_2$promis_anx_zscore <- (results_2$promis_anx_tscore - 50)/10
results_2$promis_dp_zscore <- (results_2$promis_dp_tscore - 50)/10
results_2$promis_fa_zscore <- (results_2$promis_fa_tscore - 50)/10
results_2$promis_sl_zscore <- (results_2$promis_sl_tscore - 50)/10
results_2$promis_so_zscore <- (results_2$promis_so_tscore - 50)/10
results_2$promis_pif_zscore <- (results_2$promis_pif_tscore - 50)/10

# Step 3 is to create a pain intensity zscore
results_2$promis_pin_zscore <- (results_2$promis_pain_adu_q_29 - 2.31)/2.34

# Step 4 is to create a pain composite score
results_2 <- results_2 %>%
  mutate(promis_pain_comp = rowMeans(across(c(promis_pin_zscore, promis_pif_zscore)), na.rm = T))

# Step 5 is to create an emotional distress composite score
results_2 <- results_2 %>%
  mutate(promis_emo_comp = rowMeans(across(c(promis_anx_zscore, promis_dp_zscore)), na.rm = T))

# Step 6 is to multiply the z scores with the factor scoring coefficients from the manual
# for physical health
results_2 <- results_2 %>%
  mutate(
    promis_PH_sum_z = (promis_pf_zscore * 0.872) +
      (promis_pain_comp * -0.094) +
      (promis_so_zscore * 0.113) +
      (promis_fa_zscore * -0.009) +
      (promis_emo_comp * 0.003) +
      (promis_sl_zscore * 0.002)
           )
# for mental health           
results_2 <- results_2 %>%
  mutate(
    promis_MH_sum_z = (promis_fa_zscore * -0.351) +
      (promis_emo_comp * -0.257) +
      (promis_so_zscore * 0.252) +
      (promis_pain_comp * -0.154) +
      (promis_sl_zscore * -0.139) +
      (promis_pf_zscore * -0.015)
  )

# Convert to tscores
results_2 <- results_2 %>%
  mutate(
    promis_PH_tscore = (promis_PH_sum_z * 10) + 50,
    promis_MH_tscore = (promis_MH_sum_z * 10) + 50
  )

summary(results_2$promis_PH_tscore, na.rm = T)
summary(results_2$promis_MH_tscore, na.rm = T)

summary(results_2$promis_physical_adu_q_01, na.rm = T)
summary(results_2$promis_physical_adu_q_02, na.rm = T)
summary(results_2$promis_physical_adu_q_03, na.rm = T)
summary(results_2$promis_physical_adu_q_04, na.rm = T)


# RAND general health ----------------------------------------------------------
results_2$rand_generalhealth_adu_q_01 <- factor(
  results_2$rand_generalhealth_adu_q_01, 
  levels = likert_levels_5,
  labels = c(
    "uitstekend",
    "zeer goed",
    "goed",
    "matig",
    "slecht"
  )
)

results_2$rand_generalhealth_adu_q_02 <- factor(
  results_2$rand_generalhealth_adu_q_02, 
  levels = likert_levels_5,
  labels = c(
    "veel beter dan een jaar geleden",
    "iets beter dan een jaar geleden",
    "ongeveer hetzelfde als een jaar geleden",
    "iets slechter dan een jaar geleden",
    "veel slechter dan een jaar geleden"
  )
)



# 3 ----------------------------------------------------------------------------
# Measurement
results_3$measurement <- factor(
  results_3$measurement
)

# Gender
results_3$gender <- factor(
  results_3$gender
)

# Arthritis follow-up
results_3$arthritis_followup_adu_q_1 <- factor(
  results_3$arthritis_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# Canccer follow-up
results_3$cancer_followup_adu_q_1 <- factor(
  results_3$cancer_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# copd follow-up
results_3$copd_followup_adu_q_1 <- factor(
  results_3$copd_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# heart attack follow-up
results_3$heartattack_followup_adu_q_1 <- factor(
  results_3$heartattack_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# osteoarthritis follow-up
results_3$osteoarthritis_followup_adu_q_1 <- factor(
  results_3$osteoarthritis_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# psoriasis diagnosis
results_3$psoriasis_diagnosis_adu_q_1 <- factor(
  results_3$psoriasis_diagnosis_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# stroke follow-up
results_3$stroke_followup_adu_q_1 <- factor(
  results_3$stroke_followup_adu_q_1, levels = dich_levels1, labels = dich_labels1
)

# COPSOQ
results_3$copsoq_bu_adu_q_01 <- factor(
  results_3$copsoq_bu_adu_q_01,
  levels = c("1", "2", "3", "4", "5"), 
  labels = c("ja, dagelijks", "ja, wekelijks", "ja, maandelijks", "ja, af en toe", "nee")
)

results_3$copsoq_in_adu_q_01 <- factor(
  results_3$copsoq_in_adu_q_01, levels = likert_levels_5, labels = likert_labels_frequency
)

results_3$copsoq_in_adu_q_03 <- factor(
  results_3$copsoq_in_adu_q_03, levels = likert_levels_5, labels = likert_labels_frequency
)

results_3$copsoq_ji_adu_q_01 <- factor(
  results_3$copsoq_ji_adu_q_01, levels = dich_levels1, labels = dich_labels1
)

results_3$copsoq_ji_adu_q_03 <- factor(
  results_3$copsoq_ji_adu_q_03, levels = dich_levels1, labels = dich_labels1
)

results_3$copsoq_js_adu_q_04 <- factor(
  results_3$copsoq_js_adu_q_04, levels = likert_levels_4, labels = likert_labels_satisfaction
)

results_3$copsoq_mw_adu_q_01 <- factor(
  results_3$copsoq_mw_adu_q_01, levels = likert_levels_5, labels = likert_labels_quantity
)

results_3$copsoq_mw_adu_q_02 <- factor(
  results_3$copsoq_mw_adu_q_02, levels = likert_levels_5, labels = likert_labels_quantity
)

results_3$copsoq_pd_adu_q_01 <- factor(
  results_3$copsoq_pd_adu_q_01, levels = likert_levels_5, labels = likert_labels_quantity
)

results_3$copsoq_pd_adu_q_02 <- factor(
  results_3$copsoq_pd_adu_q_02, levels = likert_levels_5, labels = likert_labels_quantity
)

results_3$copsoq_qd_adu_q_03 <- factor(
  results_3$copsoq_qd_adu_q_03, levels = likert_levels_5, labels = likert_labels_frequency
)

results_3$copsoq_qd_adu_q_04 <- factor(
  results_3$copsoq_qd_adu_q_04, levels = likert_levels_5, labels = likert_labels_frequency
)

results_3$copsoq_sc_adu_q_01 <- factor(
  results_3$copsoq_sc_adu_q_01, levels = likert_levels_6, labels = likert_labels_frequency2
)

results_3$copsoq_sc_adu_q_02 <- factor(
  results_3$copsoq_sc_adu_q_02, levels = likert_levels_6, labels = likert_labels_frequency2
)

results_3$copsoq_ss_adu_q_01 <- factor(
  results_3$copsoq_ss_adu_q_01, levels = likert_levels_6, labels = likert_labels_frequency2
)

results_3$copsoq_ss_adu_q_02 <- factor(
  results_3$copsoq_ss_adu_q_02, levels = likert_levels_6, labels = likert_labels_frequency2
)

results_3$copsoq_sw_adu_q_03 <- factor(
  results_3$copsoq_sw_adu_q_03, levels = dich_levels1, labels = dich_labels1
)

results_3$copsoq_wp_adu_q_01 <- factor(
  results_3$copsoq_wp_adu_q_01, levels = likert_levels_5, labels = likert_labels_frequency
)

results_3$copsoq_wp_adu_q_02 <- factor(
  results_3$copsoq_wp_adu_q_02, levels = likert_levels_5, labels = likert_labels_frequency
)

# In house children
results_3$inhouse_children_adu_q_1 <- factor(
  results_3$inhouse_children_adu_q_1, levels = likert_levels_4, 
  labels = c("ja", "nee", "heb ik niet", "nee/nvt")
)

# WAI
results_3$wai_demands_adu_q_02_a <- factor(
  results_3$wai_demands_adu_q_02_a, levels = likert_levels_5, labels = likert_labels_quality
  )

results_3$wai_demands_adu_q_02_b <- factor(
  results_3$wai_demands_adu_q_02_b, levels = likert_levels_5, labels = likert_labels_quality
)

results_3$wai_future_adu_q_03 <- factor(
  results_3$wai_future_adu_q_03, 
  levels =  c("1", "2", "3"), 
  labels = c("onwaarschijnlijk", "misschien", "zeer waarschijnlijk")
)

results_3$wai_rating_adu_q_01 <- as.numeric(results_3$wai_rating_adu_q_01)




# covid ------------------------------------------------------------------------
covid <- covid %>% mutate(
  employment_type = case_when(
    covt02_contract_adu_q_1_a == "1" ~ "permanent contract",
    covt02_contract_adu_q_1_b == "1" ~ "temporary contract",
    covt02_contract_adu_q_1_c == "1" ~ "zero-hour contract",
    covt02_contract_adu_q_1_d == "1" ~ "freelance",
    covt02_contract_adu_q_1_e == "1" ~ "other"),
  employment_type = as.factor(employment_type)
  )



# SAVE RESULTS =================================================================
write_rds(results_2, here("1.data", "4.finalized", "results_2.rds"))
write_rds(results_3, here("1.data", "4.finalized", "results_3.rds"))
write_rds(covid, here("1.data", "4.finalized", "covid.rds"))






