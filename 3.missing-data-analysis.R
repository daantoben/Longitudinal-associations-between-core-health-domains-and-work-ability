# SET-UP =======================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(naniar)
library(mice)
library(finalfit)
library(ggmice)

# Load data
analysis_df <- read_rds(file = here("1.data", "2.processed", "analysis_df.rds"))

# Wrangle variables into the right class
analysis_df$squash_perweek_adu_q_15_a <- as.numeric(analysis_df$squash_perweek_adu_q_15_a)
analysis_df$squash_perweek_adu_q_15_b <- as.numeric(analysis_df$squash_perweek_adu_q_15_b)
analysis_df$smoking_current_adu_q_1 <- factor(analysis_df$smoking_current_adu_q_1)
analysis_df$smoking_duration_adu_c_2 <- as.numeric(analysis_df$smoking_duration_adu_c_2)
analysis_df$smoking_endage_adu_c_2 <- as.numeric(analysis_df$smoking_endage_adu_c_2)
analysis_df$smoking_startage_adu_c_2 <- as.numeric(analysis_df$smoking_startage_adu_c_2)

# Make sure outcome is listed as an ordinal factor
analysis_df$wai_demands_adu_q_02_a <- factor(
  analysis_df$wai_demands_adu_q_02_a, 
  levels = c("zeer goed", "goed", "matig", "slecht", "zeer slecht"), ordered = TRUE)
class(analysis_df$wai_demands_adu_q_02_a)

analysis_df$wai_demands_adu_q_02_b <- factor(
  analysis_df$wai_demands_adu_q_02_b, 
  levels = c("zeer goed", "goed", "matig", "slecht", "zeer slecht"), ordered = TRUE)
class(analysis_df$wai_demands_adu_q_02_b)

analysis_df$wai_future_adu_q_03 <- factor(
  analysis_df$wai_future_adu_q_03, 
  levels = c("onwaarschijnlijk", "misschien", "zeer waarschijnlijk"), ordered = TRUE)
class(analysis_df$wai_future_adu_q_03)

analysis_df$wai_rating_adu_q_01 <- factor(
  analysis_df$wai_rating_adu_q_01, 
  levels = 0:10, ordered = TRUE)
class(analysis_df$wai_rating_adu_q_01)




# MULTIPLE IMPUTATION ==========================================================
set.seed(2025)

# We will set m = 20, to correspond with WAI's missing % of 5191/27882 = 18.6%
# Missing % derived from naive OLR models (model_wai_rating$model8)
# And we will set iterations to 15 and see if the algorithm converges

# We'll handle effect modifiers using passive imputation rather than the just another variable method, which
# may introduce bias. We'll forego splitting the dataset by the effect-modifier and imputing separately
# as this is complex and we don't envision strong effect modification. Passive imputation is a nice
# middle-of-the-road approach, keeping our dataset intact

# First. create an initial mice object to easily set a custom prediction matrix
ini <- mice(analysis_df, m=1, maxit=0)
meth <- ini$method

v1 <- names(ini$nmis[ini$nmis == 0])
outlist1 <- v1[c(1:4, 7:8, 29:32, 35:40, 45:47)]
v2 <- as.character(ini$loggedEvents[, "out"])
outlist2 <- v2[c(1:4, 5:8, 10:18, 44:47, 51:56, 62, 64, 66:86)]
outlist <- unique(c(outlist1, outlist2))


fx <- flux(analysis_df)
fluxplot(analysis_df)
v3 <- row.names(fx)[fx$outflux > 0.5]
# Doesn't seem relevant, not adding it to outlist

# Specify variables to be imputed
vars_to_impute <- c(
  "wai_demands_adu_q_02_a", "wai_demands_adu_q_02_b", "wai_future_adu_q_03", "wai_rating_adu_q_01",
  "bodyweight_kg_all_m_1", "bodylength_cm_all_m_1")

# Specify variables that have to be included because they will be used in the final model
vars_include <- c(
  "promis_pf_tscore", "promis_dp_tscore", "promis_anx_tscore", "promis_fa_tscore",
  "promis_sl_tscore", "promis_so_tscore", "promis_pif_tscore", "promis_pain_adu_q_29",
  "comorbidity",
  "age", 
  "gender",
  "partner_presence_adu_q_1",
  "employment_type",
  "degree_highest_adu_q_1",
  "work_sector",
  "income_net_adu_q_1_v3", 
  "packyears_cumulative_adu_c_2",
  "promas_chronicdisease_adu_q_01", 
  "squash_perweek_adu_q_15_a",
  "rand_generalhealth_adu_q_02"
)


# Specify methods for imputation
meth[] <- ""
meth["bodylength_cm_all_m_1"] <- "pmm"
meth["bodyweight_kg_all_m_1"] <- "pmm"

meth["wai_demands_adu_q_02_a"] <- "polr"
meth["wai_demands_adu_q_02_b"] <- "polr"
meth["wai_future_adu_q_03"] <- "polr"
meth["wai_rating_adu_q_01"] <- "polr"

# Set passive imputation for interaction term gender as well as bmi
analysis_df$PH_gender <- 0 
meth["PH_gender"] <- "~ I(promis_PH_tscore * as.numeric(gender))" # Each time PH and gender are imputed, recalculate
# the interaction term

analysis_df$bmi <- 0
meth["bmi"] <- "~ I(bodyweight_kg_all_m_1 / ((bodylength_cm_all_m_1/100)^2))"

meth

# Identify relevant auxillary variables to use for imputation
preds <- quickpred(
  analysis_df,
  minpuc = 0.7,
  include = vars_include,
  exclude = outlist
  )

# Zero out all rows except for those of the outcome
preds[!(rownames(preds) %in% vars_to_impute), ] <- 0

# use outcome to impute outcome
preds[vars_to_impute, vars_to_impute] <- 1

# use final model to impute outcome
preds[vars_to_impute, vars_include] <- 1



# Add interaction term to predictor matrix while not imputing it, or using it as a predictor
preds["PH_gender", ] <- 0
preds[, "PH_gender"] <- 0
preds["bmi", ] <- 0
preds[ ,"bmi"] <- 0

diag(preds) <- 0

# Run MICE
imp <- mice(
  analysis_df,
  m = 20,
  maxit = 25,
  method = meth,
  predictorMatrix = preds,
  seed = 2025)

# Run diagnostics
test <- imp$imp$wai_rating_adu_q_01
test_df <- complete(imp, 3, include = FALSE)

sum(is.na(imp$imp$wai_rating_adu_q_01$`3`))
test <- imp$loggedEvents

# Check plots
plot(imp)
ggmice(imp, aes(x = .imp, y = wai_rating_adu_q_01)) +
  geom_density(alpha = 0.3) +
  labs(x = "Imputation", y = "Work ability")

# Save imputed datasets
saveRDS(imp, file = here("1.data", "4.finalyzed", "imputed_datasets.rds"))



# RECOMPUTE PROMIS =============================================================
# Now we recomposite PROMIS PH and MG composite scores within each imputed dataset
# And we do the same for the centered versions of promis
long_imp <- complete(imp, "long", include = TRUE)

# Generate Physical and Mental Health summary scores (thats as general as it gets)
# Step 1 was to create the tscores.
# Step 2 is to transform the tscores to zscores
long_imp <- long_imp %>%
  group_by(.imp) %>%
  mutate(
    promis_pf_zscore <- (promis_pf_tscore - 50)/10,
    promis_anx_zscore <- (promis_anx_tscore - 50)/10,
    promis_dp_zscore <- (promis_dp_tscore - 50)/10,
    promis_fa_zscore <- (promis_fa_tscore - 50)/10,
    promis_sl_zscore <- (promis_sl_tscore - 50)/10,
    promis_so_zscore <- (promis_so_tscore - 50)/10,
    promis_pif_zscore <- (promis_pif_tscore - 50)/10,
    # Step 3 is to create a pain intensity zscore
    promis_pin_zscore <- (promis_pain_adu_q_29 - 2.31)/2.34,
    # Step 4 is to create a pain composite score
    promis_pain_comp = rowMeans(across(c(promis_pin_zscore, promis_pif_zscore)), na.rm = T),
    # Step 5 is to create an emotional distress composite score
    promis_emo_comp = rowMeans(across(c(promis_anx_zscore, promis_dp_zscore)), na.rm = T),
    # Step 6 is to multiply the z scores with the factor scoring coefficients from the manual
    # for physical health
    promis_PH_sum_z = (promis_pf_zscore * 0.872) +
          (promis_pain_comp * -0.094) +
          (promis_so_zscore * 0.113) +
          (promis_fa_zscore * -0.009) +
          (promis_emo_comp * 0.003) +
          (promis_sl_zscore * 0.002),
    # for mental health           
    promis_MH_sum_z = (promis_fa_zscore * -0.351) +
          (promis_emo_comp * -0.257) +
          (promis_so_zscore * 0.252) +
          (promis_pain_comp * -0.154) +
          (promis_sl_zscore * -0.139) +
          (promis_pf_zscore * -0.015),
    # Convert to tscores
    promis_PH_tscore = (promis_PH_sum_z * 10) + 50,
    promis_MH_tscore = (promis_MH_sum_z * 10) + 50,
    # Compute bmi (it didn't compute correctly during imputation)
    bmi = (bodyweight_kg_all_m_1 / ((bodylength_cm_all_m_1/100)^2)),
    # Compute degree
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
    degree_5 = factor(degree_5),
    # Center all variables
    promis_pf_tscore_m = promis_pf_tscore - mean(promis_pf_tscore, na.rm = TRUE),
    promis_anx_tscore_m = promis_anx_tscore - mean(promis_anx_tscore, na.rm = TRUE),
    promis_dp_tscore_m = promis_dp_tscore - mean(promis_dp_tscore, na.rm = TRUE),
    promis_sl_tscore_m = promis_sl_tscore - mean(promis_sl_tscore, na.rm = TRUE),
    promis_so_tscore_m = promis_so_tscore - mean(promis_so_tscore, na.rm = TRUE),
    promis_pif_tscore_m = promis_pif_tscore - mean(promis_pif_tscore, na.rm = TRUE),
    promis_pain_adu_q_29_m = promis_pain_adu_q_29 - mean(promis_pain_adu_q_29, na.rm = TRUE),
    promis_PH_tscore_m = promis_PH_tscore - mean(promis_PH_tscore, na.rm = TRUE),
    promis_MH_tscore_m = promis_MH_tscore - mean(promis_MH_tscore, na.rm = TRUE),
    bmi_m = bmi - mean(bmi, na.rm = TRUE)
    ) %>%
  ungroup()

# Reform to mids object and save
imp <- as.mids(long_imp)
saveRDS(imp, file = here("1.data", "4.finalized", "imputed_datasetsx.rds"))