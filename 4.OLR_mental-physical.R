# SET-UP =======================================================================
# Load libraries
library(tidyr)
library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(forcats)
library(viridisLite)
library(MASS)
library(brant)
library(lmtest)
library(car)
library(mice)

# Session info
sessionInfo()



# IMPUTED DATASETS =============================================================
# Load data
imp <- readRDS(here("1.data", "4.finalized", "imputed_datasetsx.rds"))

# Filter for respondents with a permanent contract
long_imp <- complete(imp, "long", include = T)

long_imp <- long_imp %>% filter(
  employment_type == "permanent contract" |
    employment_type == "temporary contract" |
    employment_type == "zero-hour contract")

long_imp <- long_imp %>%
  mutate(
    wai_demands_adu_q_02_a = factor(wai_demands_adu_q_02_a,
                                    levels = c("zeer slecht", "slecht", "matig", "goed", "zeer goed")),
    wai_demands_adu_q_02_b = factor(wai_demands_adu_q_02_b,
                                    levels = c("zeer slecht", "slecht", "matig", "goed", "zeer goed"))
  )



# Collapse lowest level of physical and mental wai as "zeer slecht" only holds 12 or 19 respondents
long_imp <- long_imp %>%
  mutate(wai_demands_adu_q_02_a = factor(
    dplyr::recode(wai_demands_adu_q_02_a,
                  "slecht" = "slecht of zeer slecht",
                  "zeer slecht" = "slecht of zeer slecht")
    )
  )

long_imp <- long_imp %>%
  mutate(wai_demands_adu_q_02_b = factor(
    dplyr::recode(wai_demands_adu_q_02_b,
                  "slecht" = "slecht of zeer slecht",
                  "zeer slecht" = "slecht of zeer slecht")
    )
  )

# Collapse Squash
long_imp <- long_imp %>%
  mutate(
    squash_perweek_adu_q_15_a = factor(
      dplyr::recode(squash_perweek_adu_q_15_a,
                    "0" = "0",
                    "1" = "1",
                    "2" = "2",
                    "3" = "3",
                    "4" = "4",
                    "5" = "5",
                    "6" = "6",
                    "7" = "7 or more",
                    "8" = "7 or more",
                    "9" = "7 or more")
    )
  )

long_imp$squash_perweek_adu_q_15_a[long_imp$squash_perweek_adu_q_15_a == "$6"] <- NA
long_imp$squash_perweek_adu_q_15_a[long_imp$squash_perweek_adu_q_15_a == "$7"] <- NA

imp <- as.mids(long_imp)



# WAI SCORE AND PROMIS SUBDOMAINS IMPUTED ======================================
# Mental -----------------------------------------------------------------------
# Create models list
models_wai_ment_sub_imp <- list()
pooled_wai_ment_sub_imp <- list()

# Model 1: WAS - Physical function
models_wai_ment_sub_imp[["pf"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_pf_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["pf"]] <- pool(models_wai_ment_sub_imp[["pf"]])
pooled1 <- summary(pooled_wai_ment_sub_imp[["pf"]])

# Model 2: WAS - Anxiety
models_wai_ment_sub_imp[["anx"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_anx_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["anx"]] <- pool(models_wai_ment_sub_imp[["anx"]])
pooled2 <- summary(pooled_wai_ment_sub_imp[["anx"]])


# Model 3: WAS - Depression
models_wai_ment_sub_imp[["dp"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_dp_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["dp"]] <- pool(models_wai_ment_sub_imp[["dp"]])
pooled3 <- summary(pooled_wai_ment_sub_imp[["dp"]])


# Model 4: WAS - Fatigue
models_wai_ment_sub_imp[["fa"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_fa_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["fa"]] <- pool(models_wai_ment_sub_imp[["fa"]])
pooled4 <- summary(pooled_wai_ment_sub_imp[["fa"]])


# Model 5: WAS - Sleep
models_wai_ment_sub_imp[["sl"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_sl_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["sl"]] <- pool(models_wai_ment_sub_imp[["sl"]])
pooled5 <- summary(pooled_wai_ment_sub_imp[["sl"]])


# Model 6: WAS - Social
models_wai_ment_sub_imp[["so"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_so_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["so"]] <- pool(models_wai_ment_sub_imp[["so"]])
pooled6 <- summary(pooled_wai_ment_sub_imp[["so"]])


# Model 7: WAS - Pain interference
models_wai_ment_sub_imp[["pif"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_pif_tscore_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["pif"]] <- pool(models_wai_ment_sub_imp[["pif"]])
pooled8 <- summary(pooled_wai_ment_sub_imp[["pif"]])


# Model 8: WAS - Pain intensity
models_wai_ment_sub_imp[["pi"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["pi"]] <- pool(models_wai_ment_sub_imp[["pi"]])
pooled8 <- summary(pooled_wai_ment_sub_imp[["pi"]])


# Model 9: WAS - All
models_wai_ment_sub_imp[["all"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["all"]] <- pool(models_wai_ment_sub_imp[["all"]])
pooled9 <- summary(pooled_wai_ment_sub_imp[["all"]])


# Model 10: plus controlled for confounders vd Berg
models_wai_ment_sub_imp[["model10"]] <- with(imp, polr(
  wai_demands_adu_q_02_b ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m +
    age + bmi + squash_perweek_adu_q_15_a,
  Hess = T, method = "logistic"))

pooled_wai_ment_sub_imp[["model10"]] <- pool(models_wai_ment_sub_imp[["model10"]])
summary(pooled_wai_ment_sub_imp[["model10"]])


save(models_wai_ment_sub_imp, file = here("1.data", "4.finalized", "OLR_wai_ment_sub_imp5x.RData"))
save(pooled_wai_ment_sub_imp, file = here("1.data", "4.finalized", "Pooled_wai_ment_sub_imp5x.RData"))

load(here("1.data", "3.analyzed", "OLR_wai_ment_sub_impx.RData"))



# Physical ---------------------------------------------------------------------
# Create models list
models_wai_phys_sub_imp <- list()
pooled_wai_phys_sub_imp <- list()

# Model 1: WAS - Physical function
models_wai_phys_sub_imp[["pf"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pf_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["pf"]] <- pool(models_wai_phys_sub_imp[["pf"]])
pooled1 <- summary(pooled_wai_phys_sub_imp[["pf"]])

# Model 2: WAS - Anxiety
models_wai_phys_sub_imp[["anx"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_anx_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["anx"]] <- pool(models_wai_phys_sub_imp[["anx"]])
pooled2 <- summary(pooled_wai_phys_sub_imp[["anx"]])


# Model 3: WAS - Depression
models_wai_phys_sub_imp[["dp"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_dp_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["dp"]] <- pool(models_wai_phys_sub_imp[["dp"]])
pooled3 <- summary(pooled_wai_phys_sub_imp[["dp"]])


# Model 4: WAS - Fatigue
models_wai_phys_sub_imp[["fa"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_fa_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["fa"]] <- pool(models_wai_phys_sub_imp[["fa"]])
pooled4 <- summary(pooled_wai_phys_sub_imp[["fa"]])


# Model 5: WAS - Sleep
models_wai_phys_sub_imp[["sl"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_sl_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["sl"]] <- pool(models_wai_phys_sub_imp[["sl"]])
pooled5 <- summary(pooled_wai_phys_sub_imp[["sl"]])


# Model 6: WAS - Social
models_wai_phys_sub_imp[["so"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_so_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["so"]] <- pool(models_wai_phys_sub_imp[["so"]])
pooled6 <- summary(pooled_wai_phys_sub_imp[["so"]])


# Model 7: WAS - Pain interference
models_wai_phys_sub_imp[["pif"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pif_tscore_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["pif"]] <- pool(models_wai_phys_sub_imp[["pif"]])
pooled8 <- summary(pooled_wai_phys_sub_imp[["pif"]])


# Model 8: WAS - Pain intensity
models_wai_phys_sub_imp[["pi"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["pi"]] <- pool(models_wai_phys_sub_imp[["pi"]])
pooled8 <- summary(pooled_wai_phys_sub_imp[["pi"]])


# Model 9: WAS - All
models_wai_phys_sub_imp[["all"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["all"]] <- pool(models_wai_phys_sub_imp[["all"]])
pooled9 <- summary(pooled_wai_phys_sub_imp[["all"]])


# Model 10: plus controlled by confounders vd Berg
models_wai_phys_sub_imp[["model10"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m +
    age + bmi + squash_perweek_adu_q_15_a,
  Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["model10"]] <- pool(models_wai_phys_sub_imp[["model10"]])
pooled10 <- summary(pooled_wai_phys_sub_imp[["model10"]])

# Model 11: plus controlled by confounders vd Berg + em
models_wai_phys_sub_imp[["model11"]] <- with(imp, polr(
  wai_demands_adu_q_02_a ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m +
    age + gender + bmi + squash_perweek_adu_q_15_a +
    promis_pf_tscore_m:gender,
  Hess = T, method = "logistic"))

pooled_wai_phys_sub_imp[["model11"]] <- pool(models_wai_phys_sub_imp[["model11"]])
pooled11 <- summary(pooled_wai_phys_sub_imp[["model11"]])

# Save models
save(models_wai_phys_sub_imp, file = here("1.data", "4.finalized", "OLR_wai_phys_sub_impx.RData"))
save(pooled_wai_phys_sub_imp, file = here("1.data", "4.finalized", "Pooled_wai_phys_sub_impx.RData"))
