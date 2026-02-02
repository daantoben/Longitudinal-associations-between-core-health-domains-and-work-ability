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

long_imp <- complete(imp, action = "long", include = TRUE)

long_imp <- long_imp %>%
  mutate(
    wai_rating_adu_q_01 = factor(
      dplyr::recode(wai_rating_adu_q_01,
                    "0" = "<6",
                    "1" = "<6",
                    "2" = "<6",
                    "3" = "<6",
                    "4" = "<6",
                    "5" = "<6")
    )
  )

imp <- as.mids(long_imp)




# WAI SCORE AND PROMIS SUBDOMAINS IMPUTED ======================================
# Create models list
models_wai_sub_imp <- list()
pooled_wai_sub_imp <- list()

# Model 1: WAS - Physical function
models_wai_sub_imp[["pf"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pf_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["pf"]] <- pool(models_wai_sub_imp[["pf"]])
pooled1 <- summary(pooled_wai_sub_imp[["pf"]])

# Model 2: WAS - Anxiety
models_wai_sub_imp[["anx"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_anx_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["anx"]] <- pool(models_wai_sub_imp[["anx"]])
pooled2 <- summary(pooled_wai_sub_imp[["anx"]])


# Model 3: WAS - Depression
models_wai_sub_imp[["dp"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_dp_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["dp"]] <- pool(models_wai_sub_imp[["dp"]])
pooled3 <- summary(pooled_wai_sub_imp[["dp"]])


# Model 4: WAS - Fatigue
models_wai_sub_imp[["fa"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_fa_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["fa"]] <- pool(models_wai_sub_imp[["fa"]])
pooled4 <- summary(pooled_wai_sub_imp[["fa"]])


# Model 5: WAS - Sleep
models_wai_sub_imp[["sl"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_sl_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["sl"]] <- pool(models_wai_sub_imp[["sl"]])
pooled5 <- summary(pooled_wai_sub_imp[["sl"]])


# Model 6: WAS - Social
models_wai_sub_imp[["so"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_so_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["so"]] <- pool(models_wai_sub_imp[["so"]])
pooled6 <- summary(pooled_wai_sub_imp[["so"]])


# Model 7: WAS - Pain interference
models_wai_sub_imp[["pif"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pif_tscore_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["pif"]] <- pool(models_wai_sub_imp[["pif"]])
pooled8 <- summary(pooled_wai_sub_imp[["pif"]])


# Model 8: WAS - Pain intensity
models_wai_sub_imp[["pi"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["pi"]] <- pool(models_wai_sub_imp[["pi"]])
pooled8 <- summary(pooled_wai_sub_imp[["pi"]])


# Model 9: WAS - All
models_wai_sub_imp[["all"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m, Hess = T, method = "logistic"))

pooled_wai_sub_imp[["all"]] <- pool(models_wai_sub_imp[["all"]])
pooled9 <- summary(pooled_wai_sub_imp[["all"]])


summary(pooled_wai_sub_imp[["all"]])


# Model 10: Controlled for confounder vd Berg
models_wai_sub_imp[["model10"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m +
    age + bmi + squash_perweek_adu_q_15_a,
  Hess = T, method = "logistic"))

pooled_wai_sub_imp[["model10"]] <- pool(models_wai_sub_imp[["model10"]])

a <- pool(models_wai_sub_imp[["model10"]])
summary(a)

summary(pooled_wai_sub_imp[["model10"]])


# Model 11: Controlled for confounder vd Berg + em
models_wai_sub_imp[["model11"]] <- with(imp, polr(
  wai_rating_adu_q_01 ~ promis_pf_tscore_m + promis_anx_tscore_m + promis_dp_tscore_m + 
    promis_fa_tscore_m + promis_sl_tscore_m + promis_so_tscore_m + promis_pif_tscore_m + 
    promis_pain_adu_q_29_m +
    age + gender +  bmi + squash_perweek_adu_q_15_a +
    promis_pf_tscore_m:gender,
  Hess = T, method = "logistic"))

pooled_wai_sub_imp[["model11"]] <- pool(models_wai_sub_imp[["model11"]])
a <- pool(models_wai_sub_imp[["model11"]])
summary(a)
summary(pooled_wai_sub_imp[["model11"]])

save(models_wai_sub_imp, file = here("1.data", "4.finalized", "OLR_wai_sub_impx.RData"))
save(pooled_wai_sub_imp, file = here("1.data", "4.finalized", "Pooled_wai_sub_impx.RData"))
