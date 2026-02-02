# Load libraries
library(gtsummary)
library(forcats)
library(dplyr)
library(here)

# Load data
analysis_df <- read_rds(here("1.data", "4.finalized", "analysis_df.rds"))



# Remove NA values
analysis_df$squash_perweek_adu_q_15_a[analysis_df$squash_perweek_adu_q_15_a == "$6"] <- NA
analysis_df$squash_perweek_adu_q_15_a[analysis_df$squash_perweek_adu_q_15_a == "$7"] <- NA

# Combine the latter levels of SQUASH to comply with Lifelines >=10 individuals export rule
analysis_df <- analysis_df %>%
  mutate(
    squash_perweek_adu_q_15_a = factor(
      dplyr::recode(squash_perweek_adu_q_15_a,
                    "8" = "8 or more",
                    "9" = "8 or more")
    )
  )

# Collapse WAI variables to match their unit in analysis
analysis_df <- analysis_df %>%
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

analysis_df <- analysis_df %>%
  mutate(wai_demands_adu_q_02_a = factor(
    dplyr::recode(wai_demands_adu_q_02_a,
                  "slecht" = "slecht of zeer slecht",
                  "zeer slecht" = "slecht of zeer slecht")
  )
  )

analysis_df <- analysis_df %>%
  mutate(wai_demands_adu_q_02_b = factor(
    dplyr::recode(wai_demands_adu_q_02_b,
                  "slecht" = "slecht of zeer slecht",
                  "zeer slecht" = "slecht of zeer slecht")
  )
  )

# Rename factor levels
analysis_df <- analysis_df %>%
  mutate(
    gender = dplyr::recode(as.character(gender),
      "MALE" = "male", 
      "FEMALE" = "female"),
    partner_presence_adu_q_1 = dplyr::recode(partner_presence_adu_q_1,
      `ja, ik ben getrouwd` = "married",
      `ja, ik woon samen en/of ik heb een geregistreerd partnerschap` = "cohabiting/registered partnership",
      `ja, ik heb een lat-relatie/verkering` = "long-distance relationship",
      `nee, ik ben alleenstaand/ik heb geen partner` = "single",
      `anders, nl.` = "other"),
    income_net_adu_q_1_v3 = dplyr::recode(income_net_adu_q_1_v3,
      `ik weet het niet` = "I don't know",
      `ik wil hier geen antwoord op geven` = "I don't want to answer",
      `minder dan 750 euro` = "< 750",
      `750 - 1000 euro` = "750-1000",
      `1000 - 1500 euro` = "1000-1500",
      `1500 - 2000 euro` = "1500-2000",
      `2000 - 2500 euro` = "2000-2500",
      `2500 - 3000 euro` = "2500-3000",
      `3000 - 3500 euro` = "3000-3500",
      `3500 - 4000 euro` = "3500-4000",
      `4000 - 4500 euro` = "4000-4500",
      `4500 - 5000 euro` = "4500-5000",
      `meer dan 5000 euro` = "> 5000"),
    comorbidity = dplyr::recode(comorbidity, `0` = "absent", `1` = "present"),
    rand_generalhealth_adu_q_01 = dplyr::recode(rand_generalhealth_adu_q_01,
      `uitstekend` = "excellent",
      `zeer goed` = "very good",
      `goed` = "good",
      `matig` = "mediocre",
      `slecht` = "poor"),
    rand_generalhealth_adu_q_02 = dplyr::recode(rand_generalhealth_adu_q_02,
      `veel beter dan een jaar geleden` = "much better",
      `iets beter dan een jaar geleden` = "somewhat better",
      `ongeveer hetzelfde als een jaar geleden` = "about the same",
      `iets slechter dan een jaar geleden` = "somewhat worse",
      `veel slechter dan een jaar geleden` = "much worse")
    )

                                      
# Construct table
tbl1 <- analysis_df %>% 
  tbl_summary(
    include = c(
      age, gender, degree_5, partner_presence_adu_q_1, 
      employment_type, work_sector, income_net_adu_q_1_v3,
      packyears_cumulative_adu_c_2, promas_chronicdisease_adu_q_01, squash_perweek_adu_q_15_a,
      rand_generalhealth_adu_q_01, rand_generalhealth_adu_q_02
    ), missing = "ifany",
    label = list(
      age ~ "Age",
      gender ~ "Gender",
      degree_5 ~ "Educational attainment",
      partner_presence_adu_q_1 ~ "Marital status",
      employment_type ~ "Employment type",
      work_sector ~ "Work - ISO",
      income_net_adu_q_1_v3 ~ "Net monthly income (EUR)",
      packyears_cumulative_adu_c_2 ~ "Packyears - cum.",
      promas_chronicdisease_adu_q_01 ~ "Comorbidity",
      squash_perweek_adu_q_15_a ~ "Physical activity (hrs/week)",
      rand_generalhealth_adu_q_01 ~ "RAND - current health",
      rand_generalhealth_adu_q_02 ~ "RAND - last year"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)",
      age ~ "{median} ({p25}-{p75})",
      packyears_cumulative_adu_c_2 ~ "{median} ({p25}-{p75})"
  ),
  digits = list(all_categorical() ~ 1)
  ) %>%
  bold_labels()

tbl1

# Save table
saveRDS(tbl1, here("1.data", "4.finalized", "table_1.rds"))

