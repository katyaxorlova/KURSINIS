library(sdtm.oak)
library(sdtm.terminology)
library(sdtmchecks)
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

raw_data<-read_excel('/Users/agneirkinaite/Desktop/Kursinis /Data-RITS-GS.xlsx')

raw_data <- raw_data %>%
  mutate(
    oak_id = row_number(),
    raw_source = "RAW",
    patient_number = Nr.
  )

ct_spec <- tibble(
  codelist_code = "SEX",
  collected_value = c("Vyras", "Moteris"),
  term_value = c("M", "F"),
  term_synonyms = c(NA, NA)
)

#demografiniai duomenys

dm <- assign_ct(
  tgt_var = "SEX",
  raw_dat = raw_data,
  raw_var = "Lytis",
  ct_spec = ct_spec,
  ct_clst = "SEX"
)

dm <- assign_no_ct(
  tgt_dat = dm,
  tgt_var = "AGE",
  raw_dat = raw_data,
  raw_var = "Amžius"
)


dm <- dm %>%
  mutate(
    STUDYID = "VULSK-RITS-GS",
    DOMAIN = "DM",
    USUBJID = paste0("VULSK-RITS-GS-", patient_number),
    AGEU = "YEARS"
  )

dm <- dm %>%
  select(STUDYID, DOMAIN, USUBJID, AGE, AGEU, SEX)


# laboratoriniai duom

LB_stul <- c("Nr.",
             "CRB_max", "CRB_1_para", "CRB_5_para", "CRB_7_para",
             "PCT_max", "PCT_1_para", "PCT_5_para", "PCT_7_para",
             "albuminas_min",
             "Laktatas_max", "Laktatas_1_para", "Laktatas_5_para", "Laktatas_7_para")


LB_data <- raw_data %>%
  select(any_of(LB_stul)) %>%
  mutate(across(-Nr., as.character))

LB_long <- LB_data %>%
  pivot_longer(
    cols = -Nr.,
    names_to = "LBVAR",
    values_to = "LBORRES"
  )

LB_long <- LB_long %>%
  mutate(
    LBTESTCD = case_when(
      str_detect(LBVAR, "CRB") ~ "CRP",
      str_detect(LBVAR, "PCT") ~ "PCT",
      str_detect(LBVAR, "albuminas") ~ "ALB",
      str_detect(LBVAR, "Laktatas") ~ "LACT",
      TRUE ~ NA_character_
      ),
    
    LBTEST = case_when(
      LBTESTCD == "CRP" ~ "C-Proactive Protein",
      LBTESTCD == "PCT" ~ "Procalcitonin",
      LBTESTCD == "ALB" ~ "Albumin",
      LBTESTCD == "LACT" ~ "Lactate",
      TRUE ~ NA_character_
      ),
    LBORRESU = case_when(
      LBTESTCD == "CRP" ~ "mg/L",
      LBTESTCD == "PCT" ~ "ng/mL",
      LBTESTCD == "ALB" ~ "g/L",
      LBTESTCD == "LACT" ~ "mmol/L",
      TRUE ~ NA_character_
    ),
    
    VISIT = case_when(
      str_detect(LBVAR, "_1_para") ~ "Day 1",
      str_detect(LBVAR, "_5_para") ~ "Day 5",
      str_detect(LBVAR, "_7_para") ~ "Day 7",
      str_detect(LBVAR, "_max") ~ "Maximum",
      str_detect(LBVAR, "min") ~ "Minimum",
      TRUE ~ "Unspecified"
    ),
    STUDYID = "VULSK-RITS-GS",
    DOMAIN = "LB",
    USUBJID = paste0("VULSK-RITS-GS-", Nr.)
  ) %>%
  filter(!is.na(LBORRES) & !is.na(LBTESTCD)) %>%
  select(STUDYID, DOMAIN, USUBJID, VISIT, LBTESTCD, LBTEST, LBORRES, LBORRESU)

print(LB_long)


#QoL duomenauskai :)))))))

QS_duom <- raw_data %>%
  select(`Nr.`, Gyvenimo_kokybe_PCS_1, Gyvenimo_kokybe_PCS_2,
         Gyvenimo_kokybe_MCS_1, Gyvenimo_kokybe_MCS_2)

QS_long <- QS_duom %>%
  pivot_longer(
    cols = -Nr.,
    names_to = "QSVAR",
    values_to = "QSORRES"
  )

QS_long <- QS_long %>%
  mutate(
    QSTESTCD = case_when(
      str_detect(QSVAR, "PCS") ~ "PCS",
      str_detect(QSVAR, "MCS") ~ "MCS",
      TRUE ~ NA_character_
    ),
    QSTEST = case_when(
      QSTESTCD == "PCS" ~ "Physical Component Summary",
      QSTESTCD == "MCS" ~ "Mental Component Summary",
      TRUE ~ NA_character_
    ),
    QSCAT = "SF-36",
    VISIT = case_when(
      str_detect(QSVAR, "_1") ~ "Timepoint 1",
      str_detect(QSVAR, "_2") ~ "Timepoint 2",
      TRUE ~ "Unspecified"
    ),
    STUDYID = "VULSK-RITS-GS",
    DOMAIN = "QS",
    USUBJID = paste0("VULSK-RITS-GS-", Nr.)
  ) %>%
  filter(!is.na(QSORRES)) %>%
  select(STUDYID, DOMAIN, USUBJID, VISIT, QSCAT, QSTESTCD, QSTEST, QSORRES)

print(QS_long)



writexl::write_xlsx(dm, "SDTM_DM.xlsx")

writexl::write_xlsx(LB_long, "SDTM_LB.xlsx")

writexl::write_xlsx(QS_long, "SDTM_QS.xlsx")



