library(dplyr)
library(lubridate)
library(tidyr)
library(admiral)
library(readxl)

dm <- read_excel('/Users/agneirkinaite/Desktop/Kursinis /SDTM_DM.xlsx')

lb <- read_excel('/Users/agneirkinaite/Desktop/Kursinis /SDTM_LB.xlsx')

qs <- read_excel('/Users/agneirkinaite/Desktop/Kursinis /SDTM_QS.xlsx')


adsl <- dm %>%
  mutate(
    TRT01P = "Observed",       
    TRT01A = "Observed",       
    SAFFL = "Y",               
    ITTFL = "Y",               
    AGEGR1 = case_when(
      AGE < 30 ~ "<30",
      AGE >= 30 & AGE < 40 ~ "30–39",
      AGE >= 40 & AGE < 50 ~ "40–49",
      AGE >= 50 & AGE < 60 ~ "50–59",
      AGE >= 60 & AGE < 70 ~ "60–69",
      AGE >= 70 ~ ">=70",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, AGE, AGEU,
    AGEGR1, SEX, TRT01P, TRT01A, SAFFL, ITTFL
  )


adlb <- lb %>%
  left_join(adsl, by = c("USUBJID", "STUDYID")) %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    AVAL = as.numeric(LBORRES),
    AVALU = LBORRESU
  )

baseline <- adlb %>%
  filter(VISIT == "Day 1") %>%
  select(USUBJID, PARAMCD, BASE = AVAL)

adlb <- adlb %>%
  left_join(baseline, by = c("USUBJID", "PARAMCD")) %>%
  mutate(
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    ABLFL = if_else(VISIT == "Day 1", "Y", NA_character_),
    ANL01FL = "Y"
  ) %>%
  select(
    STUDYID, USUBJID, VISIT,
    PARAMCD, PARAM, AVAL, BASE, CHG, AVALU,
    AGE, AGEGR1, SEX, TRT01A, SAFFL, ITTFL,
    ABLFL, ANL01FL
  )



adqs <- qs %>%
  left_join(adsl, by = c("USUBJID", "STUDYID"))


adqs <- qs %>%
  left_join(adsl, by = c("USUBJID", "STUDYID")) %>%
  mutate(
    PARAMCD = QSTESTCD,
    PARAM = QSTEST,
    AVAL = as.numeric(QSORRES),
    ABLFL = if_else(VISIT == "Timepoint 1", "Y", NA_character_),
    ANL01FL = "Y"
  ) %>%
  select(
    STUDYID, USUBJID, VISIT,
    QSCAT, PARAMCD, PARAM, AVAL,
    AGE, AGEGR1, SEX, TRT01A, SAFFL, ITTFL,
    ABLFL, ANL01FL
  )

