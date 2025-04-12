library(readxl)
library(dplyr)
library(psych)

# Įkeliam duomenis
setwd("/Users/katyaorlova/Desktop/KURSINIS")
data <- read_excel("Data-RITS-GS.xlsx")

# Sukuriame amžiaus grupes (C)
data <- data %>%
  mutate(C_grupe = case_when(
    Amžius < 40 ~ "<40",
    Amžius >= 40 & Amžius <= 60 ~ "40–60",
    Amžius > 60 ~ ">60",
    TRUE ~ NA_character_
  ))

# Kintamieji
DL <- LETTERS[4:12]
AM_AZ <- paste0("A", LETTERS[13:26])
DJ_DZ <- paste0("D", LETTERS[10:26])
columns_needed <- c(DL, "S", "T", AM_AZ, "BP", "BQ", "CV", "CW", DJ_DZ)

# Saugiai atrenkam esamus kintamuosius
columns_exist <- intersect(columns_needed, colnames(data))

# ----------------------------
# FUNKCIJA: aprašomoji analizė pagal grupę
analyse_by_group <- function(group_var) {
  data %>%
    select(all_of(columns_exist), Grupavimas = all_of(group_var)) %>%
    group_by(Grupavimas) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"))
}
# ----------------------------

# Pagal B: Admission Type
stat_B <- analyse_by_group("Admission Type")

# Pagal C: Amžiaus grupė
stat_C <- analyse_by_group("C_grupe")

# Pagal G: Lytis
stat_G <- analyse_by_group("Lytis")

View(stat_B)
View(stat_C)
View(stat_G)


