library(readxl)
library(dplyr)

# Įkeliame duomenis
setwd("/Users/katyaorlova/Desktop/KURSINIS")
data <- read_excel("Data-RITS-GS.xlsx")

# Sukuriame amžiaus grupes (C)
data <- data %>%
  mutate(C_grupe = case_when(
    `Amžius` < 40 ~ "<40",
    `Amžius` >= 40 & `Amžius` <= 60 ~ "40–60",
    `Amžius` > 60 ~ ">60",
    TRUE ~ NA_character_
  ))

# Tikslūs kintamieji pagal failo stulpelių pavadinimus
kintamieji <- c(
  "Ranka", "Amžius", "KMI", "Nutukimas", "SOFA_1", "SOFA_7", "APACHE_II", "NRS_2002", "Admission Type",
  "DPV_trukme_val.", "DPV", "CRB_max", "CRB_1_para", "CRB_5_para", "CRB_7_para", "PCT_max", "PCT_1_para",
  "PCT_5_para", "PCT_7_para", "albuminas_min", "Laktatas_max_7", "Laktatas_max", "Laktatas_1_para",
  "Laktatas_5_para", "Laktatas_7_para", "FK_%_pokytis_1-5", "FK_%_pokytis_1-7",
  "Skeletal_muscle_mass_%_pokytis_1-5", "Skeletal_muscle_mass_%_pokytis_1-7",
  "Viskas_%_pokytis_1-5", "Viskas_%_pokytis_1-7", "Pirmine_bakterine_infekcija", "Hospitaline_infekcija",
  "Sepsis", "Hospitaline_pneumonija", "Pneumonija", "Intraabdominaline", "STI", "Kraujas", "Meningitas",
  "Minkstieji_audiniai", "Zaizda", "Kaulai_sanariai", "CVK", "Kitos_infekcijos", "Reabilitacija"
)

# Funkcija aprašomajai analizei
analyse_by_group <- function(data, variables, group_var, group_label) {
  existing_vars <- intersect(variables, colnames(data))
  data %>%
    select(all_of(existing_vars), Grupavimas = all_of(group_var)) %>%
    group_by(Grupavimas) %>%
    summarise(across(where(is.numeric), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"),
    .groups = "drop") %>%
    mutate(Grupavimo_tipas = group_label) %>%
    relocate(Grupavimo_tipas)
}

# Apskaičiuojame aprašomąją statistiką pagal grupes
stat_B <- analyse_by_group(data, kintamieji, "Admission Type", "B (Admission Type)")
stat_C <- analyse_by_group(data, kintamieji, "C_grupe", "C (Amžiaus grupė)")
stat_G <- analyse_by_group(data, kintamieji, "Lytis", "G (Lytis)")

library(openxlsx)

file = "/Users/katyaorlova/Desktop/KURSINIS/statistika_grupe_B.xlsx"


# Įrašyti kiekvieną lentelę į atskirą Excel failą
write.xlsx(stat_B, file = "apr_statistika_grupe_B.xlsx", sheetName = "Priėmimo tipas", rowNames = FALSE)
write.xlsx(stat_C, file = "apr_statistika_grupe_C.xlsx", sheetName = "Amžiaus grupė", rowNames = FALSE)
write.xlsx(stat_G, file = "apr_statistika_grupe_G.xlsx", sheetName = "Lytis", rowNames = FALSE)


# Sujungiame į vieną lentelę
stat_all <- bind_rows(stat_B, stat_C, stat_G)

# Parodome lentelę viename lange
View(stat_all)
