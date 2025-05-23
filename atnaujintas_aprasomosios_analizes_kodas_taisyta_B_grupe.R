
library(readxl)
library(dplyr)
library(openxlsx)

setwd("/Users/katyaorlova/Desktop/KURSINIS")
data <- read_excel("Data-RITS-GS.xlsx")

# Amžiaus grupės (C)
data <- data %>%
  mutate(C_grupe = case_when(
    `Amžius` < 40 ~ "<40",
    `Amžius` >= 40 & `Amžius` <= 60 ~ "40–60",
    `Amžius` > 60 ~ ">60",
    TRUE ~ NA_character_
  ))

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

calculate_descriptive_stats <- function(data, variables, group_var, group_label) {
  existing_vars <- intersect(variables, colnames(data))
  results <- list()

  for (var in existing_vars) {
    for (grp in unique(na.omit(data[[group_var]]))) {
      subset <- data[data[[group_var]] == grp, ]
      values <- subset[[var]]
      if (is.numeric(values)) {
        shapiro_p <- if (length(na.omit(values)) >= 3 && length(unique(values)) >= 3) {
          tryCatch(shapiro.test(values)$p.value, error = function(e) NA)
        } else { NA }

        stats <- data.frame(
          Kintamasis = var,
          Grupė = as.character(grp),
          Grupavimo_tipas = group_label,
          N = sum(!is.na(values)),
          NA_sk = sum(is.na(values)),
          Normalumo_p = shapiro_p,
          Vidurkis = mean(values, na.rm = TRUE),
          SD = sd(values, na.rm = TRUE),
          Mediana = median(values, na.rm = TRUE),
          Q1 = quantile(values, 0.25, na.rm = TRUE),
          Q3 = quantile(values, 0.75, na.rm = TRUE),
          Min = min(values, na.rm = TRUE),
          Max = max(values, na.rm = TRUE)
        )
        results[[length(results)+1]] <- stats
      }
    }
  }
  bind_rows(results)
}

stat_B <- calculate_descriptive_stats(data, kintamieji, "Covid19", "B (COVID-19)")
stat_C <- calculate_descriptive_stats(data, kintamieji, "C_grupe", "C (Amžiaus grupė)")
stat_G <- calculate_descriptive_stats(data, kintamieji, "Lytis", "G (Lytis)")

write.xlsx(stat_B, file = "apr_statistika_grupe_B.xlsx", sheetName = "Admission Type", rowNames = FALSE)
write.xlsx(stat_C, file = "apr_statistika_grupe_C.xlsx", sheetName = "Amžiaus grupė", rowNames = FALSE)
write.xlsx(stat_G, file = "apr_statistika_grupe_G.xlsx", sheetName = "Lytis", rowNames = FALSE)

stat_all <- bind_rows(stat_B, stat_C, stat_G)
write.xlsx(stat_all, file = "apr_statistika_visos.xlsx", sheetName = "Visos grupės", rowNames = FALSE)
View(stat_all)
