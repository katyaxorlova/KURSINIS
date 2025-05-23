library(readxl)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(lmtest)

raw_data <- read_excel("/Users/agneirkinaite/Desktop/Kursinis /Data-RITS-GS.xlsx")

qol_vars <- c(
  "Gyvenimo_kokybe_PCS_1", "Gyvenimo_kokybe_PCS_2",
  "Gyvenimo_kokybe_MCS_1", "Gyvenimo_kokybe_MCS_2"
)

vars_to_test <- c(
  "Amžius", "KMI", "SOFA_1", "SOFA_7", "APACHE_II", "NRS_2002",
  "DPV_trukme_val.", "CRB_max", "CRB_1_para", "CRB_5_para", "CRB_7_para",
  "PCT_max", "PCT_1_para", "PCT_5_para", "PCT_7_para",
  "albuminas_min", "Laktatas_max_7", "Laktatas_max", "Laktatas_1_para",
  "Laktatas_5_para", "Laktatas_7_para",
  "FK_%_pokytis_1-5", "FK_%_pokytis_1-7",
  "Skeletal_muscle_mass_%_pokytis_1-5", "Skeletal_muscle_mass_%_pokytis_1-7",
  "Viskas_%_pokytis_1-5", "Viskas_%_pokytis_1-7"
)

palyginimas <- raw_data %>%
  select(Covid19, Lytis, Nutukimas, all_of(qol_vars)) %>%
  mutate(across(all_of(qol_vars), ~na_if(.x, "N/A"))) %>%
  mutate(across(all_of(qol_vars), as.numeric))

clean_data <- palyginimas %>%
  drop_na(all_of(qol_vars))

cor_data <- raw_data %>%
  select(all_of(c(vars_to_test, qol_vars))) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  drop_na(all_of(qol_vars))

# Shapiro-Wilk normality test
normality_results <- lapply(vars_to_test, function(var) {
  if (all(is.na(cor_data[[var]]))) {
    return(data.frame(Variable = var, W = NA, p_value = NA))
  }
  result <- shapiro.test(cor_data[[var]])
  data.frame(Variable = var, W = result$statistic, p_value = result$p.value)
})

normality_results_df <- bind_rows(normality_results)
print(normality_results_df)

group_vars <- c("Covid19", "Lytis", "Nutukimas")

summary_results <- list()

for (group in group_vars) {
  for (qol in qol_vars) {
    
    test_data <- clean_data %>%
      select(all_of(group), all_of(qol)) %>%
      drop_na()
    
    if (nrow(test_data) < 10 || n_distinct(test_data[[group]]) < 2) next
    
    summary_table <- test_data %>%
      group_by(across(all_of(group))) %>%
      summarise(
        n = n(),
        mean = mean(.data[[qol]], na.rm = TRUE),
        median = median(.data[[qol]], na.rm = TRUE),
        sd = sd(.data[[qol]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(QoL_variable = qol, Grouping_variable = group)
    
    test_result <- wilcox.test(as.formula(paste(qol, "~", group)), data = test_data)
    
    summary_table$p_value <- test_result$p.value
    
    summary_results[[paste(group, qol, sep = "_")]] <- summary_table
  }
}

final_qol_summary <- bind_rows(summary_results)
print(final_qol_summary)

normal_vars <- normality_results_df %>%
  mutate(Normal = ifelse(p_value > 0.05, TRUE, FALSE)) %>%
  select(Variable, Normal) %>%
  deframe()

adaptive_corr_results <- list()

for (qol in qol_vars) {
  for (var in vars_to_test) {
    data_pair <- cor_data[, c(qol, var)] |> na.omit()
    
    method <- ifelse(!is.na(normal_vars[[var]]) && normal_vars[[var]], "pearson", "spearman")
    
    test <- cor.test(data_pair[[1]], data_pair[[2]], method = method)
    
    adaptive_corr_results[[paste(qol, var, sep = "_vs_")]] <- data.frame(
      QoL = qol,
      Predictor = var,
      method = method,
      estimate = test$estimate,
      p_value = test$p.value
    )
  }
}

adaptive_corr_df <- do.call(rbind, adaptive_corr_results)
print(adaptive_corr_df)

filtered_corr_df <- adaptive_corr_df %>%
  filter(abs(estimate) > 0.2)

print(filtered_corr_df)
