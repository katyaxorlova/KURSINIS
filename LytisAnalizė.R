# Reikalingi paketai
library(dplyr)
library(readxl)

# Įkeliam duomenis
setwd("/Users/katyaorlova/Desktop/KURSINIS")
data <- read_excel("Data-RITS-GS.xlsx")

# Sukuriam kintamųjų sąrašą
DL <- LETTERS[4:12]
AM_AZ <- paste0("A", LETTERS[13:26])
DJ_DZ <- paste0("D", LETTERS[10:26])
columns_needed <- c(DL, "S", "T", AM_AZ, "BP", "BQ", "CV", "CW", DJ_DZ)

# Tik tie stulpeliai, kurie tikrai egzistuoja
columns_exist <- intersect(columns_needed, colnames(data))

# Švari versija be NA lyties
data_clean <- data %>% filter(!is.na(Lytis))

# Funkcija: atlikti Shapiro-Wilk normalumo testą kiekvienoje lyties grupėje
normalumo_testai <- lapply(columns_exist, function(var) {
  vyr <- shapiro.test(data_clean %>% filter(Lytis == "Vyras") %>% pull(!!sym(var)))
  mot <- shapiro.test(data_clean %>% filter(Lytis == "Moteris") %>% pull(!!sym(var)))
  
  data.frame(
    Kintamasis = var,
    P_vyras = vyr$p.value,
    P_moteris = mot$p.value,
    Normalus_vyras = ifelse(vyr$p.value > 0.05, "Taip", "Ne"),
    Normalus_moteris = ifelse(mot$p.value > 0.05, "Taip", "Ne")
  )
})

# Sujungiame į vieną lentelę
normalumo_rez <- do.call(rbind, normalumo_testai)

# Rezultatai
View(normalumo_rez)
