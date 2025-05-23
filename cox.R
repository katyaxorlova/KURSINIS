library(survival)
library(readxl)

# Įkeliam duomenis
df <- read_excel("Data-RITS-GS.xlsx")

# Automatiškai pervadinam kintamuosius
names(df) <- make.names(names(df))

# Atnaujintas sąrašas
variables <- c(
  "Covid19", "Lytis", "Nutukimas",
  "Ranka", "Amžius", "KMI", "SOFA_1", "SOFA_7", "APACHE_II", "NRS_2002", "Admission.Type",
  "DPV",
  "FK..pokytis_1.5", "FK..pokytis_1.7",
  "Skeletal_muscle_mass..pokytis_1.5", "Skeletal_muscle_mass..pokytis_1.7",
  "Viskas..pokytis_1.5", "Viskas..pokytis_1.7", "Pirmine_bakterine_infekcija",
  "Hospitaline_infekcija", "Sepsis", "Hospitaline_pneumonija", "Pneumonija",
  "Intraabdominaline", "STI", "Kraujas", "Meningitas", "Minkstieji_audiniai",
  "Zaizda", "Kaulai_sanariai", "CVK", "Kitos_infekcijos", "Reabilitacija"
)

# Tuščia lentelė rezultatams
rez <- data.frame(Kintamasis=character(), HR=numeric(), CI_lower=numeric(),
                  CI_upper=numeric(), p_reiksme=numeric(), stringsAsFactors = FALSE)

# Skaičiuojam Cox modelius
for (v in variables) {
  f <- as.formula(paste("Surv(Trukmė_hosp, Isgyveno_ligonine) ~", v))
  model <- tryCatch(coxph(f, data = df), error = function(e) NULL)
  
  if (!is.null(model)) {
    sm <- summary(model)
    hr <- sm$coef[1, "exp(coef)"]
    ci <- sm$conf.int[1, c("lower .95", "upper .95")]
    pval <- sm$coef[1, "Pr(>|z|)"]
    rez <- rbind(rez, data.frame(Kintamasis=v, HR=hr, CI_lower=ci[1], CI_upper=ci[2], p_reiksme=pval))
  }
}

# Peržiūra
print(rez)
