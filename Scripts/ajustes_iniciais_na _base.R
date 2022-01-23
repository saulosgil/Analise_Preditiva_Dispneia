# Pacotes -------------------------------------------------------------------------------------
library(readxl)
library(dplyr)

# Lendo a base --------------------------------------------------------------------------------

df <- read_excel("crude dataset/Dados_atualizados_Saulo 10.12.xls")

glimpse(df)

skimr::skim(df)

# Ajustando a base ----------------------------------------------------------------------------

df_ajustada <- df |>
  janitor::clean_names() |>
  select(-record_id,
         -rghc) |>
  mutate(age = as.numeric(age),
         bmi_admission_2 = as.numeric(bmi_admission_2),
         cvf_basal = as.numeric(cvf_basal),
         cvf_vef1_basal = as.numeric(cvf_vef1_basal),
         vef_basal = as.numeric(vef_basal),
         pefr_basal = as.numeric(pefr_basal),
         fef_basal = as.numeric(fef_basal),
         fef_predito = as.numeric(fef_predito),
         bmi_fup = as.numeric(bmi_fup),
         sao2_fup = as.numeric(sao2_fup),
         facit_score = as.numeric(vef_basal)
         ) |>
  rename(dispneia = dyspnea_persistent_fup_incident)

glimpse(df_ajustada)

# Novo .csv -----------------------------------------------------------------------------------

readr::write_csv2(x = df_ajustada,
                  file = "adjusted dataset/df_ajustada.csv")
