# Pacotes -------------------------------------------------------------------------------------
library(dplyr)

# Lendo a base --------------------------------------------------------------------------------

df <- readxl::read_excel("crude dataset/Dados_atualizados_Saulo 10.12.xls")

glimpse(df)

# Ajustando a base ----------------------------------------------------------------------------

df_ajustada <- df |> janitor::clean_names()|> # Arrumando nome dos vetores
  select( #tirando variaveis repetidas ou com pouca chance de serem preditivas (minha perspectiva!)
    -record_id,
    -rghc,
    -bmi_admission_2,
    -dialysis_10,
    -time_dialysis,
    -tept_45_persistent_fup_incident,
    -race_fup,
    -education_degree_45,
    -abep_score,
    -abep_class,
    -charlson_score,
    -cvf_basal,
    -cvf_vef1_basal,
    -vef_basal,
    -pefr_basal,
    -fef_basal,
    -bmi_fup,
    -education_degree_62,
    -time_enferm,
    -time_enferm_2,
    -sao2_fup,
    -dispnea_scale_1,
    -facit_score,
    -eq5d_q06,
    -concentration_cis_r_persistent_fup_incident,
    -depressao_total,
    -ansiedade_total,
    -icu_care_76,
    -los_total_77,
    -dialysis_78,
    -intubation_79,
    -score_isi,
    -skin_problem_persistent_fup
  ) |> # Arrumando as variáveis
  mutate(age = as.numeric(age)) |>  # arrumando age
  mutate(age = case_when(age >= 60 ~ "idoso",
                         age < 60 ~ "nao-idoso"),
         sex = case_when(sex == 0 ~ "female", # arrumando sex
                         sex == 1 ~ "male"),
         icu_care_6 = case_when(icu_care_6 == 0 ~ "No", # icu_care_6
                                icu_care_6 == 1 ~ "Yes",
                                icu_care_6 == "NA" ~ "sem info"),
         icu_los_2 = case_when(icu_los_2 > 15 ~ "> 15 dias", # tempo de internação
                               icu_los_2 <= 15 ~ "ate 15 dias",
                               is.na(icu_los_2) ~ "não foi para UTI"),
         intubation_8 = case_when(intubation_8 == 0 ~ "No", # foi intubado
                                  intubation_8 == 1 ~ "Yes"),
         time_intubation = case_when(time_intubation > 15 ~ "> 15 dias", # tempo de intubação
                                     time_intubation <= 15 ~ "ate 15 dias",
                                     is.na(time_intubation) ~ "Não fez uso de VM"),
         dyspnea_persistent_fup_incident = case_when(dyspnea_persistent_fup_incident == 0 ~ "No", # dyspnea_persistent_fup_incident
                                                     dyspnea_persistent_fup_incident == 1 ~ "Yes"),
         cough_persistent_fup = case_when(cough_persistent_fup == 0 ~ "No", # cough_persistent_fup
                                          cough_persistent_fup == 1 ~ "Yes"),
         weakness_persistent_fup_correto = case_when(weakness_persistent_fup_correto == 0 ~ "No", # weakness_persistent_fup_correto
                                                     weakness_persistent_fup_correto == 1 ~ "Yes"),
         walk_impairment_persistent_fup_correto = case_when(walk_impairment_persistent_fup_correto == 0 ~ "No", # walk_impairment_persistent_fup_correto
                                                            walk_impairment_persistent_fup_correto == 1 ~ "Yes"),
         chest_pain_persistent_fup = case_when(chest_pain_persistent_fup == 0 ~ "No", # chest_pain_persistent_fup
                                               chest_pain_persistent_fup == 1 ~ "Yes"),
         joint_pain_persistent_fup_severe = case_when(joint_pain_persistent_fup_severe == 0 ~ "No", # joint_pain_persistent_fup_severe
                                                      joint_pain_persistent_fup_severe == 1 ~ "Yes"),
         abdominal_symptoms_persistent_fup = case_when(abdominal_symptoms_persistent_fup == 0 ~ "No", # abdominal_symptoms_persistent_fup
                                                       abdominal_symptoms_persistent_fup == 1 ~ "Yes"),
         diarrhea_persistent_fup = case_when(diarrhea_persistent_fup == 0 ~ "No", # diarrhea_persistent_fup
                                             diarrhea_persistent_fup == 1 ~ "Yes"),
         nausea_vomiting_persistent_fup = case_when(nausea_vomiting_persistent_fup == 0 ~ "No", # nausea_vomiting_persistent_fup
                                                    nausea_vomiting_persistent_fup == 1 ~ "Yes"),
         loss_consciousness_presence_post_covid_incident = case_when(loss_consciousness_presence_post_covid_incident == 0 ~ "No", # loss_consciousness_presence_post_covid_incident
                                                                     loss_consciousness_presence_post_covid_incident == 1 ~ "Yes"),
         loss_appetite_persistent_fup_incident = case_when(loss_appetite_persistent_fup_incident == 0 ~ "No", # loss_appetite_persistent_fup_incident
                                                           loss_appetite_persistent_fup_incident == 1 ~ "Yes"),
         parestesia_persistent_fup_incident_correto  = case_when(parestesia_persistent_fup_incident_correto  == 0 ~ "No", # parestesia_persistent_fup_incident_correto
                                                                 parestesia_persistent_fup_incident_correto  == 1 ~ "Yes"),
         nasal_obstruction_persistent_fup_incident = case_when(nasal_obstruction_persistent_fup_incident == 0 ~ "No", # nasal_obstruction_persistent_fup_incident
                                                               nasal_obstruction_persistent_fup_incident == 1 ~ "Yes"),
         taste_persistent_fup_incident = case_when(taste_persistent_fup_incident == 0 ~ "No", # taste_persistent_fup_incident
                                                   taste_persistent_fup_incident == 1 ~ "Yes"),
         smell_persistent_fup_incident = case_when(smell_persistent_fup_incident == 0 ~ "No", # smell_persistent_fup_incident
                                                   smell_persistent_fup_incident == 1 ~ "Yes"),
         fatigue_persistent_fup_incident = case_when(fatigue_persistent_fup_incident == 0 ~ "No", # fatigue_persistent_fup_incident
                                                     fatigue_persistent_fup_incident == 1 ~ "Yes"),
         headaches_persistent_fup_incident_correto = case_when(headaches_persistent_fup_incident_correto == 0 ~ "No", # headaches_persistent_fup_incident_correto
                                                               headaches_persistent_fup_incident_correto == 1 ~ "Yes"),
         trail_making_a_persistent_fup_incident = case_when(trail_making_a_persistent_fup_incident == 0 ~ "No", # trail_making_a_persistent_fup_incident
                                                            trail_making_a_persistent_fup_incident == 1 ~ "Yes"),
         memory_persistent_fup_incident = case_when(memory_persistent_fup_incident == 0 ~ "No", # memory_persistent_fup_incident
                                                    memory_persistent_fup_incident == 1 ~ "Yes"),
         tept_30_persistent_fup_incident = case_when(tept_30_persistent_fup_incident == 0 ~ "No", # tept_30_persistent_fup_incident
                                                     tept_30_persistent_fup_incident == 1 ~ "Yes"),
         hearing_loss_persistent_fup_incident = case_when(hearing_loss_persistent_fup_incident == 0 ~ "No", # hearing_loss_persistent_fup_incident
                                                          hearing_loss_persistent_fup_incident == 1 ~ "Yes"),
         tinnitus_persistent_fup_incident = case_when(tinnitus_persistent_fup_incident == 0 ~ "No", # tinnitus_persistent_fup_incident
                                                      tinnitus_persistent_fup_incident == 1 ~ "Yes"),
         dizziness_persistent_fup_incident = case_when(dizziness_persistent_fup_incident == 0 ~ "No", # dizziness_persistent_fup_incident
                                                       dizziness_persistent_fup_incident == 1 ~ "Yes"),
         edema_persistent_fup_incident = case_when(edema_persistent_fup_incident == 0 ~ "No", # edema_persistent_fup_incident
                                                   edema_persistent_fup_incident == 1 ~ "Yes"),
         nocturia_persistent_fup_incident = case_when(nocturia_persistent_fup_incident == 0 ~ "No", # nocturia_persistent_fup_incident
                                                      nocturia_persistent_fup_incident == 1 ~ "Yes"),
         body_pain_persistent_fup = case_when(body_pain_persistent_fup == 0 ~ "No", # body_pain_persistent_fup
                                              body_pain_persistent_fup == 1 ~ "Yes"),
         insomnia_persistent_fup_incident = case_when(insomnia_persistent_fup_incident == 0 ~ "No", # insomnia_persistent_fup_incident
                                                      insomnia_persistent_fup_incident == 1 ~ "Yes"),
         depression_persistent_fup_incident = case_when(depression_persistent_fup_incident == 0 ~ "No", # depression_persistent_fup_incident
                                                        depression_persistent_fup_incident == 1 ~ "Yes"),
         anxiety_persistent_fup_incident = case_when(anxiety_persistent_fup_incident == 0 ~ "No", # anxiety_persistent_fup_incident
                                                     anxiety_persistent_fup_incident == 1 ~ "Yes"),
         los_total_49 = case_when(los_total_49 > 15 ~ "> 15 dias", # los_total_49
                                  los_total_49 <= 15 ~ "ate 15 dias"),
         fef_predito = as.numeric(fef_predito),
         smoking_history = case_when(smoking_history == 0 ~ "No", # smoking_history
                                     smoking_history == 1 ~ "Yes"),
         obesity = case_when(obesity == 0 ~ "No", # obesity
                             obesity == 1 ~ "Yes"),
         hypertension = case_when(hypertension == 0 ~ "No", # hypertension
                                  hypertension == 1 ~ "Yes"),
         diabetes = case_when(diabetes == 0 ~ "No", # icu_care_6
                              diabetes == 1 ~ "Yes"),
         who_severity_class = case_when(who_severity_class == 1 ~ "baixa",
                                        who_severity_class == 2 ~ "moderada",
                                        who_severity_class == 3 ~ "elevada",
                                        who_severity_class == 4 ~ "critico"),
         ipaq_classification = case_when(ipaq_classification < 3 ~ "inativo",
                                         ipaq_classification >= 3 ~ "ativo")) |>
  rename(idade = age, # ajustando os nomes da variaveis
         sexo = sex,
         adm_uti = icu_care_6,
         tempo_uti = icu_los_2,
         vmi = intubation_8,
         tempo_vmi = time_intubation,
         dispneia = dyspnea_persistent_fup_incident,
         tosse = cough_persistent_fup,
         fraqueza = weakness_persistent_fup_correto,
         dific_caminhar = walk_impairment_persistent_fup_correto,
         angina = chest_pain_persistent_fup,
         dor_articular = joint_pain_persistent_fup_severe,
         dor_abdominal = abdominal_symptoms_persistent_fup,
         diarreia = diarrhea_persistent_fup,
         nausea = nausea_vomiting_persistent_fup,
         perda_consciencia = loss_consciousness_presence_post_covid_incident,
         inapetencia = loss_appetite_persistent_fup_incident,
         parestesia = parestesia_persistent_fup_incident_correto,
         obstrucao_nasal = nasal_obstruction_persistent_fup_incident,
         perda_paladar = taste_persistent_fup_incident,
         anosmia = smell_persistent_fup_incident,
         fadiga = fatigue_persistent_fup_incident,
         cefaleia = headaches_persistent_fup_incident_correto,
         perda_memoria = memory_persistent_fup_incident,
         TEPT = tept_30_persistent_fup_incident,
         perda_auditiva = hearing_loss_persistent_fup_incident,
         ruido_ouvido = tinnitus_persistent_fup_incident,
         vertigem = dizziness_persistent_fup_incident,
         edema = edema_persistent_fup_incident,
         nocturia = nocturia_persistent_fup_incident,
         dor_corpo = body_pain_persistent_fup,
         insonia = insomnia_persistent_fup_incident,
         depressao = depression_persistent_fup_incident,
         ansiedade = anxiety_persistent_fup_incident,
         tempo_internacao = los_total_49,
         espiro_cvf_pred = cvf_predito,
         espiro_cvf_vef1_pred = cvf_vef1_predito,
         espiro_vef_pred = vef_predito,
         espiro_pefr_pred = pefr_predito,
         espiro_fef_pred = fef_predito,
         tabagismo = smoking_history,
         obesidade = obesity,
         hipertensao = hypertension,
         gravidade_adm = who_severity_class,
         nivel_atf = ipaq_classification
         ) |>
  filter(!is.na(dispneia)) # tirando os NAs do desfecho

# Analise Descritivas -------------------------------------------------------------------------
skimr::skim(df_ajustada)

# Novo .csv -----------------------------------------------------------------------------------

readr::write_csv2(x = df_ajustada,
                  file = "adjusted dataset/df_ajustava.csv")
