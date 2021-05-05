library(haven)
# library(dplyr)
library(tidyverse)

data_path = "data/Data"
T1_path = paste(data_path, "QoLT1", sep = "/")
T3_path = paste(data_path, "QoLT3", sep = "/")
T4_path = paste(data_path, "QoLT4", sep = "/")

# read baseline dataset ()
pathBaseline = file.path(data_path, "Cobra baseline EMPTY.sav")
baseline = read_sav(pathBaseline)

# read datasets from T1
path_T1_hads = file.path(T1_path, "T1_hads.sav")
path_T1_hads_2 = file.path(T1_path, "T1_hads_2.sav")
path_T1_qol_contr = file.path(T1_path, "T1_qol_contr.sav")
path_T1_qol_contr_2 = file.path(T1_path, "T1_qol_contr_2.sav")
path_T1_qol_pt = file.path(T1_path, "T1_qol_pt.sav")
path_T1_qol_pt_2 = file.path(T1_path, "T1_qol_pt_2.sav")

T1_hads = read_sav(path_T1_hads)
T1_hads_2 = read_sav(path_T1_hads_2)
T1_qol_contr = read_sav(path_T1_qol_contr)
T1_qol_contr_2 = read_sav(path_T1_qol_contr_2)
T1_qol_pt = read_sav(path_T1_qol_pt)
T1_qol_pt_2 = read_sav(path_T1_qol_pt_2)


# read datasets from T3
path_T3_gq_contr = file.path(T3_path, "T3_gq_contr.sav")
path_T3_gq_pt = file.path(T3_path, "T3_gq_pt.sav")
path_T3_hads = file.path(T3_path, "T3_hads.sav")
path_T3_qol_contr = file.path(T3_path, "T3_qol_contr.sav")
path_T3_qol_pt = file.path(T3_path, "T3_qol_pt.sav")
path_T3_symptoms = file.path(T3_path, "T3_symptoms.sav")

T3_gq_contr = read_sav(path_T3_gq_contr)
T3_gq_pt = read_sav(path_T3_gq_pt)
T3_hads = read_sav(path_T3_hads)
T3_qol_contr = read_sav(path_T3_qol_contr)
T3_qol_pt = read_sav(path_T3_qol_pt)
T3_symptoms = read_sav(path_T3_symptoms)


# read datasets from T4
path_T4_gq_contr = file.path(T4_path, "T4_gq_contr.sav")
path_T4_gq_pt = file.path(T4_path, "T4_gq_pt.sav")
path_T4_hads = file.path(T4_path, "T4_hads.sav")
path_T4_qol_contr = file.path(T4_path, "T4_qol_contr.sav")
path_T4_qol_pt = file.path(T4_path, "T4_qol_pt.sav")

T4_gq_contr = read_sav(path_T4_gq_contr)
T4_gq_pt = read_sav(path_T4_gq_pt)
T4_hads = read_sav(path_T4_hads)
T4_qol_contr = read_sav(path_T4_qol_contr)
T4_qol_pt = read_sav(path_T4_qol_pt)

# translate useful columns to English

baseline = rename(baseline, fill_date = bdatalg, weight = bweight, weekly_alcohol = balcpw, drugs = drugs, regular_menstruation = bmenreg, pill = pill, times_pregnant = pregno, previous_hormon_treatment = bhormone, period_treatment = bhormdu)
baseline_dataset = baseline %>% select(fill_date, weight, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment)


