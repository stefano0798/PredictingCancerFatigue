library(haven)
# library(dplyr)
library(tidyverse)

data_path = "data/Data"
T1_path = paste(data_path, "QoLT1", sep = "/")
T3_path = paste(data_path, "QoLT3", sep = "/")
T4_path = paste(data_path, "QoLT4", sep = "/")

# read baseline dataset ()
pathBaseline = file.path(data_path, "Cobra baseline.sav")
baseline = read_sav(pathBaseline)

# read medical information dataset ()
pathMedical = file.path(data_path, "Medical data.sav")
medicalData = read_sav(pathMedical)

medicalData = medicalData %>% select ("ID", "Staging", "OK", "RT", "CT", "HT") %>% rename(Surgery = OK, Radiotherapy = RT, Chemotherapy = CT, Hormonetherapy = HT)
# rm data with no stage
medicalData = medicalData[!(is.na(medicalData$Staging) | medicalData$Staging=="" | medicalData$Staging>4), ]
medicalData[medicalData==""] = NA
#normalize fileds of medical data
medicalData$Staging[medicalData$Staging=="1"] = 1/6
medicalData$Staging[medicalData$Staging=="2"] = 2/6
medicalData$Staging[medicalData$Staging=="3"] = 3/6
medicalData$Staging[medicalData$Staging=="3A"] = 3/6
medicalData$Staging[medicalData$Staging=="3B"] = 4/6
medicalData$Staging[medicalData$Staging=="3C"] = 5/6
medicalData$Staging[medicalData$Staging=="4"] = 6/6

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

baseline = rename(baseline, height = Length_avg, fill_date = bdatalg, weight = bweight, weekly_alcohol = balcpw, drugs = drugs, regular_menstruation = bmenreg, pill = pill, times_pregnant = pregno, previous_hormon_treatment = bhormone, period_treatment = bhormdu)
baseline_dataset = baseline %>% select(ID, height, fill_date, weight, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment) %>% mutate("BMI" = (weight/((height/100)^2))) %>% select(ID, fill_date, BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment)

T3_hads_datainterest = T3_hads %>% select(RESPNO, DHADS1, DHADS3, DHADS5, DHADS13) %>% rename(t3_tense = DHADS1, t3_anxious = DHADS3, t3_worried = DHADS5, t3_panic = DHADS13)

dataset_with_selected_t3 = inner_join(baseline_dataset, T3_hads_datainterest, by=c("ID"="RESPNO"))

dataset_with_selected_t3 = mutate(dataset_with_selected_t3, t3_tense = t3_tense/3, t3_anxious = t3_anxious/3, t3_worried = t3_worried/3, t3_panic = t3_panic/3)

T4_hads_datainterest = T4_hads %>% select(RESPNO, EHADS1, EHADS3, EHADS5, EHADS13) %>% rename(t4_tense = EHADS1, t4_anxious = EHADS3, t4_worried = EHADS5, t4_panic = EHADS13)

dataset_t3_t4 = inner_join(dataset_with_selected_t3, T4_hads_datainterest, by=c("ID"="RESPNO"))

dataset_t3_t4 = mutate(dataset_t3_t4,t4_tense=t4_tense/3,  t4_anxious=t4_anxious/3, t4_worried=t4_worried/3, t4_panic=t4_panic/3)

all_t4_fatigues = rbind(T4_gq_pt, T4_gq_contr) %>% select("RESPNO", "EFAT1","EFAT2","EFAT3","EFAT4","EFAT5","EFAT6","EFAT7","EFAT8","EFAT9","EFAT10","EFAT11","EFAT12","EFAT13","EFAT14","EFAT15","EFAT16","EFAT17","EFAT18","EFAT19","EFAT20")

dataset_with_fatigues = inner_join(dataset_t3_t4, all_t4_fatigues, by=c("ID"="RESPNO"))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT1 = as.factor(EFAT1)) %>% mutate(EFAT1 = as.double(EFAT1)) %>% mutate (EFAT1 = (EFAT1-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT2 = as.factor(EFAT2)) %>% mutate(EFAT2 = as.double(EFAT2)) %>% mutate (EFAT2 = (EFAT2-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT3 = as.factor(EFAT3)) %>% mutate(EFAT3 = as.double(EFAT3)) %>% mutate (EFAT3 = (EFAT3-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT4 = as.factor(EFAT4)) %>% mutate(EFAT4 = as.double(EFAT4)) %>% mutate (EFAT4 = (EFAT4-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT5 = as.factor(EFAT5)) %>% mutate(EFAT5 = as.double(EFAT5)) %>% mutate (EFAT5 = (EFAT5-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT6 = as.factor(EFAT6)) %>% mutate(EFAT6 = as.double(EFAT6)) %>% mutate (EFAT6 = (EFAT6-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT7 = as.factor(EFAT7)) %>% mutate(EFAT7 = as.double(EFAT7)) %>% mutate (EFAT7 = (EFAT7-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT8 = as.factor(EFAT8)) %>% mutate(EFAT8 = as.double(EFAT8)) %>% mutate (EFAT8 = (EFAT8-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT9 = as.factor(EFAT9)) %>% mutate(EFAT9 = as.double(EFAT9)) %>% mutate (EFAT9 = (EFAT9-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT10 = as.factor(EFAT10)) %>% mutate(EFAT10 = as.double(EFAT10)) %>% mutate (EFAT10 = (EFAT10-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT11 = as.factor(EFAT11)) %>% mutate(EFAT11 = as.double(EFAT11)) %>% mutate (EFAT11 = (EFAT11-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT12 = as.factor(EFAT12)) %>% mutate(EFAT12 = as.double(EFAT12)) %>% mutate (EFAT12 = (EFAT12-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT13 = as.factor(EFAT13)) %>% mutate(EFAT13 = as.double(EFAT13)) %>% mutate (EFAT13 = (EFAT13-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT14 = as.factor(EFAT14)) %>% mutate(EFAT14 = as.double(EFAT14)) %>% mutate (EFAT14 = (EFAT14-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT15 = as.factor(EFAT15)) %>% mutate(EFAT15 = as.double(EFAT15)) %>% mutate (EFAT15 = (EFAT15-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT16 = as.factor(EFAT16)) %>% mutate(EFAT16 = as.double(EFAT16)) %>% mutate (EFAT16 = (EFAT16-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT17 = as.factor(EFAT17)) %>% mutate(EFAT17 = as.double(EFAT17)) %>% mutate (EFAT17 = (EFAT17-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT18 = as.factor(EFAT18)) %>% mutate(EFAT18 = as.double(EFAT18)) %>% mutate (EFAT18 = (EFAT18-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT19 = as.factor(EFAT19)) %>% mutate(EFAT19 = as.double(EFAT19)) %>% mutate (EFAT19 = (EFAT19-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT20 = as.factor(EFAT20)) %>% mutate(EFAT20 = as.double(EFAT20)) %>% mutate (EFAT20 = (EFAT20-1)/4)

#full join dataset_with_fatigues and medicaldata
#change Ja/Nee with booleans
#keep groups 0/5
# for people not in medical data, set NAs

dataset_with_fatigues = full_join(dataset_with_fatigues, medicalData, )


dataset_model_1 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT1)
dataset_model_2 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT2)
dataset_model_3 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT3)
dataset_model_4 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT4)
dataset_model_5 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT5)
dataset_model_6 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT6)
dataset_model_7 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT7)
dataset_model_8 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT8)
dataset_model_9 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT9)
dataset_model_10 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT10)
dataset_model_11 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT11)
dataset_model_12 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT12)
dataset_model_13 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT13)
dataset_model_14 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT14)
dataset_model_15 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT15)
dataset_model_16 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT16)
dataset_model_17 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT17)
dataset_model_18 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT18)
dataset_model_19 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT19)
dataset_model_20 = dataset_with_fatigues %>% select(BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, EFAT20)



