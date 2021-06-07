library(haven)
# library(dplyr)
library(tidyverse)
library(randomForest)
library(ggplot2)
library(caret)

data_path = "data/Data"
T1_path = paste(data_path, "QoLT1", sep = "/")
T3_path = paste(data_path, "QoLT3", sep = "/")
T4_path = paste(data_path, "QoLT4", sep = "/")

# read baseline dataset ()
pathBaseline = file.path(data_path, "Cobra baseline.sav")
baseline = read_sav(pathBaseline)

# remove lines with age = NA
baseline = baseline[!is.na(baseline$ageatinclusion), ]

# read medical information dataset ()
pathMedical = file.path(data_path, "Medical data.sav")
medicalData = read_sav(pathMedical)

medicalData = medicalData %>% select ("ID", "Staging", "OK", "RT", "CT", "HT") %>% rename(Surgery = OK, Radiotherapy = RT, Chemotherapy = CT, Hormonetherapy = HT)
# rm data with no stage
medicalData = medicalData[!(is.na(medicalData$Staging) | medicalData$Staging=="" | medicalData$Staging>4), ]
medicalData[medicalData==""] = NA
#normalize fields of medical data
medicalData$Staging[medicalData$Staging=="1"] = 1/6
medicalData$Staging[medicalData$Staging=="2"] = 2/6
medicalData$Staging[medicalData$Staging=="3"] = 3/6
medicalData$Staging[medicalData$Staging=="3A"] = 3/6
medicalData$Staging[medicalData$Staging=="3B"] = 4/6
medicalData$Staging[medicalData$Staging=="3C"] = 5/6
medicalData$Staging[medicalData$Staging=="4"] = 6/6

medicalData = medicalData %>% mutate(Staging = as.double(Staging))

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

baseline = rename(baseline, age = ageatinclusion, height = Length_avg, fill_date = bdatalg, weight = bweight, weekly_alcohol = balcpw, drugs = drugs, regular_menstruation = bmenreg, pill = pill, times_pregnant = pregno, previous_hormon_treatment = bhormone, period_treatment = bhormdu)
baseline_dataset = baseline %>% select(ID, height, age, fill_date, weight, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, GROUP) %>% mutate("BMI" = (weight/((height/100)^2))) %>% select(ID, age, fill_date, BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, GROUP)

T3_hads_datainterest = T3_hads %>% select(RESPNO, DHADS1, DHADS3, DHADS5, DHADS13) %>% rename(t3_tense = DHADS1, t3_anxious = DHADS3, t3_worried = DHADS5, t3_panic = DHADS13)

dataset_with_selected_t3 = inner_join(baseline_dataset, T3_hads_datainterest, by=c("ID"="RESPNO"))

dataset_with_selected_t3 = mutate(dataset_with_selected_t3, t3_tense = t3_tense/3, t3_anxious = t3_anxious/3, t3_worried = t3_worried/3, t3_panic = t3_panic/3)

T4_hads_datainterest = T4_hads %>% select(RESPNO, EHADS1, EHADS3, EHADS5, EHADS13) %>% rename(t4_tense = EHADS1, t4_anxious = EHADS3, t4_worried = EHADS5, t4_panic = EHADS13)

dataset_t3_t4 = inner_join(dataset_with_selected_t3, T4_hads_datainterest, by=c("ID"="RESPNO"))

dataset_t3_t4 = mutate(dataset_t3_t4,t4_tense=t4_tense/3,  t4_anxious=t4_anxious/3, t4_worried=t4_worried/3, t4_panic=t4_panic/3)

all_t4_fatigues = rbind(T4_gq_pt, T4_gq_contr) %>% select("RESPNO", "EFAT1","EFAT2","EFAT3","EFAT4","EFAT5","EFAT6","EFAT7","EFAT8","EFAT9","EFAT10","EFAT11","EFAT12","EFAT13","EFAT14","EFAT15","EFAT16","EFAT17","EFAT18","EFAT19","EFAT20")

dataset_with_fatigues = inner_join(dataset_t3_t4, all_t4_fatigues, by=c("ID"="RESPNO"))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT1 = as.factor(EFAT1)) %>% mutate(EFAT1 = as.double(EFAT1)) # %>% mutate (EFAT1 = (EFAT1-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT2 = as.factor(EFAT2)) %>% mutate(EFAT2 = as.double(EFAT2)) # %>% mutate (EFAT2 = (EFAT2-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT3 = as.factor(EFAT3)) %>% mutate(EFAT3 = as.double(EFAT3)) # %>% mutate (EFAT3 = (EFAT3-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT4 = as.factor(EFAT4)) %>% mutate(EFAT4 = as.double(EFAT4)) # %>% mutate (EFAT4 = (EFAT4-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT5 = as.factor(EFAT5)) %>% mutate(EFAT5 = as.double(EFAT5)) # %>% mutate (EFAT5 = (EFAT5-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT6 = as.factor(EFAT6)) %>% mutate(EFAT6 = as.double(EFAT6)) # %>% mutate (EFAT6 = (EFAT6-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT7 = as.factor(EFAT7)) %>% mutate(EFAT7 = as.double(EFAT7)) # %>% mutate (EFAT7 = (EFAT7-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT8 = as.factor(EFAT8)) %>% mutate(EFAT8 = as.double(EFAT8)) # %>% mutate (EFAT8 = (EFAT8-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT9 = as.factor(EFAT9)) %>% mutate(EFAT9 = as.double(EFAT9)) # %>% mutate (EFAT9 = (EFAT9-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT10 = as.factor(EFAT10)) %>% mutate(EFAT10 = as.double(EFAT10)) # %>% mutate (EFAT10 = (EFAT10-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT11 = as.factor(EFAT11)) %>% mutate(EFAT11 = as.double(EFAT11)) # %>% mutate (EFAT11 = (EFAT11-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT12 = as.factor(EFAT12)) %>% mutate(EFAT12 = as.double(EFAT12)) # %>% mutate (EFAT12 = (EFAT12-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT13 = as.factor(EFAT13)) %>% mutate(EFAT13 = as.double(EFAT13)) # %>% mutate (EFAT13 = (EFAT13-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT14 = as.factor(EFAT14)) %>% mutate(EFAT14 = as.double(EFAT14)) # %>% mutate (EFAT14 = (EFAT14-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT15 = as.factor(EFAT15)) %>% mutate(EFAT15 = as.double(EFAT15)) # %>% mutate (EFAT15 = (EFAT15-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT16 = as.factor(EFAT16)) %>% mutate(EFAT16 = as.double(EFAT16)) # %>% mutate (EFAT16 = (EFAT16-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT17 = as.factor(EFAT17)) %>% mutate(EFAT17 = as.double(EFAT17)) # %>% mutate (EFAT17 = (EFAT17-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT18 = as.factor(EFAT18)) %>% mutate(EFAT18 = as.double(EFAT18)) # %>% mutate (EFAT18 = (EFAT18-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT19 = as.factor(EFAT19)) %>% mutate(EFAT19 = as.double(EFAT19)) # %>% mutate (EFAT19 = (EFAT19-1)/4)
dataset_with_fatigues = dataset_with_fatigues %>% mutate(EFAT20 = as.factor(EFAT20)) %>% mutate(EFAT20 = as.double(EFAT20)) # %>% mutate (EFAT20 = (EFAT20-1)/4)

dataset_with_fatigues = dataset_with_fatigues %>% mutate(age = as.integer(age))

#full join dataset_with_fatigues and medicaldata
dataset_with_fatigues = left_join(dataset_with_fatigues, medicalData, by=c("ID"="ID"))

#change Ja/Nee with booleans and for people not in medical data, set NAs
dataset_with_fatigues = dataset_with_fatigues %>% mutate(Staging = as.double(Staging)) %>% mutate(Staging = if_else(is.na(Staging), 0.0, Staging))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(Surgery = if_else(is.na(Surgery), 'Nee', Surgery)) %>% mutate(Surgery = if_else(Surgery == 'Ja', TRUE, FALSE))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(Radiotherapy = if_else(is.na(Radiotherapy), 'Nee', Radiotherapy)) %>% mutate(Radiotherapy = if_else(Radiotherapy == 'Ja', TRUE, FALSE))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(Chemotherapy = if_else(is.na(Chemotherapy), 'Nee', Chemotherapy)) %>% mutate(Chemotherapy = if_else(Chemotherapy == 'Ja', TRUE, FALSE))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(Hormonetherapy = if_else(is.na(Hormonetherapy), 'Nee', Hormonetherapy)) %>% mutate(Hormonetherapy = if_else(Hormonetherapy == 'Ja', TRUE, FALSE))


dataset_with_fatigues$period_treatment = NULL
dataset_with_fatigues$fill_date = NULL
#assumption if weekly alchohol is NA, the subject has a consumption equal to 0
#assumption if regular menstruation field is NA, the subject has regular menstruation
dataset_with_fatigues = dataset_with_fatigues %>% mutate(weekly_alcohol = if_else(is.na(weekly_alcohol), 0, weekly_alcohol))
dataset_with_fatigues = dataset_with_fatigues %>% mutate(regular_menstruation = if_else(regular_menstruation == 1, TRUE, FALSE)) %>% mutate(regular_menstruation = if_else(is.na(regular_menstruation),TRUE, regular_menstruation))

#assumption: if drugs is NA, we assume the subject takes no drug
dataset_with_fatigues = dataset_with_fatigues %>% mutate(drugs = if_else(drugs == 1, TRUE, FALSE)) %>% mutate(drugs = if_else(is.na(drugs),FALSE, drugs))

#assumption: if pill is NA, we assume the subject takes no pill
dataset_with_fatigues = dataset_with_fatigues %>% mutate(pill = if_else(pill == 1, TRUE, FALSE)) %>% mutate(pill = if_else(is.na(pill),FALSE, pill))


#assumption: if times_pregnant is NA, we assume the subject was never pregnant
dataset_with_fatigues = dataset_with_fatigues %>% mutate(times_pregnant = if_else(is.na(times_pregnant),0, times_pregnant))

#assumption: if previous_hormon_treatment is NA, we assume the subject did not have any previous_hormon_treatment
dataset_with_fatigues = dataset_with_fatigues %>% mutate(previous_hormon_treatment = if_else(previous_hormon_treatment == 1, TRUE, FALSE)) %>% mutate(previous_hormon_treatment = if_else(is.na(previous_hormon_treatment),FALSE, previous_hormon_treatment))

#replace remaining NAs with 0

dataset_with_fatigues[is.na(dataset_with_fatigues)] = 0


dataset_model_1 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT1)
dataset_model_2 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT2)
dataset_model_3 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT3)
dataset_model_4 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT4)
dataset_model_5 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT5)
dataset_model_6 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT6)
dataset_model_7 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT7)
dataset_model_8 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT8)
dataset_model_9 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT9)
dataset_model_10 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT10)
dataset_model_11 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT11)
dataset_model_12 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT12)
dataset_model_13 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT13)
dataset_model_14 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT14)
dataset_model_15 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT15)
dataset_model_16 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT16)
dataset_model_17 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT17)
dataset_model_18 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT18)
dataset_model_19 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT19)
dataset_model_20 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t3_tense, t3_anxious, t3_worried, t3_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, EFAT20)



#Building the model 1

train_lines = 232
test_lines = length(dataset_model_1$BMI) - train_lines

train_m1 = dataset_model_1 %>% slice_head(n=train_lines)
test_m1 = dataset_model_1 %>% slice_tail(n=test_lines)

model_1 = randomForest(EFAT1~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m1)
featureImportance1 = varImp(model_1)
plotModel1 = varImpPlot(model_1,type=2)

predict_y_m1 = predict(model_1, test_m1)

#Building the model 2

train_m2 = dataset_model_2 %>% slice_head(n=train_lines)
test_m2 = dataset_model_2 %>% slice_tail(n=test_lines)

model_2 = randomForest(EFAT2~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m2)
featureImportance2 = varImp(model_2)
plotModel2 = varImpPlot(model_2,type=2)

predict_y_m2 = predict(model_2, test_m2)

#Building the model 3

train_m3 = dataset_model_3 %>% slice_head(n=train_lines)
test_m3 = dataset_model_3 %>% slice_tail(n=test_lines)

model_3 = randomForest(EFAT3~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m3)
featureImportance3 = varImp(model_3)
plotModel3 = varImpPlot(model_3,type=2)

predict_y_m3 = predict(model_3, test_m3)

#Building the model 4

train_m4 = dataset_model_4 %>% slice_head(n=train_lines)
test_m4 = dataset_model_4 %>% slice_tail(n=test_lines)

model_4 = randomForest(EFAT4~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m4)
featureImportance4 = varImp(model_4)
plotModel4 = varImpPlot(model_4,type=2)

predict_y_m4 = predict(model_4, test_m4)

#Building the model 5

train_m5 = dataset_model_5 %>% slice_head(n=train_lines)
test_m5 = dataset_model_5 %>% slice_tail(n=test_lines)

model_5 = randomForest(EFAT5~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m5)
featureImportance5 = varImp(model_5)
plotModel5 = varImpPlot(model_5,type=2)

predict_y_m5 = predict(model_5, test_m5)

#Building the model 6

train_m6 = dataset_model_6 %>% slice_head(n=train_lines)
test_m6 = dataset_model_6 %>% slice_tail(n=test_lines)

model_6 = randomForest(EFAT6~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m6)
featureImportance6 = varImp(model_6)
plotModel6 = varImpPlot(model_6,type=2)

predict_y_m6 = predict(model_6, test_m6)

#Building the model 7

train_m7 = dataset_model_7 %>% slice_head(n=train_lines)
test_m7 = dataset_model_7 %>% slice_tail(n=test_lines)

model_7 = randomForest(EFAT7~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m7)
featureImportance7 = varImp(model_7)
plotModel7 = varImpPlot(model_7,type=2)

predict_y_m7 = predict(model_7, test_m7)

#Building the model 8

train_m8 = dataset_model_8 %>% slice_head(n=train_lines)
test_m8 = dataset_model_8 %>% slice_tail(n=test_lines)

model_8 = randomForest(EFAT8~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m8)
featureImportance8 = varImp(model_8)
plotModel8 = varImpPlot(model_8,type=2)

predict_y_m8 = predict(model_8, test_m8)

#Building the model 9

train_m9 = dataset_model_9 %>% slice_head(n=train_lines)
test_m9 = dataset_model_9 %>% slice_tail(n=test_lines)

model_9 = randomForest(EFAT9~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m9)
featureImportance9 = varImp(model_9)
plotModel9 = varImpPlot(model_9,type=2)

predict_y_m9 = predict(model_9, test_m9)

#Building the model 10

train_m10 = dataset_model_10 %>% slice_head(n=train_lines)
test_m10 = dataset_model_10 %>% slice_tail(n=test_lines)

model_10 = randomForest(EFAT10~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m10)
featureImportance10 = varImp(model_10)
plotModel10 = varImpPlot(model_10,type=2)

predict_y_m10 = predict(model_10, test_m10)

#Building the model 11

train_m11 = dataset_model_11 %>% slice_head(n=train_lines)
test_m11 = dataset_model_11 %>% slice_tail(n=test_lines)

model_11 = randomForest(EFAT11~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m11)
featureImportance11 = varImp(model_11)
plotModel11 = varImpPlot(model_11,type=2)

predict_y_m11 = predict(model_11, test_m11)

#Building the model 12

train_m12 = dataset_model_12 %>% slice_head(n=train_lines)
test_m12 = dataset_model_12 %>% slice_tail(n=test_lines)

model_12 = randomForest(EFAT12~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m12)
featureImportance12 = varImp(model_12)
plotModel12 = varImpPlot(model_12,type=2)

predict_y_m12 = predict(model_12, test_m12)

#Building the model 13

train_m13 = dataset_model_13 %>% slice_head(n=train_lines)
test_m13 = dataset_model_13 %>% slice_tail(n=test_lines)

model_13 = randomForest(EFAT13~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m13)
featureImportance13 = varImp(model_13)
plotModel13 = varImpPlot(model_13,type=2)

predict_y_m13 = predict(model_13, test_m13)

#Building the model 14

train_m14 = dataset_model_14 %>% slice_head(n=train_lines)
test_m14 = dataset_model_14 %>% slice_tail(n=test_lines)

model_14 = randomForest(EFAT14~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m14)
featureImportance14 = varImp(model_14)
plotModel14 = varImpPlot(model_14,type=2)

predict_y_m14 = predict(model_14, test_m14)

#Building the model 15

train_m15 = dataset_model_15 %>% slice_head(n=train_lines)
test_m15 = dataset_model_15 %>% slice_tail(n=test_lines)

model_15 = randomForest(EFAT15~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m15)
featureImportance15 = varImp(model_15)
plotModel15 = varImpPlot(model_15,type=2)

predict_y_m15 = predict(model_15, test_m15)

#Building the model 16

train_m16 = dataset_model_16 %>% slice_head(n=train_lines)
test_m16 = dataset_model_16 %>% slice_tail(n=test_lines)

model_16 = randomForest(EFAT16~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m16)
featureImportance16 = varImp(model_16)
plotModel16 = varImpPlot(model_16,type=2)

predict_y_m16 = predict(model_16, test_m16)

#Building the model 17

train_m17 = dataset_model_17 %>% slice_head(n=train_lines)
test_m17 = dataset_model_17 %>% slice_tail(n=test_lines)

model_17 = randomForest(EFAT17~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m17)
featureImportance17 = varImp(model_17)
plotModel17 = varImpPlot(model_17,type=2)

predict_y_m17 = predict(model_17, test_m17)

#Building the model 18

train_m18 = dataset_model_18 %>% slice_head(n=train_lines)
test_m18 = dataset_model_18 %>% slice_tail(n=test_lines)

model_18 = randomForest(EFAT18~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m18)
featureImportance18 = varImp(model_18)
plotModel18 = varImpPlot(model_18,type=2)

predict_y_m18 = predict(model_18, test_m18)

#Building the model 19

train_m19 = dataset_model_19 %>% slice_head(n=train_lines)
test_m19 = dataset_model_19 %>% slice_tail(n=test_lines)

model_19 = randomForest(EFAT19~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m19)
featureImportance19 = varImp(model_19)
plotModel19 = varImpPlot(model_19,type=2)

predict_y_m19 = predict(model_19, test_m19)

#Building the model 20

train_m20 = dataset_model_20 %>% slice_head(n=train_lines)
test_m20 = dataset_model_20 %>% slice_tail(n=test_lines)

model_20 = randomForest(EFAT20~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t3_tense+t3_anxious+t3_worried+t3_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy, data=train_m20)
featureImportance20 = varImp(model_20)
plotModel20 = varImpPlot(model_20,type=2)

predict_y_m20 = predict(model_20, test_m20)

# sum = 0.0

#MSE for EFAT1
# for (index in seq(1, length(predict_y_m1))) {
  # print(index)
  # sum = as.double(sum) + as.double(abs(predict_y_m1[index] - as.double(test_m1$EFAT1[index])))
# }

#Compute General Fatigue prediction and Average Error

GeneralFatigueReal = predict_y_m1
GeneralFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  GeneralFatigueReal[index] = test_m1$EFAT1[index] + (6 - test_m5$EFAT5[index]) + test_m12$EFAT12[index] + (6 - test_m16$EFAT16[index])
  GeneralFatiguePrediction[index] = predict_y_m1[index] + (6 - predict_y_m5[index]) + predict_y_m12[index] + (6 - predict_y_m16[index])
}

sum = 0.0
error_GF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index]))
  error_GF[index] = as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index]))
}
average_error_GF = sum/(length(predict_y_m1))

#Compute Physical Fatigue prediction and Average Error

PhysicalFatigueReal = predict_y_m1
PhysicalFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  PhysicalFatigueReal[index] = (6 - test_m2$EFAT2[index]) + test_m8$EFAT8[index] + (6 - test_m14$EFAT14[index]) + test_m20$EFAT20[index]
  PhysicalFatiguePrediction[index] = (6 - predict_y_m2[index]) + predict_y_m8[index] + (6 - predict_y_m14[index]) + predict_y_m20[index]
}

sum = 0.0
error_PF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index]))
  error_PF[index] = as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index]))
}
average_error_PF = sum/(length(predict_y_m1))

#Compute Reduced Activity prediction and Average Error

ReducedActivityReal = predict_y_m1
ReducedActivityPrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  ReducedActivityReal[index] = test_m3$EFAT3[index] + test_m6$EFAT6[index] + (6 - test_m10$EFAT10[index]) + (6 - test_m17$EFAT17[index])
  ReducedActivityPrediction[index] = predict_y_m3[index] + predict_y_m6[index] + (6 - predict_y_m10[index]) + (6 - predict_y_m17[index])
}

sum = 0.0
error_RA = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index]))
  error_RA[index] = as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index]))
}
average_error_RA = sum/(length(predict_y_m1))

#Compute Reduced Motivation prediction and Average Error

ReducedMotivationReal = predict_y_m1
ReducedMotivationPrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  ReducedMotivationReal[index] = test_m4$EFAT4[index] + (6 - test_m9$EFAT9[index]) + test_m15$EFAT15[index] + (6 - test_m18$EFAT18[index])
  ReducedMotivationPrediction[index] = predict_y_m4[index] + (6 - predict_y_m9[index]) + predict_y_m15[index] + (6 - predict_y_m18[index])
}

sum = 0.0
error_RM = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index]))
  error_RM[index] = as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index]))
}
average_error_RM = sum/(length(predict_y_m1))

#Compute Mental Fatigue prediction and Average Error

MentalFatigueReal = predict_y_m1
MentalFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  MentalFatigueReal[index] = test_m7$EFAT7[index] + test_m11$EFAT11[index] + (6 - test_m13$EFAT13[index]) + (6 - test_m19$EFAT19[index])
  MentalFatiguePrediction[index] = predict_y_m7[index] + predict_y_m11[index] + (6 - predict_y_m13[index]) + (6 - predict_y_m19[index])
}

sum = 0.0
error_MF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index]))
  error_MF[index] = as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index]))
}
average_error_MF = sum/(length(predict_y_m1))

#printing the average error

average_error_GF
average_error_MF
average_error_PF
average_error_RA
average_error_RM

#plotting the errors

error_GF_frame = data.frame(error_GF)
error_GF_frame = error_GF_frame %>% mutate(name = "GF") %>% rename("error" = "error_GF")

error_MF_frame = data.frame(error_MF)
error_MF_frame = error_MF_frame %>% mutate(name = "MF") %>% rename("error" = "error_MF")

error_PF_frame = data.frame(error_PF)
error_PF_frame = error_PF_frame %>% mutate(name = "PF") %>% rename("error" = "error_PF")

error_RM_frame = data.frame(error_RM)
error_RM_frame = error_RM_frame %>% mutate(name = "RM") %>% rename("error" = "error_RM")

error_RA_frame = data.frame(error_RA)
error_RA_frame = error_RA_frame %>% mutate(name = "RA") %>% rename("error" = "error_RA")

errorFrame = rbind(error_GF_frame, error_PF_frame, error_RA_frame, error_RM_frame, error_MF_frame)

errorFrame = errorFrame %>% mutate(error = as.double(error))

errorPlot = ggplot(errorFrame, aes(x=name, y=error)) + geom_boxplot()

#plotting the importance of each feature
avgImportance = c()
length(featureImportance1$Overall)
for (index in seq(1, length(featureImportance1$Overall))) {
  avgImportance[index] = as.double(featureImportance1$Overall[index] + featureImportance2$Overall[index] + featureImportance3$Overall[index] + featureImportance4$Overall[index] +
    featureImportance5$Overall[index] + featureImportance6$Overall[index] + featureImportance7$Overall[index] + 
    featureImportance8$Overall[index] + featureImportance9$Overall[index] + featureImportance10$Overall[index] + 
    featureImportance11$Overall[index] + featureImportance12$Overall[index] + featureImportance13$Overall[index] + 
    featureImportance14$Overall[index] + featureImportance15$Overall[index] + featureImportance16$Overall[index] + 
    featureImportance17$Overall[index] + featureImportance18$Overall[index] + featureImportance19$Overall[index] + 
    featureImportance20$Overall[index])/20
}

avgImportance_Frame = data.frame(avgImportance) 

avgImportance_Frame = avgImportance_Frame %>% mutate(feature = "X")

avgImportance_Frame$feature[1] = "XBMI"
avgImportance_Frame$feature[2] = "XAge"
avgImportance_Frame$feature[3] = "XWA"
avgImportance_Frame$feature[4] = "XTP"
avgImportance_Frame$feature[5] = "XTen"
avgImportance_Frame$feature[6] = "XWorr"
avgImportance_Frame$feature[7] = "XChem"
avgImportance_Frame$feature[8] = "XSurg"
avgImportance_Frame$feature[9] = "XHorm"
avgImportance_Frame$feature[10] = "XPan"
avgImportance_Frame$feature[11] = "XDrug"
avgImportance_Frame$feature[12] = "XRad"
avgImportance_Frame$feature[13] = "XRM"
avgImportance_Frame$feature[14] = "XAnx"
avgImportance_Frame$feature[15] = "XPill"
avgImportance_Frame$feature[16] = "XPHTBC"

ggplot(avgImportance_Frame, aes(x=avgImportance, y=feature)) + geom_point()
