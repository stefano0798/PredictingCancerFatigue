library(haven)
# library(dplyr)
library(tidyverse)
library(randomForest)
library(ggplot2)
library(caret)
library(data.table)
library(pdp)

data_path = "data/Data"
T1_path = paste(data_path, "QoLT1", sep = "/")
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

# translate useful columns to English, normalizing numeric data and changing the information format

baseline = rename(baseline, age = ageatinclusion, height = Length_avg, fill_date = bdatalg, weight = bweight, weekly_alcohol = balcpw, drugs = drugs, regular_menstruation = bmenreg, pill = pill, times_pregnant = pregno, previous_hormon_treatment = bhormone, period_treatment = bhormdu)
baseline_dataset = baseline %>% select(ID, height, age, fill_date, weight, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, GROUP) %>% mutate("BMI" = (weight/((height/100)^2))) %>% select(ID, age, fill_date, BMI, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, period_treatment, GROUP)

T1_hads = rbind(T1_hads, T1_hads_2)

T1_hads_datainterest = T1_hads %>% select(RESPNO, HADS1, HADS3, HADS5, HADS13) %>% rename(t1_tense = HADS1, t1_anxious = HADS3, t1_worried = HADS5, t1_panic = HADS13)

dataset_with_selected_t1 = inner_join(baseline_dataset, T1_hads_datainterest, by=c("ID"="RESPNO"))

dataset_with_selected_t1 = mutate(dataset_with_selected_t1, t1_tense = t1_tense/3, t1_anxious = t1_anxious/3, t1_worried = t1_worried/3, t1_panic = t1_panic/3)

T4_hads_datainterest = T4_hads %>% select(RESPNO, EHADS1, EHADS3, EHADS5, EHADS13) %>% rename(t4_tense = EHADS1, t4_anxious = EHADS3, t4_worried = EHADS5, t4_panic = EHADS13)

dataset_t1_t4 = inner_join(dataset_with_selected_t1, T4_hads_datainterest, by=c("ID"="RESPNO"))

dataset_t1_t4 = mutate(dataset_t1_t4,t4_tense=t4_tense/3,  t4_anxious=t4_anxious/3, t4_worried=t4_worried/3, t4_panic=t4_panic/3)

all_t4_fatigues = rbind(T4_gq_pt, T4_gq_contr) %>% select("RESPNO", "EFAT1","EFAT2","EFAT3","EFAT4","EFAT5","EFAT6","EFAT7","EFAT8","EFAT9","EFAT10","EFAT11","EFAT12","EFAT13","EFAT14","EFAT15","EFAT16","EFAT17","EFAT18","EFAT19","EFAT20")

dataset_with_fatigues = inner_join(dataset_t1_t4, all_t4_fatigues, by=c("ID"="RESPNO"))
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

# considering also baseline fatigue values
baseline_fatigues = baseline %>% select(ID, bfat1, fat2recoded, bfat3, bfat4, fat5recoded, bfat6, bfat7, bfat8, fat9recoded, fat10recoded, bfat11, bfat12, fat13recoded, fat14recoded, bfat15, fat16recoded, fat17recoded, fat18recoded, fat19recoded, bfat20)
baseline_fatigues = baseline_fatigues %>% rename(bas_fat1=bfat1, bas_fat2=fat2recoded, bas_fat3=bfat3, bas_fat4=bfat4, bas_fat5=fat5recoded, bas_fat6=bfat6, bas_fat7=bfat7, bas_fat8=bfat8, bas_fat9=fat9recoded, bas_fat10=fat10recoded,bas_fat11=bfat11, bas_fat12=bfat12, bas_fat13=fat13recoded, bas_fat14=fat14recoded, bas_fat15=bfat15, bas_fat16=fat16recoded, bas_fat17=fat17recoded, bas_fat18=fat18recoded, bas_fat19=fat19recoded, bas_fat20=bfat20)

# merging the dataset with the baseline fatigues

dataset_with_fatigues = inner_join(dataset_with_fatigues, baseline_fatigues, by=c("ID"="ID"))

# recoding fatigues here

dataset_with_fatigues = dataset_with_fatigues %>% mutate( EFAT2 = (6 - EFAT2)) %>% mutate( EFAT5 = (6 - EFAT5)) %>% mutate( EFAT9 = (6 - EFAT9)) %>% mutate( EFAT10 = (6 - EFAT10)) %>% mutate( EFAT13 = (6 - EFAT13)) %>% mutate( EFAT14 = (6 - EFAT14)) %>% mutate( EFAT16 = (6 - EFAT16)) %>% mutate( EFAT17 = (6 - EFAT17)) %>% mutate( EFAT18 = (6 - EFAT18)) %>% mutate( EFAT19 = (6 - EFAT19))

# delete rows with missing fatigues
dataset_with_fatigues = dataset_with_fatigues %>% drop_na("bas_fat1") %>% drop_na("bas_fat2") %>% drop_na("bas_fat3") %>% drop_na("bas_fat4") %>% drop_na("bas_fat5") %>% drop_na("bas_fat6") %>% drop_na("bas_fat7") %>% drop_na("bas_fat8") %>% drop_na("bas_fat9") %>% drop_na("bas_fat10") %>% drop_na("bas_fat11") %>% drop_na("bas_fat12") %>% drop_na("bas_fat13") %>% drop_na("bas_fat14") %>% drop_na("bas_fat15") %>% drop_na("bas_fat16") %>% drop_na("bas_fat17") %>% drop_na("bas_fat18") %>% drop_na("bas_fat19") %>% drop_na("bas_fat20")

#delete rows with BMI equals to 0
dataset_with_fatigues<-dataset_with_fatigues[!(dataset_with_fatigues$BMI<10),]

#build the 20 datasets for the random forest model
dataset_model_1 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat1, EFAT1)
dataset_model_2 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat2, EFAT2)
dataset_model_3 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat3, EFAT3)
dataset_model_4 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat4, EFAT4)
dataset_model_5 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat5, EFAT5)
dataset_model_6 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat6, EFAT6)
dataset_model_7 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat7, EFAT7)
dataset_model_8 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat8, EFAT8)
dataset_model_9 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat9, EFAT9)
dataset_model_10 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat10, EFAT10)
dataset_model_11 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat11, EFAT11)
dataset_model_12 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat12, EFAT12)
dataset_model_13 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat13, EFAT13)
dataset_model_14 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat14, EFAT14)
dataset_model_15 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat15, EFAT15)
dataset_model_16 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat16, EFAT16)
dataset_model_17 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat17, EFAT17)
dataset_model_18 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat18, EFAT18)
dataset_model_19 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat19, EFAT19)
dataset_model_20 = dataset_with_fatigues %>% select(BMI, age, weekly_alcohol, drugs, regular_menstruation, pill, times_pregnant, previous_hormon_treatment, t1_tense, t1_anxious, t1_worried, t1_panic, Surgery, Chemotherapy, Hormonetherapy, Radiotherapy, bas_fat20, EFAT20)

# defining train and test size 80-20

train_lines = 232 # 80% of the dataset
test_lines = length(dataset_model_1$BMI) - train_lines

#Building the model 1

train_m1 = dataset_model_1 %>% slice_head(n=train_lines)
test_m1 = dataset_model_1 %>% slice_tail(n=test_lines)

model_1 = randomForest(EFAT1~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat1, data=train_m1)

#calculate feature importance
featureImportance1 = varImp(model_1)

#performing the prediction
predict_y_m1 = predict(model_1, test_m1)

# Define and plotting PDP (Partial depedence function)
p1 <- partial(model_1, pred.var = c("bas_fat1", "BMI"), plot = TRUE, rug = TRUE)
p1

#Building the model 2

train_m2 = dataset_model_2 %>% slice_head(n=train_lines)
test_m2 = dataset_model_2 %>% slice_tail(n=test_lines)

model_2 = randomForest(EFAT2~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat2, data=train_m2)
#calculate feature importance
featureImportance2 = varImp(model_2)

#performing the prediction
predict_y_m2 = predict(model_2, test_m2)

# Define and plotting PDP (Partial depedence function)
p2 <- partial(model_2, pred.var = c("bas_fat2", "BMI"), plot = TRUE, rug = TRUE)
p2

#Building the model 3

train_m3 = dataset_model_3 %>% slice_head(n=train_lines)
test_m3 = dataset_model_3 %>% slice_tail(n=test_lines)

model_3 = randomForest(EFAT3~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat3, data=train_m3)
#calculate feature importance
featureImportance3 = varImp(model_3)

#performing the prediction
predict_y_m3 = predict(model_3, test_m3)

# Define and plotting PDP (Partial depedence function)
p3 <- partial(model_3, pred.var = c("bas_fat3", "BMI"), plot = TRUE, rug = TRUE)
p3

#Building the model 4

train_m4 = dataset_model_4 %>% slice_head(n=train_lines)
test_m4 = dataset_model_4 %>% slice_tail(n=test_lines)

model_4 = randomForest(EFAT4~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat4, data=train_m4)
#calculate feature importance
featureImportance4 = varImp(model_4)

#performing the prediction
predict_y_m4 = predict(model_4, test_m4)

# Define and plotting PDP (Partial depedence function)
p4 <- partial(model_4, pred.var = c("bas_fat4", "BMI"), plot = TRUE, rug = TRUE)
p4

#Building the model 5

train_m5 = dataset_model_5 %>% slice_head(n=train_lines)
test_m5 = dataset_model_5 %>% slice_tail(n=test_lines)

model_5 = randomForest(EFAT5~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat5, data=train_m5)
#calculate feature importance
featureImportance5 = varImp(model_5)

#performing the prediction
predict_y_m5 = predict(model_5, test_m5)

# Define and plotting PDP (Partial depedence function)
p5 <- partial(model_5, pred.var = c("bas_fat5", "BMI"), plot = TRUE, rug = TRUE)
p5

#Building the model 6

train_m6 = dataset_model_6 %>% slice_head(n=train_lines)
test_m6 = dataset_model_6 %>% slice_tail(n=test_lines)

model_6 = randomForest(EFAT6~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat6, data=train_m6)
#calculate feature importance
featureImportance6 = varImp(model_6)

#performing the prediction
predict_y_m6 = predict(model_6, test_m6)

# Define and plotting PDP (Partial depedence function)
p6 <- partial(model_6, pred.var = c("bas_fat6", "BMI"), plot = TRUE, rug = TRUE)
p6

#Building the model 7

train_m7 = dataset_model_7 %>% slice_head(n=train_lines)
test_m7 = dataset_model_7 %>% slice_tail(n=test_lines)

model_7 = randomForest(EFAT7~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat7, data=train_m7)
#calculate feature importance
featureImportance7 = varImp(model_7)

#performing the prediction
predict_y_m7 = predict(model_7, test_m7)

# Define and plotting PDP (Partial depedence function)
p7 <- partial(model_7, pred.var = c("bas_fat7", "BMI"), plot = TRUE, rug = TRUE)
p7


#Building the model 8

train_m8 = dataset_model_8 %>% slice_head(n=train_lines)
test_m8 = dataset_model_8 %>% slice_tail(n=test_lines)

model_8 = randomForest(EFAT8~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat8, data=train_m8)
#calculate feature importance
featureImportance8 = varImp(model_8)

#performing the prediction
predict_y_m8 = predict(model_8, test_m8)

# Define and plotting PDP (Partial depedence function)
p8 <- partial(model_8, pred.var = c("bas_fat8", "BMI"), plot = TRUE, rug = TRUE)
p8

#Building the model 9

train_m9 = dataset_model_9 %>% slice_head(n=train_lines)
test_m9 = dataset_model_9 %>% slice_tail(n=test_lines)

model_9 = randomForest(EFAT9~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat9, data=train_m9)
#calculate feature importance
featureImportance9 = varImp(model_9)

#performing the prediction
predict_y_m9 = predict(model_9, test_m9)

# Define and plotting PDP (Partial depedence function)
p9 <- partial(model_9, pred.var = c("bas_fat9", "BMI"), plot = TRUE, rug = TRUE)
p9

#Building the model 10

train_m10 = dataset_model_10 %>% slice_head(n=train_lines)
test_m10 = dataset_model_10 %>% slice_tail(n=test_lines)

model_10 = randomForest(EFAT10~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat10, data=train_m10)
#calculate feature importance
featureImportance10 = varImp(model_10)

#performing the prediction
predict_y_m10 = predict(model_10, test_m10)

# Define and plotting PDP (Partial depedence function)
p10 <- partial(model_10, pred.var = c("bas_fat10", "BMI"), plot = TRUE, rug = TRUE)
p10

#Building the model 11

train_m11 = dataset_model_11 %>% slice_head(n=train_lines)
test_m11 = dataset_model_11 %>% slice_tail(n=test_lines)

model_11 = randomForest(EFAT11~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat11, data=train_m11)
#calculate feature importance
featureImportance11 = varImp(model_11)

#performing the prediction
predict_y_m11 = predict(model_11, test_m11)

# Define and plotting PDP (Partial depedence function)
p11 <- partial(model_11, pred.var = c("bas_fat11", "BMI"), plot = TRUE, rug = TRUE)
p11

#Building the model 12

train_m12 = dataset_model_12 %>% slice_head(n=train_lines)
test_m12 = dataset_model_12 %>% slice_tail(n=test_lines)

model_12 = randomForest(EFAT12~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat12, data=train_m12)
#calculate feature importance
featureImportance12 = varImp(model_12)

#performing the prediction
predict_y_m12 = predict(model_12, test_m12)

# Define and plotting PDP (Partial depedence function)
p12 <- partial(model_12, pred.var = c("bas_fat12", "BMI"), plot = TRUE, rug = TRUE)
p12

#Building the model 13

train_m13 = dataset_model_13 %>% slice_head(n=train_lines)
test_m13 = dataset_model_13 %>% slice_tail(n=test_lines)

model_13 = randomForest(EFAT13~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat13, data=train_m13)
#calculate feature importance
featureImportance13 = varImp(model_13)

#performing the prediction
predict_y_m13 = predict(model_13, test_m13)

# Define and plotting PDP (Partial depedence function)
p13 <- partial(model_13, pred.var = c("bas_fat13", "BMI"), plot = TRUE, rug = TRUE)
p13

#Building the model 14

train_m14 = dataset_model_14 %>% slice_head(n=train_lines)
test_m14 = dataset_model_14 %>% slice_tail(n=test_lines)

model_14 = randomForest(EFAT14~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat14, data=train_m14)
#calculate feature importance
featureImportance14 = varImp(model_14)

#performing the prediction
predict_y_m14 = predict(model_14, test_m14)

# Define and plotting PDP (Partial depedence function)
p14 <- partial(model_14, pred.var = c("bas_fat14", "BMI"), plot = TRUE, rug = TRUE)
p14

#Building the model 15

train_m15 = dataset_model_15 %>% slice_head(n=train_lines)
test_m15 = dataset_model_15 %>% slice_tail(n=test_lines)

model_15 = randomForest(EFAT15~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat15, data=train_m15)
#calculate feature importance
featureImportance15 = varImp(model_15)

#performing the prediction
predict_y_m15 = predict(model_15, test_m15)

# Define and plotting PDP (Partial depedence function)
p15 <- partial(model_15, pred.var = c("bas_fat15", "BMI"), plot = TRUE, rug = TRUE)
p15

#Building the model 16

train_m16 = dataset_model_16 %>% slice_head(n=train_lines)
test_m16 = dataset_model_16 %>% slice_tail(n=test_lines)

model_16 = randomForest(EFAT16~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat16, data=train_m16)
#calculate feature importance
featureImportance16 = varImp(model_16)

#performing the prediction
predict_y_m16 = predict(model_16, test_m16)

# Define and plotting PDP (Partial depedence function)
p16 <- partial(model_16, pred.var = c("bas_fat16", "BMI"), plot = TRUE, rug = TRUE)
p16

#Building the model 17

train_m17 = dataset_model_17 %>% slice_head(n=train_lines)
test_m17 = dataset_model_17 %>% slice_tail(n=test_lines)

model_17 = randomForest(EFAT17~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat17, data=train_m17)
#calculate feature importance
featureImportance17 = varImp(model_17)

#performing the prediction
predict_y_m17 = predict(model_17, test_m17)

# Define and plotting PDP (Partial depedence function)
p17 <- partial(model_17, pred.var = c("bas_fat17", "BMI"), plot = TRUE, rug = TRUE)
p17

#Building the model 18

train_m18 = dataset_model_18 %>% slice_head(n=train_lines)
test_m18 = dataset_model_18 %>% slice_tail(n=test_lines)

model_18 = randomForest(EFAT18~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat18, data=train_m18)
#calculate feature importance
featureImportance18 = varImp(model_18)

#performing the prediction
predict_y_m18 = predict(model_18, test_m18)

# Define and plotting PDP (Partial depedence function)
p18 <- partial(model_18, pred.var = c("bas_fat18", "BMI"), plot = TRUE, rug = TRUE)
p18

#Building the model 19

train_m19 = dataset_model_19 %>% slice_head(n=train_lines)
test_m19 = dataset_model_19 %>% slice_tail(n=test_lines)

model_19 = randomForest(EFAT19~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat19, data=train_m19)
#calculate feature importance
featureImportance19 = varImp(model_19)

#performing the prediction
predict_y_m19 = predict(model_19, test_m19)

# Define and plotting PDP (Partial depedence function)
p19 <- partial(model_19, pred.var = c("bas_fat19", "BMI"), plot = TRUE, rug = TRUE)
p19

#Building the model 20

train_m20 = dataset_model_20 %>% slice_head(n=train_lines)
test_m20 = dataset_model_20 %>% slice_tail(n=test_lines)

model_20 = randomForest(EFAT20~BMI+age+weekly_alcohol+drugs+regular_menstruation+pill+times_pregnant+previous_hormon_treatment+t1_tense+t1_anxious+t1_worried+t1_panic+Surgery+Chemotherapy+Hormonetherapy+Radiotherapy+bas_fat20, data=train_m20)
#calculate feature importance
featureImportance20 = varImp(model_20)

#performing the prediction
predict_y_m20 = predict(model_20, test_m20)

# Define and plotting PDP (Partial depedence function)
p20 <- partial(model_20, pred.var = c("bas_fat20", "BMI"), plot = TRUE, rug = TRUE)
p20

# sum = 0.0

#Compute General Fatigue prediction and Errors

GeneralFatigueReal = predict_y_m1
GeneralFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  GeneralFatigueReal[index] = test_m1$EFAT1[index] + (test_m5$EFAT5[index]) + test_m12$EFAT12[index] + (test_m16$EFAT16[index])
  GeneralFatiguePrediction[index] = predict_y_m1[index] + (predict_y_m5[index]) + predict_y_m12[index] + (predict_y_m16[index])
}

#computing Average Error
sum = 0.0
error_GF = c()
array_MSE_GF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index]))
  error_GF[index] = as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index]))
}
average_error_GF = sum/(length(predict_y_m1))

#computing Mean Square Error
sum = 0.0
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + (as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index])))^2
  array_MSE_GF[index] = round((as.double(abs(GeneralFatigueReal[index] - GeneralFatiguePrediction[index])))^2, digits = 1)
}
GeneralFatigueReal
GeneralFatiguePrediction
array_MSE_GF
MSE_GF = sum/(length(predict_y_m1))

#Compute Physical Fatigue prediction and Errors

PhysicalFatigueReal = predict_y_m1
PhysicalFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  PhysicalFatigueReal[index] = (test_m2$EFAT2[index]) + test_m8$EFAT8[index] + (test_m14$EFAT14[index]) + test_m20$EFAT20[index]
  PhysicalFatiguePrediction[index] = (predict_y_m2[index]) + predict_y_m8[index] + (predict_y_m14[index]) + predict_y_m20[index]
}

#computing Average Error
sum = 0.0
error_PF = c()
array_MSE_PF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index]))
  error_PF[index] = as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index]))
}
average_error_PF = sum/(length(predict_y_m1))

#computing Mean Square Error
sum = 0.0
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + (as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index])))^2
  array_MSE_PF[index] = round((as.double(abs(PhysicalFatigueReal[index] - PhysicalFatiguePrediction[index])))^2, digits = 1)
}
MSE_PF = sum/(length(predict_y_m1))

#Compute Reduced Activity prediction and Errors

ReducedActivityReal = predict_y_m1
ReducedActivityPrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  ReducedActivityReal[index] = test_m3$EFAT3[index] + test_m6$EFAT6[index] + (test_m10$EFAT10[index]) + (test_m17$EFAT17[index])
  ReducedActivityPrediction[index] = predict_y_m3[index] + predict_y_m6[index] + (predict_y_m10[index]) + (predict_y_m17[index])
}

#computing Average Error
sum = 0.0
error_RA = c()
array_MSE_RA = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index]))
  error_RA[index] = as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index]))
}
average_error_RA = sum/(length(predict_y_m1))

#computing Mean Square Error
sum = 0.0
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + (as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index])))^2
  array_MSE_RA[index] = round((as.double(abs(ReducedActivityReal[index] - ReducedActivityPrediction[index])))^2, digits = 1)
}
MSE_RA = sum/(length(predict_y_m1))

#Compute Reduced Motivation prediction and Errors
ReducedMotivationReal = predict_y_m1
ReducedMotivationPrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  ReducedMotivationReal[index] = test_m4$EFAT4[index] + (test_m9$EFAT9[index]) + test_m15$EFAT15[index] + (test_m18$EFAT18[index])
  ReducedMotivationPrediction[index] = predict_y_m4[index] + (predict_y_m9[index]) + predict_y_m15[index] + (predict_y_m18[index])
}

#computing Average Error
sum = 0.0
error_RM = c()
array_MSE_RM = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index]))
  error_RM[index] = as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index]))
}
average_error_RM = sum/(length(predict_y_m1))

#computing Mean Square Error
sum = 0.0
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + (as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index])))^2
  array_MSE_RM[index] = round((as.double(abs(ReducedMotivationReal[index] - ReducedMotivationPrediction[index])))^2, digits = 1)
}
MSE_RM = sum/(length(predict_y_m1))

#Compute Mental Fatigue prediction and Errors

MentalFatigueReal = predict_y_m1
MentalFatiguePrediction = predict_y_m1

for (index in seq(1, length(predict_y_m1))) {
  MentalFatigueReal[index] = test_m7$EFAT7[index] + test_m11$EFAT11[index] + (test_m13$EFAT13[index]) + (test_m19$EFAT19[index])
  MentalFatiguePrediction[index] = predict_y_m7[index] + predict_y_m11[index] + (predict_y_m13[index]) + (predict_y_m19[index])
}

#computing Average Error
sum = 0.0
error_MF = c()
array_MSE_MF = c()
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index]))
  error_MF[index] = as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index]))
}
average_error_MF = sum/(length(predict_y_m1))

#computing Mean Square Error
sum = 0.0
for (index in seq(1, length(predict_y_m1))) {
  sum = as.double(sum) + (as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index])))^2
  array_MSE_MF[index] = round((as.double(abs(MentalFatigueReal[index] - MentalFatiguePrediction[index])))^2, digits = 1)
  
}
MSE_MF = sum/(length(predict_y_m1))

#printing the average error

average_error_GF
average_error_MF
average_error_PF
average_error_RA
average_error_RM

#printing the mean square error

MSE_GF
MSE_PF
MSE_RA
MSE_RM
MSE_MF

#printing the root mean square error

sqrt(MSE_GF)
sqrt(MSE_PF)
sqrt(MSE_RA)
sqrt(MSE_RM)
sqrt(MSE_MF)

#plotting the errors

error_GF_frame = data.frame(error_GF)
MSE_GF_frame = data.frame(array_MSE_GF) %>% mutate(name = "GF") %>% rename("SqError" = "array_MSE_GF")
error_GF_frame = error_GF_frame %>% mutate(name = "GF") %>% rename("error" = "error_GF")

error_MF_frame = data.frame(error_MF)
MSE_MF_frame = data.frame(array_MSE_MF) %>% mutate(name = "MF") %>% rename("SqError" = "array_MSE_MF")
error_MF_frame = error_MF_frame %>% mutate(name = "MF") %>% rename("error" = "error_MF")

error_PF_frame = data.frame(error_PF)
MSE_PF_frame = data.frame(array_MSE_PF) %>% mutate(name = "PF") %>% rename("SqError" = "array_MSE_PF")
error_PF_frame = error_PF_frame %>% mutate(name = "PF") %>% rename("error" = "error_PF")

error_RM_frame = data.frame(error_RM)
MSE_RM_frame = data.frame(array_MSE_RM) %>% mutate(name = "RM") %>% rename("SqError" = "array_MSE_RM")
error_RM_frame = error_RM_frame %>% mutate(name = "RM") %>% rename("error" = "error_RM")

error_RA_frame = data.frame(error_RA)
MSE_RA_frame = data.frame(array_MSE_RA) %>% mutate(name = "RA") %>% rename("SqError" = "array_MSE_RA")
error_RA_frame = error_RA_frame %>% mutate(name = "RA") %>% rename("error" = "error_RA")

errorFrame = rbind(error_GF_frame, error_PF_frame, error_RA_frame, error_RM_frame, error_MF_frame)
MSE_frame = rbind(MSE_GF_frame, MSE_MF_frame, MSE_PF_frame, MSE_RM_frame, MSE_RA_frame)

errorFrame = errorFrame %>% mutate(error = as.integer(error)) %>% group_by(error, name) %>% mutate(error_frequency = n()) %>% distinct()
MSE_frame = MSE_frame %>% mutate(error = as.integer(SqError)) %>% group_by(SqError, name) %>% mutate(error_frequency = n()) %>% distinct()

errorPlot = ggplot(errorFrame, aes(x=name, y=error)) + geom_boxplot()
sqerrorPlot = ggplot(MSE_frame, aes(x=name, y=SqError)) + geom_boxplot()

errorFrequency = ggplot(errorFrame, aes(x=error, y=error_frequency, colour=name)) + geom_histogram(stat='identity')
MSE_frequency = ggplot(MSE_frame, aes(x=SqError, y=error_frequency, colour=name)) + geom_histogram(stat='identity')

errorPlot
sqerrorPlot
errorFrequency
MSE_frequency

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
avgImportance_Frame$feature[17] = "XBasFat"

avgImportance_FrameOrdered = avgImportance_Frame[order(avgImportance_Frame$avgImportance, decreasing = TRUE),]


avgImportance_FrameOrdered$avgImportance = as.double(avgImportance_FrameOrdered$avgImportance)

ggplot(avgImportance_FrameOrdered, aes(x=avgImportance, y=feature)) + geom_bar(stat = "identity")

