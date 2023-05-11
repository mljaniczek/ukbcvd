#modification test

library(dplyr)
library(tidyr)
library(forcats)
library(gtsummary)
library(ukbtools)
library(stringr)

#pdf("test_pdf.pdf", width = 9, height = 6.5)

#dat2 <- data.table::fread("/rawdata/UKBB/ukb668829.tab", sep = "\t")


#metabs <- ukb_df("ukb672390", path = "/rawdata/UKBB/Metabolomics_Apr2023")

#ukb_key_metab <- ukb_df_field("ukb672390", path = "/rawdata/UKBB/Metabolomics_Apr2023")
#ukb_key_main <- ukb_df_field("ukb668829", path = "/rawdata/UKBB/TEMP_For_Margaret")


#dat <- ukb_df("ukb668829", path = "/rawdata/UKBB/TEMP_For_Margaret")

#save(dat, file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_main.Rdata")
#save(metabs, file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_metab.Rdata")

load(file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_main.Rdata")
load(file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_metab.Rdata")

#cutting down to 10000 for testing 
#dat <- dat[1:10000,]
#metabs <- metabs[1:10000,]

#dat2 <- left_join(dat, metabs)

#save(dat2, file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_10Ktest.Rdata")
load("/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_10Ktest.Rdata")

# trying to get ICD10 codes again

#save(ukb_key_main, ukb_key_metab, file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_keys.Rdata")

load("/rawdata/UKBB/Metabolomics_Apr2023/ukb_keys.Rdata")

# not working ukb_icd_freq_by(dat2, reference.var = "sex_f31_0_0", freq.plot = TRUE)
#ukb_context(dat2, nonmiss.var = "total_cholesterol_f23400_0_0",
#            sex.var = "sex_f31_0_0")

# primary demographics
primary_demographics = c(31, 1239,1249, 1259, 21000,21003)

# menopause
menopause = c(2724, 3581, 2814, 3536, 3546, 2824, 3591) 
#had menopause, age at menopause, ever used HRT, age started HRT, age last used HRT, age at hysterectomy, ever had hysterectomy

#physical characteristics
physical = c(21, 21001, 48, 49, 50, 77, 78, 93, 94, 95, 102, 3062, 3063, 3064, 4079, 4080, 
             4125, 4138, 10694, 10695, 21021, 21001, 23099, 23127, 23106)
medications = c(20003) #medications
#TODO not sure about GFR code
abdominal_mri = c(12140, 12223, 12224, 12623, 12664, 12848, 22403, 22404, 22405, 22406, 22407, 22408, 22409, 22410, 22411, 22412, 22413, 22414)
biomarker_measures = c(30750)
# diagnosis fields
icd <- c(41270)



cad_codes_icd10 <- c("I21", "I22", "I25", "Z951", "Z955")
atrial_fib_icd10 <- "I48"
stroke_icd10 <- c("I60", "I61", "I629", "I63", "I64", "I678",
                  "I690", "I693", "G951", "H341", "H342", "S066")


char_key <- ukb_key_main %>%
  filter(field.showcase %in% c(primary_demographics, physical, biomarker_measures, medications, menopause, abdominal_mri)) %>%
  group_by(field.showcase) %>%
  slice(1)

icd_key <- ukb_key_main %>%
  filter(field.showcase %in% icd) %>%
  group_by(field.showcase) 

icd_key_dat <- ukb_key_main %>%
  filter(field.showcase %in% c("41280")) %>%
  group_by(field.showcase)

# date of first assessment 53

# grab all columns of icd10
icd10 <- dat2 %>% select(eid, all_of(icd_key$col.name))
icd10date <- dat2 %>% select(eid, all_of(icd_key_dat$col.name))

# go to longer
icd10_long <- icd10 %>%
  pivot_longer(!eid, values_drop_na = TRUE) %>%
  group_by(eid) %>%
  mutate(id = row_number())

icd10dat_long <- icd10date %>%
  pivot_longer(!eid, values_drop_na = TRUE) %>%
  group_by(eid) %>%
  mutate(id = row_number())


# now get date of first assessment

dat_first <- dat2 %>%
  select(eid, date_of_attending_assessment_centre_f53_0_0) %>%
  right_join(icd10dat_long) %>%
  mutate(
    # subtract one date from another
    afterassessment = value > date_of_attending_assessment_centre_f53_0_0
  )

# now add the "after assessment" variable to the icd10 df
icd10dat <- icd10_long %>%
  left_join(dat_first %>% select(eid, id, afterassessment))


testwide <- icd10 %>%
  rowwise() %>%
  mutate(cad = any(grepl(c("I20|I21|I22|I23|I24|I25|Z951|Z955"), c_across(!eid)))*1)


# filter icd10 codes for coronary artery disease
icd10_long <- icd10_long %>%
  #filter(value %in% cad_codes_icd10)%>%
  mutate(cad = 
           ifelse(str_detect(value, c("I20|I21|I22|I23|I24|I25|Z951|Z955")),
                  1,0),
         afib = ifelse(str_detect(
           value, c("I48")),1,0 
         ),
         stroke = ifelse(str_detect(
           value, c("I60|I61|I629|I63|I64|1678|I690|I693|G951|H341|H342|S066")
         ), 1, 0),
         hypertension = ifelse(
           str_detect(
             value, c("I10|I11|I12|I13|I15|O10")
           ), 1,0),
         diabetes_t2 = ifelse( str_detect(
           value, c("E10|E11|E12|E13|E14")
         ), 1, 0))
         
test <- icd10_long

#test[test == 0] <- NA

test2 <- test %>%
  pivot_longer(!(eid:id), names_to="disease", 
               values_to = "has_disease",
               values_drop_na = FALSE) %>%
  group_by(eid, disease, has_disease) %>% 
  slice(1) # just grab first instance of disease

test3 <- test2 %>%
  select(eid,id, disease, has_disease)%>%
  filter(has_disease == 1) %>%
  group_by(eid, disease) %>%
  slice(1)

final <- dat_first %>%
  right_join(test3) %>%
  filter(afterassessment == TRUE)

# this is the set of unique patients that have one of the 
# disease!! after baseline! 
final2 <- final %>%
  select(eid, disease, has_disease) %>%
  pivot_wider(names_from = disease, values_from = has_disease,
              values_fill = 0)

final3 <- final2 %>%
  right_join(dat2 %>% select(eid, sex_f31_0_0))

final3[is.na(final3)] <- 0

final3 %>%
  select(-eid) %>%
  tbl_summary(by = sex_f31_0_0)

  # TODO
#match diagnosis to date of diagnosis
# grab date of first assessment
# compare date of diagnosis to date of first assessment
# filter for diagnosis that occurred after date of first asse


# dat <- dat %>%
#   select(eid, all_of(char_key$col.name), all_of(icd_key$col.name))
# 
# dat_sub_metab <- left_join(dat, metabs)

#save(dat_sub_metab, file = "/rawdata/UKBB/Metabolomics_Apr2023/dat_subset_metab.Rdata")
load("/rawdata/UKBB/Metabolomics_Apr2023/dat_subset_metab.Rdata")
tabdat <- dat_sub_metab %>%
  mutate(
    has_metabolites = ifelse(is.na(total_cholesterol_f23400_0_0), 0, 1)
  )%>%
  select(all_of(char_key$col.name), has_metabolites) %>%
  mutate(
    sex = ifelse(sex_f31_0_0 ==0, "Female", "Male"),
    current_smoker = fct_recode(
      as.factor(current_tobacco_smoking_f1239_0_0),
      yes = "1",
      occasionally = "2",
      no = "0",
      prefer_not_answer = "-3"
    ),
    ever_had_hysterectomy = fct_recode(
      as.factor(ever_had_hysterectomy_womb_removed_f3591_0_0),
      yes = "1",
      no = "0",
      not_sure = "-5",
      prefer_not_answer = "-3"
    ),
    ever_used_hrt = fct_recode(
      as.factor(
        ever_used_hormonereplacement_therapy_hrt_f2814_0_0
      ),
      yes = "1",
      no = "0",
      not_sure = "-1",
      prefer_not_answer = "-3"
    ),
    had_menopause = fct_recode(
      as.factor(
        had_menopause_f2724_0_0
      ),
      yes = "1",
      no = "0",
      not_sure_hysterectomy = "2",
      not_sure_other = "3",
      prefer_not_answer = "-3"
    )
  ) %>%
  select(-c(sex_f31_0_0, current_tobacco_smoking_f1239_0_0))

sum(tabdat$has_metabolites)

dontwant <- c("weight_method_f21_0_0", "treatmentmedication_code_f20003_0_0", "forced_expiratory_volume_in_1second_fev1_f3063_0_0",
              "forced_expiratory_volume_in_1second_fev1_pilot_f10695_0_1",
            "forced_vital_capacity_fvc_f3062_0_0", "forced_vital_capacity_fvc_pilot_f10694_0_1")
deal_later <- c("past_tobacco_smoking_f1249_0_0", 
                "had_menopause_f2724_0_0",
              "ever_used_hormonereplacement_therapy_hrt_f2814_0_0"
,                               "ever_had_hysterectomy_womb_removed_f3591_0_0",
                               "smokingsmokers_in_household_f1259_0_0", "ethnic_background_f21000_0_0")

metab_tab <- tabdat %>%
  filter(has_metabolites == 1) %>%
  select(sex:had_menopause, pulse_rate_automated_reading_f102_0_0:pulse_rate_during_bloodpressure_measurement_f95_0_0) %>%
  select(-all_of(dontwant), -all_of(deal_later)) %>%
  tbl_summary(by = sex, missing = "no") %>%
  bold_labels() %>%
  add_n() %>%
  add_overall()

all_tab <- tabdat %>%
  #filter(has_metabolites == 1) %>%
  select(sex:had_menopause, pulse_rate_automated_reading_f102_0_0:pulse_rate_during_bloodpressure_measurement_f95_0_0) %>%
  select(-all_of(dontwant), -all_of(deal_later)) %>%
  tbl_summary(by =sex, missing = "no") %>%
  bold_labels() %>%
  add_n() %>%
  add_overall()


tbl_merge1 <- 
  tbl_merge(tbls = list(all_tab, metab_tab),
            tab_spanner = c("**All Samples**", "**Samples with Metabolites**"))# %>%
  #modify_spanning_header(all_stat_cols() ~ "**Baseline Measurements**")

library(gt)

gtsave(as_gt(tbl_merge1), file = "baseline_table.rtf")

tblmerg_gt <- as_gt(tbl_merge1)
save(tblmerg_gt, file = "~/testtab.Rdata")

#grab diagnostic incidence

#codes from jama paper eTable 7 Said 2018 Associations of Combined Genetic and LIfestyle risks with ...

# coronary artery disease 
# icd-9 410, 412, 414
# icd-10 I21-25, Z951, Z955 #field 41270

#OPCS-4 K40-K46, K49, K50, K75 #field id 41272
# self-reported 6150(1), 3894, 20004(1070, 1095, 1523)
#mortality field 40000 date of death
# mortality field 40001 primary cause of death

# Stroke codes
#icd-9 3361, 36231, 36232, 430, 431, 4329, 43301, 43311, 43321, 43331, 43381, 43391, 434, 436


#join demographi and metab data
fulldat <- ukb_df_full_join(dat, metabs)

# visualize distibution of certain demographic variables in missing vs non missing data
ukb_context(fulldat,
            nonmiss.var = total_cholesterol_f23400_0_0)

# visualize ICD freq by sex in full vs metabolite dataset

metab <- read.table("~/Nightingale_biomarker_groups.txt", fill = T, sep = "\t", header=T)

# datselect <- dat2 %>%
#   select(f.eid,
#          sex = starts_with("f.31."),
#          age_at_assessment = starts_with("f.21003.0"),
#          year_of_birth = starts_with("f.34.0"),
#          month_of_birth = starts_with("f.52.0"),
#          date_of_assesment = starts_with("f.53.0"),
#          ethnic_background = starts_with("f.21000.0"),
#          current_smoker = starts_with("f.1239.0"),
#          # past_smoker = starts_with("f.1249.0"),
#          # smokers_in_house = starts_with("f.1259.0"),
#          weight = starts_with("f.21.0"),
#          bmi_assessment = starts_with("f.21001.0"),
#          heel_bone_ultrasound_tscore = starts_with("f.77.0"),
#          heel_bmd = starts_with("f.78.0"),
#          systol_bp = starts_with("f.93.0"),
#          diastol_bp = starts_with("f.94.0"),
#          fvc = starts_with("f.3062.0"),
#          fev1 = starts_with("f.3063.0"),
#          pef = starts_with("f.3064.0"),
#          pulse_wave_arterial_stiff = starts_with("f.21021.0"),
#          hba1c = starts_with("f.30750.0")
#          ) %>%
#   mutate(
#     female = as.factor(ifelse(sex == 0, 1, 0)),
#     current_smoker2 = fct_recode(as.factor(current_smoker),
#                                  yes = "1",
#                                  occasionally = "2",
#                                  no = "0"))
# 
# 
# testmerge <- right_join(datselect %>% select(eid = f.eid,female, current_smoker2, age_at_assessment, bmi_assessment, hba1c), metabs, by = "eid")
# 
# testmerge %>%
#   select(age_at_assessment, female, current_smoker2, age_at_assessment:hba1c)%>%
#   tbl_summary(by = female) %>%
#   add_overall()
# 
# testmerge %>%
#   drop_na(total_cholesterol_f23400_0_0)%>%
#   select(age_at_assessment, female, current_smoker2, age_at_assessment:hba1c) %>%
#   tbl_summary(by = female) %>%
#   add_overall()
# 
# # old before better wrangling
# #sink("full_summary.txt")
# # print("Female characteristics")
# # datselect %>%
# #   select(age_at_assessment, female, current_smoker2, weight:hba1c) %>%
# #   filter(female == "1") %>%
# #   summary()
# # 
# # print("Male characteristics")
# # datselect %>%
# #   select(age_at_assessment, female, current_smoker2, weight:hba1c) %>%
# #   filter(female == "0") %>%
# #   summary()
# # 
# # sink()
  
  
