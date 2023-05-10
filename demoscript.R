#modification test

library(dplyr)
library(tidyr)
library(forcats)
library(gtsummary)
library(ukbtools)

#pdf("test_pdf.pdf", width = 9, height = 6.5)

dat2 <- data.table::fread("/rawdata/UKBB/ukb668829.tab", sep = "\t")

dat <- ukb_df("ukb668829", path = "/rawdata/UKBB")

metabs <- ukb_df("ukb672390", path = "/rawdata/UKBB/Metabolomics_Apr2023")

ukb_key_metab <- ukb_df_field("ukb672390", path = "/rawdata/UKBB/Metabolomics_Apr2023")


dat <- ukb_df("ukb668829", path = "/rawdata/UKBB")

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

datselect <- dat2 %>%
  select(f.eid,
         sex = starts_with("f.31."),
         age_at_assessment = starts_with("f.21003.0"),
         year_of_birth = starts_with("f.34.0"),
         month_of_birth = starts_with("f.52.0"),
         date_of_assesment = starts_with("f.53.0"),
         ethnic_background = starts_with("f.21000.0"),
         current_smoker = starts_with("f.1239.0"),
         # past_smoker = starts_with("f.1249.0"),
         # smokers_in_house = starts_with("f.1259.0"),
         weight = starts_with("f.21.0"),
         bmi_assessment = starts_with("f.21001.0"),
         heel_bone_ultrasound_tscore = starts_with("f.77.0"),
         heel_bmd = starts_with("f.78.0"),
         systol_bp = starts_with("f.93.0"),
         diastol_bp = starts_with("f.94.0"),
         fvc = starts_with("f.3062.0"),
         fev1 = starts_with("f.3063.0"),
         pef = starts_with("f.3064.0"),
         pulse_wave_arterial_stiff = starts_with("f.21021.0"),
         hba1c = starts_with("f.30750.0")
         ) %>%
  mutate(
    female = as.factor(ifelse(sex == 0, 1, 0)),
    current_smoker2 = fct_recode(as.factor(current_smoker),
                                 yes = "1",
                                 occasionally = "2",
                                 no = "0"))


testmerge <- right_join(datselect %>% select(eid = f.eid,female, current_smoker2, age_at_assessment, bmi_assessment, hba1c), metabs, by = "eid")

testmerge %>%
  select(age_at_assessment, female, current_smoker2, age_at_assessment:hba1c)%>%
  tbl_summary(by = female) %>%
  add_overall()

testmerge %>%
  drop_na(total_cholesterol_f23400_0_0)%>%
  select(age_at_assessment, female, current_smoker2, age_at_assessment:hba1c) %>%
  tbl_summary(by = female) %>%
  add_overall()


#sink("full_summary.txt")
print("Female characteristics")
datselect %>%
  select(age_at_assessment, female, current_smoker2, weight:hba1c) %>%
  filter(female == "1") %>%
  summary()

print("Male characteristics")
datselect %>%
  select(age_at_assessment, female, current_smoker2, weight:hba1c) %>%
  filter(female == "0") %>%
  summary()

sink()
  
  
