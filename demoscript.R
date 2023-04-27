
library(dplyr)
library(forcats)

#pdf("test_pdf.pdf", width = 9, height = 6.5)

dat2 <- data.table::fread("/rawdata/UKBB/ukb668829.tab", sep = "\t")

metab <- read.table("Nightingale_biomarker_groups.txt", fill = T, sep = "\t", header=T)

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

sink("full_summary.txt")
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
  
  
