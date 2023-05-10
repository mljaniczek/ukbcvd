# script to get icd10 codes and dates

# load full ukb set and metab set

load(file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_main.Rdata")
load(file = "/rawdata/UKBB/Metabolomics_Apr2023/ukb_df_metab.Rdata")

# load ukb keys

load("/rawdata/UKBB/Metabolomics_Apr2023/ukb_keys.Rdata")

char_key <- ukb_key_main %>%
  filter(field.showcase %in% c(primary_demographics, physical, biomarker_measures, medications, menopause, abdominal_mri)) %>%
  group_by(field.showcase) %>%
  slice(1)

icd_key <- ukb_key_main %>%
  filter(field.showcase %in% "41270") %>%
  group_by(field.showcase) 

icd_key_dat <- ukb_key_main %>%
  filter(field.showcase %in% c("41280")) %>%
  group_by(field.showcase)

# grab all columns of icd10
icd10 <- dat %>% select(eid, all_of(icd_key$col.name))
icd10date <- dat %>% select(eid, all_of(icd_key_dat$col.name))

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

#now join this with the metab + demographics dataset
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


final3 <- final2 %>%
  right_join(tabdat %>% select(eid, sex, has_metabolites))

final3[is.na(final3)] <- 0

# now make table of prevalence
metab_tab_icd10 <- final3 %>%
  filter(has_metabolites == 1) %>%
  tbl_summary(by = sex, missing = "no") %>%
  bold_labels() %>%
  add_n() %>%
  add_overall()

all_tab_icd10 <- final3 %>%
  tbl_summary(by =sex, missing = "no") %>%
  bold_labels() %>%
  add_n() %>%
  add_overall()


tbl_merge_icd10 <- 
  tbl_merge(tbls = list(all_tab_icd10, metab_tab_icd10),
            tab_spanner = c("**All Samples**", "**Samples with Metabolites**"))# %>%
#modify_spanning_header(all_stat_cols() ~ "**Baseline Measurements**")

library(gt)

gtsave(as_gt(tbl_merge_icd10), file = "dx_prevalence_table.rtf")

tblmerg_gt_icd10 <- as_gt(tbl_merge_icd10)
save(tblmerg_gt_icd10, file = "~/testtab_icd10.Rdata")
#
