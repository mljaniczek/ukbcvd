# metabolite descriptions

load("/rawdata/UKBB/Metabolomics_Apr2023/dat_subset_metab.Rdata")
load("/rawdata/UKBB/Metabolomics_Apr2023/ukb_pts_w_metabolites.Rdata")
load("/rawdata/UKBB/Metabolomics_Apr2023/final_icd10_pts_with_disease.Rdata")
load("/rawdata/UKBB/Metabolomics_Apr2023/ukb_keys.Rdata")

metab_key2 <- ukb_key_metab %>%
  group_by(field.showcase) %>%
  slice(1) %>%
  filter(field.showcase < 23649)%>%
  ungroup() # get the first assessment measures of just the 249 metabs. Not worrying about QC or batches right now

dat_subset_metab_only <- dat_sub_metab %>%
  filter(eid %in% pts_with_metabolites)


met1 <- dat_subset_metab_only %>%
  select(all_of(metab_key2$col.name)) %>%
  transmute_all(~scale(log(. +.01), center = FALSE)) %>%
  mutate(eid = pts_with_metabolites)


testdat <- tabdat %>%select(eid, age = age_when_attended_assessment_centre_f21003_0_0, sex) %>%
  filter(eid %in% pts_with_metabolites)

t2 <- testdat %>% 
  left_join(met1)

library(ggplot2)
ggplot(t2, aes(x = concentration_of_very_small_vldl_particles_f23516_0_0, color = sex)) +
  geom_density()

ggplot(t2, aes(x = total_concentration_of_branchedchain_amino_acids_leucine_isoleucine_valine_f23464_0_0, color = sex)) +
  geom_density()

ggsave("branched_chain_amino_acids.png")

library(purrr)
met1 <- met1 %>% select(-eid)

testres <- data.frame(metid = c(1:249)) %>%
  mutate(index = map(metid, function(.x){!is.na(met1[,.x])

    }),
         mod1 = map2(metid, index, possibly(function(.x, .y){
             summary(lm(met1[.y, .x]~ testdat$sex[.y] + testdat$age[.y]))})
         )
 
 ) 



testres2 <- testres %>%
  mutate(
    coefficient_sex = map_dbl(mod1, possibly(function(.x){.x$coefficients[2,1]})), 
    pval = map_dbl(mod1, possibly(function(.x){.x$coefficients[2,4]})),
    n = map_dbl(index, ~sum(.x)),
    metabolite = metab_key2$col.name
  ) %>%
  select(metabolite, coefficient_sex, pval, n)

test27 <- testres2
test27[test27=='NULL'] <- NA


test28 <- as.data.frame(test27) %>%
  mutate(p.adjust.fdr = p.adjust(pval, "fdr"))

write.csv(test28, "lm_metabolites_051523.csv")


library(pheatmap)
met_mat <- t(as.matrix(met1))
#met_mat[met_mat==NA] <- 0
colnames(met_mat) <- testdat$eid

my_sample_row <- tabdat %>% 
  filter(eid %in% pts_with_metabolites) %>%
  select(sex, had_menopause)

rownames(my_sample_row) <- testdat$eid  

head(met_mat[1:10, 1:10])


pheatmap(met_mat,
         cluster_cols = TRUE,
         show_colnames = FALSE, show_rownames = FALSE,
         annotation_col = my_sample_row,
         main = "Log-transformed metabolites")
