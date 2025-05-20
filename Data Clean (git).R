### Data Clean  

library(readxl)
library(tidyverse)

dem_dat <- ... %>% 
  select(mrn, intervention_id, age, gender, race, bmi, LE_past_ops, procedure_cat, sdi)

dem_dat <- dem_dat %>% 
  mutate(
    procedure_cat = case_when(
      procedure_cat == "forefoot/midfoot arthritis" ~ "midfoot/forefoot arthritis/degeneration",
      procedure_cat == "forefoot/midfoot deformity" ~ "midfoot/forefoot deformity",
      TRUE ~ procedure_cat
    ),
    bmi_class = case_when(                          
      bmi < 18.5 ~ "Underweight",
      bmi > 18.5 & bmi < 25 ~ "Healthy Weight",
      bmi > 25 & bmi < 30 ~ "Overweight",
      bmi > 30 ~ "Obese"
    ),
    LE_past_ops = ifelse(test = LE_past_ops > 4, yes = "4+", no = LE_past_ops),
    sdi_cat = case_when(
      sdi > 0 & sdi <= 25 ~ "1st Q",
      sdi > 25.01 & sdi <= 50 ~ "2nd Q",
      sdi > 50.01 & sdi <= 75 ~ "3rd Q",
      sdi > 75.01  ~ "4th Q"
    ))

table(dem_dat$LE_past_ops, useNA = "always")

# dem_ids <- dem_dat$mrn %>% na.omit() 

# Physical Function 
dat_pf <- ... %>% select(contains("ID"), MRN, contains("PRO"), contains("longest")) %>% filter(FU_longest >= 180)

dat_pf <- merge(dat_pf, dem_dat, by.x = "MRN", by.y = "mrn")

dat_pf_full <- dat_pf %>% distinct(Intervention.ID, .keep_all = T)

dat_pf_full$pf_dif <- dat_pf_full$PRO_longest - dat_pf_full$PRO_baseline

dat_pf_full <- dat_pf_full %>% 
  select(Intervention.ID, age, gender, race, bmi_class, LE_past_ops, procedure_cat, sdi_cat, pf_dif, FU_longest)

# Pain Interference 
dat_pi <- ... %>% select(contains("ID"), MRN, contains("PRO"), contains("longest")) %>% filter(FU_longest >= 180)

dat_pi <- merge(dat_pi, dem_dat, by.x = "MRN", by.y = "mrn")

dat_pi_full <- dat_pi %>% distinct(Intervention.ID, .keep_all = T)

dat_pi_full$pi_dif <- dat_pi_full$PRO_longest - dat_pi_full$PRO_baseline

dat_pi_full <- dat_pi_full %>% 
  select(Intervention.ID, age, gender, race, bmi_class, LE_past_ops, procedure_cat, sdi_cat, pi_dif, FU_longest)

# SANE 
dat_sane <- ... %>% select(contains("ID"), MRN, contains("PRO"), contains("longest")) %>% filter(FU_longest >= 180)

dat_sane <- merge(dat_sane, dem_dat, by.x = "MRN", by.y = "mrn")

dat_sane_full <- dat_sane %>% distinct(Intervention.ID, .keep_all = T)

dat_sane_full$sane_dif <- dat_sane_full$PRO_longest - dat_sane_full$PRO_baseline

dat_sane_full <- dat_sane_full %>% 
  select(Intervention.ID, age, gender, race, bmi_class, LE_past_ops, procedure_cat, sdi_cat, sane_dif, FU_longest)



# Merge (variable w/ PF/PI)
# n = 1245 
dat_pi_sane <- merge(dat_pi_full, dat_sane_full, by = "Intervention.ID", no.dups = T)

# n = 1280 
dat_pf_sane <- merge(dat_pf_full, dat_sane_full, by = "Intervention.ID", no.dups = T)

dat_pf_sane <- dat_pf_sane %>% 
  filter(Intervention.ID %in% dat_pi_sane$Intervention.ID)

dat_pi_sane <- dat_pi_sane %>% 
  filter(Intervention.ID %in% dat_pf_sane$Intervention.ID)

sd.5 <- sd(dat_sane_full$sane_dif)/2  # Threshold to meet 

dat_sane_full %>% group_by(gender) %>% summarize(sd.5 = sd(sane_dif)/2)


## For Summary Tables 

dat_sum <- cbind(dat_pf_sane, dat_pi_sane)

dat_sum <- dat_sum[, c(2:5, 10)]





