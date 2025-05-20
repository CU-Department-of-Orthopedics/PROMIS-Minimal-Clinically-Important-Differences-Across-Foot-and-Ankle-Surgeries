### Physical Function ###

rm(list = ls())

library(readxl)
library(tidyverse)

source("Data Clean.R")


# Physical Function, no other vars
{
  pf_mcid_stan <- sd(dat_pf_sane$pf_dif)/2 
  
  pf_mcid.5 <- dat_sane_full %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`("sane_dif")
  
  pf_mcid.5
  
  pf_mod_no_vars <- lm(pf_dif ~ sane_dif, data = dat_pf_sane)
  summary(pf_mod_no_vars)
  
  mcid_pf_reg <- predict(pf_mod_no_vars, newdata = pf_mcid.5)
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_reg_MCID = case_when(
        pf_dif >= mcid_pf_reg ~ 1,
        pf_dif < mcid_pf_reg ~ 0
      )
    )
  
  dat_pf_sane %>% group_by(met_reg_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm"
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Physical Function"
    )
  
}



## Physical Function -- Sex (M v F)
{
  
  pf_mcid.5 <- dat_sane_full %>% group_by(gender) %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`(c("gender.x", "sane_dif"))
  
  pf_mcid_stan_sex <- dat_pf_sane %>% group_by(gender.y) %>% summarize(sd.5 = sd(pf_dif)/2) %>% 
    `names<-`(c("gender.x", "pf_dif"))
  
  pf_mcid_stan_sex
  
  full_pf_mod_sex <- lm(pf_dif ~ sane_dif*gender.x, data = dat_pf_sane)
  summary(full_pf_mod_sex)
  
  mcid_pf_sex_reg <- as.data.frame(predict(full_pf_mod_sex, interval = "confidence", level = .95, newdata = pf_mcid.5))

  mcid_pf_sex_reg$sex <- levels(factor(dat_pf_sane$gender.x))

  cbind(mcid_pf_sex_reg, table(dat_sane_full$gender), pf_mcid.5$sane_dif)
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_sex_MCID = case_when(
        gender.x == "Male" & pf_dif >= mcid_pf_sex_reg[2,1] ~ 1,
        gender.x == "Male" & pf_dif < mcid_pf_sex_reg[2, 1] ~ 0,
        gender.x == "Female" & pf_dif >= mcid_pf_sex_reg[1, 1] ~ 1,
        gender.x == "Female" & pf_dif < mcid_pf_sex_reg[1, 1] ~ 0
      )
    )
  
  dat_pf_sane %>% group_by(gender.x, met_sex_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif,
      color = gender.y
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm"
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Physical Function",
      color = "Sex"
    )
  
  ggplot(
    data = data.frame(mcid_pf_sex_reg),
    aes(
      x = sex,
      y = fit,
      fill = sex
    )
  ) + 
    geom_bar(
      stat = "identity",
      color = "black"
    ) + 
    geom_errorbar(
      aes(
        ymin = lwr, 
        ymax = upr
      ), width = .5
    ) + 
    geom_hline(
      yintercept = mcid_pf_reg,
      linetype = "dashed",
      color = "darkgrey"
    ) + 
    annotate(
      geom = "text", 
      x = 1,
      y = mcid_pf_reg + .2, 
      label = "Global Anchored MCID",
      color = "darkgrey"
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "",
      y = "Phyiscal Function SANE Difference MCID",
      fill = ""
    ) + 
    theme(
      legend.position = 'none'
    ) 
    
    c(summary(mcid_pf_sex_reg$fit), SD = sd(mcid_pf_sex_reg$fit)) # Summary 
}





### Physical Function -- Procedure Category 
{
  # table(dat_sane_full$procedure_cat, useNA = "always")
  
  pf_mcid.5 <- dat_sane_full %>% group_by(procedure_cat) %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`(c("procedure_cat.x", "sane_dif")) %>% drop_na()
  
  
  full_pf_mod_proc <- lm(pf_dif ~ sane_dif*procedure_cat.x, data = dat_pf_sane)
  summary(full_pf_mod_proc)
  
  mcid_pf_reg_proc <- as.data.frame(predict(full_pf_mod_proc, interval = "conf", level = .95, newdata = pf_mcid.5))
  mcid_pf_reg_proc$proc_name <- levels(factor(dat_pf_sane$procedure_cat.x))
  
  cbind(mcid_pf_reg_proc, table(dat_sane_full$procedure_cat), pf_mcid.5$sane_dif)
  
  dat_pf_sane <- merge(dat_pf_sane, mcid_pf_reg_proc[, c(1,4)], by.x = "procedure_cat.x", by.y = "proc_name")
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_proc_MCID = case_when(
        pf_dif > fit ~ 1,
        pf_dif < fit ~ 0
      )
    ) %>% 
    select(-c(fit))
  
  dat_pf_sane %>% group_by(procedure_cat.x, met_proc_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif,
      color = procedure_cat.x
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm",se = F
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Physical Function",
      color = "Procedure Category"
    ) +
    theme(
      legend.position = "bottom"
    )
  
  ggplot(
    data = mcid_pf_reg_proc,
    aes(
      x = proc_name,
      y = fit,
      fill = proc_name
    )
  ) + 
    geom_bar(
      stat = "identity",
      color = "black"
    ) + 
    geom_errorbar(
      aes(
        ymin = lwr, 
        ymax = upr
      ), width = .5
    ) + 
    geom_hline(
      yintercept = mcid_pf_reg,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text", 
      x = 9.75,
      y = mcid_pf_reg + .5,
      label = "Global MCID"
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "",
      y = "Physical Function SANE Difference MCID",
      fill = ""
    )+ 
    theme(
      legend.position = 'none',
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    ) 
  
  c(summary(mcid_pf_reg_proc$fit), SD = sd(mcid_pf_reg_proc$fit))
  
}



### Physical Function -- Past Operations 
{
  pf_mcid.5 <- dat_sane_full %>% group_by(LE_past_ops) %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`(c("LE_past_ops.x", "sane_dif")) %>% drop_na()
  
  pf_mcid.5$LE_past_ops.x <- as.factor(pf_mcid.5$LE_past_ops.x)
  
  full_pf_mod_ops <- lm(pf_dif ~ sane_dif*as.factor(LE_past_ops.x), data = dat_pf_sane)
  summary(full_pf_mod_ops)
  
  mcid_pf_reg_ops <- as.data.frame(predict(full_pf_mod_ops, interval = "conf", level = .95, newdata = pf_mcid.5))
  mcid_pf_reg_ops$ops <- levels(factor(dat_pf_sane$LE_past_ops.x))
  
  cbind(mcid_pf_reg_ops, table(dat_sane_full$LE_past_ops), pf_mcid.5$sane_dif)
  
  dat_pf_sane <- merge(dat_pf_sane, mcid_pf_reg_ops[ , c(1, 4)],  by.x = "LE_past_ops.x", by.y = "ops")
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_ops_MCID = case_when(
        pf_dif > fit ~ 1,
        pf_dif < fit ~ 0
      )
    ) %>% 
    select(-c(fit))
  
  dat_pf_sane %>% group_by(LE_past_ops.x, met_ops_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif,
      color = LE_past_ops.x
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm",se = F
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Pain Int.",
      color = "# Past Operations"
    ) +
    theme(
      legend.position = "bottom"
    )
  
  ggplot(
    data = mcid_pf_reg_ops,
    aes(
      x = ops,
      y = fit,
      fill = ops
    )
  ) + 
    geom_bar(
      stat = "identity",
      color = "black"
    ) + 
    geom_errorbar(
      aes(
        ymin = lwr, 
        ymax = upr
      ), width = .5
    ) + 
    geom_hline(
      yintercept = mcid_pf_reg,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text", 
      x = 2,
      y = mcid_pf_reg + .5,
      label = "Global MCID"
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Number of Past Operations",
      y = "Physical Function SANE Difference MCID",
      fill = ""
    )+ 
    theme(
      legend.position = 'none'
    ) 
  
  c(summary(mcid_pf_reg_ops$fit), SD = sd(mcid_pf_reg_ops$fit))
}



### Pain Interference -- BMI Class. Reorder lightess --> heaviest 
{
  pf_mcid.5 <- dat_sane_full %>% group_by(bmi_class) %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`(c("bmi_class.y", "sane_dif")) %>% drop_na()
  
  pf_mcid.5$bmi_class <- as.factor(pf_mcid.5$bmi_class.y)
  
  full_pf_mod_bmi <- lm(pf_dif ~ sane_dif*as.factor(bmi_class.y), data = dat_pf_sane)
  summary(full_pf_mod_bmi)
  
  mcid_pf_reg_bmi <- as.data.frame(predict(full_pf_mod_bmi, interval = "conf", level = 0.95, newdata = pf_mcid.5))
  mcid_pf_reg_bmi$bmi <- levels(factor(dat_pf_sane$bmi_class.y))
  mcid_pf_reg_bmi$bmi <- factor(mcid_pf_reg_bmi$bmi, levels = c("Underweight", "Healthy Weight", "Overweight", "Obese"))
  
  cbind(mcid_pf_reg_bmi, table(dat_sane_full$bmi_class), pf_mcid.5$sane_dif)
  
  dat_pf_sane <- merge(dat_pf_sane, mcid_pf_reg_bmi[ c(1, 4)],  by.x = "bmi_class.y", by.y = "bmi")
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_bmi_MCID = case_when(
        pf_dif > fit ~ 1,
        pf_dif < fit ~ 0
      )
    ) %>% 
    select(-c(fit))
  
  dat_pf_sane %>% group_by(bmi_class.y, met_bmi_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif,
      color = bmi_class.y
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm",se = F
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Physical Function",
      color = "BMI Class."
    ) +
    theme(
      legend.position = "bottom"
    )
  
  ggplot(
    data = mcid_pf_reg_bmi,
    aes(
      x = bmi,
      y = fit,
      fill = bmi
    )
  ) + 
    geom_bar(
      stat = "identity",
      color = "black"
    ) + 
    geom_errorbar(
      aes(
        ymin = lwr, 
        ymax = upr
      ), width = .5
    ) + 
    geom_hline(
      yintercept = mcid_pf_reg,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text", 
      x = 1.25,
      y = mcid_pf_reg + .2,
      label = "Global MCID"
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "BMI Classification",
      y = "Physical Function SANE Difference MCID",
      fill = ""
    )+ 
    theme(
      legend.position = 'none'
    ) 
  
  c(summary(mcid_pf_reg_bmi$fit), SD = sd(mcid_pf_reg_bmi$fit))
    
}




### Physical Function -- SDI (Quartiles)
{
  pf_mcid.5 <- dat_sane_full %>% group_by(sdi_cat) %>% summarize(sd.5 = sd(sane_dif)/2) %>% 
    `names<-`(c("sdi_cat.y", "sane_dif")) %>% drop_na()
  
  pf_mcid.5$sdi_cat <- as.factor(pf_mcid.5$sdi_cat.y)
  
  full_pf_mod_sdi <- lm(pf_dif ~ sane_dif*as.factor(sdi_cat.y), data = dat_pf_sane)
  summary(full_pf_mod_sdi)
  
  mcid_pf_reg_sdi <- as.data.frame(predict(full_pf_mod_sdi, interval = "conf", level = 0.95, newdata = pf_mcid.5))
  mcid_pf_reg_sdi$sdi <- levels(factor(dat_pf_sane$sdi_cat.y))
  
  cbind(mcid_pf_reg_sdi, table(dat_sane_full$sdi_cat), pf_mcid.5$sane_dif)
  
  dat_pf_sane <- merge(dat_pf_sane, mcid_pf_reg_sdi[ c(1, 4)],  by.x = "sdi_cat.y", by.y = "sdi")
  
  dat_pf_sane <- dat_pf_sane %>% 
    mutate(
      met_sdi_MCID = case_when(
        pf_dif > fit ~ 1,
        pf_dif < fit ~ 0
      )
    ) %>% 
    select(-c(fit))
  
  dat_pf_sane %>% group_by(sdi_cat.y, met_sdi_MCID) %>% summarize(count = n())
  
  ggplot(
    data = dat_pf_sane,
    aes(
      x = sane_dif, 
      y = pf_dif,
      color = sdi_cat.y
    )
  ) + 
    geom_point(
      
    ) + 
    geom_smooth(
      method = "lm",se = F
    ) + 
    theme_minimal(
      
    ) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    labs(
      x = "Difference in SANE",
      y = "Difference in Pain Int.",
      color = "SDI Cat."
    ) +
    theme(
      legend.position = "bottom"
    )
  
  ggplot(
    data = mcid_pf_reg_sdi ,
    aes(
      x = sdi,
      y = fit,
      fill = sdi
    )
  ) + 
    geom_bar(
      stat = "identity",
      color = "black"
    ) + 
    geom_errorbar(
      aes(
        ymin = lwr, 
        ymax = upr
      ), width = .5
    ) + 
    geom_hline(
      yintercept = mcid_pf_reg,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text", 
      x = .7,
      y = mcid_pf_reg + .2,
      label = "Global MCID"
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "SDI Quartile",
      y = "Physical Function SANE Difference MCID",
      fill = ""
    )+ 
    theme(
      legend.position = 'none'
    ) 
  
  c(summary(mcid_pf_reg_sdi$fit), SD = sd(mcid_pf_reg_sdi$fit))
}








