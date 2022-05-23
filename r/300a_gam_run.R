# gam models

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(mgcv)
library(meta)
library(tsibble)

source('r/functions/map_country_group.R')
source('r/functions/ci_gam.R')

options("digits" = 2)

pdt <- qs::qread("data/dt_all_100d.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

pdt[cnt>100, cnt := 100]
pdt[cnt_home>100, cnt_home := 100]
pdt[cnt_work>100, cnt_work := 100]
pdt[cnt_others>100, cnt_others := 100]

pdt[country=="uk", model := "uk"]
pdt[country=="be", model := "be"]
pdt[country=="de", model := "de"]
pdt[country=="nl", model := "nl"]
pdt[group=="G1",   model := "G1"]
pdt[group=="G2",   model := "G2"]
pdt[group=="G3",   model := "G3"]

pdt[, C1 := fifelse(C1 %in% c(0,1,2),0,1)]
pdt[, C2 := fifelse(C2 %in% c(0,1,2),0,1)]
pdt[, C3 := fifelse(C3 %in% c(0,1),0,1)]
pdt[, C6 := fifelse(C6 %in% c(0,1),0,1)]

pdt[part_attend_work_yesterday == "yes", attend := 1]
pdt[part_attend_work_week %in% c("every day", "most days"), attend := 1]
pdt[part_limit_work_atleast_day == "no", attend := 1]
pdt[part_limit_work_atleast_day == "No", attend := 1]
pdt[part_workattendance_7days_wave14_33 %in% c("Every day", "Most days"), attend := 1]
pdt[part_workattendance_yesterday_wave14_33 == "Yes", attend := 1]
pdt[part_workattendance_limit_wave1_13 == "No", attend := 1]
pdt[is.na(attend), attend := 0]

pdt[, weekday := fifelse(weekday %in% c("Saturday", "Sunday"),1,0)]

pdt[, ym := yearmonth(date)]
pdt[, first_ym := min(ym), by = .(country, panel)]
pdt[, ym := ym - first_ym + 1]

pdt[, first_survey := min(survey_round), by = .(part_id)]

pdt[, part_id := gsub("de_de_", "de_", part_id)]
pdt[, new_id := .GRP, by = .(country, panel, part_id)]
pdt[, new_id := as.numeric(new_id)]

pdt[, part_id := substr(part_id, 4, nchar(part_id))]
pdt[, part_id := as.numeric(part_id)]
pdt[, country := factor(country)]

pdt <- pdt[part_gender != "other"]

  #all contacts
  all_list <- list()
  
  for (i in 1:length(unique(pdt$model))){
    
    this_model <- unique(pdt$model)[i]
    
    print(Sys.time()); print(this_model)
    
    sub_pdt <- pdt[model == this_model]
    
    all <- bam(cnt ~ s(new_id, by = country, bs = "re") + 
                     factor(part_age_group) + 
                     part_gender + hh_size + old +
                     C1 + C2 + C3 + C6 +
                     part_att_likely + part_att_spread + part_att_serious +
                     part_symp_any + risk + mask + vacc +
                     weekday + s(ym, k=4) + s(first_survey, k=3), 
                     data = sub_pdt, 
                     family = nb(link = "log"), 
                     weights = genderageweight_proportion)
   
    all_list[[i]] <- ci_gam(all)
    all_list[[i]]$model <- this_model
    all_list[[i]]$setting <- "all"        
    
  }
  all_list <- rbindlist(all_list)
  

  #home contacts
  home_list <- list()
  
  for (i in 1:length(unique(pdt$model))){
    
    print(Sys.time()); print(i)
    
    this_model <- unique(pdt$model)[i]
    
    sub_pdt <- pdt[model == this_model]
    
    home <- bam(cnt_home ~ s(new_id, by = country, bs = "re") + 
                           factor(part_age_group) + 
                           part_gender + hh_size + old +
                           C1 + C2 + C3 + C6 +
                           part_att_likely + part_att_spread + part_att_serious +
                           part_symp_any + risk + mask + vacc +
                           weekday + s(ym, k=4) + s(first_survey, k=3), 
                           data = sub_pdt, 
                           family = nb(link = "log"), 
                           weights = genderageweight_proportion)
              
    home_list[[i]] <- ci_gam(home)
    home_list[[i]]$model <- this_model
    home_list[[i]]$setting <- "home"        
  }
  home_list <- rbindlist(home_list)
  
  
  #other contacts
  others_list <- list()
  
  for (i in 1:length(unique(pdt$model))){
    
    this_model <- unique(pdt$model)[i]
    
    sub_pdt <- pdt[model == this_model]
    
    others <- bam(cnt_others ~ s(new_id, by = country, bs = "re") + 
                              factor(part_age_group) + 
                              part_gender + hh_size + old +
                              C1 + C2 + C3 + C6 +
                              part_att_likely + part_att_spread + part_att_serious +
                              part_symp_any + risk + mask + vacc +
                              weekday + s(ym, k=4) + s(first_survey, k=3), 
                              data = sub_pdt, 
                              family = nb(link = "log"), 
                              weights = genderageweight_proportion)
                
    others_list[[i]] <- ci_gam(others)
    others_list[[i]]$model <- this_model
    others_list[[i]]$setting <- "others"    
  } 
  others_list <- rbindlist(others_list)
  
  
  #work contacts
  work_list <- list()
  
  for (i in 1:length(unique(pdt$model))){
    
    this_model <- unique(pdt$model)[i]
    
    sub_pdt <- pdt[attend==1 & model == this_model & part_age_group != "70-120"]
    
    work <- bam(cnt_work ~ s(new_id, by = country, bs = "re") + 
                           factor(part_age_group) + 
                           part_gender + hh_size + old +
                           C1 + C2 + C3 + C6 +
                           part_att_likely + part_att_spread + part_att_serious +
                           part_symp_any + risk + mask + vacc +
                           weekday + s(ym, k=4) + s(first_survey, k=3), 
                           data = sub_pdt, 
                           family = nb(link = "log"), 
                           weights = genderageweight_proportion)
    
    work_list[[i]] <- as.data.table(ci_gam(work))
    work_list[[i]]$model <- this_model
    work_list[[i]]$setting <- "work"
  }
  work_list <- rbindlist(work_list)
  
  
  

#add pooled effect 
  gam_out <- rbind(all_list, home_list, others_list, work_list)
  gam_out <- gam_out[coef != "(Intercept)"]
  gam_out <- gam_out[coef != "part_genderother"]
  gam_out <- gam_out[coef != "weekday"]
  
  coefs <- unique(gam_out$coef)
  settings <- unique(gam_out$setting)
  
  pooled <- unique(gam_out[, .(coef, setting)])
  pooled[, model := "pooled"]
  
  
  for (i in 1:length(coefs)){
    for (j in 1:length(settings)){
      
      this_coef <- coefs[i]
      this_set <- settings[j]
      
        if (this_coef == "factor(part_age_group)70-120" & this_set=="work"){
          next
        }
      
      meta <- metagen(TE = est,
                      seTE = se,
                      studlab = model,
                      data = gam_out[setting==this_set & coef==this_coef],
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "REML",
                      hakn = TRUE)
      pooled[setting==this_set & coef==this_coef, est := meta$TE.random]
      pooled[setting==this_set & coef==this_coef, se := meta$seTE.random]
      pooled[setting==this_set & coef==this_coef, lci := meta$lower.random]
      pooled[setting==this_set & coef==this_coef, uci := meta$upper.random]
    }
  }
  
  gam_out <- rbind(gam_out, pooled)
  setorder(gam_out, setting, coef, model)
  
  
  
  
  # Save data ---------------------------------------------------------------
  sys_date <- gsub("-", "", Sys.Date())
  file_path <- file.path("data", paste(sys_date, "gam_out.qs", sep = "_"))
  qs::qsave(gam_out, file_path)
  message(paste("saved to:", file_path))  
  
  
  
  
  
  