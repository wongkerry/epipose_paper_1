# gam models

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(mgcv)
library(meta)
library(tsibble)

source('r/functions/map_country_group.R')
source('r/functions/ci_gam.R')

options("digits" = 3)

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

pdt[, part_id := gsub("de_de_", "de_", part_id)]
pdt[, new_id := .GRP, by = .(country, panel, part_id)]
pdt[, new_id := as.numeric(new_id)]

pdt[, part_id := substr(part_id, 4, nchar(part_id))]
pdt[, part_id := as.numeric(part_id)]
pdt[, country := factor(country)]

setorder(pdt, country, panel, new_id, survey_round)
pdt[, `:=`(order = 1:.N), by = .(country, panel, new_id)]

pdt <- pdt[part_gender != "other"]

pdt <- pdt[!is.na(hh_size)]

  
  #all contacts
  all_list <- list()
  all_order <- list() #this list captures survey fatigue effect
  
  pdt <- pdt[!country %in% c("uk", "de", "nl", "be")]
  
  for (i in 1:length(unique(pdt$country))){
    
    this_country <- unique(pdt$country)[i]
    sub_pdt <- pdt[country == this_country]
    
    skip_to_next <- FALSE
    tryCatch(
      
      all <- bam(cnt ~ s(new_id, by = country, bs = "re") + 
                   factor(part_age_group) + 
                   part_gender + hh_size + old +
                   C1 + C2 + C3 + C6 +
                   part_att_likely + part_att_spread + part_att_serious +
                   part_symp_any + risk + mask + vacc +
                   weekday + s(ym, k=4) + s(order, k=3), 
                 data = sub_pdt, 
                 family = ziP(), 
                 weights = genderageweight_proportion),
      error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    all_list[[i]] <- ci_gam(all)
    all_list[[i]]$country <- this_country
    all_list[[i]]$setting <- "all"  
    all_list[[i]] <- data.table(all_list[[i]])
    
  
  }
  all_list <- rbindlist(all_list)
  
  
  #home contacts
  home_list <- list()
  
  pdt <- pdt[!country %in% c("uk", "de", "nl", "be")]
  
  for (i in 1:length(unique(pdt$country))){
    
    this_country <- unique(pdt$country)[i]
    sub_pdt <- pdt[country == this_country]
    
    skip_to_next <- FALSE
    tryCatch(
      
      home <- bam(cnt_home ~ s(new_id, by = country, bs = "re") + 
                   factor(part_age_group) + 
                   part_gender + hh_size + old +
                   C1 + C2 + C3 + C6 +
                   part_att_likely + part_att_spread + part_att_serious +
                   part_symp_any + risk + mask + vacc +
                   weekday + s(ym, k=4) + s(order, k=3), 
                 data = sub_pdt, 
                 family = ziP(), 
                 weights = genderageweight_proportion),
      error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    home_list[[i]] <- ci_gam(home)
    home_list[[i]]$country <- this_country
    home_list[[i]]$setting <- "home"  
    home_list[[i]] <- data.table(home_list[[i]])
    
    
  }
  home_list <- rbindlist(home_list)
  
  
  
  #work contacts
  work_list <- list()
  
  pdt <- pdt[!country %in% c("uk", "de", "nl", "be")]
  
  for (i in 1:length(unique(pdt$country))){
    
    this_country <- unique(pdt$country)[i]
    sub_pdt <- pdt[part_age_group != "70-120"]
    sub_pdt <- pdt[country == this_country]
    
    skip_to_next <- FALSE
    tryCatch(
      
      work <- bam(cnt_work ~ s(new_id, by = country, bs = "re") + 
                    factor(part_age_group) + 
                    part_gender + hh_size + old +
                    C1 + C2 + C3 + C6 +
                    part_att_likely + part_att_spread + part_att_serious +
                    part_symp_any + risk + mask + vacc +
                    weekday + s(ym, k=4) + s(order, k=3), 
                  data = sub_pdt, 
                  family = ziP(), 
                  weights = genderageweight_proportion),
      error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    work_list[[i]] <- ci_gam(work)
    work_list[[i]]$country <- this_country
    work_list[[i]]$setting <- "work"  
    work_list[[i]] <- data.table(work_list[[i]])
    
    
  }
  work_list <- rbindlist(work_list)
  
  
  
  #others contacts
  others_list <- list()
  
  pdt <- pdt[!country %in% c("uk", "de", "nl", "be")]
  
  for (i in 1:length(unique(pdt$country))){
    
    this_country <- unique(pdt$country)[i]
    sub_pdt <- pdt[country == this_country]
    
    skip_to_next <- FALSE
    tryCatch(
      
      others <- bam(cnt_others ~ s(new_id, by = country, bs = "re") + 
                    factor(part_age_group) + 
                    part_gender + hh_size + old +
                    C1 + C2 + C3 + C6 +
                    part_att_likely + part_att_spread + part_att_serious +
                    part_symp_any + risk + mask + vacc +
                    weekday + s(ym, k=4) + s(order, k=3), 
                  data = sub_pdt, 
                  family = ziP(), 
                  weights = genderageweight_proportion),
      error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    others_list[[i]] <- ci_gam(others)
    others_list[[i]]$country <- this_country
    others_list[[i]]$setting <- "others"  
    others_list[[i]] <- data.table(others_list[[i]])
    
    
  }
  others_list <- rbindlist(others_list)
  
  


gam_out <- rbind(all_list, home_list, others_list, work_list)
gam_out <- gam_out[coef != "(Intercept)"]
gam_out <- gam_out[coef != "weekday"]
gam_out[est==0, se := NA]
gam_out[est==0, lci := NA]
gam_out[est==0, uci := NA]
gam_out[est==0, est := NA]

gam_out$est <- exp(gam_out$est)
gam_out$lci <- exp(gam_out$lci)
gam_out$uci <- exp(gam_out$uci)

coefs <- unique(gam_out$coef)
settings <- unique(gam_out$setting)
  
# Save data ---------------------------------------------------------------
sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "gam_out_by_country.qs", sep = "_"))
qs::qsave(gam_out, file_path)
message(paste("saved to:", file_path))  

