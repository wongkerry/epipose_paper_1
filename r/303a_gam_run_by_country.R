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

Sys.time()

countrylist <- levels(pdt$country)

all <- list()
list <- list()
for(j in 1:length(countrylist)){
  this_country <- countrylist[j]
  sub_pdt <- pdt[country == this_country]
  
  predlist <- data.frame(sub_pdt[, .(C1, C2, C3, C6, part_age_group,
                                     part_gender, hh_size, old, 
                                     part_symp_any, risk, mask, vacc, 
                                     part_att_likely, part_att_spread, part_att_serious)])

  for(i in 1:ncol(predlist)) {       # for-loop over columns
    #print(this_country)
    sub_pdt$exp <- predlist[ , i] 
    out <- bam(cnt ~ s(new_id, bs = "re") + exp +
                      weekday + s(ym, k=3) + s(order, k=3), 
                      data = sub_pdt, 
                      family = ziP(), 
                      weights = genderageweight_proportion)
  
    list[[i]] <- ci_gam(out)
    list[[i]] <- list[[i]][coef != "(Intercept)", ]
    list[[i]] <- list[[i]][coef != "weekday", ]
    list[[i]]$coef <- paste0(list[[i]]$coef, colnames(predlist)[i])
    list[[i]]$coef <- gsub("exp", "", list[[i]]$coef) 

  }
  
  all[[j]] <- rbindlist(list)
  all[[j]]$country <- this_country

}  

Sys.time()


all_list <- rbindlist(all)

all_list$est <- exp(all_list$est)
all_list$lci <- exp(all_list$lci)
all_list$uci <- exp(all_list$uci)

# sys_date <- gsub("-", "", Sys.Date())
# file_path <- file.path("data", paste(sys_date, "all_by_country.qs", sep = "_"))
# qs::qsave(all_list, file_path)
# message(paste("saved to:", file_path))  


#home contacts
home_list <- list()
home_order <- list()

countrylist <- levels(pdt$country)

home <- list()
list <- list()

for(j in 1:length(countrylist)){
  this_country <- countrylist[j]
  sub_pdt <- pdt[country == this_country]
  
  predlist <- data.frame(sub_pdt[, .(C1, C2, C3, C6, part_age_group,
                                     part_gender, hh_size, old, 
                                     part_symp_any, risk, mask, vacc, 
                                     part_att_likely, part_att_spread, part_att_serious)])
  
  for(i in 1:ncol(predlist)) {       # for-loop over columns
    #print(this_country)
    sub_pdt$exp <- predlist[ , i] 
    
    skip_to_next <- FALSE
    tryCatch(
    
    out <- bam(cnt_home ~ s(new_id, bs = "re") + exp +
                 weekday + s(ym, k=3) + s(order, k=3), 
               data = sub_pdt, 
               family = ziP(), 
               weights = genderageweight_proportion), 
    error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    list[[i]] <- ci_gam(out)
    list[[i]] <- list[[i]][coef != "(Intercept)", ]
    list[[i]] <- list[[i]][coef != "weekday", ]
    list[[i]]$coef <- paste0(list[[i]]$coef, colnames(predlist)[i])
    list[[i]]$coef <- gsub("exp", "", list[[i]]$coef) 
    
  }
  
  home[[j]] <- rbindlist(list)
  home[[j]]$country <- this_country
  
}  

home_list <- rbindlist(home)

home_list$est <- exp(home_list$est)
home_list$lci <- exp(home_list$lci)
home_list$uci <- exp(home_list$uci)

sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "home_by_country.qs", sep = "_"))
qs::qsave(home_list, file_path)
message(paste("saved to:", file_path))  


  
  #work contacts
  work_list <- list()
  work_order <- list()
  
  countrylist <- levels(pdt$country)
  
  work <- list()
  list <- list()
  
  for(j in 1:length(countrylist)){
    this_country <- countrylist[j]
    sub_pdt <- pdt[country == this_country]
    
    predlist <- data.frame(sub_pdt[, .(C1, C2, C3, C6, part_age_group,
                                       part_gender, hh_size, old, 
                                       part_symp_any, risk, mask, vacc, 
                                       part_att_likely, part_att_spread, part_att_serious)])
    
    for(i in 1:ncol(predlist)) {       # for-loop over columns
      #print(this_country)
      sub_pdt$exp <- predlist[ , i] 
      
      skip_to_next <- FALSE
      tryCatch(
        
        out <- bam(cnt_work ~ s(new_id, bs = "re") + exp +
                     weekday + s(ym, k=3) + s(order, k=3), 
                   data = sub_pdt, 
                   family = ziP(), 
                   weights = genderageweight_proportion), 
        error = function(e) {skip_to_next <<- TRUE})
      
      if (skip_to_next) {next}
      
      list[[i]] <- ci_gam(out)
      list[[i]] <- list[[i]][coef != "(Intercept)", ]
      list[[i]] <- list[[i]][coef != "weekday", ]
      list[[i]]$coef <- paste0(list[[i]]$coef, colnames(predlist)[i])
      list[[i]]$coef <- gsub("exp", "", list[[i]]$coef) 
      
    }
    
    work[[j]] <- rbindlist(list)
    work[[j]]$country <- this_country
    
  }  
  
  work_list <- rbindlist(work)
  
  work_list$est <- exp(work_list$est)
  work_list$lci <- exp(work_list$lci)
  work_list$uci <- exp(work_list$uci)

sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "work_by_country.qs", sep = "_"))
qs::qsave(work_list, file_path)
message(paste("saved to:", file_path))  


#other contacts
other_list <- list()
other_order <- list()

countrylist <- levels(pdt$country)

other <- list()
list <- list()

for(j in 1:length(countrylist)){
  this_country <- countrylist[j]
  sub_pdt <- pdt[country == this_country]
  
  predlist <- data.frame(sub_pdt[, .(C1, C2, C3, C6, part_age_group,
                                     part_gender, hh_size, old, 
                                     part_symp_any, risk, mask, vacc, 
                                     part_att_likely, part_att_spread, part_att_serious)])
  
  for(i in 1:ncol(predlist)) {       # for-loop over columns
    #print(this_country)
    sub_pdt$exp <- predlist[ , i] 
    
    skip_to_next <- FALSE
    tryCatch(
      
      out <- bam(cnt_others ~ s(new_id, bs = "re") + exp +
                   weekday + s(ym, k=3) + s(order, k=3), 
                 data = sub_pdt, 
                 family = ziP(), 
                 weights = genderageweight_proportion), 
      error = function(e) {skip_to_next <<- TRUE})
    
    if (skip_to_next) {next}
    
    list[[i]] <- ci_gam(out)
    list[[i]] <- list[[i]][coef != "(Intercept)", ]
    list[[i]] <- list[[i]][coef != "weekday", ]
    list[[i]]$coef <- paste0(list[[i]]$coef, colnames(predlist)[i])
    list[[i]]$coef <- gsub("exp", "", list[[i]]$coef) 
    
  }
  
  other[[j]] <- rbindlist(list)
  other[[j]]$country <- this_country
  
}  

other_list <- rbindlist(other)

other_list$est <- exp(other_list$est)
other_list$lci <- exp(other_list$lci)
other_list$uci <- exp(other_list$uci)

sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "other_by_country.qs", sep = "_"))
qs::qsave(other_list, file_path)
message(paste("saved to:", file_path))  