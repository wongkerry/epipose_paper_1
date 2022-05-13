##Name: 00_load_uk_g123
##Description: create analysis files for uk and g123 countries

##Output file: dt_1w and dt_2w

#Packages
library(data.table)
library(lubridate)

#quick function
sumna <- function(x) sum(x, na.rm = TRUE)

#location of files 
path <- "C:\\Users\\wkerr\\Filr\\Net Folders\\EPH Shared\\Comix_survey\\data\\validated\\"

#load pt files 
pt <- as.data.table(qs::qread(paste0(path, "part.qs")))
pt <- pt[!country %in% c("no", "be", "nl")]
pt <- pt[sample_type == "adult"]
  #some children participant marked as adult?
  pt <- pt[!part_age_group %in% c("Under 1", "0-4", "5-11", "12-17")]
  pt <- pt[part_age_group == "18-19", part_age_group := "18-29"]
  pt[part_age_group == "25-34", part_age_group := "30-39"] #2 obs
  pt[part_age_group == "35-44", part_age_group := "40-49"] #1 obs
  pt[part_age_group == "45-54", part_age_group := "50-59"] #1 obs
  
sum(duplicated(pt$part_wave_uid)) #confirm no duplicated survey responses

  #select only the required variables
  symp <- grep("symp", names(pt), value = TRUE)
    symp <- grep("symp_dk", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_prefer_not_to_answer", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_no_answer", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_none", symp, value = TRUE, inv = TRUE)
    pt[part_symp_fatigue==1, part_symp_tired := 1]
  
  risk <- grep("risk", names(pt), value = TRUE)
  risk <- grep("hhm_", risk, value = TRUE, inv = TRUE)
  pt <- cbind(pt[, .(country, panel, wave, survey_round, date, weekday, part_id, part_wave_uid, sample_type,
               part_age_group, part_gender, 
               part_att_spread, part_att_likely, part_att_serious,
               part_attend_work_week, part_attend_work_yesterday, part_limit_work_atleast_day,
               part_face_mask,
               part_vacc, part_vacc_doses, part_vacc_newdose, part_vacc_newdose_doses
               )],
              pt[, ..symp],
              pt[, ..risk])
  pt[, part_id := paste0(country, "_", part_id)]
  
  
#load hh file
hh <- as.data.table(qs::qread(paste0(path, "households.qs")))
hh <- hh[!country %in% c("no", "be", "nl")]

  #select only the required variables
  hh <- hh[, .(part_wave_uid, hh_size, hhm_age, hhm_age_group)]
  hh <- hh[, old := fifelse(hhm_age_group %in% c("65-69", "70-74", "75-79", "80-84", "85+"), 1,0)]
  hh <- hh[, .(hh_size = mean(hh_size), old = max(old)), by = part_wave_uid]
  
pt <- merge(pt, hh, by = "part_wave_uid", all.x = TRUE)
  
    
#load ct files 
ct <- as.data.table(qs::qread(paste0(path, "contacts.qs")))
  
  #calculate contacts per survey response
  cp_n_cnts <- ct[, .(
    cnt = .N,
    cnt_home             = sumna(cnt_home),
    cnt_work             = sumna(cnt_work)
  ),
  by = part_wave_uid]
  cp_n_cnts[, cnt_others := cnt - cnt_home - cnt_work]
  
  
#Add on contacts ---------------------------------------------------------
dt = merge(pt, cp_n_cnts, by = c("part_wave_uid"), all.x = TRUE)
  
var_list <- names(cp_n_cnts)
for (j in var_list){
  set(dt,which(is.na(dt[[j]])),j,0)
}


## Remove round 6 and 7
dt <- dt[!(country == "uk" & survey_round %in% 6:7)]

##split and save
qs::qsave(dt[country=="uk"], "data/dt_uk.qs")
qs::qsave(dt[country!="uk"], "data/dt_g123.qs")

