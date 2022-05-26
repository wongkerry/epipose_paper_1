##Name: 00_load_uk_g123
##Description: create analysis files for uk and g123 countries

##Output file: dt_1w and dt_2w

#Packages
library(data.table)
library(lubridate)

#quick function
sumna <- function(x) sum(x, na.rm = TRUE)

#location of pt files (from 3 files)
filr <- "C:\\Users\\wkerr\\Filr\\Net Folders\\EPH Shared\\Comix_survey\\data\\validated\\"
local <- "C:\\Users\\wkerr\\Dropbox\\lshtm\\CoMix\\epipose_paper_1\\raw data\\be\\"
  
  pt1 <- as.data.table(qs::qread(paste0(filr, "part.qs")))
  pt1 <- pt1[country=="be"]
  pt2 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_12_32_plus\\part_v5_be.qs")))
  pt3 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_33_plus\\part_v5_be.qs")))

pt <- rbind(pt1, pt2, pt3, fill = TRUE)
sum(duplicated(pt$part_wave_uid)) #confirm no duplicated survey responses

  #select only the required variables
  symp <- grep("symp", names(pt), value = TRUE)
    symp <- grep("symp_dk", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_prefer_not_to_answer", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_no_answer", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_none", symp, value = TRUE, inv = TRUE)
    pt[part_symp_fatigue==1, part_symp_tired := 1]
    
  risk <- grep("risk", names(pt), value = TRUE)
  risk <- grep("hhm", risk, value = TRUE, inv = TRUE)
  pt <- cbind(pt[, .(country, panel, wave, survey_round, date, weekday, part_id, part_wave_uid, sample_type,
                     part_age_group, part_gender, 
                     part_att_spread, part_att_likely, part_att_serious,
                     part_attend_work_week, part_attend_work_yesterday, part_limit_work_atleast_day,
                     part_attend_education_week, part_attend_education_yesterday, part_limit_school_atleast_day, 
                     part_face_mask,
                     part_vacc, part_vacc_doses, part_vacc_newdose, part_vacc_newdose_doses
  )],
  pt[, ..symp],
  pt[, ..risk])
  pt[, part_id := paste0(country, "_", part_id)]
  
#load hh file
hh1 <- as.data.table(qs::qread(paste0(filr, "households.qs")))
hh1 <- hh1[country=="be"]  
hh2 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_12_32_plus\\households_v5_be.qs")))
hh3 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_33_plus\\households_v5_be.qs")))

hh <- rbind(hh1, hh2, hh3, fill = TRUE)
  
  #select only the required variables
  hh <- hh[, .(part_wave_uid, hh_size, hhm_age, hhm_age_group)]
  hh <- hh[, old := fifelse(hhm_age_group %in% c("65-69", "70-74", "75-79", "80-84", "85+"),1,0)]
  hh <- hh[, .(hh_size = mean(hh_size), old = max(old)), by = part_wave_uid]
  
pt <- merge(pt, hh, by = "part_wave_uid", all.x = TRUE)
  

#load ct files 
ct1 <- as.data.table(qs::qread(paste0(filr, "contacts.qs")))
ct1 <- ct1[country=="be"]  
ct2 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_12_32_plus\\contacts_v5_be.qs")))
ct3 <- as.data.table(qs::qread(paste0(local, "raw_data_comix_wave_33_plus\\contacts_v5_be.qs")))

ct <- rbind(ct1, ct2, ct3, fill = TRUE)


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


##split and save
##children
qs::qsave(dt[sample_type=="child"], "data/dt_be_child.qs")

##adult
dt <- dt[sample_type == "adult"]
qs::qsave(dt[], "data/dt_be.qs")










