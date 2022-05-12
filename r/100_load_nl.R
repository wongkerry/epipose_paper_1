##Name: 00_load_uk_g123
##Description: create analysis files for uk and g123 countries

##Output file: dt_1w and dt_2w

#Packages
library(data.table)
library(lubridate)

#quick function
sumna <- function(x) sum(x, na.rm = TRUE)


#location of files 
pt <- qs::qread("C:\\Users\\wkerr\\Dropbox\\lshtm\\CoMix\\epipose_paper_1\\raw data\\nl\\nl_kerry.qs")
pt <- as.data.table(pt)
pt[, country := "nl"]
pt <- pt[sample_type == "adult"]
pt[, weekday := wday(date, label=TRUE, abbr=FALSE)]

pt[, part_wave_uid := paste0(country, "_", panel, wave, "_", part_id)]
pt[, part_id := paste0(country, "_", part_id)]
sum(duplicated(pt$part_wave_uid)) #confirm no duplicated survey responses


#select only the required variables
  symp <- grep("symp", names(pt), value = TRUE)
  symp <- grep("symp_dk", symp, value = TRUE, inv = TRUE)
  symp <- grep("symp_prefer_not_to_answer", symp, value = TRUE, inv = TRUE)
  symp <- grep("symp_no_answer", symp, value = TRUE, inv = TRUE)
  symp <- grep("symp_none", symp, value = TRUE, inv = TRUE)
  pt[part_symp_fatigue==1, part_symp_tired := 1]
  
  risk <- grep("risk", names(pt), value = TRUE)
  
  hhm_age <- grep("hhm_age", names(pt), value = TRUE)
  pt[, hh_size := rowSums(!is.na(.SD)), .SDcols=hhm_age]
  pt[, hh_size := hh_size + 1]
  pt[, old := hhm_age_1 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  pt[, old := hhm_age_2 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  pt[, old := hhm_age_3 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  pt[, old := hhm_age_4 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  pt[, old := hhm_age_5 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  pt[, old := hhm_age_6 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
  
  pt <- cbind(pt[, .(country, panel, wave, survey_round, date, weekday, part_id, part_wave_uid, sample_type,
                     part_age_group, part_gender, 
                     hh_size, old, 
                     part_att_spread, part_att_likely, part_att_serious,
                     part_attend_work_week, part_attend_work_yesterday, part_limit_work_atleast_day,
                     part_face_mask,
                     part_vacc
  )],
  pt[, ..symp],
  pt[, ..risk])

  
#save dt
qs::qsave(pt, "data/dt_nl.qs")
  