##Name: 00_load_uk_g123
##Description: create analysis files for uk and g123 countries

##Output file: dt_1w and dt_2w

#Packages
library(data.table)
library(lubridate)

#quick function
sumna <- function(x) sum(x, na.rm = TRUE)


#location of files 
load(file="C:\\Users\\wkerr\\Dropbox\\lshtm\\CoMix\\epipose_paper_1\\raw data\\de\\COVIMOD_data_2022-03-23.Rda")
pt <- as.data.table(COVIMOD_data)
pt[, country := "de"]
pt <- pt[!part_age_group %in% c("<1", "1-4", "5-9", "10-14", "15-19")]
pt[, sample_type := "adult"]

pt[, panel := "A"]
pt[, survey_round := wave]
pt[, part_wave_uid := paste0(country, "_", panel, wave, "_", part_id)]
pt[, part_id := paste0(country, "_", part_id)]
pt[, weekday := wday(date, label=TRUE, abbr=FALSE)]
pt[, part_id := paste0(country, "_", part_id)]

  #DE had different age categories - change to match with the other countries
  pt[between(part_age, 0, 4),    part_age_group_new := "0-4"]
  pt[between(part_age, 5, 11),   part_age_group_new := "5-11"]
  pt[between(part_age, 12, 17),  part_age_group_new := "12-17"]
  pt[between(part_age, 18, 29),  part_age_group_new := "18-29"]
  pt[between(part_age, 30, 39),  part_age_group_new := "30-39"]
  pt[between(part_age, 40, 49),  part_age_group_new := "40-49"]
  pt[between(part_age, 50, 59),  part_age_group_new := "50-59"]
  pt[between(part_age, 60, 69),  part_age_group_new := "60-69"]
  pt[between(part_age, 70, 120), part_age_group_new := "70-120"]
  
  pt[is.na(part_age_group_new) & part_age_group %in% c("20-24"), part_age_group_new := "18-29"]
  pt[is.na(part_age_group_new) & part_age_group %in% c("70-74", "75-79", "80-84", "85+"), part_age_group_new := "70-120"]
  
  pt[, table(part_age_group_new, part_age_group, useNA = "always")] #check what remains
  pt[is.na(part_age_group_new) & part_age_group %in% c("25-34"), part_age_group_new := "30-39"] #147 obs
  pt[is.na(part_age_group_new) & part_age_group %in% c("35-44"), part_age_group_new := "40-49"] #111 obs
  pt[is.na(part_age_group_new) & part_age_group %in% c("45-54"), part_age_group_new := "50-59"] #50 obs
  pt[is.na(part_age_group_new) & part_age_group %in% c("55-64"), part_age_group_new := "50-59"] #13 obs
  pt[is.na(part_age_group_new) & part_age_group %in% c("65-69"), part_age_group_new := "60-69"] #13 obs
  
  setnames(pt, "part_age_group", "part_age_group_old")
  setnames(pt, "part_age_group_new", "part_age_group")

#create hh vars
pt[, hh_size := hh_p_incl_0]
pt[, old := NA]
pt[is.na(old), old := hhld_age_1 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_2 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_3 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_4 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_5 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_6 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_7 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_8 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_9 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_10 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_11 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[is.na(old), old := hhld_age_12 %in% c("65-69", "70-74", "75-79", "80-84", "85 years or older", "85+")]
pt[, old := as.numeric(old)]

  #create contact vars
  Q75 <- grep("Q75", names(pt), value = TRUE)
  Q76 <- grep("Q76", names(pt), value = TRUE)
  pt[, (Q75):=lapply(.SD, function(i){i[is.na(i)] <- 0; i}), .SDcols = Q75]
  pt[, (Q76):=lapply(.SD, function(i){i[is.na(i)] <- 0; i}), .SDcols = Q76]

  pt[, q_u18_work    := max(Q75_u18_work,    Q76_u18_work), by = part_wave_uid]
  pt[, q_u18_school  := max(Q75_u18_school,  Q76_u18_school), by = part_wave_uid]
  pt[, q_u18_else    := max(Q75_u18_else,    Q76_u18_else), by = part_wave_uid]
  pt[, q_1864_work   := max(Q75_1864_work,   Q76_1864_work), by = part_wave_uid]
  pt[, q_1864_school := max(Q75_1864_school, Q76_1864_school), by = part_wave_uid]
  pt[, q_1864_else   := max(Q75_1864_else,   Q76_1864_else), by = part_wave_uid]
  pt[, q_o64_work    := max(Q75_o64_work,    Q76_o64_work), by = part_wave_uid]
  pt[, q_o64_school  := max(Q75_o64_school,  Q76_o64_school), by = part_wave_uid]
  pt[, q_o64_else    := max(Q75_o64_else,    Q76_o64_else), by = part_wave_uid]
  
  cnts_q <- grep("q_", names(pt), value = TRUE)
  cnts_q <- c("n_cnt", cnts_q)
  
  for (j in cnts_q){
    set(pt,which(is.na(pt[[j]])),j,0)
  }
  
  pt[, cnt := rowSums(.SD), .SDcols = cnts_q]
  pt[, cnt_work := n_cnt_work + Q75_u18_work + Q75_1864_work + Q75_o64_work]
  pt[, cnt_home := n_cnt_home]
  pt[, cnt_others := n_cnt - n_cnt_home - n_cnt_work]

  #select only the required variables
  setnames(pt, "symp_bn", "symp_congestion")
  setnames(pt, "symp_st", "symp_sore_throat")
  setnames(pt, "symp_loss_taste_smell", "symp_loss_senses")
  setnames(pt, "symp_vomit", "symp_nausea")
  
  symp <- grep("symp", names(pt), value = TRUE)
    symp <- grep("symp_dk", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_no_answer", symp, value = TRUE, inv = TRUE)
    symp <- grep("symp_none", symp, value = TRUE, inv = TRUE)
    
  setnames(pt, symp, paste0("part_", symp))
    
  symp <- paste0("part_", symp)
  risk <- grep("risk", names(pt), value = TRUE)
  vacc <- grep("vacc", names(pt), value = TRUE)
    vacc <- grep("child", vacc, value = TRUE, inv = TRUE)

  pt <- cbind(pt[, .(country, panel, wave, survey_round, date, weekday, part_id, part_wave_uid, sample_type,
                     part_age_group, part_gender, 
                     part_att_spread, part_att_likely, part_att_serious,
                     part_workattendance_7days_wave14_33, 
                     part_workattendance_limit_wave1_13, 
                     part_workattendance_yesterday_wave14_33,
                     part_face_mask,
                     hh_size, old,
                     cnt, cnt_home, cnt_work, cnt_others)],
              pt[, ..symp],
              pt[, ..risk],
              pt[, ..vacc])
  
  pt[, part_gender := tolower(part_gender)]
  pt[!part_gender %in% c("male", "female"), part_gender := "other"]
  pt[part_age_group %in% c("70-74", "75-79", "80-84", "85+"), part_age_group := "70-120"]
  
#save dt
qs::qsave(pt, "data/dt_de.qs")
  
