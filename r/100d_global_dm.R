# global DM

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)

source('r/functions/map_country_group.R')

options("digits" = 3)

pdt <- qs::qread("data/dt_all_100c.qs")

#create all variable
pdt[, all := "all"]

#age
pdt[cnt_others<0, cnt_others := 0]

#perception
pdt[, part_att_likely := fifelse(part_att_likely == "Strongly agree",1,0)]
pdt[, part_att_serious := fifelse(part_att_serious == "Strongly agree",1,0)]
pdt[, part_att_spread := fifelse(part_att_spread == "Strongly agree",1,0)]

pdt[is.na(part_att_likely), part_att_likely := 0]
pdt[is.na(part_att_serious), part_att_serious := 0]
pdt[is.na(part_att_spread), part_att_spread := 0]

#mask
pdt[, mask := NA]
pdt[is.na(mask), mask := fifelse(tolower(part_face_mask) == "yes",1,0)]
pdt[is.na(mask), mask := 0]
pdt[, mask := as.numeric(mask)]

#high-risk (self-reported)
pdt[, risk := NA]
pdt[is.na(risk), risk := fifelse(tolower(part_high_risk) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_high_risk_v2) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_med_risk_v2) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_elevated_risk) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_risk_wave1_13) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_medrisk_wave13_33) == "yes",1,0)]
pdt[is.na(risk), risk := fifelse(tolower(part_highrisk_wave13_33) == "yes",1,0)]
pdt[is.na(risk), risk := 0]
pdt[, risk := as.numeric(risk)]

#vaccination
map_yes <- c(
  "yes" = 1,
  "Yes" = 1,
  "1" = 1,
  "no" = 0, 
  "No" = 0,
  "0" = 0
)
pdt[, part_vacc := map_yes[part_vacc]]
pdt[, part_vacc_new_own_doses := map_yes[part_vacc_new_own_doses]]
pdt[, part_vacc_newdose := map_yes[part_vacc_newdose]]

pdt[, vacc := NA]
pdt[is.na(vacc) & part_vacc > 0, vacc := 1]
pdt[is.na(vacc) & part_vacc_doses > 0, vacc := 1]
pdt[is.na(vacc) & part_vacc_new_own_doses > 0, vacc := 1]
pdt[is.na(vacc) & part_vacc_new_own_doses_number > 0, vacc := 1]
pdt[is.na(vacc) & part_vacc_newdose > 0, vacc := 1]
pdt[is.na(vacc) & part_vacc_number_own > 0, vacc := 1]
pdt[is.na(vacc), vacc := 0]
pdt[, vacc := as.numeric(vacc)]

#symptoms
pdt[, part_symp_fever       := map_yes[part_symp_fever]]
pdt[, part_symp_cough       := map_yes[part_symp_cough]]
pdt[, part_symp_sob         := map_yes[part_symp_sob]]
pdt[, part_symp_ache        := map_yes[part_symp_ache]]
pdt[, part_symp_congestion  := map_yes[part_symp_congestion]]
pdt[, part_symp_sore_throat := map_yes[part_symp_sore_throat]]
pdt[, part_symp_tired       := map_yes[part_symp_tired]]
pdt[, part_symp_diarrhoea   := map_yes[part_symp_diarrhoea]]
pdt[, part_symp_fatigue     := map_yes[part_symp_fatigue]]
pdt[, part_symp_bodyaches   := map_yes[part_symp_bodyaches]]
pdt[, part_symp_headache    := map_yes[part_symp_headache]]
pdt[, part_symp_loss_senses := map_yes[part_symp_loss_senses]]
pdt[, part_symp_nausea      := map_yes[part_symp_nausea]]

pdt[part_symp_bodyaches==1, part_symp_ache        := 1]
pdt[part_symp_headache==1, part_symp_ache        := 1]
pdt[part_symp_fatigue==1, part_symp_tired        := 1]

symp <- grep("symp", names(pdt), value = TRUE)
pdt[, (symp) := lapply(.SD, function(x){x[is.na(x)] <- 0; x}), .SDcols = symp]

pdt[, part_symp_any := rowSums(.SD), .SDcols = symp]
pdt[, part_symp_any := fifelse(part_symp_any==0,0,1)]

pdt[, dayweight := fifelse(weekday %in% c("Sunday", "Saturday"), 2/7, 5/7)]


qs::qsave(pdt, "data/dt_all_100d.qs")
