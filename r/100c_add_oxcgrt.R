## Name: dm100b_weights_for_avg_contact_data.R
## Description: Add population and age weights to participant data tables.

## Input file: dt_1w and dt_2w
## Output file: dt_2w

# Packages ----------------------------------------------------------------
library(data.table)
library(readxl)

source('r/functions/map_country_group.R')

pdt <- qs::qread("data/dt_all_100b.qs")

# load oxcgrt
oxcgrt <- as.data.table(read.csv("raw data/oxcgrt_latest.csv"))
oxcgrt <- oxcgrt[CountryName=="United Kingdom" & RegionName!="England", CountryName := ""]
oxcgrt <- oxcgrt[, .(CountryName,
                     Date,
                     C1_School.closing,
                     C2_Workplace.closing,
                     C3_Cancel.public.events,
                     C4_Restrictions.on.gatherings,
                     C5_Close.public.transport,
                     C6_Stay.at.home.requirements,
                     C7_Restrictions.on.internal.movement,
                     C8_International.travel.controls)]
setnames(oxcgrt, names(oxcgrt), 
         c("location", "merge_date", "C1", "C2", "C3", "C4","C5", "C6", "C7", "C8"))
oxcgrt[, merge_date := as.character(merge_date)]
oxcgrt[, merge_date := paste0(substr(merge_date,1,4), "-",
                              substr(merge_date,5,6), "-",
                              substr(merge_date,7,8))]

oxcgrt[location == "Slovak Republic", location := "Slovakia"]

#merge pdt and oxcgrt
pdt[, merge_date := as.character(mid_date)]
pdt[, location := map_country_name[country]]
pdt <- merge(pdt, oxcgrt, by = c("location", "merge_date"), all.x = TRUE)


qs::qsave(pdt, "data/dt_all_100c.qs")
message("Saved to: data/dt_all_100c.qs")
