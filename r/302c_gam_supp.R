## Name: 200c_figure2
## take BS contact results and plot over time

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs
set.seed(20220513)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(gghighlight)
library(extrafont)
library(scales)
library(ggh4x)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Source user written scripts ---------------------------------------------
source('r/functions/bs_group.R')
source('r/functions/map_country_group.R')

# Load contact (bs) data ---------------------------------------------------
dts <- qs::qread("data/20220523_gam_out.qs")

dts[, model := factor(toupper(model), levels = c("UK", "BE", "NL", "DE", 
                                                 "G1", "G2", "G3", "POOLED"))]

dts[, pooled := factor(fifelse(model == "POOLED", 1, 0))]

dts[grep("part_age", coef), group := "Demographic \ncharacteristics"]
dts[grep("part_gender", coef), group := "Demographic \ncharacteristics"]

dts[grep("hh_size", coef), group := "Household \ncharacteristics"]
dts[grep("old", coef), group := "Household \ncharacteristics"]

dts[grep("^C", coef), group := "Containment \nmeasures"]

dts[grep("part_att", coef), group := "Risk \nperception"]

dts[is.na(group), group := "Risk or risk mitigation"]

dts[, group := factor(group, levels = c("Demographic \ncharacteristics",
                                        "Household \ncharacteristics",
                                        "Containment \nmeasures", 
                                        "Risk \nperception",
                                        "Risk or risk mitigation"))]

dts[, coef := gsub("factor(part_age_group)", "Age: ", coef, fixed=TRUE)]
dts[, coef := gsub("part_gendermale", "Male", coef)]
dts[, coef := gsub("old", "Living with 65+", coef)]
dts[, coef := gsub("hh_size", "No. household members", coef)]
dts[coef=="C1", coef := "C1: School closure"]
dts[coef=="C2", coef := "C2: Workplace closure"]
dts[coef=="C3", coef := "C3: Public events \ncancellation"]
dts[coef=="C6", coef := "C6: Stay-at-home orders"]

dts[, coef := gsub("part_att_", "", coef)]
dts[coef=="likely",  coef := "Likely to catch COVID"]
dts[coef=="serious", coef := "Likely to have serious \nCOVID symptoms"]
dts[coef=="spread",  coef := "Likely to spread COVID \nto someone vulnerable"]
T
dts[coef=="part_symp_any", coef := "COVID symptoms"]
dts[coef=="risk", coef := "High risk (self-reported"]
dts[coef=="mask", coef := "Use of face covering"]
dts[coef=="vacc", coef := "Vaccinated against COVID"]

dts[, setting := factor(stringr::str_to_title(setting), levels = c("All",
                                                                   "Home", 
                                                                   "Work", 
                                                                   "Others"))]

dts <- dts[, .(coef, est, lci, uci, model, setting)]
dts[, est := round(est, 2)]
dts[, lci := round(lci, 2)]
dts[, uci := round(uci, 2)]
dts[, model := as.character(model)]
dts[, setting := as.character(setting)]
dts <- dcast(dts, coef + model ~ setting, value.var = c("est", "lci", "uci"))

write.table(dts, "outputs/supp_table_forest.csv", sep=",", row.names = FALSE)

