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

# Source user written scripts -------------------z--------------------------
source('r/functions/bs_group.R')
source('r/functions/map_country_group.R')

# Load contact (bs) data ---------------------------------------------------
all <- qs::qread("data/20230103_all_by_country.qs")
home <- qs::qread("data/20230104_home_by_country.qs")
work <- qs::qread("data/20230104_work_by_country.qs")
other <- qs::qread("data/20230105_other_by_country.qs")

all[,  setting := "all"]
home[, setting := "home"]
work[, setting := "work"]
other[,setting := "other"]

cnt <- rbind(all, home, work, other)


cnt[grep("part_age", coef), group := "Demographic \ncharacteristics"]
cnt[grep("part_gender", coef), group := "Demographic \ncharacteristics"]

cnt[grep("hh_size", coef), group := "Household \ncharacteristics"]
cnt[grep("old", coef), group := "Household \ncharacteristics"]

cnt[grep("^C", coef), group := "Containment \nmeasures"]

cnt[grep("part_att", coef), group := "Risk \nperception"]

cnt[is.na(group), group := "Risk or risk mitigation"]

cnt[, group := factor(group, levels = c("Demographic \ncharacteristics",
                                        "Household \ncharacteristics",
                                        "Containment \nmeasures", 
                                        "Risk \nperception",
                                        "Risk or risk mitigation"))]

cnt[, coef := gsub("part_age_group", " (age)", coef, fixed=TRUE)]
cnt[, coef := gsub("part_gendermale", "Male", coef)]
cnt[, coef := gsub("old", "Living with 65+", coef)]
cnt[, coef := gsub("hh_size", "No. household members", coef)]
cnt[coef=="C1", coef := "C1: School closure"]
cnt[coef=="C2", coef := "C2: Workplace closure"]
cnt[coef=="C3", coef := "C3: Public events \ncancellation"]
cnt[coef=="C6", coef := "C6: Stay-at-home orders"]

cnt[, coef := gsub("part_att_", "", coef)]
cnt[coef=="likely",  coef := "Likely to catch COVID"]
cnt[coef=="serious", coef := "Likely to have serious \nCOVID symptoms"]
cnt[coef=="spread",  coef := "Likely to spread COVID \nto someone vulnerable"]

cnt[coef=="part_symp_any", coef := "COVID symptoms"]
cnt[coef=="risk", coef := "High risk (self-reported"]
cnt[coef=="mask", coef := "Use of face covering"]
cnt[coef=="vacc", coef := "Vaccinated against COVID"]

cnt[, setting := factor(stringr::str_to_title(setting), levels = c("All",
                                                                   "Home", 
                                                                   "Work", 
                                                                   "Others"))]
cnt[, coef := factor(coef)]

ggplot(cnt[setting=="all"]) +
  geom_vline(xintercept=1, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country), height=0) +
  geom_point(aes(x = est, y = country), alpha=0.5) +
  scale_y_discrete(limits = rev(levels(cnt$country)), name = "") + 
  scale_x_continuous("Relative difference in mean number of contacts") +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c("grey50", "#1b9e77"))+
  scale_fill_manual(values=c( "grey50", "#1b9e77"))+
  facet_wrap(group+coef~.) 



ggplot(home[est<2]) + 
  geom_vline(xintercept=1, linetype="dotted") +
  geom_point(aes(x=est, y=country)) + 
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country), height=0) +
  facet_wrap(.~coef) 

ggplot(work[est<2]) + 
  geom_vline(xintercept=1, linetype="dotted") +
  geom_point(aes(x=est, y=country)) + 
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country), height=0) +
  facet_wrap(.~coef) 

ggplot(other[est<2]) + 
  geom_vline(xintercept=1, linetype="dotted") +
  geom_point(aes(x=est, y=country)) + 
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country), height=0) +
  facet_wrap(.~coef) 
