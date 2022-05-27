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
dts[, coef := factor(coef)]

ggplot(dts[], aes(color=pooled, shape=pooled)) +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=model), height=0) +
  geom_point(aes(x = est, y = model), alpha=0.8) +
  scale_y_discrete(limits = rev(levels(dts$model)), name = "") + 
  scale_x_continuous("Estimates of effect sizes") +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c("grey50", "#1b9e77"))+
  scale_fill_manual(values=c( "grey50", "#1b9e77"))+
  facet_nested(group+coef~setting, switch = "y", scale = "free",
               labeller = label_wrap_gen(15)) +
  labs(caption = "(Effect sizes adjusted for fatigue effect, year-month and weekday/weekend)") +
  theme_bw() +
  theme(legend.position = "none",
        strip.placement = "outside",
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        text = element_text(family = "Segoe UI", size=14), 
        strip.text.y.left = element_text(angle = 0, hjust=0, size=10))

