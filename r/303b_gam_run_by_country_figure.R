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
library(ggforce)
library(ggpubr)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Source user written scripts -------------------z--------------------------
source('r/functions/bs_group.R')
source('r/functions/map_country_group.R')

# Load contact (bs) data ---------------------------------------------------
dts <- qs::qread("data/20230109_gam_out_by_country.qs")

  #add results of uk, de, be, nl 
  dts2 <- qs::qread("data/20220701_gam_out.qs")
  dts2 <- dts2[!model %in% c("G1", "G2", "G3")]
  setnames(dts2, "model", "country")
  dts <- rbind(dts, dts2)
  
  pdt <- qs::qread("data/dt_all_100d.qs")
  pdt[, G := map_country_group[country]]
  pdt <- unique(pdt[, .(G, country)])

dts <- data.table(merge(pdt, dts, all.x = TRUE))

  combo <- data.table(dts[, table(setting, coef, country)])
  combo[, N := NULL]
  combo[, G := map_country_group[country]]
  
  dts <- data.table(merge(combo[], dts[, G := NULL], all.x = TRUE))
  dts[, G := gsub("G", "Group ", G)]
  dts[, G := paste0(G, " countries")]


dts[grep("part_age", coef), group := "Demographic \ncharacteristics"]
dts[grep("part_gender", coef), group := "Demographic \ncharacteristics"]

dts[grep("hh_size", coef), group := "Household \ncharacteristics"]
dts[grep("old", coef), group := "Household \ncharacteristics"]

dts[grep("^C", coef), group := "Containment measures"]

dts[grep("part_att", coef), group := "Risk perception"]

dts[is.na(group), group := "Risk or \nrisk mitigation"]

dts[, group := factor(group, levels = c("Demographic \ncharacteristics",
                                        "Household \ncharacteristics",
                                        "Containment measures", 
                                        "Risk perception",
                                        "Risk or \nrisk mitigation"))]

dts[, coef := gsub("factor(part_age_group)", "Age: ", coef, fixed=TRUE)]
dts[, coef := gsub("part_gendermale", "Male", coef)]
dts[, coef := gsub("part_age_group", "", coef, fixed=TRUE)]
dts[, coef := gsub("old", "Living with 65+", coef)]
dts[, coef := gsub("hh_size", "No. household members", coef)]
dts[coef=="C1", coef := "C1: \nSchool closure"]
dts[coef=="C2", coef := "C2: \nWorkplace closure"]
dts[coef=="C3", coef := "C3: \nPublic events cancellation"]
dts[coef=="C6", coef := "C6: \nStay-at-home orders"]

dts[, coef := gsub("part_att_", "", coef)]
dts[coef=="likely",  coef := "Likely to \ncatch COVID"]
dts[coef=="serious", coef := "Likely to have serious \nCOVID symptoms"]
dts[coef=="spread",  coef := "Likely to spread COVID \nto someone vulnerable"]

dts[coef=="part_symp_any", coef := "COVID symptoms"]
dts[coef=="risk", coef := "High risk (self-reported"]
dts[coef=="mask", coef := "Use of face covering"]
dts[coef=="vacc", coef := "Vaccinated against COVID"]

# dts <- dts[(setting == "work" & coef=="Age: 70-120"), est := NA]
# dts <- dts[(setting == "work" & coef=="Age: 70-120"), uci := NA]
# dts <- dts[(setting == "work" & coef=="Age: 70-120"), lci := NA]


dts[, setting := factor(stringr::str_to_title(setting), levels = c("All",
                                                                   "Home", 
                                                                   "Work", 
                                                                   "Others"))]

setorder(dts, setting, G, country)

dts[, country := toupper(country)]
dts[, est := log(est)]
dts[, uci := log(uci)]
dts[, lci := log(lci)]

all <- ggplot(dts[uci<4 & setting == "All"]) +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country, col=G), height=0) +
  geom_point(aes(x = est, y = country, col = G), alpha=0.5) +
  scale_y_discrete(limits = rev(unique(dts$country)), name = "") + 
  scale_x_continuous("") +
  scale_color_discrete(name="")+
  theme_bw() +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        strip.text.y.left = element_text(angle = 0, hjust=0, size=10),
        text = element_text(family = "Segoe UI")) 


all1 <- all + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 1) 
all2 <- all + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 2) 
all3 <- all + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 3) 
all4 <- all + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 4) 

all_figure <- ggarrange(all1, all2, all3, all4, nrow=1, 
                        common.legend = TRUE, legend = "bottom")

annotate_figure(all_figure, top = text_grob("Contacts across all settings", family = "Segoe UI"), 
                bottom = text_grob("Relative difference in mean number of contacts", family = "Segoe UI")
)

home <- ggplot(dts[uci<3 & setting == "Home"]) +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country, col=G), height=0) +
  geom_point(aes(x = est, y = country, col = G), alpha=0.5) +
  scale_y_discrete(limits = rev(unique(dts$country)), name = "") + 
  scale_x_continuous("") +
  scale_color_discrete(name="")+
  theme_bw() +
  theme(legend.position = "none",
        strip.placement = "outside",
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        strip.text.y.left = element_text(angle = 0, hjust=0, size=10),
        text = element_text(family = "Segoe UI")) 


home1 <- home + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 1) 
home2 <- home + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 2) 
home3 <- home + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 3) 
home4 <- home + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 4) 

home_figure <- ggarrange(home1, home2, home3, home4, nrow=1,
                         common.legend = TRUE, legend = "bottom")

annotate_figure(home_figure, top = text_grob("Contacts at home", family = "Segoe UI"), 
                bottom = text_grob("Relative difference in mean number of contacts", family = "Segoe UI")
)


work <- ggplot(dts[uci<3 & setting == "Work"]) +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country, col=G), height=0) +
  geom_point(aes(x = est, y = country, col = G), alpha=0.5) +
  scale_y_discrete(limits = rev(unique(dts$country)), name = "") + 
  scale_x_continuous("") +
  scale_color_discrete(name="")+
  theme_bw() +
  theme(legend.position = "none",
        strip.placement = "outside",
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        strip.text.y.left = element_text(angle = 0, hjust=0, size=10),
        text = element_text(family = "Segoe UI")) 
  

work1 <- work + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 1) 
work2 <- work + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 2) 
work3 <- work + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 3) 
work4 <- work + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 4) 

work_figure <- ggarrange(work1, work2, work3, work4, nrow=1,
                         common.legend = TRUE, legend = "bottom")


annotate_figure(work_figure, top = text_grob("Contacts at work", family = "Segoe UI"), 
                bottom = text_grob("Relative difference in mean number of contacts", family = "Segoe UI")
)

                


others <- ggplot(dts[uci<3 & setting == "Others"]) +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_errorbarh(aes(xmin = lci, xmax = uci, y=country, col=G), height=0) +
  geom_point(aes(x = est, y = country, col = G), alpha=0.5) +
  scale_y_discrete(limits = rev(unique(dts$country)), name = "") + 
  scale_x_continuous("") +
  scale_color_discrete(name="")+
  theme_bw() +
  theme(legend.position = "none",
        strip.placement = "outside",
        axis.text.y = element_text(size=8), 
        axis.text.x = element_text(size=8), 
        strip.text.y.left = element_text(angle = 0, hjust=0, size=10),
        text = element_text(family = "Segoe UI")) 


others1 <- others + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 1) 
others2 <- others + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 2) 
others3 <- others + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 3) 
others4 <- others + facet_wrap_paginate(~group+coef, ncol = 2, nrow = 3, page = 4) 

others_figure <- ggarrange(others1, others2, others3, others4, nrow=1,
                           common.legend = TRUE, legend = "bottom")

annotate_figure(others_figure, top = text_grob("Contacts in other settings", family = "Segoe UI"), 
                bottom = text_grob("Relative difference in mean number of contacts", family = "Segoe UI")
)



