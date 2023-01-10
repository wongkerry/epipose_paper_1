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
dts <- qs::qread("data/20220627_gam_fatigue_out.qs")
setnames(dts, "s.V1", "smooth")

dts[, setting := stringr::str_to_title(setting)]
dts[, setting := factor(setting, levels = c("All", "Home", "Work", "Others"))]
dts[, model := toupper(model)]

dts[, model := factor(model, levels = c("UK", "BE", "NL", "DE", "G1", "G2", "G3"))]
dts[, smooth := exp(smooth)]

rel <- dts[order==1, list(rel = smooth), by = .(model, setting)]

dts <- merge(dts, rel, by = c("model", "setting"))
dts[, smooth := smooth/rel]
dts[, smooth := log(smooth)]


ggplot(dts[order<=20]) + 
  geom_smooth(aes(x=order, y=smooth, group = model, color=model), size=0.5, se = FALSE) +
  geom_hline(yintercept=0, linetype="dotted") +
  facet_wrap(.~setting, nrow=1) +
  scale_y_continuous(name = "Relative difference in \nmean number of contacts", 
                     breaks = seq(-1.5, 0.5, 0.5), limits = c(-1.5, 0.5)) +
  scale_x_continuous(name = "Number of survey responded to per participant", expand = c(0,0), 
                     breaks = c(1,5,10,15,20), limits = c(1,20)) +
  guides(color=guide_legend(title = "", nrow = 1, byrow = FALSE, 
                           override.aes = list(size = 0.1))) +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI", size=14),
        legend.position="bottom")

