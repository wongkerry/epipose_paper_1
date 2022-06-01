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
dts <- qs::qread("data/20220527_gam_fatigue_out.qs")
setnames(dts, "s.V1", "smooth")

dts[, setting := stringr::str_to_title(setting)]
dts[, setting := factor(setting, levels = c("All", "Home", "Work", "Others"))]
dts[, model := toupper(model)]

dts[, model := factor(model, levels = c("UK", "BE", "NL", "DE", "G1", "G2", "G3"))]
      
ggplot(dts[order<=20]) + 
  geom_line(aes(x=order, y=smooth, group=model, color=model)) +
  geom_line(aes(x=order, y=smooth, group=model, color=model)) +
  geom_hline(yintercept=0, linetype="dotted") +
  facet_wrap(.~setting, nrow=1) +
  scale_y_continuous(name = "Effect size") +
  scale_x_continuous(name = "Number of survey responded") +
  guides(color=guide_legend(title = "", nrow = 1, byrow = FALSE, 
                           override.aes = list(size = 0.1))) +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI", size=14),
        legend.position="bottom")

