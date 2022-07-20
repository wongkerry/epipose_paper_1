# figure 1

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(tsibble)
library(ggplot2)
library(extrafont)

options("digits" = 3)
loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

source('r/functions/map_country_group.R')
source('r/functions/color_drsimonj.R')

pdt <- qs::qread("data/dt_all_100d.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]
pdt[, name := map_country_name[country]]

pdt[, ym := yearmonth(date)]

#respondents
surveys <- pdt[, .(N = .N), by = .(name, country, ym)]
surveys[, ym_num := as.numeric(ym)]
surveys[, country := toupper(country)]

  #date labels for plot
  label <- unique(surveys[, .(ym, ym_num)])
  setorder(label, ym_num)
  label[, ym := as.character(ym)]
  label[, ym := substr(ym, 6,8)]
  #label = label[seq(1, nrow(label), 3), ]
  
surveys[, country := paste0(country, " (", name, ")")]

ggplot(surveys, aes(fill=country, y=N, x=ym_num)) + 
       geom_vline(xintercept=611.5) +
       geom_vline(xintercept=623.5) +
       geom_bar(position="stack", stat="identity", alpha=0.7, color="black") + 
       geom_text(aes(x=612.2, y=39000, family = "Segoe UI"), label = "2021", size=4) +
       geom_text(aes(x=624.2, y=39000, family = "Segoe UI"), label = "2022", size=4) +
       scale_fill_drsimonj(palette = "mixed") +
       scale_x_continuous(expand = c(0,0), name = "", breaks = label$ym_num, labels = label$ym) +
       scale_y_continuous(expand = c(0.02, 0.02), name = "Number of survey responses", breaks = seq(0, 40000, 2500)) +
       theme_minimal() +
       guides(fill=guide_legend(title = "", ncol = 1, byrow = FALSE, 
                                override.aes = list(size = 0.1))) +
       theme(legend.position = "right", 
             legend.text=element_text(size=rel(0.7)),
             legend.margin = margin(0, 0, 0, 0), 
             text = element_text(family = "Segoe UI", size=15)) 
                  
ggsave(filename = "outputs/figure_1.png", figure_1, units = "cm", width = 16, height = 9)
