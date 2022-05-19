# table_1

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(extrafont)
library(dplyr)
library(ggplot2)
library(zoo)
library(scales)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Source user written scripts ---------------------------------------------
source('r/functions/map_country_group.R')
format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(year, c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart])
}

options("digits" = 2)

# load oxcgrt
oxcgrt <- as.data.table(read.csv("raw data/oxcgrt_latest.csv"))
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
                 c("location", "date", "C1", "C2", "C3", "C4","C5", "C6", "C7", "C8"))
oxcgrt[, date := as.character(date)]
oxcgrt$date <- as.Date(oxcgrt$date, format = "%Y%m%d")
oxcgrt[location == "Slovak Republic", location := "Slovakia"]

# load participant data
pdt <- qs::qread("data/dt_all_100d.qs")
pdt <- unique(pdt[, .(country, start_date)])
pdt[, location := map_country_name[country]]
setnames(pdt, "start_date", "date")

pdt <- merge(pdt, oxcgrt, by = c("location", "date"), all.x = TRUE)
pdt[, group := map_country_group[country]]
pdt <- melt(pdt, measure.vars = c("C1", "C2", "C3", "C4","C5", "C6", "C7", "C8"),
            variable.name = "rule", value.name = "value")
  
  #order the panels
  order <- pdt[, .(first = min(date)), by = .(group, country)]
  g0 <- order[group=="G0"]
  setorder(g0, first)
  g123 <- order[group!="G0"]
  setorder(g123, group, first)
  order <- rbind(g0, g123)
  order[, order := seq_len(.N), by = group]

pdt <- merge(pdt, order[, .(country, order)])
pdt[, rule := as.numeric(substr(rule,2,2))]

cols <- c("0" = "#00aedb", 
          "1" = "#ffc425", 
          "2" = "#f37735", 
          "3" = "#d11141", 
          "4" = "#8c8c8c") #https://i2.wp .com/svbtleusercontent.com/zxequdizcj5apg.png?ssl=1

  #date labels for plot
  pdt[, date_num := as.numeric(date)]
  label <- unique(pdt[, .(date, date_num)])
  label[, label := format_quarters(date)]
  #label = label[seq(1, nrow(label), 3), ]
  label <- label[, .(date_num = min(date_num)), by = label]
  label <- label[date_num!=18344]

p <- ggplot(pdt) +
  geom_point(aes(x=date_num, y=rule, color=as.character(value)), shape=15) +
  geom_text(aes(x=min(label$date_num)+50, y=9, 
                label = toupper(country)), family = "Segoe UI", size=6) +
  facet_grid(group~order, drop = TRUE) +
  scale_y_continuous(name = "", limits = c(1,9), 
                     breaks = seq(1,8,1), label=paste0("C", seq(1,8,1))) +
  scale_x_continuous(name = "", breaks = label$date_num, labels = label$label)  +
  scale_colour_manual(values = cols) +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        text = element_text(family = "Segoe UI", size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

g <- ggplotGrob(p)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-1-5", "panel-1-6", "panel-1-7", 
                                 "panel-3-6", "panel-3-7", 
                                 "panel-4-6", "panel-4-7")

# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]

grid::grid.newpage()
grid::grid.draw(g)
  

  
  
