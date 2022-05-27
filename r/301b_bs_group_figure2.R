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
library(zoo)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Source user written scripts ---------------------------------------------
source('r/functions/bs_group.R')
source('r/functions/map_country_group.R')

format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart], year)
}


# Load contact (bs) data ---------------------------------------------------
dts <- qs::qread("data/20220513_bs_means_2w.qs")
dts[, group := map_country_group[country]]
dts[, g0g123 := map_country_g0g123[country]]
dts[, country_panel := paste0(country, "_", panel)]
  
  #order the panels
  order <- dts[, .(first = min(start_date)), by = .(group, country)]
  g0 <- order[group=="G0"]
  setorder(g0, first)
  g123 <- order[group!="G0"]
  setorder(g123, group, first)
  order <- rbind(g0, g123)
  order[, order := seq_len(.N), by = group]
  
dts <- merge(dts, order[, .(country, order)])

#add polymod reference
dts[country=="be", polymod := 11.84]
dts[country=="de", polymod := 7.95]
dts[country=="fi", polymod := 11.06]
dts[country=="uk", polymod := 11.74]
dts[country=="it", polymod := 19.77]
dts[country=="nl", polymod := 13.85]
dts[country=="pl", polymod := 16.31]

p <- ggplot(dts) +
  geom_ribbon(aes(x=mid_date, ymin=lci, ymax=uci, group=country_panel), fill="red", alpha=0.2) +
  geom_line(aes(x=mid_date, y=mean, group=country_panel), color="red", size=0.8) +
  gghighlight(use_direct_label = FALSE) +
  geom_text(aes(x=min(dts$end_date)+40, y=18, label = toupper(country)), family = "Segoe UI", size=6) +
  facet_grid(group~order, drop = TRUE) +
  geom_hline(aes(yintercept=polymod), linetype = "dashed") +
  scale_y_continuous(name = "Mean contacts") +
  scale_x_date(name = "", breaks = date_breaks("3 months"), labels = format_quarters) + 
  theme_minimal() +
  theme(strip.text = element_blank(),
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


