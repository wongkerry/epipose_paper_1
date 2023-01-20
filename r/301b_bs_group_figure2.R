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
  
  paste(c("Jan","Apr","Jul","Oct")[quart], year)
}


# Load contact (bs) data ---------------------------------------------------
dts <- qs::qread("data/20220531_bs_means_2w.qs")
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


dts <- dts[!(country=="uk" & panel == "B")]
dts <- dts[!(country=="uk" & panel == "F")]
dts[, setting := gsub("_genderage", "", setting)]

#dts <- dts[setting != "All"]
dts[, setting := factor(setting, levels = c("Other", "Work", "Home", "All"))]

dts <- dts[, .(country, country_panel, mid_date, end_date, setting, mean, group, order, polymod)]
dts <- dcast(dts[], country + country_panel + mid_date + end_date + group + order + polymod~ setting, value.var = "mean")

dts[, All := Other + Work + Home]

dts <- melt(dts, id = c("country", "country_panel", "mid_date", "end_date", "group", "order", "polymod"))
setnames(dts, "variable", "setting")
setnames(dts, "value", "mean")

setattr(dts[["setting"]],"levels",c("Contacts in other settings (not work and not home)",
                                    "Contacts at work",
                                    "Contacts at home",
                                    "Contacts across all settings"))

dts[, country_name := map_country_name[country]]
dts[, group := gsub("G", "(Group ", group)]
dts[, group := paste0(group, " countries)")]
dts[group == "(Group 0 countries)", group := ""]

p <- ggplot() +
  geom_area(data=dts[setting != "Contacts across all settings"],
            aes(x=mid_date, y=mean, group=interaction(setting, country_panel), fill = setting), size=0.8) +
  geom_text(data=dts[setting != "Contacts across all settings"], 
            aes(x=min(dts$end_date)+50, y=7, label = toupper(country)), 
            family = "Segoe UI", size=5) +
  scale_fill_brewer(palette = "Set2", name = "", guide = guide_legend(reverse = T)) +
  geom_line(data=dts[setting == "Contacts across all settings"],  
            aes(x=mid_date, y=mean, 
                linetype="Contacts across all settings",
                group=interaction(setting, country_panel)), 
            color = "black", size=0.8) +
  scale_linetype_manual(name = "", 
                        values=c(POLYMOD="dotted", "Contacts across all settings"="solid"), 
                        labels=c("Contacts across all settings", 
                                 "Mean number of contacts from POLYMOD study (published in 2008)")) +
  facet_grid(group~order, drop = TRUE, switch = "y") +
  scale_y_continuous(limits = c(0, 8), name = "Daily mean number of contacts") +
  scale_x_date(name = "", breaks = date_breaks("3 months"), labels = format_quarters) + 
  theme_minimal() +
  theme(strip.placement = "outside",
        strip.text.x = element_blank(), 
        strip.text.y.left = element_blank(), 
        axis.title.y = element_text(size=12),
        strip.switch.pad.grid = unit(0, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13),
        axis.text.y = element_text(size=13), 
        legend.position = "bottom",
        #legend.box.margin=margin(-10,-10,-10,-10), 
        text = element_text(family = "Segoe UI", size=14)) +
  guides(fill = guide_legend(order = 1, reverse = T),
         linetype = guide_legend(order = 2))

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



