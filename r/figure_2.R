# figure_2

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(rcompanion)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

source('r/functions/map_country_group.R')
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

options("digits" = 2)
loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))


pdt <- qs::qread("data/dt_all_weighted_dmed.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

pdt[cnt>100, cnt := 100]
pdt[cnt_home>100, cnt_home := 100]
pdt[cnt_work>100, cnt_work := 100]
pdt[cnt_others>100, cnt_others := 100]
  
  #mean
  all <- pdt[, .(cnt = mean(cnt), setting = "all"), by = country]
  home <- pdt[, .(cnt = mean(cnt_home), setting = "home"), by = country]
  work <- pdt[, .(cnt = mean(cnt_work), setting = "work"), by = country]
  others <- pdt[, .(cnt = mean(cnt_others), setting = "other"), by = country]
  
figure2 <- rbind(home, work, others)
  
  #order the countries by most all contacts
  setorder(all, cnt)
  all[, order := 1:nrow(all)]
  
  figure2 <- merge(figure2, all[, .(country, order)])
  figure2[, country := factor(country)]
  figure2$country <- reorder(figure2$country, figure2$order)
  figure2[, country := toupper(country)]

ggplot(figure2[setting!="all"], aes(fill=setting, y=cnt, x=country)) + 
  geom_bar(position="stack", stat="identity", alpha=0.7) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "")
  scale_fill_brewer(name = "", palette = "Dark2") +
  theme(legend.position = "topleft", 
        legend.text=element_text(size=rel(0.8)),
        text = element_text(family = "Segoe UI", size=15)) 


