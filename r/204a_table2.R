# table_1

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)
library(rcompanion)
library(dplyr)

source('r/functions/map_country_group.R')
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

options("digits" = 2)

pdt <- qs::qread("data/dt_all_100d.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

pdt[cnt>100, cnt := 100]
pdt[cnt_home>100, cnt_home := 100]
pdt[cnt_work>100, cnt_work := 100]
pdt[cnt_others>100, cnt_others := 100]

  #mean
  cnt <- pdt %>%
            group_by(country) %>%
            summarise(mean = mean(cnt),
                      sd = sd(cnt),
                      n = n()) %>%
            mutate(se = sd / sqrt(n),
                   lci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                   uci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
            select(-sd, -n, -se)
  
  cnt_home <- pdt %>%
                  group_by(country) %>%
                  summarise(mean = mean(cnt_home),
                            sd = sd(cnt),
                            n = n()) %>%
                  mutate(se = sd / sqrt(n),
                         lci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                         uci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                  select(-sd, -n, -se)  
  
  cnt_work <- pdt %>%
    group_by(country) %>%
    summarise(mean = mean(cnt_work),
              sd = sd(cnt),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           uci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
    select(-sd, -n, -se)  
  
  cnt_others <- pdt %>%
    group_by(country) %>%
    summarise(mean = mean(cnt_others),
              sd = sd(cnt),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           uci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
    select(-sd, -n, -se)  
  
  cnt <- as.data.table(cnt)
  cnt_home <- as.data.table(cnt_home)
  cnt_work <- as.data.table(cnt_work)
  cnt_others <- as.data.table(cnt_others)
  
  cnt[, setting := "all"]
  cnt_home[, setting := "home"]
  cnt_work[, setting := "work"]
  cnt_others[, setting := "others"]
  
  table2 <- rbind(cnt, cnt_home, cnt_work, cnt_others)
  table2[, uci := round(uci, 2)]
  table2[, lci := round(lci, 2)]
  table2[, ci := paste0(specify_decimal(mean,2), " (", specify_decimal(lci, 2), ",", specify_decimal(uci, 2), ")")]
  table2 <- table2[, .(country, setting, ci)]
  
  #order the countries
  pdt[, first := min(start_date), by = country]
  g0 <- unique(pdt[group=="G0", .(group, country, first)])
  g123 <- unique(pdt[g0g123=="G123", .(group, country, first)])
  setorder(g0, first)
  setorder(g123, group, first)
  order <- rbind(g0, g123)
  order[, order := 1:nrow(order)]
  
  table2 <- merge(table2, order[, .(country, order)])
  setorder(table2, setting, order)
  table2[, country := factor(country)]
  table2$country <- reorder(table2$country, table2$order)
  
  table2 <- dcast(table2, setting ~ country, value.var = "ci")
  
write.table(table2, "outputs/table2.csv", sep=",", row.names = FALSE)


