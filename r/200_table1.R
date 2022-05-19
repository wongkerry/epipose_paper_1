# table_1

## create table 1 of demographic characteristics of study sample
library(data.table)
library(lubridate)

source('r/functions/map_country_group.R')

options("digits" = 3)

pdt <- qs::qread("data/dt_all_100d.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

#count
count0 <- pdt[, .(respondents = uniqueN(part_id), responses = .N), by = all]
count1 <- pdt[, .(respondents = uniqueN(part_id), responses = .N), by = country]
count2 <- pdt[, .(respondents = uniqueN(part_id), responses = .N), by = g0g123]

#age
age0 <- pdt[, prop.table(table(part_age_group, all), "all")*100]
age1 <- pdt[, prop.table(table(part_age_group, country), "country")*100]
age2 <- pdt[, prop.table(table(part_age_group, g0g123), "g0g123")*100]

age0 <- dcast(as.data.table(age0), all ~ part_age_group, value.var = "N" )
age1 <- dcast(as.data.table(age1), country ~ part_age_group, value.var = "N" )
age2 <- dcast(as.data.table(age2), g0g123 ~ part_age_group, value.var = "N" )

#gender
gender0 <- pdt[part_gender!="other", prop.table(table(part_gender, all), "all")*100]
gender1 <- pdt[part_gender!="other", prop.table(table(part_gender, country), "country")*100]
gender2 <- pdt[part_gender!="other", prop.table(table(part_gender, g0g123), "g0g123")*100]

gender0 <- dcast(as.data.table(gender0), all ~ part_gender, value.var = "N" )
gender1 <- dcast(as.data.table(gender1), country ~ part_gender, value.var = "N" )
gender2 <- dcast(as.data.table(gender2), g0g123 ~ part_gender, value.var = "N" )


#hh
hh0 <- pdt[, .(old = mean(old)*100, hh_size = mean(hh_size, na.rm = TRUE)), by = .(all)]
hh1 <- pdt[, .(old = mean(old)*100, hh_size = mean(hh_size, na.rm = TRUE)), by = .(country)]
hh2 <- pdt[, .(old = mean(old)*100, hh_size = mean(hh_size, na.rm = TRUE)), by = .(g0g123)]

#perception
perception0 <- pdt[, .(likely = mean(part_att_likely)*100,
                       serious = mean(part_att_serious)*100,
                       spread = mean(part_att_spread)*100), by = .(all)]
perception1 <- pdt[, .(likely = mean(part_att_likely)*100,
                       serious = mean(part_att_serious)*100,
                       spread = mean(part_att_spread)*100), by = .(country)]
perception2 <- pdt[, .(likely = mean(part_att_likely)*100,
                       serious = mean(part_att_serious)*100,
                       spread = mean(part_att_spread)*100), by = .(g0g123)]

#risk
risk0 <- pdt[, .(mask = mean(mask)*100,
                 vacc = mean(vacc)*100,
                 risk = mean(risk)*100), by = .(all)]
risk1 <- pdt[, .(mask = mean(mask)*100,
                 vacc = mean(vacc)*100,
                 risk = mean(risk)*100), by = .(country)]
risk2 <- pdt[, .(mask = mean(mask)*100,
                 vacc = mean(vacc)*100,
                 risk = mean(risk)*100), by = .(g0g123)]

#symp
symp0 <- pdt[, .(symp_fever       = mean(part_symp_fever)*100,
                 symp_cough       = mean(part_symp_cough)*100,
                 symp_sob         = mean(part_symp_sob)*100, 
                 symp_ache        = mean(part_symp_ache)*100, 
                 symp_congestion  = mean(part_symp_congestion)*100, 
                 symp_sore_throat = mean(part_symp_sore_throat)*100, 
                 symp_tired       = mean(part_symp_tired)*100, 
                 symp_any         = mean(part_symp_any)*100), by = .(all)]
symp1 <- pdt[, .(symp_fever       = mean(part_symp_fever)*100,
                 symp_cough       = mean(part_symp_cough)*100,
                 symp_sob         = mean(part_symp_sob)*100, 
                 symp_ache        = mean(part_symp_ache)*100, 
                 symp_congestion  = mean(part_symp_congestion)*100, 
                 symp_sore_throat = mean(part_symp_sore_throat)*100, 
                 symp_tired       = mean(part_symp_tired)*100, 
                 symp_any         = mean(part_symp_any)*100), by = .(country)]
symp2 <- pdt[, .(symp_fever       = mean(part_symp_fever)*100,
                 symp_cough       = mean(part_symp_cough)*100,
                 symp_sob         = mean(part_symp_sob)*100, 
                 symp_ache        = mean(part_symp_ache)*100, 
                 symp_congestion  = mean(part_symp_congestion)*100, 
                 symp_sore_throat = mean(part_symp_sore_throat)*100, 
                 symp_tired       = mean(part_symp_tired)*100, 
                 symp_any         = mean(part_symp_any)*100), by = .(g0g123)]


#create table, by country
merge_list <- list(count0, age0, gender0, hh0, perception0, risk0, symp0)
table1_all <- Reduce(function(x, y) merge(x, y, all=TRUE), merge_list)
setnames(table1_all, "all", "loc")

merge_list <- list(count1, age1, gender1, hh1, perception1, risk1, symp1)
table1_country <- Reduce(function(x, y) merge(x, y, all=TRUE), merge_list)
setnames(table1_country, "country", "loc")

merge_list <- list(count2, age2, gender2, hh2, perception2, risk2, symp2)
table1_group <- Reduce(function(x, y) merge(x, y, all=TRUE), merge_list)
setnames(table1_group, "g0g123", "loc")

table1 <- rbind(table1_all, table1_country, table1_group)
table1 <- transpose(table1, keep.names = "rn", make.names = "loc")


write.csv(table1, "outputs/supp_table1.csv")
write.csv(table1[, .(rn, all, uk, be, nl, de, G123)], "outputs/table1.csv")



