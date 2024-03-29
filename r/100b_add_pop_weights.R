## Name: dm100b_weights_for_avg_contact_data.R
## Description: Add population and age weights to participant data tables.

## Input file: dt_1w and dt_2w
## Output file: dt_2w

# Packages ----------------------------------------------------------------
library(data.table)
library(readxl)

source('r/functions/map_country_group.R')

# Load and shape population data -------------------------
popall <- as.data.table(readxl::read_xlsx(
  file.path("raw data", "WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx"),
  skip = 16))
popall[, part_gender := "other"]

popf <- as.data.table(readxl::read_xlsx(
  file.path("raw data", "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
  skip = 16))
popf[, part_gender := "female"]
popm <- as.data.table(readxl::read_xlsx(
  file.path("raw data", "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
  skip = 16))
popm[, part_gender := "male"]

pop <- rbindlist(list(popall, popf, popm))

setnames(pop, 
         old = c("Region, subregion, country or area *", 
                 "Reference date (as of 1 July)"), 
         new = c("location", "year"))

pop <- pop[year==2020]

pop2 <- melt(pop, id.vars = c("location", "year", "part_gender"), 
             measure.vars = as.character(0:100), 
             variable.name = "age",
             value.name = "estimate")

# age_breaks <- c(0, 4, 11, 17, 29, 39, 49, 59, 69, 120)
# cut(as.numeric(pop$age), age_breaks, right = TRUE)
pop2[, age := as.numeric(as.character(age))]
pop2[, estimate := as.numeric(as.character(estimate))]

pop2[between(age, 0, 4), part_age_group := "0-4"]
pop2[between(age, 5, 11), part_age_group := "5-11"]
pop2[between(age, 12, 17), part_age_group := "12-17"]
pop2[between(age, 18, 29), part_age_group := "18-29"]
pop2[between(age, 30, 39), part_age_group := "30-39"]
pop2[between(age, 40, 49), part_age_group := "40-49"]
pop2[between(age, 50, 59), part_age_group := "50-59"]
pop2[between(age, 60, 69), part_age_group := "60-69"]
pop2[between(age, 70, 120), part_age_group := "70-120"]

pop2[part_age_group %in% c("0-4", "5-11", "12-17"), sample_type := "child"]
pop2[part_age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-69", "70-120"), sample_type := "adult"]

table(pop2$part_age_group, pop2$part_gender, useNA = "always")
table(pop2$sample_type, pop2$part_gender, useNA = "always")

pop3 <- pop2[, .(pop_estimate = sum(estimate)), by = c("location", "part_age_group", "part_gender", "sample_type")]
pop3 <- pop3[, pop_total := sum(pop_estimate), by = c("location", "part_gender", "sample_type")]
pop3[, pop_proportion := pop_estimate / pop_total]

pop3 <- pop3[location %in% c("Austria",
                            "Switzerland",
                            "Denmark",
                            "Estonia",
                            "Spain",
                            "Finland",
                            "France",
                            "Greece",
                            "Croatia",
                            "Hungary",
                            "Italy",
                            "Lithuania",
                            "Malta",
                            "Poland",
                            "Portugal",
                            "Slovenia",
                            "Slovakia",
                            "United Kingdom",
                            "Belgium",
                            "Germany",
                            "Netherlands")]

# Load participant data ---------------------------------------------------
be <- qs::qread("data/dt_be.qs")
de <- qs::qread("data/dt_de.qs")
nl <- qs::qread("data/dt_nl.qs")
uk <- qs::qread("data/dt_uk.qs")
g123 <- qs::qread("data/dt_g123.qs")

pdt <- rbind(be, nl, de, uk, g123, fill = TRUE)
pdt[, start_date := min(date), by = .(country, survey_round)]
pdt[, end_date := max(date), by = .(country, survey_round)]
pdt[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(country, survey_round)]

nrow(pdt[!is.na(part_age_group) & part_gender != "other"])*100/nrow(pdt)

weightlookup <- pdt[!is.na(part_age_group) & part_gender != "other", 
                    .(sample = .N), by = .(country, mid_date, part_gender, part_age_group, sample_type)]
weightlookup[, sample_total := sum(sample), by = .(country, part_gender, mid_date, sample_type)]
weightlookup[, sample_proportion := sample / sample_total]

weightlookupage <- pdt[!is.na(part_age_group), 
                       .(sample = .N), by = .(country, mid_date, part_age_group, sample_type)]
weightlookupage[, sample_total := sum(sample), by = .(country, mid_date, sample_type)]
weightlookupage[, sample_proportion := sample / sample_total]
# Assign weights for "other/unknown" gender by age only
weightlookupage[, part_gender := "other"]

weightlookup <- rbind(weightlookup, weightlookupage)
weightlookup[, location := map_country_name[country]]


## Plot population data to visually check -------------
# library(ggplot2)
# ggplot(pop3, aes(x = factor(part_age_group), y = pop_estimate, fill = part_gender)) +
#   geom_col(position = "dodge")
# ggplot(pop3, aes(x = factor(part_age_group), y = pop_proportion, fill = part_gender)) +
#   geom_col(position = "dodge")
# table(pop3$part_gender, useNA = "always")
# table(weightlookup$part_gender, useNA = "always")
weightlookup2 <- merge(weightlookup, pop3, 
                       by = c("location", "part_age_group", "part_gender", "sample_type"), all.x = TRUE)

weightlookup2[, genderageweight_raw := pop_estimate/sample]
weightlookup2[, genderageweight_proportion := pop_proportion / sample_proportion]
weightlookup2[, location := NULL]

# Merge weights to pdt
pdt <- merge(pdt, weightlookup2, by = c("country", "mid_date", "part_gender", "part_age_group", "sample_type"))
summary(pdt$genderageweight_proportion)


qs::qsave(pdt, "data/dt_all_100b.qs")
message("Saved to: data/dt_all_100b.qs")


