## Plot children contact

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs
set.seed(20220513)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(extrafont)

loadfonts()
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')
source('r/functions/map_country_group.R')

# Load participant data ---------------------------------------------------
uk <- qs::qread("data/dt_uk_child.qs")
g123 <- qs::qread("data/dt_g123_child.qs")
be <- qs::qread("data/dt_be_child.qs")
de <- qs::qread("data/dt_de_child.qs")
nl <- qs::qread("data/dt_nl_child.qs")

pdt <- rbind(uk, g123, be, de, nl, fill = TRUE)

pdt[part_attend_school_yesterday %in% c("Yes", "yes"), school := "yes"]
pdt[part_attend_school_yesterday == "No, but it was open for my child", school := "yes"]
pdt[part_attend_school_yesterday == "no but open for my child", school := "yes"]
pdt[part_attend_school_yesterday == "Not applicable as it was a weekend/holiday/day off", school := "no"]
pdt[part_attend_school_yesterday == "closed", school := "no"]
pdt[part_attend_school_yesterday == "closed - holiday", school := "no"]
pdt[part_attend_school_yesterday == "Not applicable as it was closed", school := "no"]

pdt[part_attend_education_week == "every day", school := "yes"]
pdt[part_attend_education_week == "most days", school := "yes"]
pdt[part_attend_education_week == "1-2 days", school := "yes"]
pdt[part_attend_education_week == "no days - spent at home", school := "no"]
pdt[part_attend_education_week == "no days - absent", school := "no"]

pdt[part_educationattendance_7days_wave14_33 %in% c("Every day", "Most days"), school := "yes"]
pdt[part_educationattendance_7days_wave14_33 == "Once or twice per week", school := "yes"]
pdt[part_educationattendance_7days_wave14_33 == "Did not attend for other reasons", school := "no"]
pdt[part_educationattendance_7days_wave14_33 == "No days - worked from home", school := "no"]

pdt[part_educationattendance_yesterday_wave14_33 == "Yes", school := "yes"]
pdt[part_educationattendance_yesterday_wave14_33 == "No it was closed", school := "no"]
pdt[part_educationattendance_yesterday_wave14_33 == "No, but it was open", school := "no"]
pdt[part_educationattendance_yesterday_wave14_33 == "Not applicable - closed for holidays", school := "no"]

pdt[part_educationattendance_limit_wave1_13 == "Yes", school := "yes"]
pdt[part_educationattendance_limit_wave1_13 == "No", school := "no"]


pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

pdt[, start_date := min(date), by = .(country, survey_round)]
pdt[, end_date := max(date), by = .(country, survey_round)]
pdt[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(country, survey_round)]
pdt[, dayweight := fifelse(weekday %in% c("Sunday", "Saturday"), 2/7, 5/7)]

pdt <- pdt[part_age_group %in% c("Under 1",
                                 "0-4",
                                 "12-17",
                                 "1-3", 
                                 "1-4", 
                                 "11-15", 
                                 "12-15",
                                 "16-17",
                                 "4-7",
                                 "5-11",
                                 "8-10")]
pdt[part_age_group %in% c("0-4", "Under 1", "1-3", "1-4"), age := "0-4"]
pdt[part_age_group %in% c("4-7", "5-11", "8-10"), age := "5-11"]
pdt[part_age_group %in% c("11-15", "12-15", "16-17", "12-17"), age := "12-17"]

#truncate at 100
pdt[cnt>100, cnt := 100]

# Define boots ------------------------------------------------------------
args <- commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 1) boots <- as.numeric(args)
boots <- 1000

dt_boot <- data.table()
message(paste("Running", boots, "bootstrapped samples"))

# Main analysis (every country-panel)
for(j in unique(pdt$country)){
  for(k in c("yes", "no")){
    for(age in c("0-4", "5-11", "12-17", "All")){
      dt1 <- bs_school(pdt,  boots, prop = 1.0, country = j, school = k, age = age)
      dt_boot <- rbind(dt_boot, dt1)
    }
  }
}

dt_boot[, n := round(median(N)), 
        by = .(country, school, age, start_date, mid_date, end_date)]

mea_vars <- c("All_genderage")

l_dt <- melt(dt_boot, id.vars = c("country","school","age",
                                  "start_date", "mid_date", "end_date", "survey_round", "n"), 
             measure.vars = mea_vars, variable.name = "setting", value  = "avg")

dts <- l_dt[, .(
  lci = quantile(avg, 0.025, na.rm = T),  
  mean = mean(avg, na.rm = T), 
  uci = quantile(avg, 0.975, na.rm = T), 
  boots = .N),
  by = .(country, school, age,
         start_date, mid_date, end_date, setting, n)]

dts[, age := factor(age, levels = c("0-4", "5-11", "12-17", "All"))]
dts[ age == "All", age := "All children"]
dts[, school := fifelse(school=="yes", "Yes", "No")]
dts[, school := factor(school)]

# Save data ---------------------------------------------------------------
sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "bs_school.qs", sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))
# Save data ---------------------------------------------------------------


dts <- qs::qread("data/20220526_bs_school.qs")


mean <- dts[, .(mean = mean(mean)), by = .(country, school, age)]
mean[age=="0-4", polymod := 10.21]
mean[age=="5-11", polymod := 14.81]
mean[age=="12-17", polymod := 17.58]
mean$age <- factor(mean$age, levels =c("All children", "0-4", "5-11", "12-17"))

dat_text <- data.table(
  label = c("POLYMOD", "", "", ""),
  age   = c("0-4", "5-11", "12-17", "All children")
)
dat_text[, age := factor(age, levels = c("0-4", "5-11", "12-17", "All children"))]
dat_text$age <- factor(dat_text$age, levels =c("All children", "0-4", "5-11", "12-17"))


ggplot(mean, aes(x=school, y=mean)) +
  geom_violin() +
  geom_jitter(width=0.01, alpha=0.3) +
  stat_summary(fun=median, geom="point", size=2, color="black", shape=3, stroke=1.8, alpha=0.7) +
  geom_hline(aes(yintercept=polymod), linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = toupper(country)), family = "Segoe UI") +
  facet_grid(.~age) +
  scale_x_discrete(name = "Did the child attend school?") +
  scale_y_continuous(name = "Chidren's contact in different countries", limits = c(0,20)) +
  theme_bw() +
  theme(
    text = element_text(family = "Segoe UI", size=16)
  ) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = 10, label = label),
    hjust   = -0.1,
    vjust   = -1,
    family= "Segoe UI"
  )

  
  
