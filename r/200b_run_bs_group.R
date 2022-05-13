## Name: dm101_calc_avg_contact_data
## Description: Calculate and save the mean contacts over time.

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs
set.seed(20220513)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')

# Load participant data ---------------------------------------------------
pdt <- qs::qread("data/dt_all_weighted_dmed.qs")
pdt[, group := map_country_group[country]]
pdt[, g0g123 := map_country_g0g123[country]]

#truncate at 100
pdt[cnt>100, cnt := 100]
pdt[cnt_home>100, cnt_home := 100]
pdt[cnt_work>100, cnt_work := 100]
pdt[cnt_others>100, cnt_others := 100]

# Define boots ------------------------------------------------------------
args <- commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 1) boots <- as.numeric(args)
if (!exists("boots")) boots <- 1000
boots <- 1000

dt_boot <- data.table()
message(paste("Running", boots, "bootstrapped samples"))

  # Main analysis (every country-panel)
  each_panel <- unique(pdt[, .(country, panel)])
  for(i in 1:nrow(each_panel)){
    country <- each_panel$country[i]
    panel   <- each_panel$panel[i]
    
    dt1 <- bs_group(pdt,  boots, prop = 1.0, country = country, panel = panel)
    dt_boot <- rbind(dt_boot, dt1)
  }

dt_boot[, n := round(median(N)), 
        by = .(country, panel, start_date, mid_date, end_date)]

mea_vars <- c("All_genderage")

l_dt <- melt(dt_boot, id.vars = c("country","panel",
                                  "start_date", "mid_date", "end_date", "survey_round", "n"), 
             measure.vars = mea_vars, variable.name = "setting", value  = "avg")

dts <- l_dt[, .(
  lci = quantile(avg, 0.025, na.rm = T),  
  mean = mean(avg, na.rm = T), 
  uci = quantile(avg, 0.975, na.rm = T), 
  boots = .N),
  by = .(country, panel,
         start_date, mid_date, end_date, setting, n)]


# Save data ---------------------------------------------------------------
sys_date <- gsub("-", "", Sys.Date())
file_path <- file.path("data", paste(sys_date, "bs_means_2w.qs", sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

file_path <- file.path("data","bs_means_country_panel.qs")
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))