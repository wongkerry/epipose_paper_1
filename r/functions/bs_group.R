bs_group <- function(dt, 
                     sims, 
                     prop = 1.0, 
                     country_ = "All", 
                     panel_ = "All"
) {
  
  # subset by panel --------------------------------------------------------
  countryname <- country_

  # subset by panel --------------------------------------------------------
  panelname <- panel_
  
  # Subset by age -----------------------------------------------------------
  dt <- dt[country %in% country_ & panel %in% panel_
  ]
  
  bs_list <- list()
  for(i in 1:sims){
    pids <- unique(dt$part_id)
    nsamp <- length(pids)*prop
    df_samp <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
    # message(paste("pids", length(pids)))
    
    tryCatch({
      samp1 <- merge(df_samp, dt, by = "part_id")
      
    }, error = function(error_condition) {
      
      message("Resampling for merge error")
      message(paste("Error message:", error_condition, sep = "\n"))
      
      df_samp2 <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
      samp1 <- merge(df_samp2, dt, by = "part_id")
      
    })
    
    bs_dt <- samp1[, .(
      N = .N,
      country = countryname,
      panel = panelname,

      iteration = i, 
      # All = weighted.mean(cnt, w = dayweight),
      All_genderage = weighted.mean(cnt, w = dayweight * genderageweight_raw)
      
      # Home = weighted.mean(cnt_home,  w = dayweight),
      # Home_genderage = weighted.mean(cnt_home, w = dayweight * genderageweight_raw),
      # 
      # Work = weighted.mean(cnt_work,  w = dayweight),
      # Work_genderage = weighted.mean(cnt_work, w = dayweight * genderageweight_raw),
      # 
      # Other = weighted.mean(cnt_others,  w = dayweight),
      # Other_genderage = weighted.mean(cnt_others, w = dayweight * genderageweight_raw)
    
      
      # Physical = weighted.mean(cnt_phys,  w = dayweight),
      # Inside = weighted.mean(cnt_inside,  w = dayweight),
      # Outside = weighted.mean(cnt_outside,  w = dayweight),
      # `Other house` = weighted.mean(cnt_other_house,  w = dayweight),
      # `Supermarket` = weighted.mean(cnt_supermarket,  w = dayweight),
      # `Bar restaurant` = weighted.mean(cnt_bar_rest,  w = dayweight)
    ),
    by = .(start_date, mid_date, end_date, survey_round)
    ]
    bs_list[[i]] <- bs_dt
  }
  rbindlist(bs_list)
}  