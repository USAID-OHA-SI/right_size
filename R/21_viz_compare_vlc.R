# PROJECT:  right_size
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  compare adjusted VLC with normal MSD calculation
# LICENSE:  MIT
# DATE:     2021-06-11
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------
  

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  df_hist <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_rds()  

  df_rs <- vroom("Dataout/TX_CURR_NN_Calcs.csv",
                 col_types = c(.default = "c",
                               tx_curr = "d",
                               tx_curr = "d",
                               tx_net_new = "d",
                               tx_curr_lag_site = "d",
                               tx_curr_lag2_site = "d",
                               tx_net_new_adj = "d",
                               tx_net_new_adj_plus = "d",
                               tx_xfer = "d",
                               tx_pvls_d = "d",
                               tx_pvls = "d",
                               flag_loneobs = "l",
                               flag_multimech_site = "l",
                               flag_end_sitexmech = "l",
                               vlc_valid = "l"))
  
# MUNGE -------------------------------------------------------------------

  df_msd_tx <- df_msd %>% 
    bind_rows(df_hist) %>% 
    filter((indicator == "TX_CURR" & standardizeddisaggregate == "Total Numerator") |
             (indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator")) %>% 
    rename_official()
    
  df_msd_vlc <- df_msd_tx %>% 
    filter(fiscal_year >= 2018,
           fundingagency != "Dedup") %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, operatingunit, fundingagency, primepartner, indicator) %>% 
    summarize(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() 
  
  
  df_msd_vlc <- df_msd_vlc %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    filter(!period %in% c("FY18Q1", "FY18Q2")) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    arrange(primepartner, period) %>% 
    group_by(primepartner) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = period)) %>% 
    ungroup() %>% 
    select(-tx_curr) %>% 
    filter(str_detect(period, "FY18", negate = TRUE)) %>% 
    mutate(source = "MSD")

  df_rs_vlc <- df_rs %>% 
    filter(str_detect(period, "FY18", negate = TRUE),
           str_detect(fundingagency, "Dedup", negate = TRUE),
           vlc_valid == TRUE) %>% 
    group_by(period, operatingunit, fundingagency, primepartner) %>% 
    summarize(across(c(tx_pvls_d, tx_curr_lag2_site), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(tx_curr_lag2 = tx_curr_lag2_site) %>% 
    mutate(source = "Adjusted")

  df_vlc <- df_msd_vlc %>% 
    bind_rows(df_rs_vlc) %>% 
    clean_agency() %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2) %>% 
    filter(!is.na(vlc))
  
  df_vlc <- df_vlc %>% 
    group_by(operatingunit, fundingagency, primepartner) %>% 
    mutate(latest_tx = ifelse(period == max(period), tx_curr_lag2, 0),
           latest_tx = max(latest_tx)) %>% 
    ungroup() %>% 
    mutate(facet_label = glue("{fundingagency}/{primepartner}\nTX_CURR = {comma(latest_tx,1)}"),
           facet_label = fct_reorder(facet_label, latest_tx, .desc = TRUE),
           top_ln = case_when(source == "Adjusted" ~ vlc)) %>% 
    group_by(operatingunit, fundingagency, primepartner, period) %>% 
    mutate(pd_min = min(vlc, na.rm = TRUE),
           pd_max = max(vlc, na.rm = TRUE)) %>% 
    ungroup()
    
  
  df_vlc %>% 
    filter(operatingunit == "Tanzania") %>% 
    ggplot(aes(period, vlc, group = source, color = source)) +
    geom_ribbon(aes(ymin = pd_min, ymax = pd_max), fill = trolley_grey_light, alpha = .4) +
    geom_line(size = .9) +
    geom_line(aes(y = top_ln), size = 1, na.rm = TRUE) +
    facet_wrap(~facet_label,scales = "free_y") +
    scale_y_continuous(label = percent_format(1)) +
    scale_x_discrete(label = c("FY19", "", "", "",
                               "FY20", "", "", "",
                               "FY21", "")) +
    scale_color_manual(values = c("Adjusted" = scooter, "MSD" = trolley_grey)) +
    labs(x = NULL, y = NULL, color = NULL,
         subtitle = "Comparing VLC rates between Standard and Adjusted values") +
    si_style() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  
  df_vlc %>% 
    filter(operatingunit == "Haiti") %>% 
    ggplot(aes(period, vlc, group = source, color = source)) +
    geom_ribbon(aes(ymin = pd_min, ymax = pd_max), fill = trolley_grey_light, alpha = .4,
                show.legend = FALSE) +
    geom_line(size = .9) +
    geom_line(aes(y = top_ln), size = 1, na.rm = TRUE) +
    facet_wrap(~facet_label,scales = "free_y") +
    scale_y_continuous(label = percent_format(1)) +
    scale_x_discrete(label = c("FY19", "", "", "",
                               "FY20", "", "", "",
                               "FY21", "")) +
    scale_color_manual(values = c("Adjusted" = scooter, "MSD" = trolley_grey)) +
    labs(x = NULL, y = NULL, color = NULL,
         title = toupper("Comparing VLC rates between Standard and Adjusted values"),
         subtitle = glue("<span><span style='color:{scooter}'>**MSD VLC**</span> = OUxIM level, TX_PVLS_D/TX_CURR_LAG2 <br><span style='color:{trolley_grey}'>**ADJUSTED VLC**</span> = Site x IM level, TX_PVLS_D/TX_CURR_LAG2 accounting for site transitions, excludes values where mutli-mechs in current/2 prior period and where no TX_CURR 2 periods prior</span>")) +
    si_style() +
    theme(panel.spacing.x = unit(.5, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          plot.subtitle = element_markdown(),
          legend.position = "none")
  
    