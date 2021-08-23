##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: flag site shifts
##  LICENCE: MIT
##  DATE:    2020-03-18
##  UPDATE:  2021-08-23


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(gophr)

# IMPORT ------------------------------------------------------------------

  #read data
    df <- vroom("Data/TX_CURR_IM_Site.csv", col_types = c(.default = "c", value = "d"))


# MUNGE -------------------------------------------------------------------

  #store hierarchy for later
    df_org <- df %>% 
      distinct(operatingunit, countryname, snu1, psnu, facility, orgunituid)
    
  #limit df for analysis
    df <- df %>% 
      arrange(operatingunit, orgunituid, period) %>% 
      select(operatingunit, orgunituid, period, mech_code, fundingagency, value)
    
  #flag for lone obs 
    df <- df %>% 
      complete(period, nesting(operatingunit, orgunituid, mech_code, fundingagency)) %>% 
      arrange(orgunituid, mech_code, period) %>% 
      group_by(orgunituid, mech_code) %>% 
      mutate(flag_loneobs = !is.na(value) & is.na(lead(value, order_by = period)) & is.na(lag(value, order_by = period))) %>% 
      ungroup() %>% 
      mutate(flag_loneobs = ifelse(period %in% c(min(df$period), max(df$period)), FALSE, flag_loneobs)) %>% 
      filter(!is.na(value))
    
  #flag multi mechanism site
    df <- df  %>% 
      group_by(orgunituid, period) %>% 
      mutate(flag_multimech_site = n() > 1) %>% 
      ungroup()
  
  #identify last site obs
    df <- df %>% 
      group_by(orgunituid) %>% 
      mutate(last_obs_site = max(period)) %>% 
      ungroup() 
    
  #flag sitexmech end
    df <- df %>% 
      group_by(orgunituid, mech_code) %>% 
      mutate(last_obs_sitexmech = max(period)) %>% 
      ungroup() %>% 
      mutate(flag_end_sitexmech = period == last_obs_sitexmech & period != max(df$period))
  
  #end type
    df <- df %>% 
      group_by(orgunituid) %>% 
      mutate(end_type = case_when(flag_end_sitexmech == TRUE & last_obs_sitexmech == last_obs_site ~ "Transition out of PEPFAR",
                                  flag_end_sitexmech == TRUE & flag_multimech_site == TRUE ~ "Consolidate multi-mechanism site",
                                  flag_end_sitexmech == TRUE & fundingagency != lead(fundingagency, order_by = period) ~ "Transition to other agency",
                                  flag_end_sitexmech == TRUE & mech_code != lead(mech_code, order_by = period) ~ "Transition to other mechanism")) %>% 
      ungroup()
    
  #identify agency transitions
    df <- df %>% 
      group_by(orgunituid) %>%
      mutate(agency_exiting = case_when(end_type == "Transition to other agency" ~ fundingagency),
             agency_inheriting = case_when(end_type == "Transition to other agency" ~ lead(fundingagency, order_by = period))) %>% 
      ungroup()
    
  #method (adjusted NN or traditional for multi-mech sites)
    df <- df %>% 
      complete(period, nesting(orgunituid), fill = list(mech_code = "PLACEHOLDER")) %>% 
      group_by(orgunituid) %>% 
      fill(operatingunit, .direction = "downup") %>% 
      ungroup() %>% 
      arrange(operatingunit, orgunituid, period) %>% 
      group_by(orgunituid) %>% 
      mutate(method = case_when(flag_multimech_site == TRUE | lag(flag_multimech_site, order_by = period) == TRUE ~ "standard",
                                TRUE ~ "adjusted"),
             vlc_valid = case_when(flag_multimech_site == TRUE | lag(flag_multimech_site, n = 2, order_by = period) == TRUE ~ FALSE,
                                   is.na(lag(value, n = 2, order_by = period)) ~ FALSE,
                                   TRUE ~ TRUE)) %>% 
      ungroup() %>% 
      filter(mech_code != "PLACEHOLDER")


# MERGE META --------------------------------------------------------------

  #import mechanism info
    df <- rename_official(df)
    
  #reorder
    df <- df %>% 
      select(operatingunit, orgunituid, fundingagency, 
             mech_code, mech_name, primepartner, everything())
    
  #bring org hierarchy data in
    df <- df %>% 
      select(-operatingunit) %>% 
      right_join(df_org, ., by = "orgunituid")
    
    rm(df_org)
  

# EXPORT ------------------------------------------------------------------

  dir_create("Dataout")  
    
  #export all
    write_csv(df, "Dataout/TXCURR_Flags.csv", na = "")
  
  #all ending sites
    df_end <- df %>% 
      filter(fundingagency != "Dedupe adjustments Agency",
             flag_end_sitexmech == TRUE,
             period != "FY17Q4")
    
    write_csv(df_end, "Dataout/TXCURR_SiteTransitions.csv", na = "")
    
  #all transitioning out of PEPFAR
    df_end_out <- df_end %>% 
      filter(end_type == "Transition out of PEPFAR") %>% 
      select(orgunituid:value) 
    
    write_csv(df_end_out, "Dataout/TXCURR_SiteTransitions_OutofPEPFAR.csv", na = "")
    
  #all multi mechanism sites
    lst_multimechs <- df %>% 
      filter(period != "FY17Q4",
             flag_multimech_site == TRUE) %>% 
      distinct(orgunituid) %>% 
      pull()
    
    df_multimechs <- df %>% 
      filter(orgunituid %in% lst_multimechs)
    
    write_csv(df_multimechs, "Dataout/TXCURR_Site_MultiMechs.csv", na = "")


# EXPLORE -----------------------------------------------------------------

  
  #how many site x mech transition over time
    nrow(df_end)
    
  #where are the transitions occuring
  df_end %>% 
    count(operatingunit, sort = TRUE) %>% 
    mutate(cum_share = n/sum(n),
           cum_sum = cumsum(cum_share))
  
  #when are the transitions occuring
  df_end %>% 
    count(period, sort = TRUE) %>% 
    mutate(cum_share = n/sum(n),
           cum_sum = cumsum(cum_share))

  #what are the types of transitions
  df_end %>% 
    count(end_type, sort = TRUE) %>% 
    mutate(cum_share = n/sum(n),
           cum_sum = cumsum(cum_share))
  
  #which agencies are gaining/losing
  df_end %>% 
    filter(end_type == "Transition to other agency") %>% 
    count(agency_exiting)
  df_end %>% 
    filter(end_type == "Transition to other agency") %>% 
    count(agency_inheriting)
  