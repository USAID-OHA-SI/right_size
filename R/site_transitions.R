##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: flag site shifts
##  LICENCE: MIT
##  DATE:    2020-03-18
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)

# IMPORT ------------------------------------------------------------------

df <- vroom("Data/TX_CURR_IM_Site.csv", col_types = c(.default = "c", value = "d"))


# MUNGE -------------------------------------------------------------------

df <- df %>% 
  arrange(operatingunit, orgunituid, period) %>% 
  select(operatingunit, orgunituid, period, mech_code, fundingagency, value)

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
    mutate(flag_end_sitexmech = period == last_obs_sitexmech & period != "FY20Q1")

#end type
  df <- df %>% 
    group_by(orgunituid) %>% 
    mutate(end_type = case_when(flag_end_sitexmech == TRUE & last_obs_sitexmech == last_obs_site ~ "Transition out of PEPFAR",
                                flag_end_sitexmech == TRUE & flag_multimech_site == TRUE ~ "Consolidate mutli-mechanism site",
                                flag_end_sitexmech == TRUE & fundingagency != lead(fundingagency, order_by = period) ~ "Transition to other agency",
                                flag_end_sitexmech == TRUE & mech_code != lead(mech_code, order_by = period) ~ "Transition to other mechanism")) %>% 
    ungroup()
  
#identify agency transitions
  df <- df %>% 
    group_by(orgunituid) %>%
    mutate(agency_exiting = case_when(end_type == "Transition to other agency" ~ fundingagency),
           agency_inheriting = case_when(end_type == "Transition to other agency" ~ lead(fundingagency, order_by = period))) %>% 
    ungroup()


# MERGE -------------------------------------------------------------------

  #import mechanism info
    df_org <- list.files("Data", "orghierarchy", full.names = TRUE) %>% 
      vroom(col_types = c(.default = "c")) %>% 
      select(-c(latitude, longitude, orgunit, level))
  
  #merge
    df_full <- df %>% 
      select(-operatingunit) %>%
      right_join(df_org, .)
    
    

# EXPORT ------------------------------------------------------------------

  dir.create("Dataout")  
  
  #all ending sites
    df_end <- df_full %>% 
      filter(fundingagency != "Dedupe adjustments Agency",
             flag_end_sitexmech == TRUE,
             period != "FY17Q4")
  #all transitioning out of PEPFAR
    df_end_out <- df_end %>% 
      filter(end_type == "Transition out of PEPFAR") %>% 
      select(orgunituid:value) 
  #all multi mechanism sites
    df_multimechs <- df_full %>% 
      filter(flag_multimech_site == TRUE) %>% 
      select(orgunituid:flag_multimech_site) 
    
  #export
    write_csv(df_end_out, "Dataout/TXCURR_SiteTransitions_OutofPEPFAR.csv", na = "")
    write_csv(df_end, "Dataout/TXCURR_SiteTransitions.csv", na = "")
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
  