##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: gen NET NEW
##  LICENCE: MIT
##  DATE:    2020-03-29
##  UPDATE:  2020-04-04


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(ICPIutilities)

# IMPORT ------------------------------------------------------------------

  #read data
    df <- vroom("Dataout/TXCURR_Flags.csv")


# REMOVE NON-STANDARD SITES -----------------------------------------------

  #identify multi-mech sites
    lst_multimech_site <- df %>% 
      filter(flag_multimech_site == TRUE) %>% 
      distinct(orgunituid) %>% 
      pull()
    
  #TODO Remove flag_loneobs == TRUE
    

  #remove flags
    df_noflag <- select(df, operatingunit:value)
      
  #remove all sites historically where there any instance of having muli-mechs
    df_sngl_mechxsite <- filter(df_noflag, !orgunituid %in% lst_multimech_site)
    
  #rename value to tx_curr
    df_sngl_mechxsite <- rename(df_sngl_mechxsite, tx_curr = value)
  
  #store var order for export
    lst_order <- names(df_sngl_mechxsite)
  
    rm(df_noflag, lst_multimech_site)
    
# CALCULATE NORMAL NET NEW ------------------------------------------------
    
  #create a full set of periods for calculating NET NEW
    df_complete_mechxsite <- df_sngl_mechxsite %>% 
      complete(period, nesting(orgunituid, mech_code), fill = list(tx_curr = 0)) %>% 
      group_by(mech_code, orgunituid) %>% 
      fill(operatingunit, countryname, snu1, psnu, facility, 
           fundingagency, mech_name, primepartner,
           .direction = "downup") %>% 
      ungroup() %>% 
      arrange(operatingunit, orgunituid, mech_code, period)
    
  #calc normal NET_NEW
    df_complete_nn_orig <- df_complete_mechxsite %>%
      group_by(orgunituid, mech_code) %>% 
      mutate(tx_net_new = tx_curr - lag(tx_curr, order_by = period)) %>% 
      ungroup()
    
    rm(df_complete_mechxsite)

# CALCULATE ADJUSTED NET NEW ----------------------------------------------

  #create a full set of periods for calculating NET NEW
    df_complete_site <- df_sngl_mechxsite %>% 
      complete(period, nesting(orgunituid), fill = list(tx_curr = 0)) %>% 
      group_by(orgunituid) %>% 
      fill(operatingunit, countryname, snu1, psnu, facility, 
           fundingagency, mech_code, mech_name, primepartner,
           .direction = "downup") %>% 
      ungroup() %>% 
      arrange(operatingunit, orgunituid, mech_code, period)
    
  #calc adjusted NET_NEW
    df_complete_nn_adj <- df_complete_site %>% 
      select(-c(operatingunit:primepartner, -mech_code)) %>% 
      arrange(orgunituid, period) %>% 
      group_by(orgunituid) %>%
      mutate(tx_curr_lag_site = lag(tx_curr, order_by = period),
             tx_net_new_adj = tx_curr -  lag(tx_curr, order_by = period)) %>%
      ungroup() 

    rm(df_sngl_mechxsite, df_complete_site)

# JOIN BOTH NET_NEW TYPES -------------------------------------------------

  #cleanup for merging 
    df_complete_nn_adj <- df_complete_nn_adj %>% 
      select(-tx_curr) %>% 
      filter_at(vars(tx_net_new_adj, tx_curr_lag_site), any_vars(!is.na(.) & . != 0))
    
  #join
    df_complete_nn_both <- df_complete_nn_orig %>% 
      left_join(df_complete_nn_adj, by = c("period", "orgunituid", "mech_code"))
    
  #remove  any with all 0/NAs
    df_nn <- df_complete_nn_both %>% 
      filter_at(vars(tx_curr, tx_net_new, tx_net_new_adj), 
                any_vars(!is.na(.) & . != 0))
    
  #repace artifically created zeros for TX_CURR
    df_nn <- mutate_at(df_nn, vars(tx_curr, tx_curr_lag_site), ~ na_if(., 0))
  
  #reorder
    df_nn <- select(df_nn, all_of(lst_order), starts_with("tx"))

  #check
    # site_uid <- "KNVM48HI1Qn"  #"HaDaUb79d7g"  #"KNVM48HI1Qn" #"RSCJ3wnhmYf"
    # df_nn %>%
    #   filter(orgunituid == site_uid) %>%
    #   select(period, facility, mech_code, tx_curr, tx_net_new, tx_net_new_adj) %>%
    #   gather(indicator, value, tx_curr, tx_net_new, tx_net_new_adj) %>%
    #   mutate(indicator = factor(indicator, c("tx_curr", "tx_net_new", "tx_net_new_adj"))) %>%
    #   spread(period, value)
    
  rm(df_complete_nn_orig, df_complete_nn_adj, df_complete_nn_both)
  

# ADD FLAGS BACK IN -------------------------------------------------------

  #select flags to merge on from orig df
    df_flags <- select(df, orgunituid, mech_code, period, flag_loneobs:agency_inheriting)
  
  #merge onto nn
    df_nn_flags <- left_join(df_nn, df_flags, by = c("orgunituid", "mech_code", "period"))
    
  #fill missing (ie where mech has neg net_new after it transitions)
    df_nn_flags <- df_nn_flags %>% 
      group_by(mech_code, orgunituid) %>% 
      fill(flag_loneobs:last_obs_sitexmech, .direction = "downup") %>% 
      ungroup() %>% 
      mutate(flag_end_sitexmech = ifelse(is.na(flag_end_sitexmech), FALSE, flag_end_sitexmech))
     
  rm(df_flags, df_nn) 
  
# EXPORT ------------------------------------------------------------------

  #export
    write_csv(df_nn_flags, "Dataout/TX_CURR_NN_Calcs.csv", na = "")
