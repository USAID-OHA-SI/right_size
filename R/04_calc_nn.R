##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: gen NET NEW
##  LICENCE: MIT
##  DATE:    2020-03-29
##  UPDATE:  2021-06-09


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(ICPIutilities)

# IMPORT ------------------------------------------------------------------

  #read data
    df <- vroom("Dataout/TXCURR_Flags.csv")


# MUNGE -------------------------------------------------------------------

  #rename value
    df <- rename(df, tx_curr = value)
    
  #remove flags
    df_noflag <- select(df, operatingunit:tx_curr, flag_multimech_site)
    
  #store var order for export
    lst_order <- names(df_noflag)
    lst_order <- lst_order[lst_order != "flag_multimech_site"]
    
# CALCULATE NORMAL NET NEW ------------------------------------------------
    
  #create a full set of periods for calculating NET NEW
    df_complete_mechxsite <- df_noflag %>%
      select(-flag_multimech_site) %>% 
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
    df_complete_site <- df_noflag %>% 
      filter(flag_multimech_site == FALSE) %>% 
      select(-flag_multimech_site) %>% 
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
      mutate(tx_curr_lag_site = lag(tx_curr, n = 1, order_by = period),
             tx_curr_lag2_site = lag(tx_curr, n = 2, order_by = period),
             tx_net_new_adj = tx_curr -  lag(tx_curr, order_by = period)) %>%
      ungroup() 

    rm(df_noflag, df_complete_site)

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
  

# ENUMERATE TRANFERS ------------------------------------------------------

  #calc transfers in and out by partner
    df_nn <- df_nn %>% 
      complete(period, nesting(orgunituid)) %>% 
      mutate(tx_xfer = case_when(is.na(tx_curr) & tx_net_new < 0 ~ tx_net_new)) %>% 
      group_by(orgunituid, period) %>%
      mutate(tx_xfer = case_when(n() == 2 ~ tx_xfer)) %>% 
      fill(tx_xfer, .direction = "downup") %>%
      ungroup() %>% 
      mutate(tx_xfer = ifelse(tx_net_new > 0, -tx_xfer, tx_xfer)) %>% 
      filter(!is.na(mech_code))

      
# ADD FLAGS BACK IN -------------------------------------------------------

  #select flags to merge on from orig df
    df_flags <- select(df, orgunituid, mech_code, period, flag_loneobs:method)
  
  #merge onto nn
    df_nn_flags <- left_join(df_nn, df_flags, by = c("orgunituid", "mech_code", "period"))
    
  #fill missing (ie where mech has neg net_new after it transitions)
    df_nn_flags <- df_nn_flags %>% 
      group_by(mech_code, orgunituid) %>% 
      fill(flag_loneobs:last_obs_sitexmech, method, .direction = "downup") %>% 
      ungroup() %>% 
      mutate(flag_end_sitexmech = ifelse(is.na(flag_end_sitexmech), FALSE, flag_end_sitexmech))
     
  #remove transfers where there are mutliple mechanisms
    df_nn_flags <- df_nn_flags %>% 
      mutate(tx_xfer = ifelse(method == "standard", NA, tx_xfer))
    
  rm(df_flags, df_nn) 
    

# ADD COMBINED NET NEW ----------------------------------------------------

  #var that includes both adj and traditional (for multi)
    df_nn_flags <- df_nn_flags %>% 
      mutate(tx_net_new_adj_plus = ifelse(method == "adjusted", tx_net_new_adj, tx_net_new)) %>% 
      relocate(tx_net_new_adj_plus, .after = tx_net_new_adj)
  
# EXPORT ------------------------------------------------------------------

  #export
    write_csv(df_nn_flags, "Dataout/TX_CURR_NN_Calcs.csv", na = "")
