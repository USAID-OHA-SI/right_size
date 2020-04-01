##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: gen NET NEW
##  LICENCE: MIT
##  DATE:    2020-03-29
##  UPDATE:  2020-04-01


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(ICPIutilities)

# IMPORT ------------------------------------------------------------------

  #read data
    df <- vroom("Dataout/TXCURR_Flags.csv")

# GEN NET_NEW -------------------------------------------------------------

  #identify multi-mech sites
    lst_multimech_site <- df %>% 
      filter(flag_multimech_site == TRUE) %>% 
      distinct(orgunituid) %>% 
      pull()
    
  #remove all sites historically where there any instance of having muli-mechs
    df <- filter(df, !orgunituid %in% lst_multimech_site)
    
  #rename value to tx_curr
    df <- rename(df, tx_curr = value)
    
  #create a full set of periods for calculating NET NEW
    df <- df %>% 
      mutate(tx_curr_w_zero = tx_curr) %>% 
      complete(period, nesting(orgunituid, mech_code), fill = list(tx_curr_w_zero = 0)) %>% 
      arrange(operatingunit, orgunituid, mech_code, period)
    
  #calc normal NET_NEW
    df <- df %>%
      group_by(orgunituid, mech_code) %>% 
      mutate(tx_net_new = tx_curr_w_zero - lag(tx_curr_w_zero, order_by = period)) %>% 
      ungroup()
      
  #calc adjusted NET_NEW
    df <- df %>% 
      group_by(orgunituid) %>% 
      mutate(tx_net_new_adj = tx_curr_w_zero - lag(tx_curr_w_zero)) %>% 
      ungroup()
  
  #remove tx_curr w/ zero instead of NA
    df <- df %>% 
      select(-tx_curr_w_zero) %>% 
      filter(!is.na(tx_curr))
      

  site_uid <- "KNVM48HI1Qn" #"RSCJ3wnhmYf"
  #check
    df %>% 
      filter(orgunituid == site_uid) %>% 
      select(period, facility, mech_code, tx_curr, tx_net_new, tx_net_new_adj) %>% 
      gather(indicator, value, tx_curr, tx_net_new, tx_net_new_adj) %>% 
      mutate(indicator = factor(indicator, c("tx_curr", "tx_net_new", "tx_net_new_adj"))) %>% 
      spread(period, value)
    
# EXPORT ------------------------------------------------------------------

    write_csv(df, "Dataout/TX_CURR_NN_Calcs.csv", na = "")
