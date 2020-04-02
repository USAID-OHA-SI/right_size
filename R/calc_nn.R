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
    df_sngl <- filter(df, !orgunituid %in% lst_multimech_site)
    
  #rename value to tx_curr
    df_sngl <- rename(df_sngl, tx_curr = value)
    
  #create a full set of periods for calculating NET NEW
    df_full <- df_sngl %>% 
      mutate(tx_curr_w_zero = tx_curr) %>% 
      complete(period, nesting(orgunituid, mech_code), fill = list(tx_curr_w_zero = 0)) %>% 
      arrange(operatingunit, orgunituid, mech_code, period)
    
  #calc normal NET_NEW
    df_nn <- df_full %>%
      group_by(orgunituid, mech_code) %>% 
      mutate(tx_net_new = tx_curr_w_zero - lag(tx_curr_w_zero, order_by = period)) %>% 
      ungroup()
      
  #calc adjusted NET_NEW
    df_nn <- df_nn %>% 
      group_by(orgunituid) %>% 
      mutate(tx_net_new_adj = tx_curr_w_zero - lag(tx_curr_w_zero)) %>% 
      ungroup()
    
  #create a lag TX_CURR var
    df_nn <- df_nn %>% 
      group_by(orgunituid) %>% 
      mutate(tx_curr_site_lag = lag(tx_curr)) %>% 
      ungroup()
      
  #remove tx_curr w/ zero and any with all 0/NAs
    df_nn <- df_nn %>% 
      select(-tx_curr_w_zero) %>% 
      filter_at(vars(tx_curr, tx_net_new, tx_net_new_adj), 
                any_vars(!is.na(.) & . != 0))

  #fill extra lines where blank due to complete
    df_nn <- df_nn %>% 
      group_by(orgunituid) %>% 
      fill(operatingunit:primepartner, flag_loneobs:flag_end_sitexmech) %>% 
      ungroup()
    
  #check
    # site_uid <- "RSCJ3wnhmYf"  #"HaDaUb79d7g"  #"KNVM48HI1Qn" #"RSCJ3wnhmYf"
    # df_nn %>%
    #   filter(orgunituid == site_uid) %>%
    #   select(period, facility, mech_code, tx_curr, tx_net_new, tx_net_new_adj) %>%
    #   gather(indicator, value, tx_curr, tx_net_new, tx_net_new_adj) %>%
    #   mutate(indicator = factor(indicator, c("tx_curr", "tx_net_new", "tx_net_new_adj"))) %>%
    #   spread(period, value)
    
# EXPORT ------------------------------------------------------------------

    write_csv(df_nn, "Dataout/TX_CURR_NN_Calcs.csv", na = "")
