##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: gen NET NEW
##  LICENCE: MIT
##  DATE:    2020-03-29
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(ICPIutilities)

# IMPORT ------------------------------------------------------------------

  #read data
    df <- vroom("Dataout/TXCURR_Flags.csv")



# GEN NET_NEW -------------------------------------------------------------

  #normal NET_NEW
    df <- df %>% 
      mutate(value_zero = value) %>% 
      complete(period, nesting(orgunituid, mech_code), fill = list(value_zero = 0)) %>% 
      arrange(operatingunit, orgunituid, mech_code, period) %>% 
      group_by(orgunituid, mech_code) %>% 
      mutate(net_new = value_zero - lag(value_zero, order_by = period)) %>% 
      ungroup()
      
  #adjusted NET_NEW
    df <- df %>% 
      group_by(orgunituid) %>% 
      mutate(net_new_adj = value_zero - lag(value_zero, order_by = period)) %>% 
      ungroup()
    
    df <- df %>% 
      select(-value_zero) %>% 
      filter(!is.na(value))
      
  #remove multi mechanism
    # df_multi <- df %>% 
    #   filter(flag_multimech_site == TRUE | flag_loneobs == TRUE)
    #   
    # df_nn_adj <- df %>% 
    #   filter(!(flag_multimech_site == TRUE | flag_loneobs == TRUE))



# EXPORT ------------------------------------------------------------------

    write_csv(df, "Dataout/TX_CURR_NN_Calcs.csv", na = "")
