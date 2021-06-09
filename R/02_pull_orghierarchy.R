##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: pull DATIM hierarchy
##  LICENCE: MIT
##  DATE:    2020-03-18
##  UPDATE:  2020-06-09


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(glamr)

# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()


# PULL OU HIERARCHY -------------------------------------------------------

  #identify uids
    ouuids <- identify_ouuids(datim_user(), datim_pwd()) %>% 
      filter(type == "OU") %>%
      pull(uid)
    
  #pull hierarchy
    df_orgs <-map_dfr(.x = ouuids,
                      .f = ~ pull_hierarchy(.x, datim_user(), datim_pwd()))

# EXPORT ------------------------------------------------------------------

  #save
    write_csv(df_orgs, "PEPFAR_orghierarchy.csv", na.rm = TRUE)
