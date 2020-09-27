##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: pull DATIM hierarchy
##  LICENCE: MIT
##  DATE:    2020-03-18
##  UPDATE:  2020-09-04


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)


# GLOBAL VARIABLES --------------------------------------------------------

  myuser <- ""


# PULL OU HIERARCHY -------------------------------------------------------

  #identify uids
    ouuids <- identify_ouuids(myuser, mypwd(myuser)) %>% 
      dplyr::filter(is.na(regional)) %>%
      dplyr::pull(id)
    
  #pull hierarchy
    df_orgs <- purrr::map_dfr(.x = ouuids,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser)))

# EXPORT ------------------------------------------------------------------

  #save
    hfr_export(df_orgs, "Data", type = "orghierarchy")
