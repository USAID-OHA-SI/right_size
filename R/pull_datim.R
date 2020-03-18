##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: Identify number of sites with multiple partners FY18Q4-FY20Q1
##  LICENCE: MIT
##  DATE:    2020-03-12
##  UPDATE:  2020-03-18


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)


# GLOBAL VARIABLES --------------------------------------------------------

  myuser <- ""


# DATIM API FUNCTION ------------------------------------------------------

  pull_tx <- function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=pe:2017Q3;2017Q4;2018Q1;2018Q2;2018Q3;2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4&", #period
             "dimension=bw8KHXzxd9i&", #Funding Agency
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical areas, prep targets at community
             "filter=RUkVjD3BsS1:PE5QVF0w4xj&", #Top Level  - Numerator
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    df <- get_datim_targets(core_url, username, password)
    
    return(df)
  }


# IDENTIFY INPUTS FOR API -------------------------------------------------

  #identify site level in each ou
    df_lvls <- identify_levels(username = myuser, password = mypwd(myuser))
  
  #pull orgunit uids
    df_uids <- identify_ouuids(username = myuser, password = mypwd(myuser))
  
  #table for API use
    ctry_list <- left_join(df_lvls, df_uids, by = c("country_name" = "displayName")) %>% 
      select(operatingunit = name3, operatingunituid = id, countryname = country_name, site_lvl = facility)
    
    rm(df_lvls, df_uids)


# PULL DATA ---------------------------------------------------------------

  #run API across all countries for site x IM TX_CURR
    df <- map2_dfr(.x = ctry_list$operatingunituid, 
                   .y = ctry_list$site_lvl, 
                   .f = ~ pull_tx(.x, .y, myuser, mypwd(myuser)))


# MUNGE -------------------------------------------------------------------

  #convert date from CY to FY
    df <- df %>% 
      mutate(Period = Period %>% 
               str_extract("[:alpha:]{3} [:digit:]{4}") %>% 
               str_replace(" ", " 1, ") %>% 
               mdy %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>% 
               as.character() %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q")) 
  
  #rename columns
    df <- df %>% 
      select(-orglvl_1, -orglvl_2) %>% 
      rename(period = Period,
             operatingunit = orglvl_3, 
             snu1 = orglvl_4,
             fundingagency = `Funding Agency`,
             facility = `Organisation unit`, 
             mech = `Funding Mechanism`, 
             indicator = `Technical Area`, 
             value = Value) %>%
      select(period, operatingunit, snu1, starts_with("orglvl"), facility, orgunituid, everything()) 
  
  #separate mech info
    df <- df %>% 
      mutate(mech = str_replace(mech, "^0000(0|1)", "ZZZ - 0000\\1 -")) %>% 
      separate(mech, c(NA, "mech_code", "mech_name"), extra = "merge", remove = FALSE)
    
  df <- mutate(df, value = as.numeric(value))

# EXPORT ------------------------------------------------------------------

  dir.create("Data")
  write_csv(df, "Data/TX_CURR_IM_Site.csv", na = "")
  