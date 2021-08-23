##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: pull and structure TX_CURR data
##  LICENCE: MIT
##  DATE:    2020-03-12
##  UPDATE:  2021-08-23


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(fs)
library(glamr)


# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()


# DATIM API FUNCTION ------------------------------------------------------

  pull_tx <- function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    
    pds <- cross3(c(2018:2021), "Q", c(1:4)) %>% 
      map_chr(paste0, collapse = "") %>% 
      sort() %>% 
      .[-1] %>% 
      paste(collapse = ';')
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=pe:",pds,"&", #period
             "dimension=bw8KHXzxd9i:all&", #Funding Agency
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:MvszPTQrUhy;bZOF8bon1dD&", #technical areas - TX_CURR, TX_PVLS
             "dimension=RUkVjD3BsS1&", #Top Level  - Numerator + Denom
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    df <- get_datim_targets(core_url, username, password)
    
    return(df)
  }


# RENAME SELECT ORG LEVEL TO PSNU -----------------------------------------

  rename_psnu <- function(df, lvl){
    
    oldname <- paste0("orglvl_", lvl) 
    
    df <- df %>% 
      filter(psnu_lvl == lvl) %>% 
      rename(psnu = all_of(oldname)) %>% 
      select(-contains("lvl"))
    
    invisible(df)
  }
  

# IDENTIFY INPUTS FOR API -------------------------------------------------

  #table for API use
    ctry_list <- get_outable(datim_user(), datim_pwd()) %>% 
      select(operatingunit, countryname, countryname_uid, 
             psnu_lvl, site_lvl = facility_lvl)

# PULL DATA ---------------------------------------------------------------

  #run API across all countries for site x IM TX_CURR
    df <- map2_dfr(.x = ctry_list$countryname_uid, 
                   .y = ctry_list$site_lvl, 
                   .f = ~ pull_tx(.x, .y, datim_user(), datim_pwd()))


# MUNGE -------------------------------------------------------------------

  #convert date from CY to FY
    df_clean <- df %>% 
      mutate(Period = Period %>% 
               str_extract("[:alpha:]{3} [:digit:]{4}") %>% 
               str_replace(" ", " 1, ") %>% 
               mdy %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>% 
               as.character() %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q")) 
  
  #rename columns
    df_clean <- df_clean %>% 
      select(-orglvl_1, -orglvl_2) %>% 
      rename(period = Period,
             operatingunit = orglvl_3, 
             fundingagency = `Funding Agency`,
             facility = `Organisation unit`, 
             mech = `Funding Mechanism`, 
             indicator = `Technical Area`, 
             numeratordenom = `Top Level`,
             value = Value) %>%
      mutate(snu1 = orglvl_4,
             countryname = ifelse(str_detect(operatingunit, "Region"), snu1,operatingunit),
             numeratordenom = ifelse(numeratordenom == "Top Level Denominator", "D", "N"))
    
  #add psnu lvl to df for renaming purposes
    df_clean <- ctry_list %>% 
      select(countryname, psnu_lvl) %>% 
      left_join(df_clean, ., by = "countryname")
    
  #rename psnu and drop other org_lvls
    df_clean <- map_dfr(unique(ctry_list$psnu_lvl) %>% setdiff(0), 
                   ~ rename_psnu(df_clean, .x))
  
  #reorder vars
    df_clean <- df_clean %>% 
      select(period, operatingunit, countryname, snu1, psnu, 
             facility, orgunituid, everything()) 
  
  #separate mech info
    df_clean <- df_clean %>% 
      mutate(mech = str_replace(mech, "^0000(0|1)", "ZZZ - 0000\\1 -")) %>% 
      separate(mech, c(NA, "mech_code", "mech_name"), extra = "merge", remove = FALSE)
    
  #covert value to numeric
    df_clean <- mutate(df_clean, value = as.numeric(value))

# EXPORT ------------------------------------------------------------------

  dir_create("Data")
    
  #export TX_CURR data
    df_clean %>% 
      filter(indicator == "TX_CURR") %>% 
      write_csv("Data/TX_CURR_IM_Site.csv", na = "")
    
  #export TX_PVLS data
    df_clean %>% 
      filter(indicator == "TX_PVLS") %>% 
      write_csv("Data/TX_PVLS_IM_Site.csv", na = "")
  