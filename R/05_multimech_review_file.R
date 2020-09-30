##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: develop actionable google sheet with distinct mech pairs
##  LICENCE: MIT
##  DATE:    2020-04-03
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(fs)
library(ICPIutilities)

# IMPORT ------------------------------------------------------------------

  #read data
   df <- vroom("Data/TX_CURR_IM_Site.csv", col_types = c(.default = "c", value = "d"))


# MUNGE -------------------------------------------------------------------

  #limit to just multi mech sites
  df_multi <- df %>% 
    filter(period != "FY17Q4",
           flag_multimech_site == TRUE) 

# df_multi <- df_multi %>% 
#   mutate(fundingagency = str_remove(fundingagency, "HHS/| adjustments Agency")) %>%
#   select(facility, orgunituid, operatingunit, snu1, psnu, period, mech_code, fundingagency) %>% 
#   arrange(snu1, psnu, facility, period, mech_code) %>% 
#   unite(mech_code, c(fundingagency, mech_code), sep = " ") %>% 
#   group_by(snu1, psnu, facility, period) %>% 
#   mutate(n = paste0("ip_", row_number())) %>% 
#   ungroup() %>% 
#   spread(n, mech_code) %>% 
#   unite(mechs, starts_with("ip"), sep = "|", na.rm = TRUE) %>% 
#   spread(period, mechs) %>%
#   mutate(context = NA_character_) %>% 
#   select(facility, `why multi mechs? pops serving? full duplication?` = context, everything())

#create table of distinct pairs + periods
  df_multi <- df_multi %>% 
    mutate(fundingagency = str_remove(fundingagency, "HHS/| adjustments Agency")) %>%
    select(facility, orgunituid, operatingunit, snu1, psnu, period, mech_code, fundingagency) %>% 
    arrange(snu1, psnu, facility, period, mech_code) %>% 
    unite(mech_code, c(fundingagency, mech_code), sep = " ") %>% 
    group_by(snu1, psnu, facility, period) %>% 
    mutate(n = paste0("ip_", row_number())) %>% 
    ungroup() %>% 
    spread(n, mech_code) %>% 
    unite(mechs, starts_with("ip"), sep = "|", na.rm = TRUE) %>% 
    distinct(operatingunit, period, mechs) %>% 
    group_by(mechs) %>% 
    mutate(n = paste0("pd_", row_number())) %>% 
    ungroup() %>% 
    spread(n, period) %>%
    unite(period, starts_with("pd"), sep = ",", na.rm = TRUE) %>% 
    mutate(context = NA_character_) %>% 
    rename(`rationale/context for multi mechs? full duplication?` = context)


# FUNCTION TO CREATE EXCEL TABS -------------------------------------------

  #fill sheet
  
  fill_sht <- function(df, ou_sel){
    
    df_ou <- filter(df, operatingunit == ou_sel)
    
    addWorksheet(wb, sheetName = ou_sel)
    freezePane(wb, sheet = ou_sel, firstRow = TRUE) #, firstCol = TRUE
    writeData(wb, sheet = ou_sel, x = df_ou)
    
    # setColWidths(wb, ou_sel, cols=1:2, widths = 25)
    # setColWidths(wb, ou_sel, cols=7:15, widths = 18)
    setColWidths(wb, ou_sel, cols=1:3, widths = 30)
    setColWidths(wb, ou_sel, cols=2:4, widths = 40)
    
    highlight <- createStyle(fgFill = "#e67e22")
    endrow <- nrow(df_ou) + 1 
    addStyle(wb, ou_sel, highlight, rows = 2:endrow, cols = 4, gridExpand = TRUE)
  }
  

# EXPORT TO EXCEL ---------------------------------------------------------

  #OU list
    lst_ous <- df_multi %>% 
      distinct(operatingunit) %>% 
      arrange(operatingunit) %>% 
      pull()
  
  #create workbook
    wb <- createWorkbook()
    walk(lst_ous, ~ fill_sht(df_multi, .x))
     
  #xport 
    saveWorkbook(wb, "Dataout/MutiMechSiteReview.xlsx", overwrite = TRUE)

