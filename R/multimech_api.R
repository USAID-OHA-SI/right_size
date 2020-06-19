##  PROJECT: RIGHT SIZE
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: Identify number of sites with multiple partners FY18Q4-FY20Q1
##  LICENCE: MIT
##  DATE:    2020-03-12
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(extrafont)
library(scales)


# GLOBAL VARIABLES --------------------------------------------------------

myuser <- ""


# DATIM API FUNCTION ------------------------------------------------------

pull_tx <- function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
  print(paste("running ", ou_uid, " ... ", Sys.time()))
  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=pe:2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4&", #period
           "dimension=bw8KHXzxd9i&", #Funding Agency
           "dimension=SH885jaRe0o&", #Funding Mechanism
           "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical areas, prep targets at community
           "filter=RUkVjD3BsS1:PE5QVF0w4xj&", #Top Level  - Numerator
           "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
  
  df <- get_datim_targets(core_url, username, password)
  
  return(df)
}


# IDENTIFY INPUTS FOR API -------------------------------------------------

df_lvls <- identify_levels(username = myuser, password = mypwd(myuser))

df_uids <- identify_ouuids(username = myuser, password = mypwd(myuser))

ctry_list <- left_join(df_lvls, df_uids, by = c("country_name" = "displayName")) %>% 
  select(operatingunit = name3, operatingunituid = id, countryname = country_name, site_lvl = facility)

rm(df_lvls, df_uids)



# PULL DATA ---------------------------------------------------------------


df <- map2_dfr(.x = ctry_list$operatingunituid, 
               .y = ctry_list$site_lvl, 
               .f = ~ pull_tx(.x, .y, myuser, mypwd(myuser)))

df %>% glimpse()

# MUNGE -------------------------------------------------------------------

#adjust date
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
         fundingagency = `Funding Agency`,
         facility = `Organisation unit`, 
         mech = `Funding Mechanism`, 
         indicator = `Technical Area`, 
         value = Value) %>%
  select(period, operatingunit, starts_with("orglvl"), facility, orgunituid, everything()) 

df <- mutate(df, value = as.numeric(value))
# EXPORT ------------------------------------------------------------------

# write_csv(df, "../Downloads/TX_CURR_SiteCount.csv", na = "")


# EXPLORE -----------------------------------------------------------------

df <- read_csv("../Downloads/TX_CURR_SiteCount.csv", col_types = c(.default = "c", value = "d"))

df  %>% 
  distinct(period, operatingunit, orgunituid, mech) %>% 
  count(period, operatingunit, orgunituid) %>% 
  count(period, operatingunit, n) %>% 
  spread(n, nn) %>% 
  print(n = Inf)


df %>% 
  select(period, operatingunit, facility, orgunituid, mech, fundingagency, value) %>% 
  group_by(period, orgunituid) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  filter(operatingunit == "Tanzania") %>% 
  arrange(operatingunit, orgunituid, period)


# PLOT --------------------------------------------------------------------


df_site_cnts <- df %>% 
  mutate(country = ifelse(str_detect(operatingunit, "Region"), orglvl_4, operatingunit)) %>% 
  count(period, country, orgunituid) %>% 
  mutate(type = case_when(n > 1 ~ "multiple",
                          TRUE ~ "single")) %>% 
  count(period, country, type)

df_site_multi <- df_site_cnts %>% 
  spread(type, n, fill = 0) %>% 
  group_by(country) %>% 
  mutate(multi_ou = sum(multiple) > 1) %>% 
  ungroup() %>% 
  arrange(country, period) %>% 
  filter(multi_ou == TRUE) %>% 
  mutate(country = fct_reorder(country, multiple, sum, .desc = TRUE),
         lab = case_when(multiple > 0 ~ multiple))

df_site_multi %>% 
  ggplot(aes(period, multiple)) +
  geom_hline(aes(yintercept = 0)) +
  geom_col(fill = "#e34a33") +
  geom_blank(aes(y = 1.3 * multiple)) +
  geom_text(aes(label = lab), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro", color = "gray30", size = 4) +
  facet_wrap(~country) +
  scale_x_discrete(breaks = c("FY18Q4", "FY19Q2", "FY19Q4")) +
  labs(x = NULL, y = NULL,
       title = "16 OPERATING UNITS HAVE SITES WITH MULTIPLE MECHANISMS",
       subtitle = "making it difficult to calculate site adjusted TX_NET_NEW",
       caption = "Source: DATIM API [2020-03-12]") +
  theme_minimal() +
  theme(text = element_text(family = "Source Sans Pro"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        panel.border = element_rect(color = "gray60", fill = NA),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(color = "gray30", size = 9))

ggsave("../Downloads/OUs_multi_mech_sites.png",
       dpi = 300, width = 10, height = 5.6)
