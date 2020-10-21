#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)

#----- Import the data -----
#path = "C:/Users/Diana/Documents/NETN/Water/data" # change to your path
path = "C:/Users/DianaG/Documents/Work/NETN/Water/data"

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Set up site and parameter info -----
#site = "NETN_MABI_SA00"
park = "MABI"
site <- getSiteInfo(netnwd, park = park, info = "SiteCode") # get list of sites
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

char_list <- getCharInfo(netnwd, park = park, sitecode = site, 
                         category = "physical", info = "CharName")
char <- char_list[1]

unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% # fix this, doesn't work with two sites
  ifelse(. == "pct", paste("%"), .) %>% 
  ifelse(. == "pH units", paste(""), .)

water_dat <- getWData(netnwd, park = park, sitecode = site,
                      charname = char, years = 2006:2019) %>% 
             mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE))


# Compile historic water data
# water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
#                            charname = char, years = 2006:2018) %>% 
#                   mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 

# # Compile target year water data
# water_dat_new <- getWData(netnwd, park = park, sitecode = site,
#                           charname = char, years = 2019) %>% 
#                  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
#                         mon_num = as.numeric(month))
# water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")

#----- Boxplot -----
site_plot <- 
  ggplot(data = water_dat_hist, aes(x = Site, y = ValueCen, fill = Site)) +
  geom_boxplot() +
  geom_jitter() +
  forestMIDN::theme_FVM()
  
site_plot











