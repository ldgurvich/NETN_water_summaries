##------------------------------------------------------------------------------
## Load data and libraries
##------------------------------------------------------------------------------
library(NCRNWater)
library(tidyverse)
library(plotly)
library(purrr)
library(htmltools)
library(leaflet)
library(viridis)
library(kableExtra)

path = "C:/Users/Diana/Documents/NETN/Water/data"
#path = "D:/NETN/R_Dev/Water/NCRNWaterViz/Data/NETN"
netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

##------------------------------------------------------------------------------
## Global variables
##------------------------------------------------------------------------------
# Variables for cat asis code, which can't take params
parkcode = params$park
current = params$current
from = params$from
to = params$to
category = params$cat

# Long park name
long_name = getParkInfo(netnwd, park = params$park, info = "ParkLongName")

# List of sites
site_list_all <- getSiteInfo(netnwd, park = params$park, info = "SiteCode")
sites_curr <- getWData(netnwd, park = params$park, years = params$current) %>% 
  select(Site) %>% unique()

# Set up site_list so only includes sites sampled in current year
site_list <- site_list_all[site_list_all %in% sites_curr$Site] 

# Get list of site names
site_names <- getSiteInfo(netnwd, park = params$park, sitecode = site_list, info = "SiteName")

# Create site key for sitecode and type
site_key <- data.frame(Site = getSiteInfo(netnwd, parkcode = parkcode, info = "SiteCode"),
                       SiteType =  getSiteInfo(netnwd, parkcode = parkcode, info = "type"))

# Create dataframe of sites measured during target year and site type
site_df <- as.data.frame(site_list)
colnames(site_df) <- "sitecode"
site_df <- left_join(site_df, site_key, by = c("sitecode"="Site"))

# Filter dataframe to create lists of lake and stream sites 
# (There must be a less repetitive way to do this)
site_list_lake <- site_df %>% filter(SiteType == "Lake")
site_list_lake <- site_list_lake$sitecode

site_list_stream <- site_df %>% filter(SiteType == "Stream")
site_list_stream <- site_list_stream$sitecode

# Turn on comp tab if park has more than one site
show_comp <- ifelse(length(site_list) > 1, TRUE, FALSE)


## ---- test-a

##------------------------------------------------------------------------------
## STREAMS - Physical site comp setup
##------------------------------------------------------------------------------
# List of physical stream parameters (includes discharge)
char_list_p <- getCharInfo(netnwd, parkcode = params$park, category = params$cat, 
                           info = "CharName") %>%
               unique() #%>% #remove duplicates
               #.[!.=="Discharge_cfs"] #remove discharge

# Turn off all legends except last in list to remove legend duplicates from subplot
legend_list_p <- c(rep("none", length(char_list_p)-1), "bottom")

# Create list of plots comparing physical parameters across all lakes in park
comp_plots_p <- purrr::map2(char_list_p, legend_list_p,
                            ~watersite_comps(netnwd, year = params$current, 
                                             parkcode = params$park, 
                                             sitecode = site_list_stream,
                                             charname = .x,
                                             legend = .y)) %>%
  set_names(char_list_p) #name plots to refer to them by char name

heights_p <- c(0.3,0.35,0.3)

# Use plot list to create matrix of plots (plotly subplot)
comp_plots_p_final <- subplot(comp_plots_p[1:length(comp_plots_p)],
                              titleY = TRUE, titleX = FALSE, margin = 0.05,
                              #create 3 rows if more than 4 plots, otherwise 2 rows
                              nrows = ifelse(length(comp_plots_p)> 4, 3, 2),
                              #set height of each row (default makes middle row shorter)
                              heights = heights_p) %>% 
  # turn off subplot legend
  style(showlegend = FALSE) %>% 
  # manually set width and height of subplot
  layout(autosize = FALSE, width = 900, height = 650)

num_sites <- c(1:length(site_list_stream))

# Turn on legend elements 
for (i in 1:length(num_sites)){
  comp_plots_p_final$x$data[[i]]$showlegend <- TRUE #site (line/markers)
  #comp_plots_p_final$x$data[[length(comp_plots_p_final$x$data)]]$showlegend <- TRUE #threshold line
  #comp_plots_p_final$x$data[[length(comp_plots_p_final$x$data)]]$name <- "WQ Threshold" #set name
}

# Move legend down slightly and make it horizontal
comp_plots_p_final <- comp_plots_p_final %>% layout(legend = list(x = 0, y = -0.05, orientation = "h"))

##------------------------------------------------------------------------------
## LAKES - Nutrients site comp setup
##------------------------------------------------------------------------------
# List of nutrient parameters to include in site comparison
char_list_n <- c("TN_mgL", "TP_ugL")

# Turn off all legends except last in list to remove legend duplicates from subplot
legend_list_n <- c(rep("none", length(char_list_n)-1), "bottom")

# Create list of plots comparing physical parameters across all sites in park
comp_plots_n <- purrr::map2(char_list_n, legend_list_n,
                            ~watersite_comps(netnwd, 
                                             year = params$current, 
                                             parkcode = params$park, 
                                             sitecode = site_list_stream,
                                             charname = .x,
                                             legend = .y)) %>%
  set_names(char_list_n) #name plots to refer to them by char name

# Use plot list to create matrix of plots (plotly subplot)
comp_plots_n_final <- subplot(comp_plots_n[1:length(comp_plots_n)],
                              titleY = TRUE, titleX = FALSE, margin = 0.05, nrows = 1) %>% 
  # turn off subplot legend, manually set height and width of subplot
  style(showlegend = FALSE) %>% layout(autosize = FALSE, width = 900, height = 300)

num_sites <- c(1:length(site_list_stream))

# Turn on legend elements 
for (i in 1:length(num_sites)){
  comp_plots_n_final$x$data[[i]]$showlegend <- TRUE #site (line/markers)
  #comp_plots_n_final$x$data[[length(comp_plots_n_final$x$data)]]$showlegend <- TRUE #threshold line
  #comp_plots_n_final$x$data[[length(comp_plots_n_final$x$data)]]$name <- "WQ Threshold" #set name
}

# Move legend down slightly and make it horizontal
comp_plots_n_final <- comp_plots_n_final %>% layout(legend = list(x = 0, y = -0.12, orientation = "h"))

## ---- test-b
comp_plots_p_final
cat(params$park)