#-------------------------------------
# Example code for NCRNWater package
#-------------------------------------

# I'm waiting on JP to merge the pull request to the master, so you need to install 
# the branch below to use the NCRNWater package version that I'm using.
devtools::install_github("NCRN/NCRNWater", ref = "NETN_Nov2019_Updates")

#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(openair)
library(NADA)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/NETN_Water/NETN_water_summaries/data" #change to your path

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Notes about the data structure -----
# There are 3 levels, called slots within the data: Park, Site, and Parameter
# Each parameter slot (called character in the functions) is nested within a site slot, which is nested within a park slot.
# The getter functions are used to query at each level, with getParks the highest level, then getSites, then getChars.
# The getters that end in "Info" are used to pull out info about about a given park, site or character, but don't
# actually pull out data. The getters that pull out data are getParks, getSites, and getChars. getWData is the
# most useful function, and can basically do anything getParks, getSites and getChars can do.


#----- Compile/view data -----
# The first getter functions compile information about parks, sites, and parameters, but don't pull in the data.
# These info getters are useful to see what sites are in a park, what parameters have been sampled for a site,
# and to get labels for plotting based on a code (eg change ACAD to Acadia National Park or TN to Total Nitrogen)

#----- Info getters
# Get info about parks in the network
getParkInfo(netnwd, info = "ParkLongName")
getParkInfo(netnwd, info = "ParkCode")
getParkInfo(netnwd, park = "MABI", info = "ParkShortName")

# Get a list of all the sites in a park. SiteName is the full name; SiteCode is the unique ID used by package
# Other info you can retrieve here: lat, long, type (lake or stream), ParkCode, ParkShortName, ParkLongName, and Network
getSiteInfo(netnwd, park = "ACAD", info = "SiteName") # List of all sites in ACAD with full name
getSiteInfo(netnwd, park = "ACAD", info = "SiteCode") # List of all sites in ACAD by site code
getSiteInfo(netnwd, info = "SiteName") # all the sites in NETN

# Get info about different parameters
getCharInfo(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", info = "CharName") 
  # lists all the parameters by code for Eagle Lake
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "DisplayName") 
  # lists full names of parameters for The Pogue
getCharInfo(netnwd, park = "MIMA", site = "NETN_MIMA_SB00", info = "DisplayName")
  # list full names of parameters for Elm Brook in MIMA
getCharInfo(netnwd, park = "MIMA", site = "NETN_MIMA_SB00", info = "Category")
  # lists the parameter category for each parameter in the same order as output in previous line

#----- Data getters
# Get all the data for a park. 
WEFA <- getParks(netnwd, park = "WEFA")
str(WEFA) #There's only 1 site in WEFA, so only 1 Site slot, and then a slot for each parameter 
# Each parameter has multiple slots, including a Data slot, which is what we're usually most interested in.

# Get all the parameters for a site
eagle <- getSites(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL")

# Get pH data from Eagle Lake
eagle_ph <- getChars(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", charname = "pH")
head(eagle_ph) ### the return of this is unclear to me
str(eagle_ph)

# Each of the above getters returns a list of info along with the Data slot, which can be tricky to work with. 
# Most of the time we just want a dataframe of the data, which the getWData function was designed for.
eagle_ph_df <- getWData(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", charname = "pH")
head(eagle_ph_df)

eagle_all_df <- getWData(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL")
head(eagle_all_df)
sort(unique(eagle_all_df$Characteristic)) # contains all data for Eagle Lake

getSiteInfo(netnwd, park = "SAGA", info = "SiteCode")
getSiteInfo(netnwd, park = "SAGA", info = "type")

saga_streams <- getWData(netnwd, park = "SAGA", sitecode = c("NETN_SAGA_SA00", "NETN_SAGA_SB00"))
unique(saga_streams$Site)

#----- Plotting functions -----
# function: waterseries

# Dot plot for uncensored data
waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", charname = "pH", 
            censored = FALSE, 
            deseason = FALSE,  # deseason splits data by month to see trends by month. 
            assessment = FALSE,
            yname = "pH",
            layers = "points") # don't add assessment line

# Dot plot for censored data. If a lab measurement is below detection limits, it's considered censored
waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", charname = "Cl_ueqL", 
            censored = TRUE, # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = FALSE,  
            assessment = FALSE, 
            yname = "Chlorophyll A") 

# Dot plot for censored data, split up by month
waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", charname = "Cl_ueqL", 
            censored = TRUE, 
            deseason = TRUE,  
            assessment = FALSE, 
            yname = "Chlorophyll A") 

# Using getCharInfo to label the y axis
waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", charname = "Cl_ueqL", 
            yname = paste0(getCharInfo(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", 
                                       charname = "Cl_ueqL", info = "DisplayName"), " (",
                           getCharInfo(netnwd, park = "MABI", sitecode = "NETN_MABI_PA00", 
                                       charname = "Cl_ueqL", info = "Units"), ")"),
            censored = TRUE, # If a measurement is below detection limits, it's considered censored
            deseason = TRUE,  # split data by month to see trends by month
            assessment = FALSE) # don't add assessment line

# Plotting multiple sites of the same parameter
waterseries(netnwd, park = 'ACAD', site = c("NETN_ACAD_EAGL", "NETN_ACAD_JORD"), charname = "pH",
            layers = "points") + facet_wrap(~Site)

# Plotting all physical parameters
waterseries(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", category = "physical", assessment = FALSE,
            layers = 'points') +
  facet_wrap(~Characteristic, scale = "free_y")

waterseries(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", category = "nutrients", assessment = FALSE,
            layers = 'points')+
  facet_wrap(~Characteristic, scale = "free_y")

# A more finished plot of multiple variables
# Setting global variables so less typing
park = "ACAD"
sitecode = "NETN_ACAD_JORD"
cat = "physical"

water_labeller <- function(Characteristic){
  paste0(getCharInfo(netnwd, park, sitecode, category = cat, info = "DisplayName"),
         " (",
         getCharInfo(netnwd, park, sitecode, category = cat, info = "Units"), ")")
}

waterseries(netnwd, park, sitecode, category = cat, layers = 'points', assessment = FALSE, censored = FALSE, yname = "") + 
  facet_wrap(~Characteristic, scales = 'free_y', labeller = as_labeller(water_labeller), strip.position = 'left')+
  theme(legend.position = "none", strip.placement = "outside", strip.background = element_blank())

# Code  to iterate through all plots 
char_list <- getCharInfo(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", category = "physical", info = "CharName") # create list to iterate on
char_list <- char_list[-7]

water_plots <- purrr::map(char_list, ~waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = ., deseason = TRUE))

water_plots[2] # to view the 2nd plot in the list. You can also use the arrows in the plot window to scroll through


# function: waterheat- shows above and below average measurements
waterheat(netnwd, park = "MABI", site = "NETN_MABI_PA00", charname = 'Cl_ueqL', by = "month") 
waterheat(netnwd, park = "ACAD", site = "NETN_ACAD_EAGL", charname = 'Temp_C', by = "month") 

#----- Trend functions -----
nonparTrends(netnwd, "MIMA", sitecode = "NETN_MIMA_SA00", charname = "pH", censored = FALSE)
nonparTrends(netnwd, "MABI", sitecode = "NETN_MABI_PA00", charname = "Cl_ueqL", censored = TRUE)

