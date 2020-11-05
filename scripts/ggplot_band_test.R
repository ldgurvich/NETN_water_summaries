
devtools::install_github("NCRN/NCRNWater", ref = "NETNsummaries")

#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/Water/data" #change to your path

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Set up site and parameter info
# NOTE: This is set up for lower NETN only, and assumes there's not April data.
# For ACAD sites, need to update for April, including the x_axis_pad and scale_x_continuous
getSiteInfo(netnwd, park = "MABI", info = "SiteCode")
site = "NETN_MABI_SA00"
park = substr(site, 6, 9)
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

char_list <- getCharInfo(netnwd, park = park, sitecode = site, 
                        category = "physical", info = "CharName")
char <- char_list[1]

unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% 
  ifelse(. == "pct", paste("%"), .) %>% 
  ifelse(. == "pH units", paste(""), .)

# Find year range of data
#water_dat_hist <- getWData(netnwd, park = park, sitecode = site, charname = char)

#min <- min(lubridate::year(water_dat_hist$Date), na.rm = T)
#max <- max(lubridate::year(water_dat_hist$Date), na.rm = T)

# Compile historic water data
water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
                           charname = char, years = 2006:2018) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 

# Compile target year water data
water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                          charname = char, years = 2019) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         mon_num = as.numeric(month))
water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")

# Create percentile bands for input df
pct_fun <- function(df){
  # Check if month is a column, and add if it's not.
  df <- if(!"month" %in% names(df)){
    df %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
      droplevels()} else {df}
  
  df_sum <- df %>% group_by(Park, Site, Characteristic, month) %>% 
    summarize(num_samps = n(),
              median_val = median(ValueCen, na.rm = TRUE),
              lower_100 = ifelse(num_samps >= 4, min(ValueCen, na.rm = T), NA),
              upper_100 = ifelse(num_samps >= 4, max(ValueCen, na.rm = T), NA),
              lower_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.75, na.rm = T), NA),
              mon_num = as.numeric(month),
              .groups = "drop") %>% 
    filter(!is.na(lower_50)) %>% droplevels() %>% unique()
  return(df_sum)
}

# Historic percentile band values
water_pct <- pct_fun(water_dat_hist) 
head(water_pct)

# Smooth bands with monthly midpoints
loess_bands <- function(df, column, band){
  df2 <- df[ , c(column, "mon_num")]
  colnames(df2) <- c("y", "mon_num")
  loess_mod <- loess(y ~ mon_num, span = 1, data = df2)
  mid_pts <- data.frame(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))
  loess_pred <- data.frame(smooth = predict(loess_mod, newdata = mid_pts))
  colnames(loess_pred) <- paste0("smooth_", band)
  return(loess_pred)
}

# Columns with band values + median ([6])
col_list <- names(water_pct[, 6:12])

# Create list of shortened column names coinciding with col_list
band_list <- c("median", "l100", "u100", "l95", "u95", "l50", "u50") 


loess_map <- map2(col_list, band_list, ~loess_bands(water_pct, .x, .y)) %>% data.frame() %>% 
  mutate(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))

loess_res <- merge(water_pct, loess_map, by = "mon_num", all.x = T, all.y = T)
loess_res$x_axis_pad <- c(4.9, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10.1) # update for ACAD
final_data <- merge(water_dat_new, loess_res, by = c("mon_num", "Park", "month", "Site", "Characteristic"), 
                    all.x = T, all.y = T)

str(final_data)

# Create y axis label with units in parentheses, unless it's pH (no units)
ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                      info = "DisplayName") %>% 
          ifelse(. != "pH", paste0(.," (", unit, ")"), .)

# Create label for point data by removing units from char 
# (this still needs to be improved)
ptlabel <- gsub("_.*","",char)

monthly_plot <- 
  ggplot(data = final_data, aes(x = mon_num, y = median_val))+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u100, ymin = smooth_l100, text = "Historic range"), 
              fill = "#89A7E7", alpha = 0.3)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u95, ymin = smooth_l95, text = "Historic 95% range"), 
              fill = "#89A7E7", alpha = 0.4)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u50, ymin = smooth_l50, text = "Historic 50% range"), 
              fill = "#89A7E7", alpha = 0.8)+
  stat_smooth(method = "loess", aes(x = mon_num, y = median_val, text = "Historic median"), 
              color = "#1A52D0",
              position = "identity", se = F, formula = y ~ x, span = 0.8)+
  labs(y = ylabel, x = NULL, title = paste(getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                                                       info = "DisplayName"),"at", sitename)) +  
  geom_point(aes(x = mon_num, y = ValueCen, text = paste0(month, " ", year, "<br>", 
                                                          ptlabel, ": ", round(ValueCen,1), " ", unit))) +
  forestMIDN::theme_FVM()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                     labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                "9" = "Sep", "10" = "Oct")) #update for ACAD
monthly_plot

ggplotly(monthly_plot, tooltip = c("text"))

# View plotly JSON object
#plotly_json(monthly_plot)

#-------------------------------------------------------------------------------
# Draft 2 / Testing
#-------------------------------------------------------------------------------

#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/Water/data" #change to your path

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Other input functions -----
# Create percentile bands for input df
pct_fun <- function(df){
  # Check if month is a column, and add if it's not.
  df <- if(!"month" %in% names(df)){
    df %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
      droplevels()} else {df}
  
  df_sum <- df %>% group_by(Park, Site, Characteristic, month) %>% 
    summarize(num_samps = n(),
              median_val = median(ValueCen, na.rm = TRUE),
              lower_100 = ifelse(num_samps >= 4, min(ValueCen, na.rm = T), NA),
              upper_100 = ifelse(num_samps >= 4, max(ValueCen, na.rm = T), NA),
              lower_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.75, na.rm = T), NA),
              mon_num = as.numeric(month),
              .groups = "drop") %>% 
    filter(!is.na(lower_50)) %>% droplevels() %>% unique()
  return(df_sum)
}

# Smooth bands with monthly midpoints
loess_bands <- function(df, column, band){
  df2 <- df[ , c(column, "mon_num")]
  colnames(df2) <- c("y", "mon_num")
  loess_mod <- loess(y ~ mon_num, span = 0.6, data = df2)
  mid_pts <- data.frame(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))
  loess_pred <- data.frame(smooth = predict(loess_mod, newdata = mid_pts))
  colnames(loess_pred) <- paste0("smooth_", band)
  return(loess_pred)
}

#----- Deconstructed water_plot function -----

site <- "NETN_MABI_SA00"
park = substr(site, 6, 9)
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")
char_list <- getCharInfo(netnwd, park = park, sitecode = site, 
                         category = "physical", info = "CharName")
char <- char_list[1]

# Set unit for plot display
unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% 
  ifelse(. == "pct", paste("%"), .) %>% 
  ifelse(. == "pH units", paste(""), .)

# Compile historic water data
water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
                           charname = char, years = 2006:2018) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 

# Compile target year water data
water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                          charname = char, years = 2019) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         mon_num = as.numeric(month))
water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")

# Historic percentile band values
water_pct <- pct_fun(water_dat_hist) 

# Columns with band values + median ([6])
col_list <- names(water_pct[, 6:12])

# Create list of shortened column names coinciding with col_list
band_list <- c("median", "l100", "u100", "l95", "u95", "l50", "u50") 


loess_map <- map2(col_list, band_list, ~loess_bands(water_pct, .x, .y)) %>% data.frame() %>% 
             mutate(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))

loess_res <- merge(water_pct, loess_map, by = "mon_num", all.x = T, all.y = T)
loess_res$x_axis_pad <- c(4.9, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10.1) # update for ACAD
final_data <- merge(water_dat_new, loess_res, by = c("mon_num", "Park", "month", "Site", "Characteristic"), 
                    all.x = T, all.y = T)

# Create y axis label with units in parentheses, unless it's pH (no units)
ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                      info = "DisplayName") %>% 
  ifelse(. != "pH", paste0(.," (", unit, ")"), .)

# Create label for point data by removing units from char 
# (this still needs to be improved)
ptlabel <- gsub("_.*","",char)

monthly_plot <- 
  ggplot(data = final_data, aes(x = mon_num, y = median_val))+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u100, ymin = smooth_l100), 
              fill = "#89A7E7", alpha = 0.3)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u95, ymin = smooth_l95), 
              fill = "#89A7E7", alpha = 0.4)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u50, ymin = smooth_l50), 
              fill = "#89A7E7", alpha = 0.8)+
  stat_smooth(method = "loess", aes(x = mon_num, y = median_val), 
              color = "#1A52D0", position = "identity", se = F, formula = y ~ x, span = 0.6)+
#  geom_line(aes(x = mon_num, y = median_val, group = 1), color = "blue")+ 
#  I can't get geom_line to show up for some reason  
  labs(y = ylabel, x = NULL, title = sitename) +  
  geom_point(aes(x = mon_num, y = ValueCen)) +
  forestMIDN::theme_FVM()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                     labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                "9" = "Sep", "10" = "Oct")) #update for ACAD
monthly_plot

#-------------------------------------------------------------------------------
#----- Draft 3 / 20201105 ----- 
#-------------------------------------------------------------------------------
# Added lower/upper thresholds

# Load libraries
library(NCRNWater)
library(tidyverse)
library(plotly)
#library(gridExtra)
#library(cowplot)
#library(magick)

# Import the data
#path = "C:/Users/DianaG/Documents/Work/NETN/Water/data" #change to your path
path = "C:/Users/Diana/Documents/NETN/Water/data"
netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#leg_fig <- image_read("./images/legend_temp.png")

# Get park name
parkname = getParkInfo(netnwd, park = "MABI", info = "ParkLongName")

# Functions
# Create percentile bands for input df
pct_fun <- function(df){
  # Check if month is a column, and add if it's not.
  df <- if(!"month" %in% names(df)){
    df %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
      droplevels()} else {df}
  
  df_sum <- df %>% group_by(Park, Site, Characteristic, month) %>% 
    summarize(num_samps = n(),
              median_val = median(ValueCen, na.rm = TRUE),
              lower_100 = ifelse(num_samps >= 4, min(ValueCen, na.rm = T), NA),
              upper_100 = ifelse(num_samps >= 4, max(ValueCen, na.rm = T), NA),
              lower_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = T), NA),
              upper_95 = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = T), NA),
              lower_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.25, na.rm = T), NA),
              upper_50 = ifelse(num_samps >= 4, quantile(ValueCen, 0.75, na.rm = T), NA),
              mon_num = as.numeric(month),
              .groups = "drop") %>% 
    filter(!is.na(lower_50)) %>% droplevels() %>% unique()
  return(df_sum)
}

water_plot <- function(site, char){
  #####
  # Set up plotting parameters
  park = substr(site, 6, 9)
  sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")
  param_name = getCharInfo(netnwd, charname = char, info = "DisplayName") %>% unique()
  
  # Set unit for plot display
  unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% 
    unique() %>% 
    ifelse(. == "pct", paste("%"), .) %>% 
    ifelse(. == "pH units", paste(""), .)
  
  # Create y axis label with units in parentheses, unless it's pH (no units)
  ylabel <- ifelse(param_name != "pH", paste0(param_name, " (", unit, ")"),
                   paste0(param_name))
  
  # Compile historic water data
  water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
                             charname = char, years = 2006:2018) %>% #params$from:params$to
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 
  
  # Compile target year water data
  water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                            charname = char, years = 2019) %>% #params&current
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
           mon_num = as.numeric(month))
  
  water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")
  
  # Historic percentile band values
  water_pct <- pct_fun(water_dat_hist) 
  
  final_data <- merge(water_dat_new, water_pct, by = c("mon_num", "Park", "month", "Site", "Characteristic"), all.x = T, all.y = T)
  
  # str(final_data)
  
  # Add upper and lower assessment points
  final_data <- final_data %>% mutate(
    LowerPoint = case_when(Park == "MABI" & SampleDepth == "stream" & Characteristic == "DO_mgL" ~ 7,
                           Park == "MABI" & SampleDepth == "stream" & Characteristic == "pH" ~ 6.5,
                           Park == "MABI" & SampleDepth == "epilimnion" & Characteristic == "pH" ~ 6.5,
                           Park == "SAGA" & SampleDepth == "stream" & Characteristic == "DO_mgL" ~ 6,
                           Park == "SAGA" & SampleDepth == "epilimnion" & Characteristic == "DO_mgL" ~ 5,
                           Park == "SAGA" & SampleDepth == "stream" & Characteristic == "pH" ~ 6.5,
                           Park == "SAGA" & SampleDepth == "epilimnion" & Characteristic == "pH" ~ 6.5,
                           #Park == "MIMA" & SiteName == "Concord River" & Characteristic == "DO_mgL" ~ 6.0,
                           #Park == "MIMA" & SiteName != "Concord River" & Characteristic == "DO_mgL" ~ 5.0,
                           Park == "MIMA" & Characteristic == "pH" ~ 6.5,
                           Park == "SAIR" & Characteristic == "pH" ~ 6.5,
                           Park == "SARA" & Characteristic == "DO_mgL" ~ 4.0,
                           Park == "SARA" & Characteristic == "pH" ~ 6.5,
                           Park == "ROVA" & Characteristic == "DO_mgL" ~ 4.0,
                           Park == "ROVA" & Characteristic == "pH" ~ 6.5,
                           Park == "WEFA" & Characteristic == "DO_mgL" ~ 5.0,
                           Park == "WEFA" & Characteristic == "pH" ~ 6.5,
                           Park == "WEFA" & Characteristic == "SDepth1_m" ~ 4.5,
                           Park == "MORR" & Characteristic == "DO_mgL" ~ 7.0,
                           Park == "MORR" & Characteristic == "pH" ~ 6.5),
    
    UpperPoint = case_when(Park == "MABI" & SampleDepth == "stream" & Characteristic == "pH" ~ 8.5,
                           Park == "MABI" & SampleDepth == "epilimnion" & Characteristic == "pH" ~ 8.5,
                           Park == "SAGA" & SampleDepth == "stream" & Characteristic == "pH" ~ 8.5,
                           Park == "SAGA" & SampleDepth == "epilimnion" & Characteristic == "pH" ~ 8.5,
                           #Park == "MIMA" & SiteName == "Concord River" & Characteristic == "Temp_C" ~ 28.3,
                           #Park == "MIMA" & SiteName != "Concord River" & Characteristic == "Temp_C" ~ 20.3,
                           Park == "MIMA" & Characteristic == "pH" ~ 8.3,
                           Park == "MIMA" & Characteristic == "TP_ugL" ~ 31.25,
                           Park == "MIMA" & Characteristic == "TN_mgL" ~ 0.71,
                           Park == "SAIR" & Characteristic == "pH" ~ 8.3,
                           Park == "SAIR" & Characteristic == "TP_ugL" ~ 31.25,
                           Park == "SAIR" & Characteristic == "TN_mgL" ~ 0.71,
                           Park == "SAIR" & Characteristic == "Temp_C" ~ 28.3,
                           Park == "SARA" & Characteristic == "pH" ~ 8.5,
                           Park == "SARA" & Characteristic == "TN_mgL" ~ 0.54,
                           Park == "ROVA" & Characteristic == "pH" ~ 8.5,
                           Park == "ROVA" & Characteristic == "TP_ugL" ~ 33.0,
                           Park == "ROVA" & Characteristic == "TN_mgL" ~ 0.54,
                           Park == "WEFA" & Characteristic == "Temp_C" ~ 29.4,
                           Park == "WEFA" & Characteristic == "pH" ~ 8.0,
                           Park == "WEFA" & Characteristic == "TP_ugL" ~ 8.0,
                           Park == "WEFA" & Characteristic == "TN_mgL" ~ 0.32,
                           Park == "WEFA" & Characteristic == "ChlA_ugL" ~ 2.9,
                           Park == "MORR" & Characteristic == "Temp_C" ~ 22.0,
                           Park == "MORR" & Characteristic == "pH" ~ 8.5,
                           Park == "MORR" & Characteristic == "TP_ugL" ~ 36.56,
                           Park == "MORR" & Characteristic == "TN_mgL" ~ 0.69,
                           Park == "MORR" & Characteristic == "SO4_ueqL" ~ 5200,
                           Park == "MORR" & Characteristic == "Turbidity_NTU" ~ 50)
    )
  
  # str(final_data)
  
  # Remember to add back in for nutrients
  # Reg8 <- c("ACAD", "MABI", "SAGA")
  # 
  # MD <- MD %>% mutate(
  #   LowerPoint = case_when(ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "SDepth1_m" ~ 0.93,
  # UpperPoint = case_when(ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "TP_ugL" ~ 10.0,
  #                        ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "TP_ugL" ~ 8.0,
  #                        ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "TN_mgL" ~ 0.38,
  #                        ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "TN_mgL" ~ 0.24,
  #                        ParkCode %in% Reg8 & Type == "Stream" & CharacteristicName == "ChlA_ugL" ~ 0.63,
  #                        ParkCode %in% Reg8 & Type == "Lake" & CharacteristicName == "ChlA_ugL" ~ 2.43,

  monthly_plot <- ggplot(data = final_data, aes(x = mon_num, y = median_val))+
    
    # Min/max band
    geom_ribbon(aes(x = mon_num, ymax = upper_100, ymin = lower_100), fill = "#89A7E7", alpha = 0.3)+
    geom_line(aes(x = mon_num, y = upper_100, 
                  text = paste0("Historic ", month, " Maximum: ", "<br>", param_name, ": ", round(upper_100, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.3, lwd=3)+
    geom_line(aes(x = mon_num, y = lower_100, 
                  text = paste0("Historic ", month, " Minimum: ", "<br>", param_name, ": ", round(lower_100, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.3, lwd=3)+
    
    # 95% band
    geom_ribbon(aes(x = mon_num, ymax = upper_95, ymin = lower_95), 
                fill = "#89A7E7", alpha = 0.4)+
    geom_line(aes(x = mon_num, y = upper_95, 
                  text = paste0("Historic ", month, " Upper 95%: ", "<br>", param_name, ": ", round(upper_95, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.4, lwd=3)+
    geom_line(aes(x = mon_num, y = lower_95, 
                  text = paste0("Historic ", month, " Lower 95%: ", "<br>", param_name, ": ", round(lower_95, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.4, lwd=3)+
    
    # 50% band
    geom_ribbon(aes(x = mon_num, ymax = upper_50, ymin = lower_50), 
                fill = "#89A7E7", alpha = 0.8)+
    geom_line(aes(x = mon_num, y = upper_50, 
                  text = paste0("Historic ", month, " Upper 50%: ", "<br>", param_name, ": ", round(upper_50, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.8, lwd=3)+
    geom_line(aes(x = mon_num, y = lower_50, 
                  text = paste0("Historic ", month, " Lower 50%: ", "<br>", param_name, ": ", round(lower_50, 1), " ", unit)),
              color = "#89A7E7", alpha = 0.8, lwd=3)+
    
    # Median
    geom_line(aes(x = mon_num, y = median_val), color = "#1A52D0")+
    geom_line(aes(x = mon_num, y = median_val, 
                  text = paste0("Historic ", month, " Median ", "<br>", param_name, ": ", round(median_val, 1), " ", unit)), 
              color = "#1A52D0")+
    
    # Current measurement
    geom_point(aes(x = mon_num, y = ValueCen,  text = paste0(month, " ", year, "<br>", param_name, ": ", round(ValueCen,1), " ", unit)))+
    #geom_line(aes(x = mon_num, y = ValueCen), color = "black")+
  
    # Upper and lower points
    # Changed to geom_line to get labels to show throughout line
    geom_line(aes(y = LowerPoint, text = paste("Lower", param_name, "threshold:", LowerPoint, unit)),
               linetype = "dashed", color = "black")+
    geom_line(aes(y = UpperPoint, text = paste("Upper", param_name, "threshold:", UpperPoint, unit)),
               linetype = "dashed", color = "black")+
    
    # Labels
    labs(y = ylabel, x = NULL, title = NULL) + 
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                       labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct"))+ #update for ACAD
  
    # Themes
    forestMIDN::theme_FVM()+
    theme(axis.title.y = element_text(size = 10))
    
  monthly_plotly <- ggplotly(monthly_plot, tooltip = c("text"))
  
  return(monthly_plotly)
}

park <- "MABI" #params$park
site <- "NETN_MABI_PA00"
sitename <- getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")
remove <- c("DOsat_pct", "Turbidity_NTU", "SDepth1_m", "PenetrationRatio") #chars to remove from list
char_list <- getCharInfo(netnwd, 
                         parkcode = park, 
                         sitecode = site, 
                         category = "physical", 
                         info = "CharName") %>% 
  .[! . %in% remove] #remove chars above from final list

plots <- purrr::map(char_list, ~water_plot(site = site, char = .)) %>%
         set_names(c(char_list)) #name plots to refer to them by char name

plots_final <- subplot(plots$DO_mgL, plots$Temp_C, plots$SpCond_uScm, plots$Discharge_cfs, plots$pH,
                       titleY = TRUE, titleX = FALSE, nrows=3, margin = 0.05, 
                       heights = c(0.3, 0.35, 0.3))

plots_final

