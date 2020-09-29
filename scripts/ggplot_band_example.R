#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(matlib)
library(plotly)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/Water/data" #change to your path

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Set up site and parameter info
# NOTE: This is set up for lower NETN only, and assumes there's not April data.
# For ACAD sites, need to update for April, including the x_axis_pad and scale_x_continuous
getSiteInfo(netnwd, park = "MIMA", info = "SiteCode")
site = "NETN_MIMA_SA00"
park = substr(site, 6, 9)
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

char_list<- getCharInfo(netnwd, park = park, sitecode = site, 
                        category = "physical", info = "CharName")
char <- char_list[1] # pick from 1 to 7

water_dat_hist <- getWData(netnwd, park = park, sitecode = site, 
                           charname = char, years = 2006:2018) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) 

water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                          charname = char, years = 2019) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
         mon_num = as.numeric(month))

ylabel = getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                     info = "DisplayName")

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
    # this filter line is still confusing to me
    # why filter all lower_50 records that aren't NA if NAs have been removed?
    # what is the purpose of unique()? 
  return(df_sum)
}

water_pct <- pct_fun(water_dat_hist) 
head(water_pct)

loess_bands <- function(df, column, band){
  df2 <- df[ , c(column, "mon_num")] # this is creating a new dataframe with all
  # rows and just two selected columns 
  colnames(df2) <- c("y", "mon_num")
  loess_mod <- loess(y ~ mon_num, span = 1, data = df2)
  # I'm not sure what the tilde is doing. defining a relationship?
  # span controls the amount of smoothing with 0.1 being not much 
  loess_pred <- data.frame(smooth = predict(loess_mod, newdata = water_pct))
  # why feed smooth to data.frame? what is that doing? 
  # newdata is an optional dataframe in which predict looks for variables to use
  colnames(loess_pred) <- c(paste0("smooth_", band))
  return(loess_pred)
} 

loess_bands(water_pct, .x, .y)
# what does the . in front of x and y signify?
# returns "Error in `[.tbl_df`(df, , c(column, "mon_num")) : object '.x' not found"

names(water_pct)
col_list <- names(water_pct[, 6:12])
band_list <- c("median", "l100", "u100", "l95", "u95", "l50", "u50") 

loess_map <- map2(col_list, band_list, ~loess_bands(water_pct, .x, .y)) %>% data.frame()
loess_res <- cbind(water_pct, loess_map)
loess_res$x_axis_pad <- c(4.9, 6, 7, 8, 9, 10.1) # update for ACAD

monthly_plot <- 
  ggplot(data = loess_res, aes(x = mon_num, y = median_val))+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u100, ymin = smooth_l100), 
              fill = "#89A7E7", alpha = 0.3)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u95, ymin = smooth_l95), 
              fill = "#89A7E7", alpha = 0.4)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u50, ymin = smooth_l50), 
              fill = "#89A7E7", alpha = 0.8)+
  #geom_line(aes(x = mon_num, y = median_val, group = Characteristic), color = "blue")+
  stat_smooth(method = "loess", aes(x = mon_num, y = median_val), color = "#1A52D0",
              position = "identity", se = F, formula = y ~ x, span = 0.8)+
  labs(y = ylabel, x = NULL, title = sitename)+  
  geom_point(data = water_dat_new, aes(x = mon_num, y = ValueCen))+
  forestMIDN::theme_FVM()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                     labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                "9" = "Sep", "10" = "Oct")) # remember to update for ACAD

monthly_plot

ggplotly(monthly_plot)
