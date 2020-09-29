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
getSiteInfo(netnwd, park = "MABI", info = "SiteCode")
site = "NETN_MABI_PA00"
park = substr(site, 6, 9)
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

char_list <- getCharInfo(netnwd, park = park, sitecode = site, 
                        category = "physical", info = "CharName")
char <- char_list[7]

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
  return(df_sum)
}

water_pct <- pct_fun(water_dat_hist) 
head(water_pct)

loess_bands <- function(df, column, band){
  df2 <- df[ , c(column, "mon_num")]
  colnames(df2) <- c("y", "mon_num")
  loess_mod <- loess(y ~ mon_num, span = 1, data = df2)
  mid_pts <- data.frame(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))
  loess_pred <- data.frame(smooth = predict(loess_mod, newdata = mid_pts))
  colnames(loess_pred) <- paste0("smooth_", band)
  return(loess_pred)
}

col_list <- names(water_pct[, 6:12])
band_list <- c("median", "l100", "u100", "l95", "u95", "l50", "u50") 

loess_map <- map2(col_list, band_list, ~loess_bands(water_pct, .x, .y)) %>% data.frame() %>% 
  mutate(mon_num = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10))

#loess_res <- cbind(water_pct, loess_map)
loess_res <- merge(water_pct, loess_map, by = "mon_num", all.x = T, all.y = T)
view(loess_res)

loess_res$x_axis_pad <- c(4.9, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10.1) # remember to update for ACAD

monthly_plot <- 
  ggplot(data = loess_res, aes(x = mon_num, y = median_val))+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u100, ymin = smooth_l100), 
              fill = "#89A7E7", alpha = 0.3)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u95, ymin = smooth_l95), 
              fill = "#89A7E7", alpha = 0.4)+
  geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u50, ymin = smooth_l50), 
              fill = "#89A7E7", alpha = 0.8)+
  #geom_line(aes(x = mon_num, y = median_val, group = Characteristic), color = "blue")+
  stat_smooth(method = "loess", aes(text = paste("Median:", median_val)), color = "#1A52D0",
              position = "identity", se = F, formula = y ~ x, span = 0.8)+
  labs(y = ylabel, x = NULL, title = sitename)+  
  geom_point(data = water_dat_new, aes(x = mon_num, y = ValueCen, text = round(ValueCen, 2)))+ # text is for ggplotly
  forestMIDN::theme_FVM()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                     labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                "9" = "Sep", "10" = "Oct")) # remember to update for ACAD

monthly_plot

ggplotly(monthly_plot, tooltip = "text")
