#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)
library(gridExtra)
library(cowplot)

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


#----- Plot function testing -----
water_plot <- function(site, char){
  
  park = substr(site, 6, 9)
  sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")
  
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
    geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u100, ymin = smooth_l100, text = "Historic range"), 
                fill = "#89A7E7", alpha = 0.3)+
    geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u95, ymin = smooth_l95, text = "Historic 95% range"), 
                fill = "#89A7E7", alpha = 0.4)+
    geom_ribbon(aes(x = x_axis_pad, ymax = smooth_u50, ymin = smooth_l50, text = "Historic 50% range"), 
                fill = "#89A7E7", alpha = 0.8)+
    stat_smooth(method = "loess", aes(x = mon_num, y = median_val, text = "Historic median"), 
                color = "#1A52D0", position = "identity", se = F, formula = y ~ x, span = 0.6)+
    labs(y = ylabel, x = NULL, title = sitename) +  
    geom_point(aes(x = mon_num, y = ValueCen, text = paste0(month, " ", year, "<br>", 
                                                            ptlabel, ": ", round(ValueCen,1), " ", unit))) +
    forestMIDN::theme_FVM()+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                       labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct")) #update for ACAD
  
  #plot <- ggplotly(monthly_plot, tooltip = c("text"))
  
  return(monthly_plot)
}

site <- "NETN_MABI_SA00"
park <- substr(site, 6, 9)
remove <- c("DOsat_pct", "Turbidity_NTU", "SDepth1_m", "PenetrationRatio") #chars to remove from list

char_list <- getCharInfo(netnwd, 
                         park = park, 
                         sitecode = site, 
                         category = "physical", 
                         info = "CharName") %>% 
                         .[! . %in% remove] #remove chars above from final list

#water_plot("NETN_MABI_SA00", char_list[1])

# Create list of ggplots and rename them
plot <- purrr::map(char_list, ~water_plot(site = "NETN_MABI_SA00", char = .)) %>%
        set_names(c(char_list)) #name plots to refer to them by char
        #purrr::map(., ~ggplotly(., tooltip = c("text"))) #plotly iteration
names(plot)

plot["DO_mgL"]  
plot[2]
plot

p1 <- plot["DO_mgL"]
p2 <- plot["pH"]

grid.arrange(grobs = c(plot["DO_mgL"], plot["Temp_C"],
                       plot["SpCond_uScm"], plot["Discharge_cfs"],
                       plot["pH"]), 
             ncol = 2)

# plot_grid(plotlist = c(plot["DO_mgL"], plot["Temp_C"],
#                        plot["SpCond_uScm"], plot["Discharge_cfs"],
#                        plot["pH"]),
#           ncol = 2, rel_heights = c(2, 2, 2, 2, 2)
# )
