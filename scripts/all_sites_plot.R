all_sites_plot <- function(park, site_list, char){
  
  unit <- getCharInfo(netnwd, park = park, sitecode = site_list, charname = char, info = "Units") %>% 
    unique() %>% 
    ifelse(. == "pct", paste("%"), .) %>% 
    ifelse(. == "pH units", paste(""), .)
  
  param_name = getCharInfo(netnwd, charname = char, info = "DisplayName") %>% unique()
  
  # Create y axis label with units in parentheses, unless it's pH (no units)
  ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site_list, charname = char,
                        info = "DisplayName") %>% 
    ifelse(. != "pH", paste0(.," (", unit, ")"), .)
  
  # Create label for point data by removing units from char 
  ptlabel <- gsub("_.*","",char)
  
  # Compile historic water data
  water_dat_hist <- getWData(netnwd, park = park, sitecode = site_list,
                             charname = char, years = params$from:params$to) %>%
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE))
  
  # Find historic min and max for each month by site
  range_dat <- water_dat_hist %>% group_by(Site, month) %>% 
    summarize(max_val = max(ValueCen),
              min_val = min(ValueCen)) %>% 
    ungroup()
  
  # Compile target year water data
  water_dat_new <- getWData(netnwd, park = park, sitecode = site_list,
                            charname = char, years = params$current) %>%
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
           mon_num = as.numeric(month))
  water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")
  
  site_key <- data.frame(site_list, site_name)
  
  # Merge min/max df with target year df and add site names to df
  final_data <- merge(water_dat_new, range_dat, by = c("Site", "month"), 
                      all.x = T, all.y = F) %>% 
    merge(site_key, by.x = "Site", by.y = "site_list",
          all.x = T, all.y = F)
  
  lineplot <- 
    ggplot(data = final_data, aes(x = mon_num, y = ValueCen, shape = site_name)) +
    geom_line(aes(group = site_name, color = site_name)) +
    geom_point(aes(group = site_name, color = site_name, 
                   text = paste0(site_name, "<br>",
                                 month, " ", year, "<br>", 
                                 param_name, ": ", round(ValueCen,1), " ", unit)), 
               size = 2) +
    scale_color_manual(values = c("#3288bd", "#212121"), labels = site_name, name = NULL) +
    scale_shape_manual(values = c(16,17), labels = site_name, name = NULL)+
    forestMIDN::theme_FVM() +
    labs(y = ylabel, x = NULL, title = NULL)+
    theme(legend.position = "none") +
    theme(axis.title.y = element_text(size = 10))+
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                       labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct")) #update for ACAD
  
  all_sites_plotly <- ggplotly(lineplot, tooltip = c("text"))
  
  return(all_sites_plotly)
  
}