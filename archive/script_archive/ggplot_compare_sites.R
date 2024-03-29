#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(plotly)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/Water/data" # change to your path
#path = "C:/Users/DianaG/Documents/Work/NETN/Water/data"

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#----- Set up site and parameter info -----
#site = "NETN_MABI_SA00"
park = "MIMA"
site <- getSiteInfo(netnwd, park = park, info = "SiteCode") # get list of sites
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site, info = "SiteName")

char_list <- getCharInfo(netnwd, park = park, sitecode = site, 
                         category = "physical", info = "CharName")
char <- char_list[1]

unit <- getCharInfo(netnwd, park = park, sitecode = site, charname = char, info = "Units") %>% # fix this, doesn't work with two sites
  ifelse(. == "pct", paste("%"), .) %>% 
  ifelse(. == "pH units", paste(""), .)

# water_dat <- getWData(netnwd, park = park, sitecode = site,
#                       charname = char, years = 2006:2019) %>% 
#              mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE))

# Create y axis label with units in parentheses, unless it's pH (no units)
ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                      info = "DisplayName") %>% 
  ifelse(. != "pH", paste0(.," (", unit, ")"), .)

# Create label for point data by removing units from char 
# (this still needs to be improved)
ptlabel <- gsub("_.*","",char)


# Compile historic water data
water_dat_hist <- getWData(netnwd, park = park, sitecode = site,
                            charname = char, years = 2006:2018) %>%
                   mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE))

# Find historic min and max for each month by site
range_dat <- water_dat_hist %>% group_by(Site, month) %>% 
  summarize(max_val = max(ValueCen),
            min_val = min(ValueCen)) %>% 
  ungroup()
  
# Compile target year water data
water_dat_new <- getWData(netnwd, park = park, sitecode = site,
                          charname = char, years = 2019) %>%
                 mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                        mon_num = as.numeric(month))
water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")

site_key <- data.frame(site, sitename)

# Merge min/max df with target year df and add site names to df
final_data <- merge(water_dat_new, range_dat, by = c("Site", "month"), 
                    all.x = T, all.y = F) %>% 
              merge(site_key, by.x = "Site", by.y = "site",
                    all.x = T, all.y = F)

# #----- Boxplot -----
# boxplot <- 
#   ggplot(data = water_dat_hist, aes(x = Site, y = ValueCen, fill = Site)) +
#   geom_boxplot() +
#   geom_jitter() +
#   forestMIDN::theme_FVM()
#   
# boxplot

#----- Line plot w/ error bars-----
lineplot <- 
  ggplot(data = final_data, aes(x = month, y = ValueCen, shape = sitename)) +
  geom_line(aes(group = sitename, color = sitename)) +
  geom_point(aes(group = sitename, color = sitename), size = 2)+
  scale_color_manual(values = c("#3288bd", "#d53e4f"), labels = sitename, name = NULL) +
  scale_shape_manual(values = c(16,17), labels = sitename, name = NULL)+
  # geom_errorbar(aes(ymin = min_val, ymax = max_val,
  #                   group = Site, color = Site),
  #               width = 0.2,
  #               alpha = 0.6) +
  forestMIDN::theme_FVM() +
  # theme(legend.position = "none") +
  labs(y = ylabel, x = NULL, 
       title = paste(getCharInfo(netnwd, parkcode = park, sitecode = site, charname = char,
                                 info = "DisplayName")))

lineplot

ggplotly(lineplot)

#####################################
#----- Site comparison function -----
#####################################
park = "MABI" #set as params$park
site_list <- getSiteInfo(netnwd, park = park, info = "SiteCode")
sitename = getSiteInfo(netnwd, parkcode = park, sitecode = site_list, info = "SiteName")
char_list <- getCharInfo(netnwd, park = park, sitecode = site_list, category = "nutrients", info = "CharName") %>% 
  unique()
# char_list <- getCharInfo(netnwd, park = park, sitecode = site_list, category = "physical", info = "CharName") %>% 
#              unique() # only use duplicate chars
             #.[duplicated(.)] #%>% 
             #.[duplicated(.)]

char <- char_list[1]

for (i in seq(char_list)) {
  char <- char_list[i]
  watersite_comps(netnwd, parkcode=park, charname=char, year=2019)
}

comp_plots <- purrr::map(char_list, 
                          ~watersite_comps(netnwd, 
                                           year = 2019, 
                                           parkcode = park,
                                           charname = .
                                           )) %>%
  set_names(char_list)

all_sites_plot <- function(park, site_list, char){
  
  unit <- getCharInfo(netnwd, park = park, sitecode = site_list, charname = char, info = "Units") %>% 
    unique() %>% 
    ifelse(. == "pct", paste("%"), .) %>% 
    ifelse(. == "pH units", paste(""), .)
  
  param_name = getCharInfo(netnwd, charname = char, info = "DisplayName") %>% unique()
  
  # Create y axis label with units in parentheses, unless it's pH (no units)
  ylabel <- getCharInfo(netnwd, parkcode = park, sitecode = site_list, charname = char,
                        info = "DisplayName") %>% 
    ifelse(. != "pH", paste0(.," (", unit, ")"), .) %>% unique()
  
  # Create label for point data by removing units from char 
  ptlabel <- gsub("_.*","",char)
  
  # Compile historic water data
  water_dat_hist <- getWData(netnwd, park = park, sitecode = site_list,
                             charname = char, years = 2006:2018) %>% #change to params
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE))
  
  # Find historic min and max for each month by site
  range_dat <- water_dat_hist %>% group_by(Site, month) %>% 
    summarize(max_val = max(ValueCen),
              min_val = min(ValueCen)) %>% 
    ungroup()
  
  # Compile target year water data
  water_dat_new <- getWData(netnwd, park = park, sitecode = site_list,
                            charname = char, years = 2019) %>% #change to params
    mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE),
           mon_num = as.numeric(month))
  water_dat_new$year <- format(as.Date(water_dat_new$Date), "%Y")
  
  site_key <- data.frame(site_list, sitename)
  
  # Merge min/max df with target year df and add site names to df
  final_data <- merge(water_dat_new, range_dat, by = c("Site", "month"), 
                      all.x = T, all.y = F) %>% 
    merge(site_key, by.x = "Site", by.y = "site_list",
          all.x = T, all.y = F)
  
  str(final_data)
  
  # Trim color and shape options to number of sites
  colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33") %>% 
    .[1:(length(site_list))]
  shapes <- c(16, 17, 18, 15, 1, 2) %>% 
    .[1:(length(site_list))]

  lineplot <- 
    ggplot(data = final_data, aes(x = mon_num, y = ValueCen, shape = sitename)) +
    geom_line(aes(group = sitename, color = sitename)) +
    geom_point(aes(group = sitename, color = sitename, 
                   text = paste0(sitename, "<br>",
                                 month, " ", year, "<br>", 
                                 param_name, ": ", round(ValueCen,1), " ", unit)), 
               size = 2) +
    scale_color_manual(values = colors, labels = sitename, name = NULL) +
    scale_shape_manual(values = shapes, labels = sitename, name = NULL)+
    forestMIDN::theme_FVM() +
    labs(y = ylabel, x = NULL, title = NULL)+
    theme(legend.position = "none") +
    theme(axis.title.y = element_text(size = 10))+
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10), 
                       labels = c("5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", 
                                  "9" = "Sep", "10" = "Oct")) #update for ACAD
  
  all_sites_plotly <- ggplotly(lineplot, tooltip = c("text"))
    
  return(lineplot)
  
}

char <- char_list[5]

plot <- all_sites_plot(park, site_list, char)

ggplotly(plot)



