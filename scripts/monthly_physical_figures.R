#-------------------------------------
# Percentile range figures for physical 
# parameters on a monthly basis
#-------------------------------------

#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(lubridate)
library(plotly)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/NETN_Water/NETN_water_summaries/data"

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#---- Building a 95 percentile range function from historic data ----

# Set global variables
yr = 2019 # this is the target year for the analysis
park = "MIMA"
sitecode = "NETN_MIMA_SA00"
cat = "physical"
char_list = getCharInfo(netnwd, park = park, sitecode = sitecode, category = cat, info = "CharName")
char = "DO_mgL"
# everything except turbidity works for MABI

# Create dataset
water_dat <- getWData(netnwd, park = park, sitecode = sitecode, category = cat)

# Function that returns percentile range 
pct_fun <- function(df){
  # Check if month is a column, and add if it's not as a character string (label=TRUE)
  df <- if(!"month" %in% names(df)){
    df %>% mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>% 
      droplevels()}
  else {df}
  
  df_sum <- df %>% group_by(Park, Site, Characteristic, month) %>%
    filter(year(Date) < yr) %>% # I added this to make sure it doesn't take the 
    # target year into account
    summarize(mean = mean(ValueCen),
              num_samps = n(),
              lower_pct = ifelse(num_samps >= 4, quantile(ValueCen, 0.025, na.rm = TRUE), NA),
              upper_pct = ifelse(num_samps >= 4, quantile(ValueCen, 0.975, na.rm = TRUE), NA),
              .groups = "drop") %>% 
    filter(!is.na(lower_pct))
  return(df_sum)
}

water_pct <- pct_fun(water_dat)

# Subset to target characteristic (defined in global variables)
char_pct <- water_pct %>% filter(Characteristic == char) %>% 
  mutate(month_ch = as.character(month))

# Subset raw water data to only target year char data
char_dat_2019 <- water_dat %>% filter(year(Date) == yr) %>% 
  filter(Characteristic == char) %>% # is there a way to combine these filters? 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
  droplevels() %>%
  mutate(month_ch = as.character(month))
  # months as characters to be able to join the tables on month

# Join tables to add percentile value columns to raw data
char_pct_2019 <- left_join(x = char_dat_2019, 
                           y = char_pct[, c("mean", "num_samps", "lower_pct", "upper_pct", "month_ch")], 
                           by = "month_ch")

# Plot monthly characteristic measurements with 95 percentile ribbon
plot <- char_pct_2019 %>% 
  ggplot(aes(x = month, y = ValueCen)) +
  geom_ribbon(aes(x = month, ymin = lower_pct, ymax = upper_pct, fill = "band"), 
              group = 1, alpha = 0.7, fill = "steelblue2") + 
  # for some reason specifying group = 1 is what got the ribbon to appear
  geom_point(size = 3) +
  # geom_line(group = 1) +
  geom_line(aes(x = month, y = mean), group = 1, color = "blue") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = paste0(getCharInfo(netnwd, park = park, sitecode = sitecode, charname = char, info = "DisplayName"),
                  " (",
                  getCharInfo(netnwd, park = park, sitecode = sitecode, charname = char, info = "Units"), ")"),
       x = "Month") +
  scale_x_discrete(expand = expansion(0.03))
plot

ggplotly(plot)
