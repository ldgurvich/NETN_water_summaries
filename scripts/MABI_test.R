# MABI
devtools::install_github("NCRN/NCRNWater")

#----- Load libraries -----
library(NCRNWater)
library(tidyverse)
library(lubridate)
library(plotly)

# Set strings as factors to false
options(stringsAsFactors = FALSE)

#----- Import the data -----
path = "C:/Users/Diana/Documents/NETN/NETN_Water/NETN_water_summaries/data"

netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

#---- Testing parameter info ----
getSiteInfo(netnwd, park = "MABI", info = "SiteName") # The Pogue, Pogue Brook
getSiteInfo(netnwd, park = "MABI", info = "SiteCode") # NETN_MABI_PA00, NETN_MABI_SA00

# All parameter info for The Pogue sites
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "CharName")
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "DisplayName")
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "Category")

# Physical parameter info for The Pogue sites
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "CharName", category = "physical")
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_PA00", info = "DisplayName", category = "physical")

# Physical parameter info for Pogue Brook sites
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_SA00", info = "CharName", category = "physical")
getCharInfo(netnwd, park = "MABI", site = "NETN_MABI_SA00", info = "DisplayName", category = "physical")

pogue_br_df <- getWData(netnwd, park = "MABI", site = "NETN_MABI_SA00")

# Plot physical parameters of Pogue Brook
waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "DO_mgL", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,  
            assessment = FALSE, # assessment line
            yname = "DO (mg/L)")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "DOsat_pct", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,  
            assessment = TRUE, # assessment line
            yname = "DO saturation (%)")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "pH", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,  
            assessment = TRUE, # assessment line
            yname = "pH")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "SpCond_uScm", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,  
            assessment = TRUE, # assessment line
            yname = "Specific conductance")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "Temp_C", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,  
            assessment = TRUE, # assessment line
            yname = "Temperature (C)")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "Discharge_cfs", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = TRUE,   
            assessment = TRUE, # assessment line
            yname = "Discharge")

waterseries(netnwd, park = "MABI", sitecode = "NETN_MABI_SA00", charname = "Turbidity_NTU", 
            censored = TRUE,  # If there was a censored data point, and censored = FALSE, it gets dropped from the plot.
            # with censored = TRUE, the censored data is plotted as a different color
            deseason = FALSE,  # it says too few data points to plot when split by month 
            assessment = TRUE, # assessment line
            yname = "Turbidity")

#---- Multiple variable reporting with global variables ----
park = "MABI"
sitecode = "NETN_MABI_SA00"
cat = "physical"

water_labeller <- function(Characteristic){
  paste0(getCharInfo(netnwd, park, sitecode, category = cat, info = "DisplayName"),
         " (",
         getCharInfo(netnwd, park, sitecode, category = cat, info = "Units"), ")")
}

waterseries(netnwd, park, sitecode, category = cat, layers = 'points', assessment = FALSE, censored = FALSE, yname = "") + 
  facet_wrap(~Characteristic, scales = 'free_y', labeller = as_labeller(water_labeller), strip.position = 'left')+
  theme(legend.position = "none", strip.placement = "outside", strip.background = element_blank())

#-------------------------------------------------------------------------------
# Summarizing by month

# Create Pogue Brook dataframe
pogue_br_df <- getWData(netnwd, park = "MABI", site = "NETN_MABI_SA00")
str(pogue_br_df)

# Pivot wider so each parameter gets its own column
pogue_br_df_wide <- pivot_wider(pogue_br_df, names_from = "Characteristic",
                                values_from = "Value") 

# Subset to DO (mgL) and remove NAs
pogue_br_df_DOmgl <- pogue_br_df_wide %>% select(Date, SampleDepth, Censored, 
                                                 ValueCen, TextValue, Category,
                                                 Site, Park, DO_mgL) %>% 
                                          filter(!is.na(DO_mgL))

# Summarize?
monthly_avg_DOmgl <- pogue_br_df_DOmgl %>% mutate(month = month(Date)) %>% 
                                 group_by(month) %>% 
                                 summarize(avg_DO = mean(DO_mgL))

# Plot means
plot <- monthly_avg_DOmgl %>% 
  ggplot(aes(x = month, y = avg_DO)) +
  geom_ribbon(aes(ymin = avg_DO - 1, ymax = avg_DO + 1), fill = "gray70") +
  geom_point()
  
t.test(pogue_br_df_DOmgl$DO_mgL)

# ------------------------------------------------------------------------------
# Building a 95 percentile range function from existing historic data

# Set global variables
yr = 2019 # this is the target year for the analysis
park = "MIMA"
sitecode = "NETN_MIMA_SA00"
cat = "physical"
char_list = getCharInfo(netnwd, park = park, sitecode = sitecode, category = cat, info = "CharName")
char = "DO_mgL"
# everything except turbidity works for MABI

water_dat <- getWData(netnwd, park = park, sitecode = sitecode, category = cat)

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

# Get bounds for set characteristic
char_pct <- water_pct %>% filter(Characteristic == char) %>% 
  mutate(month_ch = as.character(month))

# Subset raw water data to only target year char data
char_dat_2019 <- water_dat %>% filter(year(Date) == yr) %>% 
  filter(Characteristic == char) %>% # is there a way to combine these filters? 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
  droplevels() %>%
  # months as characters to be able to join the tables on month
  mutate(month_ch = as.character(month))

# Join tables
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









