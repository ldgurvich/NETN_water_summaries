#----Libraries----
library(NCRNWater)
library(tidyverse)
library(plotly)
library(purrr)
library(htmltools)
library(leaflet)
library(viridis)
library(kableExtra)

#----Global variables----
path = "C:/Users/Diana/Documents/NETN/Water/data"
#path = "D:/NETN/R_Dev/Water/NCRNWaterViz/Data/NETN"
netnwd <- importNCRNWater(Dir = path, 
                          Data = "Water Data.csv", 
                          MetaData = "VizMetaData.csv")

# Variables for cat asis code, which can't take params
parkcode = "MABI"
current = 2019
from = 2006
to = 2018
category = "nutrients"

# Long park name
long_name = getParkInfo(netnwd, park = parkcode, info = "ParkLongName")

# List of sites
site_list_all <- getSiteInfo(netnwd, park = parkcode, info = "SiteCode")
sites_curr <- getWData(netnwd, park = parkcode, years = current) %>% 
              select(Site) %>% unique()

#Set up site_list so only includes sites sampled in current year
site_list <- site_list_all[site_list_all %in% sites_curr$Site] 

# Create site key for sitecode and type
site_key <- data.frame(Site = getSiteInfo(netnwd, parkcode = parkcode, info = "SiteCode"),
                       SiteType =  getSiteInfo(netnwd, parkcode = parkcode, info = "type"))

show_comp <- ifelse(length(site_list) > 1, TRUE, FALSE)

#---------------
# Plotly boxplot
#---------------
char_list <- getCharInfo(netnwd, parkcode = parkcode, category = category, 
                         info = "CharName") %>% unique() 

sitecode <- site_list[1]
charname <- char_list[2]
object <- netnwd
assessment <- TRUE

wdat <- getWData(object, parkcode = parkcode, sitecode = sitecode, 
                 charname = charname, category = category,
                 years = from:current) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE), 
         month_num = as.numeric(month), 
         year = lubridate::year(Date),
         ValueCen_curr = ifelse(year == current, ValueCen, NA),
         ValueCen_hist = ifelse(year != current, ValueCen, NA),
         LowerPoint = ifelse(assessment == TRUE, 
                             getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                         charname = charname, category = category, 
                                         info = "LowerPoint"), NA),
         UpperPoint = ifelse(assessment == TRUE, 
                             getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                                         charname = charname, category = category, 
                                         info = "UpperPoint"), NA), 
         pcolor = ifelse(!is.na(UpperPoint) & ValueCen_curr > UpperPoint, "orange",
                         ifelse(!is.na(LowerPoint) & ValueCen_curr < LowerPoint, "orange", "black"))) %>%
  arrange(Date) %>% 
  left_join(., site_key, by = "Site")

# Filter target months based on site type
# note: this may have problems later if target months are different
if (all(wdat$SiteType == "Lake")) {
  wdat <- filter(wdat, month == "Jun" | month == "Aug")} else {
    wdat <- filter(wdat, month == "May" | month == "Aug")
  }

wdat_hist <- wdat %>% filter(year <= to)
wdat_curr <- wdat %>% filter(year == current)

# attempt at setting break points outside of plotly
# UpperPoint <- unique(wdat_curr$UpperPoint)
# wdat_curr$color <- cut(wdat_curr$ValueCen_curr,
#                        breaks = c(0, UpperPoint, wdat_curr$ValueCen_curr > UpperPoint),
#                        labels = c("black", "orange"))

param_name <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                          charname = charname, info = "CategoryDisplay") %>% unique()

unit <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
                    charname = charname, info = "Units") %>% unique()

yname <- ifelse(charname != "pH", 
                paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                   charname = charname, info = "CategoryDisplay") %>% unique(), " (", 
                       getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                   charname = charname, info = "Units") %>% unique(),                           
                       ")"),
                paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
                                   charname = charname, info = "CategoryDisplay") %>% unique()))

style = list(
  showline = TRUE,
  showgrid = FALSE,
  autotick = TRUE,
  ticks = "outside"
)

# put shapes = list(hline()) in layout
# creates line but it doesn't show up in legend
# hline <- function(y = UpperPoint, color = "black") {
#   list(
#     type = "line", 
#     x0 = 0, 
#     x1 = 1, 
#     xref = "paper",
#     y0 = y, 
#     y1 = y, 
#     dash = "dash",
#     line = list(color = color)
#   )
# }

UpperPoint <- unique(wdat_curr$UpperPoint)

p <- plot_ly(wdat_hist, x = ~month_num, y = ~ValueCen) %>%
  add_boxplot(boxpoints = "outliers", marker = list(symbol='asterisk-open', size = 7)) %>%  
  add_markers(data = wdat_curr, marker = list(color = wdat_curr$pcolor, size = 7), split = wdat_curr$pcolor) %>%
  add_segments(y = UpperPoint, yend = UpperPoint, x = 5, xend = 9, line = list(color = "black", dash = "dash")) %>% 
  layout(xaxis = style, yaxis = style)

p 

str(wdat)

# #----Boxplot----
# 
# #waterboxplot <- function(sitecode, charname) {
#  
# sitecode <- site_list[1]
# charname <- "TP_ugL"
#  
#   # params for package function
#   #parkcode <- params$park
#   #category <- params$cat
#   object <- netnwd
#   assessment <- TRUE # function param for assessment line
#   
#   # Compile water data for target years (from - current) and add columns for month, 
#   # historical and current year data, upper and lower assessment, and outlier color
#   wdat <- getWData(object, parkcode = parkcode, sitecode = sitecode, 
#                    charname = charname, category = category,
#                    years = from:current) %>% 
#     mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE), 
#            month_num = as.numeric(month), 
#            year = lubridate::year(Date),
#            ValueCen_curr = ifelse(year == current, ValueCen, NA),
#            ValueCen_hist = ifelse(year != current, ValueCen, NA),
#            LowerPoint = ifelse(assessment == TRUE, 
#                                getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
#                                            charname = charname, category = category, 
#                                            info = "LowerPoint"), NA),
#            UpperPoint = ifelse(assessment == TRUE, 
#                                getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
#                                            charname = charname, category = category, 
#                                            info = "UpperPoint"), NA),
#            pcolor = ifelse(!is.na(UpperPoint) & ValueCen_curr > UpperPoint, "orange",
#                     ifelse(!is.na(LowerPoint) & ValueCen_curr < LowerPoint, "orange", "black"))) %>% 
#     arrange(Date) %>% 
#     left_join(., site_key, by = "Site")
#   
#   #wdat_mon <- wdat %>% filter(month_num == 6 & !is.na(ValueCen_hist)) #%>% 
#     #boxplot.stats(wdat$ValueCen_hist)
# 
#   wdat_hist <- wdat_mon$ValueCen_hist
#   
#   #boxplot.stats(wdat_hist)
#   
#   # Filter target months based on site type
#   # note: this may have problems later if target months are different
#   if (all(wdat$SiteType == "Lake")) {
#     wdat <- filter(wdat, month == "Jun" | month == "Aug")} else {
#       wdat <- filter(wdat, month == "May" | month == "Aug")
#     }
#   
#   wdat_curr <- wdat %>% filter(year == current)
#   
#   param_name <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
#                             charname = charname, info = "CategoryDisplay") %>% unique()
#   
#   unit <- getCharInfo(object, parkcode = parkcode, sitecode = sitecode, 
#                       charname = charname, info = "Units") %>% unique()
#   
#   yname <- ifelse(charname != "pH", 
#                   paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
#                                      charname = charname, info = "CategoryDisplay") %>% unique(), " (", 
#                          getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
#                                      charname = charname, info = "Units") %>% unique(),                           
#                          ")"),
#                   paste0(getCharInfo(object, parkcode = parkcode, sitecode = sitecode,
#                                      charname = charname, info = "CategoryDisplay") %>% unique()))
#   
#   
#   p <- ggplot(wdat, aes(x=month, y=ValueCen_hist, color=month))+
#     geom_point()
# 
#   # Create boxplot with historic data and overlay target year measurements
#   p <- ggplot(wdat, aes(x=month, y=ValueCen_hist)) +
#     geom_boxplot(color="#1378b5", fill="#76b4db", alpha = 0.85) +
#     # geom_point(aes(y=ValueCen_curr, color = pcolor, fill = pcolor,
#     #            text = paste0(month, " ", params$current, "<br>",
#     #            param_name, ": ", round(ValueCen_curr, 1), " ", unit)),
#     #            color = wdat$pcolor, fill = wdat$pcolor, shape = 21) +
#     geom_point(data = wdat_curr,
#                aes(y=ValueCen_curr, color = pcolor, fill = pcolor,
#                    text = paste0(month, " ", current, "<br>",
#                                  param_name, ": ", round(ValueCen_curr, 1), " ", unit)),
#                color = wdat_curr$pcolor, fill = wdat_curr$pcolor, shape = 21)+
#     
#     # Upper and lower assessment lines
#     # Note: no nutrient has a lower point assessment line. This is included for the legend. 
#     # Putting linetype in aes turns the legend on
#     #{if(assessment == TRUE)
#       geom_hline(aes(yintercept = LowerPoint, linetype="dashed", text = paste("Lower", param_name, "threshold:", LowerPoint, unit)),
#                  color = "#212121")+ #}+
#     #{if(assessment == TRUE)
#       geom_hline(aes(yintercept = UpperPoint, text = paste("Upper", param_name, "threshold:", UpperPoint, unit)),
#                  linetype="dashed", color = "#212121")+ #}+
#     
#     # Theme and labels
#     forestMIDN::theme_FVM() +
#     #theme(legend.position="bottom")+
#     labs(y = yname, x = NULL, linetype = NULL) 
#   
#   p <- ggplotly(p, tooltip = "text")
#   
#   # turn off lower threshold line legend
#   p$x$data[[3]]$showlegend = FALSE
#   p$x$data[[3]]$legendgroup = NA
#   
#   # turn on upper threshold line legend and rename
#   p$x$data[[4]]$showlegend = TRUE
#   p$x$data[[4]]$name = "WQ Threshold"
#   
#   # plotly makes two types of outliers that are controlled via different traces
#   p$x$data[[1]]$marker$symbol = "asterisk" # this changes all outliers to asterisks
#   # change color and size of first set (not as extreme outliers)
#   p$x$data[[1]]$marker$outliercolor = "#1378b5"
#   p$x$data[[1]]$marker$size = 7
#   # change color and size of second set (extreme outliers)
#   p$x$data[[1]]$marker$line$color = "#1378b5"
#   p$x$data[[1]]$marker$line$width = 1
#   
#   # attempt to turn on outlier legend
#   p$x$data[[1]]$showlegend = TRUE
#   p$x$data[[1]]$marker$showlegend = TRUE
#   p$x$data[[1]]$marker$name = "Outlier"
#   
#   p
  
#   # boxplot.stats stack exchange answer
#   # since you can't identify outliers, it writes over them with a function
#   set.seed(1234)
#   a<-rnorm(50)
#   a2 <- rnorm(50, 1)
#   
#   a_plot <- plot_ly(y = a, type = 'box') %>%
#     add_trace(y = a2) %>%
#     layout(title = 'Box Plot',xaxis = list(title = "cond", showgrid = F), yaxis = list(title = "rating"), 
#            annotations = list(
#              x = -0.01,
#              y = boxplot.stats(a)$out, 
#              text = "Outlier",
#              showarrow = FALSE,
#              xanchor = "right"
#            ))
#   
# a_stats <- boxplot.stats(a)
#   
  
  


  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  