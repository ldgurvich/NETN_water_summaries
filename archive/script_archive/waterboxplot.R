waterboxplot <- function(sitecode, charname) {
  
  # params for package function
  parkcode <- params$park
  category <- params$cat
  object <- netnwd
  assessment <- TRUE # function param for assessment line
  
  # Compile water data for target years (from - current) and add columns for month, 
  # historical and current year data, upper and lower assessment, and outlier color
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
           pcolor = ifelse(!is.na(UpperPoint) & ValueCen_curr > UpperPoint, "Poor WQ value",
                           ifelse(!is.na(LowerPoint) & ValueCen_curr < LowerPoint, "Poor WQ value", "Current value"))) %>%
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
  
  # set y axis style 
  yaxis = list(
    title = yname,
    showline = TRUE,
    showgrid = FALSE,
    autotick = TRUE,
    ticks = "outside"
  )
  
  # set x axis style
  xaxis = list(
    title = FALSE, 
    showline = TRUE,
    showgrid = FALSE,
    autotick = FALSE,
    ticks = "outside",
    #ticktext = list("Jun", "Aug"),
    ticktext = sort(unique(wdat$month)),
    #tickvals = list(6, 8),
    tickvals = sort(unique(wdat$month_num)),
    tickmode = "array"
  )
  
  color_map = c("Poor WQ value" = "orange", "Current value" = "black")
  
  p <- plot_ly(wdat_hist, x = ~month_num, y = ~ValueCen) %>%
    add_boxplot(boxpoints = "outliers", name = "Historic range", 
                marker = list(symbol='asterisk-open', size = 7, color = "#1378b5"),
                fillcolor = list(color = "#1378b5", alpha = 0.85),
                line = list(color = "#1378b5")) %>% #, showlegend = FALSE) %>%  
    add_markers(data = wdat_curr, 
                #marker = list(color = wdat_curr$pcolor, size = 7), 
                marker = list(color = color_map[wdat_curr$pcolor], size = 7),
                split = wdat_curr$pcolor,
                text = paste0(wdat_curr$month, " ", current, "<br>",
                              param_name, ": ", round(wdat_curr$ValueCen_curr, 1), " ", unit), 
                hoverinfo = "text") %>%
    layout(xaxis = xaxis, yaxis = yaxis, legend = list(orientation = "h"))
  
  # set value for WQ threshold line
  UpperPoint <- unique(wdat_curr$UpperPoint)
  
  # Find min and max months
  wq_x <- min(unique(wdat$month_num))
  wq_xend <- max(unique(wdat$month_num))
  
  # Calculate length of WQ line based on plotted months
  wq_xend <- ifelse(wq_x == 6, wq_xend+1,
                    ifelse(wq_x == 5, wq_xend+1.5,
                           NA))
  wq_x <- ifelse(wq_x == 6, wq_x-1,
                 ifelse(wq_x == 5, wq_x-1.5,
                        NA))
  
  # if value is not NA, add WQ threshold 
  ifelse(!is.na(UpperPoint), 
         p <- p %>% add_segments(y = UpperPoint, yend = UpperPoint,
                                 x = wq_x, xend = wq_xend,
                                 text = paste("Upper", param_name, "threshold:", UpperPoint, unit),
                                 hoverinfo = "text",
                                 line = list(color = "black", dash = "dash"),
                                 name = "WQ threshold"),
         NA)
  
  return(p)
  
}