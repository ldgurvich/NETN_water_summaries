#-----------------------------
# Convert svg to png for easier plotly handling
#-----------------------------
library(magick)

waterbands_leg <- image_read_svg("./rmd/images/water_legend_wide_pwq.svg", width = 4880)
waterbands_leg_png <- image_convert(waterbands_leg, "png") %>% 
  #image_resize("1000x") %>% 
  image_border("white", "150x100") %>% #add white margin
  image_border("DimGrey", "10x10") #add grey border

waterbands_leg_png

magick::image_write(waterbands_leg_png, 
                    path = "./rmd/images/water_legend_wide_pwq.png", 
                    format = 'png',
                    density = 1200,
                    depth = 16
                    )

?image_write
#test <- magick::image_read("./rmd/images/water_legend_wide_pwq.png") #test that it worked

