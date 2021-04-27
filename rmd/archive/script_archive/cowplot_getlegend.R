# load libraries 
library(NCRNWater)
library(tidyverse)
library(plotly)
library(purrr)
library(htmltools)
library(leaflet)
library(viridis)
library(cowplot)

# create basic test plot
iris_plot <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length))+
  geom_point(aes(color = Species))+
  theme_classic()

tooth_plot <- ggplot(ToothGrowth, aes(x = supp, y = len))+
  geom_boxplot(aes(fill = supp))+
  facet_grid(. ~ dose)+
  labs(title="Tooth growth of guinea pigs by supplement type and dosage (mg)",
       x="Supplement type",
       y="Tooth length",
       fill=NULL)+
  theme(legend.position="none")

tooth_plot

# extract legend
legend <- get_legend(tooth_plot)

grids <- ggdraw(plot_grid(
                  plot_grid(iris_plot, tooth_plot, ncol=1, axis="l", align='v'),
                  plot_grid(NULL, legend, ncol=1),
                  rel_widths=c(1, 0.3))
                )

grids

ggplotly(grids)

# make plotly objects
tooth_plotly <- ggplotly(tooth_plot)
iris_plotly <- ggplotly(iris_plot)

subplots <- subplot(tooth_plotly, iris_plotly, legend)


