
#### Clear the space ####
rm(list = ls()); gc()


#### Load Packages ####
library(rworldmap)
library(RColorBrewer)
library(classInt)
library(haven)
library(ggplot2)
library(rworldxtra)
library(dplyr)
library(ggplot2)
library(rworldmap) # for getMap()
library(rworldxtra)
#> Loading required package: sp
#> ### Welcome to rworldmap ###
#> For a short introduction type :   vignette('rworldmap')
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
library(classInt)
library(extrafont)



#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################


world_sf <- st_as_sf(getMap(resolution = "high"))

crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"


countries <- read_dta("data\\ISSP2020\\wtp_for_map.dta")

world_sf_joined = left_join(world_sf, countries, by = "NAME_SORT", relationship = "many-to-one")




# Calculate the minimum and maximum values for the legend
min_value <- round(min(world_sf_joined$wtp_low, na.rm = TRUE), 2)
max_value <- round(max(world_sf_joined$wtp_low, na.rm = TRUE), 2)

world_map <- ggplot(world_sf_joined) + 
  geom_sf(size = 0.3/.pt, aes(fill = wtp_low), color = "white") +  
  coord_sf(crs = crs_robin) + 
  scale_fill_continuous(
    low = "#f6c8ba",
    high = "#A63716",
    na.value = "grey90",
    name = "Share of the population that is not willing to pay higher taxes",
    limits = c(min_value,max_value), breaks = c(min_value, 0.32, 0.47, 0.62, max_value),labels = c("18% (Minimum)", "23%", "34%", "44%", "77% (Maximum)"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.hjust = 0.5,
      barwidth = 25,
      breaks = 10
    )
  ) + 
  theme(
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    text = element_text(family = "Calibri", size = 12),
    panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")
  )

world_map


# Save plot as png image
ggsave(plot = world_map, width = 11, height = 7, device = png, dpi = 600, filename = "world_map_noWTP_projection.png", path = "output\\")





















