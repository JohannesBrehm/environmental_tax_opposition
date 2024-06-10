#### Clear the space ####
rm(list = ls()); gc()

#### Packages #### 
library(haven)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(extrafont)



#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################



#### Data preparation #### 
data <- read_dta("data\\gaps.dta")
data = data[,c("c_alphan","gap")]




## oldschool plot
data %>%
  ggplot(aes(x=reorder(c_alphan,gap),y=gap)) +
  geom_point(size = 3, colour = "black", aes(color=)) + 
  geom_segment(aes(xend = c_alphan, yend = 0), size = 1.2)+
  geom_label(aes(c_alphan, gap+0.005, label = signif(gap,2)), colour = "darkred", nudge_x = 0.35, size = 4)+
  labs(y= "Change in share of tax opposition 2010-2020", x="Country") 




## newschool plot
 data %>%
  ggplot(aes(x=reorder(c_alphan,gap),y=gap)) +
  geom_segment(aes(xend = c_alphan, yend = 0), size = 1.2, colour = "#7495A6")+
  geom_point(size = 6, colour = "#15607A", aes(color=)) + 
  geom_label(aes(c_alphan, gap-0.014, label = signif(gap,2)), colour = "#15607A", size = 3.3) + 
  labs(y= "Change in share of tax opposition 2010-2020", x="Country")  +
  theme_minimal(base_size = 13)  +
  scale_y_continuous(limits = c(-0.18, 0.18), breaks = seq(-0.18, 0.18, by = 0.06)) + # y ticks
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", size = 1) +  # Add horizontal line at y = 0
  theme(axis.line.y = element_line(color = "black"),  # Set the color of the y-axis line to black
        panel.border = element_blank())  # Remove the border around the plot


gap_plot <- data %>%
  ggplot(aes(x = reorder(c_alphan, gap), y = gap)) +
  geom_segment(aes(xend = c_alphan, yend = 0, color = gap), size = 1.2) +
  geom_point(aes(color = gap), size = 6) + 
  geom_label(aes(c_alphan, gap - 0.014, label = signif(gap, 2)), color = "#15607A", size = 3.3) + 
  labs(y = "Change in share of tax opposition 2010-2020", x = "Country") +
  scale_color_gradient2(low = "#15607A", mid = "#C7CDD1", high = "#A63716") +  # Set the color scale based on "gap" values
  theme_minimal(base_size = 13) +
  scale_y_continuous(limits = c(-0.18, 0.18), breaks = seq(-0.18, 0.18, by = 0.06)) +  # y ticks
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", size = 1) +  # Add a horizontal line at y = 0
  theme(axis.line.y = element_line(color = "black"),  # Set the color of the y-axis line to black
        panel.border = element_blank()) +  # Remove the border around the plot
  guides(color = "none")  # Remove the legend


ggsave(plot = gap_plot, width = 11, height = 7, device = png, dpi = 600, filename = "gap_plot.png", path = "output//")








