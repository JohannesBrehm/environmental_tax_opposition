
# Load packages
library(tidyverse)
library(haven)
library(stats)
library(dplyr)
library(ggplot2)
library(extrafont)
library(sciplot)



#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################


# Load data
data <- read_dta("data//ISSP2010\\ISSP_2010_basic_cleaning.dta")

# Choose specific columns to keep
vdata <- data[, c("v30", "c_alphan")]

# Put 8 values (can't choose) into answer category 3 (neither willing nor unwilling)
vdata %>% count(v30) # to check whether it works
vdata$v30[vdata$v30 == 8] <- 3
vdata %>% count(v30) # 8 values added to category 3 

# Remove 9 values (no answer)
vdata <- vdata[vdata$v30 != 9, ]

# Create estimate for average Likert scale
mean_data_v30 <- vdata %>%
  group_by(c_alphan) %>%
  summarize(mean = mean(v30), se = se(v30))

# Show number of observations per country
mean_data_v30$n <- vdata %>%
  count(c_alphan)

# Reverse the scale for consistent plotting
mean_data_v30$mean_rev <- 6 - mean_data_v30$mean # flip scale from "1 to 5" to "5 to 1"
mean_data_v30$mean_rev <- (mean_data_v30$mean_rev - 1) / 4 # use range between 1 and 5 ( 5-1=4 ) to calculate percentage of estimate on that scale ( (mean - 1)/4 )

# Custom labels for Likert scale
custom_labels <- c("Very unwilling", "Fairly unwilling", "Neither willing nor unwilling / Do not know", "Fairly willing", "Very willing")

# Updated list of country codes
country_codes <- c("TW", "JP", "KR", "SI", "SK", "CH", "FI", "LT", "AT", "FR", "DE", "HR", "ES", "SE", "NZ", "AU", "DK", "US", "NO", "IS")

# Sort for descending mean
mean_data_v30 <- mean_data_v30[order(-mean_data_v30$mean_rev), ]

# sort vdata for declining mean using country code
vdata$c_alphan <- factor(vdata$c_alphan, levels = mean_data_v30$c_alphan)



# Create a bar chart for v30
plot_v30 <- ggplot() +
  geom_bar(data = vdata, aes(x = factor(c_alphan), fill = factor(v30)), position = "fill", stat = "count") +
  geom_point(data = mean_data_v30, aes(x = factor(c_alphan), y = mean_rev), color = "black", size = 5) +
  labs(x = "", y = "Share of population in %", title = "") +
  scale_fill_manual(values = c("1" = "#15607A", "2" = "#7495A6", "3" = "#C7CDD1", "4" = "#CC734D", "5" = "#A63716"),
                    labels = c("Very willing", "Fairly willing", "Neither willing nor unwilling / Do not know", "Fairly unwilling", "Very unwilling")) +
  scale_y_continuous(labels = scales::percent_format(), sec.axis = sec_axis(~ (.), breaks = seq(0, 1, by = 0.25), labels = function(x) str_wrap(custom_labels, width = 10), name = "Average Likert rating")) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        axis.text = element_text(size = 9),
        axis.text.y.right = element_text(),
        axis.title.y = element_text(size = 9),
        axis.title.y.right = element_text(angle = 90),
        legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom",
        text = element_text(family = "Calibri", size = 9)) 

# Print the plots
print(plot_v30)



# Save plot as png image
ggsave(plot = plot_v30, width = 11, height = 7, device = png, dpi = 600, filename = "WTP_taxes_2010.png", path = "output//")

