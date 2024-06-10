
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
data <- read_dta("data//ISSP2020//ZA7650_v2-0-0.dta")

# Choose specific columns to keep
vdata <- data[, c("v27", "c_alphan")]

# Put -8 values (can't choose) into answer category 3 (neither willing nor unwilling)
vdata %>% count(v27) # to check whether it works
vdata$v27[vdata$v27 == -8] <- 3
vdata %>% count(v27) # -8 values added to category 3 (4404+488=4892)



# Remove -9 values (no answer)
vdata <- vdata[vdata$v27 != -9, ]

# Create estimate for average Likert scale
mean_data_v27 <- vdata %>%
  group_by(c_alphan) %>%
  summarize(mean = mean(v27), se = se(v27))

# Show number of observations per country
mean_data_v27$n <- vdata %>%
  count(c_alphan)

# Reverse the scale for consistent plotting
mean_data_v27$mean_rev <- 6 - mean_data_v27$mean # flip scale from "1 to 5" to "5 to 1"
mean_data_v27$mean_rev <- (mean_data_v27$mean_rev - 1) / 4 # use range between 1 and 5 ( 5-1=4 ) to calculate percentage of estimate on that scale ( (mean - 1)/4 )


# 01 Create subsamples by countries

# High-income countries sub-sample
high_income_countries <- c("AU", "AT", "HR", "DK", "FI", "FR", "DE", "HU", "IS", "IT", "JP", "KR", "LT", "NZ", "NO", "SK", "SI", "ES", "SE", "CH", "TW", "US")
vdata_high_inc <- subset(vdata, c_alphan %in% high_income_countries)
mean_data_v27h <- subset(mean_data_v27, c_alphan %in% high_income_countries)

# Sort for decending mean
mean_data_v27h_sorted <- mean_data_v27h[order(-mean_data_v27h$mean_rev), ]

# Non-high-income countries sub-sample
non_high_income_countries <- c("IN", "PH", "CN", "RU", "ZA", "TH")
vdata_non_high_inc <- subset(vdata, c_alphan %in% non_high_income_countries)
mean_data_v27m <- subset(mean_data_v27, c_alphan %in% non_high_income_countries)


# Sort for descending mean
mean_data_v27m_sorted <- mean_data_v27m[order(-mean_data_v27m$mean_rev), ]


# Add labels for Income group
vdata_high_inc$income_group <- "High Income"
mean_data_v27h_sorted$income_group <- "High Income"

vdata_non_high_inc$income_group <- "Middle Income"
mean_data_v27m_sorted$income_group <- "Middle Income"


# Combine high and non-high income datasets back into one dataset

combined_vdata <- bind_rows(vdata_high_inc, vdata_non_high_inc)

combined_mean_data_v27 <- bind_rows(mean_data_v27h_sorted, mean_data_v27m_sorted)

combined_country_codes <- c("AU", "AT", "HR", "DK", "FI", "FR", "DE", "HU", "IS", 
                            "IT", "JP", "KR", "LT", "NZ", "NO", "SK", "SI", "ES", "SE", "CH", "TW", "US","IN", "PH", "CN", "RU", "ZA", "TH")

# sort vdata for declining mean using country code

combined_vdata$c_alphan <- factor(combined_vdata$c_alphan, levels = combined_mean_data_v27$c_alphan)


# Define custom labels for Likert scale
custom_labels <- c("Very unwilling", "Fairly unwilling", "Neither willing nor unwilling / Do not know", "Fairly willing", "Very willing")



#PLOT
plot_v27_income <- ggplot() +
  geom_bar(data = combined_vdata, aes(x = c_alphan, fill = factor(v27)), position = "fill", stat = "count") +
  geom_point(data = combined_mean_data_v27, aes(x = c_alphan, y = mean_rev), color = "black", size = 5) +
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
        text = element_text(family = "Calibri", size = 9))+
  facet_grid(. ~ income_group, switch = 'x', scales = 'free_x', space = "free_x") # break in x-axis to differentiate for income

print(plot_v27_income)

# Save plot as png image
ggsave(plot = plot_v27_income, width = 11, height = 7, device = png, dpi = 600, filename = "WTP_taxes_byincome.png", path = "output//")


