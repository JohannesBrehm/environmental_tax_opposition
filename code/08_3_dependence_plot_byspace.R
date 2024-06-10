#### Clear the space ####
rm(list = ls()); gc()

#### Packages #### 
library(randomForestSRC)
#library(haven)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(data.tree)
library(dplyr)
library(broom)
library(readr)
library(profvis)
library(parallel)
library(haven)
library(extrafont)
library(readxl)
library(writexl)
library(stringr)

library(doParallel)
library(purrr)
library(data.table)



#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################



#### Data preparation #### 
data <- read_dta("data\\ISSP2020\\ISSP2020_clean_wcountry.dta")
data = data[,c("age","belief_cc","brown_job","children","concern","direct_experience","donation",
               "education","env_committment","env_consumption","cc_env_problem","female",
               "gap_perception","growth_harms","knowledge","large_house",
               "modern_life_harms","nowtp_taxes","petition","INC_decile","risk_perception",
               "rural","scepticism","science_solution","seriousness","time_car","trust_institutions",
               "trust_people","enjoy_nature","vote_le",
               "worry_jobs","c_alphan","plane","priorization_env","reciprocity",
               "religiousness","right_party","everyday_life",
               "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]

#set dependent variable as factor variable
data$nowtp_taxes <- as.factor(data$nowtp_taxes)


#subsamples by countries 
# High-income countries sub-sample
high_income_countries <- c("AU", "AT", "HR", "DK", "FI", "FR", "DE", "HU", "IS", "IT", "JP", "KR", "LT", "NZ", "NO", "SK", "SI", "ES", "SE", "CH", "TW", "US")
data_high_inc <- subset(data, c_alphan %in% high_income_countries)
#without c_alphan
data_high_inc = data_high_inc[,c("age","belief_cc","brown_job","children","concern","direct_experience","donation",
                                 "education","env_committment","env_consumption","cc_env_problem","female",
                                 "gap_perception","growth_harms","knowledge","large_house",
                                 "modern_life_harms","nowtp_taxes","petition","INC_decile","risk_perception",
                                 "rural","scepticism","science_solution","seriousness","time_car","trust_institutions",
                                 "trust_people","enjoy_nature","vote_le",
                                 "worry_jobs","plane","priorization_env","reciprocity",
                                 "religiousness","right_party","everyday_life",
                                 "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]

# Non-high-income countries sub-sample
non_high_income_countries <- c("IN", "PH", "CN", "RU", "ZA", "TH")
data_non_high_inc <- subset(data, c_alphan %in% non_high_income_countries)
#without c_alphan
data_non_high_inc = data_non_high_inc[,c("age","belief_cc","brown_job","children","concern","direct_experience","donation",
                                         "education","env_committment","env_consumption","cc_env_problem","female",
                                         "gap_perception","growth_harms","knowledge","large_house",
                                         "modern_life_harms","nowtp_taxes","petition","INC_decile","risk_perception",
                                         "rural","scepticism","science_solution","seriousness","time_car","trust_institutions",
                                         "trust_people","enjoy_nature","vote_le",
                                         "worry_jobs","plane","priorization_env","reciprocity",
                                         "religiousness","right_party","everyday_life",
                                         "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]





#run random forest
set.seed(2023)
high_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1) 
print(high_forest)

set.seed(2023)
non_high_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_non_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1) 
print(non_high_forest)




## partial effect plots

#1
partial.obj1 <- partial.rfsrc(high_forest,
                              partial.xvar = "env_committment",
                              pred.var = "env_committment",
                              partial.type = NULL,
                              partial.values = high_forest$xvar$env_committment)

partial.obj2 <- partial.rfsrc(non_high_forest,
                              partial.xvar = "env_committment",
                              pred.var = "env_committment",
                              partial.type = NULL,
                              partial.values = non_high_forest$xvar$env_committment)

## extract partial effects for each species outcome
pdta1 <- get.partial.plot.data(partial.obj1, target = "1")
pdta2 <- get.partial.plot.data(partial.obj2, target = "1")

# Unlist the data
pdta1 = data.frame(unlist(pdta1$x),unlist(pdta1$yhat))
pdta2 = data.frame(unlist(pdta2$x),unlist(pdta2$yhat))

pdta1 <- pdta1 %>% 
  mutate(region = "High income")
pdta2 <- pdta2 %>% 
  mutate(region = "Middle income")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta1, aes(x = unlist.pdta1.x., y = unlist.pdta1.yhat., color = region, linetype = region), size = 1) +
  geom_line(data = pdta2, aes(x = unlist.pdta2.x., y = unlist.pdta2.yhat., color = region, linetype = region), size = 1) +
  geom_point(data = pdta1, aes(x = unlist.pdta1.x., y = unlist.pdta1.yhat., color = region, shape = region, fill = region), size = 3) +
  geom_point(data = pdta2, aes(x = unlist.pdta2.x., y = unlist.pdta2.yhat., color = region, shape = region, fill = region), size = 3) +
  labs(
    x = "Committment to pro-environmental behavior",
    y = "",
    color = "region",
  ) +
  scale_color_manual(values = c("High income" = "#15607A", "Middle income" = "#15607A"), # both blue
                     labels = c("High income", "Middle income")
  ) +
  scale_linetype_manual(values = c("High income" = "solid", "Middle income" = "longdash"),  # Set line type for each year
                        labels = c("High income", "Middle income")) +
  scale_shape_manual(values = c("High income" = 21, "Middle income" = 23),  # Set shape to "23" for both years
                     labels = c("High income", "Middle income")) +
  scale_fill_manual(values = c("High income" = "#15607A", "Middle income" = "#15607A"),  # Set fill color for both years
                    labels = c("High income", "Middle income")) +
  scale_y_continuous(limits = c(0.42, 0.67), breaks = seq(0.42, 0.67, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 


ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400,  
       filename = "env_committment.png", path = "output\\variable_dependence_space")


#2
partial.obj3 <- partial.rfsrc(high_forest,
                              partial.xvar = "concern",
                              pred.var = "concern",
                              partial.type = NULL,
                              partial.values = high_forest$xvar$concern)

partial.obj4 <- partial.rfsrc(non_high_forest,
                              partial.xvar = "concern",
                              pred.var = "concern",
                              partial.type = NULL,
                              partial.values = non_high_forest$xvar$concern)

## extract partial effects for each species outcome
pdta3 <- get.partial.plot.data(partial.obj3, target = "1")
pdta4 <- get.partial.plot.data(partial.obj4, target = "1")

# Unlist the data
pdta3 = data.frame(unlist(pdta3$x),unlist(pdta3$yhat))
pdta4 = data.frame(unlist(pdta4$x),unlist(pdta4$yhat))

pdta3 <- pdta3 %>% 
  mutate(region = "High income")
pdta4 <- pdta4 %>% 
  mutate(region = "Middle income")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta3, aes(x = unlist.pdta3.x., y = unlist.pdta3.yhat., color = region, linetype = region), size = 1) +
  geom_line(data = pdta4, aes(x = unlist.pdta4.x., y = unlist.pdta4.yhat., color = region, linetype = region), size = 1) +
  geom_point(data = pdta3, aes(x = unlist.pdta3.x., y = unlist.pdta3.yhat., color = region, shape = region, fill = region), size = 3) +
  geom_point(data = pdta4, aes(x = unlist.pdta4.x., y = unlist.pdta4.yhat., color = region, shape = region, fill = region), size = 3) +
  labs(
    x = "Concerned about environmental issues",
    y = "",
    color = "region",
  ) +
  scale_color_manual(values = c("High income" = "#15607A", "Middle income" = "#15607A"), # both blue
                     labels = c("High income", "Middle income")
  ) +
  scale_linetype_manual(values = c("High income" = "solid", "Middle income" = "longdash"),  # Set line type for each year
                        labels = c("High income", "Middle income")) +
  scale_shape_manual(values = c("High income" = 21, "Middle income" = 23),  # Set shape to "23" for both years
                     labels = c("High income", "Middle income")) +
  scale_fill_manual(values = c("High income" = "#15607A", "Middle income" = "#15607A"),  # Set fill color for both years
                    labels = c("High income", "Middle income")) +
  scale_y_continuous(limits = c(0.42, 0.67), breaks = seq(0.42, 0.67, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 


ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400,  
       filename = "concern.png", path = "output\\variable_dependence_space")





#3
partial.obj5 <- partial.rfsrc(high_forest,
                              partial.xvar = "worry_jobs",
                              pred.var = "worry_jobs",
                              partial.type = NULL,
                              partial.values = high_forest$xvar$worry_jobs)

partial.obj6 <- partial.rfsrc(non_high_forest,
                              partial.xvar = "worry_jobs",
                              pred.var = "worry_jobs",
                              partial.type = NULL,
                              partial.values = non_high_forest$xvar$worry_jobs)

## extract partial effects for each species outcome
pdta5 <- get.partial.plot.data(partial.obj5, target = "1")
pdta6 <- get.partial.plot.data(partial.obj6, target = "1")

# Unlist the data
pdta5 = data.frame(unlist(pdta5$x),unlist(pdta5$yhat))
pdta6 = data.frame(unlist(pdta6$x),unlist(pdta6$yhat))

pdta5 <- pdta5 %>% 
  mutate(region = "High income")
pdta6 <- pdta6 %>% 
  mutate(region = "Middle income")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta5, aes(x = unlist.pdta5.x., y = unlist.pdta5.yhat., color = region, linetype = region), size = 1) +
  geom_line(data = pdta6, aes(x = unlist.pdta6.x., y = unlist.pdta6.yhat., color = region, linetype = region), size = 1) +
  geom_point(data = pdta5, aes(x = unlist.pdta5.x., y = unlist.pdta5.yhat., color = region, shape = region, fill = region), size = 3) +
  geom_point(data = pdta6, aes(x = unlist.pdta6.x., y = unlist.pdta6.yhat., color = region, shape = region, fill = region), size = 3) +
  labs(
    x = "Jobs/prices more important than environment",
    y = "Adjusted Probability of Tax Opposition",
    color = "region",
  ) +
  scale_color_manual(values = c("High income" = "#A63716", "Middle income" = "#C7CDD1"), # red in high and grey in middle income
                     labels = c("High income", "Middle income")
  ) +
  scale_linetype_manual(values = c("High income" = "solid", "Middle income" = "longdash"),  # Set line type for each year
                        labels = c("High income", "Middle income")) +
  scale_shape_manual(values = c("High income" = 21, "Middle income" = 23),  # Set shape to "23" for both years
                     labels = c("High income", "Middle income")) +
  scale_fill_manual(values = c("High income" = "#A63716", "Middle income" = "#C7CDD1"),  # Set fill color for both years
                    labels = c("High income", "Middle income")) +
  scale_y_continuous(limits = c(0.42, 0.67), breaks = seq(0.42, 0.67, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 


ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400,  
       filename = "worry_jobs.png", path = "output\\variable_dependence_space")










