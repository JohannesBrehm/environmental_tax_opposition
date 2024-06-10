#### Clear the space ####
rm(list = ls()); gc()

#### Packages #### 
library(randomForestSRC)
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

# 2020
y2020_data <- read_dta("data\\ISSP2020\\ISSP2020_clean_wcountry.dta")
y2020_data = y2020_data[,c("INC_decile","nowtp_taxes","concern","cc_env_problem","priorization_env","knowledge",
                           "scepticism","seriousness","everyday_life","risk_perception","trust_people","trust_institutions",
                           "reciprocity","env_committment","modern_life_harms","science_solution","growth_harms","worry_jobs",
                           "env_consumption","right_party","vote_le","religiousness","petition","donation","female","age", 
                           "education","gap_perception","rural","children","brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling","c_alphan")]

#set dependent variable as factor variable
y2020_data$nowtp_taxes <- as.factor(y2020_data$nowtp_taxes)

# 2010
y2010_data <- read_dta("data\\ISSP2010\\ISSP2010_clean_wcountry.dta")
y2010_data = y2010_data[,c("INC_decile","nowtp_taxes","concern","cc_env_problem","priorization_env","knowledge",
                           "scepticism","seriousness","everyday_life","risk_perception","trust_people","trust_institutions",
                           "reciprocity","env_committment","modern_life_harms","science_solution","growth_harms","worry_jobs",
                           "env_consumption","right_party","vote_le","religiousness","petition","donation","female","age", 
                           "education","gap_perception","rural","children","brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling","c_alphan")]

#set dependent variable as factor variable
y2010_data$nowtp_taxes <- as.factor(y2010_data$nowtp_taxes)


#subsamples relevant countries
# 2020
y2020_countries <- c("AT", "AU","CH", "DE", "DK", "ES", "FI", "FR", "HR", "IS", "JP", "KR", "LT", "NO", "NZ", "SE", "SI", "SK", "TW", "US")
data_y2020 <- subset(y2020_data, c_alphan %in% y2020_countries)
#without c_alphan
data_y2020 = data_y2020[,c("INC_decile","nowtp_taxes","concern","cc_env_problem","priorization_env","knowledge",
                           "scepticism","seriousness","everyday_life","risk_perception","trust_people","trust_institutions",
                           "reciprocity","env_committment","modern_life_harms","science_solution","growth_harms","worry_jobs",
                           "env_consumption","right_party","vote_le","religiousness","petition","donation","female","age", 
                           "education","gap_perception","rural","children","brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]

# 2010
y2010_countries <- c("AT", "AU","CH", "DE", "DK", "ES", "FI", "FR", "HR", "IS", "JP", "KR", "LT", "NO", "NZ", "SE", "SI", "SK", "TW", "US")
data_y2010 <- subset(y2010_data, c_alphan %in% y2010_countries)
#without c_alphan
data_y2010 = data_y2010[,c("INC_decile","nowtp_taxes","concern","cc_env_problem","priorization_env","knowledge",
                           "scepticism","seriousness","everyday_life","risk_perception","trust_people","trust_institutions",
                           "reciprocity","env_committment","modern_life_harms","science_solution","growth_harms","worry_jobs",
                           "env_consumption","right_party","vote_le","religiousness","petition","donation","female","age", 
                           "education","gap_perception","rural","children","brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]






#run random forest
set.seed(2023)
y2020_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2020), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)  
print(y2020_forest)

set.seed(2023)
y2010_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2010), ntree = 1000, nodesize = 1, mtry=6, nsplit=1) 
print(y2010_forest)




## partial effect plots



#1
partial.obj1 <- partial.rfsrc(y2020_forest,
                              partial.xvar = "env_committment",
                              pred.var = "env_committment",
                              partial.type = NULL,
                              partial.values = y2020_forest$xvar$env_committment)

partial.obj2 <- partial.rfsrc(y2010_forest,
                              partial.xvar = "env_committment",
                              pred.var = "env_committment",
                              partial.type = NULL,
                              partial.values = y2010_forest$xvar$env_committment)

## extract partial effects for each species outcome
pdta1 <- get.partial.plot.data(partial.obj1, target = "1")
pdta2 <- get.partial.plot.data(partial.obj2, target = "1")

# Unlist the data
pdta1 = data.frame(unlist(pdta1$x),unlist(pdta1$yhat))
pdta2 = data.frame(unlist(pdta2$x),unlist(pdta2$yhat))

pdta1 <- pdta1 %>% 
  mutate(year = "2020")
pdta2 <- pdta2 %>% 
  mutate(year = "2010")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta1, aes(x = unlist.pdta1.x., y = unlist.pdta1.yhat., color = year, linetype = year), size = 1) +
  geom_line(data = pdta2, aes(x = unlist.pdta2.x., y = unlist.pdta2.yhat., color = year, linetype = year), size = 1) +
  geom_point(data = pdta1, aes(x = unlist.pdta1.x., y = unlist.pdta1.yhat., color = year, shape = year, fill = year), size = 3) +
  geom_point(data = pdta2, aes(x = unlist.pdta2.x., y = unlist.pdta2.yhat., color = year, shape = year, fill = year), size = 3) +
  labs(
    x = "Committment to pro-environmental behavior",
    y = "",
    color = "Year",
  ) +
  scale_color_manual(values = c("2020" = "#15607A", "2010" = "#15607A"), #both blue
                     labels = c("2020", "2010")
  ) +
  scale_linetype_manual(values = c("2020" = "solid", "2010" = "longdash"),  # Set line type for each year
                        labels = c("2020", "2010")) +
  scale_shape_manual(values = c("2020" = 21, "2010" = 23),  # Set shape for both years
                     labels = c("2020", "2010")) +
  scale_fill_manual(values = c("2020" = "#15607A", "2010" = "#15607A"),  # Set fill color for both years
                    labels = c("2020", "2010")) +
  scale_y_continuous(limits = c(0.43, 0.65), breaks = seq(0.43, 0.65, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 





ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400, 
       filename = "env_committment.png", path = "output\\variable_dependence_time")






#2
partial.obj3 <- partial.rfsrc(y2020_forest,
                              partial.xvar = "concern",
                              pred.var = "concern",
                              partial.type = NULL,
                              partial.values = y2020_forest$xvar$concern)

partial.obj4 <- partial.rfsrc(y2010_forest,
                              partial.xvar = "concern",
                              pred.var = "concern",
                              partial.type = NULL,
                              partial.values = y2010_forest$xvar$concern)

## extract partial effects for each species outcome
pdta3 <- get.partial.plot.data(partial.obj3, target = "1")
pdta4 <- get.partial.plot.data(partial.obj4, target = "1")

# Unlist the data
pdta3 = data.frame(unlist(pdta3$x),unlist(pdta3$yhat))
pdta4 = data.frame(unlist(pdta4$x),unlist(pdta4$yhat))

pdta3 <- pdta3 %>% 
  mutate(year = "2020")
pdta4 <- pdta4 %>% 
  mutate(year = "2010")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta3, aes(x = unlist.pdta3.x., y = unlist.pdta3.yhat., color = year, linetype = year), size = 1) +
  geom_line(data = pdta4, aes(x = unlist.pdta4.x., y = unlist.pdta4.yhat., color = year, linetype = year), size = 1) +
  geom_point(data = pdta3, aes(x = unlist.pdta3.x., y = unlist.pdta3.yhat., color = year, shape = year, fill = year), size = 3) +
  geom_point(data = pdta4, aes(x = unlist.pdta4.x., y = unlist.pdta4.yhat., color = year, shape = year, fill = year), size = 3) +
  labs(
    x = "Concerned about environmental issues",
    y = "",
    color = "Year",
  ) +
  scale_color_manual(values = c("2020" = "#15607A", "2010" = "#15607A"), # both blue
                     labels = c("2020", "2010")
  ) +
  scale_linetype_manual(values = c("2020" = "solid", "2010" = "longdash"),  # Set line type for each year
                        labels = c("2020", "2010")) +
  scale_shape_manual(values = c("2020" = 21, "2010" = 23),  # Set shape for both years
                     labels = c("2020", "2010")) +
  scale_fill_manual(values = c("2020" = "#15607A", "2010" = "#15607A"),  # Set fill color for both years
                    labels = c("2020", "2010")) +
  scale_y_continuous(limits = c(0.43, 0.65), breaks = seq(0.43, 0.65, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 


ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400, 
       filename = "concern.png", path = "output\\variable_dependence_time")













#3
partial.obj5 <- partial.rfsrc(y2020_forest,
                              partial.xvar = "worry_jobs",
                              pred.var = "worry_jobs",
                              partial.type = NULL,
                              partial.values = y2020_forest$xvar$worry_jobs)

partial.obj6 <- partial.rfsrc(y2010_forest,
                              partial.xvar = "worry_jobs",
                              pred.var = "worry_jobs",
                              partial.type = NULL,
                              partial.values = y2010_forest$xvar$worry_jobs)

## extract partial effects for each species outcome
pdta5 <- get.partial.plot.data(partial.obj5, target = "1")
pdta6 <- get.partial.plot.data(partial.obj6, target = "1")

# Unlist the data
pdta5 = data.frame(unlist(pdta5$x),unlist(pdta5$yhat))
pdta6 = data.frame(unlist(pdta6$x),unlist(pdta6$yhat))

pdta5 <- pdta5 %>% 
  mutate(year = "2020")
pdta6 <- pdta6 %>% 
  mutate(year = "2010")

# plot the results
gg <- ggplot() +
  geom_line(data = pdta5, aes(x = unlist.pdta5.x., y = unlist.pdta5.yhat., color = year, linetype = year), size = 1) +
  geom_line(data = pdta6, aes(x = unlist.pdta6.x., y = unlist.pdta6.yhat., color = year, linetype = year), size = 1) +
  geom_point(data = pdta5, aes(x = unlist.pdta5.x., y = unlist.pdta5.yhat., color = year, shape = year, fill = year), size = 3) +
  geom_point(data = pdta6, aes(x = unlist.pdta6.x., y = unlist.pdta6.yhat., color = year, shape = year, fill = year), size = 3) +
  labs(
    x = "Jobs/prices more important than environment",
    y = "Adjusted Probability of Tax Opposition",
    color = "Year",
  ) +
  scale_color_manual(values = c("2020" = "#A63716", "2010" = "#A63716"), # both red
                     labels = c("2020", "2010")
  ) +
  scale_linetype_manual(values = c("2020" = "solid", "2010" = "longdash"),  # Set line type for each year
                        labels = c("2020", "2010")) +
  scale_shape_manual(values = c("2020" = 21, "2010" = 23),  # Set shape for both years
                     labels = c("2020", "2010")) +
  scale_fill_manual(values = c("2020" = "#A63716", "2010" = "#A63716"),  # Set fill color for both years
                    labels = c("2020", "2010")) +
  scale_y_continuous(limits = c(0.43, 0.65), breaks = seq(0.43, 0.65, by = 0.02)) +  # Set x-axis limits
  theme_minimal() + 
  theme(legend.position = "topright") 


ggsave(plot = gg, width = 5, height = 5, device = png, dpi = 400, 
       filename = "worry_jobs.png", path = "output\\variable_dependence_time")



































