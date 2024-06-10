

#### Packages #### 
library(randomForestSRC)
library(haven)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(data.tree)
library(writexl)
library(doParallel)


#### Clear the space ####
rm(list = ls()); gc()


#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################



#### Data preparation #### 
data <- read_dta("data\\ISSP2020\\ISSP2020_clean_wcountry.dta")


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





#run random forest for high 
set.seed(2023)
forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(forest)


# Export variable importance plots for all variables in dataset to determine direction
variables_of_interest <- c("age","belief_cc","brown_job","children","concern","direct_experience","donation",
                           "education","env_committment","env_consumption","cc_env_problem","female",
                           "gap_perception","growth_harms","knowledge","large_house",
                           "modern_life_harms","petition","INC_decile","risk_perception",
                           "rural","scepticism","science_solution","seriousness","time_car","trust_institutions",
                           "trust_people","enjoy_nature","vote_le",
                           "worry_jobs","plane","priorization_env","reciprocity",
                           "religiousness","right_party","everyday_life",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")





# Define a mapping of original variable names to desired names
variable_name_mapping <- list(
  "age" = "Age", 
  "belief_cc" = "Belief in human made climate change", 
  "children" = "Household with children", 
  "concern" = "Concerned about environmental issues",
  "direct_experience" ="Direct experience of environmental issues", 
  "education" = "Education level",
  "env_committment"="Committment to pro-environmental behavior",
  "env_consumption"="Environmental consumption",
  "female"="Female", 
  "gap_perception"="Gap decile self-placement vs. decile household income",
  "growth_harms"="Economic growth harms environment",
  "INC_decile"="Country specific household income decile",
  "knowledge"="Knowledge",
  "large_house"="Large house", 
  "plane"="Used plane in last 12 months",
  "priorization_env"="The environment among most important issues in country",
  "reciprocity"="Reciprocity",
  "religiousness"="Religiousness", 
  "right_party"="Right party preference",
  "risk_perception"="Risk perception environmental issues",
  "rural"="Living in rural area",
  "scepticism"="Scepticism",
  "seriousness"="Seriousness climate change",
  "time_car"="Spends time in motorized vehicle",
  "trust_institutions"="Trust in institutions",
  "trust_people"="Trust in people",
  "enjoy_nature"="Enjoys spending time in nature",
  "vote_le"="Voted in last election",
  "worry_jobs"="Jobs/prices more important than environment",
  "brown_job" = "Brown job", 
  "cc_env_problem"="Climate change most important environmental problem", 
  "donation"="Environmental donation",
  "modern_life_harms"="Modern life harms the environment",
  "petition"="Signed environmental petition",
  "science_solution"="Science will solve environmental problems",
  "everyday_life"="Everyday life affected",
  "weighted_carbon_price"="Effective carbon price",
  "revenue_recycling"="Recycling of carbon tax revenues",
  "good_governance"="Good governance",
  "env_tax"="Level of environmental taxes",
  "inc_tax"="Level of income taxes")


# Create a function to generate variable importance plots with custom settings
generate_variable_dependence_plot <- function(variable_name) {
  jpeg(
    filename = paste0("output//variable_dependence2020//high//", variable_name, ".jpeg"),
    width = 5, height = 5, units = "in", res = 300
  )
  
  # Get the desired x-axis label from the mapping or use the original variable name
  xlab_label <- ifelse(variable_name %in% names(variable_name_mapping), variable_name_mapping[[variable_name]], variable_name)
  
  # Customize boxplot settings for selected variables
  if (variable_name %in% names(variable_name_mapping)) {
    # For selected variables, create a boxplot without fill (white background)
    plot.variable(
      forest,
      xvar.names = variable_name,
      partial = TRUE,
      target = "1",
      xlab = xlab_label,
      ylab = "Probability environmental tax resistance",
      
      # Set boxplot settings
      boxfill = "white"  # White fill color for boxplot (no fill)
    )
  } else {
    # For other variables, create default boxplot
    plot.variable(
      forest,
      xvar.names = variable_name,
      partial = TRUE,
      target = "1",
      xlab = xlab_label,
      ylab = "Probability environmental tax resistance"
    )
  }
  
  dev.off()
}

# List of selected variables
variables_of_interest <- names(variable_name_mapping)  # Use the modified variable names

# Parallelize the generation of variable importance plots
foreach(var = variables_of_interest, .packages = c("randomForestSRC")) %dopar% {
  generate_variable_dependence_plot(var)
}











#run random forest for non- high 
set.seed(2023)
forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_non_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(forest)


# Create a function to generate variable importance plots with custom settings
generate_variable_dependence_plot <- function(variable_name) {
  jpeg(
    filename = paste0("output//variable_dependence2020//non_high//", variable_name, ".jpeg"),
    width = 5, height = 5, units = "in", res = 300
  )
  
  # Get the desired x-axis label from the mapping or use the original variable name
  xlab_label <- ifelse(variable_name %in% names(variable_name_mapping), variable_name_mapping[[variable_name]], variable_name)
  
  # Customize boxplot settings for selected variables
  if (variable_name %in% names(variable_name_mapping)) {
    # For selected variables, create a boxplot without fill (white background)
    plot.variable(
      forest,
      xvar.names = variable_name,
      partial = TRUE,
      target = "1",
      xlab = xlab_label,
      ylab = "Probability environmental tax resistance",
      
      # Set boxplot settings
      boxfill = "white"  # White fill color for boxplot (no fill)
    )
  } else {
    # For other variables, create default boxplot
    plot.variable(
      forest,
      xvar.names = variable_name,
      partial = TRUE,
      target = "1",
      xlab = xlab_label,
      ylab = "Probability environmental tax resistance"
    )
  }
  
  dev.off()
}

# List of selected variables
variables_of_interest <- names(variable_name_mapping)  # Use the modified variable names

# Parallelize the generation of variable importance plots
foreach(var = variables_of_interest, .packages = c("randomForestSRC")) %dopar% {
  generate_variable_dependence_plot(var)
}





















