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




#### Set directory #### 
setwd("insert your path here")


#### Data preparation #### 

# 2020
y2020_data <- read_dta("data\\ISSP2020\\ISSP2020_clean_wcountry.dta")
y2020_data = y2020_data[,c("INC_decile", "nowtp_taxes", "concern", "cc_env_problem", "priorization_env", "knowledge",
                           "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
                           "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
                           "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
                           "education", "gap_perception", "rural", "children", "brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling","c_alphan")]

#set dependent variable as factor variable
y2020_data$nowtp_taxes <- as.factor(y2020_data$nowtp_taxes)

# 2010
y2010_data <- read_dta("data\\ISSP2010\\ISSP2010_clean_wcountry.dta")
y2010_data = y2010_data[,c("INC_decile", "nowtp_taxes", "concern", "cc_env_problem", "priorization_env", "knowledge",
                           "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
                           "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
                           "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
                           "education", "gap_perception", "rural", "children", "brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling","c_alphan")]

#set dependent variable as factor variable
y2010_data$nowtp_taxes <- as.factor(y2010_data$nowtp_taxes)


#subsamples relevant countries
# 2020
y2020_countries <- c("AT", "AU","CH", "DE", "DK", "ES", "FI", "FR", "HR", "IS", "JP", "KR", "LT", "NO", "NZ", "SE", "SI", "SK", "TW", "US")
data_y2020 <- subset(y2020_data, c_alphan %in% y2020_countries)
#without c_alphan
data_y2020 = data_y2020[,c("INC_decile", "nowtp_taxes", "concern", "cc_env_problem", "priorization_env", "knowledge",
                           "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
                           "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
                           "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
                           "education", "gap_perception", "rural", "children", "brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]

# 2010
y2010_countries <- c("AT", "AU","CH", "DE", "DK", "ES", "FI", "FR", "HR", "IS", "JP", "KR", "LT", "NO", "NZ", "SE", "SI", "SK", "TW", "US")
data_y2010 <- subset(y2010_data, c_alphan %in% y2010_countries)
#without c_alphan
data_y2010 = data_y2010[,c("INC_decile", "nowtp_taxes", "concern", "cc_env_problem", "priorization_env", "knowledge",
                           "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
                           "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
                           "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
                           "education", "gap_perception", "rural", "children", "brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]





#run random forest for 2010
set.seed(2023)
forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2010), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(forest)


# Export variable importance plots for all variables in dataset to determine direction
variables_of_interest <- c("INC_decile", "concern", "cc_env_problem", "priorization_env", "knowledge",
                           "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
                           "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
                           "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
                           "education", "gap_perception", "rural", "children", "brown_job",
                           "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")

# Define a mapping of original variable names to desired names
variable_name_mapping <- list(
  "age" = "Age", 
  "children" = "Household with children", 
  "concern" = "Concerned about environmental issues",
  "education" = "Education level",
  "env_committment"="Committment to pro-environmental behavior",
  "env_consumption"="Environmental consumption",
  "female"="Female", 
  "gap_perception"="Gap decile self-placement vs. decile household income",
  "growth_harms"="Economic growth harms environment",
  "INC_decile"="Country specific household income decile",
  "knowledge"="Knowledge",
  "priorization_env"="The environment among most important issues in country",
  "reciprocity"="Reciprocity",
  "religiousness"="Religiousness", 
  "right_party"="Right party preference",
  "risk_perception"="Risk perception environmental issues",
  "rural"="Living in rural area",
  "scepticism"="Scepticism",
  "seriousness"="Seriousness climate change",
  "trust_institutions"="Trust in institutions",
  "trust_people"="Trust in people",
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
    filename = paste0("output//variable_dependence2010//high_2010sample//", variable_name, ".jpeg"),
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










#run random forest for 2020 
set.seed(2023)
forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2020), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(forest)



# Create a function to generate variable importance plots with custom settings
generate_variable_dependence_plot <- function(variable_name) {
  jpeg(
    filename = paste0("output//variable_dependence2020//high_2010sample//", variable_name, ".jpeg"),
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



