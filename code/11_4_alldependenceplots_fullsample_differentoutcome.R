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



#### Set directory #### 
setwd("insert your path here")


#### Data preparation #### 
data <- read_dta("data\\ISSP2020\\ISSPbothwaves.dta")

data = data[,c("INC_decile", "nowtp_taxes_strong", "concern", "cc_env_problem", "priorization_env", "knowledge",
               "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
               "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
               "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
               "education", "gap_perception", "rural", "children", "brown_job",
               "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]




#set dependent variable as factor variable
data$nowtp_taxes_strong <- as.factor(data$nowtp_taxes_strong)




#run random forest for  fullsample
set.seed(2023)
forest <- rfsrc(nowtp_taxes_strong ~ ., data = as.data.frame(data), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
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
    filename = paste0("output//variable_dependence_fullsample_differentoutcome//", variable_name, ".jpeg"),
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

