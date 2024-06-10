#### Clear the space ####
rm(list = ls()); gc()

# Load packages
library(tidyverse)
library(haven)
library(stats)
library(dplyr)
library(ggplot2)
library(extrafont)
library(sciplot)
library(corrplot)


#### Set directory #### 
setwd("insert your path here")




# Load data

data <- read_dta("data\\ISSP2020\\ISSPbothwaves.dta")

# Standardize variables
vars <- c("env_committment", "worry_jobs", "trust_institutions", "donation", "env_consumption", "reciprocity", "trust_people", "petition", "right_party", "growth_harms", "modern_life_harms", "vote_le", "science_solution", "religiousness", "education", "gap_perception", "brown_job", "age", "rural", "INC_decile", "children", "female", "concern", "scepticism", "priorization_env", "everyday_life", "seriousness", "risk_perception", "cc_env_problem", "knowledge", "inc_tax", "good_governance", "env_tax", "weighted_carbon_price", "revenue_recycling")

# Remove variables
data <- data[, names(data) %in% vars]

data <- na.omit(data)


#Correlation Matrix

Cor_M <- cor(data)

# Change var names in Matrix for better display
mapping <- c("age" = "Age", 
             "belief_cc" = "Belief", 
             "children" = "Children", 
             "concern" = "Concern",
             "direct_experience" ="Experience", 
             "education" = "Education",
             "env_committment"="Committed to behavior",
             "env_consumption"="sustainable consumption",
             "female"="Female", 
             "gap_perception"="Perceived income gap",
             "growth_harms"="Growth harms environment",
             "INC_decile"="Country income decile",
             "knowledge"="Knowledge",
             "large_house"="Large house", 
             "plane"="Plane (12M)",
             "priorization_env"="Environment importance",
             "reciprocity"="Reciprocity",
             "religiousness"="Religious", 
             "right_party"="Pro Right party",
             "risk_perception"="Environmental Risk",
             "rural"="Rural area",
             "scepticism"="Scepticism",
             "seriousness"="Serious Issue",
             "time_car"="Time in Car",
             "trust_institutions"="Trusts institutions",
             "trust_people"="Trusts people",
             "enjoy_nature"="Enjoys nature",
             "vote_le"="Voter",
             "worry_jobs"="Prioritizes jobs/prices ",
             "brown_job" = "Brown job", 
             "cc_env_problem"="Importance of climate change", 
             "donation"="Donates",
             "modern_life_harms"="Modern lifestyle harms the environment",
             "petition"="Signed petition",
             "science_solution"="Science solves problems",
             "everyday_life"="Everyday life affected",
             "weighted_carbon_price"="Effective carbon price",
             "revenue_recycling"="Recycling of carbon tax revenues",
             "good_governance"="Good governance",
             "env_tax"="Level of environmental taxes",
             "inc_tax"="Level of income taxes")

rownames(Cor_M) <- mapping[rownames(Cor_M)]
colnames(Cor_M) <- mapping[colnames(Cor_M)]

png(filename = "output//cor_plot.png",  width = 1200, height = 1200, res = 300)

corrplot(Cor_M, method = "color", col = colorRampPalette(c("#15607A","white", "#A63716"))(25),
         tl.col = "black",  tl.cex = 0.35 , type = "lower", order = 'hclust')
dev.off()

