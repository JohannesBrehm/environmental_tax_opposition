############################################################################################################################################################################################################################################################
# Now impute on the whole sample 
################################################################################################################################################################
# Load required libraries
library(mice)
library(haven)
library(dplyr)

#### Clear the space ####
rm(list = ls()); gc()


#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################



data <- read_dta("data\\ISSP2010\\imputed_variables_wIncome_2010.dta")

#### Data preparation #### 
ls(data) 


# Put the income deciles variables as factors
data$INC_decile <- haven::as_factor(data$INC_decile)

# The variables "HHTODD", "HHCHILDR", "PARTY_LR", "TOPBOT" where not asked in few countries. To avoid losing these observations completely during the random forest, we will impute the on the whole sample.


# List of relevant variable names 
relevant_vars <- c("HHCHILDR","HHTODD", "AGE", "ATTEND", "c_alphan", "CASEID", "country", "DEGREE", "INC_decile", "ISCO88", "PARTY_LR", "RELIGGRP", "SEX", "TOPBOT", "URBRURAL", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "v32", "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42", "v43", "v44", "v45", "v46", "v47", "v48", "v49", "v5", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58", "v59", "v6", "v60", "v61", "v62", "v63", "v64", "v65", "v66", "v7", "v8", "v9", "VOTE_LE", "WORK")

# Create the subset of columns to impute
predictor_data <- data[, relevant_vars]

# Set the method for imputing each column (for ZA)
method_list <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for ZA
imputed_mice <- mice(predictor_data, method = method_list, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for ZA
imputed_data <- complete(imputed_mice)

### save data
write_dta(imputed_data, file.path("data\\ISSP2010\\imputed_variables_wIncome_full_2010.dta"))
#saveRDS(imputed_data, file = "data\\ISSP2010\\imputed_variables_wIncome_full.rds")
