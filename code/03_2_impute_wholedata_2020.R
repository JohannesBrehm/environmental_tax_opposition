############################################################################################################################################################################################################################################################
# Now impute on the whole sample 
################################################################################################################################################################
# Load required libraries
library(mice)
library(haven)
library(dplyr)


#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################

data <- read_dta("data\\ISSP2020\\imputed_variables_wIncome.dta")

#### Data preparation #### 
ls(data) 

# Put the income deciles variables as factors
data$RINC_deciles <- haven::as_factor(data$RINC_deciles)
data$INC_deciles <- haven::as_factor(data$INC_deciles)

# The variables "HHADULT", "URBRURAL", "HHTODD", "HHCHILDR", "PARTY_LR", "v50" where not asked in one or 2 countries each. To avoid losing these observations completely during the random forest, we will impute the on the whole sample.


# List of relevant variable names for ZA
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "RINC_deciles", "INC_deciles", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data <- data[, relevant_vars]

# Set the method for imputing each column (for ZA)
method_list <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for ZA
imputed_mice <- mice(predictor_data, method = method_list, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for ZA
imputed_data <- complete(imputed_mice)

### save data
write_dta(imputed_data, file.path("data\\ISSP2020\\imputed_variables_wIncome_full.dta"))
saveRDS(imputed_data, file = "data\\ISSP2020\\imputed_variables_wIncome_full.rds")



