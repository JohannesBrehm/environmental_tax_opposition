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


#Read data with GB
data <- read_dta("data\\ISSP2020\\ZA7650_v2-0-0.dta")

# Replace -4 with 4 in recycling variable
#data[data == -4] <- 4
a = -4
a %in% data$v52
data$v52 <- replace(data$v52, data$v52 == -4, 4) 
a = -4
a %in% data$v52 

# Replace -4 with 2 in voted last election
data$VOTE_LE <- replace(data$VOTE_LE, data$VOTE_LE == -4, 2) 


# Replace -4 with 1 in adult household members
data$HHADULT <- replace(data$HHADULT, data$HHADULT == -4, 1) 

# Replace -4 with 0 in children household members
data$HHCHILDR <- replace(data$HHCHILDR, data$HHCHILDR == -4, 0) 

# Replace -4 with 0 in toddlers household members
data$HHTODD <- replace(data$HHTODD, data$HHTODD == -4, 0) 

# Replace -4 and 96 with . in left or right leaning party
data <- data %>% 
  mutate(PARTY_LR = na_if(PARTY_LR, 96))
data <- data %>% 
  mutate(PARTY_LR = na_if(PARTY_LR, -4))



# Replace -8 values in specified columns with the given values
cols_to_replace <- c("v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v15", 
                     "v20", "v21", "v22", "v23", "v24", "v25",
                     "v29", "v30", "v31", "v32", "v33", "v34", "v35",
                     "v36", "v37", "v38", "v39", "v40")
                     
data <- data %>%
  mutate(across(all_of(cols_to_replace), ~ifelse(. == -8, 3, .)))
  

cols_to_replace <- c("v11", "v12", "v13", "v14","v18", "v19")
data <- data %>%
  mutate(across(all_of(cols_to_replace), ~ifelse(. == -8, 5, .)))

# Replace specific values with NA
data <- data%>%
  mutate_all(~ ifelse(. %in% c(-7, -8, -9, -1), NA, .))



####################################################################################################################################################################################
# Running MICE within different countries
####################################################################################################################################################################################


############################################################################################################################################################################################################################################################
# Austria (AT)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "AT"
data_AT <- data %>%
  filter(c_alphan == "AT")

# List of relevant variable names for AT
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "AT_RINC", "AT_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")



# Create the subset of columns to impute
predictor_data_AT <- data_AT[, relevant_vars]

# Set the method for imputing each column (for AT)
method_list_AT <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for AT
imputed_mice_AT <- mice(predictor_data_AT, method = method_list_AT, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for AT
imputed_data_AT <- complete(imputed_mice_AT)


############################################################################################################################################################################################################################################################
#Austrailia (AU)
###############################################################################################################################################################################################################################################################################################################################################

# Filter out rows where "c_alphan" is not "AU"
data_AU <- data %>%
  filter(c_alphan == "AU")

# List of relevant variable names for AU
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "AU_RINC", "AU_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_AU <- data_AU[, relevant_vars]

# Set the method for imputing each column (for AU)
method_list_AU <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for AU
imputed_mice_AU <- mice(predictor_data_AU, method = method_list_AU, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for AU
imputed_data_AU <- complete(imputed_mice_AU)

############################################################################################################################################################################################################################################################
#Switzerland (CH)
########################################################################################################################################################################################################################################################################################

# Filter out rows where "c_alphan" is not "CH"
data_CH <- data %>%
  filter(c_alphan == "CH")

# List of relevant variable names for CH
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "CH_RINC", "CH_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_CH <- data_CH[, relevant_vars]

# Set the method for imputing each column (for CH)
method_list_CH <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for CH
imputed_mice_CH <- mice(predictor_data_CH, method = method_list_CH, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for CH
imputed_data_CH <- complete(imputed_mice_CH)


############################################################################################################################################################################################################################################################
#China (CN)
########################################################################################################################################################################################################################################################################################

# Filter out rows where "c_alphan" is not "CN"
data_CN <- data %>%
  filter(c_alphan == "CN")

# List of relevant variable names for CN
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10","CN_v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "CN_RINC", "CN_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_CN <- data_CN[, relevant_vars]

# Set the method for imputing each column (for CN)
method_list_CN <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for CN
imputed_mice_CN <- mice(predictor_data_CN, method = method_list_CN, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for CN
imputed_data_CN <- complete(imputed_mice_CN)

############################################################################################################################################################################################################################################################
#Germany (DE)
########################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "DE"
data_DE <- data %>%
  filter(c_alphan == "DE")

# List of relevant variable names for DE
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "DE_RINC", "DE_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_DE <- data_DE[, relevant_vars]

# Set the method for imputing each column (for DE)
method_list_DE <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for DE
imputed_mice_DE <- mice(predictor_data_DE, method = method_list_DE, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for DE
imputed_data_DE <- complete(imputed_mice_DE)

############################################################################################################################################################################################################################################################
#Denmark (DK)
########################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "DK"
data_DK <- data %>%
  filter(c_alphan == "DK")

# List of relevant variable names for DK
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "DK_RINC", "DK_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_DK <- data_DK[, relevant_vars]

# Set the method for imputing each column (for DK)
method_list_DK <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for DK
imputed_mice_DK <- mice(predictor_data_DK, method = method_list_DK, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for DK
imputed_data_DK <- complete(imputed_mice_DK)

############################################################################################################################################################################################################################################################
#SPAIN (ES)
########################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "ES"
data_ES <- data %>%
  filter(c_alphan == "ES")

# List of relevant variable names for ES
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "ES_RINC", "ES_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_ES <- data_ES[, relevant_vars]

# Set the method for imputing each column (for ES)
method_list_ES <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for ES
imputed_mice_ES <- mice(predictor_data_ES, method = method_list_ES, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for ES
imputed_data_ES <- complete(imputed_mice_ES)


############################################################################################################################################################################################################################################################
#Finland (FI)
###############################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "FI"
data_FI <- data %>%
  filter(c_alphan == "FI")

# List of relevant variable names for FI
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "FI_RINC", "FI_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_FI <- data_FI[, relevant_vars]

# Set the method for imputing each column (for FI)
method_list_FI <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for FI
imputed_mice_FI <- mice(predictor_data_FI, method = method_list_FI, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for FI
imputed_data_FI <- complete(imputed_mice_FI)


############################################################################################################################################################################################################################################################
#France (FR)
###############################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "FR"
data_FR <- data %>%
  filter(c_alphan == "FR")

# List of relevant variable names for FR
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "FR_RINC", "FR_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_FR <- data_FR[, relevant_vars]

# Set the method for imputing each column (for FR)
method_list_FR <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for FR
imputed_mice_FR <- mice(predictor_data_FR, method = method_list_FR, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for FR
imputed_data_FR <- complete(imputed_mice_FR)

############################################################################################################################################################################################################################################################
#Croatia (HR)
###############################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "HR"
data_HR <- data %>%
  filter(c_alphan == "HR")

# List of relevant variable names for HR
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "HR_RINC", "HR_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_HR <- data_HR[, relevant_vars]

# Set the method for imputing each column (for HR)
method_list_HR <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for HR
imputed_mice_HR <- mice(predictor_data_HR, method = method_list_HR, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for HR
imputed_data_HR <- complete(imputed_mice_HR)



############################################################################################################################################################################################################################################################
#Hungary (HR)
###############################################################################################################################################################################################################################################

# Filter out rows where "c_alphan" is not "HU"
data_HU <- data %>%
  filter(c_alphan == "HU")

# List of relevant variable names for HU
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "HU_RINC", "HU_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_HU <- data_HU[, relevant_vars]

# Set the method for imputing each column (for HU)
method_list_HU <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for HU
imputed_mice_HU <- mice(predictor_data_HU, method = method_list_HU, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for HU
imputed_data_HU <- complete(imputed_mice_HU)


############################################################################################################################################################################################################################################################
#India (IN)
###############################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "IN"
data_IN <- data %>%
  filter(c_alphan == "IN")

# List of relevant variable names for IN
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "IN_RINC", "IN_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_IN <- data_IN[, relevant_vars]

# Set the method for imputing each column (for IN)
method_list_IN <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for IN
imputed_mice_IN <- mice(predictor_data_IN, method = method_list_IN, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for IN
imputed_data_IN <- complete(imputed_mice_IN)

############################################################################################################################################################################################################################################################
#Iceland (IS)
###############################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "IS"
data_IS <- data %>%
  filter(c_alphan == "IS")

# List of relevant variable names for IS
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "IS_RINC", "IS_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_IS <- data_IS[, relevant_vars]

# Set the method for imputing each column (for IS)
method_list_IS <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for IS
imputed_mice_IS <- mice(predictor_data_IS, method = method_list_IS, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for IS
imputed_data_IS <- complete(imputed_mice_IS)


############################################################################################################################################################################################################################################################
#Italy (IT)
###############################################################################################################################################################################################################################################

# Filter out rows where "c_alphan" is not "IT"
data_IT <- data %>%
  filter(c_alphan == "IT")

# List of relevant variable names for IT
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "IT_RINC", "IT_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_IT <- data_IT[, relevant_vars]

# Set the method for imputing each column (for IT)
method_list_IT <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for IT
imputed_mice_IT <- mice(predictor_data_IT, method = method_list_IT, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for IT
imputed_data_IT <- complete(imputed_mice_IT)


############################################################################################################################################################################################################################################################
# JAPAN (JP)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "JP"
data_JP <- data %>%
  filter(c_alphan == "JP")

# List of relevant variable names for JP
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "JP_RINC", "JP_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_JP <- data_JP[, relevant_vars]

# Set the method for imputing each column (for JP)
method_list_JP <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for JP
imputed_mice_JP <- mice(predictor_data_JP, method = method_list_JP, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for JP
imputed_data_JP <- complete(imputed_mice_JP)


############################################################################################################################################################################################################################################################
# South Korea (KR)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "KR"
data_KR <- data %>%
  filter(c_alphan == "KR")

# List of relevant variable names for KR
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "KR_RINC", "KR_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_KR <- data_KR[, relevant_vars]

# Set the method for imputing each column (for KR)
method_list_KR <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for KR
imputed_mice_KR <- mice(predictor_data_KR, method = method_list_KR, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for KR
imputed_data_KR <- complete(imputed_mice_KR)

############################################################################################################################################################################################################################################################
# Lithuania (LT)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "LT"
data_LT <- data %>%
  filter(c_alphan == "LT")

# List of relevant variable names for LT
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "LT_RINC", "LT_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_LT <- data_LT[, relevant_vars]

# Set the method for imputing each column (for LT)
method_list_LT <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for LT
imputed_mice_LT <- mice(predictor_data_LT, method = method_list_LT, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for LT
imputed_data_LT <- complete(imputed_mice_LT)

############################################################################################################################################################################################################################################################
#Norway (NO)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "NO"
data_NO <- data %>%
  filter(c_alphan == "NO")

# List of relevant variable names for NO
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "NO_RINC", "NO_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_NO <- data_NO[, relevant_vars]

# Set the method for imputing each column (for NO)
method_list_NO <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for NO
imputed_mice_NO <- mice(predictor_data_NO, method = method_list_NO, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for NO
imputed_data_NO <- complete(imputed_mice_NO)


############################################################################################################################################################################################################################################################
#New Zealand (NZ)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "NZ"
data_NZ <- data %>%
  filter(c_alphan == "NZ")

# List of relevant variable names for NZ
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "NZ_RINC", "NZ_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_NZ <- data_NZ[, relevant_vars]

# Set the method for imputing each column (for NZ)
method_list_NZ <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for NZ
imputed_mice_NZ <- mice(predictor_data_NZ, method = method_list_NZ, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for NZ
imputed_data_NZ <- complete(imputed_mice_NZ)

############################################################################################################################################################################################################################################################
#Philipines (PH)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "PH"
data_PH <- data %>%
  filter(c_alphan == "PH")

# List of relevant variable names for PH
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "PH_RINC", "PH_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_PH <- data_PH[, relevant_vars]

# Set the method for imputing each column (for PH)
method_list_PH <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for PH
imputed_mice_PH <- mice(predictor_data_PH, method = method_list_PH, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for PH
imputed_data_PH <- complete(imputed_mice_PH)


############################################################################################################################################################################################################################################################
#Russia (RU)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "RU"
data_RU <- data %>%
  filter(c_alphan == "RU")

# List of relevant variable names for RU
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "RU_RINC", "RU_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_RU <- data_RU[, relevant_vars]

# Set the method for imputing each column (for RU)
method_list_RU <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for RU
imputed_mice_RU <- mice(predictor_data_RU, method = method_list_RU, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for RU
imputed_data_RU <- complete(imputed_mice_RU)


############################################################################################################################################################################################################################################################
#Sweden (SE)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "SE"
data_SE <- data %>%
  filter(c_alphan == "SE")

# List of relevant variable names for SE
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "SE_RINC", "SE_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_SE <- data_SE[, relevant_vars]

# Set the method for imputing each column (for SE)
method_list_SE <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for SE
imputed_mice_SE <- mice(predictor_data_SE, method = method_list_SE, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for SE
imputed_data_SE <- complete(imputed_mice_SE)


############################################################################################################################################################################################################################################################
#Slovenia (SI)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "SI"
data_SI <- data %>%
  filter(c_alphan == "SI")

# List of relevant variable names for SI
relevant_vars <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                   "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                   "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                   "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                   "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                   "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "SI_RINC", "SI_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_SI <- data_SI[, relevant_vars]

# Set the method for imputing each column (for SI)
method_list_SI <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for SI
imputed_mice_SI <- mice(predictor_data_SI, method = method_list_SI, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for SI
imputed_data_SI <- complete(imputed_mice_SI)

############################################################################################################################################################################################################################################################
#Slovakia (SK)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "SK"
data_SK <- data %>%
  filter(c_alphan == "SK")

# List of relevant variable names for SK
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "SK_RINC", "SK_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_SK <- data_SK[, relevant_vars]

# Set the method for imputing each column (for SK)
method_list_SK <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for SK
imputed_mice_SK <- mice(predictor_data_SK, method = method_list_SK, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for SK
imputed_data_SK <- complete(imputed_mice_SK)

############################################################################################################################################################################################################################################################
#Thailand (TH)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "TH"
data_TH <- data %>%
  filter(c_alphan == "TH")

# List of relevant variable names for TH
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "TH_RINC", "TH_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_TH <- data_TH[, relevant_vars]

# Set the method for imputing each column (for TH)
method_list_TH <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for TH
imputed_mice_TH <- mice(predictor_data_TH, method = method_list_TH, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for TH
imputed_data_TH <- complete(imputed_mice_TH)

############################################################################################################################################################################################################################################################
#Taiwan (TW)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "TW"
data_TW <- data %>%
  filter(c_alphan == "TW")

# List of relevant variable names for TW
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "TW_RINC", "TW_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_TW <- data_TW[, relevant_vars]

# Set the method for imputing each column (for TW)
method_list_TW <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for TW
imputed_mice_TW <- mice(predictor_data_TW, method = method_list_TW, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for TW
imputed_data_TW <- complete(imputed_mice_TW)


############################################################################################################################################################################################################################################################
#United States of America (US)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "US"
data_US <- data %>%
  filter(c_alphan == "US")

# List of relevant variable names for US
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "US_RINC", "US_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_US <- data_US[, relevant_vars]

# Set the method for imputing each column (for US)
method_list_US <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for US
imputed_mice_US <- mice(predictor_data_US, method = method_list_US, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for US
imputed_data_US <- complete(imputed_mice_US)


############################################################################################################################################################################################################################################################
#South Africa (ZA)
###############################################################################################################################################################################################################################################################################################################################################
# Filter out rows where "c_alphan" is not "ZA"
data_ZA <- data %>%
  filter(c_alphan == "ZA")

# List of relevant variable names for ZA
relevant_vars <- c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11",
                    "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22",
                    "v23", "v24", "v25", "v29", "v30", "v31", "v32",
                    "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40", "v41", "v42",
                    "v43", "v44", "v45", "v46", "v47", "v49", "v50", "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58",
                    "v59", "v60", "PARTY_LR", "SEX", "EDULEVEL", "AGE", "HHTODD", "HHCHILDR", "TOPBOT", "ISCO08", "WORK", "ZA_RINC", "ZA_INC", "CASEID", "v48", "ATTEND", "VOTE_LE", "RELIGGRP", "HHADULT", "URBRURAL")

# Create the subset of columns to impute
predictor_data_ZA <- data_ZA[, relevant_vars]

# Set the method for imputing each column (for ZA)
method_list_ZA <- rep("cart", length(relevant_vars))  # Use "cart" as the imputation method

# Create the mice imputation object for ZA
imputed_mice_ZA <- mice(predictor_data_ZA, method = method_list_ZA, ridge = 0.0001, threshold = 1.05, seed = 123)

# Perform the imputation using the mice package for ZA
imputed_data_ZA <- complete(imputed_mice_ZA)

############################################################################################################################################################################################################################################################
# Merge all into 1 data set
###############################################################################################################################################################################################################################################################################################################################################


# List of country codes
countries <- c("AT", "AU", "CH", "CN", "DE", "DK", "ES", "FI", "FR", "HR", "HU", "IN", "IS", "IT", "JP", "KR", "LT", "NO", "NZ", "PH", "RU", "SE",
               "SI", "SK", "TH", "TW", "US", "ZA")

# Create a list to hold the imputed dataframes
imputed_list <- list()

# Populate the list with your imputed dataframes (replace with actual variable names)
for (country in countries) {
  imputed_list[[country]] <- get(paste0("imputed_data_", country))
}

# Combine the country-specific dataframes into a single dataframe
combined_data <- bind_rows(imputed_list)

### save data
write_dta(combined_data, file.path("data\\ISSP2020\\imputed_variables.dta"))
saveRDS(combined_data, file = "data\\ISSP2020\\imputed_variables.rds")





