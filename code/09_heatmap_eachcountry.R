#### Clear the space ####
rm(list = ls()); gc()

#### Packages #### 
library(randomForestSRC)
library(data.tree)
library(dplyr)
library(broom)
library(readr)
library(parallel)
library(haven)
library(extrafont)
library(readxl)
library(ggplot2)


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
               "worry_jobs","plane","priorization_env","reciprocity",
               "religiousness","right_party","everyday_life", "c_alphan")]

# Set dependent variable as a factor variable
data$nowtp_taxes <- as.factor(data$nowtp_taxes)

# List of countries to loop over
countries_to_analyze <- c("AU", "AT", "HR", "DK", "FI", "FR", "DE", "HU", "IS", "IT", "JP", "KR", "LT", "NZ", "NO", "SK", "SI", "ES", "SE", "CH", "TW", "US","IN", "PH", "CN", "RU", "ZA", "TH")

# Initialize an empty list to store results for each country
mean_list <- list()

# Loop over countries
for (country in countries_to_analyze) {
  
  # Subset data for the current country
  data_country <- subset(data, c_alphan == country, select = -c(c_alphan))
  
  # Run random forest
  set.seed(2023)
  forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_country), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
  
  # Variable importance
  forest_vimp <- vimp(forest)
  importance_scores <- forest_vimp$importance
  
  
  # Subsample 
  sub_forest <- subsample(forest, verbose = FALSE)
  
  # Take a delete-d-jackknife procedure for example
  set.seed(2023)
  vimpCI <- extract.subsample(sub_forest)$var.jk.sel.Z
  
  # Prepare data for plotting with reordered categories
  mean_country <- data.frame(
    variable = rownames(vimpCI),  
    lower = vimpCI$lower,
    mean = vimpCI$mean,
    upper = vimpCI$upper,
    pvalue = vimpCI$pvalue,
    signif = vimpCI$signif,  # Match categories with variable names
    stringsAsFactors = FALSE
  )
  
  # Scale to 1-100
  max_value <- max(c(vimpCI$mean))
  mean_country$lower <- mean_country$lower / max_value
  mean_country$mean <- mean_country$mean / max_value
  mean_country$upper <- mean_country$upper / max_value
  mean_country$lower <- mean_country$lower * 100
  mean_country$mean <- mean_country$mean * 100
  mean_country$upper <- mean_country$upper * 100
  
  # Store the results in the list
  mean_list[[country]] <- mean_country

}




#Combine the list of data frames into a single data frame
result_df <- dplyr::bind_rows(mean_list, .id = "country")

# Keeping relevant variables and removing env_tax and inc_tax
data_heat <- result_df[, c("variable","mean","country")]

# Categorize the "mean" variable into four equal-sized categories
data_heat <- data_heat %>%
  group_by(country) %>%
  mutate(mean_category = ntile(mean, 4))


# Sample metadata table
metadata <- data.frame(
  variable = factor(rownames(data_heat), levels = rownames(data_heat)),  # Set as a factor with custom levels
  category = c(
    "Demographics", "Environmental Evaluations ", "Demographics", "Demographics", "Environmental Evaluations ",
    "Environmental Evaluations ", "Values and Ideologies", "Demographics", "Values and Ideologies", "Values and Ideologies",
    "Environmental Evaluations ", "Demographics", "Demographics", "Values and Ideologies", "Environmental Evaluations ",
    "Values and Ideologies", "Values and Ideologies", "Values and Ideologies", "Demographics",
    "Environmental Evaluations ", "Demographics", "Environmental Evaluations ", "Values and Ideologies", "Environmental Evaluations ", "Values and Ideologies",
    "Values and Ideologies", "Values and Ideologies", "Values and Ideologies", "Values and Ideologies", "Values and Ideologies",
    "Values and Ideologies", "Environmental Evaluations ", "Values and Ideologies", "Values and Ideologies", "Values and Ideologies",
    "Environmental Evaluations "
  ),
  stringsAsFactors = FALSE
)



# Merge metadata with data_heat based on row names
data_heat <- merge(data_heat, metadata, by = "row.names", all.x = TRUE)


# Define the desired order of categories
desired_order <- c(
  "Values and Ideologies",
  "Demographics",
  "Environmental Evaluations "
)

# Create a factor with desired order for the "category" variable
data_heat$category <- factor(data_heat$category, levels = desired_order)#

# Define the desired order of countries according to four regions (North, West, East, South)
desired_country_order <- c("AU", "AT", "HR", "DK", "FI", "FR", "DE", "HU", "IS", "IT", "JP", "KR", "LT", "NZ", "NO", "SK", "SI", "ES", "SE", "CH", "TW", "US","IN", "PH", "CN", "RU", "ZA", "TH")

# Create a factor with desired order for the "country" variable
data_heat$country <- factor(data_heat$country, levels = desired_country_order)

# Define the regions for each group of countries
region_mapping <- list(
  "Northern Europe" = c("NO", "DK", "SE", "IS", "FI"),
  "Western Europe" = c("FR", "DE", "AT", "CH"),
  "Eastern Europe" = c("SK", "HU", "LT", "HR"),
  "Southern Europe" = c("ES", "IT", "SI"),
  "US & Oceania" = c("AU", "NZ", "US"),
  "RICS" = c("IN", "CN", "RU", "ZA"),
  "SEA" = c("PH", "TH"),
  "East Asia" = c("JP", "KR","TW")
)

# Create a new variable "region" based on the country groups
data_heat$region <- factor(
  sapply(data_heat$country, function(country) {
    for (region in names(region_mapping)) {
      if (country %in% region_mapping[[region]]) {
        return(region)
      }
    }
  }),
  levels = names(region_mapping)
)


# Create a transposed heatmap with a sequential color scale
gg <- ggplot(data_heat, aes(x = country, y = reorder(variable.x, mean), fill = as.numeric(mean_category), group = category)) +
  geom_tile(color = "black") +
  scale_fill_gradient(
    low = "grey96", 
    high = "#4040b8", 
    na.value = "white", 
    labels = c("  Quartile 1  ", "  Quartile 2  ", "  Quartile 3  ", "  Quartile 4  ")
  )+  
  labs(title = "",
       x = "Country",
       y = "") +
  facet_grid(category ~ region, scales = 'free', space = "free", switch = 'y')  +  
  labs(fill = "Relative Variable Importance in Quartiles") +
  theme_classic(base_size = 13) +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 40, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
    axis.text.y = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"), size=14),
    strip.text = element_text(margin = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt"), size = 12),
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = 'bold', angle = 90, vjust = 0.5, hjust = 0.5, size=14),
    panel.spacing.y = unit(2, 'mm'),
    text = element_text(family = "Arial", size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 13),  # Adjust legend text size
    legend.margin = margin(t = 0, r = 0, b = 0, l = -240, unit = "pt"),
    legend.title = element_text(size = 13),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(0.5, 'cm'), #change legend key height
    legend.key.width = unit(2, 'cm') #change legend key width
  )+ 
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"  # Add legend labels below
    )
  ) +
  scale_y_discrete(labels = c(
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
    "everyday_life"="Everyday life affected")) 




# Display the transposed heatmap
print(gg)


# Save the plot with all configurations
ggsave(filename = "output\\heat_map_taxes_all_countries.png", plot = gg, width = 18, height = 18, dpi = 300, type = "cairo")


##### oob error
oob_list <- list()

# Loop over countries 
for (country in countries_to_analyze) {
  
  # Subset data for the current country
  data_country <- subset(data, c_alphan == country, select = -c(c_alphan))
  
  # Run random forest
  set.seed(2023)
  forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_country), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
  
  
  # OOB error
  oob_country <- forest$err.rate
  oob_country <- na.omit(oob_country)
  
  # Store the results in the list
  oob_list[[country]] <- oob_country
}

oob_country <- do.call(rbind.data.frame, oob_list)
oob_country$mean_all <- mean(oob_country$all)  
oob_country$classification_rate <- (1 - oob_country$all) * 100

print(oob_country$classification_rate)


