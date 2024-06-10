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





set.seed(2022)
high_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(high_forest)


set.seed(2022)
non_high_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_non_high_inc), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(non_high_forest)

# variable importance
high_forest_vimp <- vimp(high_forest)
high_importance_scores <- high_forest_vimp$importance

non_high_forest_vimp <- vimp(non_high_forest)
non_high_importance_scores <- non_high_forest_vimp$importance


#subsample 
high_sub_forest <- subsample(high_forest, verbose = FALSE)

non_high_sub_forest <- subsample(non_high_forest, verbose = FALSE)



# take a delete-d-jackknife procedure for example
set.seed(2023)
high_vimpCI <- extract.subsample(high_sub_forest)$var.jk.sel.Z

set.seed(2023)
non_high_vimpCI <- extract.subsample(non_high_sub_forest)$var.jk.sel.Z

write_xlsx(high_vimpCI, "data\\high_vimpCI.xlsx")
write_xlsx(non_high_vimpCI, "data\\non_high_vimpCI.xlsx")

#high_vimpCI <- read_excel("data\\high_vimpCI.xlsx")
#non_high_vimpCI <- read_excel("data\\non_high_vimpCI.xlsx")



# prepare data set
new <- rbind(high_vimpCI, non_high_vimpCI)



term <- c("age1","belief_cc1","brown_job1","children1","concern1","direct_experience1","donation1",
          "education1","env_committment1","env_consumption1","cc_env_problem1","female1",
          "gap_perception1","growth_harms1","knowledge1","large_house1",
          "modern_life_harms1","petition1","INC_decile1","risk_perception1",
          "rural1","scepticism1","science_solution1","seriousness1","time_car1","trust_institutions1",
          "trust_people1","enjoy_nature1","vote_le1",
          "worry_jobs1","plane1","priorization_env1","reciprocity1",
          "religiousness1","right_party1","everyday_life1",
          "weighted_carbon_price1","good_governance1","env_tax1","inc_tax1","revenue_recycling1",
          "age2","belief_cc2","brown_job2","children2","concern2","direct_experience2","donation2",
          "education2","env_committment2","env_consumption2","cc_env_problem2","female2",
          "gap_perception2","growth_harms2","knowledge2","large_house2",
          "modern_life_harms2","petition2","INC_decile2","risk_perception2",
          "rural2","scepticism2","science_solution2","seriousness2","time_car2","trust_institutions2",
          "trust_people2","enjoy_nature2","vote_le2",
          "worry_jobs2","plane2","priorization_env2","reciprocity2",
          "religiousness2","right_party2","everyday_life2",
          "weighted_carbon_price2","good_governance2","env_tax2","inc_tax2","revenue_recycling2")


category <- c("Demographics","Environmental evaluations","Demographics","Demographics","Environmental evaluations",
              "Environmental evaluations","Values and ideologies","Demographics","Values and ideologies","Values and ideologies",
              "Environmental evaluations","Demographics","Demographics","Values and ideologies","Environmental evaluations",
              "Values and ideologies","Values and ideologies","Values and ideologies","Demographics",
              "Environmental evaluations","Demographics","Environmental evaluations","Values and ideologies","Environmental evaluations","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Environmental evaluations","Values and ideologies","Values and ideologies","Values and ideologies",
              "Environmental evaluations",
              "Tax system","Tax system","Tax system","Tax system","Tax system",
              "Demographics","Environmental evaluations","Demographics","Demographics","Environmental evaluations",
              "Environmental evaluations","Values and ideologies","Demographics","Values and ideologies","Values and ideologies",
              "Environmental evaluations","Demographics","Demographics","Values and ideologies","Environmental evaluations",
              "Values and ideologies","Values and ideologies","Values and ideologies","Demographics",
              "Environmental evaluations","Demographics","Environmental evaluations","Values and ideologies","Environmental evaluations","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Environmental evaluations","Values and ideologies","Values and ideologies","Values and ideologies",
              "Environmental evaluations",
              "Tax system","Tax system","Tax system","Tax system","Tax system")

data <- tibble(term, new, category) 



# Remove the number at the end of the term
data <- data %>%
  mutate(region = rep(c("high income", "middle income"), each = 41))  %>%
  mutate(term_2 = str_replace(term, "\\d+$", ""))


# scale to 1-100
max_value_high <- max(c(high_vimpCI$mean))
max_value_non_high <- max(c(non_high_vimpCI$mean))

data <- data %>%
  mutate(
    lower = ifelse(region=="high income", lower/ max_value_high, lower/ max_value_non_high),
    mean = ifelse(region=="high income", mean/ max_value_high, mean/ max_value_non_high),
    upper = ifelse(region=="high income", upper/ max_value_high, upper/ max_value_non_high)
  )

data$lower <- data$lower * 100
data$mean <- data$mean * 100
data$upper <- data$upper * 100




# Reorder the levels of the category variable
desired_order <- c(
  "Values and ideologies",
  "Demographics",
  "Environmental evaluations",
  "Tax system"
)


data$category <- factor(data$category, levels = desired_order)

# Reorder the levels of the sub-variables
desired_order <- c(
  "weighted_carbon_price",
  "good_governance",
  "env_tax",
  "inc_tax",
  "revenue_recycling",
  "belief_cc",
  "cc_env_problem",
  "risk_perception",
  "seriousness",
  "direct_experience",
  "everyday_life",
  "knowledge",
  "scepticism",
  "priorization_env",
  "concern",
  "gap_perception",
  "INC_decile",
  "education",
  "children",
  "age",
  "female",
  "rural",
  "brown_job",
  "science_solution",
  "modern_life_harms",
  "growth_harms",
  "worry_jobs",
  "donation",
  "petition",
  "plane",
  "enjoy_nature",
  "time_car",
  "large_house",
  "env_consumption",
  "reciprocity",
  "religiousness",
  "env_committment",
  "vote_le",
  "right_party",
  "trust_people",
  "trust_institutions"
)

data$term_2 <- factor(data$term_2, levels = desired_order)




#add direction 
direction_data <- read_excel("data\\effect_direction_regions.xlsx")

data_plot = left_join(data, direction_data, by = "term", relationship = "one-to-one")


data_plot <- data_plot %>%
  mutate(linetype_label = ifelse(region == "high income", "High Income", "Middle Income"))









gg <- ggplot(data_plot, aes(x = reorder(term_2,mean), y = mean, ymin = lower, ymax = upper, fill = region)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "longdash") +
  geom_pointrange(aes(fill = factor(direction), color = factor(direction),
                      linetype = linetype_label, shape = linetype_label), 
                  position = position_dodge(width = 0),  # Adjust the width as needed
                  linewidth = 0.5, fatten = 5, stroke = 1) +
  labs(x = NULL, y = "Relative variable importance estimate") +
  coord_flip() +  
  facet_grid(category ~ ., switch = 'y', scales = 'free_y', space = "free_y") +
  theme_classic(base_size = 13) +
  theme(
    plot.margin = margin(t = 5, r = 5, b = 40, l = 5, unit = "pt"),
    axis.text.x = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
    axis.text.y = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
    strip.text = element_text(margin = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")),
    strip.placement = 'outside',
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = 'bold', angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing.y = unit(5, 'mm'),
    text = element_text(family = "Calibri", size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 10),  # Adjust legend text size
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = -220, unit = "pt"),
    legend.title = element_blank(),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm') #change legend key width
  )+
  scale_x_discrete(labels = c(
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
    "inc_tax"="Level of income taxes")) +
  scale_fill_manual(values = c("1"= "#15607A",
                               "2" = "#A63716",
                               "3" = "#C7CDD1"),
                    labels = c("higher value, less likely opposition", "higher value, more likely opposition", "ambiguous")
  ) +
  scale_color_manual(values = c("1"= "#15607A",
                                "2" = "#A63716",
                                "3" = "#C7CDD1"),
                     labels = c("higher value, less likely opposition", "higher value, more likely opposition", "ambiguous")
  ) +
 scale_shape_manual(values = c(21,23)) +
  guides(
    color = guide_legend(
      title = NULL,
      override.aes = list(linetype = 0, size = 1.2)  # Remove linetype from the legend
    ),
    fill = FALSE,
    shape = FALSE,
    linetype = guide_legend(
      title = "Line Type",
      override.aes = list(linetype = c("solid", "longdash"),
                          shape = c(21, 23),
                          labels = c("High Income", "Middle Income"))
    )
  )





ggsave(plot = gg, width = 11, height = 11, device = png, dpi = 600, 
       filename = "importance_plot_regions.png", path = "output\\")

# Print the plot
print(gg)





