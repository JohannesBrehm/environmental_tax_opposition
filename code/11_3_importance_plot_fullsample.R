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





#### Set directory #### 
setwd("insert your path here")


#### Data preparation #### 
data <- read_dta("data\\ISSP2020\\ISSPbothwaves.dta")

data = data[,c("INC_decile", "nowtp_taxes", "concern", "cc_env_problem", "priorization_env", "knowledge",
               "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
               "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
               "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
               "education", "gap_perception", "rural", "children", "brown_job",
               "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")]




#set dependent variable as factor variable
data$nowtp_taxes <- as.factor(data$nowtp_taxes)




#run random forest


set.seed(2022)
forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(forest)


# variable importance
forest_vimp <- vimp(forest)
importance_scores <- forest_vimp$importance



#subsample 
sub_forest <- subsample(forest, verbose = FALSE)


# take a delete-d-jackknife procedure for example
set.seed(2023)
vimpCI <- extract.subsample(sub_forest)$var.jk.sel.Z



write_xlsx(vimpCI, "data\\fullsample_vimpCI.xlsx")




# prepare data set
new <- read_excel("data\\fullsample_vimpCI.xlsx")


term <- c("INC_decile", "concern", "cc_env_problem", "priorization_env", "knowledge",
          "scepticism", "seriousness", "everyday_life", "risk_perception", "trust_people", "trust_institutions",
          "reciprocity", "env_committment", "modern_life_harms", "science_solution", "growth_harms", "worry_jobs",
          "env_consumption", "right_party", "vote_le", "religiousness", "petition", "donation", "female", "age", 
          "education", "gap_perception", "rural", "children", "brown_job",
          "weighted_carbon_price","good_governance","env_tax","inc_tax","revenue_recycling")


category <- c("Demographics","Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations",
              "Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Demographics","Demographics",
              "Demographics","Demographics","Demographics","Demographics","Demographics",
              "Tax system","Tax system","Tax system","Tax system","Tax system")

data <- tibble(term, new, category) 




# scale to 1-100
max_value <- max(c(new$mean))


data <- data %>%
  mutate(
    lower =  lower/ max_value,
    mean =  mean/ max_value,
    upper = upper/ max_value
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

data$term <- factor(data$term, levels = desired_order)




#add direction 
direction_data <- read_excel("data\\effect_direction_fullsample.xlsx")

data_plot = left_join(data, direction_data, by = "term", relationship = "one-to-one")






gg <- ggplot(data_plot, aes(x = reorder(term,mean), y = mean, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "longdash") +
  geom_pointrange(aes(fill = factor(direction), color = factor(direction)), 
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
  ) 
                    
    
  





ggsave(plot = gg, width = 11, height = 11, device = png, dpi = 600, 
       filename = "importance_plot_fullsample.png", path = "output\\")

# Print the plot
print(gg)



