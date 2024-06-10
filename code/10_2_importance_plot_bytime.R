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



#############################
#  Set your directory here  #
#############################


# Set directory to the Replication Package folder
setwd("insert your path here")

############################################################################################


#### Data preparation #### 

# 2020
y2020_data <- read_dta("data\\ISSP2020\\ISSP2020_clean_wcountry.dta")
y2020_data = y2020_data[,c("INC_decile","nowtp_taxes","concern","cc_env_problem","priorization_env","knowledge",
                           "scepticism","seriousness","everyday_life","risk_perception","trust_people","trust_institutions",
                           "reciprocity","env_committment","modern_life_harms","science_solution","growth_harms","worry_jobs",
                           "env_consumption","right_party","vote_le","religiousness","petition","donation","female","age", 
                           "education","gap_perception","rural","children","brown_job",
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




set.seed(2022)
y2020_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2020), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(y2020_forest)



set.seed(2022)
y2010_forest <- rfsrc(nowtp_taxes ~ ., data = as.data.frame(data_y2010), ntree = 1000, nodesize = 1, mtry=6, nsplit=1)
print(y2010_forest)

# variable importance
y2020_forest_vimp <- vimp(y2020_forest)
y2020_importance_scores <- y2020_forest_vimp$importance

y2010_forest_vimp <- vimp(y2010_forest)
y2010_importance_scores <- y2010_forest_vimp$importance





#subsample 
y2020_sub_forest <- subsample(y2020_forest, verbose = FALSE)

y2010_sub_forest <- subsample(y2010_forest, verbose = FALSE)



# take a delete-d-jackknife procedure for example
set.seed(2023)
y2020_vimpCI <- extract.subsample(y2020_sub_forest, alpha = .05)$var.jk.sel.Z

set.seed(2023)
y2010_vimpCI <- extract.subsample(y2010_sub_forest, alpha = .05)$var.jk.sel.Z

write_xlsx(y2020_vimpCI, "data\\y2020_vimpCI.xlsx")
write_xlsx(y2010_vimpCI, "data\\y2010_vimpCI.xlsx")

#y2020_vimpCI <- read_excel("data\\y2020_vimpCI.xlsx")
#y2010_vimpCI <- read_excel("data\\y2010_vimpCI.xlsx")


# prepare data set
new <- rbind(y2020_vimpCI, y2010_vimpCI)

term <- c("INC_decile1", "concern1", "cc_env_problem1", "priorization_env1", "knowledge1",
          "scepticism1", "seriousness1", "everyday_life1", "risk_perception1", "trust_people1", "trust_institutions1",
          "reciprocity1", "env_committment1", "modern_life_harms1", "science_solution1", "growth_harms1", "worry_jobs1",
          "env_consumption1", "right_party1", "vote_le1", "religiousness1", "petition1", "donation1", "female1", "age1", 
          "education1", "gap_perception1", "rural1", "children1", "brown_job1",
          "weighted_carbon_price1","good_governance1","env_tax1","inc_tax1","revenue_recycling1",
          "INC_decile2", "concern2", "cc_env_problem2", "priorization_env2", "knowledge2",
          "scepticism2", "seriousness2", "everyday_life2", "risk_perception2", "trust_people2", "trust_institutions2",
          "reciprocity2", "env_committment2", "modern_life_harms2", "science_solution2", "growth_harms2", "worry_jobs2",
          "env_consumption2", "right_party2", "vote_le2", "religiousness2", "petition2", "donation2", "female2", "age2", 
          "education2", "gap_perception2", "rural2", "children2", "brown_job2",
          "weighted_carbon_price2","good_governance2","env_tax2","inc_tax2","revenue_recycling2")


category <- c("Demographics","Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations",
              "Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Demographics","Demographics",
              "Demographics","Demographics","Demographics","Demographics","Demographics",
              "Tax system","Tax system","Tax system","Tax system","Tax system",
              "Demographics","Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations",
              "Environmental evaluations","Environmental evaluations","Environmental evaluations","Environmental evaluations","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies",
              "Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Values and ideologies","Demographics","Demographics",
              "Demographics","Demographics","Demographics","Demographics","Demographics",
              "Tax system","Tax system","Tax system","Tax system","Tax system")



data <- tibble(term, new, category) 




# Remove the number at the end of the term
data <- data %>%
  mutate(region = rep(c("2020", "2010"), each = 35))  %>%
  mutate(term_2 = str_replace(term, "\\d+$", ""))



# scale to 1-100
max_value_y2020 <- max(c(y2020_vimpCI$mean))
max_value_y2010 <- max(c(y2010_vimpCI$mean))

data <- data %>%
  mutate(
    lower = ifelse(region=="2020", lower/ max_value_y2020, lower/ max_value_y2010),
    mean = ifelse(region=="2020", mean/ max_value_y2020, mean/ max_value_y2010),
    upper = ifelse(region=="2020", upper/ max_value_y2020, upper/ max_value_y2010)
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
  "cc_env_problem",
  "risk_perception",
  "seriousness",
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
direction_data <- read_excel("data\\effect_direction_time.xlsx")

data_plot = left_join(data, direction_data, by = "term", relationship = "one-to-one")


data_plot <- data_plot %>%
  mutate(linetype_label = ifelse(region == "2020", "2010", "2020"))




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
    linetype = FALSE,
    shape = guide_legend(
      title = NULL,
      override.aes = list(linetype = c("longdash", "solid"), shape = c(23,21)
      )
    )
  )




  





ggsave(plot = gg, width = 11, height = 11, device = png, dpi = 600, 
       filename = "importance_plot_regions.png", path = "output\\")

# Print the plot
print(gg)







#############################################################################################################################################
#############################################################################################################################################

#############################################################################################################################################
#############################################################################################################################################






# Change in importance from 2010 to 2020
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



# Reorder the levels of the category variable
desired_order1 <- c(
  "Values and ideologies",
  "Demographics",
  "Environmental evaluations",
  "Tax system"
)

# Reorder the levels of the sub-variables
desired_order2 <- c(
  "weighted_carbon_price",
  "good_governance",
  "env_tax",
  "inc_tax",
  "revenue_recycling",
  "cc_env_problem",
  "risk_perception",
  "seriousness",
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
  "env_consumption",
  "reciprocity",
  "religiousness",
  "env_committment",
  "vote_le",
  "right_party",
  "trust_people",
  "trust_institutions"
)

data2010 <- tibble(term, y2010_vimpCI, category) 
data2010$category <- factor(data2010$category, levels = desired_order1)
data2010$term <- factor(data2010$term, levels = desired_order2)

data2020 <- tibble(term, y2020_vimpCI, category) 
data2020$category <- factor(data2020$category, levels = desired_order1)
data2020$term_2 <- factor(data2020$term, levels = desired_order2)



data_change = left_join(data2010, data2020, by = "term", relationship = "one-to-one")


# scale to 1-100
max_value_y2020 <- max(c(y2020_vimpCI$mean))
max_value_y2010 <- max(c(y2010_vimpCI$mean))

data_change <- data_change %>%
  mutate(
    mean.y = mean.y/ max_value_y2020,
    mean.x = mean.x / max_value_y2010
  )

data_change$mean.y <- data_change$mean.y * 100
data_change$mean.x <- data_change$mean.x * 100




data_change$gap = data_change$mean.y - data_change$mean.x


## oldschool plot
data_change %>%
  ggplot(aes(x=reorder(term,gap),y=gap)) +
  geom_point(size = 3, colour = "black", aes(color=)) + 
  geom_segment(aes(xend = term, yend = 0), size = 1.2)+
  geom_label(aes(term, gap+0.005, label = signif(gap,2)), colour = "darkred", nudge_x = 0.35, size = 4)+
  labs(y= "Change in variable importance 2010-2020", x="term") +
  coord_flip()

## newschool plot
gg <- data_change %>%
  ggplot(aes(x = reorder(term, gap), y = gap)) +
  geom_segment(aes(xend = term, yend = 0, color = gap), size = 1.2) +
  geom_point(aes(color = gap), size = 4) + 
  labs(y = "Change in relative variable importance 2010-2020", x = "") +
  scale_color_gradient2(low = "#A63716", mid = "#C7CDD1", high = "#15607A") +  # Set the color scale based on "gap" values
  theme_minimal(base_size = 13) +
  scale_y_continuous(limits = c(-31, 42), breaks = seq(-31, 42, by = 5)) +  # y ticks
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed", size = 1) +  # Add a horizontal line at y = 0
  theme(axis.line.y = element_line(color = "black"),  # Set the color of the y-axis line to black
        panel.border = element_blank()) +  # Remove the border around the plot
  guides(color = "none")+# Remove the legend
  coord_flip()  +  
  facet_grid(category.x ~ ., switch = 'y', scales = 'free_y', space = "free_y") +
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
    "inc_tax"="Level of income taxes"))




ggsave(plot = gg, width = 11, height = 11, device = png, dpi = 600, 
       filename = "importance_plot_time.png", path = "output\\")

# Print the plot
print(gg)












