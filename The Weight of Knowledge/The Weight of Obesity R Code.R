library(ggplot2)
library(dplyr)
library(broom) 
library(patchwork)
library(tidyr)  

#rm(list=ls())
rawdata <- read.csv("Obesity.csv")
#head(str(rawdata))
####################Clean the data##########
rev_data <- rawdata %>%
  mutate(
    Age = as.integer(Age),
    BMI = Weight / (Height ^ 2),
    MealsDaily = as.integer(NCP),
    TechDeviceUse = as.integer(TUE),
    WaterDrank = as.integer(CH2O),
    PhysicalActivity = as.integer(FAF),
    HighCalDiet = FAVC,
    MonitorCal = SCC,
    Smoke = SMOKE,
    BMI_Category = factor(
      case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI < 25 ~ "Healthy Weight",
        BMI >= 25 & BMI < 30 ~ "Overweight",
        BMI >= 30 ~ "Obesity"),
      levels = c("Underweight", "Healthy Weight", "Overweight", "Obesity")),
    FamilyHistory = family_history_with_overweight,
    Gender_num = ifelse(Gender == "Male", 1, 0),
    family_history_num = ifelse(FamilyHistory == "yes", 1, 0),
    HighCalDiet_num = ifelse(FAVC == "yes", 1, 0),
    Smoke_num = ifelse(SMOKE == "yes", 1, 0),
    MonitorCal_num = ifelse(SCC == "yes", 1, 0),
    MealsDaily = factor(MealsDaily, levels = c("1", "2", "3", "4")),
    WaterDrank = factor(WaterDrank, levels = c("1", "2", "3")),
    PhysicalActivity = factor(PhysicalActivity, levels = c("0", "1", "2", "3")),
    TechDeviceUse = factor(TechDeviceUse, levels = c("0", "1", "2")),
    Alcohol = factor(CALC, levels = c("no", "Sometimes", "Frequently", "Always")),
    EatBetweenMeals = factor(CAEC, levels = c("no", "Sometimes", "Frequently", "Always")),
    Transportation = factor(MTRANS, levels = c("Walking", "Bike", "Motorbike", "Public_Transportation", "Automobile")),
    BMICategory_1 = factor(NObeyesdad, levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", 
                                                  "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
  )

#########################Box plots#######
plot_box <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, y = "BMI")) +
    geom_boxplot(fill = "#ABA3E3", color = "black") +
    labs(
      title = paste(x_var),
      y = "BMI"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      axis.title.x = element_blank()  # Removes the x-axis label
    )
}

#head(str(rev_data))
#unique(rev_data)
p1 <- plot_box(rev_data, "Gender")
p2 <- plot_box(rev_data, "Alcohol") 
p3 <- plot_box(rev_data, "HighCalDiet")
p4 <- plot_box(rev_data, "MealsDaily") 
p5 <- plot_box(rev_data, "MonitorCal")
p6 <- plot_box(rev_data, "Smoke")
p7 <- plot_box(rev_data, "WaterDrank")
p8 <- plot_box(rev_data, "FamilyHistory")
p9 <- plot_box(rev_data, "PhysicalActivity")
p10 <- plot_box(rev_data, "TechDeviceUse")
p11 <- plot_box(rev_data, "EatBetweenMeals")
p12 <- plot_box(rev_data, "Transportation") 
p13 <- plot_box(rev_data, "BMICategory_1")

p8  + p3 + p6  + p1  + p9 + p4 + p11  + p5 + p2  +  p7 + p12 + p13
# Combine the plots
combined_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12
# reorder
combined_plot2 <-p8  + p3 + p6  + p1 + p5 + p9 +p10  + p7+ p4 + p2   + p11  +  p12
ggsave("combined_plot2.png", plot = combined_plot2, width = 8, height = 8, dpi = 300)




# Logistic regression function for each BMI category
run_logistic_regression_per_category <- function(var) {
  results <- lapply(levels(rev_data$BMI_Category), function(category) {
    # Recode the BMI_Category to a binary outcome for each level
    rev_data_temp <- rev_data %>%
      mutate(BMI_Category_Binary = ifelse(BMI_Category == category, 1, 0))
    
    # Fit the logistic regression model
    model <- glm(BMI_Category_Binary ~ get(var), data = rev_data_temp, family = "binomial")
    coef <- tidy(model) %>% filter(term == paste0("get(var)"))
    
    # Extract coefficients and create a data frame with category information
    coef %>%
      select(term, estimate) %>%
      rename(Variable = term, Coefficient_Estimate = estimate) %>%
      mutate(Variable = var, BMI_Category = category)
  })
  bind_rows(results)
}

# List of binary variables for analysis
binary_vars <- c("Gender_num", "family_history_num", "HighCalDiet_num", "Smoke_num", "MonitorCal_num")

# Run logistic regression for each binary variable across BMI categories
logistic_results_per_category <- bind_rows(lapply(binary_vars, run_logistic_regression_per_category))

logistic_results_per_category$BMI_Category <- factor(
  logistic_results_per_category$BMI_Category,
  levels = c("Underweight", "Healthy Weight", "Overweight", "Obesity")
)

logistic_results_per_category$Variable <- factor(
  logistic_results_per_category$Variable,
  levels = c("family_history_num", "HighCalDiet_num", "Smoke_num", "Gender_num", "MonitorCal_num")
)


# Create a heatmap to show influence on each BMI category
ggplot(logistic_results_per_category, aes(x = Variable, y = BMI_Category, fill = Coefficient_Estimate)) +
  geom_tile() +
  scale_fill_gradient2(low = "#FF99BF", mid = "#F7FAFF", high = "#422CB2", midpoint = 0) +
  labs(title = "Influence of Lifestyle on BMI Categories",
       x = "Variable",
       y = "BMI Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 0.5))


###########

# Convert ordinal variables to numeric for correlation
rev_data <- rev_data %>%
  mutate(
    MealsDaily_num = as.numeric(as.character(MealsDaily)),
    TechDeviceUse_num = as.numeric(as.character(TechDeviceUse)),
    WaterDrank_num = as.numeric(as.character(WaterDrank)),
    PhysicalActivity_num = as.numeric(as.character(PhysicalActivity)),
    BMI_Category = factor(BMI_Category, levels = c("Underweight", "Healthy Weight", "Overweight", "Obesity"))
  )

# Add Age to the continuous variables
continuous_vars <- c("Age", "PhysicalActivity_num", "TechDeviceUse_num", "MealsDaily_num", "WaterDrank_num")

# Calculate correlation for each BMI category
calculate_correlation_per_category <- function(var) {
  results <- lapply(levels(rev_data$BMI_Category), function(category) {
    # Recode BMI_Category into binary for the current category
    rev_data_temp <- rev_data %>%
      mutate(BMI_Category_Binary = ifelse(BMI_Category == category, 1, 0)) %>%
      mutate(BMI_Category_Binary = as.numeric(BMI_Category_Binary)) # Ensure numeric type
    
    # Calculate Pearson correlation
    correlation <- cor(as.numeric(rev_data_temp[[var]]), rev_data_temp$BMI_Category_Binary, use = "complete.obs")
    
    # Store results
    data.frame(Variable = var, BMI_Category = category, Correlation = correlation)
  })
  bind_rows(results)
}

# Run correlation analysis for selected variables
correlation_results <- bind_rows(
  lapply(continuous_vars, calculate_correlation_per_category)
)

# Rename variables for visualization
correlation_results <- correlation_results %>%
  mutate(Variable = factor(Variable, levels = c("Age", "MealsDaily_num", "WaterDrank_num", "TechDeviceUse_num", "PhysicalActivity_num")))
correlation_results <- correlation_results %>%
  mutate(BMI_Category = factor(BMI_Category, levels = c("Underweight", "Healthy Weight", "Overweight", "Obesity")))

# Heatmap for correlations
ggplot(correlation_results, aes(x = Variable, y = BMI_Category, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#FF99BF", mid = "#F7FAFF", high = "#422CB2", midpoint = 0) +
  labs(title = "Influence of Lifestyle and Age on BMI Categories",
       x = "Variable",
       y = "BMI Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.y = element_text(angle = 0, hjust = 0.5))


######### to form a table####
library(gt)

# Create the data frame
variable_table <- data.frame(
  Original_Variable_Name = c(
    "Gender", "Age", "Height", "Weight", "family_history_with_overweight", 
    "FAVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC", "FAF", "TUE", "CALC", 
    "MTRANS", "NObeyesdad", "*New variable*", "*New variable*"
  ),
  Revised_Variable_Name = c(
    "Gender", "Age", "Height", "Weight", "FamilyHistory", 
    "HighCalDiet", "MealsDaily", "EatBetweenMeals", "Smoke", "WaterDrank", 
    "MonitorCal", "PhysicalActivity", "TechDeviceUse", "Alcohol", 
    "Transportation", "BMICategory_1", "BMI_Category", "BMI"
  ),
  Role = c(
    "Feature", "Feature", "Feature", "Feature", "Feature", 
    "Feature", "Feature", "Feature", "Feature", "Feature", 
    "Feature", "Feature", "Feature", "Feature", "Feature", 
    "Target", "Feature", "Feature"
  ),
  Type = c(
    "Categorical", "Continuous", "Continuous", "Continuous", "Binary", 
    "Binary", "Continuous", "Categorical", "Binary", "Continuous", 
    "Binary", "Continuous", "Integer", "Categorical", "Categorical", 
    "Categorical", "Categorical", "Continuous"
  ),
  Description = c(
    "Gender", "Age", "Height", "Weight", 
    "Has a family member suffered or suffers from overweight?", 
    "Do you eat high-caloric food frequently?", 
    "How many main meals do you have daily?", 
    "Do you eat any food between meals?", 
    "Do you smoke?", 
    "How much water do you drink daily?", 
    "Do you monitor the calories you eat daily?", 
    "How often do you have physical activity?", 
    "How much time do you use technological devices?", 
    "How often do you drink alcohol?", 
    "Which transportation do you usually use?", 
    "Obesity level (detailed)", 
    "Obesity level (generalized)", 
    "Body Mass Index"
  ),
  Unique_Values = c(
    "\"Female\", \"Male\"", "-", "-", "-", 
    "\"yes\", \"no\"", 
    "\"no\", \"yes\"", 
    "1, 2, 3, 4", 
    "\"Sometimes\", \"Frequently\", \"Always\", \"no\"", 
    "\"no\", \"yes\"", 
    "-", 
    "\"no\", \"yes\"", 
    "-", 
    "0, 1, 2", 
    "\"no\", \"Sometimes\", \"Frequently\", \"Always\"", 
    "\"Public_Transportation\", \"Walking\", \"Automobile\", \"Motorbike\", \"Bike\"", 
    "\"Normal_Weight\", \"Overweight_Level_I\", \"Overweight_Level_II\", \"Obesity_Type_I\", \"Insufficient_Weight\", \"Obesity_Type_II\", \"Obesity_Type_III\"", 
    "\"Underweight\", \"Healthy Weight\", \"Overweight\", \"Obesity\"", 
    "-"
  )
)

# Create a styled table using gt
variable_table %>%
  gt() %>%
  tab_header(
    title = "Variable Overview with Revised Names",
    subtitle = "Including Role, Type, and Description"
  ) %>%
  cols_label(
    Original_Variable_Name = "Original Variable Name",
    Revised_Variable_Name = "Revised Variable Name",
    Role = "Role",
    Type = "Type",
    Description = "Description",
    Unique_Values = "Unique Values"
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.title.font.size = "large",
    heading.subtitle.font.size = "medium"
  )
