#Bardh Kukaj's Predictive Models on Adult Income Census (from Kaggle)/Capstone Course - CYO project


##########################################################
#                   Adult Income Census                  #
##########################################################

# Installing/Setting libraries in case they are not already installed - the code below that is provided by the course to import and set the data
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(caret)) install.packages("caret")
if (!require(ggthemes)) install.packages("ggthemes")
if (!require(knitr)) install.packages("knitr")
if (!require(rpart)) install.packages("rpart")
if (!require(randomForest)) install.packages("randomForest")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(doParallel)) install.packages("doParallel") # to speed up computation time for the last model

library(tidyverse)
library(caret)
library(ggthemes)
library(knitr)
library(rpart)
library(randomForest)
library(rpart.plot)
library(gridExtra)
library(doParallel)


# Data set stored in my GitHub repository: https://github.com/bardhkukaj91/CYO_adult_census_income/raw/main/adult.xlsx
# downloading the data

githubfile <- "https://github.com/bardhkukaj91/CYO_adult_census_income/raw/main/adult.csv"

aic_data <- read.csv(url(githubfile))

# exploring the data - how many NAs?

head(aic_data)
na_summary <- sapply(aic_data, {function(x) any(is.na(x))})

kable(na_summary, caption = "Checking for missing data")

# I see some ? characters in some rows. Creating a function that replaces them with NA in all columns and then removing the NAs as well

clean_data <- function(aic_data) {
  aic_data <- lapply(aic_data, function(x) {
    if (is.character(x)) {
    x[x == "?"] <- NA
    }
    return(x)
  })
    aic_data <- as.data.frame(aic_data)
  
  
  # Remove rows with NA values
  aic_data <- na.omit(aic_data)
  return(aic_data)
}

# Applying the function to the dataset
aic_data_clean <- clean_data(aic_data)

# checking the data, we see fewer observations now: 30,162; previously there were 32,561
head(aic_data_clean)

# checking the types of data
str(aic_data_clean)

# since I will be running a logistic regression, I need to specify that several variables/columns are factors
categorical_variables <- c("workclass", "sex", "race", "marital.status", "occupation", "native.country", "education", "relationship", "income")
aic_data_clean[categorical_variables] <- lapply(aic_data_clean[categorical_variables], factor)

# checking if the encoding was done correctly
str(aic_data_clean)

# double-checking if the levels (categories) are in the right order, with "<=50k" being the first one, as that is how I'd prefer it to be
levels(aic_data_clean$income)

summary(aic_data_clean)

# Creating the train and test sets
set.seed(2008)
test_index <- createDataPartition(y = aic_data_clean$income, times = 1, p = 0.2, list = FALSE)
test_set <- aic_data_clean[test_index, ]
train_set <- aic_data_clean[-test_index, ]

# visualization of variables

# adding the percentage of adults earning on average less than 50k annually - i'll be using that on each graph
mean_hline <- mean(aic_data_clean$income== ">50K")*100

## occupation - the percent of each occupation in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
occ_percentage <- aic_data_clean %>%
  group_by(occupation, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar for each occupation
ggplot(occ_percentage, aes(x = occupation, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Occupation", fill = "Income",
       caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Occupation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Rotates x-axis labels to 90 degrees
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## education years - the percent of each year of education status in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
ed_percentage <- aic_data_clean %>%
  group_by(education.num, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each year of education
ggplot(ed_percentage, aes(x = education.num, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Education Years", fill = "Income",
       caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Education Years") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## workclass - the percent of each workclass in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
workc_percentage <- aic_data_clean %>%
  group_by(workclass, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each work class
ggplot(workc_percentage, aes(x = workclass, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Work Class", fill = "Income", 
       caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Work Class") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # x-axis labels rotated to 90 degrees
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## capital.gain - categorizing over or under 50k income in case one has capital gains
### firstly categorizing the data so that I can visualize them easily with ggplot

cap_cat <- aic_data_clean %>% 
  mutate(capital_gain = ifelse(capital.gain == "0", "No Capital Gain", "Has Capital Gain"))

### then summarizing the data so that I can visualize them easily with ggplot
cap_percentage <- cap_cat %>%
  group_by(capital_gain, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar - Those who have capital gains, and those who haven't
g1 <- ggplot(cap_percentage, aes(x = capital_gain, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Capital gain", fill = "Income") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels - Capital Gains vs. None") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## capital.loss - categorizing over or under 50k income in case one has capital losses
### firstly categorizing the data so that I can visualize them easily with ggplot

capl_cat <- aic_data_clean %>% 
  mutate(capital_loss = ifelse(capital.loss == "0", "No Capital Loss", "Has Capital Loss"))

### then summarizing the data so that I can visualize them easily with ggplot
capl_percentage <- capl_cat %>%
  group_by(capital_loss, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar - Those who have capital gains, and those who haven't
g2 <- ggplot(capl_percentage, aes(x = capital_loss, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Capital loss", fill = "Income",
       caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels - Capital Losses vs. None") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

grid.arrange(g1, g2, nrow=2)

## working hours per week - the percent of each category of working type (part-, full-, full- + overt-time) 
## in terms of over or under 50k income
### firstly categorizing the data so that I can visualize them easily with ggplot

hrs_cat <- aic_data_clean %>% 
  mutate(work_hours_category = case_when(
    hours.per.week < 40 ~ "Part-time",               # less than 40 hrs is categorized as part-time
    hours.per.week == 40 ~ "Full-time",              # exactly 40 hrs is categorized as full-time
    hours.per.week > 40 ~ "Full-time + Overtime"))   # more than 40 hrs is categorized as full-time + overtime

### then summarizing the data so that I can visualize them easily with ggplot

hrs_percentage <- hrs_cat %>%
  group_by(work_hours_category, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each work type category
ggplot(hrs_percentage, aes(x = work_hours_category, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Type of Work", fill = "Income",
       caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Type of Work") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## race - the percent of each race in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
race_percentage <- aic_data_clean %>%
  group_by(race, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each race
ggplot(race_percentage, aes(x = race, y = Percentage, fill = income)) +
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Race", fill = "Income",
  caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Race") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## sex - the percent of each sex in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
sex_percentage <- aic_data_clean %>%
  group_by(sex, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar for each sex
ggplot(sex_percentage, aes(x = sex, y = Percentage, fill = income)) +
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Sex", fill = "Income",
  caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Sex") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## marital status - the percent of each marital status in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
ms_percentage <- aic_data_clean %>%
  group_by(marital.status, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each marital status
ggplot(ms_percentage, aes(x = marital.status, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Marital Status", fill = "Income",
  caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Marital Status") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## relationship status - the percent of each relationship status in terms of over or under 50k income
### firstly summarizing the data so that I can visualize them easily with ggplot
rs_percentage <- aic_data_clean %>%
  group_by(relationship, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar per each relationship status
ggplot(rs_percentage, aes(x = relationship, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Relationship Status", fill = "Income",
  caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Relationship Status") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

## native country - the percent of each native country in terms of over or under 50k income
### firstly categorizing the data so that I can visualize them easily with ggplot

nat_cat <- aic_data_clean %>% 
  mutate(native_country = ifelse(native.country == "United-States", "United-States", "Non-US"))

### then summarizing the data so that I can visualize them easily with ggplot
natc_percentage <- nat_cat %>%
  group_by(native_country, income) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  ungroup()

### visualizing through a stacked bar - US and Non-US
ggplot(natc_percentage, aes(x = native_country, y = Percentage, fill = income)) + 
  geom_bar(stat = "identity", position = "stack") +     # specifying the x and y axes + specifying the stacked bar
  labs(y = "Percentage", x = "Native Country", fill = "Income",
  caption = "Note: Red line represents the % of adult population earning more than 50K USD annually") +
  theme_clean() +                                       # using the "clean" theme from ggthemes package 
  scale_fill_brewer(palette = "Pastel1") +              # 1st selection of the pastel colors
  ggtitle("Income Levels by Country of Origin") +
  geom_hline(yintercept = mean_hline, linetype = "solid", color = "red")+
  theme(plot.caption = element_text(size = 8, color = "red"))

# training the baseline model

train_base_aic <- train(income ~ workclass + education + occupation + capital.gain + capital.loss + hours.per.week, method = "glm", data = train_set)

# saving the predicted values in y_hat_aic
y_hat_base_aic <- predict(train_base_aic, newdata = test_set, type = "raw")

# checking the accuracy and F1 score
baseOA <- confusionMatrix(y_hat_base_aic, test_set$income)$overall[["Accuracy"]]

baseF1 <- F_meas(data = y_hat_base_aic, reference = test_set$income)

baselinemodel <- train_base_aic$finalModel

summary(baselinemodel)

Results_Summary <- tibble(method="Baseline Logistic Regression", Overall_Accuracy = baseOA, F1_Score = baseF1)

# training the full logistic regression model
train_aic <- train(income ~ . , method = "glm", data = train_set)
                   
# saving the predicted values in y_hat_aic
y_hat_aic <- predict(train_aic, newdata = test_set, type = "raw")

# checking the accuracy and F1 score
logOA <- confusionMatrix(y_hat_aic, test_set$income)$overall[["Accuracy"]]

logF1 <- F_meas(data = y_hat_aic, reference = test_set$income)

# checking the coefficients and their statistical significance
full_log_model <- train_aic$finalModel

summary(full_log_model)

Results_Summary <- bind_rows(Results_Summary, tibble(method="Logistic Regression + biases", Overall_Accuracy = logOA, F1_Score = logF1))

# Likelihood ratio test
# as I am primarily working with the train function of the caret package, I will switch to the glm function
# to obtain the predict values and run the LTR test in a simpler code, as I couldn't do it with the predict values
# from the train function (caret package)

# Fitting the baseline model
baseline_model <- glm(income ~ age + workclass + education + occupation + capital.gain + capital.loss + hours.per.week, 
                  data = train_set, family = binomial())

# Fit the full model (assuming you want all other variables included)
full_model <- glm(income ~ ., data = train_set, family = binomial())

lrt_result<- anova(baseline_model, full_model, test = "Chisq")

lrt_result

# Decision tree - default - training and predicting the values first
dtdef_model <- train(income ~ ., data = train_set, method = "rpart")

predictions_dtdef_model <- predict(dtdef_model, newdata = test_set, type = "raw")

# calculating OA and F1, and saving the results

classOA <- confusionMatrix(predictions_dtdef_model, test_set$income)$overall[["Accuracy"]]

classF1 <- F_meas(data = predictions_dtdef_model, reference = test_set$income)

Results_Summary <- bind_rows(Results_Summary, tibble(method="Decision tree - Default Settings", Overall_Accuracy = classOA, F1_Score = classF1))

# plotting the decision tree

rpart.plot(dtdef_model$finalModel)

# Controlled decision tree - cross validation + tuning of complexity parameters; training the model first

dt_control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.cp = seq(0.01, 0.1, by = 0.01))

dt_model <- train(income ~ ., data = train_set, method = "rpart", trControl = dt_control, tuneGrid = grid)

# Predict the model and run the OA and F1 tests
predictions_dt_model <- predict(dt_model, newdata = test_set)

dtOA <- confusionMatrix(predictions_dt_model, test_set$income)$overall[["Accuracy"]]
dtF1 <- F_meas(data = predictions_dt_model, reference = test_set$income)

# Save the reults
Results_Summary <- bind_rows(Results_Summary, tibble(method="Controlled Decision Tree", Overall_Accuracy = dtOA, F1_Score = dtF1))

# Random forest - default settings

# Train the model
rfmodel_default <- randomForest(income ~ ., data = train_set)

# Predict the model and run the OA and F1 results
predictions_rfmodel_def <- predict(rfmodel_default, newdata = test_set)

rfmodel_defOA <- confusionMatrix(predictions_rfmodel_def, test_set$income)$overall[["Accuracy"]]

rfmodel_defF1 <- F_meas(data = predictions_rfmodel_def, reference = test_set$income)

# Save the results
Results_Summary <- bind_rows(Results_Summary, tibble(method="Random Forest - Default Settings", Overall_Accuracy = rfmodel_defOA, F1_Score = rfmodel_defF1))

# Random forest - Tuned Model

# Defining the range of mtry
grid <- data.frame(mtry =  c(1, 3, 5, 7))

# for faster computation
registerDoParallel(cores = detectCores()) # activating parallel processing to speed up computation time

# Train control with cross-validation
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

# Train the Tuned Random Forest model
rfmodel_tuned <- train(income ~ ., data = train_set, method = "rf", tuneGrid = grid, trControl = train_control, ntree = 100)

# Predict the model
predictions_rfmodel_tuned <- predict(rfmodel_tuned, newdata = test_set)

# stop faster computation

stopImplicitCluster() # deactivating parallel processing


rfmodel_tunedOA <- confusionMatrix(predictions_rfmodel_tuned, test_set$income)$overall[["Accuracy"]]

rfmodel_tunedF1 <- F_meas(data = predictions_rfmodel_tuned, reference = test_set$income)

# Save the results
Results_Summary <- bind_rows(Results_Summary, tibble(method="Random Forest - Tuned", 
                                                     Overall_Accuracy = rfmodel_tunedOA, F1_Score = rfmodel_tunedF1))

# Adding five figures behind the decimal point
Results_Summary <- Results_Summary %>% mutate(Overall_Accuracy = sprintf("%.5f", Overall_Accuracy), 
                                              F1_Score = sprintf("%.5f", F1_Score))

# Show the results
kable(Results_Summary, caption = "Summary of Results")

# as the RF default settings model has the highest OA and F1 score
# its predicted values will be plotted alongside the actual values of the test set using a tileplot

conf_matrix_visuals <- confusionMatrix(predictions_rfmodel_def, test_set$income)

# Extracting the table from the confusion matrix
conf_matrix_table <- as.table(conf_matrix_visuals$table)

# Converting to data frame to visualize with ggplot
conf_matrix_plot <- as.data.frame(conf_matrix_table)

# Renaming columns
names(conf_matrix_plot) <- c("Reference", "Prediction", "Frequency")

ggplot(data = conf_matrix_plot, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() + # plotting with a tile for each combination of actual and predicted values
  geom_text(aes(label = sprintf("%0.0f", Frequency)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red") + # adding color gradient for visualizing frequency
  theme_minimal() + # Minimal theme
  labs(title = "Confusion Matrix: Actual vs. Predicted",
       x = "Actual Value",
       y = "Predicted Value",
       fill = "Frequency")
