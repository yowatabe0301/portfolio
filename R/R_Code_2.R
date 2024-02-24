#' Name: Yoshiyuki Watabe
#' Date: Feb 2, 2024

# libraries
library(rpart)
library(caret)
library(plyr)
library(dplyr)
library(MLmetrics)
library(vtreat)
library(ggplot2)
library(corrplot)

# WD
setwd("~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/r_studio/hult_class/personalFiles")

# training data
allTrainingFiles <- list.files(path = '~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/r_studio/hult_class/Cases/A2_Household_Spend/studentTables',
                               pattern = 'training',
                               full.names = T)

# Load the files and arrange them with a left join
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by='tmpID', type='left')

# testing data
allTestingFiles <- list.files(path = '~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/r_studio/hult_class/Cases/A2_Household_Spend/studentTables',
                               pattern = 'testing',
                               full.names = T)

# Load the files and arrange them with a left join
allTestingDF <- lapply(allTestingFiles, read.csv)
allTestingDF <- join_all(allTestingDF, by='tmpID', type='left')

# prospect data
allProspectFiles <- list.files(path = '~/Desktop/+/Hult/MBAN/Visualizing_and_Analyzing_Data_with_R/r_studio/hult_class/Cases/A2_Household_Spend/studentTables',
                              pattern = 'prospect',
                              full.names = T)

# Load the files and arrange them with a left join
allProspectDF <- lapply(allProspectFiles, read.csv)
allProspectDF <- join_all(allProspectDF, by='tmpID', type='left')

# Train 80% / Validation 20% Partitioning for allTrainingDF
splitPercent <- round(nrow(allTrainingDF) * 0.8)
totalRecords <- 1:nrow(allTrainingDF)
totalRecords
idx <- sample(totalRecords, splitPercent)

# Using the vector of numbers in the "row" position to create a training set and the minus for validation set.
train <- allTrainingDF[idx, ]
validation <- allTrainingDF[-idx, ]

# EXPLORE
### Perform robust exploratory data analysis, drop any columns that you think don't make sense, or are unethical to use in use case.  You can build visuals, tables, summaries and explore the data's overall integrity.

# Summary
summary(train)

# Dimensions
dim(train)

# Convert character variables to factors and then check the number of unique categories
train_factors <- train
train_factors[] <- lapply(train_factors, function(x) if(is.character(x)) factor(x) else x)
sapply(train_factors[, sapply(train_factors, is.factor)], function(x) length(unique(x)))

# Save the unique counts to a data frame
unique_counts <- sapply(train_factors[, sapply(train_factors, is.factor)], function(x) length(unique(x)))
category_names <- names(unique_counts)
data <- data.frame(Category = category_names, UniqueCount = unique_counts)

# Create a bar chart
ggplot(data, aes(x = reorder(Category, -UniqueCount), y = UniqueCount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Category", y = "Unique Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Convert blank strings to NA
train_na <- train
train_na[] <- lapply(train_na, function(x) if(is.character(x)) {x[x == ""] <- NA; x} else x)

# Check for missing values including blank strings
colSums(is.na(train_na))

# Proportion of missing values in each variable
missing_percentage <- colSums(is.na(train)) / nrow(train)
print(missing_percentage)

# Detect outliers
boxplot(train$Age, main="Age Boxplot")
boxplot(train$MedianEducationYears, main="MedianEducationYears Boxplot")
boxplot(train$yHat, main="yHat Boxplot")

# Check distribution of data
hist(train$Age, main="Histogram of Age", xlab="Age")
hist(train$MedianEducationYears, main="Histogram of MedianEducationYears", xlab="MedianEducationYears")
hist(train$yHat, main="Histogram of yHat", xlab="yHat")

# Correlation analysis
cor_matrix <- cor(train[, sapply(train, is.numeric)], use="complete.obs")
corrplot(cor_matrix, method="circle")
print(cor_matrix)

# Analyze categorical variables (using Gender column as an example)
ggplot(train, aes(x=Gender)) + geom_bar() + theme_minimal() + labs(title="Distribution of Gender")

# MODIFY 
### Using the training, create a design treatment plan.
#1 Identify the informative features (x variables)
#2 Identify the target variable (y variable)

# All column names, after EXPLORE you should know which variables you want to use
names(allTrainingDF) # all variables


# Model 1
informartiveFeatures <- c('ResidenceHHGenderDescription', 'EthnicDescription', 'BroadEthnicGroupings', 'PresenceOfChildrenCode', 'ISPSA', 'HomeOwnerRenter', 'MedianEducationYears', 'NetWorth', 'Investor', 'Education', 'OccupationIndustry', 'ComputerOwnerInHome', 'DonatesEnvironmentCauseInHome', 'DonatesToCharityInHome', 'Gender', 'Age', 'lat', 'lon', 'county', 'city', 'state', 'fips', 'stateFips', 'HomePurchasePrice', 'LandValue', 'DwellingUnitSize', 'storeVisitFrequency', 'PropertyType', 'EstHomeValue', 'PartiesDescription', 'ReligionsDescription', 'LikelyUnionMember', 'GunOwner', 'Veteran')
targetName <- 'yHat'
plan <- designTreatmentsN(dframe      = train, 
                          varlist     = informartiveFeatures,
                          outcomename = targetName)

# Apply the plan to all sections of the data
treatedTrain <- prepare(plan, train)
treatedValidation <- prepare(plan, validation) # this is the subset of data from line 30
treatedTest <- prepare(plan, allTestingDF) # this is the data set from repeating the read.csv section but with the test CSV files
treatedProspects <- prepare(plan, allProspectDF) #this is the data set from repeating read.csv but with the prospect CSV files

# MODEL
### You should be able to fit a linear model and you should challenge yourself to research and build additional algorithm types
fit <- lm(yHat~., treatedTrain)
summary(fit)

# ASSESS
### Make predictions
linearTrainPredictions      <- predict(fit, treatedTrain)
linearValidationPredictions <- predict(fit, treatedValidation)

# Calculate RMSE for the training set
actualTrain <- treatedTrain$yHat
rmseTrain <- RMSE(y_pred = linearTrainPredictions, y_true = actualTrain)
print(paste("Training RMSE:", rmseTrain))

# Calculate RMSE for the validation set
actualValidation <- treatedValidation$yHat
rmseValidation <- RMSE(y_pred = linearValidationPredictions, y_true = actualValidation)
print(paste("Validation RMSE:", rmseValidation))

# Once you are done get predictions for the test set
linearTestPredictions       <- predict(fit, treatedTest) 

# Calculate RMSE for the test set
actualTest <- treatedTest$yHat
rmseTest <- RMSE(y_pred = linearTestPredictions, y_true = actualTest)
print(paste("Test RMSE:", rmseTest))

# Compare the training, validation and test set RMSE.  Look for consistency.
if (abs(rmseTrain - rmseValidation) < 10 && abs(rmseTrain - rmseTest) < 10) {
  print("The model shows consistency across the datasets.")
} else {
  print("There may be overfitting. Consider revising the model.")
}

# Calculate MAPE for the training set using MLmetrics
mapeTraining <- MAPE(y_pred = linearTrainPredictions, y_true = actualTrain)
print(paste("Training MAPE:", mapeTraining))

# Calculate MAPE for the validation set using MLmetrics
mapeValidation <- MAPE(y_pred = linearValidationPredictions, y_true = actualValidation)
print(paste("Validation MAPE:", mapeValidation))

# Calculate MAPE for the test set using MLmetrics
mapeTest <- MAPE(y_pred = linearTestPredictions, y_true = actualTest)
print(paste("Test MAPE:", mapeTest))

# Model 2
informartiveFeatures2 <- c('ISPSA', 'MedianEducationYears', 'NetWorth','HomeOwnerRenter', 'MosaicZ4','storeVisitFrequency')
targetName <- 'yHat'
plan2 <- designTreatmentsN(dframe      = train, 
                          varlist     = informartiveFeatures2,
                          outcomename = targetName)

# Apply the plan to all sections of the data
treatedTrain2 <- prepare(plan2, train)
treatedValidation2 <- prepare(plan2, validation) # this is the subset of data from line 30
treatedTest2 <- prepare(plan2, allTestingDF) # this is the data set from repeating the read.csv section but with the test CSV files
treatedProspects2 <- prepare(plan2, allProspectDF) #this is the data set from repeating read.csv but with the prospect CSV files

# MODEL
### You should be able to fit a linear model and you should challenge yourself to research and build additional algorithm types
fit2 <- lm(yHat~., treatedTrain2)
summary(fit2)

# ASSESS
### Make predictions
linearTrainPredictions2      <- predict(fit2, treatedTrain2)
linearValidationPredictions2 <- predict(fit2, treatedValidation2)

# Calculate RMSE for the training set
actualTrain2 <- treatedTrain2$yHat
rmseTrain2 <- RMSE(y_pred = linearTrainPredictions2, y_true = actualTrain2)
print(paste("Training RMSE:", rmseTrain2))

# Calculate RMSE for the validation set
actualValidation2 <- treatedValidation2$yHat
rmseValidation2 <- RMSE(y_pred = linearValidationPredictions2, y_true = actualValidation2)
print(paste("Validation RMSE:", rmseValidation2))

# Once you are done get predictions for the test set
linearTestPredictions2       <- predict(fit2, treatedTest2) 

# Calculate RMSE for the test set
actualTest2 <- treatedTest2$yHat
rmseTest2 <- RMSE(y_pred = linearTestPredictions2, y_true = actualTest2)
print(paste("Test RMSE:", rmseTest2))

# Compare the training, validation and test set RMSE.  Look for consistency.
if (abs(rmseTrain2 - rmseValidation2) < 10 && abs(rmseTrain2 - rmseTest2) < 10) {
  print("The model shows consistency across the datasets.")
} else {
  print("There may be overfitting. Consider revising the model.")
}

# Calculate MAPE for the training set using MLmetrics
mapeTraining2 <- MAPE(y_pred = linearTrainPredictions2, y_true = actualTrain2)
print(paste("Training MAPE:", mapeTraining2))

# Calculate MAPE for the validation set using MLmetrics
mapeValidation2 <- MAPE(y_pred = linearValidationPredictions2, y_true = actualValidation2)
print(paste("Validation MAPE:", mapeValidation2))

# Calculate MAPE for the test set using MLmetrics
mapeTest2 <- MAPE(y_pred = linearTestPredictions2, y_true = actualTest2)
print(paste("Test MAPE:", mapeTest2))

# Model 3
informartiveFeatures3 <- c('BookBuyerInHome', 'UpscaleBuyerInHome', 'ComputerOwnerInHome','DonatestoArtsandCulture', 'DonatestoHealthcare','GardeningMagazineInHome')
targetName <- 'yHat'
plan3 <- designTreatmentsN(dframe      = train, 
                           varlist     = informartiveFeatures3,
                           outcomename = targetName)

# Apply the plan to all sections of the data
treatedTrain3 <- prepare(plan3, train)
treatedValidation3 <- prepare(plan3, validation) # this is the subset of data from line 30
treatedTest3 <- prepare(plan3, allTestingDF) # this is the data set from repeating the read.csv section but with the test CSV files
treatedProspects3 <- prepare(plan3, allProspectDF) #this is the data set from repeating read.csv but with the prospect CSV files

# MODEL
### You should be able to fit a linear model and you should challenge yourself to research and build additional algorithm types
fit3 <- lm(yHat~., treatedTrain3)
summary(fit3)

# ASSESS
### Make predictions
linearTrainPredictions3      <- predict(fit3, treatedTrain3)
linearValidationPredictions3 <- predict(fit3, treatedValidation3)

# Calculate RMSE for the training set
actualTrain3 <- treatedTrain3$yHat
rmseTrain3 <- RMSE(y_pred = linearTrainPredictions3, y_true = actualTrain3)
print(paste("Training RMSE:", rmseTrain3))

# Calculate RMSE for the validation set
actualValidation3 <- treatedValidation3$yHat
rmseValidation3 <- RMSE(y_pred = linearValidationPredictions3, y_true = actualValidation2)
print(paste("Validation RMSE:", rmseValidation3))

# Once you are done get predictions for the test set
linearTestPredictions3       <- predict(fit3, treatedTest3) 

# Calculate RMSE for the test set
actualTest3 <- treatedTest3$yHat
rmseTest3 <- RMSE(y_pred = linearTestPredictions3, y_true = actualTest3)
print(paste("Test RMSE:", rmseTest3))

# Compare the training, validation and test set RMSE.  Look for consistency.
if (abs(rmseTrain2 - rmseValidation2) < 10 && abs(rmseTrain2 - rmseTest2) < 10) {
  print("The model shows consistency across the datasets.")
} else {
  print("There may be overfitting. Consider revising the model.")
}

# Calculate MAPE for the training set using MLmetrics
mapeTraining3 <- MAPE(y_pred = linearTrainPredictions3, y_true = actualTrain3)
print(paste("Training MAPE:", mapeTraining3))

# Calculate MAPE for the validation set using MLmetrics
mapeValidation3 <- MAPE(y_pred = linearValidationPredictions3, y_true = actualValidation3)
print(paste("Validation MAPE:", mapeValidation3))

# Calculate MAPE for the test set using MLmetrics
mapeTest3 <- MAPE(y_pred = linearTestPredictions3, y_true = actualTest3)
print(paste("Test MAPE:", mapeTest3))


# Select the best model you have based on the RMSE score in the test set that is also fairly consistent among other data sections.
# Made a decision to go with Model 3

# Using the best possible model make predictions on the prospect set.
prospectPredictions <- predict(fit3, treatedProspects3)

# Column-bind the predictions to the original prospect dataset
finalProspectsWithPredictions <- cbind(allProspectDF, Prediction = prospectPredictions)

# View the first few rows of the dataset with predictions
head(finalProspectsWithPredictions)

# Save the dataset with predictions to a new CSV file
write.csv(finalProspectsWithPredictions, "Yoshiyuki_Watabe_A2_Household_Spend.csv", row.names = FALSE)


# End