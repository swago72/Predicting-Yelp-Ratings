rm(list = ls()) #clear Global Environment

#the path of the json file you want to access in your local computer
#file2 <- "yelp_academic_dataset_user.json"
#user <- jsonlite::stream_in(textConnection(readLines(file2, n=300000)), flatten = TRUE)


library(jsonlite)

#the path of the json file to access in your local computer
file <- "yelp_academic_dataset_business.json"

business <- jsonlite::stream_in(textConnection(readLines(file, n=300000)), flatten = TRUE)
business_dummy = business; #to have a reference original dataset

head(business,10)

# checking the count of NA variables in each column
na_counts <- colSums(is.na(business_dummy))
# Convert the vector to a data frame
na_counts_df <- data.frame(Column = names(na_counts), NA_Count = na_counts)
# Sort the data frame in descending order by "NA_Count"
na_counts_df <- na_counts_df[order(-na_counts_df$NA_Count), ]
# Display the sorted data frame
print(na_counts_df)


#Now, driopping the variables with the NA count > 120,000 and low significance
variables_to_drop <- c(
  "attributes.RestaurantsGoodForGroups",
  "attributes.ByAppointmentOnly",
  "attributes.Caters",
  "attributes.RestaurantsAttire",
  "attributes.GoodForMeal",
  "attributes.BusinessAcceptsBitcoin",
  "attributes.AcceptsInsurance",
  "attributes.BestNights",
  "attributes.CoatCheck",
  "attributes.HairSpecializesIn",
  "attributes.AgesAllowed",
  "attributes.Open24Hours",
  "attributes.DietaryRestrictions",
  "attributes.RestaurantsCounterService",
  "attributes.BYOBCorkage",
  "attributes.Ambience",
  "attributes.NoiseLevel",
  "hours.Monday","hours.Tuesday",
  "hours.Wednesday", "hours.Thursday",
  "hours.Friday", "hours.Saturday" ,
  "hours.Sunday"
)
# Drop the variables from the dataset
business <- business[, -which(names(business) %in% variables_to_drop)]

library(dplyr)
# Convert "None" to "False" in the "attributes.OutdoorSeating" column
business <- business %>%
  mutate(attributes.OutdoorSeating = ifelse(attributes.OutdoorSeating == "None", 
                                            FALSE, attributes.OutdoorSeating))
# Convert u'no', 'no' and u'free', 'free' to 1s and 0s
business <- business %>%
  mutate(attributes.WiFi = recode(attributes.WiFi, "u'free'" = 1, "'free'" = 1, 
                                  "u'no'" = 0, "'no'" = 0))

# Converting any means of True parking as 1s and rest 0s
business <- business %>%
  mutate(attributes.BusinessParking = as.integer(sapply(attributes.BusinessParking, function(x) {
    ifelse(is.na(x), NA, any(grepl("True", toString(x))))
  })))


# Assuming 'business' is your dataset
business <- business %>%
  mutate(attributes.Alcohol = ifelse(attributes.Alcohol %in% c("none", "u'none'"), 
                                     0, ifelse(!is.na(attributes.Alcohol), 1, NA)))

# Assuming 'business' is your dataset
unique_values <- unique(business$attributes.Smoking)
# Display the unique values
print(unique_values)
# Assuming 'business' is your dataset
business <- business %>%
  mutate(attributes.Smoking = ifelse(attributes.Smoking %in% c("u'no'", "None", "'no'"), 0,
                                     ifelse(attributes.Smoking %in% c("u'outdoor'", "u'yes'", "'outdoor'"), 1,
                                            attributes.Smoking)))

# Assuming 'business' is your dataset
business <- business %>%
  mutate(attributes.Music = as.integer(sapply(attributes.Music, function(x) {
    ifelse(is.na(x), NA, any(grepl("True", toString(x))))
  })))


#checking the column names
all_cols <- colnames(business)
all_cols
# Columns that are NOT logical
non_logical_cols <- c("business_id", "name", "address",  
                      "city", "state", "postal_code", "attributes.Smoking",
                      "latitude", "longitude", "stars", "attributes.WiFi",
                      "review_count", "categories", "attributes.Alcohol",
                      "attributes.RestaurantsPriceRange2", "attributes.Music",
                      "attributes.BusinessParking", "attributes.RestaurantsAttire",
                      "attributes.Ambience", "attributes.NoiseLevel", "attributes.BestNights",
                      "attributes.GoodForMeal", "attributes.HairSpecializesIn",
                      "hours.Monday", "hours.Tuesday", "hours.Wednesday", 
                      "hours.Thursday", "hours.Friday", "hours.Saturday", 
                      "hours.Sunday")


# Find logical columns by set difference 
logical_cols <- setdiff(all_cols, non_logical_cols)
# Print logical columns
print(logical_cols)

#Converting logical values to 1s and 0s
business[logical_cols] <- lapply(business[logical_cols], function(x) ifelse(x, 1, 0))

#taking a final look at the variables before modeling
names(business)


set.seed(123)  # Set seed for reproducibility
sample_size <- 10000
sample_data <- business[sample(nrow(business), sample_size), ]


# Define the target variable
target_variable <- "stars"
# Exclude unnecessary columns
exclude_variables <- c("business_id", "name", "address", "categories")
predictor_variables <- setdiff(names(sample_data), c(target_variable, exclude_variables))


library(VIM)
# Perform KNN imputation for missing values
sample_data_imputed <- kNN(sample_data[, predictor_variables], k = 5)
# Select only the original columns from the imputed dataset
sample_data_imputed <- sample_data_imputed[, predictor_variables]
# Adding the target variable back, if it's not already in the original columns
sample_data_imputed[[target_variable]] <- sample_data[[target_variable]]


library(randomForest)
# Create a random forest model on the sample data
rf_model_sample <- randomForest(sample_data_imputed[, predictor_variables], sample_data_imputed[, target_variable], ntree = 100)

#IncNodePurity
print(importance(rf_model_sample))
# Plot variable importance
varImpPlot(rf_model_sample)
# Drop the variables from the dataset
#sample_data_imputed <- sample_data_imputed[, !names(sample_data_imputed) %in% variables_to_drop]


# Split data into training and testing set
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(sample_data_imputed), 0.7 * nrow(sample_data_imputed))
train_data <- sample_data_imputed[train_index, ]
test_data <- sample_data_imputed[-train_index, ]

# Train the model
rf_model <- randomForest(x = train_data[, predictor_variables], y = train_data[, target_variable], ntree = 100)

# Predict on test data
predictions <- predict(rf_model, test_data[, predictor_variables])

# Calculate RMSE and MAE
rmse <- sqrt(mean((predictions - test_data[, target_variable])^2))
mae <- mean(abs(predictions - test_data[, target_variable]))

# Calculate R-squared
ss_res <- sum((predictions - test_data[, target_variable])^2)
ss_tot <- sum((mean(train_data[, target_variable]) - test_data[, target_variable])^2)
r_squared <- 1 - (ss_res / ss_tot)

# Print the metrics
cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r_squared)



########################################################################

library(randomForest)

# Manually create a k-fold cross-validation
set.seed(123) # for reproducibility
k <- 10
folds <- cut(seq(1, nrow(sample_data_imputed)), breaks=k, labels=FALSE)

# Initialize variables to store metrics
rmse_values <- c()
mae_values <- c()
r_squared_values <- c()

# Perform k-fold cross-validation
for(i in 1:k){
  # Splitting the data
  testIndexes <- which(folds == i, arr.ind=TRUE)
  testData <- sample_data_imputed[testIndexes, ]
  trainData <- sample_data_imputed[-testIndexes, ]
  
  # Train the model
  rf_model <- randomForest(x = trainData[, predictor_variables], y = trainData[, target_variable], ntree = 100)
  
  # Predict on test data
  predictions <- predict(rf_model, testData[, predictor_variables])
  
  # Calculate metrics
  rmse_values[i] <- sqrt(mean((predictions - testData[, target_variable])^2))
  mae_values[i] <- mean(abs(predictions - testData[, target_variable]))
  ss_res <- sum((predictions - testData[, target_variable])^2)
  ss_tot <- sum((mean(trainData[, target_variable]) - testData[, target_variable])^2)
  r_squared_values[i] <- 1 - (ss_res / ss_tot)
}

# Calculate average of metrics
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_r_squared <- mean(r_squared_values)

# Print the results
cat("Average RMSE:", mean_rmse, "\nAverage MAE:", mean_mae, "\nAverage R-squared:", mean_r_squared)


###############################################################################
# List of variables to drop < 100 IncNodePurity
exclude_variables <- c("attributes.BusinessAcceptsCreditCards", "attributes.BikeParking", 
                       "attributes.RestaurantsTakeOut", "attributes.WheelchairAccessible", 
                       "attributes.HappyHour", "attributes.OutdoorSeating", "attributes.HasTV", 
                       "attributes.RestaurantsReservations", "attributes.DogsAllowed", 
                       "attributes.GoodForKids", "attributes.RestaurantsTableService", 
                       "attributes.DriveThru", "attributes.GoodForDancing", "attributes.BYOB", 
                       "attributes.Corkage", "business_id", "name", "address", "categories")

business <- business[, !names(business) %in% exclude_variables]
names(business)

######################################
# Sum of NA values in each column
na_count_per_column <- sapply(business, function(x) sum(is.na(x)))

# Total NA values in the dataset
total_na_count <- sum(na_count_per_column)

print(na_count_per_column)
cat("Total NA values in the dataset:", total_na_count, "\n")


# Drop rows with any NA values
business_clean <- na.omit(business)

# Check the dimensions of the new dataset
cat("Dimensions of business dataset after dropping NAs:", dim(business_clean), "\n")
######################################

predictor_variables <- setdiff(names(business_clean), c(target_variable, exclude_variables)); predictor_variables
target_variable <- "stars"

set.seed(123)  # Set seed for reproducibility
sample_size <- 50000
sample_data <- business[sample(nrow(business), sample_size), ]


library(VIM)
# Perform KNN imputation for missing values
sample_data_imputed <- kNN(sample_data[, predictor_variables], k = 5)
# Select only the original columns from the imputed dataset
sample_data_imputed <- sample_data_imputed[, predictor_variables]
# Adding the target variable back, if it's not already in the original columns
sample_data_imputed[[target_variable]] <- sample_data[[target_variable]]


library(randomForest)
# Train the Random Forest model
rf_model <- randomForest(x = business_clean[, predictor_variables], 
                                y = business_clean[["stars"]], 
                                ntree = 100)

# Split data into training and testing set
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(business_clean), 0.7 * nrow(business_clean))
train_data <- business_clean[train_index, ]
test_data <- business_clean[-train_index, ]

# Train the model
#rf_model <- randomForest(x = train_data[, predictor_variables], y = train_data[, target_variable], ntree = 100)

# Predict on test data
predictions <- predict(rf_model, test_data[, predictor_variables])

# Calculate RMSE and MAE
rmse <- sqrt(mean((predictions - test_data[, target_variable])^2))
mae <- mean(abs(predictions - test_data[, target_variable]))

# Calculate R-squared
ss_res <- sum((predictions - test_data[, target_variable])^2)
ss_tot <- sum((mean(train_data[, target_variable]) - test_data[, target_variable])^2)
r_squared <- 1 - (ss_res / ss_tot)

# Print the metrics
cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r_squared)

# Print model summary
print(rf_model)
summary(rf_model)


#IncNodePurity
print(importance(rf_model))
# Plot variable importance
varImpPlot(rf_model)
# Drop the variables from the dataset
#sample_data_imputed <- sample_data_imputed[, !names(sample_data_imputed) %in% variables_to_drop]















###########################################################################

#Trying to work on logical columns separately

logical <- c("is_open", "BusinessAcceptsCreditCards", "BikeParking",  
             "RestaurantsTakeOut", "RestaurantsDelivery", "WiFi",
             "BusinessParking", "WheelchairAccessible", "HappyHour",
             "OutdoorSeating", "HasTV", "RestaurantsReservations", 
             "DogsAllowed", "Alcohol", "GoodForKids", "RestaurantsTableService",
             "DriveThru", "Smoking", "Music", "GoodForDancing", 
             "BYOB", "Corkage")

business_logical <- business_imputed %>% 
  select(all_of(logical))

# Define the target variable
target_variable <- "stars"
predictor_variables <- logical; predictor_variables

# Train the Random Forest model
rf_model <- randomForest(x = business_imputed[, predictor_variables], 
                         y = business_imputed[["stars"]], 
                         ntree = 100)

# Split data into training and testing set
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(business_imputed), 0.7 * nrow(business_imputed))
train_data <- business_imputed[train_index, ]
test_data <- business_imputed[-train_index, ]


# Predict on test data
predictions <- predict(rf_model, test_data[, predictor_variables])

# Calculate RMSE and MAE
rmse <- sqrt(mean((predictions - test_data[, target_variable])^2))
mae <- mean(abs(predictions - test_data[, target_variable]))

# Calculate R-squared
ss_res <- sum((predictions - test_data[, target_variable])^2)
ss_tot <- sum((mean(train_data[, target_variable]) - test_data[, target_variable])^2)
r_squared <- 1 - (ss_res / ss_tot)

# Print the metrics
cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r_squared)

# Print model summary
print(rf_model)
summary(rf_model)


####################################################################
#Non logical columns
target_variable <- "stars"

predictor_variables <- setdiff(names(business_imputed), c(logical,"stars")); predictor_variables

business_logical <- business_imputed %>% 
  select(all_of(predictor_variables)); business_logical


# Train the Random Forest model
rf_model <- randomForest(x = business_imputed[, predictor_variables], 
                         y = business_imputed[["stars"]], 
                         ntree = 100)

# Split data into training and testing set
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(business_imputed), 0.7 * nrow(business_imputed))
train_data <- business_imputed[train_index, ]
test_data <- business_imputed[-train_index, ]


# Predict on test data
predictions <- predict(rf_model, test_data[, predictor_variables])

# Calculate RMSE and MAE
rmse <- sqrt(mean((predictions - test_data[, target_variable])^2))
mae <- mean(abs(predictions - test_data[, target_variable]))

# Calculate R-squared
ss_res <- sum((predictions - test_data[, target_variable])^2)
ss_tot <- sum((mean(train_data[, target_variable]) - test_data[, target_variable])^2)
r_squared <- 1 - (ss_res / ss_tot)

# Print the metrics
cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r_squared)

# Print model summary
print(rf_model)
summary(rf_model)


