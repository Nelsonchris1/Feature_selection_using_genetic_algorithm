
library(dplyr)
library(DMwR)

preprocess_data <- function(){
  df = read.csv('weatherAUS.csv', header=TRUE)
  new_df = subset(df, Location == "Sydney")
  
  # Convert the Date column to date format and extract year, month, and day columns
  new_df$Date <- as.Date(new_df$Date)
  new_df$year <- format(new_df$Date, "%Y")
  new_df$month <- format(new_df$Date, "%m")
  new_df$day <- format(new_df$Date, "%d")
  
  # Remove unwanted columns
  
  new_df <- new_df[, -c(1,2, 8, 9)]
  
  # Replace missing values with the rounded mean of the respective column
  new_df$Evaporation[is.na(new_df$Evaporation)] <- round(mean(new_df$Evaporation, na.rm = TRUE))
  new_df$Sunshine[is.na(new_df$Sunshine)] <- round(mean(new_df$Sunshine, na.rm = TRUE))
  new_df$Cloud9am[is.na(new_df$Cloud9am)] <- round(mean(new_df$Cloud9am, na.rm = TRUE))
  new_df$Cloud3pm[is.na(new_df$Cloud3pm)] <- round(mean(new_df$Cloud3pm, na.rm = TRUE))
  new_df$WindSpeed9am[is.na(new_df$WindSpeed9am)] <- round(mean(new_df$WindSpeed9am, na.rm = TRUE))
  new_df$WindSpeed3pm[is.na(new_df$WindSpeed3pm)] <- round(mean(new_df$WindSpeed3pm, na.rm = TRUE))
  new_df$Humidity9am[is.na(new_df$Humidity9am)] <- round(mean(new_df$Humidity9am, na.rm = TRUE))
  new_df$Humidity3pm[is.na(new_df$Humidity3pm)] <- round(mean(new_df$Humidity3pm, na.rm = TRUE))
  new_df$Pressure9am[is.na(new_df$Pressure9am)] <- round(mean(new_df$Pressure9am, na.rm = TRUE))
  new_df$Pressure3pm[is.na(new_df$Pressure3pm)] <- round(mean(new_df$Pressure3pm, na.rm = TRUE))
  new_df$Temp9am[is.na(new_df$Temp9am)] <- round(mean(new_df$Temp9am, na.rm = TRUE))
  new_df$Temp3pm[is.na(new_df$Temp3pm)] <- round(mean(new_df$Temp3pm, na.rm = TRUE))
  
  # Remove any rows containing missing values
  new_df <- na.omit(new_df)
  
  # Convert categorical variables to factors
  new_df$WindDir9am <- factor(new_df$WindDir9am)
  new_df$WindDir3pm <- factor(new_df$WindDir3pm)
  new_df$year <- factor(new_df$year)
  new_df$month <- factor(new_df$month)
  new_df$day <- factor(new_df$day)
  
  # Move the target column 'RainTomorrow' to the end of the dataframe
  new_df = select(new_df, -RainTomorrow, everything())
  
  #. Convert RainToday and RainTomorrow columns to factors with levels "No" and "Yes"
  new_df$RainToday <- factor(new_df$RainToday, levels = c("No", "Yes"))
  new_df$RainTomorrow <- factor(new_df$RainTomorrow, levels = c("No", "Yes"))
  
  return (new_df)
}


# Define a normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalize_and_split <- function(){
  new_df <- preprocess_data()
  # Normalize the numerical columns of new_df
  df_norm =as.data.frame(lapply(new_df[,c(1:5,8:17)], normalize))
  df1_r <- new_df[c(6, 7, 18, 19, 20, 21,22)]
  final_df <- cbind(df_norm, df1_r)
  # Split data into train and test
  df_train <- final_df[1:2444, ]
  df_test <- final_df[2445:3259, ]
  # Use SMOTE to oversample training data
  oversampledData <- SMOTE(df_train[, -RainTomorrow], df_train[, -RainTomorrow], k = 5)
  
  # Combine oversampled data with original training data
  df_train <- rbind(df_train, oversampledData)
  write.csv(df_train, "df_train.csv", row.names = FALSE)
  write.csv(df_test, "df_test.csv", row.names = FALSE)
}


