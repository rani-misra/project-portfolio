library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(fastDummies)
library(corrplot)

# Reads the Raw_Data and Calendar worksheets of RawData.xlsx into R
raw_data <- read_xlsx("RawData.xlsx")
calendar <- read_xlsx("RawData.xlsx", 
                      sheet = "Calendar")

# Converts Receipt Date into POSIXct format; adds Quarter and Year columns to Raw_Data
raw_data <- raw_data %>%
  mutate(`Receipt Date` = as.POSIXct(`Receipt Date`),
         `Ship Date` = as.POSIXct(`Ship Date`),
         Quarter = NA,
         Year = NA)

# Converts Start and End Date into POSIXct format
calendar <- calendar %>%
  mutate(Start_Date = as.POSIXct(Start_Date),
         End_Date = as.POSIXct(End_date))

# Loops through each row in Raw_Data sheet
for (i in 1:nrow(raw_data)) {
  receipt_date <- raw_data[i, 'Receipt Date']

# Finds matching date range in Calendar sheet
match <- which(receipt_date >= calendar$Start_Date & receipt_date <= calendar$End_date)

# If a match is found, assign Quarter and Year values
if (length(match) > 0) {
    raw_data$Quarter[i] <- calendar$Quarter[match[1]]
    raw_data$Year[i] <- calendar$Year[match[1]]
  }
}

#  Calculates In-transit Lead Time and Manufacturing Lead Time for each row in the joined dataset  
raw_data$'In-transit Lead Time' <- raw_data$`Receipt Date` - raw_data$`Ship Date`
raw_data$'Manufacturing Lead Time' <- raw_data$`Ship Date` - raw_data$`PO Download Date`

# Function created to convert seconds into days
seconds_to_D <- function(secs) {
  d <- (secs / (24*60*60))
  as.numeric(d)
}

# In-transit Lead Time and Manufacturing Lead Time converted from seconds to days
raw_data$'In-transit Lead Time' <- raw_data$'In-transit Lead Time' %>% 
                                    as.numeric() %>%
                                    seconds_to_D()
  
raw_data$'Manufacturing Lead Time' <- raw_data$'Manufacturing Lead Time' %>% 
                                        as.numeric() %>%
                                        seconds_to_D()
  
# Finds observations that contains missing values
colSums(is.na(raw_data))

# Replaces missing values with the mean lead time
raw_data[is.na(raw_data$'In-transit Lead Time'),9] <- round(mean(raw_data$'In-transit Lead Time', na.rm = T))
raw_data[is.na(raw_data$'Manufacturing Lead Time'),10] <- round(mean(raw_data$'Manufacturing Lead Time', na.rm = T))

# Removes all negative values
raw_data <- raw_data[raw_data$`In-transit Lead Time` >= 0 & raw_data$`Manufacturing Lead Time` >= 0, ]

# Finds the outliers
transit_stats <- boxplot.stats(raw_data$`In-transit Lead Time`)
transit_outliers <- transit_stats$out
manufacturing_stats <- boxplot.stats(raw_data$`Manufacturing Lead Time`)
manufacturing_outliers <- manufacturing_stats$out

# Sets threshold values for each column
threshold_transit <- round(mean(transit_outliers))
threshold_manufacturing <- round(mean(manufacturing_outliers))

# Replace values over threshold with mean for each column separately
raw_data$'In-transit Lead Time'[raw_data$'In-transit Lead Time' > threshold_transit] <- mean(raw_data$'In-transit Lead Time'[raw_data$'In-transit Lead Time' <= threshold_transit])
raw_data$'Manufacturing Lead Time'[raw_data$'Manufacturing Lead Time' > threshold_manufacturing] <- mean(raw_data$'Manufacturing Lead Time'[raw_data$'Manufacturing Lead Time' <= threshold_manufacturing])

# Adds Manufacturing Lead Time to PO Download Date for missing values to find Start Date
raw_data$`PO Download Date` <- as.Date(raw_data$`PO Download Date`, format = "%Y-%m-%d")
raw_data$`Ship Date`[is.na(raw_data$`Ship Date`)] <- as.character(as.Date(raw_data$`PO Download Date`, format = "%Y-%m-%d") + raw_data$`Manufacturing Lead Time`)
raw_data$`Ship Date` <- as.Date(raw_data$`Ship Date`, format = "%Y-%m-%d")

# Adds In-transit Lead Time to Ship Date for missing values to find Receipt Date
raw_data$`Ship Date` <- as.Date(raw_data$`Ship Date`, format = "%Y-%m-%d")
raw_data$`Receipt Date`[is.na(raw_data$`Receipt Date`)] <- as.character(as.Date(raw_data$`Ship Date`, format = "%Y-%m-%d") + raw_data$`In-transit Lead Time`)
raw_data$`Receipt Date` <- as.Date(raw_data$`Receipt Date`, format = "%Y-%m-%d")

# Matches Quarter and Year for all missing values 
raw_data$'Receipt Date' <- as.POSIXct(raw_data$'Receipt Date')
for (i in 1:nrow(raw_data)) {
  receipt_date <- raw_data[i, 'Receipt Date']
  match <- which(receipt_date >= calendar$Start_Date & receipt_date <= calendar$End_date)
  if (length(match) > 0) {
    raw_data$Quarter[i] <- calendar$Quarter[match[1]]
    raw_data$Year[i] <- calendar$Year[match[1]]
  }
}

# Plots In-transit Lead Time vs LOB
ggplot(raw_data, aes(x = LOB, y = `In-transit Lead Time`)) +
  geom_boxplot() +
  labs(x = "LOB", y = "In-transit Lead Time (days)")

# Plots In-transit Lead Time vs Origin
ggplot(raw_data, aes(x = Origin, y = `In-transit Lead Time`)) +
  geom_boxplot() +
  labs(x = "Origin", y = "In-transit Lead Time (days)")

# Plots In-transit Lead Time vs Ship Mode
ggplot(raw_data, aes(x = `Ship Mode`, y = `In-transit Lead Time`)) +
  geom_boxplot() +
  labs(x = "Ship Mode", y = "In-transit Lead Time (days)")

# Plots Quarter & In-transit Lead Time in Boxplot format
ggplot(raw_data, aes(Quarter, `In-transit Lead Time`)) +
  geom_boxplot()

# Plots Year & In-transit Lead Time in Boxplot format
ggplot(raw_data, aes(Year, `In-transit Lead Time`)) +
  geom_boxplot()

# Constructs appropriate numeric dataset for correlation/regression analysis
raw_data_dummy <- raw_data %>% 
  na.omit() %>%
  dummy_cols(select_columns = c("LOB", "Origin", "Ship Mode", "Quarter"), 
             remove_first_dummy = TRUE) %>%
  mutate(NumDownload = as.numeric(`PO Download Date`), 
         NumShip = as.numeric(`Ship Date`), 
         NumReceipt = as.numeric(`Receipt Date`)) %>%
  select(-c("LOB", "Origin", "Ship Mode", "Quarter", "PO Download Date", 
            "Ship Date", "Receipt Date")) 

# Summarizes and calculates correlation for dummy dataset
corr_dummy <- cor(raw_data_dummy)
corr_dummy

# Creates correlation plot for dummy dataset
corrplot(corr_dummy, type = "lower", method = "ellipse")

# Linear models composed of all dummy variables from raw data predicting In-transit Lead Time
summary(lm(`In-transit Lead Time` ~ `LOB_Product B` + `LOB_Product C`, data=raw_data_dummy))

summary(lm(`In-transit Lead Time` ~ `Origin_Site B` + `Origin_Site C` + `Origin_Site D`, data=raw_data_dummy))

summary(lm(`In-transit Lead Time` ~ `Ship Mode_FASTBOAT` + `Ship Mode_GROUND` + `Ship Mode_OCEAN`, data=raw_data_dummy))
