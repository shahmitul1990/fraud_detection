#### Fraud Detection

## Loading the required libraries
require(dplyr)
require(randomForest)
require(ROCR)

## Load the data
data = read.csv("Fraud/Fraud_Data.csv")
ip_addresses = read.csv("Fraud/IpAddress_to_Country.csv")

# Are there any duplicates?
nrow(data) == length(unique(data$user_id))

## Adding country to the original dataset by using the ip address
data_country = rep(NA, nrow(data))
for (i in 1:nrow(data)) {
  tmp = as.character(ip_addresses [data$ip_address[i] >= ip_addresses$lower_bound_ip_address
                                   & data$ip_address[i] <= ip_addresses$upper_bound_ip_address,
                                   "country"])
  if (length(tmp) == 1)  {data_country[i] = tmp}
}

data$country <- data_country

## Looking at the summary of the countries
summary(as.factor(data$country))

## Changing the mode of sign up time and purchase time
data[, "signup_time"] = as.POSIXct(data[, "signup_time"], tz="GMT")
data[, "purchase_time"] = as.POSIXct(data[, "purchase_time"], tz="GMT")

## Creating a variable time difference between purchase time and signup time
data$purchase_signup_diff = as.numeric(difftime(as.POSIXct(data$purchase_time, tz="GMT"), as.POSIXct(data$signup_time, tz="GMT"), unit="secs"))

## Check for each device id how many different users had it
data = data %>%
  group_by(device_id) %>%
  mutate (device_id_count = n())

## Check for each ip address how many different users had it
data = data.frame(data %>%
                    group_by(ip_address) %>%
                    mutate (ip_address_count = n())
)

## Day of the week
data$signup_time_wd  = format(data$signup_time, "%A")
data$purchase_time_wd = format(data$purchase_time, "%A" )

## Week of the yr
data$signup_time_wy   = as.numeric(format(data$signup_time, "%U"))
data$purchase_time_wy = as.numeric(format(data$purchase_time, "%U" ))

## Data set for the model. Drop first 3 vars and device id.
data_rf = data[, -c(1:3, 5)]

## Replace the NA in the country var
data_rf$country[is.na(data_rf$country)] = "Not_found"

## Just keep the top 50 country, everything else is "other"
data_rf$country = ifelse(data_rf$country %in% names(sort(table(data_rf$country), 
                         decreasing = TRUE ))[51:length(unique(data_rf$country))], 
                         # after top 50 countries
                         "Other", as.character(data_rf$country)
)

## Make class a factor
data_rf$class = as.factor(data_rf$class)

## All characters become factors
data_rf[sapply(data_rf, is.character)] <- lapply(data_rf[sapply(data_rf, 
                                           is.character)], as.factor)

## Train/test split
train_sample = sample(nrow(data_rf), size = nrow(data)*0.66)
train_data = data_rf[train_sample,]
test_data = data_rf[-train_sample,]

## Random Forest model
rf <- randomForest(y = train_data$class, x = train_data[, -7],
             ytest = test_data$class, xtest = test_data[, -7],
             ntree = 50, mtry = 3, keep.forest = TRUE)

## View the model
rf

## Combining model predictions and the actual values in one dataframe
rf_results = data.frame (true_values = test_data$class,
                         predictions = rf$test$votes[,2]
)

## Comparing rf predictions with rf model having threshold of 0.5
identical(as.numeric(as.character(rf$test$predicted)),
           ifelse (rf_results$predictions > 0.5, 1, 0))
                                                                
pred = prediction(rf_results$predictions, rf_results$true_values)

## Plotting the ROC curve
perf = performance(pred, measure = 'tpr', x.measure = "fpr")
plot(perf) + abline(a=0, b=1, col = 'red')


