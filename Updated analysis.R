
# 0. IMPORT DATA -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(vtree)
library(corrplot)
# install.packages("caTools")
library(caTools)

data <- read_excel("telecom_customer_churn_edited.xlsx")


# 0. DATA STRUCTURE --------------------------------------------------------------
## Remove spaces in variable names
names(data) <- gsub(" ", 
                    "", 
                    names(data))  # Remove spaces
## To check the data type of all variables
sapply(data,class) 

## To see a summary of the dataframe structure
str(data)

## To see descriptive stats
summary(data)

## Convert character vectors to factors
convert_chars_to_factors <- function(data) {
  char_vars <- sapply(data, is.character)  # Identify character variables
  data[char_vars] <- lapply(data[char_vars], as.factor)  # Convert to factors
  return(data)
}

data <- convert_chars_to_factors(data)

# str(data)

# Set scipen to a higher value
options(scipen = 10)

# Replace 'NA' values
data$MultipleLines[is.na(data$MultipleLines)] <- "No"
data$InternetService[is.na(data$InternetService)] <- "No"
data$InternetType[is.na(data$InternetType)] <- "No"
data$AvgMonthlyGBDownload[is.na(data$AvgMonthlyGBDownload)] <- "No"
data$OnlineSecurity[is.na(data$OnlineSecurity)] <- "No"
data$OnlineBackup[is.na(data$OnlineBackup)] <- "No"
data$DeviceProtectionPlan[is.na(data$DeviceProtectionPlan)] <- "No"
data$PremiumTechSupport[is.na(data$PremiumTechSupport)] <- "No"
data$StreamingTV[is.na(data$StreamingTV)] <- "No"
data$StreamingMusic[is.na(data$StreamingMusic)] <- "No"
data$StreamingMovies[is.na(data$StreamingMovies)] <- "No"
data$UnlimitedData[is.na(data$UnlimitedData)] <- "No"

# 1. EXPLORATORY DATA ANALYSIS --------------------------------------------

## 1.1 Profile of those who churned by gender ------------------------------
data %>%
  group_by(Gender) %>% # Of this gender
  count(CustomerStatus) %>%  # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

vtree(data,c("Gender","CustomerStatus"))


## 1.2 Profile of those who churned by age ------------------------------
data %>%
  group_by(Age) %>% # of this age 
  count(CustomerStatus) %>% # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

vtree(data,c("Age","CustomerStatus"))

# To check min/max of Age
summary(data$Age)

data <- data %>%
  mutate(Age4 = case_when(
    Age < 25 ~ "Below 25 y/o",
    Age >= 25 & Age < 40 ~ "25 to 39 y/o",
    Age >= 40 & Age < 55 ~ "40 to 54 y/o",
    Age >= 55 ~ "55 and above"
  ))

# To convert character vector to factor
data$Age4 <- as.factor(data$Age4)

# Check if data type has changed
class(data$Age4)

### 1.2.1 To check age recoding ---------------------------------------------
# Define the input age
inputage <- 24

# Define the conditions and corresponding Age4 values
if (inputage >= 25 & inputage <= 39) {
  Age4 <- "25 to 39 y/o"
} else if (inputage >= 40 & inputage <= 54) {
  Age4 <- "40 to 54 y/o"
} else if (inputage < 25) {
  Age4 <- "Below 25"
} else {
  Age4 <- "55 and above"
}

# Print the result
cat("For inputage =", inputage, ", Age4 =", Age4, "\n")

data %>%
  group_by(Age4) %>% # of this age 
  count(CustomerStatus) %>% # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

vtree(data,c("Age4","CustomerStatus"))

# Chi-square between CustomerStatus and Age
chisq_result <- chisq.test(data$CustomerStatus, data$Age4)

# Print the test result
print(chisq_result)

# Column prop test of "55 y/o" and 25-39
prop.test(x=c(455,778), n=c(1888,2413), alternative="two.sided")
 
# Column prop test of "55 y/o" and between 40-54
prop.test(x=c(478,778), n=c(1963,2413), alternative="two.sided")

# Column prop test of "55 y/o" and below 20.
prop.test(x=c(158,778), n=c(779,2413), alternative="two.sided")


## 1.3 Profile of churn by contract type -----------------------------------
data %>%
  group_by(Contract) %>% # of this contract type 
  count(CustomerStatus) %>% # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

vtree(data,c("Contract","CustomerStatus"))


vtree(data,c("Age4","Contract","CustomerStatus"),
      horiz=F,
      keep = list(CustomerStatus = "Churned"))


# 2. LOGISTIC REGRESSION --------------------------------------------------
# Create binary dependent variable
data_log <- data%>%
  mutate(churned = recode(CustomerStatus,
                          "Churned" = 1,
                          "Joined"=0,
                          "Stayed"=0))

# Check if recoded correctly
data_log %>%
  group_by(churned) %>% # of this contract type 
  count(CustomerStatus) %>% # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

# Remove unnecessary variables
data_clean <-data_log[,!names(data_log) %in% c("CustomerID", "Latitude", "Longitude", "Age","ZipCode","ChurnCategory", "ChurnReason")]

# Create training and testing set
set.seed(7474)

# TO see how many rows are there
nrow(data_clean)

# To create an 'index' to split by
n <- sample.split(data_clean$churned,SplitRatio = 0.30)

train_data <- subset(data_clean, n== TRUE)

test_data <- subset(data_clean,n==F)


## 2.1 MODEL ONE ---------------------------------------------------------
modelA1 <- glm(churned ~ Offer + Gender + Age4 +Married, data= train_data,
                   family="binomial")

summary(modelA1)

# To get the odds since the model shows log-odds
exp(coef(modelA1))

# Predict probability using test data
pred.probs <- predict.glm(modelA1,           # trained model
                          newdata = test_data[,-33], # using testing data set; dropping last col
                          type = "response")
pred.probs

pred <- ifelse(pred.probs<0.5,"no","yes")
pred

# Metric 1: Confusion matrix
table(test_data$churned, pred)

# Metric 2: Sensitivity (True positive)
sensitivity <- sum(ifelse(pred.probs > 0.5 & test_data$churned == 1, 1, 0)) / sum(test_data$churned == 1)
sensitivity
## Sensitivity = True Positive/False Negative = [294/(294 + 1018)]

# Metric 3: Specificity (True negative)
specificity <- sum(ifelse(pred.probs <= 0.5 & test_data$churned == 0, 1, 0)) / sum(test_data$churned == 0)
specificity
## Specificity = True N/True N + False Positive [(]3357/(3357+262)]

# Metric 4: ROC Curve
require(ROCR)
pr <- prediction(pred.probs, test_data$churned)

perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC Curve (A1)")

# Area under ROC curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Metric 5: AIC
aicA1 <- AIC(modelA1)
aicA1


## 2.2 MODEL TWO -----------------------------------------------------
# Create training and testing set
set.seed(7474)

modelA2 <- glm(churned ~ Offer + Gender + Age4 +Married + NumberofReferrals + TenureinMonths +
               InternetService + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup +
                 DeviceProtectionPlan + PremiumTechSupport + StreamingTV + StreamingMovies +
                 StreamingMusic + UnlimitedData,
               data= train_data, family = "binomial")


summary(modelA2)

# To get the odds since the model shows log-odds
exp(coef(modelA2))

# Predict probability using test data
pred.probs <- predict.glm(modelA2,           # trained model
                          newdata = test_data[,-33], # using testing data set; dropping last col
                          type = "response")
pred.probs

pred <- ifelse(pred.probs<0.5,"no","yes")
pred

# Metric 1: Confusion matrix
table(test_data$churned, pred)

# Metric 2: Sensitivity (True positive)
sensitivity <- sum(ifelse(pred.probs > 0.5 & test_data$churned == 1, 1, 0)) / sum(test_data$churned == 1)
sensitivity
## Sensitivity = True Positive/False Negative = [720/(720 + 588)]

# Metric 3: Specificity (True negative)
specificity <- sum(ifelse(pred.probs <= 0.5 & test_data$churned == 0, 1, 0)) / sum(test_data$churned == 0)
specificity
## Specificity = True N/True N + False Positive = [3233/(3233+389)]

# Metric 4: ROC Curve
require(ROCR)
pr <- prediction(pred.probs, test_data$churned)

perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC Curve (A2)")

# Area under ROC curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Metric 5: AIC
aicA2 <- AIC(modelA2)
aicA2

# 3. REASONS FOR CHURN ----------------------------------------------------
data %>%
  group_by(CustomerStatus) %>% # of this contract type 
  count(ChurnCategory) %>% # how many churned?
  mutate(pct_Type = scales::percent(n/sum(n)))

vtree(data,c("CustomerStatus","ChurnCategory"),
      keep = list(CustomerStatus = "Churned"))


vtree(data,c("Age4","Contract","CustomerStatus"),
      horiz=F,
      keep = list(CustomerStatus = "Churned"))


