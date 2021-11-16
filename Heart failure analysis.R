
##########     Heart failure analysis     #########
##########         Jody Iabichino         #########
###################################################

#DATA PREPARATION
#First of all, we start by loading the libraries we may need in the course of the analysis.

if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


library(utils)
library(tidyverse)
library(lubridate)
library(caret)
library(rpart)
library(randomForest)

# Now let's start downloading the database in R. We have chosen to use Heart failure clinical records Data Set.
#This dataset contains the medical records of 299 patients who had heart failure, collected during their follow-up period, where each patient profile has 13 clinical features.

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"
path <- file.path("~", "heart_failure_clinical_records_dataset.csv")
download.file(url, path)
dataset <- read.table("heart_failure_clinical_records_dataset.csv", 
                      header = TRUE, 
                      sep = ",",
                      stringsAsFactors = FALSE)


# DATA EXPLORATION
#We begin the exploration of the data by first looking at the structure of the database and then analyzing the first few rows.

str(dataset)
head(dataset)

# Data cleaning
# We check if there are missing values in the dataset

sum(is.na(dataset)) # --> no missing value

#Let's now analyze the variables that make up the database. Let's start with the target variable: death event.

#1. Death_event

table(dataset$DEATH_EVENT) #<-- the survived patients (death event = 0) are 203, while the dead patients (death event = 1) are 96.


#2. Serum creatinine

dataset %>%
  summarize(min_serum = min(serum_creatinine),
            max_serum = max(serum_creatinine)) # --> serum_creatinine range from 0.5 to 9.4

dataset <- dataset %>% 
  mutate(serum_range = as.factor(case_when(serum_creatinine <= 1.5 ~ "<=1.5",
                                           serum_creatinine >1.5 & serum_creatinine <=3 ~ "1.5-3",
                                           serum_creatinine >3 & serum_creatinine <=4.5 ~ "3-4.5",
                                           serum_creatinine >4.5 & serum_creatinine <=6 ~ "4.5-6",
                                           serum_creatinine >6 ~ ">6"
  ))) #<-- We group the values of the variable into bands so that we can better analyze its behavior.

table(dataset$serum_range, dataset$DEATH_EVENT)#<- Frequencies show that for high values of the variable, there is a higher probability that the patient will die.


#3. Ejection_fraction

dataset %>%
  summarize(min_eje = min(ejection_fraction),
            max_eje = max(ejection_fraction)) # --> ejection_fraction range from 14 to 80

dataset <- dataset %>% 
  mutate(eje_range = as.factor(case_when(ejection_fraction <= 20 ~ "<=20",
                                         ejection_fraction >20 & ejection_fraction <=40 ~ "20-40",
                                         ejection_fraction >40 & ejection_fraction <=60 ~ "40-60",
                                         ejection_fraction >60 & ejection_fraction <=80 ~ "60-80",
                                         ejection_fraction >80 ~ ">80"
  ))) #<-- We group the values of the variable into bands so that we can better analyze its behavior.

table(dataset$eje_range, dataset$DEATH_EVENT) #<- Frequencies show that for low values of the variable, there is a higher probability that the patient will die.

#4. Age
table(dataset$age, dataset$DEATH_EVENT) # --> the patients are between 40 and 95 years old

dataset <- dataset %>% 
  mutate(age_range = as.factor(case_when(age <= 50 ~ "<=50",
                                          age >50 & age<=60 ~ "51-60",
                                          age >60 & age<=70 ~ "61-70",
                                          age >70 & age<=80 ~ "71-80",
                                          age>=81 ~ "Over 80"
         )))#<-- We group the values of the variable into bands so that we can better analyze its behavior.

table(dataset$age_range, dataset$DEATH_EVENT) #--> Frequencies show that only in case of patients with more than 70 years old, there is an higher probability of die


#5. Diabetes

table(dataset$diabetes, dataset$DEATH_EVENT) # --> Deaths are equally distributed between patients with and without diabetes

#6. Anaemia

table(dataset$anaemia, dataset$DEATH_EVENT) # --> Deaths are equally distributed between patients with and without anaemia

#7. Sex

table(dataset$sex, dataset$DEATH_EVENT) # --> Deaths are equally distributed between women and men

#8. Creatinine_phosphokinase

table(dataset$creatinine_phosphokinase, dataset$DEATH_EVENT) # --> Deaths are equally distributed between patients with high and low creatinine phosphokinase levels

#9. High_blood_pressure

table(dataset$high_blood_pressure, dataset$DEATH_EVENT) # --> Deaths are equally distributed between patients with high blood pressure (37%) and patients with low blood pressure (29%)

#10. Platelets

table(dataset$platelets, dataset$DEATH_EVENT) # --> Deaths are equally distributed between patients with high and low platelets levels

#11. Serum_sodium

table(dataset$serum_sodium, dataset$DEATH_EVENT) # --> The most important number of survived patients are between 134 and 141

#12. Smoking

table(dataset$smoking, dataset$DEATH_EVENT) # --> Smoking patients have a similar probability to die (31%) respect to no-smoking patients (32%)

#13. Time

table(dataset$time, dataset$DEATH_EVENT) 

dataset <- dataset %>% 
  mutate(time_range = as.factor(case_when(time <= 50 ~ "<=50",
                                         time >50 & time <=100 ~ "50-100",
                                         time >100 & time <=150 ~ "100-150",
                                         time >150 & time <=200 ~ "150-200",
                                         time >200 & time <=250 ~ "200-250",
                                         time >250 ~ ">250"
  )))

table(dataset$time_range, dataset$DEATH_EVENT) #<--analyzing the time variable, no particular trends are found 

# DATA VISUALIZATION
# Given the importance of the first two variables in the explanation of death_events, let's try to perform also a graphical analysis through data visualization.

ggplot(data = dataset, aes(x = serum_creatinine, y = DEATH_EVENT)) + 
  geom_point() +
  labs(x = "Serum creatinine", 
       y = "Death") +
  ggtitle("Count of death/ Serum creatinine")

ggplot(data = dataset, aes(x = ejection_fraction, y = DEATH_EVENT)) + 
  geom_point() +
  labs(x = "ejection fraction", 
       y = "Death") +
  ggtitle("Count of death/ ejection fraction")


#DATA PARTITION

#Based on the indications from the exploratory analysis, we select only the variables relevant to machine learning. 
#Specifically, we select death_events (which is our target variable), serum_creatinine, and ejection_fraction, discarding all others that make up the database.

dataset_selection <- dataset %>%
  select(DEATH_EVENT, serum_creatinine, ejection_fraction)

# Now, we split data into a train set ("dataset_selection_train") and a test set ("dataset_selection_test"):

set.seed(1)
# if using R 3.6 or later, use set.seed(1, sample.kind="Rounding")
index<-createDataPartition(dataset$DEATH_EVENT,times=1,p=0.3,list=FALSE)
dataset_selection_train <- dataset_selection[index,] #<-- training set
dataset_selection_test <- dataset_selection[-index,] #<-- validation set


# MACHINE LEARNING MODELS
# Prepared the training-set, the task is to predict DEATH_EVENT. We use like first model a knn, then a random forest and a logistic regression.

#1. KNN
knn_fit <- knn3(DEATH_EVENT ~ ., 
                data = dataset_selection_train, k=5) #<-- We estimate parameters on the trainining set, dividing it into k (5) groups.

y_hat_knn <- predict(knn_fit, dataset_selection_test, type = "prob") #<-- At this point we estimate the death_events on the validation set 


#2. Random forest

rf_fit <- randomForest(DEATH_EVENT ~ .,data = dataset_selection_train) #<-- We estimate parameters on the trainining set

plot(rf_fit) # --> 300 trees are sufficient for explaining most of variability

y_hat_rf <- predict(rf_fit, newdata = dataset_selection_test) # <-- At this point we estimate the death_events on the validation set


#3. Logistic Regression

log_fit <-glm(DEATH_EVENT ~ .,data = dataset_selection_train, family = "binomial") #<-- We estimate parameters on the trainining set
y_hat_log <- predict(log_fit, newdata = dataset_selection_test, type = "response") # <-- At this point we estimate the death_events on the validation set


# RESULTS: RMSES and model performance

#For each of the three applied methods, we go on to estimate the RMSE, and then make a comparison:

rmses <- dataset_selection_test %>%
  mutate(residual_knn = y_hat_knn - DEATH_EVENT,
         residual_rf = y_hat_rf - DEATH_EVENT,
         residual_log = y_hat_log - DEATH_EVENT) %>%
  summarize(rmse_knn_model = sqrt(mean(residual_knn^2)),
            rmse_rf_model = sqrt(mean(residual_rf^2)),
            rmse_log_model = sqrt(mean(residual_log^2)),
            ) 

rmses 

# CONCLUSIONS: Logistic Regression performs better than KNN and Random Forest. Indeed, its rmse is the lowest.
#We can use it for death event prediction.






