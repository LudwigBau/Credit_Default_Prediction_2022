# Code to solve Empirical.Assignment in Seminar (Topics in Fintech)
# Author: Ludwig Baunach 
# Date: 27.03.2022 - 24.04.2022


# Clear R
remove(list = ls()) # Remove existing data
cat("\014")  # Clear console

# Load packages
library(stargazer)
library(creditmodel)
library(tidyverse)
library(rsample)   
library(caret)     
library(vip) 
library(ROCR)
library(rcompanion)
library(corrplot)
library(vcd)



# read data
loan_data <- read_csv("LoanData.csv")

# Dataset
df_na <- loan_data %>% 
  select(c(LoanId, ListedOnUTC, LoanApplicationStartedDate,  ModelVersion, Rating, UseOfLoan,
          Status, NewCreditCustomer, PreviousEarlyRepaymentsCountBeforeLoan, Gender, Age, 
          IncomeTotal, Education, Country)) 

#         Cleaning 

# Missing Values 
colSums(is.na(df_na))

# Drop missing values
df <- na.omit(df_na)

colSums(is.na(df))

#         Variable Formatting 

#Date and Time 
df$ListedDate <- as.Date(df$ListedOnUTC)
df$ListedTime <- format(df$ListedOnUTC,"%H:%M:%S")


df$AppDate <- as.Date(df$LoanApplicationStartedDate)
df$AppTime <- as.numeric(format(df$LoanApplicationStartedDate,"%H"))
df$AppYear <- as.numeric(format(df$LoanApplicationStartedDate,"%Y"))

# Create Categorical for time 

# Morning (6-12), Afternoon(12-18), Evening(18-24), Night(24-6)

df <- df %>% 
  mutate(AppTimeCat = ifelse(AppTime >= 0 & AppTime<=6, "Night",
                       ifelse(AppTime > 6 & AppTime<=12, "Morning",
                       ifelse(AppTime > 12 & AppTime<=18, "Afternoon",
                       ifelse(AppTime > 18 & AppTime<=24, "Evening", NA)))))
# Weekday
df$Weekday <- weekdays(as.Date(df$LoanApplicationStartedDate))

#df <- df %>% 
  #mutate(Weekend = ifelse(AppWeekdays == "Saturday" | AppWeekdays == "Sunday", 1, 0)) 

# Duration of Loan application, may have explanatory power
df$Duration <- difftime(df$ListedOnUTC, df$LoanApplicationStartedDate, units="hours")

aggregate(Duration ~ModelVersion, data = df, FUN = function(x) 
  c(mn = mean(x), std = sd(x), min = min(x), max=max(x)))

# Clean values where duration is less than 0
df_clean <- subset(df, Duration >= 0)

aggregate(Duration ~ModelVersion, data = df_clean, FUN = function(x) 
  c(mn = mean(x), std = sd(x), min = min(x), max=max(x)))

# We have to leave out duration out of the model due to inconsistencies 

# New Customer
df$NewCustomer <- ifelse(df$NewCreditCustomer == TRUE, 1, 0)
table(df$NewCustomer)

# EarlyRepayment
df$EarlyRepayment <- df$PreviousEarlyRepaymentsCountBeforeLoan
table(df$PreviousEarlyRepaymentsCountBeforeLoan)

# UseOfLoan
df$UseOfLoan <- as.character(df$UseOfLoan)

# Loan Status "Late" is considered credit distress
df <- df %>% 
  mutate(Distress = ifelse(Status == "Late",1,0))
df$Distress <- as.factor(df$Distress)


# Select differnet model version of Bondora to use as benchmark
# (they updated their credit ratings several times)

df_model1 <- subset(df, ModelVersion == 1)
df_model5 <- subset(df, ModelVersion == 5)

# Calculate distress rates and distribution of rating scores

default_mean <- df %>% 
  group_by(Rating) %>%
  summarise(Distress_rate = mean(as.numeric(Distress)-1))

freq_rating <- df %>%
  group_by(Rating) %>%
  summarise(Freq = n())


# merge two data frames by ID and Country
summarised_df <- default_mean
summarised_df$density <- freq_rating$Freq/sum(freq_rating$Freq)
summarised_df
summarised_df$Rating <- factor(summarised_df$Rating , 
                    levels = c("AA","A","B","C","D","E","F","HR"))

# Plot distribution of credit ratings and corresponing distress rate
ggp <- ggplot(summarised_df)  + 
  geom_bar(aes(x=Rating, y=density),stat="identity", 
           fill="#FFD54F",colour= "white")+
  geom_line(aes(x=Rating, y=Distress_rate), stat ="identity",  
            color="black",size=0.5, group=1)+
  labs(title= "Distress Rate - Credit Rating",
       x="Credit Rating",y="Frequency") +
  scale_y_continuous(sec.axis=sec_axis(~.*1.0,name="Distress Rate"))

ggp

#         Some Data Exploration

# What Years did the models start and end 
model_agg <- aggregate(AppYear ~ ModelVersion, data = df, 
          FUN = function(x) c(min = min(x), max = max(x), n=length(x)))
model_agg

#based on Model Version 
table(df$ModelVersion)

# Observations
count(df)
count(df_model1)
count(df_model5)

# Gender 0 Male, 1 Femnale, 2 Undefined 
table(df$Gender)
table(df_model1$Gender)
table(df_model5$Gender)

# Country
table(df$Country)
table(df_model1$Country)
table(df_model5$Country)

# Age 
summary(df$Age)
sd(df$Age)
summary(df_model1$Age)
sd(df_model1$Age)
summary(df_model5$Age)
sd(df_model5$Age)


#Education 1 Primary education 
# 2 Basic education 3 Vocational education 4 Secondary education 5 Higher education

table(df$Education)
table(df_model1$Education)
table(df_model5$Education)



#           Correlation Matrix

# We have to make age categorical 10% quartiles 
df_model1$Age_Q <- as.character(cut(df_model1$Age, quantile(df_model1$Age, probs = seq(0, 1, 0.1 )), 
                       include.lowest=TRUE,labels=FALSE) )

# Make cat duration 
df_model1$Duration <- as.numeric(df_model1$Duration)
df_model1$Duration_Q <- as.character(cut(df_model1$Duration, 
                                         quantile(df_model1$Duration, 
                                                  probs = seq(0, 1, 0.1 )), 
                                    include.lowest=TRUE,labels=FALSE) )
  
df_m1_cor <- df_model1 %>% 
  select(c(AppTimeCat, Weekday, EarlyRepayment, UseOfLoan, Rating, 
           Gender, Age_Q, Education, Country, Duration_Q))

# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(df_m1_cor),
                  nrow = length(df_m1_cor),
                  dimnames = list(names(df_m1_cor), 
                                  names(df_m1_cor)))

# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, df_m1_cor) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df_m1_cor[[r]], df_m1_cor[[c]]))$cramer
    }
  }
  return(m)
}


cor_matrix <- calculate_cramer(empty_m ,df_m1_cor)

print(cor_matrix)



# Relevel Data

df_model1 <- df_model1 %>% mutate_if(is.ordered, factor, ordered = FALSE)

df_model5 <- df_model5 %>% mutate_if(is.ordered, factor, ordered = FALSE)


# Unique values model 6
lapply(df_model6[c("AppTimeCat", "Weekday", "EarlyRepayment", "UseOfLoan",
                   "Rating")], unique)


#         Model with Complete Data
# Models 

# Solo
ms <- Distress ~ AppTimeCat + Weekday + EarlyRepayment + Duration
# Rating 
mr <- Distress ~ Rating
# Combined 
mc <- Distress ~ AppTimeCat + Weekday + EarlyRepayment + Duration  + Rating 

# Model 1
m1_s =  glm(ms, 
  family = "binomial", 
  data = df_model1)

m1_r =  glm(mr, 
  family = "binomial", 
  data = df_model1)


m1_c =  glm(mc, 
  family = "binomial", 
  data = df_model1)


m1_cc =  glm(Distress ~ AppTimeCat + Weekday + 
               EarlyRepayment + Rating + Duration +
               Gender + Age + Education + Country, 
            family = "binomial", 
            data = df_model1)
# Model 5
m5_s =  glm(
  ms, 
  family = "binomial", 
  data = df_model5)


m5_r =  glm(
  mr, 
  family = "binomial", 
  data = df_model5)


m5_c =  glm(
  mc, 
  family = "binomial", 
  data = df_model5)


# Coef

# Model 1
odds_m1s <- tidy(m1_s)
view(odds_m1s)

odds_m1r <- tidy(m1_r)
view(odds_m1r)
 
odds_m1c <- tidy(m1_c)
view(odds_m1c)

odds_m1cc <- tidy(m1_cc)
view(odds_m1cc)

coef_m1s <- coef(m1_s)
exp(coef(m1_s))
exp(coef(m1_r))
exp(coef(m1_c))
exp(coef(m1_cc))

count(df_model1)

# Model 5
tidy(m5_s)
tidy(m5_r)
tidy(m5_c)

exp(coef(m5_s))
exp(coef(m5_r))
exp(coef(m5_c))


# Train Cross Validated Models 


# Function 
train_cv <- function (Model, DATA) {
  train(Model, 
        data = DATA, 
        method = "glm",
        family = "binomial",
        trControl = trainControl(method = "cv", number = 10)
  )
}

# Solo Models # (Model 5 is commented out)
#1

set.seed(101)
cv_m1_s <- train_cv(ms,df_model1)
#5
#set.seed(101)
#cv_m5_s <- train_cv(ms,df_model5)

# Rating Model 
#1
set.seed(101)
cv_m1_r <- train_cv(mr,df_model1)

#5
#set.seed(101)
#cv_m5_r <- train_cv(mr,df_model5)


# Combined Models 
#1
set.seed(101)
cv_m1_c <- train_cv(mc,df_model1)

#5
#set.seed(101)
#cv_m5_c <- train_cv(mc,df_model5)

# Combined with Control Controls: Gender, Age, Education, Country

#1
set.seed(101)
cv_m1_cc <- train(
  Distress ~ AppTimeCat + Weekday + EarlyRepayment + Rating + Duration +
    Gender + Age + Education + Country,
  data = df_model1, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

#5
#set.seed(101)
#cv_m5_cc <- train(
 # Distress ~ AppTimeCat + Weekday + EarlyRepayment + Rating +
#    Gender + Age + Education + Country,
#  data = df_model5, 
#  method = "glm",
#  family = "binomial",
#  trControl = trainControl(method = "cv", number = 10)
#)

# Acc

summary(
  resamples(
    list(
      SoloAll = cv_m1_s, 
      RatingAll = cv_m1_r,
      CombinedAll = cv_m1_c
    )
  )
)$statistics$Accuracy

# confusion matrix

#m1
pred_m1_s <- predict(cv_m1_s, df_model1)
pred_m1_r <- predict(cv_m1_r, df_model1)
pred_m1_c <- predict(cv_m1_c, df_model1)

# create confusion matrix
confusionMatrix(
  data = relevel(pred_m1_s, ref = 1), 
  reference = relevel(df_model1$Distress, ref = 1)
)

confusionMatrix(
  data = relevel(pred_m1_r, ref = 1), 
  reference = relevel(df_model1$Distress, ref = 1)
)

confusionMatrix(
  data = relevel(pred_m1_c, ref = 1), 
  reference = relevel(df_model1$Distress, ref = 1)
)

# Area Under Curve 


# Compute predicted probabilities
# Model 1
m1_p_s <- predict(cv_m1_s, df_model1, type = "prob")[,2]
m1_p_r <- predict(cv_m1_r, df_model1, type = "prob")[,2]
m1_p_c <- predict(cv_m1_c, df_model1, type = "prob")[,2]
m1_p_cc <- predict(cv_m1_cc, df_model1, type = "prob")[,2]


# Model 5
#m5_p_s <- predict(cv_m5_s, df_model5, type = "prob")[,2]
#m5_p_r <- predict(cv_m5_r, df_model5, type = "prob")[,2]
#m5_p_c <- predict(cv_m5_c, df_model5, type = "prob")[,2]
#m5_p_cc <- predict(cv_m5_cc, df_model5, type = "prob")[,2]



# AUC Predictions 

# Model 1
aucperf1_solo <-  prediction(m1_p_s, df_model1$Distress) %>%
  performance(measure = "auc")

aucperf1_rating <-  prediction(m1_p_r, df_model1$Distress) %>%
  performance(measure = "auc")

aucperf1_combined <-  prediction(m1_p_c, df_model1$Distress) %>%
  performance(measure = "auc")

aucperf1_combined_c <-  prediction(m1_p_cc, df_model1$Distress) %>%
  performance(measure = "auc")



# Model 5

#aucperf5_solo <-  prediction(m5_p_s, df_model5$Distress) %>%
#  performance(measure = "auc")

#aucperf5_rating <-  prediction(m5_p_r, df_model5$Distress) %>%
#  performance(measure = "auc")

#aucperf5_combined <-  prediction(m5_p_c, df_model5$Distress) %>%
 # performance(measure = "auc")

#aucperf5_combined_c <-  prediction(m5_p_cc, df_model5$Distress) %>%
 # performance(measure = "auc")

#         RESULTS

# Solo
aucperf1_solo@y.values[[1]]
#aucperf5_solo@y.values[[1]]


# Rating
aucperf1_rating@y.values[[1]]
#aucperf5_rating@y.values[[1]]


# Combined
aucperf1_combined@y.values[[1]]
#aucperf5_combined@y.values[[1]]


# Combined with Controls 
aucperf1_combined_c@y.values[[1]]
#aucperf5_combined_c@y.values[[1]]

#Improvements
# Rating vs Combined 
aucperf1_combined@y.values[[1]] - aucperf1_rating@y.values[[1]]
#aucperf5_combined@y.values[[1]] - aucperf5_rating@y.values[[1]]


# Rating vs. Combined with Controls
aucperf1_combined_c@y.values[[1]] - aucperf1_rating@y.values[[1]]
#aucperf5_combined_c@y.values[[1]] - aucperf5_rating@y.values[[1]]

# Compute AUC metrics for cv_model1 and cv_model3

# Pseudo R Squared 
with(summary(cv_m1_s), 1 - deviance/null.deviance)
with(summary(cv_m1_r), 1 - deviance/null.deviance)
with(summary(cv_m1_c), 1 - deviance/null.deviance)
with(summary(cv_m1_cc), 1 - deviance/null.deviance)



# Plot predictions 

# Model 1 
perf1_s <- prediction(m1_p_s, df_model1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")

perf1_r<- prediction(m1_p_r, df_model1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")

perf1_c <-  prediction(m1_p_c, df_model1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Model 5

#perf5_s <- prediction(m5_p_s, df_model5$Distress) %>%
 # performance(measure = "tpr", x.measure = "fpr")

#perf5_r<- prediction(m5_p_r, df_model5$Distress) %>%
 # performance(measure = "tpr", x.measure = "fpr")

#perf5_c <-  prediction(m5_p_c, df_model5$Distress) %>%
  #performance(measure = "tpr", x.measure = "fpr")


#           Plot ROC curves for cv_model1 and cv_model3

# Model 1
plot(perf1_s, col = "black", lty = 3)
plot(perf1_r, add = TRUE, col = "blue")
plot(perf1_c, add = TRUE, col = "red")
abline(a=0, b= 1)
legend(0.8, 0.2, legend = c("Behavioural Footprint", "Rating", "Combined Model"),
  col = c("black", "blue", "red"), lty = 3:1, cex = 0.6)
title("ROC Curve")

# Model 2
plot(perf5_s, col = "black", lty = 3)
plot(perf5_r, add = TRUE, col = "blue")
plot(perf5_c, add = TRUE, col = "red")
abline(a=0, b= 1)
legend(0.8, 0.2, legend = c("Solo5", "Rating5", "Combined5"),
       col = c("black", "blue", "red"), lty = 3:1, cex = 0.6)
title("Model 5")





#         Model with Data Split 
# Model 1
set.seed(101)  # for reproducibility
Distress_split_1 <- initial_split(df_model1, prop = .7, strata = "Distress")



# Assign Data Frames
Distress_train_1 <- training(Distress_split_1)
Distress_test_1  <- testing(Distress_split_1)


# Model: 
# Variables: AppTimeCat + Weekday RepaymentEarlyCount

# Model 1
model_1_solo =  glm(
  Distress ~ AppTimeCat + Weekday + EarlyRepayment + UseOfLoan, 
  family = "binomial", 
  data = Distress_train_1)


model_1_rating =  glm(
  Distress ~ Rating, 
  family = "binomial", 
  data = Distress_train_1)


model_1_combined =  glm(
  mc
  + Rating, 
  family = "binomial", 
  data = Distress_train_1)





# Coef
tidy(model_1_solo)
tidy(model_1_combined)


exp(coef(model_1_solo))
exp(coef(model_1_combined))

# Model Accuracy

# Solo Models 
set.seed(101)
cv_model_1_solo <- train(
  Distress ~ AppTimeCat + Weekend + Duration + EarlyRepayment + UseOfLoan,
  data = Distress_train_1, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)


# Rating Model 

set.seed(101)
cv_model_1_rating <- train(
  Distress ~ Rating,
  data = Distress_train_1, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)


# Combined Models 
set.seed(101)
cv_model_1_combined <- train(
  Distress ~ AppTimeCat + Weekend + Duration + EarlyRepayment + UseOfLoan
  + Rating,
  data = Distress_train_1, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)



summary(
  resamples(
    list(
      SoloAll = cv_model_1_solo, 
      RatingAll = cv_model_1_rating,
      CombinedAll = cv_model_1_combined,
    )
  )
)$statistics$Accuracy

# Area Under Curve 


# Compute predicted probabilities
m1_prob_solo <- predict(cv_model_1_solo, Distress_train_1, type = "prob")[,2]

m1_prob_rating <- predict(cv_model_1_rating, Distress_train_1, type = "prob")[,2]

m1_prob_combined <- predict(cv_model_1_combined, Distress_train_1, type = "prob")[,2]


# Compute AUC metrics for cv_model1 and cv_model3
perf1_solo <- prediction(m1_prob_solo, Distress_train_1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")


perf1_rating <- prediction(m1_prob_rating, Distress_train_1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")


perf1_combined <- prediction(m1_prob_combined, Distress_train_1$Distress) %>%
  performance(measure = "tpr", x.measure = "fpr")

# AUC

# Solo 
aucperf1_solo <- prediction(m1_prob_solo, Distress_train_1$Distress) %>%
  performance(measure = "auc")
aucperf1_solo@y.values[[1]]


# Rating
aucperf1_rating <- prediction(m1_prob_rating, Distress_train_1$Distress) %>%
  performance(measure = "auc")
aucperf1_rating@y.values[[1]]

# Combined 

aucperf1_combined <- prediction(m1_prob_combined, Distress_train_1$Distress) %>%
  performance(measure = "auc")
aucperf1_combined@y.values[[1]]


# Solo
aucperf1_solo@y.values[[1]]


# Rating
aucperf1_rating@y.values[[1]]

# Combined
aucperf1_combined@y.values[[1]]


#Improvements
aucperf1_combined@y.values[[1]] - aucperf1_rating@y.values[[1]]