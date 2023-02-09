# -*- coding: utf-8 -*-
"""
Created on Tue Mar 22

@author: Calvin
"""
# EBD2 Empirical Assignment

# Data Cleaning

# Load necessary libraries
import pandas as pd
import numpy as np
from statsmodels.discrete.discrete_model import Probit
import statsmodels.api as sm
import matplotlib.pyplot as plt
from stargazer.stargazer import Stargazer
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.ensemble import GradientBoostingClassifier

# Import datasets (Change directory accordingly)
Loan_df = pd.read_csv(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\LoanData_10-4-2022.csv')
Stock_market_df = pd.read_excel(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\Data\Stock_market.xlsx')
Inflation_df = pd.read_excel(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\Data\CPI.xlsx')
Unemployment_df = pd.read_excel(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\Data\Unemployment.xlsx')

# Add full country names
conditions = [
    (Loan_df['Country'] == 'EE'),
    (Loan_df['Country'] == 'FI'),
    (Loan_df['Country'] == 'ES'),
    (Loan_df['Country'] == 'SK')
]

country_names = ['Estonia', 'Finland', 'Spain', 'Slovakia']
Loan_df['Full_country_names'] = np.select(conditions, country_names)

# Filter for Estonia
raw_data = Loan_df[Loan_df['Country'] == 'EE']

# Filter dates from 2012 to 2021
raw_data['LoanDate'] = pd.to_datetime(raw_data['LoanDate'], format='%Y-%m-%d')
raw_data = raw_data[(raw_data['LoanDate'] > "2011-12-31") & (raw_data['LoanDate'] < "2022-01-01")]

# Remove current loans
raw_data = raw_data[(raw_data['Status'] == 'Repaid') | (raw_data['Status'] == 'Late')]

# Generate dependent variable: Bad_loans
raw_data['Bad_loans'] = np.where(raw_data['Status'] == 'Late', 1, 0)

# Replace Expected Loss null values with 0
raw_data['ExpectedLoss'].fillna(0, inplace=True)

# Append Stock market Indices
Stock_market_df['Date'] = pd.to_datetime(Stock_market_df['Date'], format='%m/%d/%Y')
Stock_market_df['Log_Stock_Price'] = np.log(Stock_market_df['Stock_Price'])

raw_data = pd.merge(raw_data.assign(grouper=raw_data['LoanDate'].dt.to_period('M')),
                    Stock_market_df.assign(grouper=Stock_market_df['Date'].dt.to_period('M')),
                    how='left', on='grouper')
raw_data.drop(['Date', 'grouper'], inplace=True, axis=1)

# Append CPI
raw_data = pd.merge(raw_data.assign(grouper=raw_data['LoanDate'].dt.to_period('M')),
                    Inflation_df.assign(grouper=Inflation_df['Date'].dt.to_period('M')),
                    how='left', on='grouper')
raw_data.drop(['Date', 'grouper'], inplace=True, axis=1)

# Append Unemployment Rates
raw_data = pd.merge(raw_data.assign(grouper=raw_data['LoanDate'].dt.to_period('M')),
                    Unemployment_df.assign(grouper=Unemployment_df['Period'].dt.to_period('M')),
                    how='left', on='grouper')
raw_data.drop(['Period', 'grouper'], inplace=True, axis=1)

# Summary Statistics
desc_stat = raw_data[['Stock_Price', 'CPI', 'Unemployment rate (in %)', 'Bad_loans']].describe(include='all')
desc_stat.to_csv(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\Data\Descriptive_Statistics.csv',
    index=True)

# Rating Frequency table
expectedloss_by_rating = raw_data.groupby('Rating')['ExpectedLoss'].describe()
expectedloss_by_rating.to_csv(
    r'C:\Users\calvi\Data\University\Seminars\Topics in FinTech\Empirical Assignment\Data\Expectedloss_by_Creditrating.csv',
    index=True)

# Macroeconomic variables time-series graphs

# Stock Price line graph
plt.plot(Stock_market_df['Date'], Stock_market_df['Stock_Price'])
plt.title('Estonia Stock Market Index by Year')
plt.xlabel('Year')
plt.ylabel('OMX Tallinn')
plt.show()

plt.plot(Stock_market_df['Date'], Stock_market_df['Log_Stock_Price'], '--')
plt.title('Estonia Stock Market Index (Ln) by Year')
plt.xlabel('Year')
plt.ylabel('Ln(OMX Tallinn)')
plt.show()

# CPI line graph
plt.plot(Inflation_df['Date'], Inflation_df['CPI'])
plt.title('Estonia CPI by Year')
plt.xlabel('Year')
plt.ylabel('Consumer Price Index')
plt.show()

# Unemployment rate line graph
plt.plot(Unemployment_df['Period'], Unemployment_df['Unemployment rate (in %)'])
plt.title('Estonia Unemployment Rate by Year')
plt.xlabel('Year')
plt.ylabel('Unemployment Rate (%)')
plt.show()

# Random Forest Analysis

# RF Model 1: Borrower characteristics only
rf_data = raw_data[['Bad_loans', 'ExpectedLoss']]
# RF: Define dependent variable
Y = rf_data['Bad_loans'].values
Y = Y.astype('int')
# RF: Define independent variables
X = rf_data.drop(labels=['Bad_loans'], axis=1)
# Split into train and test datasets
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)
# Generate model
model = RandomForestClassifier(n_estimators=10, random_state=20)
model.fit(X_train, Y_train)
prediction_test = model.predict(X_test)
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))

# RF Model 2: Macro only
rf_data = raw_data[['Bad_loans', 'Stock_Price', 'CPI', 'Unemployment rate (in %)']]
# RF: Define dependent variable
Y = rf_data['Bad_loans'].values
Y = Y.astype('int')
# RF: Define independent variables
X = rf_data.drop(labels=['Bad_loans'], axis=1)
# Split into train and test datasets
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)
# Generate model
model = RandomForestClassifier(n_estimators=10, random_state=20)
model.fit(X_train, Y_train)
prediction_test = model.predict(X_test)
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))
# Feature importance
feature_list = list(X.columns)
feature_imp = pd.Series(model.feature_importances_, index=feature_list).sort_values(ascending=False)
print(feature_imp)

# RF Model 3: Borrower characteristics + Macro
rf_data = raw_data[['Bad_loans', 'ExpectedLoss', 'Stock_Price', 'CPI', 'Unemployment rate (in %)']]
# RF: Define dependent variable
Y = rf_data['Bad_loans'].values
Y = Y.astype('int')
# RF: Define independent variables
X = rf_data.drop(labels=['Bad_loans'], axis=1)
# Split into train and test datasets
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)
# Generate model
model = RandomForestClassifier(n_estimators=10, random_state=20)
model.fit(X_train, Y_train)
prediction_test = model.predict(X_test)
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))
# Feature importance
feature_list = list(X.columns)
feature_imp = pd.Series(model.feature_importances_, index=feature_list).sort_values(ascending=False)
print(feature_imp)

# Probit Analysis

# Probit regression with borrower characteristics only
Y = raw_data['Bad_loans']
X = raw_data[['ExpectedLoss']]
X = sm.add_constant(X)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)

model = Probit(Y_train, X_train.astype(float))
probit_model_borrow = model.fit()
probit_model_borrow_me = probit_model_borrow.get_margeff()
print(probit_model_borrow.summary())
print(probit_model_borrow.get_margeff().summary())

prediction_test = probit_model_borrow.predict(X_test)
prediction_test[prediction_test <= 0.5] = 0
prediction_test[prediction_test > 0.5] = 1
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))

# Probit regression with macroeconomic variables only
Y = raw_data['Bad_loans']
X = raw_data[['Log_Stock_Price', 'CPI', 'Unemployment rate (in %)']]
X = sm.add_constant(X)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)

model = Probit(Y_train, X_train.astype(float))
probit_model_macro = model.fit()
print(probit_model_macro.summary())
print(probit_model_macro.get_margeff().summary())

prediction_test = probit_model_macro.predict(X_test)
prediction_test[prediction_test <= 0.5] = 0
prediction_test[prediction_test > 0.5] = 1
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))

# Probit regression with borrower chracteristics and macroeconomic variables
Y = raw_data['Bad_loans']
X = raw_data[['ExpectedLoss', 'Log_Stock_Price', 'CPI', 'Unemployment rate (in %)']]
X = sm.add_constant(X)

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=20)

model = Probit(Y_train, X_train.astype(float))
probit_model_both = model.fit()
print(probit_model_both.summary())
print(probit_model_both.get_margeff().summary())

prediction_test = probit_model_both.predict(X_test)
prediction_test[prediction_test <= 0.5] = 0
prediction_test[prediction_test > 0.5] = 1
print("Accuracy = ", metrics.accuracy_score(Y_test, prediction_test))

#Collated Probit Results
stargazer_output = Stargazer([probit_model_borrow, probit_model_macro, probit_model_both])
print(stargazer_output.render_latex())

me_stargazer_output = Stargazer([probit_model_borrow_me])
print(me_stargazer_output.render_latex())


# Gradient Boosting Algorithm

#Model 1: Personal Characteristics only
Y = raw_data['Bad_loans']
X = raw_data[['ExpectedLoss']]
X = sm.add_constant(X)

model = GradientBoostingClassifier()
cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=20)
n_scores = cross_val_score(model, X, Y, scoring='accuracy', cv=cv, n_jobs=-1)
print('Mean Accuracy: %.3f (%.3f)' % (np.mean(n_scores), np.std(n_scores)))

#Model 2: Macro only
Y = raw_data['Bad_loans']
X = raw_data[['Stock_Price', 'CPI', 'Unemployment rate (in %)']]
X = sm.add_constant(X)

model = GradientBoostingClassifier()
cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=20)
n_scores = cross_val_score(model, X, Y, scoring='accuracy', cv=cv, n_jobs=-1)
print('Mean Accuracy: %.3f (%.3f)' % (np.mean(n_scores), np.std(n_scores)))

#Model 3: Macro + Personal Characteristics
Y = raw_data['Bad_loans']
X = raw_data[['ExpectedLoss', 'Stock_Price', 'CPI', 'Unemployment rate (in %)']]
X = sm.add_constant(X)

model = GradientBoostingClassifier()
cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=20)
n_scores = cross_val_score(model, X, Y, scoring='accuracy', cv=cv, n_jobs=-1)
print('Mean Accuracy: %.3f (%.3f)' % (np.mean(n_scores), np.std(n_scores)))

# End of Assignment
