# Credit_Default_Prediction_2022

## Introduction

This group project is part of master's seminar "Topics in Fintech"

Recently new approaches to better predict loan risk are being used with the complementation of soft information. Bandora has its own credit rating, and has its data open to access. This study tried to use soft information inside Bandora's data to check if soft information improves the default prediction of Bandora`s credit system or can replace it entirely. The variables Date and Time, Duration and Early Repayment were used as behavioural traits in the log-regression tenfold cross validation (10x CV). To test whether our model adds discriminatory power to the Bondora rating model, we use the area under the curve (AUC). The introduction of the behavioural variables did not have a better result than Bandora's credit system but when combined to it increases the AUC by 1 percentage point.

## Comments

The R file is a little bit messy. It was one of the first projects I ever did in R. However, I provide a detailed report of the results as well as a discussion about implications as well as limitiations.
