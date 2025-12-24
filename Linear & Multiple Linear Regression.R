## Toyota Corolla assignment

### Clear environment
rm(list = ls())

# Importing data
# NOTE: Make sure you have the right path to your data file
setwd("C:/Predictive Analytics")


# Load dataset
Toyota_Corolla <- read.csv("ToyotaCorolla1.csv", header = TRUE)
View(Toyota_Corolla)

# find data types of the  data
str(Toyota_Corolla)

##Convert categorical variables into factors
Toyota_Corolla$Fuel_Type <- as.factor(Toyota_Corolla$Fuel_Type)
Toyota_Corolla$Color <- as.factor(Toyota_Corolla$Color)
Toyota_Corolla$Model <- as.factor(Toyota_Corolla$Model)


# find summary statistics for each variable
summary(Toyota_Corolla)

#check for missing values
colSums(is.na(Toyota_Corolla))#no missing values, we can proceed with visualizing distributions


# Load required libraries
#install.packages("dplyr")
library(dplyr)

# Set up plotting layout: 4 rows × 4 columns per page
par(mfcol = c(4, 4))

#Data Viz
# Frequency bar plot
library(ggplot2)
library(ggcorrplot)
ggplot(Toyota_Corolla, aes(x = Fuel_Type)) +
  geom_bar(fill = "steelblue") +
  theme_minimal()

# Boxplot of Price by Fuel Type
ggplot(Toyota_Corolla, aes(x = Fuel_Type, y = Price)) +
  geom_boxplot(fill = "tomato") +
  theme_minimal()


# Step 1: Remove the 3 categorical columns for creating distribution plots
excluded_vars <- c("Model", "Fuel_Type", "Color")

df_numeric <- Toyota_Corolla %>%
  select(-all_of(excluded_vars)) %>%
  select(where(is.numeric))

# Step 2: Plot boxplots for each numeric column
for (i in 1:ncol(df_numeric)) {
  boxplot(df_numeric[[i]],
          main = paste("Boxplot of", names(df_numeric)[i]),
          col = "tomato", border = "black")
}

# Step 3: Reset plotting layout for histograms
par(mfcol = c(4, 4))

# Step 4: Plot histograms for each numeric column
for (i in 1:ncol(df_numeric)) {
  hist(df_numeric[[i]],
       main = paste("Histogram of", names(df_numeric)[i]),
       col = "steelblue", border = "white", breaks = 30)
}

##check for exclusing categorical variables
# Remove columns with zero standard deviation
df_numeric_filtered <- df_numeric %>%
  select(where(~ sd(.) > 0))

# heatmap with values
#install.packages("ggcorrplot")
library(ggcorrplot)

# Compute correlation matrix
cor_matrix <- cor(na.omit(df_numeric_filtered))

# Plot using ggcorrplot
ggcorrplot(cor_matrix,
           hc.order = TRUE,         # hierarchical clustering
           type = "full",          # show lower triangle
           lab = TRUE,              # show correlation values
           lab_size = 3,
           colors = c("red", "white", "green"),
           title = "Correlation Matrix",
           ggtheme = theme_minimal())

##examples to see correlation b/w 2 variables
# Price vs KM
ggplot(Toyota_Corolla, aes(x = KM, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal()

# Price vs Age
ggplot(Toyota_Corolla, aes(x = Age_08_04, y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal()

#Write above the data, missing values, graph (heatmap, boxplot, hist) for Q1.

#Q2
#Partition the data to train a regression model
# use set.seed() to get the same partitions when re-running the R code.
set.seed(16)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(Toyota_Corolla), dim(Toyota_Corolla)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- Toyota_Corolla[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(Toyota_Corolla), train.rows) 
valid.data <- Toyota_Corolla[valid.rows, ] 

# Generate a multiple linear regression model
reg <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
            Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player +
            Powered_Windows + Sport_Model + Tow_Bar,
          data = Toyota_Corolla, subset = train.rows)

summary(reg)

##Q3
##a) Four important car specifications would be based on low p-values and high value
#Imporatant variables are: Age_08_04,Automatic_airco, KM, HP
#they are statistical significant predictor variables and have largest impact on price

#b)# turn off scientific notation for numbers
options (scipen=999, digits =2)

# plot the residuals    – check for heteroskedasticity
par(mfcol=c(1,1))

plot(reg$fitted.values, reg$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")


# Use the Breusch–Pagan Test for heteroscedacity
# Breusch–Pagan Test
# If the p-value is small (< 0.05) → reject H₀ → evidence of heteroscedasticity.
#install.packages("lmtest")
library(lmtest)
bptest(reg)


"RESULTS:

	studentized Breusch-Pagan test

data:  reg
BP = 154, df = 16, p-value <0.0000000000000002"
##Above p-value shows evidence of heteroscedasticity.

#c)Validate the model
pred <- predict(reg, newdata = valid.data)


#3METRICS TO ACCESS THE ACCURACY-- for complex model
#RMSE
#install.packages("caret")
library(caret)
RMSE(pred, valid.data$Price)

#MAPE
#install.packages("MLmetrics")
library(MLmetrics)
MAPE(pred,valid.data$Price)

# compute the mean absolute error-MAE
MAE(pred,valid.data$Price)

##Accuracy
#install.packages("forecast")
library(forecast)

# compute accuracy on training set
accuracy(reg$fitted.values, train.data$Price)
"                         ME RMSE MAE  MPE MAPE
Test set -0.000000000000048 1236 926 -1.1  9.1"

# compute accuracy on prediction set
accuracy(pred, valid.data$Price)
"         ME RMSE MAE   MPE MAPE
Test set 48 1182 879 -0.26  8.7"

#d) Build simpler model
reg_simple <- lm(Price ~ Age_08_04 + KM + Automatic_airco,
                 data = Toyota_Corolla, subset = train.rows)
summary(reg_simple)

# Predict and evaluate on validation set
preds_simple <- predict(reg_simple, newdata = valid.data)
RMSE(preds_simple,valid.data$Price)
MAPE(preds_simple,valid.data$Price)
MAE(preds_simple,valid.data$Price)

# compute accuracy on training set
accuracy(reg_simple$fitted.values, train.data$Price)

"                        ME RMSE  MAE  MPE MAPE
Test set -0.00000000000022 1456 1065 -1.5   10"
# compute accuracy on prediction set

accuracy(preds_simple, valid.data$Price)
"          ME RMSE MAE   MPE MAPE
Test set 105 1416 992 -0.26  9.5"


"Comparison of two models:
Accuracy of complex model is more as the metrics RMSE, MAE, MAPE are lower
as compared to simpler model.
But the simpler model is earsier to interpret due to less number of predictors.
So, if our goal is to maximize the predictive accuracy, we should go with
the complex model and if we want a more interpretable model,
the simpler one is good to go for instead. 
"

# Calculate median price by Fuel_Type
aggregate(Price ~ Fuel_Type, data = Toyota_Corolla, FUN = median)




