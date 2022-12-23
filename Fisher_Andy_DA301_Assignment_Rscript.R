## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
## performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)

# install and import dply so 'select' function works
install.packages("dplyr")
library(dplyr)

# set working directory
setwd(dir='/Users/fishi/OneDrive/Documents/Course 3/Assignment') 

# Import the data set.
sales_raw <- read.csv('turtle_sales.csv', header=T)

# Print the data frame.
sales_raw

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales <- select(sales_raw, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
sales

# View the descriptive statistics.
summary(sales)
as_tibble(sales)

# comments
# 352 rows, 5 columns
# product is integer type when should be character
class(sales$Product)
class(sales$Product) = "character"
class(sales$Product)
# means > median => suggests right skew

################################################################################

# 2. Review plots to determine insights into the data set.

# Import ggplot2
library(ggplot2)

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, NA_Sales, data=sales)
# hardly any products generate over £10m

qplot(EU_Sales, NA_Sales, colour=Platform, data=sales, main='NA vs EU sales')
# shows one major outlier that sold incredibly well in NA and EU
# shows a few outliers where NA product sales have been much higher than EU sales
# shows positive correlation between product sales in NA and EU

qplot(Product, EU_Sales, data=sales)
# hardly any products generate over £5m

qplot(Product, Global_Sales, data=sales)
# appears to be a somewhat defined "sales ceiling" forming, needs investigating

# plot product and platform
qplot(Product, Platform, colour=Global_Sales, data=sales, 
      main='Platform vs Product sales',
      size=I(3)) +
  scale_colour_continuous(name = "Global Sales", 
                          low = "light blue", 
                          high = "red") +
  theme_classic() 
# this plot attempts demonstrates which platforms have the greatest product sales
# i.e. Wii


## 2b) Histograms
# Create histograms.

qplot(EU_Sales, data=sales)
qplot(NA_Sales, data=sales)
# similar shapes with a right skew
# many products have relatively low sales

qplot(Global_Sales, fill=Platform, col=I('black'), data=sales,
      main='Histogram of global sales with platform in colour') +
  theme_minimal() 
# driver of severe majority of sales is quantity of games, not a few blockbusters
# colour allows the spread across platforms to be better understood


## 2c) Boxplots
# Create boxplots.
qplot(Platform, EU_Sales, data=sales, geom='boxplot', ylim=c(0,35))
qplot(Platform, NA_Sales, data=sales, geom='boxplot', ylim=c(0,35))
# variation between platform preference by region (EU/NA) can easily be seen, 
# especially when the y-axis is kept constant
# e.g. NES sales in EU are unremarkable, but in NA the IQR and spread are much more prominent

qplot(Platform, Global_Sales, ylim=c(0,30), data=sales, geom='boxplot',
      main='Boxplot showing spread of global sales by platform') +
  theme_minimal() 
# by setting the y-axis limit, 1 outlier is ignored to allow us to see the boxplots shapes better
# GB platform sales are heavily influenced by a positive extremes from a small sample size



###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# there is great variation in the success (total sales) of certain games and platforms
# EU sales and NA sales do not follow the same form
# the Wii has almost all the high outliers
# units sold along with the sales value would enable volume, mix, rate analysis



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
sales

# Check output: Determine the min, max, and mean values.
min(sales$NA_Sales)
# => 0
max(sales$NA_Sales)
# => 34.02
mean(sales$NA_Sales)
# => 2.516

min(sales$EU_Sales)
# => 0
max(sales$EU_Sales)
# => 23.8
mean(sales$EU_Sales)
# => 1.644

min(sales$Global_Sales)
# => 0.01
max(sales$Global_Sales)
# => 67.85
mean(sales$Global_Sales)
# => 5.335

# View the descriptive statistics.
summary(sales)
# skim(sales) -> could not find function. deprecated?
DataExplorer::create_report(sales)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_product <- sales %>% group_by(Product) %>% summarise(sum_NA_Sales=sum(NA_Sales),
                                                            sum_EU_Sales=sum(EU_Sales),
                                                            sum_Global_sales=sum(Global_Sales))

# View the data frame.
sales_product
# 175 rows and 4 columns

# Explore the data frame.
as_tibble(sales_product)

# group by product *and* platform to determine impact on sales
sales_product_platform <- sales %>% group_by(Product, Platform) %>% summarise(sum_NA_Sales=sum(NA_Sales),
                                                           sum_EU_Sales=sum(EU_Sales),
                                                           sum_Global_sales=sum(Global_Sales))

sales_product_platform
# 342 rows and 5 columns


## 2b) Determine which plot is the best to compare game sales.

# Create scatterplots.
ggplot(sales_product, aes(x=sum_NA_Sales, y=sum_EU_Sales)) + 
  geom_point(color = 'red', 
             alpha = 0.5,
             size = 1.5) +
  geom_smooth(method = 'lm', se = T) +
  ggtitle("Scatterplot of NA and EU sales")

# => line of best fit not a suitable illustration of relationship between sales regions
# => strong product sales in NA do not necessarily correlate to strong product sales in EU and vice versa
# => majority of products do not generate £10m of sales in both regions
# => more products generate >£10m in NA than EU



ggplot(sales_product, aes(x=sum_Global_sales, y=sum_EU_Sales)) + 
  geom_point(color = 'red', 
             alpha = 0.5,
             size = 1.5) +
  geom_smooth(method = 'lm', se = T) +
  xlim(0, 40) + 
  ylim(0, 40) +
  ggtitle("Scatterplot of global and EU sales")
# => dense cluster at lower sales
# => axies limits set the same so steepness of blue line can be directly compared to NA sales





# Create histograms.
hist(sales_product$sum_NA_Sales,
     xlab='Sum of NA sales',
     main='Histogram showing distribution of NA sales',
     breaks=15)
# => most products sold in NA sell less than £5m

hist(sales_product$sum_EU_Sales,
     xlab='Sum of EU sales',
     main='Histogram showing distribution of EU sales',
     breaks=20)
# => most products sold in NA sell less than £5m
# => much lower overall quantity of sales than NA

hist(sales_product$sum_Global_sales,
     xlab='Sum of Global sales',
     main='Histogram showing distribution of Global sales',
     breaks=20)
# => most products sold globally sell less than £10m
# => a tiny proportion of products generate over £60m globally



# Create boxplots.
boxplot(sales_product$sum_NA_Sales, 
        xlab = "Sum of NA sales",
        ylab = "Sales £m")
# => quite a few positive outliers 
# => a handful of games account for a disproportionate amount of sales

boxplot(sales_product$sum_EU_Sales, 
        xlab = "Sum of EU sales",
        ylab = "Sales £m")
# => overall shape of the boxplot is comparable to NA sales, 
# but no firm conclusions can be made

boxplot(sales_product$sum_Global_sales, 
        xlab = "Sum of Global sales",
        ylab = "Sales £m")
# => top 3 products (outliers) account for incredibly high sales
# => these 3 products need analysing to understand drivers of success

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_product$sum_NA_Sales,
       xlab="NA Sales",
       ylab='Sales £m',
       main='Normal QQ Plot - NA Sales')
qqline(sales_product$sum_NA_Sales,
       col='red')
# => indicates a right skew

qqnorm(sales_product$sum_EU_Sales,
       xlab="EU Sales",
       ylab='Sales £m',
       main='Normal QQ Plot - EU Sales')
qqline(sales_product$sum_EU_Sales,
       col='red')
# => indicates a right skew

qqnorm(sales_product$sum_Global_sales,
       xlab="Global Sales",
       ylab='Sales £m',
       main='Normal QQ Plot - Global Sales')
qqline(sales_product$sum_Global_sales,
       col='red')
# => indicates a right skew

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_product$sum_NA_Sales)
# => Our p-value is less than 0.5, so we cannot assume normality.

shapiro.test(sales_product$sum_EU_Sales)
# => Our p-value is less than 0.5, so we cannot assume normality.

shapiro.test(sales_product$sum_Global_sales)
# => Our p-value is less than 0.5, so we cannot assume normality.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_product$sum_NA_Sales)
kurtosis(sales_product$sum_NA_Sales)
# => skewness = 3.05, greater than 1 so highly skewed
# => kurtosis = 15.6, way above 3 meaning heavy tails

skewness(sales_product$sum_EU_Sales)
kurtosis(sales_product$sum_EU_Sales)
# => skewness = 2.89, greater than 1 so highly skewed
# => kurtosis = 16.2, way above 3 meaning heavy tails

skewness(sales_product$sum_Global_sales)
kurtosis(sales_product$sum_Global_sales)
# => skewness = 3.07, greater than 1 so highly skewed
# => kurtosis = 17.8, way above 3 meaning heavy tails

## 3d) Determine correlation
# Determine correlation.
cor(sales_product$sum_NA_Sales, sales_product$sum_EU_Sales)
# => correlation coefficient of 0.62 indicates a moderate correlation
# => to the best of our knowledge, these are the most independent variables 

cor(sales_product$sum_NA_Sales, sales_product$sum_Global_sales)
# => correlation coefficient of 0.92 indicates a strong correlation
# => strong correlation would be expected 

cor(sales_product$sum_Global_sales, sales_product$sum_EU_Sales)
# => correlation coefficient of 0.85 indicates a strong correlation
# => strong correlation would be expected, but slightly weaker than NA vs Global sales 
# => this suggests that NA sales are more indicative of global sales, 
# which is probably because they account for the majority of global sales

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# install ggplot2
library(ggplot2)

# colour changed to indicate platform to see if any correlation becomes apparent
# also, axies are limited to exclude outliers
  ggplot(sales_product_platform, 
       aes(x=sum_NA_Sales, 
           y=sum_EU_Sales)) + 
  geom_point(alpha = 0.5,
             size = 1.5,
             aes(colour=Platform)) +
  xlim(0, 10) + 
  ylim(0, 10) +
  ggtitle("NA sales vs EU sales with platform in colour")
# => some faint platform sales correlations appear, 
# e.g. playstation products sell better in EU than NA 


ggplot(sales_product, aes(x=sum_NA_Sales, y=sum_EU_Sales)) + 
  geom_point(color = 'red', 
             alpha = 0.5,
             size = 1.5) +
  geom_abline(intercept=0, slope=1,linetype='dashed', size=1) +
  ggtitle("NA sales vs EU sales with sales parity line") +
  labs(x='NA Sales', y='EU Sales') +
  xlim(0,25) +
  ylim(0,25)
# => when I add a line that indicates EU and NA sales parity, 
# you can see that the overwhelming majority of products sell more in NA than EU,
# (because they are below the parity line)


ggplot(sales_product, show.legend = T) + 
  geom_point(aes(y=sum_Global_sales, x=sum_EU_Sales, color='EU Sales'),
             alpha = 0.5,
             size = 1.5) +
  geom_point(aes(y=sum_Global_sales, x=sum_NA_Sales, color='NA Sales'),
             alpha = 0.5,
             size = 1.5) +
  geom_smooth(aes(y=sum_Global_sales, x=sum_NA_Sales), method = 'lm', se = T, color='cyan') +
  geom_smooth(aes(y=sum_Global_sales, x=sum_EU_Sales), method = 'lm', se = T, color='red') +
  ylim(0, 30) + 
  xlim(0, 15) +
  ggtitle("Scatterplot of regional and global sales") +
  labs(y = "Global sales",
       x = "Regional sales") +
  geom_rect(aes(xmin = 0, xmax = 15, ymin = 9.6, ymax = 10.4),
    fill = NA, color = "orange", size = 1.5, linetype=2)

# => NA sales make up a higher proportion of total sales than EU sales
# => orange box identifies that Global sales of £10 correspond to:
# £4m of NA sales and £3m of EU sales
# => also demonstrates that NA sales have a higher sales ceiling



ggplot(sales_product_platform, 
       aes(x=sum_NA_Sales, 
           y=sum_EU_Sales)) + 
  geom_point(alpha = 0.5,
             size = 2.5,
             aes(colour=Platform)) +
  xlim(0, 10) + 
  ylim(0, 10) +
  geom_abline(intercept=0, slope=1,linetype='dashed', size=1) +
  ggtitle("NA sales vs EU sales with sales parity line and platform in colour")+
  labs(x = "NA sales £m",
       y = "EU sales £m") +
  theme_minimal()
# => adding another "sales parity" line gives us a glimpse of the sales weighting for platform 
# => the next step here would be to filter out the low platform and/or product sales,
# so that the most meaningful insights can be generated

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
sales_product

# Determine a summary of the data frame.
str(sales_product)
summary(sales_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# NA_sales ~ EU_sales
cor(sales_product$sum_NA_Sales, sales_product$sum_EU_Sales)

model_NA_EU <- lm(sum_NA_Sales ~ sum_EU_Sales, data = sales_product)

model_NA_EU

summary(model_NA_EU)
# r-squared 0.3856 means EU sales account for almost 39% variability of NA sales

# Global_sales ~ EU_sales
cor(sales_product$sum_Global_sales, sales_product$sum_EU_Sales)
# correlation coefficient = 84.9%

model_Global_EU <- lm(sum_Global_sales ~ sum_EU_Sales, data = sales_product)

model_Global_EU

summary(model_Global_EU)
# r-squared 0.7201 means EU sales account for 72% variability of Global sales


# Global_sales ~ NA_sales
cor(sales_product$sum_Global_sales, sales_product$sum_NA_Sales)
# correlation coefficient = 91.6%

model_Global_NA <- lm(sum_Global_sales ~ sum_NA_Sales, data = sales_product)

model_Global_NA

summary(model_Global_NA)
# r-squared 0.8395 means NA sales account for almost 84% variability of Global sales

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# model_NA_EU
plot(sales_product$sum_EU_Sales, sales_product$sum_NA_Sales)
coefficients(model_NA_EU)

# Add line-of-best-fit.
abline(coefficients(model_NA_EU))


# model_Global_EU
plot(sales_product$sum_EU_Sales, sales_product$sum_Global_sales)
coefficients(model_Global_EU)

# Add line-of-best-fit.
abline(coefficients(model_Global_EU))


# model_Global_NA
plot(sales_product$sum_NA_Sales, sales_product$sum_Global_sales)
coefficients(model_Global_NA)

# Add line-of-best-fit.
abline(coefficients(model_Global_NA))

###############################################################################

# 3. Create a multiple linear regression model
library(dplyr)

# Select only numeric columns from the original data frame.
head(sales_product)

sales_product_numeric <- select(sales_product, -Product)
sales_product_numeric

# Multiple linear regression model.
mlr_model = lm(sum_Global_sales~sum_NA_Sales+sum_EU_Sales,
            data=sales_product_numeric)


summary(mlr_model)
# the t-values of NA and EU sales are both flagged as being highly significant
# adjusted r squared is a whopping 96.6% demonstrating strong correlation

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

observed_values <- data.frame(sum_NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08),
                              sum_EU_Sales =c(23.80, 1.56, 0.65, 0.97, 0.52))

observed_values$sum_Global_sales_pred <- predict(mlr_model, newdata = observed_values)
observed_values
# => 68.06, 7.36, 4.91, 4.76, 26.63


filter(sales, NA_Sales == '34.02' & EU_Sales == '23.8')
# global sales = 67.85
# prediction = 68.06
# accuracy = 99.6%

filter(sales, NA_Sales == '3.93' & EU_Sales == '1.56')
# global sales = 6.04
# prediction = 7.36
# accuracy = 82.1%

filter(sales, NA_Sales == '2.73' & EU_Sales == '0.65')
# global sales = 4.32
# prediction = 4.91
# accuracy = 88.0%

filter(sales, NA_Sales == '2.26' & EU_Sales == '0.97')
# global sales = 3.53
# prediction = 4.76
# accuracy = 74.0%

filter(sales, NA_Sales == '22.08' & EU_Sales == '0.52')
# global sales = 23.21
# prediction = 26.63
# accuracy = 87.2%


observed_values$sum_Global_sales <-c(67.85, 6.04, 4.32, 3.53, 23.21)
observed_values


# plot mlr
library(car)
library(gridExtra)
avPlots(mlr_model,
        grid=F,
        ID=F,
        pch=16,
        lwd=3,
        main='Multiple linear regression model predicting Global sales',
        ylab='Predicted global sales')



###############################################################################

# 5. Observations and insights
# Your observations and insights here...
## interestingly, from the samples taken, all of the predictions were **over** 
# the actual global sales. this could be due to the small sample size

# accuracy is high but this should be expected 
colSums(sales_product_numeric, na.rm=T)
885.62+578.61
1464.23/1877.81
# NA and EU sales combined account for 78% of global sales


###############################################################################
###############################################################################




