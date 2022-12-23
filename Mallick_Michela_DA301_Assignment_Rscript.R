## Michela Mallick - LSE Data Analytics Online Career Accelerator

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

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
setwd(dir='/Users/michelamallick/Dropbox/LSE/Course 3/LSE_DA301_assignment_files')

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Import the data set.
sales <- read.csv(file.choose(), header=T)
# Opens a window to allow the file to be selected and imported

# Print the data frame.
View(sales)
as_tibble(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_sub <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# Create a new column for sales in the rest of the world (ROW)
sales_sub["ROW_Sales"]= round(sales_sub['Global_Sales']- 
  (sales_sub['NA_Sales']+sales_sub['EU_Sales']), 2)
sales_sub <- select(sales_sub, Product, Platform, NA_Sales, EU_Sales, ROW_Sales, Global_Sales)

# View the data frame.
view(sales_sub)

# View the descriptive statistics.
summary(sales_sub)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, Global_Sales, colour=Platform, data=sales_sub)
qplot(NA_Sales, EU_Sales, colour=Platform, data=sales_sub)
qplot(Global_Sales, EU_Sales, colour=Platform, data=sales_sub)
qplot(Global_Sales, ROW_Sales, colour=Platform, data=sales_sub)

## 2b) Histograms
# Create histograms (Platform)
qplot(Platform, data=sales_sub, geom='bar')

## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, data=sales_sub, colour=I('red'), geom='boxplot')
qplot(NA_Sales, data=sales_sub, colour=I('red'), geom='boxplot')
qplot(EU_Sales, data=sales_sub, colour=I('red'), geom='boxplot')

###############################################################################

# 3. Observations and insights

## A first observation on the scatter plots suggests that the sales from the
## different regions are partially correlated.
## Boxplots generally highlights that most of the sales are made by a limited 
## of products; further explorations could give more insights.

## There is an interesting aspect that will be worth exploring;
## - Platforms seem to account for quite different distribution of product sales 
## between regions, meaning that regions seem to have preferred platforms, 
## hence development and distribution could be optimised

###############################################################################
###############################################################################

# Week 5 assignment: Cleaning and maniulating data using R

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
head(sales_sub)

# Check output: Determine the min, max, and mean values.
sales_sub_stat <- group_by(sales_sub) %>% summarise(min_NA_Sales=min(NA_Sales),
                                                    min_EU_Sales=min(EU_Sales),
                                                    min_ROW_Sales=min(ROW_Sales),
                                                    min_Global_Sales=min(Global_Sales),
                                                    max_NA_Sales=max(NA_Sales),
                                                    max_EU_Sales=max(EU_Sales),
                                                    max_ROW_Sales=max(ROW_Sales),
                                                    max_Global_Sales=max(Global_Sales),
                                                    mean_NA_Sales=mean(NA_Sales),
                                                    mean_EU_Sales=mean(EU_Sales),
                                                    mean_ROW_Sales=mean(ROW_Sales),
                                                    mean_Global_Sales=mean(Global_Sales),)

# View the descriptive statistics.
summary(sales_sub_stat)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_sub_product <- sales_sub %>% group_by(Product) %>%
  summarise(NA_Sales=sum(NA_Sales), EU_Sales=sum(EU_Sales), 
            ROW_Sales=sum(ROW_Sales), Global_Sales=sum(Global_Sales))

# View the data frame.
view(sales_sub_product)

# Explore the data frame.
summary(sales_sub_product)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
ggplot(sales_sub_product,
       mapping=aes(x=Product, y=Global_Sales)) +
  geom_point()

ggplot(sales_sub_product,
       mapping=aes(x=NA_Sales, y=Global_Sales)) +
  geom_point()

ggplot(sales_sub_product,
       mapping=aes(x=EU_Sales, y=Global_Sales)) +
  geom_point()

# Create histograms.
ggplot(sales_sub_product, aes(x = Global_Sales)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)
ggplot(sales_sub_product, aes(x = NA_Sales)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)
ggplot(sales_sub_product, aes(x = EU_Sales)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)
ggplot(sales_sub_product, aes(x = ROW_Sales)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)

# Create boxplots.
ggplot(sales_sub_product,aes(x=Global_Sales)) + geom_boxplot()
ggplot(sales_sub_product,aes(x=NA_Sales)) + geom_boxplot()
ggplot(sales_sub_product,aes(x=EU_Sales)) + geom_boxplot()
ggplot(sales_sub_product,aes(x=ROW_Sales)) + geom_boxplot()

# Top 10 product per region
# NA
top10_NA<- sales_sub_product %>% arrange(., desc(NA_Sales)) %>% 
  select(Product, NA_Sales)
top10_NA<- top10_NA[1:10, ]
top10_NA
# EU
top10_EU<- sales_sub_product %>% arrange(., desc(EU_Sales)) %>% 
  select(Product, EU_Sales)
top10_EU<- top10_EU[1:10, ]
top10_EU
# ROW
top10_ROW<- sales_sub_product %>% arrange(., desc(ROW_Sales)) %>% 
  select(Product, ROW_Sales)
top10_ROW<- top10_ROW[1:10, ]
top10_ROW
# Global
top10_Global<- sales_sub_product %>% arrange(., desc(Global_Sales)) %>% 
  select(Product, Global_Sales)
top10_Global<- top10_Global[1:10, ]
top10_Global
# Top 10 product globally
top_product<- intersect(intersect(top5_NA$Product, top5_EU$Product), top5_ROW$Product)
top_product
top_product_sales<- sales_sub_product[sales_sub_product$Product %in% top_product, ]
top_product_sales

top_product_sales$Percentage <- (top_product_sales$Global_Sales/
  sum(sales_sub_product$Global_Sales))*100
top_product_sales
###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
ggplot(sales_sub_product, aes(sample=Global_Sales))+stat_qq()
ggplot(sales_sub_product, aes(sample=EU_Sales))+stat_qq()
ggplot(sales_sub_product, aes(sample=NA_Sales))+stat_qq()
ggplot(sales_sub_product, aes(sample=ROW_Sales))+stat_qq()
# Curved shape of the plots indicates non-normality and right skew, which means
# the majority of sales are made by just a few product in each region. 
# A further investigation could drill down the most popular products to focus marketing resources

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_sub_product$Global_Sales)
shapiro.test(sales_sub_product$EU_Sales)
shapiro.test(sales_sub_product$NA_Sales)
shapiro.test(sales_sub_product$EU_Sales)
# P values are much less than 0.05 so we REJECT the hypothesis that sales 
# are normally distributed. This confirms the previous insight from the Q-Q Plots.

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_sub_product[2:length(sales_sub_product)])
kurtosis(sales_sub_product[2:length(sales_sub_product)])
# EU and NA present greater skew and kurtosis than ROW, meaning that as highlighted on the Q-Q Plots
# even fewer products drive the majority of the sales. 

## 3d) Determine correlation
# Determine correlation.
cor(sales_sub_product[2:length(sales_sub_product)])
# The sales columns excluding Global are moderately correlated with each other.
# Global is highly correlated with sales from all other regions but this
# is because it is a sum of all other sales.

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

mobile_platforms = c('3DS','DS','GB','GBA','PSP','PSV')
# Grouping platforms by use case
sales_sub['PlatformType'] <-
  ifelse(unlist(sales_sub['Platform']) %in% mobile_platforms,"Mobile","Home")

# Histograms showing differentiation of platform type popularity between regions
ggplot(sales_sub, aes(x = NA_Sales,fill=PlatformType)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)
ggplot(sales_sub, aes(x = ROW_Sales,fill=PlatformType)) +  
  # Add a geom layer to specify the plot type.
  geom_histogram(bins=50)
# With the above type of plots and the below box plots I can identify
# and confirm what types of platforms are more popular for each region.
# Mobile games are much more popular clearly in ROW than NA.

ggplot(sales_sub,aes(x=PlatformType,y=NA_Sales)) + geom_boxplot()
ggplot(sales_sub,aes(x=PlatformType,y=ROW_Sales)) + geom_boxplot()
# Business Insight: Focus on mobile games to target ROW market.
# The same type of analysis could be done using 'Genre' from the original
# sales dataset. Could profitability be driven by genre, in different markets?

###############################################################################

# 5. Observations and insights
# - Sales are driven (especially in EU and NA) by a limited number of products. 
#   Further exploration could determine a better focus for the target market
# - Platform type also seems to impact the pattern of sales by region; further exploration
#   could yeld more exploitable differentiation (i.e. genre)

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

# 1. Load and explor the data
# View data frame created in Week 5.
view(sales_sub_product)

# Determine a summary of the data frame.
summary(sales_sub_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns

cor(sales_sub_product[2:length(sales_sub_product)])
# Above is correlation between sales columns
# Correlation is strongest between NA Sales and Global Sales
# and it is the biggest market (almost half), therefor I choose NA as my predictor variable
# to predict global sales.

colSums(sales_sub_product[2:length(sales_sub_product)])
# from here we can see NA is the biggest market.

# Create a linear regression model on the original data.
lr_model <- lm(Global_Sales~NA_Sales,sales_sub_product)
summary(lr_model)
# NA_Sales highly significant, Adjusted R Squared 0.8385

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_sub_product$NA_Sales, sales_sub_product$Global_Sales,
     xlab = "North America Sales", ylab = "Global Sales",
     main = "Actual vs Predicted Global Sales using North America Sales")
abline(coefficients(lr_model), col='red')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

sales_sub_product_numeric <- sales_sub_product[2:length(sales_sub_product)]

# Created a dataframe after selecting only numeric columns.
cor(sales_sub_product_numeric)
# Calculated correlation between sales columns again, as asked for.
# Will now include EU_Sales in model. I will not include ROW_Sales because that
# would cause perfect multi-collinearity (Global sales is already the sum of the regions)

# Multiple linear regression model.
mlr_model <- lm(Global_Sales~NA_Sales+EU_Sales,sales_sub_product_numeric)
summary(mlr_model)
# Both NA and EU sales highly significant in model, Adjusted R Squared improved to
# 0.9664

plot(sales_sub_product_numeric$NA_Sales, sales_sub_product_numeric$Global_Sales,
     xlab = "North America Sales", ylab = "Global Sales",
     main = "Actual vs Predicted Global Sales comparing LR and MLR model")
abline(coefficients(lr_model), col='red')
abline(line(x = sales_sub_product_numeric$NA_Sales,
            predict(mlr_model,sales_sub_product_numeric)), 
       col='blue')
# The new MLR model (blue line) fits the data much better than the old LR model
# (red line).

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

sales_sub_product_numeric$predictions <- predict(mlr_model,sales_sub_product_numeric)
# Predicted my new model on the data.

NA_Sales_sum <- c(34.02,3.93,2.73,2.26,22.08)
EU_Sales_sum <- c(23.8,1.56,0.65,0.97,0.52)
# These are the observations with their NA and EU Sales values I have been asked
# to compare my model's predictions to.

predicted_data <- 
  sales_sub_product_numeric[sales_sub_product_numeric$EU_Sales %in% EU_Sales_sum,]
view(predicted_data)
# The above are paired observations (34.02 and 23.8 are from the first row of the data)
# However (NA=3.93,EU=1.56) refers to do data before aggregation, so there is no correspondence with the new dataset.

plot(predicted_data$predictions,predicted_data$Global_Sales)
abline(0,1,col='red')
# We can see from the above view and plot that the model predictions are mostly
# quite close to actual global sales. The biggest error comes from the data point
# when EU_Sales=0.52 and NA_Sales=22.1. Then the model over-predicts 26.6 while
# actual Global_Sales=23.2. 

view(predicted_data)
###############################################################################

# 5. Observations and insights
# There is a strong relationship between NA and Global Sales
# NA and Europe are partially correlated; further investigation could clarify

###############################################################################
###############################################################################