# Philip Kim
# Ecommerce Project

rm(list=ls()) #removes all previously stored variables

#---------------------------------------------------------
# IMPORT DATA 
#---------------------------------------------------------
data <- read.csv("./data/ecommerce-users")

View(data)
str(data)
summary(data)

#----------------------------------------------------------
# CREATE PLOTS AND SEARCH FOR INSIGHTS
#----------------------------------------------------------

library(ggplot2)

# correlation between time on website and yearly amount spent

ggplot(data, aes(x = Time.on.Website, y = Yearly.Amount.Spent)) +
  geom_point(color = "orange") +
  ggtitle("Time on Website against Yearly Amount Spent") +
  xlab("Time on Website") +
  ylab("Yearly Amount Spent")

# average session length against yearly amount spent

ggplot(data, aes(x = Avg..Session.Length, y = Yearly.Amount.Spent)) +
  geom_point(color = "red") +
  ggtitle("Average Session Length against Yearly Amount Spent") +
  xlab("Average Session Length") +
  ylab("Yearly Amount Spent")

cor(data$Avg..Session.Length, data$Yearly.Amount.Spent)


## pairplot of all continuous variables

pairs(data[c("Avg..Session.Length",
             "Time.on.App",
             "Time.on.Website",
             "Length.of.Membership",
             "Yearly.Amount.Spent")],
      col="orange",
      pch = 16,
      title = "Pairplot of all continuous variables"
      )


#----------------------------------------------------------
# EXPLORING THE SELECTED VARIABLE
#----------------------------------------------------------

# is the variable normally distributed
hist(data$Length.of.Membership)

ggplot(data, aes(x = Length.of.Membership)) +
  geom_histogram(
    color = "white", 
    fill = "orange",
    binwidth = 0.5,
  ) +
  xlab("Length of Membership")

boxplot(data$Length.of.Membership)

ggplot(data, aes(x = Length.of.Membership)) +
  geom_boxplot(fill = "orange") +
  xlab("Length of Membership")


#----------------------------------------------------------
# FITTING THE LINEAR MODEL
#----------------------------------------------------------

attach(data) # so we dont have to say data$variable_name, we can just say variable_name

lm_fit1 <- lm(Yearly.Amount.Spent ~ Length.of.Membership) # linear model
summary(lm_fit1)

plot(Yearly.Amount.Spent ~ Length.of.Membership)
abline(lm_fit1, col = "red")


#----------------------------------------------------------
# RESIDUALS ANALYSIS
#----------------------------------------------------------

qqnorm(residuals(lm_fit1)) # normality plot
qqline(residuals(lm_fit1), col = "red")

shapiro.test(residuals(lm_fit1)) # p-value is above 0.05, so the residuals are normal


#----------------------------------------------------------
# EVALUATION OF THE MODEL
#----------------------------------------------------------

set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data)) # returns random row numbers to be included in sample
train <- data[row.number,] # gives the actual data values for the previously recorded row numbers
test <- data[-row.number,] # gives the opposite from train

# estimate the linear fit with the training set
lm_fit0.8 <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = train)
summary(lm_fit0.8)

# predict in the test data set
prediction0.8 <- predict(lm_fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error
rmse <- sqrt(mean(err0.8^2))

# mean absolute percent error
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE = rmse, MAPE = mape, R2 = summary(lm_fit0.8)$r.squared)



#----------------------------------------------------------
# MULTIPLE REGRESSION
#----------------------------------------------------------

attach(data)

lm_fit <- lm(Yearly.Amount.Spent ~ Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership)
summary(lm_fit)



#----------------------------------------------------------
# EVALUATION OF MULTIPLE REGRESSION
#----------------------------------------------------------

set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data)) # returns random row numbers to be included in sample
train <- data[row.number,] # gives the actual data values for the previously recorded row numbers
test <- data[-row.number,] # gives the opposite from train

# estimate the linear fit with the training set
multi_lm_fit0.8 <- lm(Yearly.Amount.Spent ~ Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership, data = train)
summary(multi_lm_fit0.8)

# predict in the test data set
prediction0.8 <- predict(multi_lm_fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error
rmse <- sqrt(mean(err0.8^2))

# mean absolute percent error
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE = rmse, MAPE = mape, R2 = summary(multi_lm_fit0.8)$r.squared)



#----------------------------------------------------------
# RUN REGRESSION AGAIN WITH TIME SPENT ON WEBSITE REMOVED
#----------------------------------------------------------

newdata <- subset(data, select = -c(6))
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data)) # returns random row numbers to be included in sample
train <- data[row.number,] # gives the actual data values for the previously recorded row numbers
test <- data[-row.number,] # gives the opposite from train

# estimate the linear fit with the training set
new_multi_lm_fit0.8 <- lm(Yearly.Amount.Spent ~ Avg..Session.Length + Time.on.App + Length.of.Membership, data = train)
summary(new_multi_lm_fit0.8)

# predict in the test data set
prediction0.8 <- predict(new_multi_lm_fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error
rmse <- sqrt(mean(err0.8^2))

# mean absolute percent error
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE = rmse, MAPE = mape, R2 = summary(new_multi_lm_fit0.8)$r.squared)


#----------------------------------------------------------
# CONCLUSIONS:
  # the length of membership was the most significant predictor for yearly amount spent
  # using multiple regression decreased the RMSE and MAPE, as well as increasing the R2 value
  # removing the time spent on website did not affect the error/R2 at all, showing that it was an insignificant predictor. 
#----------------------------------------------------------


