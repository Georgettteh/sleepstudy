

# Load required libraries
library(lme4)
library(ggplot2)
install.packages("lattice")
#1a
#Load the "sleepstudy" dataset built in R
data("sleepstudy")
library(psych)
describe(sleepstudy)

#1b.
#Explore the structure of the dataset

str(sleepstudy)
# The dataset has 180 observations of 3 variables
?sleepstudy
#Reaction :Average reaction time (ms)
#Days: Number of days of sleep deprivation
#Subject: Subject number on which the observation was made.


#1c.
# Visualize the data
# a scatter plot of Reaction vs. Days


ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  labs(title = "Reaction Time vs. Days",
       x = "Days of Sleep Deprivation",
       y = "Reaction Time")





#2a.
#  summary statistics
summary(sleepstudy[, c("Reaction", "Days")])

#2b.
# Create to better understand the distribution of reaction times
#over different days

ggplot(sleepstudy, aes(x = as.factor(Days), y = Reaction)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Reaction Times by Days",
       x = "Days of Sleep Deprivation",
       y = "Reaction Time")




#3a.
#Fit a mixed-effects model

require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")
(M1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy, subset=Days>=2))
## independent model
(M2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy, subset=Days>=2))

anova(M1, M2)
summary(M2)

#4.
# print model summary
summary(M1)




# 6.  Analysis of Residual
# Conduct residual analysis:plot residuals vs. fitted values
# Create a scatterplot of residuals vs. fitted values
 

# Fitting the mixed-effects model

# Obtain model residuals
residuals <- resid(M1)

# Checking for normality of residuals with  Q-Q plot
qqnorm(residuals)
qqline(residuals)

#Assuming fm1 is our selected model for validation
residuals_M1 <- residuals(M1)
fitted_values_M1 <- fitted(M1)

# Create a scatterplot of residuals vs. fitted values
plot(fitted_values_M1, residuals_M1, 
     main = "Residuals vs. Fitted",
     xlab = "Fitted Values", 
     ylab = "Residuals",
     col = "blue")  # Adjust the color for better visibility

abline(h = 0, col = "red", lty = 2)









