# clean the environment
clean = TRUE

if (clean){
  rm(list=ls())
}

# import useful libraries
library(readr)
library(My.stepwise)
library(ggplot2)
library(dplyr)
library(My.stepwise)
library(goodness_of_fit)
library(AER) # for dispersion test 


# read the data
westnilesc <- read.table('westnilesc.csv', 
                         header=FALSE)

colnames(westnilesc) <- c('county',
                          'bird_cases',
                          'equine_cases',
                          'farms',
                          'area',
                          'population',
                          'human_density',
                          'PBR',
                          'PER')

head(westnilesc)

# add a column to the dataframe based on the HD class
breakpoints <- c(40,134)

westnilesc$HD_class <- cut(westnilesc$human_density, breaks = c(-Inf, breakpoints, Inf), 
                           labels = c("low", "medium", "high"), include.lowest = TRUE)




# EDA ---------------------------------------------------------------------

# Univariate numerical statistics
summary(westnilesc[c("equine_cases", "bird_cases", "farms", "population", "human_density")])

ggplot(westnilesc, aes(equine_cases, fill = HD_class)) +
  geom_histogram(binwidth=.5, position="dodge")

with(westnilesc, tapply(equine_cases, HD_class, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

# Correlation matrix
cor(westnilesc[c("equine_cases", "bird_cases", "farms", "human_density")])

library(corrplot)
cor_matrix <- cor(westnilesc[c("equine_cases", "bird_cases", "farms", "human_density")])
corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45)


# Scatter plot of Equine cases vs Positive bird rate
ggplot(westnilesc, aes(x = PBR, y = equine_cases)) + 
  geom_point() + 
  xlab("Positive bird rate") + 
  ylab("Equine cases")

# Histogram of Human density
ggplot(data = westnilesc, aes(x = county, y = human_density)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  xlab("County") + 
  ylab("Human density") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Box plot of Equine cases vs Positive Equine Rate
ggplot(westnilesc, aes(x = PER, y = equine_cases)) + 
  geom_boxplot(color = "black", fill = "lightblue") + 
  xlab("Positive Equine Rate") + 
  ylab("Equine cases")



# Poisson Regression Implementation ---------------------------------------

# selection of variables
variable.list <- c("human_density", "PBR")
My.stepwise.glm(Y = 'equine_cases', variable.list, in.variable = "NULL", data=westnilesc, sle = 0.15,
                sls = 0.15, myfamily = 'poisson', myoffset = 'farms')


m1 <- glm(equine_cases ~ PBR + human_density + PBR*human_density + offset(log(farms)),
          family="poisson", data=westnilesc)

options(scipen = 4)
summary(m1)

dispersiontest(m1, alternative = 'greater')

# what if we try to remove the outlier? : overdispersion gets worse...

westnilesc_no_out <- westnilesc[-26,]

m1_no_out <- glm(equine_cases ~ PBR + human_density + PBR*human_density + offset(log(farms)),
          family="poisson", data=westnilesc_no_out)

dispersiontest(m1_no_out, alternative = 'greater')

qpoisson.model1<-glm(equine_cases ~ PBR + human_density + PBR*human_density + offset(log(farms)),
                     family="quasipoisson", data=westnilesc)
summary(qpoisson.model1)
dispersiontest(m1, trafo = 1)

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
                          p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# from the test above, we can evince that data does not fit the model well

#MODEL ASSESSMENT
residuals <- residuals(m1)
plot(fitted(m1), residuals, type = "p", xlab = "Fitted values", ylab = "Residuals")

hist(residuals, main = "Residuals Histogram")
density_res <- density(residuals)
plot(density_res, main = "Residuals Density Plot")

lambda <- mean(westnilesc$equine_cases)  # Calculate the mean of the response variable
expected_quantiles <- qpois(ppoints(length(residuals)), lambda)
qqplot(expected_quantiles, residuals, main = "Q-Q Plot")

# Calculate the Pearson residuals
pearson_resid <- residuals(m1, type = "pearson")
# Calculate the Pearson chi-square statistic
pearson_chi_sq <- sum(pearson_resid^2)
# Obtain the residual degrees of freedom
df_resid <- df.residual(m1)
# Estimate the dispersion parameter
dispersion <- pearson_chi_sq / df_resid
# Print the estimated dispersion parameter
print(dispersion)



# Create a scatterplot for (PBR, PER)
ggplot(data = westnilesc, aes(x = PBR, y = PER)) +
  geom_point()
  
# check goodness of fit:
# Sort the counties by population density
sorted <- westnilesc[order(westnilesc$human_density),]

# Partition the counties into five groups of similar density
cut_points <- quantile(sorted$human_density, probs = seq(0, 1, length.out = 6))
density_groups <- cut(sorted$human_density, breaks = cut_points, labels = FALSE)
density_groups[1] <- 1


# Compute the expected number of equids per group
expected <- rep(0, 5)
for (i in 1:5) {
  group_data <- sorted[density_groups == i, ]
  expected[i] <- sum(predict(m1, newdata = group_data, type = "response"))
}
expected
# Compute the observed number of equids per group
observed <- rep(0, 5)
for (i in 1:5) {
  group_data <- sorted[density_groups == i, ]
  observed[i] <- sum(group_data$equine_cases)
}

# Perform the chi-square test
chisq <- sum((observed - expected)^2 / expected)
df <- length(expected) - 1
pval <- pchisq(chisq, df = df)
pval

# their pval
sum((c(7.901, 7.505, 6.469, 20.712, 11.311)- c(13, 3, 7, 22, 9))^2/c(7.901, 7.505, 6.469, 20.712, 11.311))
pchisq(6.590751, 4)
  

# Model Assumptions -------------------------------------------------------

###### 1. count outcome Poisson 

# we can verify this using histogram and qqplot. Also statistical test?

# qq plot + histogram 

library('fitdistrplus')
plot(fitdist(westnilesc$equine_cases,"pois"))

# stat test: ho già fattop il chi quadro sopra


##### 2. independent obs 

# scatterplot

# Create a data frame with the fitted values and residuals
df <- data.frame(fitted.values = m1$fitted.values, residuals = residuals(m1, type = "pearson"))

# Create the plot
ggplot(df, aes(x = fitted.values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

# test
# Calculate the deviance and degrees of freedom
deviance <- sum(m1$deviance)
df <- m1$df.residual

# Calculate the p-value using the chi-squared distribution
pvalue <- 1 - pchisq(deviance, df)

# Print the results
cat("Deviance = ", deviance, ", df = ", df, ", p-value = ", pvalue, "\n")


# 3. linear relation between log count and linear predictor

# Create a data frame with the predictor variable and residuals
data <- data.frame(hd = westnilesc$human_density, residuals = residuals(m1, type = "pearson"))

# Add the fitted values to the data frame
data$fitted <- exp(predict(m1, type = "link"))

# Calculate the partial residuals by removing the effect of the other predictor variables
data$partial_resid <- residuals(update(m1, . ~ . - hd), type = "pearson")

# Create the partial residual plot
ggplot(data, aes(x = hd, y = partial_resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Partial Residual Plot", x = "hd", y = "Partial Residuals")


# 4. conditional mean = conditional variance

# Calculate the Pearson residuals
residuals <- residuals(m1, type = "pearson")

# Calculate the square root of the absolute Pearson residuals
sqrt_abs_residuals <- sqrt(abs(residuals))

# Create a data frame with the fitted values and the square root of the absolute Pearson residuals
data <- data.frame(fitted = exp(predict(m1, type = "link")), sqrt_abs_residuals)

# Create the residual plot
ggplot(data, aes(x = fitted, y = sqrt_abs_residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Square Root of Absolute Pearson Residuals")
# This will create a scatter plot with the fitted values on the x-axis and the square root of the absolute 
# Pearson residuals on the y-axis, with a smooth curve overlaid on the points. 
# The smooth curve represents the pattern in the data, and should show a roughly horizontal line with a 
# constant spread of points if the assumption of equality between the conditional mean and the conditional variance holds.
# If the spread of points around the horizontal line is not constant, or if the line has an upward or downward trend, 
# this suggests that the variance is not equal to the mean, and you may need to consider using a different modeling 
# approach, such as a negative binomial regression model.

