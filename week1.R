data(coagulation, package='faraway') #it will load the data coagulation from faraway package
plot(coag~diet, data=coagulation) #it will plot the variance bar plot diet over coag

# Histogram
small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)
hist(small.size.dataset, xlab='My data points', main='Histogram of my data', freq=F, col='green', breaks=10)
lines(density(small.size.dataset), col='red', lwd=5)

# Scatter plot
set.seed(2016)
Test_1_scores=round(rnorm(50, 78, 10))
Test_2_scores=round(rnorm(50, 78, 14))
plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)',
              xlab='Test_1_scores', ylab='Test 2 scores', , col='blue')

#Simple linear regression
# Y_i = linear_model_plus_noise = (B_0 + B_1*x_i) + e_i
# e_i is noise term (because of measurement error or lack of knowledge of other important factors or ...)
# Vanilla Assumptions: 1)error simply would be normally distributed in an average zero
#    2) same variance (homoscedastic) 3) unrelated to eachother, independant accross observations
co2.linear.model = lm(co2~time(co2)) # it means co2 as a function of time
#Coefficients:
#(Intercept)    time(co2)  
#-2249.774        1.307 -> This is the slope 
plot(co2, main='Atmospheric CO2 Concentration with Fitted Line')
abline(co2.linear.model)
# Base assumption: the residuals should be normaly distributed
co2.residuals = resid(co2.linear.model)
hist(co2.residuals, main = "Histogram of Residuals", breaks=20)
# Histogram not necessarly help you to find normality
qqnorm(co2.residuals) # if it was normal we would see straigt line
qqline(co2.residuals, col='red') # we will see systematic shifts/deviations from normality
plot(co2.residuals~time(co2))
plot(co2.residuals~time(co2), xlim=c(1960,1963), main="Zoomed Residuals")

plot(extra~group, data=sleep, main="Extra Sleep in Gossett Data by Group")
attach(sleep)
extra.1 <- extra[group==1]
extra.2 <- extra[group==2]
# t-Test
t.test(extra.1, extra.2, paired = T, alternative = "two.sided")
# t = -4.06 is rather big value p-val = 0.0028 is less than 0.05, alternative hypo is there is a difference between 2 drogs
# the 95% conf-interval doesn't include 0
# Null-hypo: mean response is equal for both drugs
# alpha = prob( type1 error) = 0.05 or 0.01 prob of rejecting the true null-hypo
# t = (avg_of_diffs(or diff_of_avgs) - 0 (null-hypo value)) / (sample_std_of_diffs/ qrt(N) ) = -4.06
# p-val is the likelihood of seeing data this extreme under null-hypo: 2*tailprob(-4.06) = 0.0028
# p < alpha reject p>alpha not reject
#Confidence Intervals: where we believe the actual mean would be (Estimate +_ Table-val * (estimated_std_err))
# Standard_error = standard deviation of sampling distribution

#Correlation Func.
attach(trees)
pairs(trees, pch=21, bg=c('red')) #shows the correlation between features
cov(trees) # we will see that the cov between Volume and Height are even bigger (because of units)
cor(trees) # no matter what units we have (it has scaling)
