# Autocorrelation Function (ACFs)
# Timeseries: DS collected through time which brings some correlation
# `astsa` package
# 1. jj (Johnson and Johnson Quarterly Earnings) it is already timeseries
require(astsa)
plot(jj, type='o', ylab='Earnings', xlab='Years') # both seasonal + trending (non-stationary)
plot(flu, ylab='Flu', xlab='Month') # seasonality but no big trend (non-stationary)
plot(globtemp, ylab='Temp', xlab='Years') # some trand + seasonality
plot(star, xlab = 'Days')
# Stationarity: no systematic change in mean (no trend)/ in variation/ no periodic fluctuation (seasonality)
#    we say stationary ts when it can be modeled by stationary stochastic process
#    we do some transformations to reach stationary ts because usually they are not stationary
# Random variable: a function which goes from sample space (all possible outcomes of experience) to real number (a measurable function)
#    here we start with samples and assume that they came from a random variable S then we will try to extract properties and distribution of S
# Covariance: linear dependence between tweo RV Cov(X,Y)=E[(X-mu_x)(Y-mu_y)]=Cov(Y,X) if it's symmetrical
# Stochastic Processes: collection of RVs X1,X2,X3,... Xt ~ distribution(mu_t, sig_t^2)
# Deterministic Process: we know exactly what is going to be next value (no randomness)
#    `timeseries` can assume to be a realization of a stochastic process
# Autocovariance Function: 
#    covariance of different elements in our sequence (stochastic process)
#        y(s,t)=Cov(Xs,Xt)=E[(Xs-mu_s)(Xt-mu_t)], y(t,t)=E[(Xt-mu_t)^2]=Var(Xt)=sig_t^2
#    y_k = y(t,t+k) ~ c_k because we assume we are working with stationary ts, the property of one part of ts is equal to other parts
#    y_k (gamma_k): autocovariance func and c_k: autocovariance coefficient
# Estimation of the covariance: we have paired ds (x1,y1),(x2,y2),... => s_xy = sum(from1toN)[(x_t-x_bar)(y_t-y_bar)]/(N-1) => in R: cov()
#    since we assumed the stationarity c_k is an estimation of gamma_k => c_k = sum(from1toN-k)[(x_t+k-x_bar)(y_t-y_bar)]/N in R: acf(ts, type='covariance')
prp = ts(rnorm(100))
acf(prp, type='covariance') # autocovariance coefficients not autocorrelation coefficients (in different lags)
# Autocorrelation Function:
# The autocorrelation between Xt and Xt+k is  -1<= (gamma_k/gamma_0) <=1, and since we have samples (ts) we need to estimate that r_k = c_k / c_0
# Autocorrelation coefficients at different lags: Correlogram alwauys starts from 1 r_0 = c_0/c_0 = 1   
acf(prp) # here we don't expect to see any significant difference between different lags

# Random Walk: Xt = Xt-1 + Zt (Zt = white noise ~ N(mu,sig)) as you go forward you are accumulating these noises
# E[Xt] = E[sigma(Zi)] = sigmaE[Zi] = mu*t, Var(Xt) = Var(sigma(Zi)) = sigma(Var(Zi)) = sig * t
# it is not a stationary process since we know the mean at each step 
X=NULL
X[1]=0
for(i in 2:1000){
  X[i] = X[i-1] + rnorm(1)
}
rw = ts(X)
plot(rw)
acf(rw) # high correlation in this ds and no stationarity it has trend (deltaXt=Zt) Zt is normal so random process
plot(diff(rw)) # white noise
acf(diff(rw))
# Moving Average order q: MA(q) it goes q days back
# Sample of Moving Average Process
noise=rnorm(1000)
ma_2=NULL
for(i in 3:1000){
  ma_2[i] = noise[i] + 0.7*noise[i-1] + 0.2*noise[i-2]
}
moving_average_process=ts(ma_2[3:1000])
par(mfrow=c(2,1)) # Partition output graphics as a multi frame of 2 rows and 1 column
par(mar=c(1,1,1,1))
plot(moving_average_process, main='A moving average process of order 2', ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 2') # We will see that there is a correlation between first 3 lags
# in MA(q) Autocorrelation will be cut off at lag q so if we see this pattern we can model our data with moving average process
