#Setting directory

setwd("C:/Users/daily/Desktop/Bayesian-Statistics-Techniques-Models")

#Importing Data

source_data <- read.csv(file = "C:/Users/daily/Desktop/Bayesian-Statistics-Techniques-Models/MLB FA.csv")

#Creating new columns in data

source_data$MixedWAR_1 = (source_data$fWAR_1 + source_data$bWAR_1) / 2
source_data$MixedWAR_2_3 = (source_data$fWAR_2_3 + source_data$bWAR_2_3) / 2
source_data$AAV = (source_data$Dollars / source_data$Contract_Years)
source_data$PV_AAV = (source_data$PV_Dollars / source_data$Contract_Years)
source_data$log_AAV = log(source_data$AAV)
source_data$log_PV_AAV = log(source_data$PV_AAV)
source_data$MVP_Candidate = ifelse(source_data$MVP_Pts>0, 1, 0)
source_data$Contract_Category <- ifelse(source_data$Contract_Years > 6, 5,
ifelse(source_data$Contract_Years > 4, 4,
ifelse(source_data$Contract_Years > 2, 3,
ifelse(source_data$Contract_Years == 2, 2, 1))))

#Splitting data to test model later

library("dplyr")

model_data <- filter(source_data, Year < 2020)

test_data <- filter(source_data, Year == 2020)

#non-informative prior according to R

lmod <- lm(data = model_data, log_PV_AAV ~ MixedWAR_1 + MixedWAR_2_3 + MVP_Candidate + All_Star + Pitcher)
summary(lmod)

#Model 1

library("rjags")

mod1_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*MixedWAR_1[i] + b[3]*MixedWAR_2_3[i] + b[4]*MVP_Candidate[i] + b[5]*All_Star[i] + b[6]*Pitcher[i]
}

for (i in 1:6) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt( 1.0 / prec )
} "

set.seed(73)

data1_jags = list(y=model_data$log_PV_AAV, n=nrow(model_data), MixedWAR_1=model_data$MixedWAR_1,
MixedWAR_2_3=model_data$MixedWAR_2_3, MVP_Candidate=model_data$MVP_Candidate, All_Star = model_data$All_Star, Pitcher = model_data$Pitcher)

params1 = c("b", "sig")

inits1 = function() {
inits = list("b"=rnorm(6,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)

update(mod1, 1e3) # burn-in

mod1_sim = coda.samples(model=mod1,
variable.names=params1,
n.iter=5e3)

mod1_csim = as.mcmc(do.call(rbind, mod1_sim)) # combine multiple chains

plot(mod1_sim)

autocorr.diag(mod1_sim)

autocorr.plot(mod1_sim)

effectiveSize(mod1_sim)

X = cbind(rep(1.0, data1_jags$n), data1_jags$MixedWAR_1, data1_jags$MixedWAR_2_3, data1_jags$MVP_Candidate, data1_jags$All_Star, data1_jags$Pitcher)
head(X)

pm_params1 = colMeans(mod1_csim) # posterior mean

yhat1 = drop(X %*% pm_params1[1:6])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index

qqnorm(resid1) # checking normality of residuals

plot(predict(lmod), resid(lmod)) # to compare with reference linear model
plot(yhat1, resid1)

dic.samples(mod1, n.iter=1e3)

summary(mod1_csim)

#Model 2

mod2_string = " model {
for (i in 1:n) {
y[i] ~ dnorm(mu[i], prec)
mu[i] = a[Contract_Category[i]] + b[1]*MixedWAR_1[i] + b[2]*MixedWAR_2_3[i] + b[3]*MVP_Candidate[i] + b[4]*All_Star[i] + b[5]*Pitcher[i]
}

for (j in 1:max(Contract_Category)){
a[j] ~ dnorm(a0, prec_a)
}

a0 ~ dnorm(0.0, 1.0/1.0e6)
prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
tau = sqrt(1.0 / prec_a)

for (j in 1:5) {
b[j] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt( 1.0 / prec )
} "

set.seed(73)

data2_jags = list(y=model_data$log_PV_AAV, n=nrow(model_data), MixedWAR_1=model_data$MixedWAR_1,
MixedWAR_2_3=model_data$MixedWAR_2_3, MVP_Candidate=model_data$MVP_Candidate, All_Star = model_data$All_Star, Pitcher = model_data$Pitcher, Contract_Category = model_data$Contract_Category)

params2 = c("a0", "a", "b", "sig", "tau")

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)

update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains

plot(mod2_sim)

autocorr.diag(mod2_sim)

autocorr.plot(mod2_sim)

effectiveSize(mod2_sim)

pred_matrix <- data2_jags

pred_matrix$Contract_Category_1 <- ifelse(pred_matrix$Contract_Category==1, 1, 0)
pred_matrix$Contract_Category_2 <- ifelse(pred_matrix$Contract_Category==2, 1, 0)
pred_matrix$Contract_Category_3 <- ifelse(pred_matrix$Contract_Category==3, 1, 0)
pred_matrix$Contract_Category_4 <- ifelse(pred_matrix$Contract_Category==4, 1, 0)
pred_matrix$Contract_Category_5 <- ifelse(pred_matrix$Contract_Category==5, 1, 0)

X2 = cbind(pred_matrix$Contract_Category_1, pred_matrix$Contract_Category_2, pred_matrix$Contract_Category_3, pred_matrix$Contract_Category_4, pred_matrix$Contract_Category_5, rep(0.0, pred_matrix$n),
pred_matrix$MixedWAR_1, pred_matrix$MixedWAR_2_3, pred_matrix$MVP_Candidate, pred_matrix$All_Star, pred_matrix$Pitcher)

head(X2)

pm_params2 = colMeans(mod2_csim) # posterior mean

pm_params2

yhat2 = drop(X2 %*% pm_params2[1:11])
resid2 = data2_jags$y - yhat2
plot(resid2) # against data index
plot(yhat2, resid2)

qqnorm(resid2) # checking normality of residuals

dic.samples(mod2, n.iter=1e3)

summary(mod2_sim)

#Checking model results on 2020 free agents

test_data$Contract_Category1 <- ifelse(test_data$Contract_Category==1, 1, 0)
test_data$Contract_Category2 <- ifelse(test_data$Contract_Category==2, 1, 0)
test_data$Contract_Category3 <- ifelse(test_data$Contract_Category==3, 1, 0)
test_data$Contract_Category4 <- ifelse(test_data$Contract_Category==4, 1, 0)
test_data$Contract_Category5 <- ifelse(test_data$Contract_Category==5, 1, 0)

test_data_X = cbind(test_data$Contract_Category1, test_data$Contract_Category2, test_data$Contract_Category3, test_data$Contract_Category4, test_data$Contract_Category5, 0,
test_data$MixedWAR_1, test_data$MixedWAR_2_3, test_data$MVP_Candidate, test_data$All_Star, test_data$Pitcher)

yhat = drop(test_data_X %*% pm_params2[1:11])
resid = test_data$log_PV_AAV - yhat
plot(resid)
plot(yhat, resid)

test_data$yhat = yhat
test_data$resid = resid
test_data$pred = exp(yhat)
test_data$actual_resid = test_data$pred - test_data$PV_AAV
