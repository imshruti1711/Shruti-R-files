x=rnorm(100)
e=rnorm(100)
y=seq(100)
y.fit=lm(y~ x+poly(x,2)+poly(x,3)+e)
df=data.frame(y=y,x=x,x2=x^2,x3=x^3,x4=x^4,x5=x^5,x6=x^6,x7=x^7,x8=x^8,x9=x^9,x10=x^10)
library(leaps)
regfit_full = regsubsets(y ~ ., data = df,nvmax=10)
reg_summary = summary(regfit_full)
names(reg_summary)
reg_summary$rsq
par(mfrow=c(1,1))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS") #plot of rss

#plot and best value using adj R square
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)
(best_model = which.max(reg_summary$adjr2))  #Ans: 5

#plot and best value using Cp
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
(best_model = which.min(reg_summary$cp))  #Ans: 4
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)

#plot and best value using BIC
plot(reg_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
(best_model = which.min(reg_summary$bic))  #Ans : 1
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)

#FSS
regfit_fwd = regsubsets(y ~ .,
                        data = df,
                        nvmax = 10,
                        method = "forward")
summary(regfit_fwd)
plot(regfit_fwd, scale = "r2")
plot(regfit_fwd, scale = "adjr2")
plot(regfit_fwd, scale = "Cp")
plot(regfit_fwd, scale = "bic")
#BSS
regfit_bwd = regsubsets(y ~ .,
                        data = df,
                        nvmax = 10,
                        method = "backward")
summary(regfit_bwd)
plot(regfit_bwd, scale = "r2")  #all variables are being selected
plot(regfit_bwd, scale = "adjr2")  #6 
plot(regfit_bwd, scale = "Cp")   #6
plot(regfit_bwd, scale = "bic") #2

#lasso model
install.packages('glmnet')
library(glmnet)
x <-model.matrix(y~., data=df)[,-1]
y <-df$y
fit.lasso <-glmnet(x,y)
plot(fit.lasso, xvar="lambda", label= TRUE)
plot(fit.lasso, xvar="dev", label= TRUE)
cv.lasso <-cv.glmnet(x, y)
plot(cv.lasso)
## coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.lasso)
## coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y, lambda=cv.lasso$lambda.min))

y=0.01+(0.05*x^7)+e
regfit_bwd = regsubsets(y ~ x7+e,
                        data = df,
                        method = "backward")
summary(regfit_bwd)
plot(regfit_bwd, scale = "r2")  #all variables are being selected
plot(regfit_bwd, scale = "adjr2")  #only x7 and intercept 
plot(regfit_bwd, scale = "Cp")   #only x7 and intercept 
plot(regfit_bwd, scale = "bic") #only x7 and intercept 
