# packages
library(Matrix)
library(glmnet)
library(gplots)
library(MASS)

# input data
setwd("~/Desktop/courses/SL/medical data")
full <- read.csv("full.csv",header=TRUE)

# Lasso-Logit model
y <- full[, 1]
x <- as.matrix(full[, -1])
# select the model
set.seed(11) #set random seed in order to repeat the result
fit_cv <- cv.glmnet(x, y, alpha=1.0, family = 'multinomial')
plot(fit_cv)
# fit the model
fit <- glmnet(x, y, alpha=1.0, family = 'multinomial')

# model result
Coef_cont <- coef(fit, s = fit_cv$lambda.min)[['Control']]
Active.Index1 <- which(Coef_cont != 0)
cont <- data.frame('varibles' = rownames(Coef_cont)[Active.Index1],
                  'coef' = Coef_cont[Active.Index1])
Coef_low <- coef(fit, s = fit_cv$lambda.min)[['Low-risk']]
Active.Index2 <- which(Coef_low != 0)
low <- data.frame('varibles' = rownames(Coef_low)[Active.Index2],
                  'coef' = Coef_low[Active.Index2])
Coef_high<- coef(fit, fit_cv$lambda.min)[['High-risk']]
Active.Index3 <- which(Coef_high != 0)
high <- data.frame('varibles' = rownames(Coef_high)[Active.Index3],
                     'coef' = Coef_high[Active.Index3])
Coef_full <- list("Control"=cont, "Low-risk"=low, "High-risk"=high)

Coef_names <- c(rownames(Coef_high)[Active.Index3[-1]], rownames(Coef_low)[Active.Index2[-1]],
                rownames(Coef_cont)[Active.Index1[-1]])
Coef_names <- Coef_names[!duplicated(Coef_names)]

get_plot<- function(the_fit,the_fit_cv,the_lambda){
  Coefficients <- coef(the_fit, s = the_lambda)[["Control"]]
  Active.Index <- which(Coefficients != 0)
  coe_all <- coef(the_fit, s = the_fit_cv$lambda)[["Control"]]
  coe <- coe_all[Active.Index[-1],] # not include intercept
  ylims=c(-max(abs(coe)),max(abs(coe)))
  sp <- spline(log(the_fit_cv$lambda),coe[1,],n=1000)
  plot(sp,type='l',col =1,lty=1, 
       ylim = ylims,ylab = 'Coefficient', xlab = 'log(lambda)')
  title("Control")
  abline(h = 0)
  abline(v = log(the_lambda), lty = 2)
  for(i in c(2:nrow(coe))){
    lines(spline(log(the_fit_cv$lambda),coe[i,],n=1000),
          col =i,lty=i)
  }
  legend("bottomright",legend=rownames(coe),col=c(1:nrow(coe)),
         lty=c(1:nrow(coe)),
         cex=0.5)
  Coefficients <- coef(the_fit, s = the_lambda)[["Low-risk"]]
  Active.Index <- which(Coefficients != 0)
  coe_all <- coef(the_fit, s = the_fit_cv$lambda)[["Low-risk"]]
  coe <- coe_all[Active.Index[-1],] # not include intercept
  ylims=c(-max(abs(coe)),max(abs(coe)))
  sp <- spline(log(the_fit_cv$lambda),coe[1,],n=1000)
  plot(sp,type='l',col =1,lty=1, 
       ylim = ylims,ylab = 'Coefficient', xlab = 'log(lambda)') 
  title("Low risk")
  abline(h = 0)
  abline(v = log(the_lambda), lty = 2)
  for(i in c(2:nrow(coe))){
    lines(spline(log(the_fit_cv$lambda),coe[i,],n=1000),
          col =i,lty=i)
  }
  legend("bottomright",legend=rownames(coe),col=c(1:nrow(coe)),
         lty=c(1:nrow(coe)),
         cex=0.5)
  Coefficients <- coef(the_fit, s = the_lambda)[["High-risk"]]
  Active.Index <- which(Coefficients != 0)
  coe_all <- coef(the_fit, s = the_fit_cv$lambda)[["High-risk"]]
  coe <- coe_all[Active.Index[-1],] # not include intercept
  ylims=c(-max(abs(coe)),max(abs(coe)))
  sp <- spline(log(the_fit_cv$lambda),coe[1,],n=1000)
  plot(sp,type='l',col =1,lty=1, 
       ylim = ylims,ylab = 'Coefficient', xlab = 'log(lambda)')
  title("High risk")
  abline(h = 0)
  abline(v = log(the_lambda), lty = 2)
  for(i in c(2:nrow(coe))){
    lines(spline(log(the_fit_cv$lambda),coe[i,],n=1000),
          col =i,lty=i)
  }
  legend("bottomright",legend=rownames(coe),col=c(1:nrow(coe)),
         lty=c(1:nrow(coe)),
         cex=0.5)
}
get_plot(fit,fit_cv,fit_cv$lambda.min)



