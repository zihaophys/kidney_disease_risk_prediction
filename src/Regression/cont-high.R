# packages
library(Matrix)
library(glmnet)
library(gplots)
library(ROCR)

# input data
setwd("~/Desktop/courses/SL/medical data")
cont_high <- read.csv("cont-high.csv",header=TRUE)

# Lasso-Logit model
y <- cont_high[, 1]
x <- as.matrix(cont_high[,-1])
# select the model
set.seed(11) #set random seed in order to repeat the result
fit_cv <- cv.glmnet(x, y, alpha=1.0, family = 'binomial')
plot(fit_cv)
# fit the model
fit <- glmnet(x, y, alpha=1.0, family = 'binomial')

# model result
Coefficients <- coef(fit_cv)
Active.Index <- which(Coefficients != 0)
coef_fit <- data.frame('varibles' = rownames(Coefficients)[Active.Index],
                       'coef' = Coefficients[Active.Index])
coef_fit

# plot
get_plot<- function(the_fit,the_fit_cv,the_lambda){
  Coefficients <- coef(the_fit, s = the_lambda)
  Active.Index <- which(Coefficients != 0)
  coe_all <- coef(the_fit, s = the_fit_cv$lambda)
  coe <- coe_all[Active.Index[-1],] # not include intercept
  ylims=c(-max(abs(coe)),max(abs(coe)))
  sp <- spline(log(the_fit_cv$lambda),coe[1,],n=1000)
  plot(sp,type='l',col =1,lty=1, 
       ylim = ylims,ylab = 'Coefficient', xlab = 'log(lambda)') 
  abline(h = 0)
  for(i in c(2:nrow(coe))){
    lines(spline(log(the_fit_cv$lambda),coe[i,],n=1000),
          col =i,lty=i)
  }
  legend("bottomright",legend=rownames(coe),col=c(1:nrow(coe)),
         lty=c(1:nrow(coe)),
         cex=0.5)
  title("Lasso Path")
}
get_plot(fit,fit_cv,fit_cv$lambda.min)

# model evaluation
get_confusion_stat <- function(pred,y_real,threshold=0.5){
  # statistics
  pred_new <- as.integer(pred>threshold) 
  tab <- table(pred_new,y_real)
  TP <- tab[2,2]
  TN <- tab[1,1]
  FP <- tab[2,1]
  FN <- tab[1,2]
  accuracy <- round((TP+TN)/(TP+FN+FP+TN),4)
  recall_sensitivity <- round(TP/(TP+FN),4)
  precision <- round(TP/(TP+FP),4)
  specificity <- round(TN/(TN+FP),4)
  re <- list('Confusion_Matrix'=tab,
             'Statistics'=data.frame(value=c('accuracy'=accuracy,
                                             'recall_sensitivity'=recall_sensitivity,
                                             'precision'=precision,
                                             'specificity'=specificity)))
  return(re)
}
pred <- predict(fit, newx = x, s = fit_cv$lambda.min, type = 'response')
get_confusion_stat(pred, y)

# factor analysis
library(psych)
library(GPArotation)
correlations <- cor(cont_high[Active.Index[-1]])
fa.parallel(correlations, n.obs = 38, fa = "fa")
fa <- fa(correlations, nfactors = 2, rotate = "promax")
factor.plot(fa, labels = rownames(fa$loadings))
