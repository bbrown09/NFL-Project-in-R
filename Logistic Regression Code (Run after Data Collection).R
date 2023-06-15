##
##  Code based from stats 330 CHD code
##
rm(list = ls())

Playoffs_final = read.csv("Playoffs_master.csv")
Playoffs_final = Playoffs_final[c(3:30, 32:46, 49:52, 55, 56)]

names(Playoffs_final)
attach(Playoffs_final)   ## this moves the Playoffs variables into the global environment

################ side-by-side box plot

boxplot(combined.epa ~ Playoffs)

################ scatterplot

library(ggplot2)

plot(combined.epa,Playoffs,pch=19,cex=.5,xlab="combined.epa",ylab="Playoffs")

plot(jitter(combined.epa,amount=.5),jitter(Playoffs,amount=.1),
     pch=19,cex=.5,xlab="combined.epa",ylab="Playoffs")


ggplot(Playoffs_final,aes(x=combined.epa,y=as.numeric(Playoffs))) + geom_jitter(width=.5,height=.1) + 
  geom_smooth()

scatter.smooth(combined.epa,Playoffs,xlab="combined.epa",ylab="Playoffs",pch=19)

ggplot(Playoffs_final,aes(x=combined.epa,y=Playoffs)) + 
  geom_smooth()+ 
  geom_jitter(width=.5,height=.1)

scatter.smooth(combined.epa,Playoffs,xlab="combined.epa",ylab="Playoffs",pch=19)


################ Table
table(combined.epa,Playoffs)


## add row and column sums

addmargins(table(combined.epa,Playoffs))

##################################################################
###################### GLM Model fitting
##################################################################

head(Playoffs_final)

logistic_reg_model = glm(Playoffs~.,data=Playoffs_final,family="binomial")

library(car)

summary(logistic_reg_model)
vif(logistic_reg_model)

##################################################################
###################### Variable Selection
##################################################################

library(bestglm)

data.frame(colnames(Playoffs_final))
Playoffs_small = Playoffs_final[c(1, 6, 8, 19, 29, 30, 36, 40, 41, 43, 47:49)]

var.select = bestglm(Playoffs_small,IC="BIC",family=binomial,
                     method = "exhaustive")
var.select$BestModel

## Fit the best model
best_model = glm(Playoffs ~ tot.PA + ps.PD + ps.Sk + ps.EXP, 
                 data=Playoffs_final, family="binomial")

##################################################################
###################### Looking at coefficients 
##################################################################

summary(best_model)


## transform coefficients

best_model$coefficients                   #### interpretted on log-odds scale
round(exp(best_model$coefficients),3)     #### interpretted on the odds scale
100 * (exp(best_model$coefficients) - 1)  #### interpretted as percent change in odds

## Calculate confidence intervals

CI = confint(best_model) 

CI                             #### interpretted on log-odds scale
round(exp(CI),3)                        #### interpretted on odds scale
round(100*(exp(CI)-1),3)               #### interpretted on percent change scale



##################################################################
## Let's set up a confusion matrix
##################################################################

pred.probs = predict.glm(best_model,type="response")
cutoff = .5 

preds = 1 * (pred.probs > cutoff)               ##### predict 
conf.mat = table(preds,Playoffs_final$Playoffs)            ##### confusion matrix
conf.mat
misclass = 1 - sum(diag(conf.mat)) / sum(conf.mat)  ### miscalssification rate
misclass       ### we are wrong 21.3% of the time using this cutoff


############################## Let's do a lot cutoffs to see what it looks like
### sequence of cutoffs

n_cutoff = 100
cutoff = seq(0.05,0.95,length=n_cutoff)
misclass = rep(0,n_cutoff)
sensitivity = rep(0,n_cutoff)
specificity = rep(0,n_cutoff)

for(i in 1:n_cutoff){
  preds = 1 * (pred.probs > cutoff[i])               ##### predict 
  conf.mat = table(preds,Playoffs_final$Playoffs)                  ##### get confusion matrix
  misclass[i] = 1 - sum(diag(conf.mat))/sum(conf.mat)    #### get misclassification rate
  sensitivity[i] = conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2]) 
  specificity[i] = conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1]) 
}

plot(cutoff,misclass,type="l",ylab="Misclassification Rate",xlab="Cutoff")
plot(cutoff,sensitivity,type="l",ylab="sensitivity",xlab="Cutoff")
plot(cutoff,specificity,type="l",ylab="specificity",xlab="Cutoff")
plot(cutoff,specificity + sensitivity,type="l",ylab="specificity + sensitivity",xlab="Cutoff")

cutoff[which.min(misclass)]  ### the cutoff that minimizes missclassification rate
cutoff[which.max(specificity + sensitivity)]  ### the cutoff that minimizes missclassification rate


## Confusion matrix
cutoff_use = cutoff[which.min(misclass)]
pred_use = pred.probs > cutoff_use
conf.mat = table(pred_use,Playoffs)
conf.mat


addmargins(table(pred_use,Playoffs))

### ### ### ### ### ### ### ### ### ### ### ### ### 
###  Model Comparison/Performance Metrics
### ### ### ### ### ### ### ### ### ### ### ### ### 

### sensitivity

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2])    ## TP/(TP + FN)

### specificity 

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1])    ## TN/(FP + TN)

###  Positive predictive value

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[2,1])    ## TP/(FP + TP)

###  Negative predictive value

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[1,2])    ## TN/(FN + TN)

### percent predicted correctly

sum(diag(conf.mat))/sum(conf.mat)


### Brier Score

Playoffs_number = as.numeric(Playoffs) 
Playoffs_number

## model based estimate
mean((pred.probs - Playoffs_number)^2)

## naive sample mean 
mean((mean(Playoffs_number) - Playoffs_number)^2)

## naive coin flip estimate
mean((0.5 - Playoffs_number)^2)


## AUC

library(pROC)

my.roc = roc(Playoffs,pred.probs)
plot(my.roc,legacy.axes=TRUE, main = "AUC")
auc(my.roc)

## Psuedo R^2  --- Deviance = -2 * log-likelihood

1 - best_model$deviance/best_model$null.deviance  ## our model vs. intercept only.

## Check VIFs
library(car)

vif(best_model)

vif(logistic_reg_model)


## ## ## ## ## ## ## ## ## ## ## 
## Use test set for validations
## ## ## ## ## ## ## ## ## ## ## 
n.test = 100

test.obs = sample(1:nrow(Playoffs_final),n.test)

test.data = Playoffs_final[test.obs,]
train.data = Playoffs_final[-test.obs,]

train.mod = glm(Playoffs ~ tot.PA + ps.PD + ps.Sk+ ps.EXP, data=train.data, family="binomial")


test.preds = predict.glm(train.mod,newdata = test.data, type="response")

test.class = ifelse(test.preds > 0.5, 1, 0)

conf.mat = table(test.data$Playoffs,test.class) ##True Class is column

conf.mat = addmargins(conf.mat)

conf.mat

auc(roc(test.data$Playoffs,test.preds)) 

### sensitivity

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[1,2])    ## TP/(TP + FN)

### specificity 

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[2,1])    ## TN/(FP + TN)

###  Positive predictive value

conf.mat[2,2] / (conf.mat[2,2] + conf.mat[2,1])    ## TP/(FP + TP)

###  Negative predictive value

conf.mat[1,1] / (conf.mat[1,1] + conf.mat[1,2])    ## TN/(FN + TN)

### percent predicted correctly

sum(diag(conf.mat))/sum(conf.mat)



# graphs for powerpoint

library(popbio)
logi.hist.plot(ps.Cmp, Playoffs, boxp = FALSE, type = "hist", col = "blue",
               xlabel = "Passes Completed" , ylabel = "Playoffs",
               mainlabel = "Correlation between Passes Completed and Playoff Appearance")
logi.hist.plot(ps.PD, Playoffs, boxp = FALSE, type = "hist", col = "blue",
               xlabel = "Passes Deflected" , ylabel = "Playoffs",
               mainlabel = "Correlation between Passes Deflected and Playoff Appearance")

# predicted probabilities chart/how good is our logistic regression?

predicted.data <- data.frame(
  probability.of.playoffs=logistic_reg_model$fitted.values,
  Playoffs)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.playoffs, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.playoffs)) +
  geom_point(aes(color=Playoffs), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of making playoffs")

