library(ResourceSelection)
library(car)
library(glmnet)

#--------------------------------------
# Training (75%) - Test (25%) Set Split
#--------------------------------------
set.seed(123)
index <- sample(1:dim(heart)[1], dim(heart)[1]*(3/4),replace=F)
train <- heart[index, -1]
test <- heart[-index,-1]

#--------------------------------------------
# Logistic Regression Model w/ All Predictors
#--------------------------------------------
model.full<-glm(target ~ ., data=heart,family = binomial(link="logit"))
(vif(model.full)[,3])^2
summary(model.full)

#Hosmer Lemeshow test for lack of fit.  Use as needed.  The g=10 is an option that deals with the continuous predictors if any are there.
#This should be increased with caution. 
hoslem.test(model.full$y, fitted(model.full), g=10)
# p-value of 0.02752 indicates a lack of fit

# Odds ratio metrics
# Using the summary coefficients we can generate CI for each one in the table
exp(cbind("Odds ratio" = coef(model.full), confint.default(model.full, level = 0.95)))



#This starts with a null model and then builds up using forward selection up to all the predictors that were specified in my
#main model previously.
model.null<-glm(target ~ 1, data=heart,family = binomial(link="logit"))

#------------------
# Forward Selection
#------------------
step(model.null,
     scope = list(upper=model.full),
     direction="forward",
     test="Chisq",
     data=heart)

model.fwd <- glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + exang, data = heart, family = binomial(link="logit"))
(vif(model.fwd)[,3])^2
summary(model.fwd)
hoslem.test(model.full$y, fitted(model.fwd), g=10)
# p-value of 0.05514 indicates no lack of fit at 95% confidence level?


#-------------------
# Backward Selection
#-------------------
step(model.full,
     scope = list(upper=model.null),
     direction="backward",
     test="Chisq",
     data=heart)

model.bwd <- glm(target ~ sex + cp + trestbps + exang + oldpeak + slope + ca + thal, data = heart, family = binomial(link="logit"))
(vif(model.bwd)[,3])^2
summary(model.bwd)
hoslem.test(model.full$y, fitted(model.bwd), g=10)
# p-value of 0.05514 indicates no lack of fit at 95% confidence level?


#-------------------
# Stepwise Selection
#-------------------
step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=heart)

model.step <- glm(target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + exang, data = heart, family = binomial(link="logit"))
(vif(model.step)[,3])^2
summary(model.step)
hoslem.test(model.full$y, fitted(model.step), g=10)
# p-value of 0.05514 indicates no lack of fit at 95% confidence level?

#--------------------------------------
# LASSO Variable Selection
#--------------------------------------
x <- model.matrix(target~., train)[,-14]
y <- train$target

xtest <- model.matrix(target~., test)[,-14]
ytest <- test$target

grid <- 10^seq(10,-2, length =100)
lasso.mod <- glmnet(x, y, alpha=1, lambda = grid, family = "binomial")

cv.out <- cv.glmnet(x, y, alpha=1, family = "binomial") #alpha=1 performs LASSO
plot(cv.out)

bestlambda <- cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred <- predict(lasso.mod ,s=bestlambda ,newx=xtest, family = "binomial")

coef(lasso.mod, s=bestlambda)
