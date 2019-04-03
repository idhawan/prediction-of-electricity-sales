
options(max.print=100000)
Midterm.df<-read.csv(file="D:/Purdue/2nd Sem/Predictive Modeling(IE 590)/Midterm/Midterm_FittingData.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
Midterm1.df<-Midterm.df[complete.cases(Midterm.df),]

#Plotting the histograms of all the variables
library(ggplot2)
plot1<-qplot(Midterm1.df$res.price,geom="histogram",binwidth = 0.5,  main = "Histogram for residential price", xlab = "Electricity price in Residential sector(cents/kW h)")
plot2<-qplot(Midterm1.df$res.sales.adj,geom="histogram",  main = "Histogram for residential sales", xlab = "Electricity sales in Residential sector(GW h)")
plot3<-qplot(Midterm1.df$com.price,geom="histogram",  main = "Histogram for commercial price", xlab = "Electricity price in commercial sector(cents/kW h)")
plot4<-qplot(Midterm1.df$com.sales.adj,geom="histogram",  main = "Histogram for commercial sales", xlab = "Electricity sales in commercial sector(GW h)")
plot5<-qplot(Midterm1.df$EMXP,geom="histogram",  main = "Histogram for maximum precipitation", xlab = "Extreme maximum daily precipitation in a month(mm,inches)")
plot6<-qplot(Midterm1.df$MXSD,geom="histogram",  main = "Histogram for maximum snow depth ", xlab = "Maximum snow depth observed in a month(mm,inches)")# the graph shows that the maximum values are 0 so we can remove the data from dataset
plot7<-qplot(Midterm1.df$TPCP,geom="histogram",  main = "Histogram for Total precipitation", xlab = "Total precipitation in a month(mm,inches)")
plot8<-qplot(Midterm1.df$TSNW,geom="histogram",  main = "Histogram for total snowfall in a month", xlab = "Total snowfall in a month(mm,inches)")
plot9<-qplot(Midterm1.df$EMXT,geom="histogram",  main = "Histogram for Extreme maximum Daily temperature", xlab = "Extreme maximum Daily temperature observed in a month(degrees,fahrenheit)")
plot10<-qplot(Midterm1.df$EMNT,geom="histogram",  main = "Histogram for Extreme minimum Daily temperature", xlab = "Extreme Minimum Daily temperature observed in a month(degrees,fahrenheit)")
plot11<-qplot(Midterm1.df$MMXT,geom="histogram",  main = "Histogram for monthly mean maximum temperature", xlab = "Monthly mean maximum temperature(degrees,fahrenheit)")
plot12<-qplot(Midterm1.df$MMNT,geom="histogram",  main = "Histogram for monthly mean minimum temperature", xlab = "Monthly mean minimum temperature(degrees,fahrenheit)")
plot13<-qplot(Midterm1.df$MNTM,geom="histogram",  main = "Histogram for monthly mean temperature", xlab = "Monthly mean temperature(degrees,fahrenheit)")
plot14<-qplot(Midterm1.df$DT90,geom="histogram",  main = "Histogram for number days in a month with max temp.(90)", xlab = "no. of days in a month with max temp. greater than equal to 90 fahrenheit")
plot15<-qplot(Midterm1.df$DX32,geom="histogram",  main = "Histogram for number days in a month with max temp.(32)", xlab = "no. of days in a month with max temp. less than equal to 32 fahrenheit")
plot16<-qplot(Midterm1.df$DT00,geom="histogram",  main = "Histogram for number days in a month with minimum temperature(0)", xlab = "number days in a month with minimum temperature less than or equal to 0 fahrenheit")
plot17<-qplot(Midterm1.df$DT32,geom="histogram",  main = "Histogram for number days in a month with minimum temperature(32)", xlab = "number days in a month with minimum temperature less than or equal to 32 fahrenheit")
plot18<-qplot(Midterm1.df$DP01,geom="histogram",  main = "Histogram for Days with precipitation", xlab = "days with precipitation greater than or equal to 2.54mm")
plot19<-qplot(Midterm1.df$DP05,geom="histogram",  main = "Histogram for Days with precipitation", xlab = "days with precipitation greater than or equal to 12.7mm")
plot20<-qplot(Midterm1.df$DP10,geom="histogram",  main = "Histogram for Days with precipitation", xlab = "days with precipitation greater than or equal to 25.4mm")
plot21<-qplot(Midterm1.df$MDPT,geom="histogram",  main = "Histogram for dew point temperature", xlab = "Avg. monthly dew point temperature aggregated from daily dew point temp.")
plot22<-qplot(Midterm1.df$VISIB,geom="histogram",  main = "Histogram for visibility", xlab = "Avg. daily meteorological visibility recorded over a month")
plot23<-qplot(Midterm1.df$WDSP,geom="histogram",  main = "Histogram for wind speed", xlab = "Avg. daily wind speed recorded over a month(m/s,miles/hour)")
plot24<-qplot(Midterm1.df$MWSPD,geom="histogram",  main = "Histogram for sustained wind speed", xlab = "Avg. daily maximum sustained wind speed recorded over a month(m/s,miles/hour)")
plot25<-qplot(Midterm1.df$GUST,geom="histogram",  main = "Histogram for gust", xlab = "Avg. daily wind gust recorded over a month(m/s,miles/hour)")
plot26<-qplot(Midterm1.df$HTDD,geom="histogram",  main = "Histogram for HTDD", xlab = "Heating Degree Days")
plot27<-qplot(Midterm1.df$CLDD,geom="histogram",  main = "Histogram for CLDD", xlab = "Cooling degree days")
plot28<-qplot(Midterm1.df$LABOR,geom="histogram",  main = "Histogram for labor", xlab = "Labor force")
plot29<-qplot(Midterm1.df$EMP,geom="histogram",  main = "Histogram for people in labor force employed", xlab = "No. of people in labor force employed per month")
plot30<-qplot(Midterm1.df$UNEMP,geom="histogram",  main = "Histogram for labor force(unemployed)", xlab = "No. of people in labor force unemployed per month")
plot31<-qplot(Midterm1.df$UNEMPRATE,geom="histogram",  main = "Histogram for unemployment rate", xlab = "Unemployment rate per month")
plot32<-qplot(Midterm1.df$PCINCOME,geom="histogram",  main = "Histogram for per capita income", xlab = "Per capita income(USD)")
plot33<-qplot(Midterm1.df$GSP,geom="histogram",  main = "Histogram for Real Gross state product", xlab = "Real Gross State Product(million USD)")
Midterm2.df<-Midterm1.df[-c(8,10,17,18)]
str(Midterm2.df)

#PLotting the correlation matrix to get an idea of highly correlated variables.

library(corrplot)
correlation_matrix<-cor(Midterm1.df,method=c("spearman"))
correlation_matrix

Midterm_data<-sample(length(Midterm2.df[,2]),size=0.85*(length(Midterm2.df[,2])),replace=FALSE,set.seed(42))
training_set_Midterm<-Midterm2.df[Midterm_data,]
testing_set_Midterm<-Midterm2.df[-Midterm_data,]

#When we run GLM UNEMP is coming out as NA which shows that there is extremely high correlation and hence, UNEMP has to be removed from the dataset.
model_glm<-glm(res.sales.adj~.,data=training_set_Midterm)
summary(model_glm)
model_glm2<-glm(res.sales.adj~.-UNEMP,data=training_set_Midterm)
summary(model_glm2)

#Running step regression initially we can get the best variables
null=lm(res.sales.adj~1, data=training_set_Midterm)
full=lm(res.sales.adj~.,data=training_set_Midterm)
step_regression<-step(null, scope=list(lower=null, upper=full), direction="forward",trace=1)
summary(step_regression)

#Simple Linear regression on variables selected from Stepwise Regression
model_slr<-lm(res.sales.adj~com.sales.adj+month+HTDD+DT90+CLDD+DT32+VISIB+WDSP+DP05+MWSPD+DP10+MMXT+MNTM,data=training_set_Midterm)
model_slr

#Residual plots of the variables after running the linear regression model.

slr.residuals<-resid(model_slr)
plot34<-plot(training_set_Midterm$com.sales.adj,slr.residuals) 
plot35<-plot(training_set_Midterm$month,slr.residuals)
plot36<-plot(training_set_Midterm$HTDD,slr.residuals)
plot37<-plot(training_set_Midterm$DT90,slr.residuals) 
plot38<-plot(training_set_Midterm$CLDD,slr.residuals)
plot39<-plot(training_set_Midterm$DT32,slr.residuals)
plot40<-plot(training_set_Midterm$VISIB,slr.residuals)
plot41<-plot(training_set_Midterm$WDSP,slr.residuals)
plot42<-plot(training_set_Midterm$DP05,slr.residuals)
plot43<-plot(training_set_Midterm$MWSPD,slr.residuals)
plot44<-plot(training_set_Midterm$DP10,slr.residuals)
plot45<-plot(training_set_Midterm$MMXT,slr.residuals)
plot46<-plot(training_set_Midterm$MNTM,slr.residuals)
#No heteroscadisticity observed from the residual plots.

plot47<-qqnorm(slr.residuals)
predicted_residuals<-predict(model_slr,newdata=training_set_Midterm)
plot48<-plot(predicted_residuals,slr.residuals)

summary(model_slr)
predmodel_slr<-predict(model_slr,newdata =testing_set_Midterm) 
predmodel_slr
library(ModelMetrics)
y_test<-as.matrix(testing_set_Midterm$res.sales.adj)
rmselm<-rmse(y_test,predmodel_slr)
rmselm
library(glmnet)
library(ModelMetrics)
kfolds<-10
folds <- cut(seq(1,nrow(Midterm2.df)),breaks=kfolds,labels=FALSE)
for(i in 1:kfolds){
  
  set.seed(42)
  testfolds <- which(folds==i,arr.ind=TRUE)
  testData <- Midterm2.df[testfolds, ]
  trainData <- Midterm2.df[-testfolds, ]
ordinary.model<-lm(res.sales.adj~.,data=training_set_Midterm)
pred.ordinary.model<-predict(ordinary.model,newdata=testing_set_Midterm)
y.test.ordinary <- as.matrix(testing_set_Midterm$res.sales.adj)
  rmse.kfold_linear_model<-rmse(y.test.ordinary,pred.ordinary.model)
  }
rmse.kfold_linear_model


#BOXcox transformation
library(MASS)
library(ModelMetrics)
boxcox_tran<-boxcox(res.sales.adj~.-UNEMP,data=training_set_Midterm)
lambda <- boxcox_tran$x[which.max(boxcox_tran$y)]
lambda
model_transformed<-lm(res.sales.adj**(lambda)~.-UNEMP,data=training_set_Midterm)
summary(model_transformed)

predmodel_transformed<-predict(model_transformed,newdata=testing_set_Midterm) 
predmodel_transformed
y.test <- as.matrix(testing_set_Midterm$res.sales.adj)
rmse_transformed<-rmse(y.test,predmodel_transformed)
rmse_transformed

#Ridge Regression

set.seed(42)
library(glmnet)
y.train <- as.matrix(training_set_Midterm$res.sales.adj)
x.train <- as.matrix(cbind(training_set_Midterm[,-4]))
model_ridge<-glmnet(x=x.train,y=y.train,family = "gaussian",alpha=0)
cv_model_ridge<-cv.glmnet(x=x.train,y=y.train,alpha=0)
plot(cv_model_ridge)
lambda_value <- cv_model_ridge$lambda.min
lambda_value
ridge_coefficient<-as.matrix(coef(cv_model_ridge,s="lambda.min"))
ridge_coefficient
y.test <- as.matrix(testing_set_Midterm$res.sales.adj)
x.test <- as.matrix(cbind(testing_set_Midterm[,-4]))

#Prediction for ridge regression
predict_ridge <- predict(cv_model_ridge, s = lambda_value,newx=x.test) 
predict_ridge
library(ModelMetrics)
rmse_ridge<-rmse(y.test,predict_ridge)
rmse_ridge

set.seed(42)
library(glmnet)
library(ModelMetrics)
kfolds<-10
folds <- cut(seq(1,nrow(Midterm2.df)),breaks=kfolds,labels=FALSE)
for(i in 1:kfolds){
  
  set.seed(42)
  testfolds <- which(folds==i,arr.ind=TRUE)
  testData <- Midterm2.df[testfolds, ]
  trainData <- Midterm2.df[-testfolds, ]
y.train.ridge <- as.matrix(training_set_Midterm$res.sales.adj)
x.train.ridge <- as.matrix(cbind(training_set_Midterm[,-4]))
y.test.ridge <- as.matrix(testing_set_Midterm$res.sales.adj)
x.test.ridge <- as.matrix(cbind(testing_set_Midterm[,-4]))

cv.ridge.model<-cv.glmnet(x=x.train.ridge,y=y.train.ridge,alpha=0)
cv.lambda <- cv.ridge.model$lambda.min
y.ridge <- predict(cv.ridge.model, s = cv.lambda,newx=x.test.ridge)
rmse.kfold_ridge<-rmse(y.test.ridge,y.ridge)
}
rmse.kfold_ridge


#Lasso Regression
set.seed(42)
model_lasso<-glmnet(x=x.train,y=y.train,family = "gaussian",alpha=1)
cv_model_lasso<-cv.glmnet(x=x.train,y=y.train,alpha=1)
plot(cv_model_lasso)
lambda_value2 <- cv_model_lasso$lambda.min
lambda_value2
lasso_coefficient<-as.matrix(coef(cv_model_lasso,s="lambda.min"))
lasso_coefficient

predict_lasso<- predict(cv_model_lasso, s = lambda_value2,newx=x.test) 
predict_lasso

library(ModelMetrics)
rmse_lasso<-rmse(y.test,predict_lasso)
rmse_lasso

set.seed(42)
library(glmnet)
library(ModelMetrics)
kfolds<-10
folds <- cut(seq(1,nrow(Midterm2.df)),breaks=kfolds,labels=FALSE)
for(i in 1:kfolds){
  
  set.seed(42)
  testfolds <- which(folds==i,arr.ind=TRUE)
  testData <- Midterm2.df[testfolds, ]
  trainData <- Midterm2.df[-testfolds, ]
y.train.lasso <- as.matrix(training_set_Midterm$res.sales.adj)
x.train.lasso <- as.matrix(cbind(training_set_Midterm[,-4]))
y.test.lasso<- as.matrix(testing_set_Midterm$res.sales.adj)
x.test.lasso <- as.matrix(cbind(testing_set_Midterm[,-4]))

cv.lasso.model<-cv.glmnet(x=x.train.lasso,y=y.train.lasso,alpha=1)
cv.lambda.lasso <- cv.lasso.model$lambda.min

y.lasso <- predict(cv.lasso.model, s = cv.lambda.lasso,newx=x.test.lasso)
rmse.kfold_lasso<-rmse(y.test.lasso,y.lasso)
}

rmse.kfold_lasso


#GAM
set.seed(42)
library(ISLR)
library(gam)
gam.fit<-gam(res.sales.adj~s(com.sales.adj)+month+s(MMXT)+s(DT90)+s(DT32)+s(DP05)+s(VISIB)+s(WDSP)+s(GUST)+s(HTDD)+s(CLDD)+s(PCINCOME),data=training_set_Midterm)
plot(gam.fit, se = T, col = "blue")
summary(gam.fit)

gam_output<-gam(res.sales.adj~com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+HTDD+CLDD+PCINCOME,data=training_set_Midterm)
gam.step <- step.gam(gam_output,scope = list("month" =~ 1+ month,
                                        "com.sales.adj"=~1+com.sales.adj+s(com.sales.adj,df=2)+s(com.sales.adj,df=3)+s(com.sales.adj,df=4)+s(com.sales.adj,df=5),
                                        "HTDD" =~ 1+HTDD+ s(HTDD,df=2)+ s(HTDD, df = 3)+ s(HTDD, df = 4)+ s(HTDD, df = 5),
                                         
                                        "DT90"=~1+DT90+s(DT90,df=2)+s(DT90,df=3)+s(DT90,df=4)+s(DT90,df=5),
                                        "CLDD"=~1+CLDD+s(CLDD,df=2)+s(CLDD,df=3)+s(CLDD,df=4)+s(CLDD,df=5),
                                        
                                        "DP05"=~1+DP05+s(DP05,df=2)+s(DP05,df=3)+s(DP05,df=4)+s(DP05,df=5),
                                        "DT32"=~1+DT32+s(DT32,df=2)+s(DT32,df=3)+s(DT32,df=4)+s(DT32,df=5),
                                        "WDSP" =~ 1+ WDSP+ s(WDSP, df = 2)+ s(WDSP, df = 3)+ s(WDSP, df = 4)+s(WDSP,df=5), 
                                        
                                        
                                        
                                        "MMXT" =~ 1+ MMXT+ s(MMXT, df = 2)+ s(MMXT, df = 4)+ s(MMXT, df = 5),
                     
                     "VISIB" =~ 1+ VISIB+ s(VISIB, df = 2)+ s(VISIB, df = 4)+ s(VISIB, df = 5),


"PCINCOME"=~ 1+ PCINCOME+ s(PCINCOME, df = 2)+ s(PCINCOME, df = 4)+ s(PCINCOME, df = 5)),


                     direction = "both", trace=1)

gam.step
summary(gam.step)
paste("The AIC for the best model is:", gam.step$aic)

#Best model obtained from GAM
model_GAM<-gam(res.sales.adj~GUST + month + s(com.sales.adj, df = 4) + s(HTDD,      df = 2) + CLDD + s(DP05, df = 2) + s(DT32, df = 3) + WDSP +      s(MMXT, df = 5) + VISIB + PCINCOME ,data=training_set_Midterm)
model_GAM
plot(model_GAM, se = T, col = "blue")
summary(model_GAM)
model_GAM
predmodel_GAM<-predict(model_GAM,newdata =testing_set_Midterm) 
predmodel_GAM
library(ModelMetrics)
y_test_GAM<-as.matrix(testing_set_Midterm$res.sales.adj)
rmse_GAM<-rmse(y_test_GAM,predmodel_GAM)
rmse_GAM


#random forest on the variables selected from Lasso by penalizing

library(rpart)
library(rpart.plot)
library(Hmisc)
library(caret)
library(randomForest)
set.seed(42)

finalmodel<-randomForest(res.sales.adj ~  com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+
                           HTDD+CLDD+PCINCOME, data=training_set_Midterm,ntree=1000,importance=T)
finalmodel
predicted_randomforest<-predict(finalmodel,newdata=testing_set_Midterm)
predicted_randomforest
library(ModelMetrics)
y_test_randomforest<-as.matrix(testing_set_Midterm$res.sales.adj)
rmse_randomforest<-rmse(y_test_randomforest,predicted_randomforest)
rmse_randomforest
library("party")
x <- ctree(res.sales.adj ~ com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+HTDD+CLDD+PCINCOME, data=training_set_Midterm)
plot(x, type="simple")
Importance_plot<-varImpPlot(finalmodel,type=2)
Importance_plot
cforest(res.sales.adj ~ com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+HTDD+CLDD+PCINCOME, data=training_set_Midterm, controls=cforest_control(mtry=2, mincriterion=0))
plot(finalmodel, type="l", main=deparse(substitute(finalmodel)))
getTree(finalmodel, k=250, labelVar=FALSE)

#On full dataset
#MDPT variable removed because it is not significant according to Lasso Regression
set.seed(42)
model_randomforest1<-randomForest(res.sales.adj ~ . , data=training_set_Midterm,ntree=1000,importance=T)
predicted_randomforest1<-predict(model_randomforest1,newdata=testing_set_Midterm)
predicted_randomforest1
library(ModelMetrics)
y_test_randomforest1<-as.matrix(testing_set_Midterm$res.sales.adj)
rmse_randomforest1<-rmse(y_test_randomforest1,predicted_randomforest1)
rmse_randomforest1
Importance_plot1<-varImpPlot(model_randomforest1,type=2)
Importance_plot1

#Regression trees approach(rpart method)
set.seed(42)
model_rpart<-rpart(res.sales.adj~com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+HTDD+CLDD+PCINCOME,data=training_set_Midterm)
summary(model_rpart)
rpart.plot(model_rpart)
train_rpart<- trainControl(method="cv", number=10,savePredictions=T)
model6<- train(res.sales.adj~com.sales.adj+month+MMXT+DT90+DT32+DP05+VISIB+WDSP+GUST+HTDD+CLDD+PCINCOME, data=training_set_Midterm, trControl=train_rpart, method="rpart")
model6
summary(model6)
predmodel_rpart<-predict(model6,newdata =testing_set_Midterm) 
predmodel_rpart
library(ModelMetrics)
y_test_rpart<-as.matrix(testing_set_Midterm$res.sales.adj)
rmse_rpart<-rmse(y_test_rpart,predmodel_rpart)
rmse_rpart

save(finalmodel,file='0029958159.RData')
