#####SUPPORT VECTOR MACHINES#####

# Prepare support vector machines model for classifying the area under fire 
# for foresfires data

#Loading Dataset
ff<- read.csv(file.choose())
View(ff)
str(ff)

library(dplyr)
ff<- select(ff,-month,-day)
View(ff)
attach(ff)

library(DataExplorer)
plot_str(ff)
plot_missing(ff)

#EDA and Visualizations
summary(ff)
#FFMC: Mean= 90.64, Median= 91.60; As Mean<Median,it is skewed to the right.
#DMC: Mean= 110.9, Median= 108.3; As Mean>Median,it is skewed to the left.
#DC: Mean= 547.9, Median= 664.2; As Mean<Median,it is skewed to the right.
#ISI:Mean= 8.400, Median= 19.30; As Mean<Median,it is skewed to the right.
#temp: Mean= 18.89, Median= 19.30; As Mean<Median,it is skewed to the right.
#RH: Mean= 44.29, Median= 42.00; As Mean>Median,it is skewed to the left.
#wind: Mean= 4.018, Median= 4.000; As Mean>Median,it is skewed to the left.
#rain: Mean= 0.02166, Median= 0.0000; As Mean>Median,it is skewed to the left.
#area: Mean= 12.85, Median= 0.52; As Mean>Median,it is skewed to the left.

plot_histogram(FFMC)
plot_histogram(DMC)
plot_histogram(DC)
plot_histogram(ISI)
plot_histogram(temp)
plot_histogram(RH)
plot_histogram(wind)
plot_histogram(area)

# Prediction of Forest fires requires only prediction from temperature, rain, 
#relative humidity and wind speed

# Applying Normalization technique
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
temp = normalize(temp)
RH   = normalize(RH)
wind = normalize(wind)
rain = normalize(rain)

# Pratitioning data
set.seed(12345)
ind <- sample(2, nrow(ff), replace = TRUE, prob = c(0.7,0.3))
ff_train <- ff[ind==1,]
ff_test  <- ff[ind==2,]

#building SVM model- vanilladot kernel
vanilla_model1<-ksvm(size_category~temp+rain+wind+RH, 
                     data= ff_train,kernel = "vanilladot")

vanilla_model1
prediction_area <- predict(vanilla_model1, ff_test)
table(prediction_area,ff_test$size_category)
agreement <- prediction_area == ff_test$size_category
table(agreement)

prop.table(table(agreement))

#SVM- rbfdot kernel
ff_model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                     data= ff_train,kernel = "rbfdot")
ff_model_rfdot
rbfdot_predict<-predict(ff_model_rfdot,newdata=ff_test)
mean(rbfdot_predict==ff_test$size_category)*100
#Model accuracy is 71.823%; Misclassification error is 28.17%

#SVM- vanilladot
ff_model_vd<- ksvm(size_category~temp+rain+wind+RH,
                   data= ff_train,kernel= "vanilladot")
ff_model_vd
vd_predict<- predict(ff_model_vd, newdata=ff_test)
mean(vd_predict==ff_test$size_category)*100
#Model accuracy is 70.71%; Misclassification error is 29.29%

#SVM- besseldot
ff_model_bd<- ksvm(size_category~temp+rain+wind+RH,
                   data= ff_train, kernel= "besseldot")
ff_model_bd
bd_predict<- predict(ff_model_bd, newdata=ff_test)
mean(bd_predict==ff_test$size_category)*100
#Model accuracy is 70.71%; Misclassification error is 29.29%

#SVM- polydot
ff_model_pd<- ksvm(size_category~temp+rain+wind+RH,
                   data= ff_train, kernel= "polydot")
ff_model_pd
pd_predict<- predict(ff_model_bd, newdata=ff_test)
mean(bd_predict==ff_test$size_category)*100
