library(ggplot2)

#Data

Data = read.csv('C:\\Users\\PRAVEEN\\Downloads\\Cars.csv')
Data

#Correlation between x ann y
cor(Data$Displacement,Data$Weight)

#==========================================================================
#    BUILDING POLYNOMIAL LINEAR REGRESSION of DEGREES (1,2,3,4,5,7)
#==========================================================================

#First order polynomial regression model

set.seed(2)
rand = sample(1:nrow(Data),300)
train = Data[rand, ]
test = Data[-rand, ]

#Building model over train data
m1 <- lm(Displacement ~ Weight, train)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 1' ))
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='red', type='l') 

#Test and Train Accuracy
pred1 = predict(m1, newdata=test)
pred_train1 = predict(m1 , train)
error_train = sqrt(mean((pred_train1-train$Displacement)^2))   #Rmse of train
e1 = sqrt(mean((pred1-test$Displacement)^2))                  #Rmse of test

#Second Order Polynomial Regression model

#Building model over train data
m2 <- lm(Displacement ~ Weight + I(Weight^2), train)
m2

#plotting the model over data

plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 2' ))
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='red', type='l') 


#Test and Train Accuracy
pred2 = predict(m2, newdata=test)
pred_train2 = predict(m2 , train)
error_train2 = sqrt(mean((pred_train2-train$Displacement)^2)) #Train RMSE
e2 = sqrt(mean((pred2-test$Displacement)^2)) #Test RMSE

#Third Order Polynomial Regression model
m3 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3), train)
m3

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 3' ))
lines(sort(train$Weight), fitted(m3)[order(train$Weight)], col='darkgreen', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
pred3 = predict(m3, newdata=test)
e3 = sqrt(mean((pred3-test$Displacement)^2)) #Test RMSE

pred_train3 = predict(m3 , train)
error_train3 = sqrt(mean((pred_train3-train$Displacement)^2)) #Train RMSE

#Fourth degree Polynomial regression
m4 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) , train)
m4

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 4' ))
lines(sort(train$Weight), fitted(m4)[order(train$Weight)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
pred4 = predict(m4, newdata=test)
e4 = sqrt(mean((pred4-test$Displacement)^2))              #Test RMSE
pred_train4 = predict(m4 , train)
error_train4 = sqrt(mean((pred_train4-train$Displacement)^2))  #Train RMSE

#Fifth degree polynomial model

#Building model over train data
m5 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5), train)
m5

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 5' ))
lines(sort(train$Weight), fitted(m5)[order(train$Weight)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
mean(m5$residuals^2)      
pred5 = predict(m5, newdata=test)
e5 = sqrt(mean((pred5-test$Displacement)^2))              #Test RMSE

pred_train5 = predict(m5 , train)
error_train5 = sqrt(mean((pred_train5-train$Displacement)^2))  #Train RMSE

#Seventh Degree Polynomial model

#Building model over train data
m7 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('Polynomial Degree - 7' ))
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
mean(m7$residuals^2)
pred7 = predict(m7, newdata=test)
e7 = sqrt(mean((pred7-test$Displacement)^2))  #Test RMSE

pred_train6 = predict(m7 , train)  
error_train6 = sqrt(mean((pred_train6-train$Displacement)^2)) #Train RMSE

#Plotting all regression lines in a single graph

plot(train$Weight,train$Displacement, pch=19, cex=0.5,xlab='Weight',ylab='Displacement',title('All Polynomial Regeression lines' ))
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='red', type='l')+
  lines(sort(train$Weight), fitted(m3)[order(train$Weight)], col='darkgreen', type='l', pch=19) 
lines(sort(train$Weight), fitted(m5)[order(train$Weight)], col='green', type='l',pch=20)+
  lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20)


#==============================================================================================
# Test RMSE and Train RMSE vs Training sets of different sample sizes for each degree polynomial
#==============================================================================================
#Objective: To check the change in Test and Train errors with increase in sample sizes
Obs = seq(10, 290, length.out = 50)  #returns 50 values between 10 and 290
Obs  #Sizes of each sample data

#For first degree polynomial regression vs Different sizes of sample sets 

train_error=c()
test_error=c()
for( i in Obs){
  set.seed(0)
  rand = sample(1:nrow(Data),i)
  train = Data[rand, ]
  test = Data[-rand, ]
  
  m1 <- lm(Displacement ~ Weight, train)
  pred_t = predict(m1,train)
  pred = predict(m1, newdata=test)
  rmse= sqrt(mean((test$Displacement-pred)^2))
  rmse_t =  sqrt(mean((train$Displacement-pred_t)^2))
  test_error = c(test_error,rmse)
  train_error = c(train_error,rmse_t)
  
}

#Plotting first degree polynomial regression vs Different sizes of sample sets 

ggplot() + 
  xlab('Number of training examples')+ylab ('RMSE of one degree model' )+
  geom_smooth(aes(x = Obs, y = test_error, color = "testing"), geom = "smooth" ) + 
  geom_smooth(aes(x = Obs, y = train_error, color = "training"), geom = "smooth")+
  ggtitle('RMSE of one degree polynomial vs Different Sample sets')

#For third degree polynomial regression vs Different sizes of sample sets 
train_error1 = c()
test_error1=c()
for( i in Obs){
  set.seed(0)
  rand = sample(1:nrow(Data),i)
  train = Data[rand, ]
  test = Data[-rand, ]
  
  m3 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3), train)
  pred_t2 = predict(m3,train)
  
  pred1 = predict(m3, newdata=test)
  
  rmse1= sqrt(mean((test$Displacement-pred1)^2))
  rmse_t2 = sqrt(mean((train$Displacement-pred_t2)^2))
  train_error1 = c(train_error1,rmse_t2)
  test_error1 = c(test_error1,rmse1)
  
  
}

# plotting third degree polynomial regression vs Different sizes of sample sets 

ggplot() + 
  xlab('Number of training examples')+ylab ('RMSE of Third degree model' )+
  geom_smooth(aes(x = Obs, y = test_error1, color = "testing"), geom = "smooth" ) + 
  geom_smooth(aes(x = Obs, y = train_error1, color = "training"), geom = "smooth")+
  ggtitle('RMSE of third degree polynomial vs Different Sample sets')


#For fifth degree polynomial regression vs Different sizes of sample sets 

train_error3 = c()
test_error2=c()
for( i in Obs){
  set.seed(0)
  rand = sample(1:nrow(Data),i)
  train = Data[rand, ]
  test = Data[-rand, ]
  
  m5 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5), train)
  pred_t3 = predict(m5,train)
  pred2 = predict(m5, newdata=test)
  
  rmse2= sqrt(mean((test$Displacement-pred2)^2))
  rmse_t3 = sqrt(mean((train$Displacement-pred_t3)^2))
  
  test_error2 = c(test_error2,rmse2)
  train_error3 = c(train_error3,rmse_t3)
  
}

#plotting fifth degree polynomial regression vs Different sizes of sample sets 

ggplot() + 
  xlab('Number of training examples')+ylab ('RMSE of fifth degree model' )+
  geom_smooth(aes(x = Obs, y = test_error2, color = "testing"), geom = "smooth" ) + 
  geom_smooth(aes(x = Obs, y = train_error3, color = "training"), geom = "smooth")+
  ggtitle('RMSE of fifth degree polynomial vs Different Sample sets')


#For Seventh degree polynomial regression vs Different sizes of sample sets 

train_error4 = c()
test_error3=c()
for( i in Obs){
  set.seed(0)
  rand = sample(1:nrow(Data),i)
  train = Data[rand, ]
  test = Data[-rand, ]
  
  m7 <- lm(Displacement ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
           + I(Weight^6) + I(Weight^7), train)  
  pred3 = predict(m7, newdata=test)
  pred_t4 = predict(m7,train)
  rmse3 = sqrt(mean((test$Displacement-pred3)^2))
  rmse_t4 = sqrt(mean((train$Displacement-pred_t4)^2))
  test_error3 = c(test_error3,rmse3)
  train_error4 = c(train_error4,rmse_t4)  
  
}

#Plotting seventh degree polynomial regression vs Different sizes of sample sets 

ggplot() + 
  xlab('Number of training examples')+ylab ('RMSE of seventh degree model' )+ggtitle('Test Error of first degree polynomial vs Different samples') +
  geom_smooth(aes(x = Obs, y = test_error3, color = "testing"), geom = "smooth" ) + 
  geom_smooth(aes(x = Obs, y = train_error4, color = "training"), geom = "smooth")+
  ggtitle('RMSE of seventh degree polynomial vs Different Sample sets')

#=========================================================================================================
# Complexity vs RMSE 
#=========================================================================================================
#Objective: To check the change in Test and train error with increasing in Degress of Polynomial model

set.seed(2)
rand = sample(1:nrow(Data),300)
train = Data[rand, ]
test = Data[-rand, ]

#Vector of testing RMSE for degrees of polynomial regression model of 1,2,3,4,5,7

e = c(e1,e2,e3,e4,e5,e7)
degree = c(1,2,3,4,5,7)

#Vector of training RMSE for degrees of polynomial regression model of 1,2,3,4,5,7

e_train = c(error_train,error_train2,error_train3,error_train4,error_train5,error_train6) 

#For Plotting Degrees of polynomial regression model vs RMSE of training and testing

ggplot() + 
  xlab('Degrees Of model')+ylab ('RMSE of Models' )+ggtitle('Complexity vs RMSE of models') +
  geom_smooth(aes(x = degree, y = e, color = "testing"), geom = "smooth" ) + 
  geom_smooth(aes(x = degree, y = e_train, color = "training"), geom = "smooth")
  


