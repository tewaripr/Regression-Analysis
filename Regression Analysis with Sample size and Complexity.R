library(ggplot2)
data("diamonds")
summary(diamonds)
testerr = c()
trainrss = c()
sample_size = c(10,20,30,50,70,100,200,350)
poly_order = 7
setwd('C:\\Users\\erpri\\Desktop\\R practise\\R code\\Graphs')
for(i in sample_size){
  
#DATA:
set.seed(2*i)
rand = sample(1:nrow(diamonds),i)
train = diamonds[rand, ]
set.seed(17*i)
rand = sample(1:nrow(diamonds),i)
test = diamonds[rand, ]

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^6) + I(carat^7), train)
m7
filname=paste(paste("graph of sample size",toString(i)),".jpg")

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = filname)
plot(train$carat,train$price, pch=19, cex=0.9,main = paste("Carat/Price of order ",toString((poly_order))),sub = paste("Sample size ",toString((i))), xlab = 'Carat',ylab = 'Price')
lines(sort(train$carat), fitted(m7)[order(train$carat)], col='blue', type='l') 
dev.off()

#TRAIN AND TEST ACCURACY
trainrss = append(trainrss,sum(m7$residuals^2))
pred = predict(m7, newdata=test)
testerr = append(testerr,sum((pred-test$price)^2))

}

# Test error/Sample Size graph
jpeg("Test Error Vs Sample Size.jpg")
plot(sample_size,testerr,xlab = 'Sample Size',ylab = 'Test Error',main = paste("Test Error Vs Sample Size "))
lines(sample_size,testerr, col='green', type='l')
dev.off()
print(testerr)
print(trainrmse)

#=============================================================================================
# Problem 2
#=============================================================================================
sam_size = c(20,100)
pol_order = c(1,2,7,8,9,10)
initial_seed_train=99
initial_seed_test= 127
terror = c('er1','er2','er3','er4')


# Dividing dataframe in 4 sample 
rtrain = c('tr1', 'tr2', 'tr3', 'tr4')
rtest = c('te1', 'te2', 'te3', 'te4')
trainnames = c('train1', 'train2', 'train3', 'train4')
testnames = c('test1', 'test2', 'test3', 'test4')
colors = c('red','blue','green','black')
for(j in sam_size){
terror1 = c()
trainrss = c()
for (i in c(1:4)){
  testerr1 = c()
  set.seed(initial_seed_train*i)
  assign(rtrain[i], sample(1:nrow(diamonds),j)) 
  assign(trainnames[i], diamonds[get(rtrain[i]), ]) 
  set.seed(initial_seed_test*i)
  assign(rtrain[i], sample(1:nrow(diamonds),j))
  assign(testnames[i], diamonds[get(rtrain[i]), ])
  for(k in pol_order){
    formule = ""
    ini = "price ~ carat"
    if(k>1){
    for(x in c(2:k)){
        formule = paste(formule," + I(carat^",x, ") ",sep = "") 
    }}
  else{formule=""}
    formule = paste(ini,formule,sep = "")
    m <- lm(formula = as.formula(formule),data = get(trainnames[i]))
    filname=paste(paste("graph of sample size",toString(j))," and order",k,"sample ",i,".jpg")
    jpeg(filename = filname)
    plot(get(trainnames[i])$carat,get(trainnames[i])$price, pch=19, cex=0.9,main = paste("Carat/Price of order ",k," and sample# ",i),sub = paste("Sample size ",toString((j))), xlab = 'Carat',ylab = 'Price')
    lines(sort(get(trainnames[i])$carat), fitted(m)[order(get(trainnames[i])$carat)], col='blue', type='l') 
    dev.off()
    #TRAIN AND TEST ACCURACY
    trainrss = append(trainrss, sum(m$residuals^2))
    pred = predict(m, newdata=get(testnames[i]))
    testerr1 = append(testerr1,sum((pred-get(testnames[i])$price)^2))
  }
  terror1 = append(terror1,testerr1)
  assign(terror[i],testerr1)
  # Test and Train RMSE Vs Complexity graph
  if(i==1){
    trainrmse = sqrt(trainrss/j)
    testrmse = sqrt(testerr1/j)
    jpeg(paste("Train RMSE Vs Complexity of sample size",j,".jpg"))
    plot(pol_order,trainrmse,xlab = 'Complexity',ylab = 'Train RMSE',ylim =c(min(trainrmse),max(trainrmse)),main = "Train RMSE Vs Complexity")
    lines(pol_order,trainrmse, col='red', type='l')
    dev.off() 
    jpeg(paste("Test RMSE Vs Complexity of sample size",j,".jpg"))
    plot(pol_order,testrmse,xlab = 'Complexity',ylab = 'Test RMSE',ylim =c(min(testrmse),max(testrmse)),main = "Train RMSE Vs Complexity")
    lines(pol_order,testrmse, col='red', type='l')
    dev.off() 
  }
}
  # Test error Vs Complexity graph
  jpeg(paste("Test RSS Vs Complexity of sample size",j,".jpg"))
  plot(pol_order,get(terror[1]),xlab = 'Complexity',ylab = 'Test RSS',ylim =c(min(terror1),max(terror1)),main = "Test RSS Vs Complexity among same sample sizes")
  lines(pol_order,get(terror[1]), col='black', type='l')
  points(pol_order,get(terror[2]), col="red", pch="+")
  lines(pol_order,get(terror[2]), col='red', type='l')
  points(pol_order,get(terror[3]), col="green", pch="x")
  lines(pol_order,get(terror[3]), col='green', type='l')
  points(pol_order,get(terror[4]), col="blue", pch="i")
  lines(pol_order,get(terror[4]), col='blue', type='l')
  legend("topleft" ,legend=c(paste("sample",j,"set1"),paste("sample",j,"set2"),paste("sample",j,"set3"),paste("sample",j,"set4")), col=c("black","red","green","blue"),pch=c("o","+","x","i"), ncol=1)
  dev.off()
}






