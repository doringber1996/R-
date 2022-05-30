#q2-a
setwd("C:/Users/user/Desktop/R/final matala")
getwd()
#install.packages ('cdcfluview')
#library (cdcfluview)
national_ili <- ilinet( "national" , years = c ( 1997 : 2022 ) )

#q2-b
ts.mydata <- ts( data = national_ili$total_patients,end = c ( 2022 , 14 ) , frequency = 52)
autoplot(ts.mydata, series = 'national_ili',xlab = "Time", ylab = "total_patients")+labs(title = " national_ili total_patients",subtitle ="from 1997-2022")


#q2-c
ts_after2020 <- window(ts.mydata , start=c( 2019,52 ))
ts_till2020 <- window(ts.mydata , end=c( 2020,1 ))
plot(ts_till2020)
plot(ts_after2020)

ma <- ma (ts_till2020 , order = 117 ) 
p2 <- forecast (ma , h = 117 )
accuracy ( ts_after2020 , p2$mean )

#q3 - d 
components_dfts <- decompose(ts_till2020)
plot(components_dfts)
#holtwinter 1
HoltWinters1<- HoltWinters(ts_till2020, seasonal = "additive") # Creating a Holt Winters model
HoltWinters1$alpha # Displays the alpha value
## alpha 
##     1 
HoltWinters1$beta # Displays the beta value
## beta 
##    0
HoltWinters1$gamma # Displays the gamma value
## gamma 
##     1

Prediction <- predict(HoltWinters1, 119, prediction.interval = FALSE) #Making predict for 300 periods ahead
plot(HoltWinters1, Prediction) # Print the forecast
HoltWinters1$SSE    
accuracy( ts_after2020 , Prediction )

#HOLT WINTER a = 0.8 ,b = 0.015, g = 1

HoltWinters1<- HoltWinters(ts_till2020, seasonal = "additive",alpha = 0.8 ,beta = 0.015 , gamma = 1) # Creating a Holt Winters model
Prediction <- predict(HoltWinters1, 119, prediction.interval = FALSE) #Making predict for 300 periods ahead
plot(HoltWinters1 ,Prediction) # Print the forecast
HoltWinters1$SSE
accuracy( ts_after2020 , Prediction )

#holtwinters multi

ts_after2003 <- window(ts.mydata , start=c( 2003,1 ) , end =c(2020,1))
HoltWinters2<- HoltWinters(ts_after2003, seasonal = "multiplicative") # Creating a Holt Winters model
Prediction <- predict(HoltWinters2, 119, prediction.interval = FALSE) #Making predict for 119 periods ahead
plot(HoltWinters2, Prediction) # Print the forecast
HoltWinters2$SSE
HoltWinters2$alpha#0.5528214 
HoltWinters2$beta#0
HoltWinters2$gamma#1
accuracy( ts_after2020 , Prediction )

#second prediction with multi
HoltWinters_multi_2<- HoltWinters(ts_after2003, seasonal = "multiplicative",alpha = 0.7,beta = 0 ,gamma = 0.7) # Creating a Holt Winters model
Prediction <- predict(HoltWinters_multi_2, 119, prediction.interval = FALSE) #Making predict for 500 periods ahead
plot(HoltWinters_multi_2, Prediction) # Print the forecast
HoltWinters_multi_2$SSE
accuracy( ts_after2020 , Prediction )


#predict with regression
plot(ts_till2020)
dat.lm1 <- tslm(ts_till2020 ~ trend)
summary(dat.lm1)
lines(dat.lm1$fitted,col = "blue", lwd = 1)
accuracy(dat.lm1)


#predict with Exponential Smoothing
fc <- ses(ts_till2020, h=119)
p1<-autoplot(ts_till2020, series="Data") +
  autolayer(fc, series="Exponential Smooting") +  labs(title = "Monthly show",subtitle ="Exponential Smoothing" )
p1
accuracy( ts_after2020 , fc$mean )

#predict for (2022,5) till (2022,10) With additive

HoltWinters_flu<- HoltWinters(ts_after2020, seasonal = "additive",alpha = 0.8 ,beta = 0.015 , gamma = 1) # Creating a Holt Winters model
HWflu.pred <- predict(HoltWinters_flu, 29, prediction.interval = TRUE, level=0.95)#
plot(HoltWinters_flu ,HWflu.pred) # Print the forecast
