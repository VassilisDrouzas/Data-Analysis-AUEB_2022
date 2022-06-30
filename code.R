 data<-read.csv("Beef_pork_usa.csv",sep=",")        #Import data
 str(data)
 

 
 data$PBE<-2.2046*data$PBE                       #Lines 4-7 are for the conversions
 data$CBE<-0.45359237*data$CBE
 data$PPO<-2.2046*data$PPO
 data$CPO<-0.45359237*data$CPO
 summary(data)                                  #Describing metrics for our data
 data2<-data[,c(2,3,4,5,6,7,8,9)]               #Extract the YEAR and RFP columns to make a better plot
 boxplot(data2)
 boxplot(data$RFP)
 par(mfrow=c(3,3))                              # Bind a 3x3 space for our histograms
 hist(data$PBE,xlab='Beef price',main='',freq=FALSE)
 lines(density(data$PBE), col = "red",lwd=2)
 hist(data$CBE,xlab='Beef consumption/person',main='',freq=FALSE)
 lines(density(data$CBE), col = "red",lwd=2)
 hist(data$PPO,xlab='Pork price',main='',freq=FALSE)
 lines(density(data$PPO), col = "red",lwd=2)
 hist(data$CPO,xlab='Pork consumption/person',main='',freq=FALSE)
 lines(density(data$CPO), col = "red",lwd=2)
 hist(data$PFO,xlab='Index of retail prices',main='',freq=FALSE)
 lines(density(data$PFO), col = "red",lwd=2)
 hist(data$DINC,xlab='Index of available income/person',main='',freq=FALSE)
 lines(density(data$DINC), col = "red",lwd=2)
 hist(data$CFO,xlab='Index of consumption/person',main='',freq=FALSE)
 lines(density(data$CFO), col = "red",lwd=2)
 hist(data$RDINC,xlab='Index of real available income/person',main='',freq=FALSE)
 lines(density(data$RDINC), col = "red",lwd=2)
 hist(data$RFP,xlab='Index of food price regarding CPI',main='',freq=FALSE)
 lines(density(data$RFP), col = "red",lwd=2)
 
 
 shapiro.test(data$PPO)      #Do the shapiro-wilk test on all variables



 shapiro.test(data$PBE)



 shapiro.test(data$CBE)

	

 shapiro.test(data$DINC)



 shapiro.test(data$CFO)

	

 shapiro.test(data$RDINC)

	

 shapiro.test(data$RFP)

	

 shapiro.test(data$PFO)



 shapiro.test(data$CPO)
 library(ggplot2)
 qplot(sample=data$PPO,stat="qq")
 qplot(sample=data$PFO,stat="qq")
 qplot(sample=data$CPO,stat="qq")
 qplot(sample=data$CBE,stat="qq")

 library(sjPlot)
 
 sjp.corr(data,decimals=2,corr.method="kendall",title="Kendall's correlation diagram")     #Construct the correlation diagrams
 sjp.corr(data,decimals=2,corr.method="spearman",title="Spearman's correlation diagram")
 
 chisq.test(data$PBE,data$CBE,correct=FALSE)      #Run chi-squared tests to make sure of the relationships
 
 chisq.test(data$PBE,data$CPO,correct=FALSE)
 
 chisq.test(data$PBE,data$PPO,correct=FALSE)
 
 library(ggplot2)

 ggplot(data,aes(x=PBE,y=CBE))+geom_point()+geom_smooth(method=lm,se=FALSE)+labs(title="Plot of beef price vs consumption",x="Price",y="Consumption")

 ggplot(data,aes(x=PBE,y=YEAR))+geom_point()+geom_smooth(method=lm,se=FALSE)+labs(title="Plot of beef price vs time",x="Price",y="Year")

 ggplot(data,aes(x=PPO,y=YEAR))+geom_point()+geom_smooth(method=lm,se=FALSE)+labs(title="Plot of pork price vs time", x="Price",y="Year")
 
 ggplot(data,aes(x=PPO,y=CPO))+geom_point()+geom_smooth(method=lm,se=FALSE)+labs(title="Plot of pork price vs consumption", x="Price",y="Consumption")
 
 ggplot(data,aes(x=PPO,y=PBE))+geom_point()+geom_smooth(method=lm,se=FALSE)+labs(title="Plot of pork price vs beef price", x="Pork price",y="Beef price")
 
 
 
 library(gtools)
 library(car)

 
 full<-lm(data$PBE~.,data=data) #Start from a full model
 summary(full)
 vif(full)
 
 
 model2<-step(full,direction='both')    #Stepwise selection
 summary(model2)
 vif(model2)
 
 model3<-step(model2,direction='both')
 summary(model3)
 vif(model3)
 
 recommended_model<-lm(data$PBE~data$CBE+data$PPO+data$CPO)
 par(mfrow=c(2,2))
 plot(recommended_model)  #Plot diagrams
 summary(recommended_model)
 vif(recommended_model)
 
                    
 
 shapiro.test(rstandard(recommended_model))    #Shapiro test

 library(nortest)
 lillie.test(rstandard(recommended_model))     #Lillie test
 

 
 library(car)
 dwt(recommended_model)                       #Durbin-Watson test
 
 
 library(gtools)
 
 cut<-function(x,digits=6){cut(x,breaks=quantile(x),include.lowest=TRUE,dig.lab=digits)}
 qfits<-quantcut(fitted(recommended_model))
 leveneTest(rstandard(recommended_model),qfits)        #Levene test for homoscedasticity
 
 full_pork<-lm(data$PPO~.,data=data) #Start from a full model
 summary(full_pork)
 model2_pork<-step(full_pork,direction='both')  #Stepwise selection 
 
 random_model_beef<-lm(data$PPO~data$PFO+data$DINC+data$YEAR)
 summary(random_model_beef)
 
 
 recommended_model2<-lm(data$PPO~data$CPO+data$CBE+data$PBE+data$YEAR)
 summary(recommended_model2)
 vif(recommended_model2)
 
 recommended_model2_2<-update(recommended_model2,~.-data$PBE)
 summary(recommended_model2_2)
 vif(recommended_model2_2)
 
 recommended_model2_3<-update(recommended_model2_2,~.-data$CBE)
 summary(recommended_model2_3)
 vif(recommended_model2_3)
 
 random_model_pork<-lm(data$PPO~data$PFO+data$YEAR)
 summary(random_model_pork)
 
 shapiro.test(rstandard(recommended_model2_3))    #Shapiro test
 

 lillie.test(rstandard(recommended_model2_3))     #Lillie test
 
 dwt(recommended_model2_3)
 
 par(mfrow=c(2,2))
 plot(recommended_model2_3)
 
 cut<-function(x,digits=6){cut(x,breaks=quantile(x),include.lowest=TRUE,dig.lab=digits)}
 qfits<-quantcut(fitted(recommended_model2_3))
 leveneTest(rstandard(recommended_model2_3),qfits)      
 
 AIC(full,model2,recommended_model,random_model_beef)           #Get AIC score for the models
 AIC(full_pork,model2_pork,recommended_model2_3,random_model_pork)
 
 BIC(full,model2,recommended_model,random_model_beef)           #Get BIC score for the models
 BIC(full_pork,model2_pork,recommended_model2_3,random_model_pork)
 
 
   
 


 PPO_first<-data$PPO[1:6]         #Divide the variables in the three needed categories
 PPO_second<-data$PPO[7:11]
 PPO_third<-data$PPO[12:17]
 PPO_first_median<-median(PPO_first)        #Find the median of each category
 PPO_second_median<-median(PPO_second)
 PPO_third_median<-median(PPO_third)
 
 
 t.test(PPO_first,PPO_second)     #Do a t-test to see if the means differ statistically




 t.test(PPO_first,PPO_third)



 t.test(PPO_second,PPO_third)



 PBE_first<-data$PBE[1:6]
 PBE_second<-data$PBE[7:11]
 PBE_third<-data$PBE[12:17]
 PBE_first_median<-median(PBE_first)
 PBE_second_median<-median(PBE_second)
 PBE_third_median<-median(PBE_third)
 t.test(PBE_first,PBE_second)



 t.test(PBE_first,PBE_third)



 t.test(PBE_second,PBE_third)
 
 
 wilcox.test(PBE_first,PBE_second)

 wilcox.test(PBE_first,PBE_third)
  
 wilcox.test(PBE_second,PBE_third)

 wilcox.test(PPO_first,PPO_second)
 
 wilcox.test(PPO_first,PPO_third)
 
 wilcox.test(PPO_second,PPO_third)

  