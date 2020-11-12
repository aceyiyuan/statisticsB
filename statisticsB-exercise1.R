install.packages("tidyverse")
library(tidyverse)
mercury<-read_tsv("mercury.txt")
summary(mercury)

# > summary(mercury)
# length       mercuryconc    
# Min.   :315.0   Min.   :0.2680  
# 1st Qu.:382.5   1st Qu.:0.4838  
# Median :401.0   Median :0.5785  
# Mean   :404.8   Mean   :0.5690  
# 3rd Qu.:430.2   3rd Qu.:0.6927  
# Max.   :490.0   Max.   :0.9230  
# > 
view(mercury)#view entire dataset
mercury %>%
  ggplot(aes(x=length,y=mercuryconc))+
  geom_point()#create scatter plot
#a nicer picture
mercury %>%
  ggplot(aes(x=length, y=mercuryconc))+
  geom_point()+
  xlab("Fish length")+
  ylab(expression(paste("Mercury concentration")))+
  theme_bw()

#Model

mercury.lm<-lm(mercuryconc~length, data=mercury)
summary(mercury.lm)

#   Output:
#   Call:
#   lm(formula = mercuryconc ~ length, data = mercury)
# 
# Residuals:
#   Min        1Q      Median        3Q       Max 
# -0.176008 -0.058283 -0.001059  0.063799  0.151906 
# 
# Coefficients:
#                 Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)   -0.7374358  0.1900069  -3.881  0.00109 ** 
#   length       0.0032274  0.0004666   6.917  1.82e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.09269 on 18 degrees of freedom
# Multiple R-squared:  0.7266,	Adjusted R-squared:  0.7114 
# F-statistic: 47.84 on 1 and 18 DF,  p-value: 1.822e-06
# 

 #Coefficients

#residual plot #cehck assumptions
plot(mercury.lm)
#predictions
predict(mercury.lm)
new<-data.frame(length=400)
predict(mercury.lm,newdata=new)
#       1 
#0.5535087 
#
#Confidence and prediction intervals
predict(mercury.lm, newdata=new, interval="confidence")
predict(mercury.lm, newdata=new, interval="prediction")

# fit            lwr       upr
# 1 0.5535087 0.5097119 0.5973054
# > predict(mercury.lm, newdata=new, interval="prediction")
#     fit      lwr       upr
# 1 0.5535087 0.353913 0.7531043

#confidence interval can be changed using level

predict(mercury.lm, newdata=new, interval = "prediction", level=0.99)

#        fit       lwr       upr
#1    0.5535087 0.2800458 0.8269716

#confidence and prediction bands can be produced in the following way: (see table new)

new<-data.frame(length=315:400)
new$conf<-predict(mercury.lm, newdata=new, interval="confidence")
new$pred<-predict(mercury.lm, newdata=new, interval="prediction")

plot(mercury$length, mercury$mercuryconc)
matlines(new$length, new$conf,lty=c(1,2,2), col="black")
matlines(new$length, new$pred,lty=c(1,3,3), col="black")

#the same plot using the function ggplot and using x- and y- axis

mercury %>%
  ggplot()+
  geom_point(aes(x=length, y=mercuryconc))+
  geom_line(data=new, aes(x=length, y=conf[,1]), linetype=1)+
  geom_line(data=new, aes(x=length, y=conf[,2]), linetype=2)+
  geom_line(data=new, aes(x=length, y=conf[,3]), linetype=2)+
  geom_line(data=new, aes(x=length, y=pred[,2]), linetype=3)+
  geom_line(data=new, aes(x=length, y=pred[,3]), linetype=3)+
  xlab("Fish length")+
  ylab(expression(paste("Mercury concentration")))+
  theme_bw()
  
#multilinear regression

library(tidyverse)
soilPh<-read_tsv("soilpH.txt")
soilph.lm<-lm(pH~lat+age+tempsum+NO3,data=soilPh)
summary(soilph.lm)


#Residuals:
 # Min       1Q      Median       3Q      Max 
#-0.81629 -0.27096 -0.07196  0.14913  1.61360 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5.199e+01  8.046e+00  -6.462 1.65e-09 ***
#   lat          8.818e-01  1.246e-01   7.074 6.89e-11 ***
#   age         -2.789e-03  7.877e-04  -3.541 0.000543 ***
#   tempsum     -1.189e-01  7.949e-01  -0.150 0.881352    
# NO3          1.014e-02  1.958e-03   5.180 7.70e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4579 on 138 degrees of freedom
# Multiple R-squared:  0.3734,	Adjusted R-squared:  0.3552 
# F-statistic: 20.56 on 4 and 138 DF,  p-value: 2.629e-13

plot(soilph.lm)
soilPh%>%
  ggplot(aes(x=NO3,y=pH))+
  geom_point()
library(car)
crPlots(soilph.lm)

#multicollinearity
library(tidyverse)
hillsrun<-read_tsv("hillsrun.txt")
hillsrun%>%
  select(-name)%>%
  plot()
install.packages("GGally")
library(GGally)
hillsrun %>%
  select(-name)%>%
  ggpairs()

hillsrun.lm<-lm(time~dist+climb, data=hillsrun)
summary(hillsrun.lm)

hillsrunsdist.lm<-lm(time~dist, data=hillsrun)
summary(hillsrunsdist.lm)

hillsrunsclimb.lm<-lm(time~climb, data=hillsrun)
summary(hillsrunsclimb.lm)
vif(hillsrun.lm)
#leverage observations

time_dist.lm<-lm(time~dist, data=hillsrun)
plot(time_dist.lm)
view(hillsrun)
hillsrun_new<-hillsrun%>%
  filter(name!="Two Breweries")

hillsrun_new.lm<-lm(time~dist+climb, data=hillsrun_new)
summary(hillsrun_new.lm)
plot(hillsrun_new.lm)

