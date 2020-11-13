 
install.packages("realxl")
install.packages("tidyverse")

#load libraries
library(readxl)
library(tidyverse)

soilpH<-read_tsv("soilpH.txt")
soilpH.lm2<-lm(pH~lat+age+s+tempsum*NO3, data=soilpH)
plot(soilpH.lm2)
soilpH.interact<-lm(pH~lat+age+s+tempsum+NO3+tempsum:NO3, data=soilpH)
plot(soilpH.interact)
#interaction can be illustrated by contour plot using the package in visreg:
install.packages("visreg")
library(visreg)
visreg2d(soilpH.interact,"NO3","tempsum")

#One continous and one categorical explainatory variable
tooth<-read_tsv("toothgrowth.txt")
tooth.lm1<-lm(len~supp+dose, data=tooth)
summary(tooth.lm1)


#Call:
#  lm(formula = len ~ supp + dose, data = tooth)

#Residuals:
  #Min     1Q Median     3Q    Max 
#-6.600 -3.700  0.373  2.116  8.800 

# Coefficients:
#                Estimate    Std. Error t value   Pr(>|t|)    
#   (Intercept)   9.2725     1.2824      7.231   1.31e-09 ***
#   suppVC       -3.7000     1.0936      -3.383   0.0013 ** 
#   dose          9.7636     0.8768      11.135   6.31e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.236 on 57 degrees of freedom
# Multiple R-squared:  0.7038,	Adjusted R-squared:  0.6934 
# F-statistic: 67.72 on 2 and 57 DF,  p-value: 8.716e-16
#
# 
# stat_smooth() calculates four variables:
#   y - predicted value.
#   ymin - lower pointwise confidence interval around the mean.
#   ymax - upper pointwise confidence interval around the mean.
#   se - standard error.
tooth%>%
  ggplot(aes(x=dose,y=len, col=supp))+
  geom_point()+
  stat_smooth(method="lm")


tooth.lm3<-lm(len~supp+as.factor(dose),data=tooth)
summary(tooth.lm3)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.085 -2.751 -0.800  2.446  9.650 
# 
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         12.4550     0.9883  12.603  < 2e-16 ***
#   suppVC            -3.7000     0.9883  -3.744  0.000429 ***
#   as.factor(dose)1   9.1300     1.2104   7.543  4.38e-10 ***
#   as.factor(dose)2  15.4950     1.2104  12.802   < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.828 on 56 degrees of freedom
# Multiple R-squared:  0.7623,	Adjusted R-squared:  0.7496 
# F-statistic: 59.88 on 3 and 56 DF,  p-value: < 2.2e-16
# 
#alternative of lm3

toothgrowth$dose_fac<-as.factor(toothgrowth$dose)
tooth.lm3a<-lm(len~supp+dose_fac, data=toothgrowth)
summary(tooth.lm3a) #same output as lm3

#A model without interactions


tooth%>%
  ggplot(aes(x=dose, y=len))+
  geom_point()+
  facet_wrap(~supp)

#Including interactions

tooth.lm2<-lm(len~supp*dose, data=toothgrowth)
summary(tooth.lm2)
new<-data.frame(supp="OJ",dose=0.5)
predict(tooth.lm2,newdata=new,)

#1 
#15.45571 

#Overall F test

# library(car)
# Anova(tooth.lm3)
# 
# Anova Table (Type II tests)
# 
# Response: len
#             Sum Sq   Df F value    Pr(>F)    
#   supp       205.35  1  14.017  0.0004293 ***
#   dose_fac  2426.43  2  82.811  < 2.2e-16 ***
#   Residuals  820.43 56                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 
#A model with interactions

tooth.lm4<-lm(len~supp*as.factor(dose), data=toothgrowth)
summary(tooth.lm4)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -8.20  -2.72  -0.27   2.65   8.27 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)               13.230      1.148  11.521 3.60e-16 ***
#   suppVC                    -5.250      1.624  -3.233  0.00209 ** 
#   as.factor(dose)1           9.470      1.624   5.831 3.18e-07 ***
#   as.factor(dose)2          12.830      1.624   7.900 1.43e-10 ***
#   suppVC:as.factor(dose)1   -0.680      2.297  -0.296  0.76831    
#   suppVC:as.factor(dose)2    5.330      2.297   2.321  0.02411 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#   Residual standard error: 3.631 on 54 degrees of freedom
#    Multiple R-squared:  0.7937,	Adjusted R-squared:  0.7746 
#    F-statistic: 41.56 on 5 and 54 DF,  p-value: < 2.2e-16

#4. Model comparisions and ANOVA-tables

#Partial F-test

anova(tooth.lm1,tooth.lm2)

# Analysis of Variance Table
# 
# Model 1: len ~ supp + dose
# Model 2: len ~ supp * dose
# Res.Df     RSS Df Sum of Sq      F  Pr(>F)  
# 1     57 1022.56                              
# 2     56  933.63  1     88.92 5.3335 0.02463 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#Sequential anova tables
anova(tooth.lm4)


#Analysis of Variance Table

# Response: len
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# supp                  1  205.35  205.35  15.572 0.0002312 ***
#   as.factor(dose)       2 2426.43 1213.22  92.000 < 2.2e-16 ***
#   supp:as.factor(dose)  2  108.32   54.16   4.107 0.0218603 *  
#   Residuals            54  712.11   13.19                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

#Non_linear regression


biomass<-read_tsv("biomass_comp.txt")

plot(x=biomass$time) #Simple Scatter Plot

biomass.nls<-nls(biomass~bO*exp(b1*time),start=list(bO=1,b1=0),trace=T, data=biomass)
# 6.885915 :  1 0
# 0.9392188 :   0.95212488 -0.07076968
# 0.5306143 :   0.9801791 -0.1100449
# 0.5214942 :   0.9860775 -0.1172581
# 0.5214413 :   0.9866687 -0.1178327
# 0.5214411 :   0.9867106 -0.1178695
# 0.5214411 :   0.9867133 -0.1178719
#check assumptions
plot(biomass.nls)

#A QQ plot:

qqnorm(residuals(biomass.nls))
qqline(residuals(biomass.nls))


#6. Transform to linearity

biomass2040<-biomass%>%
  filter(diameter=="20-40")
biomasss.lm<-lm(log(biomass)~time,data=biomass2040)