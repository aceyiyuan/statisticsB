install.packages('mvtnorm', dep = TRUE)
library(lattice)
library(tidyverse)
library(car)
library(gplots)
library("multcomp")
library(lme4)
library(agricolae)
library(multcompView)
library(emmeans)

potatoes<-read.csv("potatoes.txt", sep = "\t")
head(potatoes)
potatoes$Fert<-factor(potatoes$Fert)
potatoes$Variety<-factor(potatoes$Variety)
str(potatoes) #check datatype


# View(potatoes)
# > potatoes<-read.csv("potatoes.txt", sep = "\t")
# > head(potatoes)
# Fert Variety Replicate Yield
# 1    1       1         1   220
# 2    1       1         2   190
# 3    1       1         3   180
# 4    1       1         4   160
# 5    1       2         1   250
# 6    1       2         2   230
# > potatoes$Fert<-factor(potatoes$Fert)
# > potatoes$Variety<-factor(potatoes$Variety)
# > str(potatoes) #check datatype
# 'data.frame':	24 obs. of  4 variables:
#   $ Fert     : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 2 2 ...
# $ Variety  : Factor w/ 2 levels "1","2": 1 1 1 1 2 2 2 2 1 1 ...
# $ Replicate: int  1 2 3 4 1 2 3 4 1 2 ...
# $ Yield    : int  220 190 180 160 250 230 200 190 250 200 ...

#plot the means
#Fret 1,2, 3
#Variety 1,2,
# Replicate 1,2,3,4 
attach(potatoes)
fit<-aov(Yield~Fert)
aggregate(Yield, by=list(Fert),mean)
plotmeans(Yield~Fert,
          xlab="Fert",
          ylab="Yield",
          main="Mean plot \n with 95% CI")

#Fit models
model.1<-lm(Yield~Fert+Variety+Fert:Variety, data=potatoes)
summary(model.1)
anova(model.1)

# 
# Call:
#   lm(formula = Yield ~ Fert + Variety + Fert:Variety, data = potatoes)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -27.50 -15.62  -3.75  13.12  37.50 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      187.50      11.23  16.701 2.11e-12 ***
# Fert2             35.00      15.88   2.204  0.04075 *  
# Fert3             67.50      15.88   4.251  0.00048 ***
# Variety2          30.00      15.88   1.890  0.07504 .  
# Fert2:Variety2     5.00      22.45   0.223  0.82629    
# Fert3:Variety2    67.50      22.45   3.006  0.00758 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.45 on 18 degrees of freedom
# Multiple R-squared:  0.8778,	Adjusted R-squared:  0.8439 
# F-statistic: 25.86 on 5 and 18 DF,  p-value: 1.259e-07
# 
# > anova(model.1)
# Analysis of Variance Table
# 
# Response: Yield
#                Df   Sum Sq Mean Sq F value    Pr(>F)    
# Fert            2  41925 20962.5 41.5785 1.788e-07 ***
#   Variety       1  17604 17604.2 34.9174 1.358e-05 ***
#   Fert:Variety  2   5658  2829.2  5.6116   0.01276 *  
#   Residuals    18   9075   504.2                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

model.1b<-lm(Yield~Fert*Variety, data=potatoes)
Anova(model.1b)



# Anova Table (Type II tests)
# 
# Response: Yield
#                 Sum Sq Df F value    Pr(>F)    
#   Fert          41925  2 41.5785 1.788e-07 ***
#   Variety       17604  1 34.9174 1.358e-05 ***
#   Fert:Variety   5658  2  5.6116   0.01276 *  
#   Residuals      9075 18                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 
#interaction lot

interaction.plot(potatoes$Fert, potatoes$Variety,potatoes$Yield)

#check the residuals of the model fit
#the plot indicated approx. homogeneity of variance
plot(fitted(model.1),residuals(model.1))
abline(h=0)

#study the normal probability plot (qqlot) #by default, in indicated 25th and 75th percentiles

qqnorm(residuals(model.1))
qqline(residuals(model.1))
# 15th and 85th percentiles
qqnorm(residuals(model.1))
qqline(residuals(model.1),probs=c(0.15,0.85))
#or use


par(mfrow=c(2,2))
plot(model.1)

#Shapiro_Walk test on the Anova residuals. A p<0.05 indciates violation of the assumption
res<-residuals(model.1)
shapiro.test(res)

# Shapiro-Wilk normality test
# 
# data:  res
# W = 0.94057, p-value = 0.1679
#For the homogeneity of variances, Levene's test in car package. A p<.05 indciate violation of the assumption
leveneTest(Yield~Fert*Variety)

# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  5  0.5215 0.7569
#       18 

#Test main effects and interactions

#Estimated marginal means
emmeans(model.1,~Fert:Variety)

# emmeans(model.1,~Fert:Variety)
# Fert Variety emmean   SE df lower.CL upper.CL
# 1    1          188 11.2 18      164      211
# 2    1          222 11.2 18      199      246
# 3    1          255 11.2 18      231      279
# 1    2          218 11.2 18      194      241
# 2    2          258 11.2 18      234      281
# 3    2          352 11.2 18      329      376
# 
# Confidence level used: 0.95 

#interaction p,ot using estimated marginal means:
means<-emmeans(model.1,~Fert:Variety)
LSMeans<-as.data.frame(means)
interaction.plot(LSMeans$Fert, LSMeans$Variety, LSMeans$emmean)
LSMeans$Fert

# [1] 1 2 3 1 2 3
# Levels: 1 2 3

LSMeans$Variety

# [1] 1 1 1 2 2 2
# Levels: 1 2

LSMeans$emmean
#fert1variety1, fertgroup2 variety2, fertgroup3variety1, fert1variaety2,fert2viety2,fert3variety2 each group 4 observations
#[1] 187.5 222.5 255.0 217.5 257.5 352.5

#plot the original observations:

xyplot(Yield~Fert, groups=Variety, data=potatoes)


#pairwise comparisons
# 
# emmeans(model.1, pairwise~~Fert:Variety )
# emmeans(model.1, pairwise~~Fert:Variety,adust="none" )
# 
# $emmeans
# Fert Variety emmean   SE df lower.CL upper.CL
# 1    1          188 11.2 18      154      221
# 2    1          222 11.2 18      189      256
# 3    1          255 11.2 18      222      288
# 1    2          218 11.2 18      184      251
# 2    2          258 11.2 18      224      291
# 3    2          352 11.2 18      319      386

#Confidence level used: 0.95 
#Conf-level adjustment: sidak method for 6 estimates 

#For confidence intervals for the differences between the treatmenst, use:

diffs<-emmeans(model.1, pairwise~Fert:Variety)
confint(diffs,adjust="tukey")

# 
# onfidence level used: 0.95 
# Conf-level adjustment: sidak method for 6 estimates 
# 
# $contrasts
# contrast  estimate   SE df lower.CL upper.CL
# 1 1 - 2 1    -35.0 15.9 18    -85.5     15.5
# 1 1 - 3 1    -67.5 15.9 18   -118.0    -17.0
# 1 1 - 1 2    -30.0 15.9 18    -80.5     20.5
# 1 1 - 2 2    -70.0 15.9 18   -120.5    -19.5
# 1 1 - 3 2   -165.0 15.9 18   -215.5   -114.5
# 2 1 - 3 1    -32.5 15.9 18    -83.0     18.0
# 2 1 - 1 2      5.0 15.9 18    -45.5     55.5
# 2 1 - 2 2    -35.0 15.9 18    -85.5     15.5
# 2 1 - 3 2   -130.0 15.9 18   -180.5    -79.5
# 3 1 - 1 2     37.5 15.9 18    -13.0     88.0
# 3 1 - 2 2     -2.5 15.9 18    -53.0     48.0
# 3 1 - 3 2    -97.5 15.9 18   -148.0    -47.0
# 1 2 - 2 2    -40.0 15.9 18    -90.5     10.5
# 1 2 - 3 2   -135.0 15.9 18   -185.5    -84.5
# 2 2 - 3 2    -95.0 15.9 18   -145.5    -44.5

#Confidence level used: 0.95 
#Conf-level adjustment: tukey method for comparing a family of 6 estimates 
#library(multcomp)
#cld(means,letters=letters, adjust="sidak") #changed drom name is "tukey"
# 
# Fert Variety emmean   SE df lower.CL upper.CL .group
# 1    1          188 11.2 18      154      221   1    
# 1    2          218 11.2 18      184      251  12   
# 2    1          222 11.2 18      189      256  12   
# 3    1          255 11.2 18      222      288   2   
# 2    2          258 11.2 18      224      291   2   
# 3    2          352 11.2 18      319      386   3  
# 
# Confidence level used: 0.95 
# Conf-level adjustment: sidak method for 6 estimates 
# P value adjustment: tukey method for comparing a family of 6 estimates 
# significance level used: alpha = 0.05 

#not compare all combination but only the three fertilizers for each of the two varieties

emmeans(model.1, pairwise~ Fert|Variety, adjust="none")

#One way random experiment
install.packages(c("gWidgets", "gWidgetsRGtk2", "RGtk2", "RSQLite", "igraph"))
#install.packages("https://cran.r-project.org/src/contrib/Archive/RQDA/RQDA_0.3-1.tar.gz", type = "source") #not work
protein1<-read_tsv("protein1.txt")
edit(protein1)#not work
#install.packages("https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg") does not work
names(protein1)#name of variables

#[1] "Protein" "bagnr"   "obs"    
head(protein1)
protein1$bagnr<-factor(protein1$bagnr)
#Model

# attach(protein1)
# mod.lmer<-lmer(Protein~(1 | bagnr), data=protein1)
# summary(mod.lmer)
# 
# > mod.lmer<-lmer(Protein~(1 | bagnr), data=protein1)
# > summary(mod.lmer)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Protein ~ (1 | bagnr)
# Data: protein1
# 


# 
# > mod.lmer<-lmer(Protein~(1 | bagnr), data=protein1)
# > summary(mod.lmer)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Protein ~ (1 | bagnr)
# Data: protein1
# 
# REML criterion at convergence: 74.7
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -1.05579 -0.67230 -0.03903  0.57863  1.53975 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# bagnr    (Intercept) 11.152   3.339   
# Residual              2.375   1.541   
# Number of obs: 16, groups:  bagnr, 8
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)   12.125      1.242   9.763
# # REML criterion at convergence: 74.7
# # 
# # Scaled residuals: 
# #   Min       1Q   Median       3Q      Max 
# # -1.05579 -0.67230 -0.03903  0.57863  1.53975 
# # 
# # Random effects:
# #   Groups   Name        Variance Std.Dev.
# # bagnr    (Intercept) 11.152   3.339   
# # Residual              2.375   1.541   
# # Number of obs: 16, groups:  bagnr, 8
# # 
# # Fixed effects:
# #              Estimate Std. Error t value
# # (Intercept)   12.125      1.242   9.763


