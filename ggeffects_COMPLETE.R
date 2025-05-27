####
#
#Using ggeffects for effective model graphics
#ACCESS 2025 Conference Workshop
#Trevor Avery
####

#----Load Libraries----
library(tidyverse)
library(ggeffects)
library(broom)
library(emmeans)

#----Import Data----
mydata<-read.csv("sharkmercury.csv")
names(mydata)

#----Wrangling----
#ggeffects likes characters as factors - we make it so like so
mydata<-mydata%>%
  mutate(common_name=as.factor(common_name),
         sex=as.factor(sex))

#----Model1: Linear Regression----
#Demo of a (poor) model for this data
m0<-glm(hg_ww_ppm~tl_cm*sex*common_name,
        data=mydata)

#Model summary
summary(m0)

#Predicted responses as Figures
#Mean and 95% CI for sex by species, at specific values of tl_cm
predict_response(m0, terms = c("sex", "tl_cm", "common_name"))%>%
  plot()

#Regressions and 95% CI for species by sex, across the range of values of tl_cm
predict_response(m0, terms = c("tl_cm", "sex", "common_name"))%>%
  plot()

#Regressions and 95% CI for species faceted by sex, 
#across a wider range of values of tl_cm (as specified)
predict_response(m0, terms = c("tl_cm [0:175]", "common_name", "sex"))%>%
  plot() + #addition of ggplot elements
  scale_x_continuous(limits = c(0, 175))

#Alternatively...save responses
resp<-predict_response(m0, terms = c("tl_cm [0:175]", "common_name", "sex"))
#...and use ggplot...
#BUT beware that all the variable names change!
ggplot(resp, aes(x = x, y = predicted, color = group)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 175)) +
  facet_wrap(facet~.)+
  theme_bw()

#Predict the y-intercept of the regression lines by sex
#Setting tl_cm, x value to 0
#y-intercept in a y=mx+b linear model
predict_response(m0, terms = c("sex", "tl_cm[0]", "common_name"))

#Set tl_cm to the mean
predict_response(m0, terms = c("sex", "tl_cm[mean]", "common_name"))
predict_response(m0, terms = c("sex", "tl_cm", "common_name"))

#Test predictions at tl_cm average
#At the mean level of tl_cm, is there a significant
#difference between sexes?
#Sex values are on each line at the mean value of tl_cm
test_predictions(m0, terms = c("sex", "common_name"))

#Test differences in slopes
test_predictions(m0, terms = c("tl_cm"), by=c("sex", "common_name"))

#----Model 2: Linear with log-link regression----
#Same analysis as above, but now the linear equation
#has a log link (this is different than the response and/or
#the predictor being logged!)
#Also, in R, log is e, whereas log10 is base 10 log
m1<-glm(hg_ww_ppm~tl_cm*sex*common_name, 
         family=gaussian(link="log"), 
         data=mydata)
summary(m1)

#To get the coefficients on the response scale
#we must exponentiate them otherwise
#they remain on the log scale
tidy(m1) #same as summary(m1)
tidy(m1, exponentiate = TRUE)

fig0<-predict_response(m1, terms = c("tl_cm","common_name", "sex"))%>%
   plot(show_data=TRUE)+
  #  plot(show_data=TRUE,limit_range=TRUE)+
  theme_bw(12) +
  theme(legend.position = "bottom") +
  labs(colour = "Species",
       y = expression("Mercury Concentration (mg·kg"^-1*" ww)"),
       x = "Total Length (cm)",
       title = "")
fig0

#Save figure
ggsave("Model0.png", fig0, width=20, height = 15, units = "cm")

#----Model for STIB----
# #Filter the data
bonnet<-mydata%>%
  filter(common_name=="Bonnethead")
str(bonnet)

#Linear regression
m2<-glm(hg_ww_ppm~tl_cm*sex,
        data=bonnet)
#Model summary
#To get sex M coefficient (or slope) add up coefficients
summary(m2)

#Visualize the model
predict_response(m2, terms = c("tl_cm[0,95]", "sex"))%>%
  plot()

#Predict the y-intercept of the regression lines by sex
#Setting tl_cm to 0
predict_response(m2, terms = c("sex", "tl_cm[0]"))
#Values match those in the summary(m2)

#Set tl_cm to the mean
mean(bonnet$tl_cm)
predict_response(m2, terms = c("sex", "tl_cm[mean]"))
predict_response(m2, terms = c("sex"))

#At the mean level of tl_cm, is there a significant
#difference between sexes?
#Sex values are on each line at the mean value of tl_cm
1.22-0.74
test_predictions(m2, terms = c("sex"))

#What are the slopes?
FemaleSlope<-summary(m2)$coefficients[2] #have to calculate slopes based on 'intercept'
MaleSlope<-sum(summary(m2)$coefficients[4],summary(m2)$coefficients[2])
Diff<-MaleSlope-FemaleSlope
tidy(m2)

#Here's another way!
#library(marginaleffects)
# Compute slopes for tl_cm by sex
#comparisons(m2, variables = "tl_cm", by = "sex")

#Test differences in slopes
test_predictions(m2, terms = c("tl_cm"), by=c("sex"), engine = "marginaleffects")

#Linear with log-link regression
#Same analysis as above, but now the linear equation
#has a log link (this is different than the response and/or
#the predictor being logged!)
#Also, in R, log is e, whereas log10 is base 10 log
m3<-glm(hg_ww_ppm~tl_cm*sex, 
         family=gaussian(link="log"), 
         data=bonnet)
summary(m3)

alpha=exp(-2.76017)  #Value of THg when TL=0, intercept for Females
alpha
alpha+exp(-2.05270)  #value of THg when TL=0, intercept for Males
slopeF<-0.03238      #log(slope) for Females

#To get the coefficients on the response scale
#we must exponentiate them otherwise
#they remain on the log scale
tidy(m3) #same as summary(m3)
tidy(m3, exponentiate = TRUE)

#Test the y-intercept of the regression lines by sex
#Where x=0 i.e. at the intercept
predict_response(m3, terms = c("sex", "tl_cm[0]"))

#Model outcome plot
fig3<-predict_response(m3, terms = c("tl_cm","sex"))%>%
  plot(show_data=TRUE)+
  theme_bw(12) +
  theme(legend.position = "bottom") +
  labs(colour = "Sex",
       y = expression("Mercury Concentration (mg·kg"^-1*" ww)"),
       x = "Total Length (cm)",
       title = "")

fig3

#Save figure
ggsave("Model3.png", fig3, width=20, height = 15, units = "cm")

#----Testing Differences----
predict_response(m3, terms = c("sex", "tl_cm[70]"))
predict_response(m3, terms = c("sex", "tl_cm[90]"))
predict_response(m3, terms = c("sex", "tl_cm[minmax]"))
predict_response(m3, terms = c("sex", "tl_cm[threenum]"))

#Fun with predictions (or confusion...)
#Where x=mean(x) i.e. at the mean length
predict_response(m3, terms = c("sex", "tl_cm"))
mean(bonnet$tl_cm)
predict_response(m3, terms = c("sex", "tl_cm[mean]"))
predict_response(m3, terms = c("sex", "tl_cm[73.89]"))

#At the mean level of tl_cm, is there a significant
#difference between sexes?
#Test predictions at tl_cm average - what what 'average'?!?
test_predictions(m3, terms = c("sex")) #default is at mean x
predict_response(m3, terms = c("sex", "tl_cm[74]"))

#Get some predicted concentrations across tl_cm
predict_response(m3, terms = c("tl_cm"), by=c("sex"))

#Test differences in slopes
#BUT CAREFUL - It's only at ONE slope!
#Remember this is a non-linear line (slope is linear on the log scale!)
test_predictions(m3, terms = c("tl_cm"), by=c("sex"))

#Use these contrasts to report on differences in slopes
#BUT they are on the log scale!
#summary(m3) #shows that slopes are on the log scale
summary(m3)
slopes <- emtrends(m3, specs = c("sex"), var = "tl_cm")
summary(slopes, infer = TRUE)
contrast(slopes, method = "pairwise")
