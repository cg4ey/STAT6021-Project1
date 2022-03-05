#setwd("C:/Users/ddd/Documents/UVA/MSDS/Spring 2022/STAT6021 Project1")

df<-read.csv(("diamonds4.csv"))
df2<-read.csv(("diamonds4.csv"))
  #df2 does not do the order properly for SLR/MLR

##Let's make some plots that have fillers with other variables to see where the relationships are. 

library(ggplot2)
#remove numerical transformations of categorical data

ggplot(df,aes(y=price,x=carat,color=clarity,size=color,shape=cut))+
  geom_point(alpha=.3)+
  theme(plot.title=element_text(hjust=.5))+
  labs(x="Carat Weight",title="Diamond Prices by Carat, Cut, Clarity and Color")

##really hard to see overlapping, so lets cut out those above 2.5 carats

df1.2<-df%>%
  filter(carat<=2.5)
ggplot(df1.2,aes(y=carat,x=price,color=clarity,size=color,shape=cut))+
  geom_point(alpha=.3)+
  theme(plot.title=element_text(hjust=.5))+
  labs(x="Carat Weight",title="Diamond Prices by Carat, Cut, Clarity and Color")
##here should be y=price, x=carat, but easier to look at this way for me because left to right
## looks like we see more less ideally cut stones, more yellow stones, and stones with more inclusions as we get lower in price

##let's zoom in again
df1.3<-df%>%
  filter(carat<=1)
ggplot(df1.3,aes(x=carat,y=price,color=clarity,size=color,shape=cut))+
  geom_point(alpha=.3)+
  theme(plot.title=element_text(hjust=.5))+
  labs(x="Carat Weight",title="Diamond Prices by Carat, Cut, Clarity and Color")
##better view here to see that the stones with more inclusions and more yellow stones are cheaper
## hard to tell where the Astor cut lies, but for all others it seems that the more ideal the cut the more expensive the stone. 

##need to make all predictor values numerical.
library(tidyverse)

df2<-read.csv(("diamonds4.csv"))
#df2 does not do the order properly for SLR/MLR

df2$cut<-factor(df2$cut)
df2$cut<-as.numeric(df2$cut)

df2$clarity<-factor(df$clarity)
df2$clarity<-as.numeric(df2$clarity)

df2$color<-factor(df2$color)
df2$color<-as.numeric(df2$color)

is.numeric(df2$carat)
is.numeric(df2$price)

##these values not really assigning properly.. let's do it manually

unique(df$cut)
##Very Good, Ideal, Good, Astor Ideal
##Order here is Poor/Fair, Good, Very Good, Ideal, Astor by BlueNile
  
df['cut'][df['cut']=='Good']<-0
df['cut'][df['cut']=='Very Good']<-1
df['cut'][df['cut']=='Ideal']<-2
df['cut'][df['cut']=='Astor Ideal']<-3

unique(df$color)
## "I" "H" "D" "F" "G" "J" "E"
##Order: L-Z,K,J,I,H,G,F,E,D

df['color'][df['color']=='J']<-0
df['color'][df['color']=='I']<-1
df['color'][df['color']=='H']<-2
df['color'][df['color']=='G']<-3
df['color'][df['color']=='F']<-4
df['color'][df['color']=='E']<-5
df['color'][df['color']=='D']<-6

unique(df$clarity)
##"SI2"  "IF"   "VVS2" "VS1"  "SI1"  "VVS1" "VS2"  "FL"
##Order: I3-I2, I1, SI2, SI1, VS2, VS1, VVS2, VVS1, IF, FL

df['clarity'][df['clarity']=='SI2']<-0
df['clarity'][df['clarity']=='SI1']<-1
df['clarity'][df['clarity']=='VS2']<-2
df['clarity'][df['clarity']=='VS1']<-3
df['clarity'][df['clarity']=='VVS2']<-4
df['clarity'][df['clarity']=='VVS1']<-5
df['clarity'][df['clarity']=='IF']<-6
df['clarity'][df['clarity']=='FL']<-7

is.numeric(df$carat)
is.numeric(df$clarity)
df$clarity<-as.numeric(df$clarity)
is.numeric(df$color)
df$color<-as.numeric(df$color)
is.numeric(df$cut)
df$cut<-as.numeric(df$cut)
is.numeric(df$price)


###all predictor variables are now numeric and in proper order. Now we will be able to properly use lm()

pairs(df,upper.panel = NULL)
##this makes it look like the carat is the only thing that really has a near-linear relationship with price. However, I believe that all of the other
## variables will contribute to the price. So, we will have to do MLR tests to see which ones create the best model. 
## Also need to determine which predictors have greatest weight on the models - to see if their claims of importance are correct

##Want to test for SLR first (carat, clarity, color, cut)
##Then test all combos for MLR:
#Carat + one other variable (clarity, color, cut)
#Carat + two other variables (clarity & color, clarity & cut, cut & color)
#Carat + all variables (clarity & color & cut)

##Things to do: Residuals plots, transformations if necessary, ACF, QQ
##Want to find all statistical values for these models, interpret results, 


#####This project only covers up to SLR... so no MLR... hmm... 

