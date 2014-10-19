statesInfo<-read.csv('stateData.csv')
subset1<-subset(statesInfo,income>4000)
subset<-statesInfo[statesInfo$state.region==1,]

reddit<-read.csv('reddit.csv')
table(reddit$employment.status)
str(reddit)
levels(reddit$age.range)

install.packages("ggplot2")
library(ggplot2)
qplot(data=reddit,x=ordered(age.range))

list.files()
pf<-read.csv('pseudo_facebook.tsv', sep='\t')
names(pf)

library(ggplot2)
qplot(x=dob_day,data=pf) +
  scale_x_discrete(breaks=1:31) +
  facet_wrap(~dob_month,ncol=3)

qplot(x=friend_count, data=pf,xlim=c(0,1000),binwidth=25,breaks=seq(0,1000,50))

qplot(x=friend_count, data=na.omit(pf),binwidth=25) +
  scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50)) +
  facet_wrap(~gender)

qplot(x=friend_count, data=subset(pf,!is.na(gender)),binwidth=25) +
  scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50)) +
  facet_wrap(~gender)

qplot(x=friend_count, y=..count../sum(..count..),data=subset(pf,!is.na(gender)),binwidth=10,
      geom='freqpoly',color=gender) +
  scale_x_continuous(lim=c(0,1000),breaks=seq(0,1000,50)) 

table(pf$gender)
by(pf$friend_count,pf$gender,summary)

qplot(x=tenure/365,data=pf,binwidth=0.5,xlab='x',ylab='y',
      color=I('black'),fill=I('#F79420')) +
  scale_x_continuous(breaks=seq(1,7,1),lim=c(0,7))

qplot(x=age,data=pf,binwidth=1,xlab='age',ylab='number',
      color=I('black'),fill=I('#F79420'))+ 
  scale_x_continuous(breaks=seq(0,100,5),lim=c(0,100))+
  facet_wrap(~gender)

qplot(x=1+friend_count,data=pf,log='x')
summary(pf$friend_count)
summary(log10(1+pf$friend_count))

install.packages("gridExtra")
library(gridExtra)
p1<-qplot(x=friend_count,data=pf)
p2<-qplot(x=log10(1+friend_count),data=pf)
p3<-qplot(x=sqrt(friend_count),data=pf)

grid.arrange(p1,p2,p3)


p1<-ggplot(aes(x=friend.count), data=pf) + geom_histogram()
p2<-p1 + scale_x_log10()
p3<-p1 + scale_x_sqrt()



qplot(x=www_likes, data=subset(pf,!is.na(gender)),geom='freqpoly',color=gender) +
  scale_x_continuous() + scale_x_log10()
summary(pf$www_likes)
by(pf$www_likes,pf$gender,sum)

qplot(x=gender, y=friend_count, data=subset(pf,!is.na(gender)),geom='boxplot',ylim=c(0,1000))
qplot(x=gender, y=friend_count, data=subset(pf,!is.na(gender)),geom='boxplot') +
  scale_y_continuous(limits=c(0,1000))
qplot(x=gender, y=friend_count, data=subset(pf,!is.na(gender)),geom='boxplot') +
  coord_cartesian(ylim=c(0,1000))
by(pf$friend_count,pf$gender,summary)

mobile_check_in = NA
pf$mobile_check_in<-ifelse(pf$mobile_likes>0,1,0)
pf$mobile_check_in<-factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in==1)/length(pf$mobile_check_in)


#Problem set 1
library(ggplot2)
data(diamonds)
names(diamonds)
length(names(diamonds))
summary(diamonds)
dim(diamonds)
qplot(x=diamonds$price, data=diamonds,log='x')
diamonds500<-subset(diamonds,price>=15000)
dim(diamonds500)

qplot(x=diamonds$price, data=diamonds, binwidth=100) +
  scale_x_continuous(breaks=seq(1,1000,100)) +
  facet_wrap(~cut)
ggsave('ex2.tif')
by(diamonds$price,diamonds$color,summary)

qplot(x=cut,y=price, data=diamonds, binwidth=100, logs='x', geom='boxplot') 
IQR(subset(diamonds, price <1000)$price) 

qplot(x=carat, data=diamonds,geom='freqpoly') 

pf<-read.csv('pseudo_facebook.tsv',sep='\t')
qplot(x=age,y=friend_count,data=pf)
ggplot(aes(x=age,y=friend_count),data=pf)+
  xlim(13,90)+
  geom_point(alpha=1/20,position=position_jitter(h=0))+
  coord_trans(y='sqrt')
install.packages('dplyr')
library(dplyr)

#filter()


setwd('/Users/Arash/Documents/Data analysis')
pf<-read.csv('pseudo_facebook.csv',sep='\t')
age_groups<-group_by(pf,age)
pf.fc_by_age<-summarise(age_groups,
                        friend_count_mean=mean(friend_count),
                        firend_count_median=median(friend_count),
                        n=n())
pf.fc_by_age<-arrange(pf.fc_by_age,age)
head(pf.fc_by_age)




setwd('/Users/Arash/Documents/Data analysis/data')
list.files()
pf<-read.csv('pseudo_facebook.tsv',sep='\t')
library(ggplot2)
qplot(x=age,y=friend_count,data=pf)
ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20)+
  xlim(13,90)

ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20, position=position_jitter(h=0))+
  xlim(13,90)+
  coord_trans(y='sqrt')

library(dplyr)
# filter, group_by, mutate, arrange
age_groups<-group_by(pf,age)
pf.fc_by_age <- summarise(age_groups,
          friend_count_mean=mean(friend_count),
          friend_count_median=median(friend_count),
          n=n())
pf.fc_by_age<-arrange(pf.fc_by_age,age)

# chain functions using %.% 

pf.fc_by_age<-pf %.% group_by(age) %.% summarise(friend_count_mean=mean(friend_count),
                                   friend_count_median=median(friend_count),
                                   n=n()) %.%
  arrange(age)

# or
pf.fc_by_age_tapply<-tapply(pf$friend_count,pf$age,mean)
plot(pf.fc_by_age_tapply)

by_cyl <- group_by(mtcars, cyl)
summarise(by_cyl, mean(disp))
# Equivalent to
tapply(mtcars$disp,mtcars$cyl,mean)

ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20, 
             position=position_jitter(h=0),
             color='orange')+
  geom_line(stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=quantile,probs=.1,linetype=2,color='blue')+
  coord_trans(y='sqrt')+
  xlim(13,90)+
  ylim(0,1000)

cor.test(x=pf$age,y=pf$friend_count)
# use with to operate in the space of a particular
# dataframe, $ not needed
with(pf,cor.test(x=age,y=friend_count))
with(pf[pf$age<=70,],cor.test(x=age,y=friend_count))
# or
with(subset(pf,age<=70),cor.test(x=age,y=friend_count))


ggplot(aes(x=www_likes_received,y=likes_received),data=pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$www_likes_received,0.95))+
  geom_smooth(method='lm',color='red')

cor.test(pf$www_likes_received,pf$likes_received)

install.packages('alr3')
install.packages('car')
library(car)
library(alr3)
data(Mitchell)
ggplot(aes(x=Month,y=Temp),data=Mitchell)+
  geom_point()+
  scale_x_discrete(breaks=seq(0,203,11))
cor.test(Mitchell$Month,Mitchell$Temp)

pf$age_with_months<-pf$age+(12-pf$dob_month)/12

pf.fc_by_age_with_months<-pf %.% group_by(age_with_months) %.% summarise(friend_count_mean=mean(friend_count),
                                                 friend_count_median=median(friend_count),
                                                 n=n()) %.%
  arrange(age_with_months)

p1<-ggplot(aes(x=age,y=friend_count_mean),
           data=subset(pf.fc_by_age, age_with_months<71))+
  geom_line()

p2<-ggplot(aes(x=age_with_months,y=friend_count_mean),
       data=subset(pf.fc_by_age_month, age_with_months<71))+
  geom_line()
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p2,p1,ncol=1)


ggplot(aes(x=gender,y=age),
       data=subset(pf,!is.na(gender)))+
  geom_boxplot()+
  stat_summary(fun.y=mean,geom='point',shape=4)


ggplot(aes(x=age,y=friend_count),
       data=subset(pf,!is.na(gender)))+
  geom_line(aes(color=gender),stat='summary',fun.y=median)


pf.fc_by_gender<-pf%.%
  filter(!is.na(gender))%.%
  group_by(age,gender)%.%
  summarise(mean_friend_count=mean(friend_count),
            median_friend_count=median(friend_count),
            n=n())%.%
  ungroup()%.%
  arrange(age)

ggplot(aes(x=age,y=median_friend_count),
       data=pf.fc_by_gender)+
  geom_line(aes(color=gender))

install.packages("reshape2")
library(reshape2)
pf.fc_by_gender.wide<-dcast(pf.fc_by_gender,
                            age~gender,
                            value.var='median_friend_count')

ggplot(aes(x=age,y=female/male),
       data=pf.fc_by_gender.wide)+
  geom_line()+
  geom_hline(yintercept=1,alpha=0.2,linetype=2)

pf$year_joined<-floor(2014-(pf$tenure/365))
table(pf$year_joined)
pf$year_joined.bucket<-cut(pf$year_joined,
                           breaks=c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket)

ggplot(aes(x=age,y=friend_count),
       data=subset(pf,!is.na(year_joined.bucket)))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=mean)+
  geom_line(stat='summary',fun.y=mean,linetype=2)

with(subset(pf,tenure>=1),summary(friend_count/tenure))

file_url<-'https://s3.amazonaws.com/udacity-hosted-downloads/ud651/yogurt.csv'
download.file(file_url,destfile='yogurt.csv',method='curl')

yo<-read.csv('yogurt.csv')
str(yo)
yo$id<-factor(yo$id)

qplot(x=price,data=yo)
unique(yo$price)
yo<-transform(yo,all.purchases=strawberry+blueberry+pina.colada+plain+mixed.berry)

set.seed(4230)
sample.ids<-sample(levels(yo$id),16)

ggplot(aes(x=time,y=price),
       data=subset(yo,id %in% sample.ids))+
  facet_wrap(~id)+
  geom_line()+
  geom_point(aes(size=all.purchases),pch=1)

# x %in% y returns a logical (boolean) vector 
# the same length as x that says whether each
# entry in x appears in y. That is, for each
# entry in x, it checks to see whether it is
# in y.

install.packages("GGally")
set.seed(1836)
library(GGally)
pf_subset<-pf[,c(2:15)]
ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),])

file_url<-'https://s3.amazonaws.com/udacity-hosted-downloads/ud651/nci.tsv'
download.file(file_url,destfil='nci.tsv',method='curl')
nci<-read.table('nci.tsv')
nci.long.samp<-melt(as.matrix(nci[1:200,]))
names(nci.long.samp)<-c('gene','case','value')
head(nci.long.samp)

ggplot(aes(y=gene,x=case,fill=value),
       data=nci.long.samp)+
  geom_tile()+
  scale_fill_gradientn(colours=colorRampPalette(c('blue','red'))(100))




data(diamonds)
qplot(x=carat,y=price,data=diamonds)

