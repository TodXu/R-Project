library(readr)
library(car)
install.packages("ggplot2")
install.packages("leaps")
library(leaps)
library(ggplot2)
basketball_teams$fg_rate<-basketball_teams$o_fgm/basketball_teams$o_fga
##2 and 3 points shooting rate
summary(basketball_teams$fg_rate)

#2 points shooting rate
basketball_teams$t2p_rate<-(basketball_teams$o_fgm-basketball_teams$o_3pm)/(basketball_teams$o_fga-basketball_teams$o_3pa )*100
summary(t2p_rate)

basketball_teams$ft_rate<-basketball_teams$o_ftm/basketball_teams$o_fta
##free theow shooting rate
summary(ft_rate)

basketball_teams$t3p_rate<-basketball_teams$o_3pm/basketball_teams$o_3pa*100
##three point shooting rate 
summary(t3p_rate)

##basketball_teams$t3p_percent<-basketball_teams$o_3pa/basketball_teams$o_fga
##summary(t3p_percent)



basketball_teams$t3pm_percent<-basketball_teams$o_3pm/basketball_teams$o_fgm*100
summary(t3pm_percent)

##o_oreb    offensive rebound
summary(basketball_teams$o_oreb)

##o_dreb    defensive rebound
summary(basketball_teams$o_dreb)

##o_asts    assist 
summary(basketball_teams$o_asts)

##o_stl     steal
summary(basketball_teams$o_stl)

##o_to    turnover
summary(basketball_teams$o_to)

##o_blk   block 
summary(basketball_teams$o_blk)

##o_pf  foul
summary(basketball_teams$o_pf)

##won
summary(basketball_teams$won)

##lost
summary(basketball_teams$lost)

basketball_teams$won_rate<-basketball_teams$won/(basketball_teams$won+basketball_teams$lost)



library(readr)




#boston is really a fucking outlier





## in order to implement step wise selection and Leave one out cross validation,  we clean our data set and make sure that there is no linear dependencies in our variables

cor(basketball_teams$o_stl,basketball_teams$d_to)
cor(basketball_teams$d_asts,basketball_teams$d_pts)
cor(basketball_teams$o_stl,basketball_teams$d_to)
cor(basketball_teams$d_asts,basketball_teams$d_pts)
cor(basketball_teams$d_to,basketball_teams$o_pf)

d1<-data.frame(basketball_teams)
d1[1:10]<-list(NULL)##first, variables such as team name, team location and so on are removed from our data set  
d1[50]<-list(NULL)
d1[48]<-list(NULL)
d1[31:48]<-list(NULL)##further, redundant variables like home,away won lost

d1[9]<-list(NULL)##reb
d1[23]<-list(NULL)

d1[1:2]<-list(NULL)##field goal

d1[9]<-list(NULL)

d1[20]<-list(NULL)

d1[22]<-list(NULL)

summary(d1)


##we use leave one out cross validtion£¬ forward step wise selection

b1<-rep(0,30)
b2<-rep(0,30)
b3<-rep(0,30)
b4<-rep(0,30)

for (i in 1 : 30 ){
  temp1<-d1
  temp1<-temp1[-c(i,30+i,60+i,90+i,120+i),]
  t2<-regsubsets(won_rate~.,data=temp1,nvmax=15, method = "forward")
  t3<-summary(t2)
  names(t3)
  b1[i]<-which.max(t3$adjr2)
  b2[i]<-max(t3$adjr2)
  b3[i]<-which.min(t3$rss)
  b4[i]<-min(t3$rss)
  
  par(mfrow=c(2,2))
  plot(t3$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
  plot(t3$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
  points (which.max(t3$adjr2),t3$adjr2[which.max(t3$adjr2)], col="red",cex=2,pch =20)
  plot(t3$cp ,xlab="Number of Variables ",ylab="Cp",type="l")
  points (which.min(t3$cp ),t3$cp [which.min(t3$cp )], col ="red",cex=2,pch =20)
  plot(t3$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
  points (which.min(t3$bic ),t3$bic [which.min(t3$bic )],col="red",cex=2,pch =20)
  
}



which.min(b4)

b2[28]

b1[28]


temp2<-d1
temp2<-temp2[-c(28,30+28,60+28,90+28,120+28),]
t4<-regsubsets(won_rate~.,data=temp2,nvmax=15, method = "forward")
t5<-summary(t4)
par(mfrow=c(2,2))
plot(t5$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(t5$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
points (which.max(t5$adjr2),t5$adjr2[which.max(t5$adjr2)], col="red",cex=2,pch =20)
plot(t5$cp ,xlab="Number of Variables ",ylab="Cp",type="l")
points (which.min(t5$cp ),t5$cp [which.min(t5$cp )], col ="red",cex=2,pch =20)
plot(t5$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points (which.min(t5$bic ),t5$bic [which.min(t5$bic )],col="red",cex=2,pch =20)

names(t5)

t5$adjr2

t5

t10<-lm(won_rate~o_3pm+o_3pa+o_dreb+o_pf+o_stl+o_to+o_blk+o_pts+d_dreb+d_asts+d_pf+d_to+d_pts+fg_rate+t2p_rate,data=basketball_teams )

summary(t10)


t11<-lm(won_rate~o_3pm+o_3pa+o_dreb+o_pf+o_to+o_blk+o_pts+d_dreb+d_pf+d_to+d_pts+fg_rate+t2p_rate,data=basketball_teams )
summary(t11)

t12<-lm(won_rate~o_oreb+o_dreb+o_asts+o_to+o_blk+o_pts+d_fgm+d_ftm+d_pf+d_stl+d_to+d_pts +t2p_rate+ft_rate+t3p_rate,data=basketball_teams )
summary(t12)
