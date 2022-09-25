
options(scipen=999)

library('varhandle')
library('data.table')
library('plyr')
library('dplyr')
library('gdata')



Firstbaseball=read.csv('baseballsavant.csv',TRUE,',')

baseballID=read.csv('baseballnamestoID.csv',TRUE,',')
baseballID2=read.csv('baseballnamestoID.csv',TRUE,',')
setnames(baseballID2,"PitcherName","ExtraPlayerName")





realpitchfx=read.csv("realpitchfx.csv",TRUE,',')
realpitchfx$X=NULL
realpitchfx2=read.csv("realpitchfx2.csv",TRUE,',')
realpitchfx2$X=NULL
realpitchfx=rbind(realpitchfx,realpitchfx2)
realpitchfx=subset(realpitchfx,sv_id!="_")
Firstbaseball=merge(Firstbaseball,realpitchfx,by="sv_id")

Firstbaseball=merge(Firstbaseball,baseballID,by.x='pitcher.y',by.y='mlbid')
Firstbaseball=merge(Firstbaseball,baseballID2,by.x='batter.x',by.y='mlbid')
Firstbaseball$player_name=Firstbaseball$ExtraPlayerName
Firstbaseball=subset(Firstbaseball,sv_id!="null")
Firstbaseball$ExtraPlayerName=NULL

Firstbaseball[is.na(Firstbaseball)]<-0
Firstbaseball=unfactor(Firstbaseball)
Firstbaseball[is.na(Firstbaseball)]<-0

Firstbaseball$pitch_type=ifelse(Firstbaseball$pitch_type=="KC","CU",Firstbaseball$pitch_type)

keep(Firstbaseball,sure=TRUE)


###########################################################







batterlineup=read.csv('baseballlineup2.csv',TRUE,',')
pitcherlineup=read.csv('pitcherlineup.csv',TRUE,',')
BaseballTeams=read.csv("BaseballTeams.csv",TRUE,",")
averagehitspeed=read.csv("averagehitspeed.csv",TRUE,",")
averagelaunchangle=read.csv("averagelaunchangle.csv",TRUE,",")

BaseballTeams$X=NULL
HomeTeam="Dodgers"
AwayTeam="Yankees"
pitcherlineup=subset(pitcherlineup,Team==HomeTeam)
batterlineup=subset(batterlineup,Team==AwayTeam)

baseball=Firstbaseball



batters=as.data.frame(batterlineup$Player)
pitchers=as.data.frame(pitcherlineup$Pitcher)

baseball=subset(baseball,player_name==batters[1,]| player_name==batters[2,]| player_name==batters[3,]| player_name==batters[4,]| player_name==batters[5,]| player_name==batters[6,]| player_name==batters[7,]| player_name==batters[8,]| player_name==batters[9,]| player_name==batters[10,]| player_name==batters[11,]| player_name==batters[12,]| player_name==batters[13,]| player_name==batters[14,]| player_name==batters[15,]| player_name==batters[16,]| player_name==batters[17,]| player_name==batters[18,]| player_name==batters[19,]|player_name==batters[20,]|player_name==batters[21,]|player_name==batters[22,]|player_name==batters[23,]|PitcherName==pitchers[1,]| PitcherName==pitchers[2,]| PitcherName==pitchers[3,]| PitcherName==pitchers[4,]| PitcherName==pitchers[5,]| PitcherName==pitchers[6,]| PitcherName==pitchers[7,])






batterlineup=read.csv('baseballlineup2.csv',TRUE,',')
pitcherlineup=read.csv('pitcherlineup.csv',TRUE,',')
pitcherlineup=subset(pitcherlineup,Team==HomeTeam)
batterlineup=subset(batterlineup,Team==AwayTeam)

baseball=na.omit(baseball)

baseball=subset(baseball,effective_speed!="NA")



baseball$FF=0
baseball$SL=0
baseball$CU=0
baseball$CH=0
baseball$FT=0
baseball$SI=0
baseball$FS=0
baseball$KC=0
baseball$IN=0
baseball$PO=0
baseball$UN=0
baseball$EP=0
baseball$SC=0
baseball$FC=0
baseball$FF=ifelse(baseball$pitch_type=="FF",1,0)
baseball$SL=ifelse(baseball$pitch_type=="SL",1,0)
baseball$CU=ifelse(baseball$pitch_type=="CU",1,0)
baseball$CH=ifelse(baseball$pitch_type=="CH",1,0)
baseball$FT=ifelse(baseball$pitch_type=="FT",1,0)
baseball$SI=ifelse(baseball$pitch_type=="SI",1,0)
baseball$FS=ifelse(baseball$pitch_type=="FS",1,0)
baseball$KC=ifelse(baseball$pitch_type=="KC",1,0)
baseball$IN=ifelse(baseball$pitch_type=="IN",1,0)
baseball$PO=ifelse(baseball$pitch_type=="PO",1,0)
baseball$UN=ifelse(baseball$pitch_type=="UN",1,0)
baseball$EP=ifelse(baseball$pitch_type=="EP",1,0)
baseball$SC=ifelse(baseball$pitch_type=="SC",1,0)
baseball$FC=ifelse(baseball$pitch_type=="FC",1,0)





playerpercentagesFF=with(baseball,aggregate(FF,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFF,"Group.2","PitchHand")
setnames(playerpercentagesFF,"Group.3","Balls")
setnames(playerpercentagesFF,"Group.4","Strikes")
setnames(playerpercentagesFF,"x","FFPercentage")
playerpercentagesSL=with(baseball,aggregate(SL,list(player_name,p_throws,balls,strikes),mean))				
setnames(playerpercentagesSL,"Group.2","PitchHand")				
setnames(playerpercentagesSL,"x","SLPercentage")				
setnames(playerpercentagesSL,"Group.3","Balls")
setnames(playerpercentagesSL,"Group.4","Strikes")

playerpercentagesCU=with(baseball,aggregate(CU,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesCU,"Group.2","PitchHand")
setnames(playerpercentagesCU,"x","CUPercentage")
setnames(playerpercentagesCU,"Group.3","Balls")
setnames(playerpercentagesCU,"Group.4","Strikes")

playerpercentagesCH=with(baseball,aggregate(CH,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesCH,"Group.2","PitchHand")
setnames(playerpercentagesCH,"x","CHPercentage")
setnames(playerpercentagesCH,"Group.3","Balls")
setnames(playerpercentagesCH,"Group.4","Strikes")

playerpercentagesFT=with(baseball,aggregate(FT,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFT,"Group.2","PitchHand")
setnames(playerpercentagesFT,"x","FTPercentage")
setnames(playerpercentagesFT,"Group.3","Balls")
setnames(playerpercentagesFT,"Group.4","Strikes")

playerpercentagesSI=with(baseball,aggregate(SI,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesSI,"Group.2","PitchHand")
setnames(playerpercentagesSI,"x","SIPercentage")
setnames(playerpercentagesSI,"Group.3","Balls")
setnames(playerpercentagesSI,"Group.4","Strikes")

playerpercentagesFS=with(baseball,aggregate(FS,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFS,"Group.2","PitchHand")
setnames(playerpercentagesFS,"x","FSPercentage")
setnames(playerpercentagesFS,"Group.3","Balls")
setnames(playerpercentagesFS,"Group.4","Strikes")

playerpercentagesKC=with(baseball,aggregate(KC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesKC,"Group.2","PitchHand")
setnames(playerpercentagesKC,"x","KCPercentage")
setnames(playerpercentagesKC,"Group.3","Balls")
setnames(playerpercentagesKC,"Group.4","Strikes")

playerpercentagesIN=with(baseball,aggregate(IN,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesIN,"Group.2","PitchHand")
setnames(playerpercentagesIN,"x","INPercentage")
setnames(playerpercentagesIN,"Group.3","Balls")
setnames(playerpercentagesIN,"Group.4","Strikes")

playerpercentagesPO=with(baseball,aggregate(PO,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesPO,"Group.2","PitchHand")
setnames(playerpercentagesPO,"x","POPercentage")
setnames(playerpercentagesPO,"Group.3","Balls")
setnames(playerpercentagesPO,"Group.4","Strikes")

playerpercentagesUN=with(baseball,aggregate(UN,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesUN,"Group.2","PitchHand")
setnames(playerpercentagesUN,"x","UNPercentage")
setnames(playerpercentagesUN,"Group.3","Balls")
setnames(playerpercentagesUN,"Group.4","Strikes")

playerpercentagesEP=with(baseball,aggregate(EP,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesEP,"Group.2","PitchHand")
setnames(playerpercentagesEP,"x","EPPercentage")
setnames(playerpercentagesEP,"Group.3","Balls")
setnames(playerpercentagesEP,"Group.4","Strikes")

playerpercentagesSC=with(baseball,aggregate(SC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesSC,"Group.2","PitchHand")
setnames(playerpercentagesSC,"x","SCPercentage")
setnames(playerpercentagesSC,"Group.3","Balls")
setnames(playerpercentagesSC,"Group.4","Strikes")

playerpercentagesFC=with(baseball,aggregate(FC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFC,"Group.2","PitchHand")
setnames(playerpercentagesFC,"x","FCPercentage")
setnames(playerpercentagesFC,"Group.3","Balls")
setnames(playerpercentagesFC,"Group.4","Strikes")






pitcherpercentagesFF=with(baseball,aggregate(FF,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFF,"Group.2","Batterhand")
setnames(pitcherpercentagesFF,"Group.3","Balls")
setnames(pitcherpercentagesFF,"Group.4","Strikes")
setnames(pitcherpercentagesFF,"x","FFPercentage")


pitcherpercentagesSL=with(baseball,aggregate(SL,list(PitcherName,stand,balls,strikes),mean))				
setnames(pitcherpercentagesSL,"Group.2","Batterhand")				
setnames(pitcherpercentagesSL,"x","SLPercentage")				
setnames(pitcherpercentagesSL,"Group.3","Balls")
setnames(pitcherpercentagesSL,"Group.4","Strikes")

pitcherpercentagesCU=with(baseball,aggregate(CU,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesCU,"Group.2","Batterhand")
setnames(pitcherpercentagesCU,"x","CUPercentage")
setnames(pitcherpercentagesCU,"Group.3","Balls")
setnames(pitcherpercentagesCU,"Group.4","Strikes")

pitcherpercentagesCH=with(baseball,aggregate(CH,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesCH,"Group.2","Batterhand")
setnames(pitcherpercentagesCH,"x","CHPercentage")
setnames(pitcherpercentagesCH,"Group.3","Balls")
setnames(pitcherpercentagesCH,"Group.4","Strikes")

pitcherpercentagesFT=with(baseball,aggregate(FT,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFT,"Group.2","Batterhand")
setnames(pitcherpercentagesFT,"x","FTPercentage")
setnames(pitcherpercentagesFT,"Group.3","Balls")
setnames(pitcherpercentagesFT,"Group.4","Strikes")

pitcherpercentagesSI=with(baseball,aggregate(SI,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesSI,"Group.2","Batterhand")
setnames(pitcherpercentagesSI,"x","SIPercentage")
setnames(pitcherpercentagesSI,"Group.3","Balls")
setnames(pitcherpercentagesSI,"Group.4","Strikes")

pitcherpercentagesFS=with(baseball,aggregate(FS,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFS,"Group.2","Batterhand")
setnames(pitcherpercentagesFS,"x","FSPercentage")
setnames(pitcherpercentagesFS,"Group.3","Balls")
setnames(pitcherpercentagesFS,"Group.4","Strikes")

pitcherpercentagesKC=with(baseball,aggregate(KC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesKC,"Group.2","Batterhand")
setnames(pitcherpercentagesKC,"x","KCPercentage")
setnames(pitcherpercentagesKC,"Group.3","Balls")
setnames(pitcherpercentagesKC,"Group.4","Strikes")

pitcherpercentagesIN=with(baseball,aggregate(IN,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesIN,"Group.2","Batterhand")
setnames(pitcherpercentagesIN,"x","INPercentage")
setnames(pitcherpercentagesIN,"Group.3","Balls")
setnames(pitcherpercentagesIN,"Group.4","Strikes")

pitcherpercentagesPO=with(baseball,aggregate(PO,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesPO,"Group.2","Batterhand")
setnames(pitcherpercentagesPO,"x","POPercentage")
setnames(pitcherpercentagesPO,"Group.3","Balls")
setnames(pitcherpercentagesPO,"Group.4","Strikes")

pitcherpercentagesUN=with(baseball,aggregate(UN,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesUN,"Group.2","Batterhand")
setnames(pitcherpercentagesUN,"x","UNPercentage")
setnames(pitcherpercentagesUN,"Group.3","Balls")
setnames(pitcherpercentagesUN,"Group.4","Strikes")

pitcherpercentagesEP=with(baseball,aggregate(EP,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesEP,"Group.2","Batterhand")
setnames(pitcherpercentagesEP,"x","EPPercentage")
setnames(pitcherpercentagesEP,"Group.3","Balls")
setnames(pitcherpercentagesEP,"Group.4","Strikes")

pitcherpercentagesSC=with(baseball,aggregate(SC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesSC,"Group.2","Batterhand")
setnames(pitcherpercentagesSC,"x","SCPercentage")
setnames(pitcherpercentagesSC,"Group.3","Balls")
setnames(pitcherpercentagesSC,"Group.4","Strikes")

pitcherpercentagesFC=with(baseball,aggregate(FC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFC,"Group.2","Batterhand")
setnames(pitcherpercentagesFC,"x","FCPercentage")
setnames(pitcherpercentagesFC,"Group.3","Balls")
setnames(pitcherpercentagesFC,"Group.4","Strikes")



playerpercentages=merge(playerpercentagesFF,playerpercentagesCU,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSL,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesCH,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFT,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSI,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFS,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesKC,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesIN,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesPO,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesUN,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesEP,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSC,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFC,by=c("Group.1","PitchHand",'Balls','Strikes'))




pitcherpercentages=merge(pitcherpercentagesFF,pitcherpercentagesCU,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSL,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesCH,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFT,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSI,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFS,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesKC,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesIN,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesPO,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesUN,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesEP,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSC,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFC,by=c("Group.1","Batterhand",'Balls','Strikes'))

pitcherpercentages$FFPercentage=ifelse(pitcherpercentages$FFPercentage<0.15,0,pitcherpercentages$FFPercentage)
pitcherpercentages$SLPercentage=ifelse(pitcherpercentages$SLPercentage<0.15,0,pitcherpercentages$SLPercentage)
pitcherpercentages$CHPercentage=ifelse(pitcherpercentages$CHPercentage<0.15,0,pitcherpercentages$CHPercentage)
pitcherpercentages$FTPercentage=ifelse(pitcherpercentages$FTPercentage<0.15,0,pitcherpercentages$FTPercentage)
pitcherpercentages$SIPercentage=ifelse(pitcherpercentages$SIPercentage<0.15,0,pitcherpercentages$SIPercentage)
pitcherpercentages$FSPercentage=ifelse(pitcherpercentages$FSPercentage<0.15,0,pitcherpercentages$FSPercentage)
pitcherpercentages$KCPercentage=ifelse(pitcherpercentages$KCPercentage<0.15,0,pitcherpercentages$KCPercentage)
pitcherpercentages$INPercentage=ifelse(pitcherpercentages$INPercentage<0.15,0,pitcherpercentages$INPercentage)
pitcherpercentages$POPercentage=ifelse(pitcherpercentages$POPercentage<0.15,0,pitcherpercentages$POPercentage)
pitcherpercentages$UNPercentage=ifelse(pitcherpercentages$UNPercentage<0.15,0,pitcherpercentages$UNPercentage)
pitcherpercentages$EPPercentage=ifelse(pitcherpercentages$EPPercentage<0.15,0,pitcherpercentages$EPPercentage)
pitcherpercentages$SCPercentage=ifelse(pitcherpercentages$SCPercentage<0.15,0,pitcherpercentages$SCPercentage)
pitcherpercentages$FCPercentage=ifelse(pitcherpercentages$FCPercentage<0.15,0,pitcherpercentages$FCPercentage)
pitcherpercentages$CUPercentage=ifelse(pitcherpercentages$CUPercentage<0.15,0,pitcherpercentages$CUPercentage)


####################
pitcherpercentages$sum=rowSums(pitcherpercentages[,5:18])
pitcherpercentages$multiplier=1/pitcherpercentages$sum
pitcherpercentages$FFPercentage=pitcherpercentages$FFPercentage*pitcherpercentages$multiplier
pitcherpercentages$CUPercentage=pitcherpercentages$CUPercentage*pitcherpercentages$multiplier
pitcherpercentages$SLPercentage=pitcherpercentages$SLPercentage*pitcherpercentages$multiplier
pitcherpercentages$CHPercentage=pitcherpercentages$CHPercentage*pitcherpercentages$multiplier
pitcherpercentages$FTPercentage=pitcherpercentages$FTPercentage*pitcherpercentages$multiplier
pitcherpercentages$SIPercentage=pitcherpercentages$SIPercentage*pitcherpercentages$multiplier
pitcherpercentages$FSPercentage=pitcherpercentages$FSPercentage*pitcherpercentages$multiplier
pitcherpercentages$KCPercentage=pitcherpercentages$KCPercentage*pitcherpercentages$multiplier
pitcherpercentages$INPercentage=pitcherpercentages$INPercentage*pitcherpercentages$multiplier
pitcherpercentages$POPercentage=pitcherpercentages$POPercentage*pitcherpercentages$multiplier
pitcherpercentages$UNPercentage=pitcherpercentages$UNPercentage*pitcherpercentages$multiplier
pitcherpercentages$EPPercentage=pitcherpercentages$EPPercentage*pitcherpercentages$multiplier
pitcherpercentages$SCPercentage=pitcherpercentages$SCPercentage*pitcherpercentages$multiplier
pitcherpercentages$FCPercentage=pitcherpercentages$FCPercentage*pitcherpercentages$multiplier
pitcherpercentages$newsum=rowSums(pitcherpercentages[,5:18])


############










setnames(playerpercentages,"Group.1","Player")
setnames(pitcherpercentages,"Group.1","Pitcher")
playerpercentages=merge(batterlineup,playerpercentages,by="Player")
pitcherpercentages=merge(pitcherlineup,pitcherpercentages,by="Pitcher")
Homepitcherpercentages=subset(pitcherpercentages,Team==HomeTeam)
Awaypitcherpercentages=subset(pitcherpercentages,Team==AwayTeam)
Homeplayerpercentages=subset(playerpercentages,Team==HomeTeam)
Awayplayerpercentages=subset(playerpercentages,Team==AwayTeam)

HomeBatterPercentages=merge(Homepitcherpercentages,Awayplayerpercentages,by=c("PitchHand","Balls","Strikes"))
AwayBatterPercentages=merge(Awaypitcherpercentages,Homeplayerpercentages,by=c("PitchHand","Balls","Strikes"))
HomeBatterPercentages$RealHand=HomeBatterPercentages$Hand
HomeBatterPercentages$FakeHand=ifelse((HomeBatterPercentages$Hand=='S'|HomeBatterPercentages$PitchHand=='R'),"L","R")
HomeBatterPercentages$Hand=as.character(HomeBatterPercentages$Hand)
HomeBatterPercentages$RealHand=ifelse(HomeBatterPercentages$Hand=='S',HomeBatterPercentages$FakeHand,HomeBatterPercentages$Hand)
HomeBatterPercentages=subset(HomeBatterPercentages,HomeBatterPercentages$RealHand==HomeBatterPercentages$Batterhand)
HomeBatterPercentages$prepercentageFF=ifelse(HomeBatterPercentages$FFPercentage.x>0.15,(HomeBatterPercentages$FFPercentage.x+HomeBatterPercentages$FFPercentage.y)/2,0)
HomeBatterPercentages$prepercentageCU=ifelse(HomeBatterPercentages$CUPercentage.x>0.15,(HomeBatterPercentages$CUPercentage.x+HomeBatterPercentages$CUPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSL=ifelse(HomeBatterPercentages$SLPercentage.x>0.15  ,(HomeBatterPercentages$SLPercentage.x+HomeBatterPercentages$SLPercentage.y)/2,0)
HomeBatterPercentages$prepercentageCH=ifelse(HomeBatterPercentages$CHPercentage.x>0.15 ,(HomeBatterPercentages$CHPercentage.x+HomeBatterPercentages$CHPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFT=ifelse(HomeBatterPercentages$FTPercentage.x>0.15 ,(HomeBatterPercentages$FTPercentage.x+HomeBatterPercentages$FTPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSI=ifelse(HomeBatterPercentages$SIPercentage.x>0.15  ,(HomeBatterPercentages$SIPercentage.x+HomeBatterPercentages$SIPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFS=ifelse(HomeBatterPercentages$FSPercentage.x>0.15  ,(HomeBatterPercentages$FSPercentage.x+HomeBatterPercentages$FSPercentage.y)/2,0)
HomeBatterPercentages$prepercentageKC=ifelse(HomeBatterPercentages$KCPercentage.x>0.15  ,(HomeBatterPercentages$KCPercentage.x+HomeBatterPercentages$KCPercentage.y)/2,0)
HomeBatterPercentages$prepercentageIN=ifelse(HomeBatterPercentages$INPercentage.x>0.15  ,(HomeBatterPercentages$INPercentage.x+HomeBatterPercentages$INPercentage.y)/2,0)
HomeBatterPercentages$prepercentagePO=ifelse(HomeBatterPercentages$POPercentage.x>0.15 ,(HomeBatterPercentages$POPercentage.x+HomeBatterPercentages$POPercentage.y)/2,0)
HomeBatterPercentages$prepercentageUN=ifelse(HomeBatterPercentages$UNPercentage.x>0.15  ,(HomeBatterPercentages$UNPercentage.x+HomeBatterPercentages$UNPercentage.y)/2,0)
HomeBatterPercentages$prepercentageEP=ifelse(HomeBatterPercentages$EPPercentage.x>0.15  ,(HomeBatterPercentages$EPPercentage.x+HomeBatterPercentages$EPPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSC=ifelse(HomeBatterPercentages$SCPercentage.x>0.15 ,(HomeBatterPercentages$SCPercentage.x+HomeBatterPercentages$SCPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFC=ifelse(HomeBatterPercentages$FCPercentage.x>0.15 ,(HomeBatterPercentages$FCPercentage.x+HomeBatterPercentages$FCPercentage.y)/2,0)
HomeBatterPercentages$SUM=HomeBatterPercentages$prepercentageFF+HomeBatterPercentages$prepercentageCU+HomeBatterPercentages$prepercentageSL+HomeBatterPercentages$prepercentageCH+HomeBatterPercentages$prepercentageFT+HomeBatterPercentages$prepercentageSI+HomeBatterPercentages$prepercentageFS+HomeBatterPercentages$prepercentageKC+HomeBatterPercentages$prepercentageIN+HomeBatterPercentages$prepercentagePO+HomeBatterPercentages$prepercentageUN+HomeBatterPercentages$prepercentageEP+HomeBatterPercentages$prepercentageSC+HomeBatterPercentages$prepercentageFC
HomeBatterPercentages$Multiplier=1/HomeBatterPercentages$SUM
HomeBatterPercentages$percentageFF=HomeBatterPercentages$prepercentageFF*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageCU=HomeBatterPercentages$prepercentageCU*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSL=HomeBatterPercentages$prepercentageSL*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageCH=HomeBatterPercentages$prepercentageCH*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFT=HomeBatterPercentages$prepercentageFT*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSI=HomeBatterPercentages$prepercentageSI*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFS=HomeBatterPercentages$prepercentageFS*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageKC=HomeBatterPercentages$prepercentageKC*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageIN=HomeBatterPercentages$prepercentageIN*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentagePO=HomeBatterPercentages$prepercentagePO*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageUN=HomeBatterPercentages$prepercentageUN*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageEP=HomeBatterPercentages$prepercentageEP*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSC=HomeBatterPercentages$prepercentageSC*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFC=HomeBatterPercentages$prepercentageFC*HomeBatterPercentages$Multiplier

AwayBatterPercentages$RealHand=AwayBatterPercentages$Hand
AwayBatterPercentages$FakeHand=ifelse((AwayBatterPercentages$Hand=='S'|AwayBatterPercentages$PitchHand=='R'),"L","R")
AwayBatterPercentages$Hand=as.character(AwayBatterPercentages$Hand)
AwayBatterPercentages$RealHand=ifelse(AwayBatterPercentages$Hand=='S',AwayBatterPercentages$FakeHand,AwayBatterPercentages$Hand)
AwayBatterPercentages=subset(AwayBatterPercentages,AwayBatterPercentages$RealHand==AwayBatterPercentages$Batterhand)
AwayBatterPercentages$prepercentageFF=ifelse(AwayBatterPercentages$FFPercentage.x>0.15 ,(AwayBatterPercentages$FFPercentage.x+AwayBatterPercentages$FFPercentage.y)/2,0)
AwayBatterPercentages$prepercentageCU=ifelse(AwayBatterPercentages$CUPercentage.x>0.15 ,(AwayBatterPercentages$CUPercentage.x+AwayBatterPercentages$CUPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSL=ifelse(AwayBatterPercentages$SLPercentage.x>0.15  ,(AwayBatterPercentages$SLPercentage.x+AwayBatterPercentages$SLPercentage.y)/2,0)
AwayBatterPercentages$prepercentageCH=ifelse(AwayBatterPercentages$CHPercentage.x>0.15 ,(AwayBatterPercentages$CHPercentage.x+AwayBatterPercentages$CHPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFT=ifelse(AwayBatterPercentages$FTPercentage.x>0.15  ,(AwayBatterPercentages$FTPercentage.x+AwayBatterPercentages$FTPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSI=ifelse(AwayBatterPercentages$SIPercentage.x>0.15  ,(AwayBatterPercentages$SIPercentage.x+AwayBatterPercentages$SIPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFS=ifelse(AwayBatterPercentages$FSPercentage.x>0.15 ,(AwayBatterPercentages$FSPercentage.x+AwayBatterPercentages$FSPercentage.y)/2,0)
AwayBatterPercentages$prepercentageKC=ifelse(AwayBatterPercentages$KCPercentage.x>0.15 ,(AwayBatterPercentages$KCPercentage.x+AwayBatterPercentages$KCPercentage.y)/2,0)
AwayBatterPercentages$prepercentageIN=ifelse(AwayBatterPercentages$INPercentage.x>0.15  ,(AwayBatterPercentages$INPercentage.x+AwayBatterPercentages$INPercentage.y)/2,0)
AwayBatterPercentages$prepercentagePO=ifelse(AwayBatterPercentages$POPercentage.x>0.15 ,(AwayBatterPercentages$POPercentage.x+AwayBatterPercentages$POPercentage.y)/2,0)
AwayBatterPercentages$prepercentageUN=ifelse(AwayBatterPercentages$UNPercentage.x>0.15 ,(AwayBatterPercentages$UNPercentage.x+AwayBatterPercentages$UNPercentage.y)/2,0)
AwayBatterPercentages$prepercentageEP=ifelse(AwayBatterPercentages$EPPercentage.x>0.15  ,(AwayBatterPercentages$EPPercentage.x+AwayBatterPercentages$EPPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSC=ifelse(AwayBatterPercentages$SCPercentage.x>0.15 ,(AwayBatterPercentages$SCPercentage.x+AwayBatterPercentages$SCPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFC=ifelse(AwayBatterPercentages$FCPercentage.x>0.15,(AwayBatterPercentages$FCPercentage.x+AwayBatterPercentages$FCPercentage.y)/2,0)
AwayBatterPercentages$SUM=AwayBatterPercentages$prepercentageFF+AwayBatterPercentages$prepercentageCU+AwayBatterPercentages$prepercentageSL+AwayBatterPercentages$prepercentageCH+AwayBatterPercentages$prepercentageFT+AwayBatterPercentages$prepercentageSI+AwayBatterPercentages$prepercentageFS+AwayBatterPercentages$prepercentageKC+AwayBatterPercentages$prepercentageIN+AwayBatterPercentages$prepercentagePO+AwayBatterPercentages$prepercentageUN+AwayBatterPercentages$prepercentageEP+AwayBatterPercentages$prepercentageSC+AwayBatterPercentages$prepercentageFC
AwayBatterPercentages$Multiplier=1/AwayBatterPercentages$SUM
AwayBatterPercentages$percentageFF=AwayBatterPercentages$prepercentageFF*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageCU=AwayBatterPercentages$prepercentageCU*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSL=AwayBatterPercentages$prepercentageSL*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageCH=AwayBatterPercentages$prepercentageCH*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFT=AwayBatterPercentages$prepercentageFT*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSI=AwayBatterPercentages$prepercentageSI*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFS=AwayBatterPercentages$prepercentageFS*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageKC=AwayBatterPercentages$prepercentageKC*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageIN=AwayBatterPercentages$prepercentageIN*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentagePO=AwayBatterPercentages$prepercentagePO*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageUN=AwayBatterPercentages$prepercentageUN*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageEP=AwayBatterPercentages$prepercentageEP*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSC=AwayBatterPercentages$prepercentageSC*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFC=AwayBatterPercentages$prepercentageFC*AwayBatterPercentages$Multiplier


baseball$L1=ifelse(baseball$zone=="1",1,0)
baseball$L2=ifelse(baseball$zone=="2",1,0)
baseball$L3=ifelse(baseball$zone=="3",1,0)
baseball$L4=ifelse(baseball$zone=="4",1,0)
baseball$L5=ifelse(baseball$zone=="5",1,0)
baseball$L6=ifelse(baseball$zone=="6",1,0)
baseball$L7=ifelse(baseball$zone=="7",1,0)
baseball$L8=ifelse(baseball$zone=="8",1,0)
baseball$L9=ifelse(baseball$zone=="9",1,0)
baseball$L11=ifelse(baseball$zone=="11",1,0)
baseball$L12=ifelse(baseball$zone=="12",1,0)
baseball$L13=ifelse(baseball$zone=="13",1,0)
baseball$L14=ifelse(baseball$zone=="14",1,0)


playerpercentagesL1=with(baseball,aggregate(L1,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL1,"Group.2","PitchHand")
setnames(playerpercentagesL1,"Group.3","Balls")
setnames(playerpercentagesL1,"Group.4","Strikes")
setnames(playerpercentagesL1,"x","L1Percentage")

playerpercentagesL2=with(baseball,aggregate(L2,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL2,"Group.2","PitchHand")
setnames(playerpercentagesL2,"Group.3","Balls")
setnames(playerpercentagesL2,"Group.4","Strikes")
setnames(playerpercentagesL2,"x","L2Percentage")

playerpercentagesL3=with(baseball,aggregate(L3,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL3,"Group.2","PitchHand")
setnames(playerpercentagesL3,"Group.3","Balls")
setnames(playerpercentagesL3,"Group.4","Strikes")
setnames(playerpercentagesL3,"x","L3Percentage")

playerpercentagesL4=with(baseball,aggregate(L4,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL4,"Group.2","PitchHand")
setnames(playerpercentagesL4,"Group.3","Balls")
setnames(playerpercentagesL4,"Group.4","Strikes")
setnames(playerpercentagesL4,"x","L4Percentage")

playerpercentagesL5=with(baseball,aggregate(L5,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL5,"Group.2","PitchHand")
setnames(playerpercentagesL5,"Group.3","Balls")
setnames(playerpercentagesL5,"Group.4","Strikes")
setnames(playerpercentagesL5,"x","L5Percentage")

playerpercentagesL6=with(baseball,aggregate(L6,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL6,"Group.2","PitchHand")
setnames(playerpercentagesL6,"Group.3","Balls")
setnames(playerpercentagesL6,"Group.4","Strikes")
setnames(playerpercentagesL6,"x","L6Percentage")

playerpercentagesL7=with(baseball,aggregate(L7,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL7,"Group.2","PitchHand")
setnames(playerpercentagesL7,"Group.3","Balls")
setnames(playerpercentagesL7,"Group.4","Strikes")
setnames(playerpercentagesL7,"x","L7Percentage")

playerpercentagesL8=with(baseball,aggregate(L8,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL8,"Group.2","PitchHand")
setnames(playerpercentagesL8,"Group.3","Balls")
setnames(playerpercentagesL8,"Group.4","Strikes")
setnames(playerpercentagesL8,"x","L8Percentage")

playerpercentagesL9=with(baseball,aggregate(L9,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL9,"Group.2","PitchHand")
setnames(playerpercentagesL9,"Group.3","Balls")
setnames(playerpercentagesL9,"Group.4","Strikes")
setnames(playerpercentagesL9,"x","L9Percentage")

playerpercentagesL11=with(baseball,aggregate(L11,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL11,"Group.2","PitchHand")
setnames(playerpercentagesL11,"Group.3","Balls")
setnames(playerpercentagesL11,"Group.4","Strikes")
setnames(playerpercentagesL11,"x","L11Percentage")

playerpercentagesL12=with(baseball,aggregate(L12,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL12,"Group.2","PitchHand")
setnames(playerpercentagesL12,"Group.3","Balls")
setnames(playerpercentagesL12,"Group.4","Strikes")
setnames(playerpercentagesL12,"x","L12Percentage")

playerpercentagesL13=with(baseball,aggregate(L13,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL13,"Group.2","PitchHand")
setnames(playerpercentagesL13,"Group.3","Balls")
setnames(playerpercentagesL13,"Group.4","Strikes")
setnames(playerpercentagesL13,"x","L13Percentage")

playerpercentagesL14=with(baseball,aggregate(L14,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL14,"Group.2","PitchHand")
setnames(playerpercentagesL14,"Group.3","Balls")
setnames(playerpercentagesL14,"Group.4","Strikes")
setnames(playerpercentagesL14,"x","L14Percentage")


pitcherpercentagesL1=with(baseball,aggregate(L1,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL1,"Group.2","Batterhand")
setnames(pitcherpercentagesL1,"Group.3","Balls")
setnames(pitcherpercentagesL1,"Group.4","Strikes")
setnames(pitcherpercentagesL1,"x","L1Percentage")

pitcherpercentagesL2=with(baseball,aggregate(L2,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL2,"Group.2","Batterhand")
setnames(pitcherpercentagesL2,"Group.3","Balls")
setnames(pitcherpercentagesL2,"Group.4","Strikes")
setnames(pitcherpercentagesL2,"x","L2Percentage")

pitcherpercentagesL3=with(baseball,aggregate(L3,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL3,"Group.2","Batterhand")
setnames(pitcherpercentagesL3,"Group.3","Balls")
setnames(pitcherpercentagesL3,"Group.4","Strikes")
setnames(pitcherpercentagesL3,"x","L3Percentage")

pitcherpercentagesL4=with(baseball,aggregate(L4,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL4,"Group.2","Batterhand")
setnames(pitcherpercentagesL4,"Group.3","Balls")
setnames(pitcherpercentagesL4,"Group.4","Strikes")
setnames(pitcherpercentagesL4,"x","L4Percentage")

pitcherpercentagesL5=with(baseball,aggregate(L5,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL5,"Group.2","Batterhand")
setnames(pitcherpercentagesL5,"Group.3","Balls")
setnames(pitcherpercentagesL5,"Group.4","Strikes")
setnames(pitcherpercentagesL5,"x","L5Percentage")

pitcherpercentagesL6=with(baseball,aggregate(L6,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL6,"Group.2","Batterhand")
setnames(pitcherpercentagesL6,"Group.3","Balls")
setnames(pitcherpercentagesL6,"Group.4","Strikes")
setnames(pitcherpercentagesL6,"x","L6Percentage")

pitcherpercentagesL7=with(baseball,aggregate(L7,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL7,"Group.2","Batterhand")
setnames(pitcherpercentagesL7,"Group.3","Balls")
setnames(pitcherpercentagesL7,"Group.4","Strikes")
setnames(pitcherpercentagesL7,"x","L7Percentage")

pitcherpercentagesL8=with(baseball,aggregate(L8,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL8,"Group.2","Batterhand")
setnames(pitcherpercentagesL8,"Group.3","Balls")
setnames(pitcherpercentagesL8,"Group.4","Strikes")
setnames(pitcherpercentagesL8,"x","L8Percentage")

pitcherpercentagesL9=with(baseball,aggregate(L9,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL9,"Group.2","Batterhand")
setnames(pitcherpercentagesL9,"Group.3","Balls")
setnames(pitcherpercentagesL9,"Group.4","Strikes")
setnames(pitcherpercentagesL9,"x","L9Percentage")

pitcherpercentagesL11=with(baseball,aggregate(L11,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL11,"Group.2","Batterhand")
setnames(pitcherpercentagesL11,"Group.3","Balls")
setnames(pitcherpercentagesL11,"Group.4","Strikes")
setnames(pitcherpercentagesL11,"x","L11Percentage")

pitcherpercentagesL12=with(baseball,aggregate(L12,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL12,"Group.2","Batterhand")
setnames(pitcherpercentagesL12,"Group.3","Balls")
setnames(pitcherpercentagesL12,"Group.4","Strikes")
setnames(pitcherpercentagesL12,"x","L12Percentage")

pitcherpercentagesL13=with(baseball,aggregate(L13,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL13,"Group.2","Batterhand")
setnames(pitcherpercentagesL13,"Group.3","Balls")
setnames(pitcherpercentagesL13,"Group.4","Strikes")
setnames(pitcherpercentagesL13,"x","L13Percentage")

pitcherpercentagesL14=with(baseball,aggregate(L14,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL14,"Group.2","Batterhand")
setnames(pitcherpercentagesL14,"Group.3","Balls")
setnames(pitcherpercentagesL14,"Group.4","Strikes")
setnames(pitcherpercentagesL14,"x","L14Percentage")

playerpercentageszone=merge(playerpercentagesL1,playerpercentagesL2,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL3,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL4,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL5,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL6,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL7,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL8,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL9,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL11,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL12,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL13,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL14,by=c("Group.1","PitchHand",'Balls','Strikes'))


pitcherpercentageszone=merge(pitcherpercentagesL1,pitcherpercentagesL2,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL3,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL4,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL5,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL6,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL7,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL8,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL9,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL11,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL12,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL13,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL14,by=c("Group.1","Batterhand",'Balls','Strikes'))

setnames(playerpercentageszone,"Group.1","Player")
setnames(pitcherpercentageszone,"Group.1","Pitcher")
playerpercentageszone=merge(batterlineup,playerpercentageszone,by="Player")
pitcherpercentageszone=merge(pitcherlineup,pitcherpercentageszone,by="Pitcher")
Homepitcherpercentageszone=subset(pitcherpercentageszone,Team==HomeTeam)
Awaypitcherpercentageszone=subset(pitcherpercentageszone,Team==AwayTeam)
Homeplayerpercentageszone=subset(playerpercentageszone,Team==HomeTeam)
Awayplayerpercentageszone=subset(playerpercentageszone,Team==AwayTeam)
########################################################

HomeBatterPercentageszone=merge(Homepitcherpercentageszone,Awayplayerpercentageszone,by=c("PitchHand","Balls","Strikes"))
AwayBatterPercentageszone=merge(Awaypitcherpercentageszone,Homeplayerpercentageszone,by=c("PitchHand","Balls","Strikes"))
HomeBatterPercentageszone$RealHand=HomeBatterPercentageszone$Hand
HomeBatterPercentageszone$FakeHand=ifelse((HomeBatterPercentageszone$Hand=='S'|HomeBatterPercentageszone$PitchHand=='R'),"L","R")
HomeBatterPercentageszone$Hand=as.character(HomeBatterPercentageszone$Hand)
HomeBatterPercentageszone$RealHand=ifelse(HomeBatterPercentageszone$Hand=='S',HomeBatterPercentageszone$FakeHand,HomeBatterPercentageszone$Hand)
HomeBatterPercentageszone=subset(HomeBatterPercentageszone,HomeBatterPercentageszone$RealHand==HomeBatterPercentageszone$Batterhand)
HomeBatterPercentageszone$prepercentageL1=ifelse(HomeBatterPercentageszone$L1Percentage.x>0.02,(HomeBatterPercentageszone$L1Percentage.x+HomeBatterPercentageszone$L1Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL2=ifelse(HomeBatterPercentageszone$L2Percentage.x>0.02,(HomeBatterPercentageszone$L2Percentage.x+HomeBatterPercentageszone$L2Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL3=ifelse(HomeBatterPercentageszone$L3Percentage.x>0.02,(HomeBatterPercentageszone$L3Percentage.x+HomeBatterPercentageszone$L3Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL4=ifelse(HomeBatterPercentageszone$L4Percentage.x>0.02,(HomeBatterPercentageszone$L4Percentage.x+HomeBatterPercentageszone$L4Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL5=ifelse(HomeBatterPercentageszone$L5Percentage.x>0.02,(HomeBatterPercentageszone$L5Percentage.x+HomeBatterPercentageszone$L5Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL6=ifelse(HomeBatterPercentageszone$L6Percentage.x>0.02,(HomeBatterPercentageszone$L6Percentage.x+HomeBatterPercentageszone$L6Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL7=ifelse(HomeBatterPercentageszone$L7Percentage.x>0.02,(HomeBatterPercentageszone$L7Percentage.x+HomeBatterPercentageszone$L7Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL8=ifelse(HomeBatterPercentageszone$L8Percentage.x>0.02,(HomeBatterPercentageszone$L8Percentage.x+HomeBatterPercentageszone$L8Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL9=ifelse(HomeBatterPercentageszone$L9Percentage.x>0.02,(HomeBatterPercentageszone$L9Percentage.x+HomeBatterPercentageszone$L9Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL11=ifelse(HomeBatterPercentageszone$L11Percentage.x>0.02,(HomeBatterPercentageszone$L11Percentage.x+HomeBatterPercentageszone$L11Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL12=ifelse(HomeBatterPercentageszone$L12Percentage.x>0.02,(HomeBatterPercentageszone$L12Percentage.x+HomeBatterPercentageszone$L12Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL13=ifelse(HomeBatterPercentageszone$L13Percentage.x>0.02,(HomeBatterPercentageszone$L13Percentage.x+HomeBatterPercentageszone$L13Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL14=ifelse(HomeBatterPercentageszone$L14Percentage.x>0.02,(HomeBatterPercentageszone$L14Percentage.x+HomeBatterPercentageszone$L14Percentage.y)/2,0)
HomeBatterPercentageszone$SUM=HomeBatterPercentageszone$prepercentageL1+HomeBatterPercentageszone$prepercentageL2+HomeBatterPercentageszone$prepercentageL3+HomeBatterPercentageszone$prepercentageL4+HomeBatterPercentageszone$prepercentageL5+HomeBatterPercentageszone$prepercentageL6+HomeBatterPercentageszone$prepercentageL7+HomeBatterPercentageszone$prepercentageL8+HomeBatterPercentageszone$prepercentageL9+HomeBatterPercentageszone$prepercentageL11+HomeBatterPercentageszone$prepercentageL12+HomeBatterPercentageszone$prepercentageL13+HomeBatterPercentageszone$prepercentageL14
HomeBatterPercentageszone$Multiplier=1/HomeBatterPercentageszone$SUM
HomeBatterPercentageszone$percentageL1=HomeBatterPercentageszone$prepercentageL1*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL2=HomeBatterPercentageszone$prepercentageL2*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL3=HomeBatterPercentageszone$prepercentageL3*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL4=HomeBatterPercentageszone$prepercentageL4*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL5=HomeBatterPercentageszone$prepercentageL5*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL6=HomeBatterPercentageszone$prepercentageL6*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL7=HomeBatterPercentageszone$prepercentageL7*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL8=HomeBatterPercentageszone$prepercentageL8*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL9=HomeBatterPercentageszone$prepercentageL9*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL11=HomeBatterPercentageszone$prepercentageL11*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL12=HomeBatterPercentageszone$prepercentageL12*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL13=HomeBatterPercentageszone$prepercentageL13*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL14=HomeBatterPercentageszone$prepercentageL14*HomeBatterPercentageszone$Multiplier

AwayBatterPercentageszone$RealHand=AwayBatterPercentageszone$Hand
AwayBatterPercentageszone$FakeHand=ifelse((AwayBatterPercentageszone$Hand=='S'|AwayBatterPercentageszone$PitchHand=='R'),"L","R")
AwayBatterPercentageszone$Hand=as.character(AwayBatterPercentageszone$Hand)
AwayBatterPercentageszone$RealHand=ifelse(AwayBatterPercentageszone$Hand=='S',AwayBatterPercentageszone$FakeHand,AwayBatterPercentageszone$Hand)
AwayBatterPercentageszone=subset(AwayBatterPercentageszone,AwayBatterPercentageszone$RealHand==AwayBatterPercentageszone$Batterhand)
AwayBatterPercentageszone$prepercentageL1=ifelse(AwayBatterPercentageszone$L1Percentage.x>0.02,(AwayBatterPercentageszone$L1Percentage.x+AwayBatterPercentageszone$L1Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL2=ifelse(AwayBatterPercentageszone$L2Percentage.x>0.02,(AwayBatterPercentageszone$L2Percentage.x+AwayBatterPercentageszone$L2Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL3=ifelse(AwayBatterPercentageszone$L3Percentage.x>0.02,(AwayBatterPercentageszone$L3Percentage.x+AwayBatterPercentageszone$L3Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL4=ifelse(AwayBatterPercentageszone$L4Percentage.x>0.02,(AwayBatterPercentageszone$L4Percentage.x+AwayBatterPercentageszone$L4Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL5=ifelse(AwayBatterPercentageszone$L5Percentage.x>0.02,(AwayBatterPercentageszone$L5Percentage.x+AwayBatterPercentageszone$L5Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL6=ifelse(AwayBatterPercentageszone$L6Percentage.x>0.02,(AwayBatterPercentageszone$L6Percentage.x+AwayBatterPercentageszone$L6Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL7=ifelse(AwayBatterPercentageszone$L7Percentage.x>0.02,(AwayBatterPercentageszone$L7Percentage.x+AwayBatterPercentageszone$L7Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL8=ifelse(AwayBatterPercentageszone$L8Percentage.x>0.02,(AwayBatterPercentageszone$L8Percentage.x+AwayBatterPercentageszone$L8Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL9=ifelse(AwayBatterPercentageszone$L9Percentage.x>0.02,(AwayBatterPercentageszone$L9Percentage.x+AwayBatterPercentageszone$L9Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL11=ifelse(AwayBatterPercentageszone$L11Percentage.x>0.02,(AwayBatterPercentageszone$L11Percentage.x+AwayBatterPercentageszone$L11Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL12=ifelse(AwayBatterPercentageszone$L12Percentage.x>0.02,(AwayBatterPercentageszone$L12Percentage.x+AwayBatterPercentageszone$L12Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL13=ifelse(AwayBatterPercentageszone$L13Percentage.x>0.02,(AwayBatterPercentageszone$L13Percentage.x+AwayBatterPercentageszone$L13Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL14=ifelse(AwayBatterPercentageszone$L14Percentage.x>0.02,(AwayBatterPercentageszone$L14Percentage.x+AwayBatterPercentageszone$L14Percentage.y)/2,0)
AwayBatterPercentageszone$SUM=AwayBatterPercentageszone$prepercentageL1+AwayBatterPercentageszone$prepercentageL2+AwayBatterPercentageszone$prepercentageL3+AwayBatterPercentageszone$prepercentageL4+AwayBatterPercentageszone$prepercentageL5+AwayBatterPercentageszone$prepercentageL6+AwayBatterPercentageszone$prepercentageL7+AwayBatterPercentageszone$prepercentageL8+AwayBatterPercentageszone$prepercentageL9+AwayBatterPercentageszone$prepercentageL11+AwayBatterPercentageszone$prepercentageL12+AwayBatterPercentageszone$prepercentageL13+AwayBatterPercentageszone$prepercentageL14
AwayBatterPercentageszone$Multiplier=1/AwayBatterPercentageszone$SUM
AwayBatterPercentageszone$percentageL1=AwayBatterPercentageszone$prepercentageL1*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL2=AwayBatterPercentageszone$prepercentageL2*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL3=AwayBatterPercentageszone$prepercentageL3*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL4=AwayBatterPercentageszone$prepercentageL4*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL5=AwayBatterPercentageszone$prepercentageL5*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL6=AwayBatterPercentageszone$prepercentageL6*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL7=AwayBatterPercentageszone$prepercentageL7*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL8=AwayBatterPercentageszone$prepercentageL8*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL9=AwayBatterPercentageszone$prepercentageL9*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL11=AwayBatterPercentageszone$prepercentageL11*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL12=AwayBatterPercentageszone$prepercentageL12*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL13=AwayBatterPercentageszone$prepercentageL13*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL14=AwayBatterPercentageszone$prepercentageL14*AwayBatterPercentageszone$Multiplier




#############################################################
baseball$Swing=ifelse(baseball$description=="hit_into_play",1,ifelse(baseball$description=="foul_tip",1,ifelse(baseball$description=="hit_into_play_no_out",1,ifelse(baseball$description=="foul",1,ifelse(baseball$description=="hit_into_play_score",1,ifelse(baseball$description=="swinging_strike",1,ifelse(baseball$description=="swinging_strike_blocked",1,0)))))))
baseball$Contact=ifelse(baseball$description=="hit_into_play",1,ifelse(baseball$description=="hit_into_play_no_out",1,ifelse(baseball$description=="foul",1,ifelse(baseball$description=="hit_into_play_score",1,0))))

playerpercentageswingpitch=with(baseball,aggregate(Swing,list(player_name,p_throws,balls,strikes,pitch_type),mean))
setnames(playerpercentageswingpitch,"Group.5",'pitch_type')
setnames(playerpercentageswingpitch,"x",'pitchswingpercentage')
playerpercentageswingpitch=playerpercentageswingpitch[!(playerpercentageswingpitch$pitch_type=='null'),]

playerpercentageswinglocation=with(baseball,aggregate(Swing,list(player_name,p_throws,balls,strikes,zone),mean))
setnames(playerpercentageswinglocation,"Group.5",'zone')
setnames(playerpercentageswinglocation,"x",'locationswingpercentage')
playerpercentageswinglocation=playerpercentageswinglocation[!(playerpercentageswinglocation$location=='null'),]

swingpercentage=merge(playerpercentageswingpitch,playerpercentageswinglocation,by=c('Group.1','Group.2','Group.3','Group.4'))
swingpercentagezero=subset(swingpercentage,locationswingpercentage==0)
swingpercentageone=subset(swingpercentage,locationswingpercentage==1)
swingpercentagenotzero=subset(swingpercentage,locationswingpercentage>0.00001 & locationswingpercentage<1)

averageswing=with(swingpercentagenotzero,aggregate(locationswingpercentage,list(Group.1,Group.2,Group.3,Group.4,pitch_type),mean))
setnames(averageswing,'x','averageswing')
setnames(averageswing,'Group.5','pitch_type')
swingpercentagezero$averageswing=0
swingpercentageone$averageswing=1

averageswing=merge(swingpercentagenotzero,averageswing,by=c('Group.1','Group.2','Group.3','Group.4','pitch_type'))
averageswing$difference=averageswing$locationswingpercentage/averageswing$averageswing
averageswing$realaveswing=averageswing$pitchswingpercentage*averageswing$difference

swingpercentagezero$difference=0
swingpercentagezero$realaveswing=0
swingpercentageone$difference=1
swingpercentageone$realaveswing=1


averageswing=rbind(averageswing,swingpercentagezero,swingpercentageone)
averageswing$realaveswing=ifelse(averageswing$realaveswing>1,1,averageswing$realaveswing)




zone1=as.data.frame(c(1,2,3,4,5,6,7,8,9,11,12,13,14))
balls1=as.data.frame(c(0,1,2,3))
strikes1=as.data.frame(c(0,1,2))
hand1=as.data.frame(c("L","R"))
batterlineup1=batterlineup$Player
pitches=as.data.frame(c("FF","CU","SL","CH",'FT','SI','FS','KC','IN','PO','UN','EP','SC','FC'))
zone2=as.data.frame(c(1,2,3,4,5,6,7,8,9))
zone3=as.data.frame(c(11,12,13,14))

batterdifferences=merge(batterlineup1,zone1)
batterdifferences=merge(batterdifferences,balls1)
batterdifferences=merge(batterdifferences,strikes1)
batterdifferences=merge(batterdifferences,hand1)


names(batterdifferences)[1]="Player"
names(batterdifferences)[2]="zone"
names(batterdifferences)[3]="balls"
names(batterdifferences)[4]="strikes"
names(batterdifferences)[5]="p_throw"



uu=baseball
uu=subset(uu,zone!="null")
uu$zone=as.numeric(uu$zone)
uu=subset(uu,zone < 10)
uu=with(uu,aggregate(Swing,list(player_name,p_throws,balls,strikes),mean))
names(uu)[1]="Player"
names(uu)[2]="p_throw"
names(uu)[3]="balls"
names(uu)[4]="strikes"
names(uu)[5]="averageswingpercent"

batterdifferences1=subset(batterdifferences,zone<10)

batterdifferences1=merge(batterdifferences1,uu,by=c("Player","p_throw","balls","strikes"),all=TRUE)
batterdifferences1$averageswingpercent[is.na(batterdifferences1$averageswingpercent)]<-0
batterdifferences1=merge(batterdifferences1,pitches)
names(batterdifferences1)[7]="pitch_type"


yy=baseball
yy=subset(yy,zone!="null")
yy$zone=as.numeric(yy$zone)
yy=subset(yy,zone > 10)
yy=with(yy,aggregate(Swing,list(player_name,p_throws,balls,strikes),mean))
names(yy)[1]="Player"
names(yy)[2]="p_throw"
names(yy)[3]="balls"
names(yy)[4]="strikes"
names(yy)[5]="averageswingpercent"

batterdifferences2=subset(batterdifferences,zone>10)

batterdifferences2=merge(batterdifferences2,uu,by=c("Player","p_throw","balls","strikes"),all=TRUE)
batterdifferences2$averageswingpercent[is.na(batterdifferences2$averageswingpercent)]<-0
batterdifferences2=merge(batterdifferences2,pitches)
names(batterdifferences2)[7]="pitch_type"
batterdifferences=rbind(batterdifferences1,batterdifferences2)




names(averageswing)[1]="Player"
names(averageswing)[2]="p_throw"
names(averageswing)[3]="balls"
names(averageswing)[4]="strikes"






averageswing=averageswing[,c("Player","p_throw","balls","strikes","pitch_type","zone","realaveswing")]
batterdifferences=merge(batterdifferences,averageswing,by=c("Player","p_throw","balls","strikes","pitch_type","zone"),all =TRUE)
batterdifferences=subset(batterdifferences,averageswingpercent>-0.1)

batterdifferences$realaveswing[is.na(batterdifferences$realaveswing)]<-55
batterdifferences$realaveswing=ifelse(batterdifferences$realaveswing==55,batterdifferences$averageswingpercent,batterdifferences$realaveswing)


baseball=subset(baseball,effective_speed!="null")
baseball$effective_speed=as.numeric(baseball$effective_speed)

avespin=with(baseball,aggregate(spin_rate,list(pitch_type,p_throws),mean))
avebreak_angle=with(baseball,aggregate(break_angle,list(pitch_type,p_throws),mean))
avebreak_length=with(baseball,aggregate(break_length,list(pitch_type,p_throws),mean))
aveeffective_speed=with(baseball,aggregate(effective_speed,list(pitch_type,p_throws),mean))

baseball=subset(baseball,effective_speed>0.1)


avepitchereffective_speed=with(baseball,aggregate(effective_speed,list(pitch_type,stand,PitcherName),mean))
setnames(avepitchereffective_speed,'x','avepitchereffective_speed')

avepitcherspin=with(baseball,aggregate(spin_rate,list(pitch_type,stand,PitcherName),mean))
avepitcherspin=subset(avepitcherspin,x>0.1)
setnames(avepitcherspin,'x','avepitcherspin')
avepitcherbreak_angle=with(baseball,aggregate(break_angle,list(pitch_type,stand,PitcherName),mean))
avepitcherbreak_angle=subset(avepitcherbreak_angle,x !=0)
setnames(avepitcherbreak_angle,'x','aveavepitcherbreak_angle')
avepitcherbreak_length=with(baseball,aggregate(break_length,list(pitch_type,stand,PitcherName),mean))
avepitcherbreak_length=subset(avepitcherbreak_length,x>0.1)
setnames(avepitcherbreak_length,'x','avepitcherbreak_length')
pitchernumbers=merge(avepitcherspin,avepitcherbreak_angle,by=c("Group.1","Group.2","Group.3"))
pitchernumbers=merge(pitchernumbers,avepitcherbreak_length,by=c("Group.1","Group.2","Group.3"))
pitchernumbers=merge(pitchernumbers,avepitchereffective_speed,by=c("Group.1","Group.2","Group.3"))
setnames(pitchernumbers,"Group.3","Pitcher")
pitchernumbers=merge(pitchernumbers,pitcherlineup,by="Pitcher")
setnames(pitchernumbers,"Group.2","Stand")
setnames(pitchernumbers,"Group.1","pitch_type")


batterhand=batterlineup[,c("Player","Hand")]
PitcherBatter=merge(pitchernumbers,batterhand)
PitcherBatter$RealHand=ifelse(PitcherBatter$Hand=="R","R", ifelse(PitcherBatter$Hand=="L","L",ifelse(PitcherBatter$Hand=="S"& PitcherBatter$PitchHand=="R","L",ifelse(PitcherBatter$Hand=="S"& PitcherBatter$PitchHand=="L","R","A"))))
PitcherBatter=subset(PitcherBatter,Stand==RealHand)


baseball$Pitch_typeHand=paste(baseball$player_name,baseball$p_throws,baseball$pitch_type,sep="")
PitcherBatter$Pitch_typeHand=paste(PitcherBatter$Player,PitcherBatter$PitchHand,PitcherBatter$pitch_type,sep="")

setnames(PitcherBatter,"avepitcherspin","spin_rate")
setnames(PitcherBatter,"aveavepitcherbreak_angle","break_angle")
setnames(PitcherBatter,"avepitcherbreak_length","break_length")
setnames(PitcherBatter,"avepitchereffective_speed","effective_speed")



##############

pitcherpercentagesFF2=with(baseball,aggregate(FF,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFF2,"Group.1","Pitcher")
setnames(pitcherpercentagesFF2,"Group.2","RealHand")
setnames(pitcherpercentagesFF2,"x","Percentage")
pitcherpercentagesFF2$pitch_type='FF'

pitcherpercentagesSL2=with(baseball,aggregate(SL,list(PitcherName,stand),mean))				
setnames(pitcherpercentagesSL2,"Group.2","RealHand")				
setnames(pitcherpercentagesSL2,"x","Percentage")				
setnames(pitcherpercentagesSL2,"Group.1","Pitcher")
pitcherpercentagesSL2$pitch_type='SL'

pitcherpercentagesCU2=with(baseball,aggregate(CU,list(PitcherName,stand),mean))
setnames(pitcherpercentagesCU2,"Group.2","RealHand")
setnames(pitcherpercentagesCU2,"x","Percentage")
setnames(pitcherpercentagesCU2,"Group.1","Pitcher")
pitcherpercentagesCU2$pitch_type='CU'

pitcherpercentagesCH2=with(baseball,aggregate(CH,list(PitcherName,stand),mean))
setnames(pitcherpercentagesCH2,"Group.2","RealHand")
setnames(pitcherpercentagesCH2,"x","Percentage")
setnames(pitcherpercentagesCH2,"Group.1","Pitcher")
pitcherpercentagesCH2$pitch_type='CH'

pitcherpercentagesFT2=with(baseball,aggregate(FT,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFT2,"Group.2","RealHand")
setnames(pitcherpercentagesFT2,"x","Percentage")
setnames(pitcherpercentagesFT2,"Group.1","Pitcher")
pitcherpercentagesFT2$pitch_type='FT'

pitcherpercentagesSI2=with(baseball,aggregate(SI,list(PitcherName,stand),mean))
setnames(pitcherpercentagesSI2,"Group.2","RealHand")
setnames(pitcherpercentagesSI2,"x","Percentage")
setnames(pitcherpercentagesSI2,"Group.1","Pitcher")
pitcherpercentagesSI2$pitch_type='SI'

pitcherpercentagesFS2=with(baseball,aggregate(FS,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFS2,"Group.2","RealHand")
setnames(pitcherpercentagesFS2,"x","Percentage")
setnames(pitcherpercentagesFS2,"Group.1","Pitcher")
pitcherpercentagesFS2$pitch_type='FS'

pitcherpercentagesKC2=with(baseball,aggregate(KC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesKC2,"Group.2","RealHand")
setnames(pitcherpercentagesKC2,"x","Percentage")
setnames(pitcherpercentagesKC2,"Group.1","Pitcher")
pitcherpercentagesKC2$pitch_type='KC'

pitcherpercentagesIN2=with(baseball,aggregate(IN,list(PitcherName,stand),mean))
setnames(pitcherpercentagesIN2,"Group.2","RealHand")
setnames(pitcherpercentagesIN2,"x","Percentage")
setnames(pitcherpercentagesIN2,"Group.1","Pitcher")
pitcherpercentagesIN2$pitch_type='IN'

pitcherpercentagesPO2=with(baseball,aggregate(PO,list(PitcherName,stand),mean))
setnames(pitcherpercentagesPO2,"Group.2","RealHand")
setnames(pitcherpercentagesPO2,"x","Percentage")
setnames(pitcherpercentagesPO2,"Group.1","Pitcher")
pitcherpercentagesPO2$pitch_type='PO'

pitcherpercentagesUN2=with(baseball,aggregate(UN,list(PitcherName,stand),mean))
setnames(pitcherpercentagesUN2,"Group.2","RealHand")
setnames(pitcherpercentagesUN2,"x","Percentage")
setnames(pitcherpercentagesUN2,"Group.1","Pitcher")
pitcherpercentagesUN2$pitch_type='UN'

pitcherpercentagesEP2=with(baseball,aggregate(EP,list(PitcherName,stand),mean))
setnames(pitcherpercentagesEP2,"Group.2","RealHand")
setnames(pitcherpercentagesEP2,"x","Percentage")
setnames(pitcherpercentagesEP2,"Group.1","Pitcher")
pitcherpercentagesEP2$pitch_type='EP'

pitcherpercentagesSC2=with(baseball,aggregate(SC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesSC2,"Group.2","RealHand")
setnames(pitcherpercentagesSC2,"x","Percentage")
setnames(pitcherpercentagesSC2,"Group.1","Pitcher")
pitcherpercentagesSC2$pitch_type='SC'

pitcherpercentagesFC2=with(baseball,aggregate(FC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFC2,"Group.2","RealHand")
setnames(pitcherpercentagesFC2,"x","Percentage")
setnames(pitcherpercentagesFC2,"Group.1","Pitcher")
pitcherpercentagesFC2$pitch_type='FC'



PitcherBatterFF=merge(PitcherBatter,pitcherpercentagesFF2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSL=merge(PitcherBatter,pitcherpercentagesSL2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterCU=merge(PitcherBatter,pitcherpercentagesCU2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterCH=merge(PitcherBatter,pitcherpercentagesCH2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFT=merge(PitcherBatter,pitcherpercentagesFT2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSI=merge(PitcherBatter,pitcherpercentagesSI2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFS=merge(PitcherBatter,pitcherpercentagesFS2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterKC=merge(PitcherBatter,pitcherpercentagesKC2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterIN=merge(PitcherBatter,pitcherpercentagesIN2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterPO=merge(PitcherBatter,pitcherpercentagesPO2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterUN=merge(PitcherBatter,pitcherpercentagesUN2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterEP=merge(PitcherBatter,pitcherpercentagesEP2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSC=merge(PitcherBatter,pitcherpercentagesSC2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFC=merge(PitcherBatter,pitcherpercentagesFC2,by=c("Pitcher","pitch_type","RealHand"))


RealPitcherBatter=rbind(PitcherBatterFF,PitcherBatterSL)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterCU)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterCH)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFT)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterSI)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFS)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterKC)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterIN)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterPO)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterUN)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterEP)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterSC)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFC)


playerpercentagesFF1=with(baseball,aggregate(FF,list(player_name,p_throws),mean))
playerpercentagesFF1$pitch_type='FF'
playerpercentagesSL1=with(baseball,aggregate(SL,list(player_name,p_throws),mean))
playerpercentagesSL1$pitch_type='SL'
playerpercentagesCU1=with(baseball,aggregate(CU,list(player_name,p_throws),mean))
playerpercentagesCU1$pitch_type='CU'
playerpercentagesCH1=with(baseball,aggregate(CH,list(player_name,p_throws),mean))
playerpercentagesCH1$pitch_type='CH'
playerpercentagesFT1=with(baseball,aggregate(FT,list(player_name,p_throws),mean))
playerpercentagesFT1$pitch_type='FT'
playerpercentagesSI1=with(baseball,aggregate(SI,list(player_name,p_throws),mean))
playerpercentagesSI1$pitch_type='SI'
playerpercentagesFS1=with(baseball,aggregate(FS,list(player_name,p_throws),mean))
playerpercentagesFS1$pitch_type='FS'
playerpercentagesKC1=with(baseball,aggregate(KC,list(player_name,p_throws),mean))
playerpercentagesKC1$pitch_type='KC'
playerpercentagesIN1=with(baseball,aggregate(IN,list(player_name,p_throws),mean))
playerpercentagesIN1$pitch_type='IN'
playerpercentagesPO1=with(baseball,aggregate(PO,list(player_name,p_throws),mean))
playerpercentagesPO1$pitch_type='PO'
playerpercentagesUN1=with(baseball,aggregate(UN,list(player_name,p_throws),mean))
playerpercentagesUN1$pitch_type='UN'
playerpercentagesEP1=with(baseball,aggregate(EP,list(player_name,p_throws),mean))
playerpercentagesEP1$pitch_type='EP'
playerpercentagesSC1=with(baseball,aggregate(SC,list(player_name,p_throws),mean))
playerpercentagesSC1$pitch_type='SC'
playerpercentagesFC1=with(baseball,aggregate(FC,list(player_name,p_throws),mean))
playerpercentagesFC1$pitch_type='FC'

playerpercentages1=rbind(playerpercentagesFF1,playerpercentagesSL1,playerpercentagesCU1,playerpercentagesCH1,playerpercentagesFT1,playerpercentagesSI1,playerpercentagesFS1,playerpercentagesKC1,playerpercentagesIN1,playerpercentagesPO1,playerpercentagesUN1,playerpercentagesEP1,playerpercentagesSC1,playerpercentagesFC1)
setnames(playerpercentages1,"Group.1","Player")
setnames(playerpercentages1,"Group.2","PitchHand")
setnames(playerpercentages1,"x","HitterPercentage")

RealPitcherBatter=merge(RealPitcherBatter,playerpercentages1,by=c("Player","PitchHand","pitch_type"))
RealPitcherBatter=subset(RealPitcherBatter,HitterPercentage>.08)


is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))

HomeBatterPercentages[is.nan(HomeBatterPercentages)]=0
AwayBatterPercentages[is.nan(AwayBatterPercentages)]=0



#########################works here#######################################

RealPitcherBatterContact=RealPitcherBatter
RealPitcherBatterContactB=RealPitcherBatter
PitcherBatter2=RealPitcherBatter$Pitch_typeHand

kk=subset(baseball,Swing=='1')



pitcherlineup1=subset(RealPitcherBatter,PitcherNumber==1)
extra1PitcherBatter2=pitcherlineup1[,c('Pitch_typeHand',"PitcherNumber")]
DataframePitcherBatter2=as.data.frame(PitcherBatter2)
setnames(DataframePitcherBatter2,"PitcherBatter2","Pitch_typeHand")
extra1PitcherBatter2aa=merge(extra1PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher1=extra1PitcherBatter2aa$Pitch_typeHand
Pitcher1=unique(Pitcher1)
ActualPitcher=unique(pitcherlineup1$Pitcher)
Pitcher1A=as.data.frame(Pitcher1)
Pitcher1B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher1B,"Var1","Pitcher1")
Pitcher1A=merge(Pitcher1A,Pitcher1B,by=c("Pitcher1"))
Pitcher1A=subset(Pitcher1A,Freq > 40)
Pitcher1=as.character(Pitcher1A$Pitcher1)

batterswing1=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing1 <- data.frame(lapply(batterswing1, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher1){ try({batterswing1=rbind(batterswing1,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing1$Pitcher=ActualPitcher



pitcherlineup2=subset(RealPitcherBatter,PitcherNumber==2)
extra2PitcherBatter2=pitcherlineup2[,c('Pitch_typeHand',"PitcherNumber")]
extra2PitcherBatter2aa=merge(extra2PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher2=extra2PitcherBatter2aa$Pitch_typeHand
Pitcher2=unique(Pitcher2)
ActualPitcher=unique(pitcherlineup2$Pitcher)
Pitcher2A=as.data.frame(Pitcher2)
Pitcher2B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher2B,"Var1","Pitcher2")
Pitcher2A=merge(Pitcher2A,Pitcher2B,by=c("Pitcher2"))
Pitcher2A=subset(Pitcher2A,Freq > 40)
Pitcher2=as.character(Pitcher2A$Pitcher2)

batterswing2=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing2 <- data.frame(lapply(batterswing2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({batterswing2=rbind(batterswing2,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup2,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing2$Pitcher=ActualPitcher


pitcherlineup3=subset(RealPitcherBatter,PitcherNumber==3)
extra3PitcherBatter2=pitcherlineup3[,c('Pitch_typeHand',"PitcherNumber")]
extra3PitcherBatter2aa=merge(extra3PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher3=extra3PitcherBatter2aa$Pitch_typeHand
Pitcher3=unique(Pitcher3)
ActualPitcher=unique(pitcherlineup3$Pitcher)

Pitcher3A=as.data.frame(Pitcher3)
Pitcher3B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher3B,"Var1","Pitcher3")
Pitcher3A=merge(Pitcher3A,Pitcher3B,by=c("Pitcher3"))
Pitcher3A=subset(Pitcher3A,Freq > 40)
Pitcher3=as.character(Pitcher3A$Pitcher3)

batterswing3=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing3 <- data.frame(lapply(batterswing3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({batterswing3=rbind(batterswing3,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing3$Pitcher=ActualPitcher


pitcherlineup4=subset(RealPitcherBatter,PitcherNumber==4)
extra4PitcherBatter2=pitcherlineup4[,c('Pitch_typeHand',"PitcherNumber")]
extra4PitcherBatter2aa=merge(extra4PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher4=extra4PitcherBatter2aa$Pitch_typeHand
Pitcher4=unique(Pitcher4)
ActualPitcher=unique(pitcherlineup4$Pitcher)

Pitcher4A=as.data.frame(Pitcher4)
Pitcher4B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher4B,"Var1","Pitcher4")
Pitcher4A=merge(Pitcher4A,Pitcher4B,by=c("Pitcher4"))
Pitcher4A=subset(Pitcher4A,Freq > 40)
Pitcher4=as.character(Pitcher4A$Pitcher4)

batterswing4=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing4 <- data.frame(lapply(batterswing4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({batterswing4=rbind(batterswing4,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing4$Pitcher=ActualPitcher

pitcherlineup5=subset(RealPitcherBatter,PitcherNumber==5)
extra5PitcherBatter2=pitcherlineup5[,c('Pitch_typeHand',"PitcherNumber")]
extra5PitcherBatter2aa=merge(extra5PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher5=extra5PitcherBatter2aa$Pitch_typeHand
Pitcher5=unique(Pitcher5)
ActualPitcher=unique(pitcherlineup5$Pitcher)

Pitcher5A=as.data.frame(Pitcher5)
Pitcher5B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher5B,"Var1","Pitcher5")
Pitcher5A=merge(Pitcher5A,Pitcher5B,by=c("Pitcher5"))
Pitcher5A=subset(Pitcher5A,Freq > 40)
Pitcher5=as.character(Pitcher5A$Pitcher5)

batterswing5=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing5 <- data.frame(lapply(batterswing5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({batterswing5=rbind(batterswing5,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing5$Pitcher=ActualPitcher

pitcherlineup6=subset(RealPitcherBatter,PitcherNumber==6)
extra6PitcherBatter2=pitcherlineup6[,c('Pitch_typeHand',"PitcherNumber")]
extra6PitcherBatter2aa=merge(extra6PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher6=extra6PitcherBatter2aa$Pitch_typeHand
Pitcher6=unique(Pitcher6)
ActualPitcher=unique(pitcherlineup6$Pitcher)

Pitcher6A=as.data.frame(Pitcher6)
Pitcher6B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher6B,"Var1","Pitcher6")
Pitcher6A=merge(Pitcher6A,Pitcher6B,by=c("Pitcher6"))
Pitcher6A=subset(Pitcher6A,Freq > 40)
Pitcher6=as.character(Pitcher6A$Pitcher6)

batterswing6=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing6 <- data.frame(lapply(batterswing6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({batterswing6=rbind(batterswing6,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing6$Pitcher=ActualPitcher


pitcherlineup7=subset(RealPitcherBatter,PitcherNumber==7)
extra7PitcherBatter2=pitcherlineup7[,c('Pitch_typeHand',"PitcherNumber")]
extra7PitcherBatter2aa=merge(extra7PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher7=extra7PitcherBatter2aa$Pitch_typeHand
Pitcher7=unique(Pitcher7)
ActualPitcher=unique(pitcherlineup7$Pitcher)

Pitcher7A=as.data.frame(Pitcher7)
Pitcher7B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher7B,"Var1","Pitcher7")
Pitcher7A=merge(Pitcher7A,Pitcher7B,by=c("Pitcher7"))
Pitcher7A=subset(Pitcher7A,Freq > 40)
Pitcher7=as.character(Pitcher7A$Pitcher7)

batterswing7=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing7 <- data.frame(lapply(batterswing7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({batterswing7=rbind(batterswing7,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing7$Pitcher=ActualPitcher


batterswing=rbind(batterswing1,batterswing2,batterswing3,batterswing4,batterswing5,batterswing6,batterswing7)









batterswing$Fit=ifelse(batterswing$Fit>.92,.8,batterswing$Fit)
batterswing$Fit=ifelse(batterswing$Fit<.5,.5,batterswing$Fit)



ZoneContact=with(kk,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact,"x","averagezone")

kk1=subset(baseball,Swing=='1'& L1=="1")
ZoneContact1=with(kk1,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact1,"x","L1")

kk2=subset(baseball,Swing=='1'& L2=="1")
ZoneContact2=with(kk2,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact2,"x","L2")

kk3=subset(baseball,Swing=='1'& L3=="1")
ZoneContact3=with(kk3,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact3,"x","L3")
kk4=subset(baseball,Swing=='1'& L4=="1")
ZoneContact4=with(kk4,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact4,"x","L4")
kk5=subset(baseball,Swing=='1'& L5=="1")

ZoneContact5=with(kk5,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact5,"x","L5")
kk6=subset(baseball,Swing=='1'& L6=="1")
ZoneContact6=with(kk6,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact6,"x","L6")
kk7=subset(baseball,Swing=='1'& L7=="1")


ZoneContact7=with(kk7,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact7,"x","L7")
kk8=subset(baseball,Swing=='1'& L8=="1")

ZoneContact8=with(kk8,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact8,"x","L8")
kk9=subset(baseball,Swing=='1'& L9=="1")

ZoneContact9=with(kk9,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact9,"x","L9")
kk11=subset(baseball,Swing=='1'& L11=="1")

ZoneContact11=with(kk11,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact11,"x","L11")
kk12=subset(baseball,Swing=='1'& L12=="1")

ZoneContact12=with(kk12,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact12,"x","L12")
kk13=subset(baseball,Swing=='1'& L13=="1")

ZoneContact13=with(kk13,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact13,"x","L13")
kk14=subset(baseball,Swing=='1'& L1=="1")

ZoneContact14=with(kk14,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact14,"x","L14")


ZoneContact=merge(ZoneContact,ZoneContact1,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact2,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact3,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact4,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact5,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact6,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact7,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact8,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact9,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact11,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact12,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact13,by=c('Group.1',"Group.2"),all=T)
ZoneContact=merge(ZoneContact,ZoneContact14,by=c('Group.1',"Group.2"),all=T)

ZoneContact[is.na(ZoneContact)]<-0
ZoneContact$L1=ifelse(ZoneContact$L1==0,0.95*ZoneContact$averagezone,ZoneContact$L1)
ZoneContact$L2=ifelse(ZoneContact$L2==0,0.95*ZoneContact$averagezone,ZoneContact$L2)
ZoneContact$L3=ifelse(ZoneContact$L3==0,0.95*ZoneContact$averagezone,ZoneContact$L3)
ZoneContact$L4=ifelse(ZoneContact$L4==0,0.95*ZoneContact$averagezone,ZoneContact$L4)
ZoneContact$L5=ifelse(ZoneContact$L5==0,0.95*ZoneContact$averagezone,ZoneContact$L5)
ZoneContact$L6=ifelse(ZoneContact$L6==0,0.95*ZoneContact$averagezone,ZoneContact$L6)
ZoneContact$L7=ifelse(ZoneContact$L7==0,0.95*ZoneContact$averagezone,ZoneContact$L7)
ZoneContact$L8=ifelse(ZoneContact$L8==0,0.95*ZoneContact$averagezone,ZoneContact$L8)
ZoneContact$L9=ifelse(ZoneContact$L9==0,0.95*ZoneContact$averagezone,ZoneContact$L9)
ZoneContact$L11=ifelse(ZoneContact$L11==0,0.95*ZoneContact$averagezone,ZoneContact$L11)
ZoneContact$L12=ifelse(ZoneContact$L12==0,0.95*ZoneContact$averagezone,ZoneContact$L12)
ZoneContact$L13=ifelse(ZoneContact$L13==0,0.95*ZoneContact$averagezone,ZoneContact$L13)
ZoneContact$L14=ifelse(ZoneContact$L14==0,0.95*ZoneContact$averagezone,ZoneContact$L14)
setnames(ZoneContact,'Group.1','Player')
setnames(ZoneContact,'Group.2','PitchHand')


#############


CountContact=with(kk,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact,"x","averagecountcontact")

count00=subset(baseball,Swing=='1'& balls=="0" & strikes =="0")
CountContact00=with(count00,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact00,"x","C00")


count10=subset(baseball,Swing=='1'& balls=="1" & strikes =="0")
CountContact10=with(count10,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact10,"x","C10")

count11=subset(baseball,Swing=='1'& balls=="1" & strikes =="1")
CountContact11=with(count11,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact11,"x","C11")

count12=subset(baseball,Swing=='1'& balls=="1" & strikes =="2")
CountContact12=with(count12,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact12,"x","C12")

count20=subset(baseball,Swing=='1'& balls=="2" & strikes =="0")
CountContact20=with(count20,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact20,"x","C20")

count21=subset(baseball,Swing=='1'& balls=="2" & strikes =="1")
CountContact21=with(count21,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact21,"x","C21")

count22=subset(baseball,Swing=='1'& balls=="2" & strikes =="2")
CountContact22=with(count22,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact22,"x","C22")

count30=subset(baseball,Swing=='1'& balls=="3" & strikes =="0")
CountContact30=with(count30,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact30,"x","C30")

count31=subset(baseball,Swing=='1'& balls=="3" & strikes =="1")
CountContact31=with(count31,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact31,"x","C31")

count32=subset(baseball,Swing=='1'& balls=="3" & strikes =="2")
CountContact32=with(count32,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact32,"x","C32")

count01=subset(baseball,Swing=='1'& balls=="0" & strikes =="1")
CountContact01=with(count01,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact01,"x","C01")

count02=subset(baseball,Swing=='1'& balls=="0" & strikes =="2")
CountContact02=with(count02,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact02,"x","C02")



CountContact=merge(CountContact,CountContact00,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact01,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact02,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact10,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact11,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact12,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact20,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact21,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact22,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact30,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact31,by=c('Group.1',"Group.2"),all=T)
CountContact=merge(CountContact,CountContact32,by=c('Group.1',"Group.2"),all=T)

CountContact[is.na(CountContact)]<-0
CountContact$C00=ifelse(CountContact$C00==0,1*CountContact$averagecountcontact,CountContact$C00)
CountContact$C01=ifelse(CountContact$C01==0,.95*CountContact$averagecountcontact,CountContact$C01)
CountContact$C02=ifelse(CountContact$C02==0,0.90*CountContact$averagecountcontact,CountContact$C02)
CountContact$C10=ifelse(CountContact$C10==0,1*CountContact$averagecountcontact,CountContact$C10)
CountContact$C11=ifelse(CountContact$C11==0,1*CountContact$averagecountcontact,CountContact$C11)
CountContact$C12=ifelse(CountContact$C12==0,.95*CountContact$averagecountcontact,CountContact$C12)
CountContact$C20=ifelse(CountContact$C20==0,1.05*CountContact$averagecountcontact,CountContact$C20)
CountContact$C21=ifelse(CountContact$C21==0,1.02*CountContact$averagecountcontact,CountContact$C21)
CountContact$C22=ifelse(CountContact$C22==0,1*CountContact$averagecountcontact,CountContact$C22)
CountContact$C30=ifelse(CountContact$C30==0,1*CountContact$averagecountcontact,CountContact$C30)
CountContact$C31=ifelse(CountContact$C31==0,1.10*CountContact$averagecountcontact,CountContact$C31)
CountContact$C32=ifelse(CountContact$C32==0,1.10*CountContact$averagecountcontact,CountContact$C32)

setnames(CountContact,'Group.1','Player')
setnames(CountContact,'Group.2','PitchHand')

Contact=merge(ZoneContact,CountContact,by=c("Player","PitchHand"))

RealPitcherBatter=merge(RealPitcherBatter,batterswing,by=c("Pitch_typeHand","Pitcher"))



balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatter=merge(RealPitcherBatter,balls)
setnames(RealPitcherBatter,"y","balls")
RealPitcherBatter=merge(RealPitcherBatter,strikes)
setnames(RealPitcherBatter,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatter=merge(RealPitcherBatter,zone)
setnames(RealPitcherBatter,"y","zone")
RealPitcherBatter=merge(RealPitcherBatter,Contact,by=c("Player","PitchHand"))


RealPitcherBatter$RealL1=RealPitcherBatter$L1/RealPitcherBatter$averagezone
RealPitcherBatter$RealL2=RealPitcherBatter$L2/RealPitcherBatter$averagezone
RealPitcherBatter$RealL3=RealPitcherBatter$L3/RealPitcherBatter$averagezone
RealPitcherBatter$RealL4=RealPitcherBatter$L4/RealPitcherBatter$averagezone
RealPitcherBatter$RealL5=RealPitcherBatter$L5/RealPitcherBatter$averagezone
RealPitcherBatter$RealL6=RealPitcherBatter$L6/RealPitcherBatter$averagezone
RealPitcherBatter$RealL7=RealPitcherBatter$L7/RealPitcherBatter$averagezone
RealPitcherBatter$RealL8=RealPitcherBatter$L8/RealPitcherBatter$averagezone
RealPitcherBatter$RealL9=RealPitcherBatter$L9/RealPitcherBatter$averagezone
RealPitcherBatter$RealL11=RealPitcherBatter$L11/RealPitcherBatter$averagezone
RealPitcherBatter$RealL12=RealPitcherBatter$L12/RealPitcherBatter$averagezone
RealPitcherBatter$RealL13=RealPitcherBatter$L13/RealPitcherBatter$averagezone
RealPitcherBatter$RealL14=RealPitcherBatter$L14/RealPitcherBatter$averagezone

RealPitcherBatter$RealC00=RealPitcherBatter$C00/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC01=RealPitcherBatter$C01/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC02=RealPitcherBatter$C02/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC10=RealPitcherBatter$C10/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC11=RealPitcherBatter$C11/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC12=RealPitcherBatter$C12/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC20=RealPitcherBatter$C20/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC21=RealPitcherBatter$C21/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC22=RealPitcherBatter$C22/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC30=RealPitcherBatter$C30/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC31=RealPitcherBatter$C31/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC32=RealPitcherBatter$C32/RealPitcherBatter$averagecountcontact

RealPitcherBatter$Fit=as.numeric(RealPitcherBatter$Fit)
RealPitcherBatter$ZoneFit=ifelse(RealPitcherBatter$zone==1,RealPitcherBatter$Fit*RealPitcherBatter$RealL1,
                                 ifelse(RealPitcherBatter$zone==2,RealPitcherBatter$Fit*RealPitcherBatter$RealL2,
                                        ifelse(RealPitcherBatter$zone==3,RealPitcherBatter$Fit*RealPitcherBatter$RealL3,
                                               ifelse(RealPitcherBatter$zone==4,RealPitcherBatter$Fit*RealPitcherBatter$RealL4,
                                                      ifelse(RealPitcherBatter$zone==5,RealPitcherBatter$Fit*RealPitcherBatter$RealL5,
                                                             ifelse(RealPitcherBatter$zone==6,RealPitcherBatter$Fit*RealPitcherBatter$RealL6,
                                                                    ifelse(RealPitcherBatter$zone==7,RealPitcherBatter$Fit*RealPitcherBatter$RealL7,
                                                                           ifelse(RealPitcherBatter$zone==8,RealPitcherBatter$Fit*RealPitcherBatter$RealL8,
                                                                                  ifelse(RealPitcherBatter$zone==9,RealPitcherBatter$Fit*RealPitcherBatter$RealL9,
                                                                                         ifelse(RealPitcherBatter$zone==11,RealPitcherBatter$Fit*RealPitcherBatter$RealL11,
                                                                                                ifelse(RealPitcherBatter$zone==12,RealPitcherBatter$Fit*RealPitcherBatter$RealL12,
                                                                                                       ifelse(RealPitcherBatter$zone==13,RealPitcherBatter$Fit*RealPitcherBatter$RealL13,
                                                                                                              ifelse(RealPitcherBatter$zone==14,RealPitcherBatter$Fit*RealPitcherBatter$RealL14,.7)))))))))))))


RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC00,
                                  ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC01,
                                         ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC02,
                                                ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC10,
                                                       ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC11,
                                                              ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC12,
                                                                     ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC20,
                                                                            ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC21,
                                                                                   ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC22,
                                                                                          ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC30,
                                                                                                 ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC31,
                                                                                                        ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC32,.9))))))))))))





RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$CountFit>0.97,.97,RealPitcherBatter$CountFit)
RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$CountFit<0.35,.35,RealPitcherBatter$CountFit)


##########################################################################                                 




############################################################################
jj=subset(baseball,Contact=='1')

jj=subset(jj,launch_angle != "null")
jj=subset(jj,launch_angle!="NA")
jj=subset(jj,description!="foul")
jj$launch_angle=as.numeric(jj$launch_angle)
jj$launch_speed=as.numeric(jj$launch_speed)




ActualPitcher=unique(pitcherlineup1$Pitcher)
Pitcher1A1=as.data.frame(Pitcher1)
Pitcher1B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher1B1,"Var1","Pitcher1")
Pitcher1A1=merge(Pitcher1A1,Pitcher1B1,by=c("Pitcher1"))
Pitcher1A1=subset(Pitcher1A1,Freq > 40)
Pitcher1=as.character(Pitcher1A1$Pitcher1)

launch_speed1=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed1 <- data.frame(lapply(launch_speed1, as.character), stringsAsFactors=FALSE)
options(warn=-1)



for(i in Pitcher1){ try({launch_speed1=rbind(launch_speed1,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

launch_speed1$Pitcher=ActualPitcher

SDFirst=aggregate(jj$launch_speed,by=list(jj$Pitch_typeHand), FUN=sd)

SDSecond=merge(Pitcher1A1,SDFirst, by.x="Pitcher1",by.y="Group.1")

SDThird=SDSecond

ActualPitcher=unique(pitcherlineup2$Pitcher)
Pitcher2A1=as.data.frame(Pitcher2)
Pitcher2B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher2B1,"Var1","Pitcher2")
Pitcher2A1=merge(Pitcher2A1,Pitcher2B1,by=c("Pitcher2"))
Pitcher2A1=subset(Pitcher2A1,Freq > 40)
Pitcher2=as.character(Pitcher2A1$Pitcher2)

launch_speed2=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed2 <- data.frame(lapply(launch_speed2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({launch_speed2=rbind(launch_speed2,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup2,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed2$Pitcher=ActualPitcher

SDFirst=aggregate(jj$launch_speed,by=list(jj$Pitch_typeHand), FUN=sd)

SDSecond=merge(Pitcher2A1,SDFirst, by.x="Pitcher2",by.y="Group.1")
setnames(SDSecond,"Pitcher2","Pitcher1")
SDThird=rbind(SDThird,SDSecond)


ActualPitcher=unique(pitcherlineup3$Pitcher)
Pitcher3A1=as.data.frame(Pitcher3)
Pitcher3B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher3B1,"Var1","Pitcher3")
Pitcher3A1=merge(Pitcher3A1,Pitcher3B1,by=c("Pitcher3"))
Pitcher3A1=subset(Pitcher3A1,Freq > 40)
Pitcher3=as.character(Pitcher3A1$Pitcher3)

launch_speed3=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed3 <- data.frame(lapply(launch_speed3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({launch_speed3=rbind(launch_speed3,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed3$Pitcher=ActualPitcher

SDSecond=merge(Pitcher3A1,SDFirst, by.x="Pitcher3",by.y="Group.1")
setnames(SDSecond,"Pitcher3","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup4$Pitcher)
Pitcher4A1=as.data.frame(Pitcher4)
Pitcher4B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher4B1,"Var1","Pitcher4")
Pitcher4A1=merge(Pitcher4A1,Pitcher4B1,by=c("Pitcher4"))
Pitcher4A1=subset(Pitcher4A1,Freq > 40)
Pitcher4=as.character(Pitcher4A1$Pitcher4)

launch_speed4=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed4 <- data.frame(lapply(launch_speed4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({launch_speed4=rbind(launch_speed4,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed4$Pitcher=ActualPitcher

SDSecond=merge(Pitcher4A1,SDFirst, by.x="Pitcher4",by.y="Group.1")
setnames(SDSecond,"Pitcher4","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup5$Pitcher)
Pitcher5A1=as.data.frame(Pitcher5)
Pitcher5B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher5B1,"Var1","Pitcher5")
Pitcher5A1=merge(Pitcher5A1,Pitcher5B1,by=c("Pitcher5"))
Pitcher5A1=subset(Pitcher5A1,Freq > 40)
Pitcher5=as.character(Pitcher5A1$Pitcher5)

launch_speed5=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed5 <- data.frame(lapply(launch_speed5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({launch_speed5=rbind(launch_speed5,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed5$Pitcher=ActualPitcher

SDSecond=merge(Pitcher5A1,SDFirst, by.x="Pitcher5",by.y="Group.1")
setnames(SDSecond,"Pitcher5","Pitcher1")
SDThird=rbind(SDThird,SDSecond)


ActualPitcher=unique(pitcherlineup6$Pitcher)
Pitcher6A1=as.data.frame(Pitcher6)
Pitcher6B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher6B1,"Var1","Pitcher6")
Pitcher6A1=merge(Pitcher6A1,Pitcher6B1,by=c("Pitcher6"))
Pitcher6A1=subset(Pitcher6A1,Freq > 40)
Pitcher6=as.character(Pitcher6A1$Pitcher6)

launch_speed6=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed6 <- data.frame(lapply(launch_speed6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({launch_speed6=rbind(launch_speed6,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed6$Pitcher=ActualPitcher

SDSecond=merge(Pitcher6A1,SDFirst, by.x="Pitcher6",by.y="Group.1")
setnames(SDSecond,"Pitcher6","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup7$Pitcher)
Pitcher7A1=as.data.frame(Pitcher7)
Pitcher7B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher7B1,"Var1","Pitcher7")
Pitcher7A1=merge(Pitcher7A1,Pitcher7B1,by=c("Pitcher7"))
Pitcher7A1=subset(Pitcher7A1,Freq > 40)
Pitcher7=as.character(Pitcher7A1$Pitcher7)

launch_speed7=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed7 <- data.frame(lapply(launch_speed7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({launch_speed7=rbind(launch_speed7,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed7$Pitcher=ActualPitcher

SDSecond=merge(Pitcher7A1,SDFirst, by.x="Pitcher7",by.y="Group.1")
setnames(SDSecond,"Pitcher7","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

SDFourth=unique(SDThird)


launch_speed=rbind(launch_speed1,launch_speed2,launch_speed3,launch_speed4,launch_speed5,launch_speed6,launch_speed7)

launch_speed$launch_speedLower=1
launch_speed$launch_speedHigher=200


launch_speed=launch_speed[,c("Pitch_typeHand","launchspeedFit","launch_speedLower","launch_speedHigher","Pitcher")]








#####################





ZoneContactA=with(jj,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContactA,"x","averagezone")

jj1=subset(jj,Contact=='1'& L1=="1")
ZoneContact1A=with(jj1,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact1A,"x","L1")

jj2=subset(jj,Contact=='1'& L2=="1")
ZoneContact2A=with(jj2,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact2A,"x","L2")

jj3=subset(jj,Contact=='1'& L3=="1")
ZoneContact3A=with(jj3,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact3A,"x","L3")
jj4=subset(jj,Contact=='1'& L4=="1")
ZoneContact4A=with(jj4,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact4A,"x","L4")
jj5=subset(jj,Contact=='1'& L5=="1")

ZoneContact5A=with(jj5,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact5A,"x","L5")
jj6=subset(jj,Contact=='1'& L6=="1")
ZoneContact6A=with(jj6,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact6A,"x","L6")
jj7=subset(jj,Contact=='1'& L7=="1")


ZoneContact7A=with(jj7,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact7A,"x","L7")
jj8=subset(jj,Contact=='1'& L8=="1")

ZoneContact8A=with(jj8,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact8A,"x","L8")
jj9=subset(jj,Contact=='1'& L9=="1")

ZoneContact9A=with(jj9,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact9A,"x","L9")
jj11=subset(jj,Contact=='1'& L11=="1")

ZoneContact11A=with(jj11,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact11A,"x","L11")
jj12=subset(jj,Contact=='1'& L12=="1")

ZoneContact12A=with(jj12,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact12A,"x","L12")
jj13=subset(jj,Contact=='1'& L13=="1")

ZoneContact13A=with(jj13,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact13A,"x","L13")
jj14=subset(jj,Contact=='1'& L1=="1")

ZoneContact14A=with(jj14,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact14A,"x","L14")



ZoneContactA=merge(ZoneContactA,ZoneContact1A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact2A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact3A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact4A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact5A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact6A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact7A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact8A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact9A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact11A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact12A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact13A,by=c('Group.1',"Group.2"),all=T)
ZoneContactA=merge(ZoneContactA,ZoneContact14A,by=c('Group.1',"Group.2"),all=T)

ZoneContactA[is.na(ZoneContactA)]<-0
ZoneContactA$L1=ifelse(ZoneContactA$L1==0,1.0434*ZoneContactA$averagezone,ZoneContactA$L1)
ZoneContactA$L2=ifelse(ZoneContactA$L2==0,1.0592544*ZoneContactA$averagezone,ZoneContactA$L2)
ZoneContactA$L3=ifelse(ZoneContactA$L3==0,0.9868112*ZoneContactA$averagezone,ZoneContactA$L3)
ZoneContactA$L4=ifelse(ZoneContactA$L4==0,1.0789315*ZoneContactA$averagezone,ZoneContactA$L4)
ZoneContactA$L5=ifelse(ZoneContactA$L5==0,1.1102549*ZoneContactA$averagezone,ZoneContactA$L5)
ZoneContactA$L6=ifelse(ZoneContactA$L6==0,1.1157934*ZoneContactA$averagezone,ZoneContactA$L6)
ZoneContactA$L7=ifelse(ZoneContactA$L7==0,0.9487843*ZoneContactA$averagezone,ZoneContactA$L7)
ZoneContactA$L8=ifelse(ZoneContactA$L8==0,1.0132233*ZoneContactA$averagezone,ZoneContactA$L8)
ZoneContactA$L9=ifelse(ZoneContactA$L9==0,0.9304815*ZoneContactA$averagezone,ZoneContactA$L9)
ZoneContactA$L11=ifelse(ZoneContactA$L11==0,.969728*ZoneContactA$averagezone,ZoneContactA$L11)
ZoneContactA$L12=ifelse(ZoneContactA$L12==0,0.9372415*ZoneContactA$averagezone,ZoneContactA$L12)
ZoneContactA$L13=ifelse(ZoneContactA$L13==0,0.748483*ZoneContactA$averagezone,ZoneContactA$L13)
ZoneContactA$L14=ifelse(ZoneContactA$L14==0,0.8235190*ZoneContactA$averagezone,ZoneContactA$L14)
setnames(ZoneContactA,'Group.1','Player')
setnames(ZoneContactA,'Group.2','PitchHand')


#############


CountContactA=with(jj,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContactA,"x","averagecountcontact")

count00A=subset(jj,Contact=='1'& balls=="0" & strikes =="0")
CountContact00A=with(count00A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact00A,"x","C00")


count10A=subset(jj,Contact=='1'& balls=="1" & strikes =="0")
CountContact10A=with(count10A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact10A,"x","C10")

count11A=subset(jj,Contact=='1'& balls=="1" & strikes =="1")
CountContact11A=with(count11A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact11A,"x","C11")

count12A=subset(jj,Contact=='1'& balls=="1" & strikes =="2")
CountContact12A=with(count12A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact12A,"x","C12")

count20A=subset(jj,Contact=='1'& balls=="2" & strikes =="0")
CountContact20A=with(count20A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact20A,"x","C20")

count21A=subset(jj,Contact=='1'& balls=="2" & strikes =="1")
CountContact21A=with(count21A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact21A,"x","C21")

count22A=subset(jj,Contact=='1'& balls=="2" & strikes =="2")
CountContact22A=with(count22A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact22A,"x","C22")

count30A=subset(jj,Contact=='1'& balls=="3" & strikes =="0")
CountContact30A=with(count30A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact30A,"x","C30")

count31A=subset(jj,Contact=='1'& balls=="3" & strikes =="1")
CountContact31A=with(count31A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact31A,"x","C31")

count32A=subset(jj,Contact=='1'& balls=="3" & strikes =="2")
CountContact32A=with(count32A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact32A,"x","C32")

count01A=subset(jj,Contact=='1'& balls=="0" & strikes =="1")
CountContact01A=with(count01A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact01A,"x","C01")

count02A=subset(jj,Contact=='1'& balls=="0" & strikes =="2")
CountContact02A=with(count02A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact02A,"x","C02")



CountContactA=merge(CountContactA,CountContact00A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact01A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact02A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact10A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact11A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact12A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact20A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact21A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact22A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact30A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact31A,by=c('Group.1',"Group.2"),all=T)
CountContactA=merge(CountContactA,CountContact32A,by=c('Group.1',"Group.2"),all=T)

CountContactA[is.na(CountContactA)]<-0
CountContactA$C00=ifelse(CountContactA$C00==0,1.0054962*CountContactA$averagecountcontact,CountContactA$C00)
CountContactA$C01=ifelse(CountContactA$C01==0,.9873316*CountContactA$averagecountcontact,CountContactA$C01)
CountContactA$C02=ifelse(CountContactA$C02==0,0.9457973*CountContactA$averagecountcontact,CountContactA$C02)
CountContactA$C10=ifelse(CountContactA$C10==0,1.0055785*CountContactA$averagecountcontact,CountContactA$C10)
CountContactA$C11=ifelse(CountContactA$C11==0,.9977059*CountContactA$averagecountcontact,CountContactA$C11)
CountContactA$C12=ifelse(CountContactA$C12==0,.9630723*CountContactA$averagecountcontact,CountContactA$C12)
CountContactA$C20=ifelse(CountContactA$C20==0,1.0476564*CountContactA$averagecountcontact,CountContactA$C20)
CountContactA$C21=ifelse(CountContactA$C21==0,1.0274699*CountContactA$averagecountcontact,CountContactA$C21)
CountContactA$C22=ifelse(CountContactA$C22==0,.9986828*CountContactA$averagecountcontact,CountContactA$C22)
CountContactA$C30=ifelse(CountContactA$C30==0,1.0624326*CountContactA$averagecountcontact,CountContactA$C30)
CountContactA$C31=ifelse(CountContactA$C31==0,1.1139230*CountContactA$averagecountcontact,CountContactA$C31)
CountContactA$C32=ifelse(CountContactA$C32==0,1.0513556*CountContactA$averagecountcontact,CountContactA$C32)

setnames(CountContactA,'Group.1','Player')
setnames(CountContactA,'Group.2','PitchHand')

ContactA=merge(ZoneContactA,CountContactA,by=c("Player","PitchHand"))


RealPitcherBatterContact=merge(RealPitcherBatterContact,launch_speed,by=c("Pitch_typeHand","Pitcher"))


balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatterContact=merge(RealPitcherBatterContact,balls)
setnames(RealPitcherBatterContact,"y","balls")
RealPitcherBatterContact=merge(RealPitcherBatterContact,strikes)
setnames(RealPitcherBatterContact,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatterContact=merge(RealPitcherBatterContact,zone)
setnames(RealPitcherBatterContact,"y","zone")
RealPitcherBatterContact=merge(RealPitcherBatterContact,ContactA,by=c("Player","PitchHand"))


RealPitcherBatterContact$RealL1=RealPitcherBatterContact$L1-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL2=RealPitcherBatterContact$L2-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL3=RealPitcherBatterContact$L3-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL4=RealPitcherBatterContact$L4-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL5=RealPitcherBatterContact$L5-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL6=RealPitcherBatterContact$L6-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL7=RealPitcherBatterContact$L7-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL8=RealPitcherBatterContact$L8-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL9=RealPitcherBatterContact$L9-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL11=RealPitcherBatterContact$L11-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL12=RealPitcherBatterContact$L12-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL13=RealPitcherBatterContact$L13-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL14=RealPitcherBatterContact$L14-RealPitcherBatterContact$averagezone

RealPitcherBatterContact$RealC00=RealPitcherBatterContact$C00-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC01=RealPitcherBatterContact$C01-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC02=RealPitcherBatterContact$C02-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC10=RealPitcherBatterContact$C10-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC11=RealPitcherBatterContact$C11-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC12=RealPitcherBatterContact$C12-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC20=RealPitcherBatterContact$C20-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC21=RealPitcherBatterContact$C21-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC22=RealPitcherBatterContact$C22-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC30=RealPitcherBatterContact$C30-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC31=RealPitcherBatterContact$C31-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC32=RealPitcherBatterContact$C32-RealPitcherBatterContact$averagecountcontact

RealPitcherBatterContact$launchspeedFit=as.numeric(RealPitcherBatterContact$launchspeedFit)
RealPitcherBatterContact$ZoneLaunchspeedFit =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL1,
                                                    ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL2,
                                                           ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL3,
                                                                  ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL4,
                                                                         ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL5,
                                                                                ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL6,
                                                                                       ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL7,
                                                                                              ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL8,
                                                                                                     ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL9,
                                                                                                            ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL11,
                                                                                                                   ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL12,
                                                                                                                          ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL13,
                                                                                                                                 ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL14,.7)))))))))))))





RealPitcherBatterContact$RealLaunchSpeed=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC00,
                                                ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC01,
                                                       ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC02,
                                                              ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC10,
                                                                     ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC11,
                                                                            ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC12,
                                                                                   ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC20,
                                                                                          ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC21,
                                                                                                 ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC22,
                                                                                                        ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC30,
                                                                                                               ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC31,
                                                                                                                      ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC32,.9))))))))))))












RealPitcherBatterContact$launch_speedLower=as.numeric(RealPitcherBatterContact$launch_speedLower)
RealPitcherBatterContact$ZoneLaunch_speedLower =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL1,
                                                       ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL2,
                                                              ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL3,
                                                                     ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL4,
                                                                            ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL5,
                                                                                   ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL6,
                                                                                          ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL7,
                                                                                                 ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL8,
                                                                                                        ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL9,
                                                                                                               ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL11,
                                                                                                                      ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL12,
                                                                                                                             ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL13,
                                                                                                                                    ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL14,.7)))))))))))))


RealPitcherBatterContact$RealLaunch_speedLower=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC00,
                                                      ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC01,
                                                             ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC02,
                                                                    ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC10,
                                                                           ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC11,
                                                                                  ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC12,
                                                                                         ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC20,
                                                                                                ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC21,
                                                                                                       ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC22,
                                                                                                              ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC30,
                                                                                                                     ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC31,
                                                                                                                            ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC32,.9))))))))))))




RealPitcherBatterContact$launch_speedHigher=as.numeric(RealPitcherBatterContact$launch_speedHigher)
RealPitcherBatterContact$ZoneLaunch_speedHigher =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL1,
                                                        ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL2,
                                                               ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL3,
                                                                      ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL4,
                                                                             ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL5,
                                                                                    ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL6,
                                                                                           ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL7,
                                                                                                  ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL8,
                                                                                                         ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL9,
                                                                                                                ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL11,
                                                                                                                       ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL12,
                                                                                                                              ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL13,
                                                                                                                                     ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL14,.7)))))))))))))


RealPitcherBatterContact$RealLaunch_speedHigher=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC00,
                                                       ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC01,
                                                              ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC02,
                                                                     ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC10,
                                                                            ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC11,
                                                                                   ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC12,
                                                                                          ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC20,
                                                                                                 ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC21,
                                                                                                        ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC22,
                                                                                                               ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC30,
                                                                                                                      ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC31,
                                                                                                                             ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC32,.9))))))))))))









#####################################launch_angle#################################################################

ActualPitcher=unique(pitcherlineup1$Pitcher)


launch_angle1=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle1 <- data.frame(lapply(launch_angle1, as.character), stringsAsFactors=FALSE)
options(warn=-1)

for(i in Pitcher1){ try({launch_angle1=rbind(launch_angle1,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

launch_angle1$Pitcher=ActualPitcher




ActualPitcher=unique(pitcherlineup2$Pitcher)


launch_angle2=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle2 <- data.frame(lapply(launch_angle2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({launch_angle2=rbind(launch_angle2,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup2,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle2$Pitcher=ActualPitcher



ActualPitcher=unique(pitcherlineup3$Pitcher)


launch_angle3=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle3 <- data.frame(lapply(launch_angle3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({launch_angle3=rbind(launch_angle3,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle3$Pitcher=ActualPitcher



ActualPitcher=unique(pitcherlineup4$Pitcher)


launch_angle4=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle4 <- data.frame(lapply(launch_angle4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({launch_angle4=rbind(launch_angle4,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle4$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup5$Pitcher)


launch_angle5=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle5 <- data.frame(lapply(launch_angle5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({launch_angle5=rbind(launch_angle5,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle5$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup6$Pitcher)


launch_angle6=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle6 <- data.frame(lapply(launch_angle6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({launch_angle6=rbind(launch_angle6,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle6$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup7$Pitcher)


launch_angle7=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle7 <- data.frame(lapply(launch_angle7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({launch_angle7=rbind(launch_angle7,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle7$Pitcher=ActualPitcher

launch_angle=rbind(launch_angle1,launch_angle2,launch_angle3,launch_angle4,launch_angle5,launch_angle6,launch_angle7)

launch_angle$launch_angleLower=-100
launch_angle$launch_angleHigher=100


launch_angle=launch_angle[,c("Pitch_typeHand","launch_angleFit","launch_angleLower","launch_angleHigher","Pitcher")]



launch_speed$launch_speedLower=ifelse(launch_speed$launch_speedLower=="NaN",0,launch_speed$launch_speedLower)
launch_speed$launch_speedHigher=ifelse(launch_speed$launch_speedHigher=="NaN",0,launch_speed$launch_speedHigher)
launch_angle$launch_angleLower=ifelse(launch_angle$launch_angleLower=="NaN",0,launch_angle$launch_angleLower)
launch_angle$launch_angleHigher=ifelse(launch_angle$launch_angleHigher=="NaN",0,launch_angle$launch_angleHigher)

launch_speed$launch_speedLower=ifelse(launch_speed$launch_speedLower==0,launch_speed$launchspeedFit,launch_speed$launch_speedLower)
launch_speed$launch_speedHigher=ifelse(launch_speed$launch_speedHigher==0,launch_speed$launchspeedFit,launch_speed$launch_speedHigher)
launch_angle$launch_angleLower=ifelse(launch_angle$launch_angleLower==0,launch_angle$launch_angleFit,launch_angle$launch_angleLower)
launch_angle$launch_angleHigher=ifelse(launch_angle$launch_angleHigher==0,launch_angle$launch_angleFit,launch_angle$launch_angleHigher)



ZoneContactB=with(jj,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContactB,"x","averagezone")

jj1B=subset(jj,Contact=='1'& L1=="1")
jj1B$launch_angle=as.numeric(jj1B$launch_angle)
ZoneContact1B=with(jj1B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact1B,"x","L1")

jj2B=subset(jj,Contact=='1'& L2=="1")
jj2B$launch_angle=as.numeric(jj2B$launch_angle)
ZoneContact2B=with(jj2B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact2B,"x","L2")

jj3B=subset(jj,Contact=='1'& L3=="1")
jj3B$launch_angle=as.numeric(jj3B$launch_angle)
ZoneContact3B=with(jj3B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact3B,"x","L3")

jj4B=subset(jj,Contact=='1'& L4=="1")
jj4B$launch_angle=as.numeric(jj4B$launch_angle)
ZoneContact4B=with(jj4B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact4B,"x","L4")

jj5B=subset(jj,Contact=='1'& L5=="1")
jj5B$launch_angle=as.numeric(jj5B$launch_angle)
ZoneContact5B=with(jj5B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact5B,"x","L5")

jj6B=subset(jj,Contact=='1'& L6=="1")
jj6B$launch_angle=as.numeric(jj6B$launch_angle)
ZoneContact6B=with(jj6B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact6B,"x","L6")

jj7B=subset(jj,Contact=='1'& L7=="1")
jj7B$launch_angle=as.numeric(jj7B$launch_angle)

ZoneContact7B=with(jj7B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact7B,"x","L7")

jj8B=subset(jj,Contact=='1'& L8=="1")
jj8B$launch_angle=as.numeric(jj8B$launch_angle)

ZoneContact8B=with(jj8B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact8B,"x","L8")

jj9B=subset(jj,Contact=='1'& L9=="1")
jj9B$launch_angle=as.numeric(jj9B$launch_angle)

ZoneContact9B=with(jj9B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact9B,"x","L9")

jj11B=subset(jj,Contact=='1'& L11=="1")
jj11B$launch_angle=as.numeric(jj11B$launch_angle)

ZoneContact11B=with(jj11B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact11B,"x","L11")

jj12B=subset(jj,Contact=='1'& L12=="1")
jj12B$launch_angle=as.numeric(jj12B$launch_angle)

ZoneContact12B=with(jj12B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact12B,"x","L12")

jj13B=subset(jj,Contact=='1'& L13=="1")
jj13B$launch_angle=as.numeric(jj13B$launch_angle)

ZoneContact13B=with(jj13B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact13B,"x","L13")

jj14B=subset(jj,Contact=='1'& L14=="1")
jj14B$launch_angle=as.numeric(jj14B$launch_angle)

ZoneContact14B=with(jj14B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact14B,"x","L14")



ZoneContactB=merge(ZoneContactB,ZoneContact1B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact2B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact3B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact4B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact5B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact6B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact7B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact8B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact9B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact11B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact12B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact13B,by=c('Group.1',"Group.2"),all=T)
ZoneContactB=merge(ZoneContactB,ZoneContact14B,by=c('Group.1',"Group.2"),all=T)

ZoneContactB[is.na(ZoneContactB)]<-0
ZoneContactB$L1=ifelse(ZoneContactB$L1==0,1.0434*ZoneContactB$averagezone,ZoneContactB$L1)
ZoneContactB$L2=ifelse(ZoneContactB$L2==0,1.0592544*ZoneContactB$averagezone,ZoneContactB$L2)
ZoneContactB$L3=ifelse(ZoneContactB$L3==0,0.9868112*ZoneContactB$averagezone,ZoneContactB$L3)
ZoneContactB$L4=ifelse(ZoneContactB$L4==0,1.0789315*ZoneContactB$averagezone,ZoneContactB$L4)
ZoneContactB$L5=ifelse(ZoneContactB$L5==0,1.1102549*ZoneContactB$averagezone,ZoneContactB$L5)
ZoneContactB$L6=ifelse(ZoneContactB$L6==0,1.1157934*ZoneContactB$averagezone,ZoneContactB$L6)
ZoneContactB$L7=ifelse(ZoneContactB$L7==0,0.9487843*ZoneContactB$averagezone,ZoneContactB$L7)
ZoneContactB$L8=ifelse(ZoneContactB$L8==0,1.0132233*ZoneContactB$averagezone,ZoneContactB$L8)
ZoneContactB$L9=ifelse(ZoneContactB$L9==0,0.9304815*ZoneContactB$averagezone,ZoneContactB$L9)
ZoneContactB$L11=ifelse(ZoneContactB$L11==0,.969728*ZoneContactB$averagezone,ZoneContactB$L11)
ZoneContactB$L12=ifelse(ZoneContactB$L12==0,0.9372415*ZoneContactB$averagezone,ZoneContactB$L12)
ZoneContactB$L13=ifelse(ZoneContactB$L13==0,0.748483*ZoneContactB$averagezone,ZoneContactB$L13)
ZoneContactB$L14=ifelse(ZoneContactB$L14==0,0.8235190*ZoneContactB$averagezone,ZoneContactB$L14)



#############


CountContactB=with(jj,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContactB,"x","averagecountcontact")

count00B=subset(jj,Contact=='1'& balls=="0" & strikes =="0")
CountContact00B=with(count00B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact00B,"x","C00")


count10B=subset(jj,Contact=='1'& balls=="1" & strikes =="0")
CountContact10B=with(count10B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact10B,"x","C10")

count11B=subset(jj,Contact=='1'& balls=="1" & strikes =="1")
CountContact11B=with(count11B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact11B,"x","C11")

count12B=subset(jj,Contact=='1'& balls=="1" & strikes =="2")
CountContact12B=with(count12B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact12B,"x","C12")

count20B=subset(jj,Contact=='1'& balls=="2" & strikes =="0")
CountContact20B=with(count20B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact20B,"x","C20")

count21B=subset(jj,Contact=='1'& balls=="2" & strikes =="1")
CountContact21B=with(count21B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact21B,"x","C21")

count22B=subset(jj,Contact=='1'& balls=="2" & strikes =="2")
CountContact22B=with(count22B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact22B,"x","C22")

count30B=subset(jj,Contact=='1'& balls=="3" & strikes =="0")
CountContact30B=with(count30B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact30B,"x","C30")

count31B=subset(jj,Contact=='1'& balls=="3" & strikes =="1")
CountContact31B=with(count31B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact31B,"x","C31")

count32B=subset(jj,Contact=='1'& balls=="3" & strikes =="2")
CountContact32B=with(count32B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact32B,"x","C32")

count01B=subset(jj,Contact=='1'& balls=="0" & strikes =="1")
CountContact01B=with(count01B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact01B,"x","C01")

count02B=subset(jj,Contact=='1'& balls=="0" & strikes =="2")
CountContact02B=with(count02B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact02B,"x","C02")



CountContactB=merge(CountContactB,CountContact00B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact01B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact02B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact10B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact11B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact12B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact20B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact21B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact22B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact30B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact31B,by=c('Group.1',"Group.2"),all=T)
CountContactB=merge(CountContactB,CountContact32B,by=c('Group.1',"Group.2"),all=T)

CountContactB[is.na(CountContactB)]<-0
CountContactB$C00=ifelse(CountContactB$C00==0,1.0054962*CountContactB$averagecountcontact,CountContactB$C00)
CountContactB$C01=ifelse(CountContactB$C01==0,.9873316*CountContactB$averagecountcontact,CountContactB$C01)
CountContactB$C02=ifelse(CountContactB$C02==0,0.9457973*CountContactB$averagecountcontact,CountContactB$C02)
CountContactB$C10=ifelse(CountContactB$C10==0,1.0055785*CountContactB$averagecountcontact,CountContactB$C10)
CountContactB$C11=ifelse(CountContactB$C11==0,.9977059*CountContactB$averagecountcontact,CountContactB$C11)
CountContactB$C12=ifelse(CountContactB$C12==0,.9630723*CountContactB$averagecountcontact,CountContactB$C12)
CountContactB$C20=ifelse(CountContactB$C20==0,1.0476564*CountContactB$averagecountcontact,CountContactB$C20)
CountContactB$C21=ifelse(CountContactB$C21==0,1.0274699*CountContactB$averagecountcontact,CountContactB$C21)
CountContactB$C22=ifelse(CountContactB$C22==0,.9986828*CountContactB$averagecountcontact,CountContactB$C22)
CountContactB$C30=ifelse(CountContactB$C30==0,1.0624326*CountContactB$averagecountcontact,CountContactB$C30)
CountContactB$C31=ifelse(CountContactB$C31==0,1.1139230*CountContactB$averagecountcontact,CountContactB$C31)
CountContactB$C32=ifelse(CountContactB$C32==0,1.0513556*CountContactB$averagecountcontact,CountContactB$C32)

setnames(CountContactB,'Group.1','Player')
setnames(CountContactB,'Group.2','PitchHand')
setnames(ZoneContactB,'Group.1','Player')
setnames(ZoneContactB,'Group.2','PitchHand')
ContactB=merge(ZoneContactB,CountContactB,by=c("Player","PitchHand"))


RealPitcherBatterContactB=merge(RealPitcherBatterContactB,launch_angle,by=c("Pitch_typeHand","Pitcher"))


balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,balls)
setnames(RealPitcherBatterContactB,"y","balls")
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,strikes)
setnames(RealPitcherBatterContactB,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,zone)
setnames(RealPitcherBatterContactB,"y","zone")
#######righthere#######
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,ContactB,by=c("Player","PitchHand"))


RealPitcherBatterContactB$RealL1=(RealPitcherBatterContactB$L1 + 65)/(RealPitcherBatterContactB$averagezone +65)
RealPitcherBatterContactB$RealL2=(RealPitcherBatterContactB$L2 + 65)/(RealPitcherBatterContactB$averagezone +65)
RealPitcherBatterContactB$RealL3=(RealPitcherBatterContactB$L3 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL4=(RealPitcherBatterContactB$L4 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL5=(RealPitcherBatterContactB$L5 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL6=(RealPitcherBatterContactB$L6 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL7=(RealPitcherBatterContactB$L7 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL8=(RealPitcherBatterContactB$L8 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL9=(RealPitcherBatterContactB$L9 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL11=(RealPitcherBatterContactB$L11 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL12=(RealPitcherBatterContactB$L12 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL13=(RealPitcherBatterContactB$L13 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL14=(RealPitcherBatterContactB$L14 + 65)/(RealPitcherBatterContactB$averagezone + 65)

RealPitcherBatterContactB$RealC00=RealPitcherBatterContactB$C00-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC01=RealPitcherBatterContactB$C01-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC02=RealPitcherBatterContactB$C02-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC10=RealPitcherBatterContactB$C10-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC11=RealPitcherBatterContactB$C11-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC12=RealPitcherBatterContactB$C12-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC20=RealPitcherBatterContactB$C20-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC21=RealPitcherBatterContactB$C21-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC22=RealPitcherBatterContactB$C22-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC30=RealPitcherBatterContactB$C30-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC31=RealPitcherBatterContactB$C31-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC32=RealPitcherBatterContactB$C32-RealPitcherBatterContactB$averagecountcontact

RealPitcherBatterContactB$launch_angleFit=as.numeric(RealPitcherBatterContactB$launch_angleFit)
RealPitcherBatterContactB$Zonelaunch_angleFit =ifelse(RealPitcherBatterContactB$zone==1,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL1)-65,
                                                      ifelse(RealPitcherBatterContactB$zone==2,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL2)-65,
                                                             ifelse(RealPitcherBatterContactB$zone==3,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL3)-65,
                                                                    ifelse(RealPitcherBatterContactB$zone==4,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL4)-65,
                                                                           ifelse(RealPitcherBatterContactB$zone==5,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL5)-65,
                                                                                  ifelse(RealPitcherBatterContactB$zone==6,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL6)-65,
                                                                                         ifelse(RealPitcherBatterContactB$zone==7,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL7)-65,
                                                                                                ifelse(RealPitcherBatterContactB$zone==8,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL8)-65,
                                                                                                       ifelse(RealPitcherBatterContactB$zone==9,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL9)-65,
                                                                                                              ifelse(RealPitcherBatterContactB$zone==11,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL11)-65,
                                                                                                                     ifelse(RealPitcherBatterContactB$zone==12,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL12)-65,
                                                                                                                            ifelse(RealPitcherBatterContactB$zone==13,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL13)-65,
                                                                                                                                   ifelse(RealPitcherBatterContactB$zone==14,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL14)-65,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angle=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0054962)-65,
                                                  ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*0.9873316)-65,
                                                         ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*0.9457973)-65,
                                                                ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0055785)-65,
                                                                       ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9977059)-65,
                                                                              ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9630723)-65,
                                                                                     ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0476564)-65,
                                                                                            ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0274699)-65,
                                                                                                   ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9986828)-65,
                                                                                                          ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0624326)-65,
                                                                                                                 ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.1139230)-65,
                                                                                                                        ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0513556)-65,.9))))))))))))










RealPitcherBatterContactB$launch_angleLower=as.numeric(RealPitcherBatterContactB$launch_angleLower)
RealPitcherBatterContactB$Zonelaunch_angleLowerFit =ifelse(RealPitcherBatterContactB$zone==1,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL1,
                                                           ifelse(RealPitcherBatterContactB$zone==2,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL2,
                                                                  ifelse(RealPitcherBatterContactB$zone==3,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL3,
                                                                         ifelse(RealPitcherBatterContactB$zone==4,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL4,
                                                                                ifelse(RealPitcherBatterContactB$zone==5,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL5,
                                                                                       ifelse(RealPitcherBatterContactB$zone==6,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL6,
                                                                                              ifelse(RealPitcherBatterContactB$zone==7,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL7,
                                                                                                     ifelse(RealPitcherBatterContactB$zone==8,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL8,
                                                                                                            ifelse(RealPitcherBatterContactB$zone==9,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL9,
                                                                                                                   ifelse(RealPitcherBatterContactB$zone==11,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL11,
                                                                                                                          ifelse(RealPitcherBatterContactB$zone==12,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL12,
                                                                                                                                 ifelse(RealPitcherBatterContactB$zone==13,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL13,
                                                                                                                                        ifelse(RealPitcherBatterContactB$zone==14,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL14,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angleLower=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC00,
                                                       ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC01,
                                                              ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC02,
                                                                     ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC10,
                                                                            ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC11,
                                                                                   ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC12,
                                                                                          ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC20,
                                                                                                 ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC21,
                                                                                                        ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC22,
                                                                                                               ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC30,
                                                                                                                      ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC31,
                                                                                                                             ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC32,.9))))))))))))

RealPitcherBatterContactB$launch_angleHigher=as.numeric(RealPitcherBatterContactB$launch_angleHigher)
RealPitcherBatterContactB$Zonelaunch_angleHigherFit =ifelse(RealPitcherBatterContactB$zone==1,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL1,
                                                            ifelse(RealPitcherBatterContactB$zone==2,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL2,
                                                                   ifelse(RealPitcherBatterContactB$zone==3,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL3,
                                                                          ifelse(RealPitcherBatterContactB$zone==4,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL4,
                                                                                 ifelse(RealPitcherBatterContactB$zone==5,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL5,
                                                                                        ifelse(RealPitcherBatterContactB$zone==6,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL6,
                                                                                               ifelse(RealPitcherBatterContactB$zone==7,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL7,
                                                                                                      ifelse(RealPitcherBatterContactB$zone==8,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL8,
                                                                                                             ifelse(RealPitcherBatterContactB$zone==9,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL9,
                                                                                                                    ifelse(RealPitcherBatterContactB$zone==11,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL11,
                                                                                                                           ifelse(RealPitcherBatterContactB$zone==12,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL12,
                                                                                                                                  ifelse(RealPitcherBatterContactB$zone==13,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL13,
                                                                                                                                         ifelse(RealPitcherBatterContactB$zone==14,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL14,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angleHigher=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC00,
                                                        ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC01,
                                                               ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC02,
                                                                      ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC10,
                                                                             ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC11,
                                                                                    ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC12,
                                                                                           ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC20,
                                                                                                  ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC21,
                                                                                                         ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC22,
                                                                                                                ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC30,
                                                                                                                       ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC31,
                                                                                                                              ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC32,.9))))))))))))






setnames(Awayplayerpercentageszone,"Balls","balls")
setnames(Awayplayerpercentageszone,"Strikes","strikes")



FinalPitcherBatter=RealPitcherBatterContactB[,c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone","Reallaunch_angle","Reallaunch_angleLower","Reallaunch_angleHigher")]
FinalPitcherBattertemp=RealPitcherBatterContact[,c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone","RealLaunchSpeed","RealLaunch_speedLower","RealLaunch_speedHigher")]
FinalPitcherBatter=merge(FinalPitcherBatter,FinalPitcherBattertemp,by=c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone"))





FinalHomeBatterPercentages=HomeBatterPercentages[,c("PitchHand","Balls","Strikes","Pitcher","Player","RealHand","percentageFF","percentageCU","percentageSL","percentageCH","percentageFT","percentageSI","percentageFS","percentageKC","percentageIN","percentagePO","percentageUN","percentageEP","percentageSC","percentageFC")]



PitchThrowType=FinalHomeBatterPercentages
PitchThrowZone=HomeBatterPercentageszone
Swing=batterdifferences
ChanceofContact=RealPitcherBatter
hitspeed=RealPitcherBatterContact
launchangle=RealPitcherBatterContactB

jj=subset(jj,hc_x!="null")  
jj=subset(jj,hc_x!="null")
jj$hc_x=as.numeric(jj$hc_x)
jj$hc_y=as.numeric(jj$hc_y)

jj$horangle=(round(tan((jj$hc_x-128)/(208-jj$hc_y))*180/pi*.75))
jj=subset(jj,horangle<45)
jj=subset(jj,horangle>-45)
jj$angle1=ifelse(jj$horangle>=27,1,0)
jj$angle2=ifelse(jj$horangle>=9 & jj$horangle<26.5,1,0)
jj$angle3=ifelse(jj$horangle<8.5 & jj$horangle>-8.5,1,0)
jj$angle4=ifelse(jj$horangle<=-8.5 & jj$horangle>=-27.5,1,0)
jj$angle5=ifelse(jj$horangle<=-27.5,1,0)

Angle1=with(jj,aggregate(angle1,list(player_name),mean))
setnames(Angle1,"x",'Angle1')
Angle2=with(jj,aggregate(angle2,list(player_name),mean))
setnames(Angle2,"x",'Angle2')
Angle3=with(jj,aggregate(angle3,list(player_name),mean))
setnames(Angle3,"x",'Angle3')
Angle4=with(jj,aggregate(angle4,list(player_name),mean))
setnames(Angle4,"x",'Angle4')
Angle5=with(jj,aggregate(angle5,list(player_name),mean))
setnames(Angle5,"x",'Angle5')

Angle=merge(Angle1,Angle2,by="Group.1")
Angle=merge(Angle,Angle3,by="Group.1")
Angle=merge(Angle,Angle4,by="Group.1")
Angle=merge(Angle,Angle5,by="Group.1")
setnames(Angle,"Group.1","Player")
Angle=merge(Angle,batterlineup,by="Player")
Angle[,7:10]=NULL

cc=subset(baseball,Contact=='1')
cc$foul=ifelse(cc$description=="foul",1,0)
foul=with(cc,aggregate(foul,list(player_name),mean))
setnames(foul,'Group.1',"Player")
setnames(foul,'x',"FoulPercentage")
foul=merge(foul,batterlineup,by="Player")
foul[,3:6]=NULL




#######################################################
######################################################
#####################################################









batterlineup=read.csv('baseballlineup2.csv',TRUE,',')
pitcherlineup=read.csv('pitcherlineup.csv',TRUE,',')
BaseballTeams=read.csv("BaseballTeams.csv",TRUE,",")

BaseballTeams$X=NULL
pitcherlineup=subset(pitcherlineup,Team==AwayTeam)
batterlineup=subset(batterlineup,Team==HomeTeam)


baseball=Firstbaseball

batters=as.data.frame(batterlineup$Player)
pitchers=as.data.frame(pitcherlineup$Pitcher)

baseball=subset(baseball,player_name==batters[1,]| player_name==batters[2,]| player_name==batters[3,]| player_name==batters[4,]| player_name==batters[5,]| player_name==batters[6,]| player_name==batters[7,]| player_name==batters[8,]| player_name==batters[9,]| player_name==batters[10,]| player_name==batters[11,]| player_name==batters[12,]| player_name==batters[13,]| player_name==batters[14,]| player_name==batters[15,]| player_name==batters[16,]| player_name==batters[17,]| player_name==batters[18,]| player_name==batters[19,]|player_name==batters[20,]|player_name==batters[21,]|player_name==batters[22,]|player_name==batters[23,]|PitcherName==pitchers[1,]| PitcherName==pitchers[2,]| PitcherName==pitchers[3,]| PitcherName==pitchers[4,]| PitcherName==pitchers[5,]| PitcherName==pitchers[6,]| PitcherName==pitchers[7,])




batterlineup=read.csv('baseballlineup2.csv',TRUE,',')
pitcherlineup=read.csv('pitcherlineup.csv',TRUE,',')
pitcherlineup=subset(pitcherlineup,Team==AwayTeam)
batterlineup=subset(batterlineup,Team==HomeTeam)

baseball=na.omit(baseball)

baseball=subset(baseball,effective_speed!="NA")

baseball$FF=0
baseball$SL=0
baseball$CU=0
baseball$CH=0
baseball$FT=0
baseball$SI=0
baseball$FS=0
baseball$KC=0
baseball$IN=0
baseball$PO=0
baseball$UN=0
baseball$EP=0
baseball$SC=0
baseball$FC=0
baseball$FF=ifelse(baseball$pitch_type=="FF",1,0)
baseball$SL=ifelse(baseball$pitch_type=="SL",1,0)
baseball$CU=ifelse(baseball$pitch_type=="CU",1,0)
baseball$CH=ifelse(baseball$pitch_type=="CH",1,0)
baseball$FT=ifelse(baseball$pitch_type=="FT",1,0)
baseball$SI=ifelse(baseball$pitch_type=="SI",1,0)
baseball$FS=ifelse(baseball$pitch_type=="FS",1,0)
baseball$KC=ifelse(baseball$pitch_type=="KC",1,0)
baseball$IN=ifelse(baseball$pitch_type=="IN",1,0)
baseball$PO=ifelse(baseball$pitch_type=="PO",1,0)
baseball$UN=ifelse(baseball$pitch_type=="UN",1,0)
baseball$EP=ifelse(baseball$pitch_type=="EP",1,0)
baseball$SC=ifelse(baseball$pitch_type=="SC",1,0)
baseball$FC=ifelse(baseball$pitch_type=="FC",1,0)





playerpercentagesFF=with(baseball,aggregate(FF,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFF,"Group.2","PitchHand")
setnames(playerpercentagesFF,"Group.3","Balls")
setnames(playerpercentagesFF,"Group.4","Strikes")
setnames(playerpercentagesFF,"x","FFPercentage")
playerpercentagesSL=with(baseball,aggregate(SL,list(player_name,p_throws,balls,strikes),mean))				
setnames(playerpercentagesSL,"Group.2","PitchHand")				
setnames(playerpercentagesSL,"x","SLPercentage")				
setnames(playerpercentagesSL,"Group.3","Balls")
setnames(playerpercentagesSL,"Group.4","Strikes")

playerpercentagesCU=with(baseball,aggregate(CU,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesCU,"Group.2","PitchHand")
setnames(playerpercentagesCU,"x","CUPercentage")
setnames(playerpercentagesCU,"Group.3","Balls")
setnames(playerpercentagesCU,"Group.4","Strikes")

playerpercentagesCH=with(baseball,aggregate(CH,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesCH,"Group.2","PitchHand")
setnames(playerpercentagesCH,"x","CHPercentage")
setnames(playerpercentagesCH,"Group.3","Balls")
setnames(playerpercentagesCH,"Group.4","Strikes")

playerpercentagesFT=with(baseball,aggregate(FT,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFT,"Group.2","PitchHand")
setnames(playerpercentagesFT,"x","FTPercentage")
setnames(playerpercentagesFT,"Group.3","Balls")
setnames(playerpercentagesFT,"Group.4","Strikes")

playerpercentagesSI=with(baseball,aggregate(SI,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesSI,"Group.2","PitchHand")
setnames(playerpercentagesSI,"x","SIPercentage")
setnames(playerpercentagesSI,"Group.3","Balls")
setnames(playerpercentagesSI,"Group.4","Strikes")

playerpercentagesFS=with(baseball,aggregate(FS,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFS,"Group.2","PitchHand")
setnames(playerpercentagesFS,"x","FSPercentage")
setnames(playerpercentagesFS,"Group.3","Balls")
setnames(playerpercentagesFS,"Group.4","Strikes")

playerpercentagesKC=with(baseball,aggregate(KC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesKC,"Group.2","PitchHand")
setnames(playerpercentagesKC,"x","KCPercentage")
setnames(playerpercentagesKC,"Group.3","Balls")
setnames(playerpercentagesKC,"Group.4","Strikes")

playerpercentagesIN=with(baseball,aggregate(IN,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesIN,"Group.2","PitchHand")
setnames(playerpercentagesIN,"x","INPercentage")
setnames(playerpercentagesIN,"Group.3","Balls")
setnames(playerpercentagesIN,"Group.4","Strikes")

playerpercentagesPO=with(baseball,aggregate(PO,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesPO,"Group.2","PitchHand")
setnames(playerpercentagesPO,"x","POPercentage")
setnames(playerpercentagesPO,"Group.3","Balls")
setnames(playerpercentagesPO,"Group.4","Strikes")

playerpercentagesUN=with(baseball,aggregate(UN,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesUN,"Group.2","PitchHand")
setnames(playerpercentagesUN,"x","UNPercentage")
setnames(playerpercentagesUN,"Group.3","Balls")
setnames(playerpercentagesUN,"Group.4","Strikes")

playerpercentagesEP=with(baseball,aggregate(EP,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesEP,"Group.2","PitchHand")
setnames(playerpercentagesEP,"x","EPPercentage")
setnames(playerpercentagesEP,"Group.3","Balls")
setnames(playerpercentagesEP,"Group.4","Strikes")

playerpercentagesSC=with(baseball,aggregate(SC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesSC,"Group.2","PitchHand")
setnames(playerpercentagesSC,"x","SCPercentage")
setnames(playerpercentagesSC,"Group.3","Balls")
setnames(playerpercentagesSC,"Group.4","Strikes")

playerpercentagesFC=with(baseball,aggregate(FC,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesFC,"Group.2","PitchHand")
setnames(playerpercentagesFC,"x","FCPercentage")
setnames(playerpercentagesFC,"Group.3","Balls")
setnames(playerpercentagesFC,"Group.4","Strikes")






pitcherpercentagesFF=with(baseball,aggregate(FF,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFF,"Group.2","Batterhand")
setnames(pitcherpercentagesFF,"Group.3","Balls")
setnames(pitcherpercentagesFF,"Group.4","Strikes")
setnames(pitcherpercentagesFF,"x","FFPercentage")


pitcherpercentagesSL=with(baseball,aggregate(SL,list(PitcherName,stand,balls,strikes),mean))				
setnames(pitcherpercentagesSL,"Group.2","Batterhand")				
setnames(pitcherpercentagesSL,"x","SLPercentage")				
setnames(pitcherpercentagesSL,"Group.3","Balls")
setnames(pitcherpercentagesSL,"Group.4","Strikes")

pitcherpercentagesCU=with(baseball,aggregate(CU,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesCU,"Group.2","Batterhand")
setnames(pitcherpercentagesCU,"x","CUPercentage")
setnames(pitcherpercentagesCU,"Group.3","Balls")
setnames(pitcherpercentagesCU,"Group.4","Strikes")

pitcherpercentagesCH=with(baseball,aggregate(CH,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesCH,"Group.2","Batterhand")
setnames(pitcherpercentagesCH,"x","CHPercentage")
setnames(pitcherpercentagesCH,"Group.3","Balls")
setnames(pitcherpercentagesCH,"Group.4","Strikes")

pitcherpercentagesFT=with(baseball,aggregate(FT,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFT,"Group.2","Batterhand")
setnames(pitcherpercentagesFT,"x","FTPercentage")
setnames(pitcherpercentagesFT,"Group.3","Balls")
setnames(pitcherpercentagesFT,"Group.4","Strikes")

pitcherpercentagesSI=with(baseball,aggregate(SI,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesSI,"Group.2","Batterhand")
setnames(pitcherpercentagesSI,"x","SIPercentage")
setnames(pitcherpercentagesSI,"Group.3","Balls")
setnames(pitcherpercentagesSI,"Group.4","Strikes")

pitcherpercentagesFS=with(baseball,aggregate(FS,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFS,"Group.2","Batterhand")
setnames(pitcherpercentagesFS,"x","FSPercentage")
setnames(pitcherpercentagesFS,"Group.3","Balls")
setnames(pitcherpercentagesFS,"Group.4","Strikes")

pitcherpercentagesKC=with(baseball,aggregate(KC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesKC,"Group.2","Batterhand")
setnames(pitcherpercentagesKC,"x","KCPercentage")
setnames(pitcherpercentagesKC,"Group.3","Balls")
setnames(pitcherpercentagesKC,"Group.4","Strikes")

pitcherpercentagesIN=with(baseball,aggregate(IN,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesIN,"Group.2","Batterhand")
setnames(pitcherpercentagesIN,"x","INPercentage")
setnames(pitcherpercentagesIN,"Group.3","Balls")
setnames(pitcherpercentagesIN,"Group.4","Strikes")

pitcherpercentagesPO=with(baseball,aggregate(PO,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesPO,"Group.2","Batterhand")
setnames(pitcherpercentagesPO,"x","POPercentage")
setnames(pitcherpercentagesPO,"Group.3","Balls")
setnames(pitcherpercentagesPO,"Group.4","Strikes")

pitcherpercentagesUN=with(baseball,aggregate(UN,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesUN,"Group.2","Batterhand")
setnames(pitcherpercentagesUN,"x","UNPercentage")
setnames(pitcherpercentagesUN,"Group.3","Balls")
setnames(pitcherpercentagesUN,"Group.4","Strikes")

pitcherpercentagesEP=with(baseball,aggregate(EP,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesEP,"Group.2","Batterhand")
setnames(pitcherpercentagesEP,"x","EPPercentage")
setnames(pitcherpercentagesEP,"Group.3","Balls")
setnames(pitcherpercentagesEP,"Group.4","Strikes")

pitcherpercentagesSC=with(baseball,aggregate(SC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesSC,"Group.2","Batterhand")
setnames(pitcherpercentagesSC,"x","SCPercentage")
setnames(pitcherpercentagesSC,"Group.3","Balls")
setnames(pitcherpercentagesSC,"Group.4","Strikes")

pitcherpercentagesFC=with(baseball,aggregate(FC,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesFC,"Group.2","Batterhand")
setnames(pitcherpercentagesFC,"x","FCPercentage")
setnames(pitcherpercentagesFC,"Group.3","Balls")
setnames(pitcherpercentagesFC,"Group.4","Strikes")



playerpercentages=merge(playerpercentagesFF,playerpercentagesCU,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSL,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesCH,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFT,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSI,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFS,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesKC,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesIN,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesPO,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesUN,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesEP,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesSC,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentages=merge(playerpercentages,playerpercentagesFC,by=c("Group.1","PitchHand",'Balls','Strikes'))



pitcherpercentages=merge(pitcherpercentagesFF,pitcherpercentagesCU,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSL,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesCH,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFT,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSI,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFS,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesKC,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesIN,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesPO,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesUN,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesEP,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesSC,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentages=merge(pitcherpercentages,pitcherpercentagesFC,by=c("Group.1","Batterhand",'Balls','Strikes'))

pitcherpercentages$FFPercentage=ifelse(pitcherpercentages$FFPercentage<0.15,0,pitcherpercentages$FFPercentage)
pitcherpercentages$SLPercentage=ifelse(pitcherpercentages$SLPercentage<0.15,0,pitcherpercentages$SLPercentage)
pitcherpercentages$CHPercentage=ifelse(pitcherpercentages$CHPercentage<0.15,0,pitcherpercentages$CHPercentage)
pitcherpercentages$FTPercentage=ifelse(pitcherpercentages$FTPercentage<0.15,0,pitcherpercentages$FTPercentage)
pitcherpercentages$SIPercentage=ifelse(pitcherpercentages$SIPercentage<0.15,0,pitcherpercentages$SIPercentage)
pitcherpercentages$FSPercentage=ifelse(pitcherpercentages$FSPercentage<0.15,0,pitcherpercentages$FSPercentage)
pitcherpercentages$KCPercentage=ifelse(pitcherpercentages$KCPercentage<0.15,0,pitcherpercentages$KCPercentage)
pitcherpercentages$INPercentage=ifelse(pitcherpercentages$INPercentage<0.15,0,pitcherpercentages$INPercentage)
pitcherpercentages$POPercentage=ifelse(pitcherpercentages$POPercentage<0.15,0,pitcherpercentages$POPercentage)
pitcherpercentages$UNPercentage=ifelse(pitcherpercentages$UNPercentage<0.15,0,pitcherpercentages$UNPercentage)
pitcherpercentages$EPPercentage=ifelse(pitcherpercentages$EPPercentage<0.15,0,pitcherpercentages$EPPercentage)
pitcherpercentages$SCPercentage=ifelse(pitcherpercentages$SCPercentage<0.15,0,pitcherpercentages$SCPercentage)
pitcherpercentages$FCPercentage=ifelse(pitcherpercentages$FCPercentage<0.15,0,pitcherpercentages$FCPercentage)
pitcherpercentages$CUPercentage=ifelse(pitcherpercentages$CUPercentage<0.15,0,pitcherpercentages$CUPercentage)


####################
pitcherpercentages$sum=rowSums(pitcherpercentages[,5:18])
pitcherpercentages$multiplier=1/pitcherpercentages$sum
pitcherpercentages$FFPercentage=pitcherpercentages$FFPercentage*pitcherpercentages$multiplier
pitcherpercentages$CUPercentage=pitcherpercentages$CUPercentage*pitcherpercentages$multiplier
pitcherpercentages$SLPercentage=pitcherpercentages$SLPercentage*pitcherpercentages$multiplier
pitcherpercentages$CHPercentage=pitcherpercentages$CHPercentage*pitcherpercentages$multiplier
pitcherpercentages$FTPercentage=pitcherpercentages$FTPercentage*pitcherpercentages$multiplier
pitcherpercentages$SIPercentage=pitcherpercentages$SIPercentage*pitcherpercentages$multiplier
pitcherpercentages$FSPercentage=pitcherpercentages$FSPercentage*pitcherpercentages$multiplier
pitcherpercentages$KCPercentage=pitcherpercentages$KCPercentage*pitcherpercentages$multiplier
pitcherpercentages$INPercentage=pitcherpercentages$INPercentage*pitcherpercentages$multiplier
pitcherpercentages$POPercentage=pitcherpercentages$POPercentage*pitcherpercentages$multiplier
pitcherpercentages$UNPercentage=pitcherpercentages$UNPercentage*pitcherpercentages$multiplier
pitcherpercentages$EPPercentage=pitcherpercentages$EPPercentage*pitcherpercentages$multiplier
pitcherpercentages$SCPercentage=pitcherpercentages$SCPercentage*pitcherpercentages$multiplier
pitcherpercentages$FCPercentage=pitcherpercentages$FCPercentage*pitcherpercentages$multiplier
pitcherpercentages$newsum=rowSums(pitcherpercentages[,5:18])


############










setnames(playerpercentages,"Group.1","Player")
setnames(pitcherpercentages,"Group.1","Pitcher")
playerpercentages=merge(batterlineup,playerpercentages,by="Player")
pitcherpercentages=merge(pitcherlineup,pitcherpercentages,by="Pitcher")
Homepitcherpercentages=subset(pitcherpercentages,Team==HomeTeam)
Awaypitcherpercentages=subset(pitcherpercentages,Team==AwayTeam)
Homeplayerpercentages=subset(playerpercentages,Team==HomeTeam)
Awayplayerpercentages=subset(playerpercentages,Team==AwayTeam)

HomeBatterPercentages=merge(Homepitcherpercentages,Awayplayerpercentages,by=c("PitchHand","Balls","Strikes"))
AwayBatterPercentages=merge(Awaypitcherpercentages,Homeplayerpercentages,by=c("PitchHand","Balls","Strikes"))
HomeBatterPercentages$RealHand=HomeBatterPercentages$Hand
HomeBatterPercentages$FakeHand=ifelse((HomeBatterPercentages$Hand=='S'|HomeBatterPercentages$PitchHand=='R'),"L","R")
HomeBatterPercentages$Hand=as.character(HomeBatterPercentages$Hand)
HomeBatterPercentages$RealHand=ifelse(HomeBatterPercentages$Hand=='S',HomeBatterPercentages$FakeHand,HomeBatterPercentages$Hand)
HomeBatterPercentages=subset(HomeBatterPercentages,HomeBatterPercentages$RealHand==HomeBatterPercentages$Batterhand)
HomeBatterPercentages$prepercentageFF=ifelse(HomeBatterPercentages$FFPercentage.x>0.15,(HomeBatterPercentages$FFPercentage.x+HomeBatterPercentages$FFPercentage.y)/2,0)
HomeBatterPercentages$prepercentageCU=ifelse(HomeBatterPercentages$CUPercentage.x>0.15,(HomeBatterPercentages$CUPercentage.x+HomeBatterPercentages$CUPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSL=ifelse(HomeBatterPercentages$SLPercentage.x>0.15  ,(HomeBatterPercentages$SLPercentage.x+HomeBatterPercentages$SLPercentage.y)/2,0)
HomeBatterPercentages$prepercentageCH=ifelse(HomeBatterPercentages$CHPercentage.x>0.15 ,(HomeBatterPercentages$CHPercentage.x+HomeBatterPercentages$CHPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFT=ifelse(HomeBatterPercentages$FTPercentage.x>0.15 ,(HomeBatterPercentages$FTPercentage.x+HomeBatterPercentages$FTPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSI=ifelse(HomeBatterPercentages$SIPercentage.x>0.15  ,(HomeBatterPercentages$SIPercentage.x+HomeBatterPercentages$SIPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFS=ifelse(HomeBatterPercentages$FSPercentage.x>0.15  ,(HomeBatterPercentages$FSPercentage.x+HomeBatterPercentages$FSPercentage.y)/2,0)
HomeBatterPercentages$prepercentageKC=ifelse(HomeBatterPercentages$KCPercentage.x>0.15  ,(HomeBatterPercentages$KCPercentage.x+HomeBatterPercentages$KCPercentage.y)/2,0)
HomeBatterPercentages$prepercentageIN=ifelse(HomeBatterPercentages$INPercentage.x>0.15  ,(HomeBatterPercentages$INPercentage.x+HomeBatterPercentages$INPercentage.y)/2,0)
HomeBatterPercentages$prepercentagePO=ifelse(HomeBatterPercentages$POPercentage.x>0.15 ,(HomeBatterPercentages$POPercentage.x+HomeBatterPercentages$POPercentage.y)/2,0)
HomeBatterPercentages$prepercentageUN=ifelse(HomeBatterPercentages$UNPercentage.x>0.15  ,(HomeBatterPercentages$UNPercentage.x+HomeBatterPercentages$UNPercentage.y)/2,0)
HomeBatterPercentages$prepercentageEP=ifelse(HomeBatterPercentages$EPPercentage.x>0.15  ,(HomeBatterPercentages$EPPercentage.x+HomeBatterPercentages$EPPercentage.y)/2,0)
HomeBatterPercentages$prepercentageSC=ifelse(HomeBatterPercentages$SCPercentage.x>0.15 ,(HomeBatterPercentages$SCPercentage.x+HomeBatterPercentages$SCPercentage.y)/2,0)
HomeBatterPercentages$prepercentageFC=ifelse(HomeBatterPercentages$FCPercentage.x>0.15 ,(HomeBatterPercentages$FCPercentage.x+HomeBatterPercentages$FCPercentage.y)/2,0)
HomeBatterPercentages$SUM=HomeBatterPercentages$prepercentageFF+HomeBatterPercentages$prepercentageCU+HomeBatterPercentages$prepercentageSL+HomeBatterPercentages$prepercentageCH+HomeBatterPercentages$prepercentageFT+HomeBatterPercentages$prepercentageSI+HomeBatterPercentages$prepercentageFS+HomeBatterPercentages$prepercentageKC+HomeBatterPercentages$prepercentageIN+HomeBatterPercentages$prepercentagePO+HomeBatterPercentages$prepercentageUN+HomeBatterPercentages$prepercentageEP+HomeBatterPercentages$prepercentageSC+HomeBatterPercentages$prepercentageFC
HomeBatterPercentages$Multiplier=1/HomeBatterPercentages$SUM
HomeBatterPercentages$percentageFF=HomeBatterPercentages$prepercentageFF*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageCU=HomeBatterPercentages$prepercentageCU*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSL=HomeBatterPercentages$prepercentageSL*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageCH=HomeBatterPercentages$prepercentageCH*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFT=HomeBatterPercentages$prepercentageFT*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSI=HomeBatterPercentages$prepercentageSI*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFS=HomeBatterPercentages$prepercentageFS*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageKC=HomeBatterPercentages$prepercentageKC*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageIN=HomeBatterPercentages$prepercentageIN*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentagePO=HomeBatterPercentages$prepercentagePO*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageUN=HomeBatterPercentages$prepercentageUN*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageEP=HomeBatterPercentages$prepercentageEP*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageSC=HomeBatterPercentages$prepercentageSC*HomeBatterPercentages$Multiplier
HomeBatterPercentages$percentageFC=HomeBatterPercentages$prepercentageFC*HomeBatterPercentages$Multiplier

AwayBatterPercentages$RealHand=AwayBatterPercentages$Hand
AwayBatterPercentages$FakeHand=ifelse((AwayBatterPercentages$Hand=='S'|AwayBatterPercentages$PitchHand=='R'),"L","R")
AwayBatterPercentages$Hand=as.character(AwayBatterPercentages$Hand)
AwayBatterPercentages$RealHand=ifelse(AwayBatterPercentages$Hand=='S',AwayBatterPercentages$FakeHand,AwayBatterPercentages$Hand)
AwayBatterPercentages=subset(AwayBatterPercentages,AwayBatterPercentages$RealHand==AwayBatterPercentages$Batterhand)
AwayBatterPercentages$prepercentageFF=ifelse(AwayBatterPercentages$FFPercentage.x>0.15 ,(AwayBatterPercentages$FFPercentage.x+AwayBatterPercentages$FFPercentage.y)/2,0)
AwayBatterPercentages$prepercentageCU=ifelse(AwayBatterPercentages$CUPercentage.x>0.15 ,(AwayBatterPercentages$CUPercentage.x+AwayBatterPercentages$CUPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSL=ifelse(AwayBatterPercentages$SLPercentage.x>0.15  ,(AwayBatterPercentages$SLPercentage.x+AwayBatterPercentages$SLPercentage.y)/2,0)
AwayBatterPercentages$prepercentageCH=ifelse(AwayBatterPercentages$CHPercentage.x>0.15 ,(AwayBatterPercentages$CHPercentage.x+AwayBatterPercentages$CHPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFT=ifelse(AwayBatterPercentages$FTPercentage.x>0.15  ,(AwayBatterPercentages$FTPercentage.x+AwayBatterPercentages$FTPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSI=ifelse(AwayBatterPercentages$SIPercentage.x>0.15  ,(AwayBatterPercentages$SIPercentage.x+AwayBatterPercentages$SIPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFS=ifelse(AwayBatterPercentages$FSPercentage.x>0.15 ,(AwayBatterPercentages$FSPercentage.x+AwayBatterPercentages$FSPercentage.y)/2,0)
AwayBatterPercentages$prepercentageKC=ifelse(AwayBatterPercentages$KCPercentage.x>0.15 ,(AwayBatterPercentages$KCPercentage.x+AwayBatterPercentages$KCPercentage.y)/2,0)
AwayBatterPercentages$prepercentageIN=ifelse(AwayBatterPercentages$INPercentage.x>0.15  ,(AwayBatterPercentages$INPercentage.x+AwayBatterPercentages$INPercentage.y)/2,0)
AwayBatterPercentages$prepercentagePO=ifelse(AwayBatterPercentages$POPercentage.x>0.15 ,(AwayBatterPercentages$POPercentage.x+AwayBatterPercentages$POPercentage.y)/2,0)
AwayBatterPercentages$prepercentageUN=ifelse(AwayBatterPercentages$UNPercentage.x>0.15 ,(AwayBatterPercentages$UNPercentage.x+AwayBatterPercentages$UNPercentage.y)/2,0)
AwayBatterPercentages$prepercentageEP=ifelse(AwayBatterPercentages$EPPercentage.x>0.15  ,(AwayBatterPercentages$EPPercentage.x+AwayBatterPercentages$EPPercentage.y)/2,0)
AwayBatterPercentages$prepercentageSC=ifelse(AwayBatterPercentages$SCPercentage.x>0.15 ,(AwayBatterPercentages$SCPercentage.x+AwayBatterPercentages$SCPercentage.y)/2,0)
AwayBatterPercentages$prepercentageFC=ifelse(AwayBatterPercentages$FCPercentage.x>0.15,(AwayBatterPercentages$FCPercentage.x+AwayBatterPercentages$FCPercentage.y)/2,0)
AwayBatterPercentages$SUM=AwayBatterPercentages$prepercentageFF+AwayBatterPercentages$prepercentageCU+AwayBatterPercentages$prepercentageSL+AwayBatterPercentages$prepercentageCH+AwayBatterPercentages$prepercentageFT+AwayBatterPercentages$prepercentageSI+AwayBatterPercentages$prepercentageFS+AwayBatterPercentages$prepercentageKC+AwayBatterPercentages$prepercentageIN+AwayBatterPercentages$prepercentagePO+AwayBatterPercentages$prepercentageUN+AwayBatterPercentages$prepercentageEP+AwayBatterPercentages$prepercentageSC+AwayBatterPercentages$prepercentageFC
AwayBatterPercentages$Multiplier=1/AwayBatterPercentages$SUM
AwayBatterPercentages$percentageFF=AwayBatterPercentages$prepercentageFF*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageCU=AwayBatterPercentages$prepercentageCU*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSL=AwayBatterPercentages$prepercentageSL*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageCH=AwayBatterPercentages$prepercentageCH*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFT=AwayBatterPercentages$prepercentageFT*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSI=AwayBatterPercentages$prepercentageSI*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFS=AwayBatterPercentages$prepercentageFS*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageKC=AwayBatterPercentages$prepercentageKC*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageIN=AwayBatterPercentages$prepercentageIN*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentagePO=AwayBatterPercentages$prepercentagePO*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageUN=AwayBatterPercentages$prepercentageUN*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageEP=AwayBatterPercentages$prepercentageEP*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageSC=AwayBatterPercentages$prepercentageSC*AwayBatterPercentages$Multiplier
AwayBatterPercentages$percentageFC=AwayBatterPercentages$prepercentageFC*AwayBatterPercentages$Multiplier


baseball$L1=ifelse(baseball$zone=="1",1,0)
baseball$L2=ifelse(baseball$zone=="2",1,0)
baseball$L3=ifelse(baseball$zone=="3",1,0)
baseball$L4=ifelse(baseball$zone=="4",1,0)
baseball$L5=ifelse(baseball$zone=="5",1,0)
baseball$L6=ifelse(baseball$zone=="6",1,0)
baseball$L7=ifelse(baseball$zone=="7",1,0)
baseball$L8=ifelse(baseball$zone=="8",1,0)
baseball$L9=ifelse(baseball$zone=="9",1,0)
baseball$L11=ifelse(baseball$zone=="11",1,0)
baseball$L12=ifelse(baseball$zone=="12",1,0)
baseball$L13=ifelse(baseball$zone=="13",1,0)
baseball$L14=ifelse(baseball$zone=="14",1,0)


playerpercentagesL1=with(baseball,aggregate(L1,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL1,"Group.2","PitchHand")
setnames(playerpercentagesL1,"Group.3","Balls")
setnames(playerpercentagesL1,"Group.4","Strikes")
setnames(playerpercentagesL1,"x","L1Percentage")

playerpercentagesL2=with(baseball,aggregate(L2,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL2,"Group.2","PitchHand")
setnames(playerpercentagesL2,"Group.3","Balls")
setnames(playerpercentagesL2,"Group.4","Strikes")
setnames(playerpercentagesL2,"x","L2Percentage")

playerpercentagesL3=with(baseball,aggregate(L3,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL3,"Group.2","PitchHand")
setnames(playerpercentagesL3,"Group.3","Balls")
setnames(playerpercentagesL3,"Group.4","Strikes")
setnames(playerpercentagesL3,"x","L3Percentage")

playerpercentagesL4=with(baseball,aggregate(L4,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL4,"Group.2","PitchHand")
setnames(playerpercentagesL4,"Group.3","Balls")
setnames(playerpercentagesL4,"Group.4","Strikes")
setnames(playerpercentagesL4,"x","L4Percentage")

playerpercentagesL5=with(baseball,aggregate(L5,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL5,"Group.2","PitchHand")
setnames(playerpercentagesL5,"Group.3","Balls")
setnames(playerpercentagesL5,"Group.4","Strikes")
setnames(playerpercentagesL5,"x","L5Percentage")

playerpercentagesL6=with(baseball,aggregate(L6,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL6,"Group.2","PitchHand")
setnames(playerpercentagesL6,"Group.3","Balls")
setnames(playerpercentagesL6,"Group.4","Strikes")
setnames(playerpercentagesL6,"x","L6Percentage")

playerpercentagesL7=with(baseball,aggregate(L7,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL7,"Group.2","PitchHand")
setnames(playerpercentagesL7,"Group.3","Balls")
setnames(playerpercentagesL7,"Group.4","Strikes")
setnames(playerpercentagesL7,"x","L7Percentage")

playerpercentagesL8=with(baseball,aggregate(L8,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL8,"Group.2","PitchHand")
setnames(playerpercentagesL8,"Group.3","Balls")
setnames(playerpercentagesL8,"Group.4","Strikes")
setnames(playerpercentagesL8,"x","L8Percentage")

playerpercentagesL9=with(baseball,aggregate(L9,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL9,"Group.2","PitchHand")
setnames(playerpercentagesL9,"Group.3","Balls")
setnames(playerpercentagesL9,"Group.4","Strikes")
setnames(playerpercentagesL9,"x","L9Percentage")

playerpercentagesL11=with(baseball,aggregate(L11,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL11,"Group.2","PitchHand")
setnames(playerpercentagesL11,"Group.3","Balls")
setnames(playerpercentagesL11,"Group.4","Strikes")
setnames(playerpercentagesL11,"x","L11Percentage")

playerpercentagesL12=with(baseball,aggregate(L12,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL12,"Group.2","PitchHand")
setnames(playerpercentagesL12,"Group.3","Balls")
setnames(playerpercentagesL12,"Group.4","Strikes")
setnames(playerpercentagesL12,"x","L12Percentage")

playerpercentagesL13=with(baseball,aggregate(L13,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL13,"Group.2","PitchHand")
setnames(playerpercentagesL13,"Group.3","Balls")
setnames(playerpercentagesL13,"Group.4","Strikes")
setnames(playerpercentagesL13,"x","L13Percentage")

playerpercentagesL14=with(baseball,aggregate(L14,list(player_name,p_throws,balls,strikes),mean))
setnames(playerpercentagesL14,"Group.2","PitchHand")
setnames(playerpercentagesL14,"Group.3","Balls")
setnames(playerpercentagesL14,"Group.4","Strikes")
setnames(playerpercentagesL14,"x","L14Percentage")


pitcherpercentagesL1=with(baseball,aggregate(L1,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL1,"Group.2","Batterhand")
setnames(pitcherpercentagesL1,"Group.3","Balls")
setnames(pitcherpercentagesL1,"Group.4","Strikes")
setnames(pitcherpercentagesL1,"x","L1Percentage")

pitcherpercentagesL2=with(baseball,aggregate(L2,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL2,"Group.2","Batterhand")
setnames(pitcherpercentagesL2,"Group.3","Balls")
setnames(pitcherpercentagesL2,"Group.4","Strikes")
setnames(pitcherpercentagesL2,"x","L2Percentage")

pitcherpercentagesL3=with(baseball,aggregate(L3,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL3,"Group.2","Batterhand")
setnames(pitcherpercentagesL3,"Group.3","Balls")
setnames(pitcherpercentagesL3,"Group.4","Strikes")
setnames(pitcherpercentagesL3,"x","L3Percentage")

pitcherpercentagesL4=with(baseball,aggregate(L4,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL4,"Group.2","Batterhand")
setnames(pitcherpercentagesL4,"Group.3","Balls")
setnames(pitcherpercentagesL4,"Group.4","Strikes")
setnames(pitcherpercentagesL4,"x","L4Percentage")

pitcherpercentagesL5=with(baseball,aggregate(L5,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL5,"Group.2","Batterhand")
setnames(pitcherpercentagesL5,"Group.3","Balls")
setnames(pitcherpercentagesL5,"Group.4","Strikes")
setnames(pitcherpercentagesL5,"x","L5Percentage")

pitcherpercentagesL6=with(baseball,aggregate(L6,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL6,"Group.2","Batterhand")
setnames(pitcherpercentagesL6,"Group.3","Balls")
setnames(pitcherpercentagesL6,"Group.4","Strikes")
setnames(pitcherpercentagesL6,"x","L6Percentage")

pitcherpercentagesL7=with(baseball,aggregate(L7,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL7,"Group.2","Batterhand")
setnames(pitcherpercentagesL7,"Group.3","Balls")
setnames(pitcherpercentagesL7,"Group.4","Strikes")
setnames(pitcherpercentagesL7,"x","L7Percentage")

pitcherpercentagesL8=with(baseball,aggregate(L8,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL8,"Group.2","Batterhand")
setnames(pitcherpercentagesL8,"Group.3","Balls")
setnames(pitcherpercentagesL8,"Group.4","Strikes")
setnames(pitcherpercentagesL8,"x","L8Percentage")

pitcherpercentagesL9=with(baseball,aggregate(L9,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL9,"Group.2","Batterhand")
setnames(pitcherpercentagesL9,"Group.3","Balls")
setnames(pitcherpercentagesL9,"Group.4","Strikes")
setnames(pitcherpercentagesL9,"x","L9Percentage")

pitcherpercentagesL11=with(baseball,aggregate(L11,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL11,"Group.2","Batterhand")
setnames(pitcherpercentagesL11,"Group.3","Balls")
setnames(pitcherpercentagesL11,"Group.4","Strikes")
setnames(pitcherpercentagesL11,"x","L11Percentage")

pitcherpercentagesL12=with(baseball,aggregate(L12,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL12,"Group.2","Batterhand")
setnames(pitcherpercentagesL12,"Group.3","Balls")
setnames(pitcherpercentagesL12,"Group.4","Strikes")
setnames(pitcherpercentagesL12,"x","L12Percentage")

pitcherpercentagesL13=with(baseball,aggregate(L13,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL13,"Group.2","Batterhand")
setnames(pitcherpercentagesL13,"Group.3","Balls")
setnames(pitcherpercentagesL13,"Group.4","Strikes")
setnames(pitcherpercentagesL13,"x","L13Percentage")

pitcherpercentagesL14=with(baseball,aggregate(L14,list(PitcherName,stand,balls,strikes),mean))
setnames(pitcherpercentagesL14,"Group.2","Batterhand")
setnames(pitcherpercentagesL14,"Group.3","Balls")
setnames(pitcherpercentagesL14,"Group.4","Strikes")
setnames(pitcherpercentagesL14,"x","L14Percentage")

playerpercentageszone=merge(playerpercentagesL1,playerpercentagesL2,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL3,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL4,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL5,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL6,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL7,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL8,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL9,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL11,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL12,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL13,by=c("Group.1","PitchHand",'Balls','Strikes'))
playerpercentageszone=merge(playerpercentageszone,playerpercentagesL14,by=c("Group.1","PitchHand",'Balls','Strikes'))


pitcherpercentageszone=merge(pitcherpercentagesL1,pitcherpercentagesL2,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL3,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL4,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL5,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL6,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL7,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL8,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL9,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL11,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL12,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL13,by=c("Group.1","Batterhand",'Balls','Strikes'))
pitcherpercentageszone=merge(pitcherpercentageszone,pitcherpercentagesL14,by=c("Group.1","Batterhand",'Balls','Strikes'))

setnames(playerpercentageszone,"Group.1","Player")
setnames(pitcherpercentageszone,"Group.1","Pitcher")
playerpercentageszone=merge(batterlineup,playerpercentageszone,by="Player")
pitcherpercentageszone=merge(pitcherlineup,pitcherpercentageszone,by="Pitcher")
Homepitcherpercentageszone=subset(pitcherpercentageszone,Team==HomeTeam)
Awaypitcherpercentageszone=subset(pitcherpercentageszone,Team==AwayTeam)
Homeplayerpercentageszone=subset(playerpercentageszone,Team==HomeTeam)
Awayplayerpercentageszone=subset(playerpercentageszone,Team==AwayTeam)
########################################################

HomeBatterPercentageszone=merge(Homepitcherpercentageszone,Awayplayerpercentageszone,by=c("PitchHand","Balls","Strikes"))
AwayBatterPercentageszone=merge(Awaypitcherpercentageszone,Homeplayerpercentageszone,by=c("PitchHand","Balls","Strikes"))
HomeBatterPercentageszone$RealHand=HomeBatterPercentageszone$Hand
HomeBatterPercentageszone$FakeHand=ifelse((HomeBatterPercentageszone$Hand=='S'|HomeBatterPercentageszone$PitchHand=='R'),"L","R")
HomeBatterPercentageszone$Hand=as.character(HomeBatterPercentageszone$Hand)
HomeBatterPercentageszone$RealHand=ifelse(HomeBatterPercentageszone$Hand=='S',HomeBatterPercentageszone$FakeHand,HomeBatterPercentageszone$Hand)
HomeBatterPercentageszone=subset(HomeBatterPercentageszone,HomeBatterPercentageszone$RealHand==HomeBatterPercentageszone$Batterhand)
HomeBatterPercentageszone$prepercentageL1=ifelse(HomeBatterPercentageszone$L1Percentage.x>0.02,(HomeBatterPercentageszone$L1Percentage.x+HomeBatterPercentageszone$L1Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL2=ifelse(HomeBatterPercentageszone$L2Percentage.x>0.02,(HomeBatterPercentageszone$L2Percentage.x+HomeBatterPercentageszone$L2Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL3=ifelse(HomeBatterPercentageszone$L3Percentage.x>0.02,(HomeBatterPercentageszone$L3Percentage.x+HomeBatterPercentageszone$L3Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL4=ifelse(HomeBatterPercentageszone$L4Percentage.x>0.02,(HomeBatterPercentageszone$L4Percentage.x+HomeBatterPercentageszone$L4Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL5=ifelse(HomeBatterPercentageszone$L5Percentage.x>0.02,(HomeBatterPercentageszone$L5Percentage.x+HomeBatterPercentageszone$L5Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL6=ifelse(HomeBatterPercentageszone$L6Percentage.x>0.02,(HomeBatterPercentageszone$L6Percentage.x+HomeBatterPercentageszone$L6Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL7=ifelse(HomeBatterPercentageszone$L7Percentage.x>0.02,(HomeBatterPercentageszone$L7Percentage.x+HomeBatterPercentageszone$L7Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL8=ifelse(HomeBatterPercentageszone$L8Percentage.x>0.02,(HomeBatterPercentageszone$L8Percentage.x+HomeBatterPercentageszone$L8Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL9=ifelse(HomeBatterPercentageszone$L9Percentage.x>0.02,(HomeBatterPercentageszone$L9Percentage.x+HomeBatterPercentageszone$L9Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL11=ifelse(HomeBatterPercentageszone$L11Percentage.x>0.02,(HomeBatterPercentageszone$L11Percentage.x+HomeBatterPercentageszone$L11Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL12=ifelse(HomeBatterPercentageszone$L12Percentage.x>0.02,(HomeBatterPercentageszone$L12Percentage.x+HomeBatterPercentageszone$L12Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL13=ifelse(HomeBatterPercentageszone$L13Percentage.x>0.02,(HomeBatterPercentageszone$L13Percentage.x+HomeBatterPercentageszone$L13Percentage.y)/2,0)
HomeBatterPercentageszone$prepercentageL14=ifelse(HomeBatterPercentageszone$L14Percentage.x>0.02,(HomeBatterPercentageszone$L14Percentage.x+HomeBatterPercentageszone$L14Percentage.y)/2,0)
HomeBatterPercentageszone$SUM=HomeBatterPercentageszone$prepercentageL1+HomeBatterPercentageszone$prepercentageL2+HomeBatterPercentageszone$prepercentageL3+HomeBatterPercentageszone$prepercentageL4+HomeBatterPercentageszone$prepercentageL5+HomeBatterPercentageszone$prepercentageL6+HomeBatterPercentageszone$prepercentageL7+HomeBatterPercentageszone$prepercentageL8+HomeBatterPercentageszone$prepercentageL9+HomeBatterPercentageszone$prepercentageL11+HomeBatterPercentageszone$prepercentageL12+HomeBatterPercentageszone$prepercentageL13+HomeBatterPercentageszone$prepercentageL14
HomeBatterPercentageszone$Multiplier=1/HomeBatterPercentageszone$SUM
HomeBatterPercentageszone$percentageL1=HomeBatterPercentageszone$prepercentageL1*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL2=HomeBatterPercentageszone$prepercentageL2*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL3=HomeBatterPercentageszone$prepercentageL3*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL4=HomeBatterPercentageszone$prepercentageL4*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL5=HomeBatterPercentageszone$prepercentageL5*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL6=HomeBatterPercentageszone$prepercentageL6*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL7=HomeBatterPercentageszone$prepercentageL7*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL8=HomeBatterPercentageszone$prepercentageL8*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL9=HomeBatterPercentageszone$prepercentageL9*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL11=HomeBatterPercentageszone$prepercentageL11*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL12=HomeBatterPercentageszone$prepercentageL12*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL13=HomeBatterPercentageszone$prepercentageL13*HomeBatterPercentageszone$Multiplier
HomeBatterPercentageszone$percentageL14=HomeBatterPercentageszone$prepercentageL14*HomeBatterPercentageszone$Multiplier

AwayBatterPercentageszone$RealHand=AwayBatterPercentageszone$Hand
AwayBatterPercentageszone$FakeHand=ifelse((AwayBatterPercentageszone$Hand=='S'|AwayBatterPercentageszone$PitchHand=='R'),"L","R")
AwayBatterPercentageszone$Hand=as.character(AwayBatterPercentageszone$Hand)
AwayBatterPercentageszone$RealHand=ifelse(AwayBatterPercentageszone$Hand=='S',AwayBatterPercentageszone$FakeHand,AwayBatterPercentageszone$Hand)
AwayBatterPercentageszone=subset(AwayBatterPercentageszone,AwayBatterPercentageszone$RealHand==AwayBatterPercentageszone$Batterhand)
AwayBatterPercentageszone$prepercentageL1=ifelse(AwayBatterPercentageszone$L1Percentage.x>0.02,(AwayBatterPercentageszone$L1Percentage.x+AwayBatterPercentageszone$L1Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL2=ifelse(AwayBatterPercentageszone$L2Percentage.x>0.02,(AwayBatterPercentageszone$L2Percentage.x+AwayBatterPercentageszone$L2Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL3=ifelse(AwayBatterPercentageszone$L3Percentage.x>0.02,(AwayBatterPercentageszone$L3Percentage.x+AwayBatterPercentageszone$L3Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL4=ifelse(AwayBatterPercentageszone$L4Percentage.x>0.02,(AwayBatterPercentageszone$L4Percentage.x+AwayBatterPercentageszone$L4Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL5=ifelse(AwayBatterPercentageszone$L5Percentage.x>0.02,(AwayBatterPercentageszone$L5Percentage.x+AwayBatterPercentageszone$L5Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL6=ifelse(AwayBatterPercentageszone$L6Percentage.x>0.02,(AwayBatterPercentageszone$L6Percentage.x+AwayBatterPercentageszone$L6Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL7=ifelse(AwayBatterPercentageszone$L7Percentage.x>0.02,(AwayBatterPercentageszone$L7Percentage.x+AwayBatterPercentageszone$L7Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL8=ifelse(AwayBatterPercentageszone$L8Percentage.x>0.02,(AwayBatterPercentageszone$L8Percentage.x+AwayBatterPercentageszone$L8Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL9=ifelse(AwayBatterPercentageszone$L9Percentage.x>0.02,(AwayBatterPercentageszone$L9Percentage.x+AwayBatterPercentageszone$L9Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL11=ifelse(AwayBatterPercentageszone$L11Percentage.x>0.02,(AwayBatterPercentageszone$L11Percentage.x+AwayBatterPercentageszone$L11Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL12=ifelse(AwayBatterPercentageszone$L12Percentage.x>0.02,(AwayBatterPercentageszone$L12Percentage.x+AwayBatterPercentageszone$L12Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL13=ifelse(AwayBatterPercentageszone$L13Percentage.x>0.02,(AwayBatterPercentageszone$L13Percentage.x+AwayBatterPercentageszone$L13Percentage.y)/2,0)
AwayBatterPercentageszone$prepercentageL14=ifelse(AwayBatterPercentageszone$L14Percentage.x>0.02,(AwayBatterPercentageszone$L14Percentage.x+AwayBatterPercentageszone$L14Percentage.y)/2,0)
AwayBatterPercentageszone$SUM=AwayBatterPercentageszone$prepercentageL1+AwayBatterPercentageszone$prepercentageL2+AwayBatterPercentageszone$prepercentageL3+AwayBatterPercentageszone$prepercentageL4+AwayBatterPercentageszone$prepercentageL5+AwayBatterPercentageszone$prepercentageL6+AwayBatterPercentageszone$prepercentageL7+AwayBatterPercentageszone$prepercentageL8+AwayBatterPercentageszone$prepercentageL9+AwayBatterPercentageszone$prepercentageL11+AwayBatterPercentageszone$prepercentageL12+AwayBatterPercentageszone$prepercentageL13+AwayBatterPercentageszone$prepercentageL14
AwayBatterPercentageszone$Multiplier=1/AwayBatterPercentageszone$SUM
AwayBatterPercentageszone$percentageL1=AwayBatterPercentageszone$prepercentageL1*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL2=AwayBatterPercentageszone$prepercentageL2*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL3=AwayBatterPercentageszone$prepercentageL3*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL4=AwayBatterPercentageszone$prepercentageL4*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL5=AwayBatterPercentageszone$prepercentageL5*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL6=AwayBatterPercentageszone$prepercentageL6*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL7=AwayBatterPercentageszone$prepercentageL7*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL8=AwayBatterPercentageszone$prepercentageL8*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL9=AwayBatterPercentageszone$prepercentageL9*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL11=AwayBatterPercentageszone$prepercentageL11*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL12=AwayBatterPercentageszone$prepercentageL12*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL13=AwayBatterPercentageszone$prepercentageL13*AwayBatterPercentageszone$Multiplier
AwayBatterPercentageszone$percentageL14=AwayBatterPercentageszone$prepercentageL14*AwayBatterPercentageszone$Multiplier




#############################################################
baseball$Swing=ifelse(baseball$description=="hit_into_play",1,ifelse(baseball$description=="foul_tip",1,ifelse(baseball$description=="hit_into_play_no_out",1,ifelse(baseball$description=="foul",1,ifelse(baseball$description=="hit_into_play_score",1,ifelse(baseball$description=="swinging_strike",1,ifelse(baseball$description=="swinging_strike_blocked",1,0)))))))
baseball$Contact=ifelse(baseball$description=="hit_into_play",1,ifelse(baseball$description=="hit_into_play_no_out",1,ifelse(baseball$description=="foul",1,ifelse(baseball$description=="hit_into_play_score",1,0))))

playerpercentageswingpitch=with(baseball,aggregate(Swing,list(player_name,p_throws,balls,strikes,pitch_type),mean))
setnames(playerpercentageswingpitch,"Group.5",'pitch_type')
setnames(playerpercentageswingpitch,"x",'pitchswingpercentage')
playerpercentageswingpitch=playerpercentageswingpitch[!(playerpercentageswingpitch$pitch_type=='null'),]

playerpercentageswinglocation=with(baseball,aggregate(Swing,list(player_name,p_throws,balls,strikes,zone),mean))
setnames(playerpercentageswinglocation,"Group.5",'zone')
setnames(playerpercentageswinglocation,"x",'locationswingpercentage')
playerpercentageswinglocation=playerpercentageswinglocation[!(playerpercentageswinglocation$location=='null'),]

swingpercentage=merge(playerpercentageswingpitch,playerpercentageswinglocation,by=c('Group.1','Group.2','Group.3','Group.4'))
swingpercentagezero=subset(swingpercentage,locationswingpercentage==0)
swingpercentageone=subset(swingpercentage,locationswingpercentage==1)
swingpercentagenotzero=subset(swingpercentage,locationswingpercentage>0.00001 & locationswingpercentage<1)

averageswing=with(swingpercentagenotzero,aggregate(locationswingpercentage,list(Group.1,Group.2,Group.3,Group.4,pitch_type),mean))
setnames(averageswing,'x','averageswing')
setnames(averageswing,'Group.5','pitch_type')
swingpercentagezero$averageswing=0
swingpercentageone$averageswing=1

averageswing=merge(swingpercentagenotzero,averageswing,by=c('Group.1','Group.2','Group.3','Group.4','pitch_type'))
averageswing$difference=averageswing$locationswingpercentage/averageswing$averageswing
averageswing$realaveswing=averageswing$pitchswingpercentage*averageswing$difference

swingpercentagezero$difference=0
swingpercentagezero$realaveswing=0
swingpercentageone$difference=1
swingpercentageone$realaveswing=1


averageswing=rbind(averageswing,swingpercentagezero,swingpercentageone)
averageswing$realaveswing=ifelse(averageswing$realaveswing>1,1,averageswing$realaveswing)




zone1=as.data.frame(c(1,2,3,4,5,6,7,8,9,11,12,13,14))
balls1=as.data.frame(c(0,1,2,3))
strikes1=as.data.frame(c(0,1,2))
hand1=as.data.frame(c("L","R"))
batterlineup1=batterlineup$Player
pitches=as.data.frame(c("FF","CU","SL","CH",'FT','SI','FS','KC','IN','PO','UN','EP','SC','FC'))
zone2=as.data.frame(c(1,2,3,4,5,6,7,8,9))
zone3=as.data.frame(c(11,12,13,14))

batterdifferences=merge(batterlineup1,zone1)
batterdifferences=merge(batterdifferences,balls1)
batterdifferences=merge(batterdifferences,strikes1)
batterdifferences=merge(batterdifferences,hand1)


names(batterdifferences)[1]="Player"
names(batterdifferences)[2]="zone"
names(batterdifferences)[3]="balls"
names(batterdifferences)[4]="strikes"
names(batterdifferences)[5]="p_throw"



uu=baseball
uu=subset(uu,zone!="null")
uu$zone=as.numeric(uu$zone)
uu=subset(uu,zone < 10)
uu=with(uu,aggregate(Swing,list(player_name,p_throws,balls,strikes),mean))
names(uu)[1]="Player"
names(uu)[2]="p_throw"
names(uu)[3]="balls"
names(uu)[4]="strikes"
names(uu)[5]="averageswingpercent"

batterdifferences1=subset(batterdifferences,zone<10)

batterdifferences1=merge(batterdifferences1,uu,by=c("Player","p_throw","balls","strikes"),all=TRUE)
batterdifferences1$averageswingpercent[is.na(batterdifferences1$averageswingpercent)]<-0
batterdifferences1=merge(batterdifferences1,pitches)
names(batterdifferences1)[7]="pitch_type"


yy=baseball
yy=subset(yy,zone!="null")
yy$zone=as.numeric(yy$zone)
yy=subset(yy,zone > 10)
yy=with(yy,aggregate(Swing,list(player_name,p_throws,balls,strikes),mean))
names(yy)[1]="Player"
names(yy)[2]="p_throw"
names(yy)[3]="balls"
names(yy)[4]="strikes"
names(yy)[5]="averageswingpercent"

batterdifferences2=subset(batterdifferences,zone>10)

batterdifferences2=merge(batterdifferences2,uu,by=c("Player","p_throw","balls","strikes"),all=TRUE)
batterdifferences2$averageswingpercent[is.na(batterdifferences2$averageswingpercent)]<-0
batterdifferences2=merge(batterdifferences2,pitches)
names(batterdifferences2)[7]="pitch_type"
batterdifferences=rbind(batterdifferences1,batterdifferences2)




names(averageswing)[1]="Player"
names(averageswing)[2]="p_throw"
names(averageswing)[3]="balls"
names(averageswing)[4]="strikes"






averageswing=averageswing[,c("Player","p_throw","balls","strikes","pitch_type","zone","realaveswing")]
batterdifferences=merge(batterdifferences,averageswing,by=c("Player","p_throw","balls","strikes","pitch_type","zone"),all =TRUE)
batterdifferences=subset(batterdifferences,averageswingpercent>-0.1)

batterdifferences$realaveswing[is.na(batterdifferences$realaveswing)]<-55
batterdifferences$realaveswing=ifelse(batterdifferences$realaveswing==55,batterdifferences$averageswingpercent,batterdifferences$realaveswing)


baseball=subset(baseball,effective_speed!="null")
baseball$effective_speed=as.numeric(baseball$effective_speed)

avespin=with(baseball,aggregate(spin_rate,list(pitch_type,p_throws),mean))
avebreak_angle=with(baseball,aggregate(break_angle,list(pitch_type,p_throws),mean))
avebreak_length=with(baseball,aggregate(break_length,list(pitch_type,p_throws),mean))
aveeffective_speed=with(baseball,aggregate(effective_speed,list(pitch_type,p_throws),mean))

baseball=subset(baseball,effective_speed>0.1)


avepitchereffective_speed=with(baseball,aggregate(effective_speed,list(pitch_type,stand,PitcherName),mean))
setnames(avepitchereffective_speed,'x','avepitchereffective_speed')

avepitcherspin=with(baseball,aggregate(spin_rate,list(pitch_type,stand,PitcherName),mean))
avepitcherspin=subset(avepitcherspin,x>0.1)
setnames(avepitcherspin,'x','avepitcherspin')
avepitcherbreak_angle=with(baseball,aggregate(break_angle,list(pitch_type,stand,PitcherName),mean))
avepitcherbreak_angle=subset(avepitcherbreak_angle,x !=0)
setnames(avepitcherbreak_angle,'x','aveavepitcherbreak_angle')
avepitcherbreak_length=with(baseball,aggregate(break_length,list(pitch_type,stand,PitcherName),mean))
avepitcherbreak_length=subset(avepitcherbreak_length,x>0.1)
setnames(avepitcherbreak_length,'x','avepitcherbreak_length')
pitchernumbers=merge(avepitcherspin,avepitcherbreak_angle,by=c("Group.1","Group.2","Group.3"))
pitchernumbers=merge(pitchernumbers,avepitcherbreak_length,by=c("Group.1","Group.2","Group.3"))
pitchernumbers=merge(pitchernumbers,avepitchereffective_speed,by=c("Group.1","Group.2","Group.3"))
setnames(pitchernumbers,"Group.3","Pitcher")
pitchernumbers=merge(pitchernumbers,pitcherlineup,by="Pitcher")
setnames(pitchernumbers,"Group.2","Stand")
setnames(pitchernumbers,"Group.1","pitch_type")


batterhand=batterlineup[,c("Player","Hand")]
PitcherBatter=merge(pitchernumbers,batterhand)
PitcherBatter$RealHand=ifelse(PitcherBatter$Hand=="R","R", ifelse(PitcherBatter$Hand=="L","L",ifelse(PitcherBatter$Hand=="S"& PitcherBatter$PitchHand=="R","L",ifelse(PitcherBatter$Hand=="S"& PitcherBatter$PitchHand=="L","R","A"))))
PitcherBatter=subset(PitcherBatter,Stand==RealHand)


baseball$Pitch_typeHand=paste(baseball$player_name,baseball$p_throws,baseball$pitch_type,sep="")
PitcherBatter$Pitch_typeHand=paste(PitcherBatter$Player,PitcherBatter$PitchHand,PitcherBatter$pitch_type,sep="")

setnames(PitcherBatter,"avepitcherspin","spin_rate")
setnames(PitcherBatter,"aveavepitcherbreak_angle","break_angle")
setnames(PitcherBatter,"avepitcherbreak_length","break_length")
setnames(PitcherBatter,"avepitchereffective_speed","effective_speed")



##############

pitcherpercentagesFF2=with(baseball,aggregate(FF,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFF2,"Group.1","Pitcher")
setnames(pitcherpercentagesFF2,"Group.2","RealHand")
setnames(pitcherpercentagesFF2,"x","Percentage")
pitcherpercentagesFF2$pitch_type='FF'

pitcherpercentagesSL2=with(baseball,aggregate(SL,list(PitcherName,stand),mean))				
setnames(pitcherpercentagesSL2,"Group.2","RealHand")				
setnames(pitcherpercentagesSL2,"x","Percentage")				
setnames(pitcherpercentagesSL2,"Group.1","Pitcher")
pitcherpercentagesSL2$pitch_type='SL'

pitcherpercentagesCU2=with(baseball,aggregate(CU,list(PitcherName,stand),mean))
setnames(pitcherpercentagesCU2,"Group.2","RealHand")
setnames(pitcherpercentagesCU2,"x","Percentage")
setnames(pitcherpercentagesCU2,"Group.1","Pitcher")
pitcherpercentagesCU2$pitch_type='CU'

pitcherpercentagesCH2=with(baseball,aggregate(CH,list(PitcherName,stand),mean))
setnames(pitcherpercentagesCH2,"Group.2","RealHand")
setnames(pitcherpercentagesCH2,"x","Percentage")
setnames(pitcherpercentagesCH2,"Group.1","Pitcher")
pitcherpercentagesCH2$pitch_type='CH'

pitcherpercentagesFT2=with(baseball,aggregate(FT,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFT2,"Group.2","RealHand")
setnames(pitcherpercentagesFT2,"x","Percentage")
setnames(pitcherpercentagesFT2,"Group.1","Pitcher")
pitcherpercentagesFT2$pitch_type='FT'

pitcherpercentagesSI2=with(baseball,aggregate(SI,list(PitcherName,stand),mean))
setnames(pitcherpercentagesSI2,"Group.2","RealHand")
setnames(pitcherpercentagesSI2,"x","Percentage")
setnames(pitcherpercentagesSI2,"Group.1","Pitcher")
pitcherpercentagesSI2$pitch_type='SI'

pitcherpercentagesFS2=with(baseball,aggregate(FS,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFS2,"Group.2","RealHand")
setnames(pitcherpercentagesFS2,"x","Percentage")
setnames(pitcherpercentagesFS2,"Group.1","Pitcher")
pitcherpercentagesFS2$pitch_type='FS'

pitcherpercentagesKC2=with(baseball,aggregate(KC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesKC2,"Group.2","RealHand")
setnames(pitcherpercentagesKC2,"x","Percentage")
setnames(pitcherpercentagesKC2,"Group.1","Pitcher")
pitcherpercentagesKC2$pitch_type='KC'

pitcherpercentagesIN2=with(baseball,aggregate(IN,list(PitcherName,stand),mean))
setnames(pitcherpercentagesIN2,"Group.2","RealHand")
setnames(pitcherpercentagesIN2,"x","Percentage")
setnames(pitcherpercentagesIN2,"Group.1","Pitcher")
pitcherpercentagesIN2$pitch_type='IN'

pitcherpercentagesPO2=with(baseball,aggregate(PO,list(PitcherName,stand),mean))
setnames(pitcherpercentagesPO2,"Group.2","RealHand")
setnames(pitcherpercentagesPO2,"x","Percentage")
setnames(pitcherpercentagesPO2,"Group.1","Pitcher")
pitcherpercentagesPO2$pitch_type='PO'

pitcherpercentagesUN2=with(baseball,aggregate(UN,list(PitcherName,stand),mean))
setnames(pitcherpercentagesUN2,"Group.2","RealHand")
setnames(pitcherpercentagesUN2,"x","Percentage")
setnames(pitcherpercentagesUN2,"Group.1","Pitcher")
pitcherpercentagesUN2$pitch_type='UN'

pitcherpercentagesEP2=with(baseball,aggregate(EP,list(PitcherName,stand),mean))
setnames(pitcherpercentagesEP2,"Group.2","RealHand")
setnames(pitcherpercentagesEP2,"x","Percentage")
setnames(pitcherpercentagesEP2,"Group.1","Pitcher")
pitcherpercentagesEP2$pitch_type='EP'

pitcherpercentagesSC2=with(baseball,aggregate(SC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesSC2,"Group.2","RealHand")
setnames(pitcherpercentagesSC2,"x","Percentage")
setnames(pitcherpercentagesSC2,"Group.1","Pitcher")
pitcherpercentagesSC2$pitch_type='SC'

pitcherpercentagesFC2=with(baseball,aggregate(FC,list(PitcherName,stand),mean))
setnames(pitcherpercentagesFC2,"Group.2","RealHand")
setnames(pitcherpercentagesFC2,"x","Percentage")
setnames(pitcherpercentagesFC2,"Group.1","Pitcher")
pitcherpercentagesFC2$pitch_type='FC'



PitcherBatterFF=merge(PitcherBatter,pitcherpercentagesFF2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSL=merge(PitcherBatter,pitcherpercentagesSL2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterCU=merge(PitcherBatter,pitcherpercentagesCU2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterCH=merge(PitcherBatter,pitcherpercentagesCH2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFT=merge(PitcherBatter,pitcherpercentagesFT2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSI=merge(PitcherBatter,pitcherpercentagesSI2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFS=merge(PitcherBatter,pitcherpercentagesFS2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterKC=merge(PitcherBatter,pitcherpercentagesKC2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterIN=merge(PitcherBatter,pitcherpercentagesIN2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterPO=merge(PitcherBatter,pitcherpercentagesPO2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterUN=merge(PitcherBatter,pitcherpercentagesUN2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterEP=merge(PitcherBatter,pitcherpercentagesEP2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterSC=merge(PitcherBatter,pitcherpercentagesSC2,by=c("Pitcher","pitch_type","RealHand"))
PitcherBatterFC=merge(PitcherBatter,pitcherpercentagesFC2,by=c("Pitcher","pitch_type","RealHand"))


RealPitcherBatter=rbind(PitcherBatterFF,PitcherBatterSL)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterCU)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterCH)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFT)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterSI)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFS)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterKC)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterIN)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterPO)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterUN)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterEP)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterSC)
RealPitcherBatter=rbind(RealPitcherBatter,PitcherBatterFC)


playerpercentagesFF1=with(baseball,aggregate(FF,list(player_name,p_throws),mean))
playerpercentagesFF1$pitch_type='FF'
playerpercentagesSL1=with(baseball,aggregate(SL,list(player_name,p_throws),mean))
playerpercentagesSL1$pitch_type='SL'
playerpercentagesCU1=with(baseball,aggregate(CU,list(player_name,p_throws),mean))
playerpercentagesCU1$pitch_type='CU'
playerpercentagesCH1=with(baseball,aggregate(CH,list(player_name,p_throws),mean))
playerpercentagesCH1$pitch_type='CH'
playerpercentagesFT1=with(baseball,aggregate(FT,list(player_name,p_throws),mean))
playerpercentagesFT1$pitch_type='FT'
playerpercentagesSI1=with(baseball,aggregate(SI,list(player_name,p_throws),mean))
playerpercentagesSI1$pitch_type='SI'
playerpercentagesFS1=with(baseball,aggregate(FS,list(player_name,p_throws),mean))
playerpercentagesFS1$pitch_type='FS'
playerpercentagesKC1=with(baseball,aggregate(KC,list(player_name,p_throws),mean))
playerpercentagesKC1$pitch_type='KC'
playerpercentagesIN1=with(baseball,aggregate(IN,list(player_name,p_throws),mean))
playerpercentagesIN1$pitch_type='IN'
playerpercentagesPO1=with(baseball,aggregate(PO,list(player_name,p_throws),mean))
playerpercentagesPO1$pitch_type='PO'
playerpercentagesUN1=with(baseball,aggregate(UN,list(player_name,p_throws),mean))
playerpercentagesUN1$pitch_type='UN'
playerpercentagesEP1=with(baseball,aggregate(EP,list(player_name,p_throws),mean))
playerpercentagesEP1$pitch_type='EP'
playerpercentagesSC1=with(baseball,aggregate(SC,list(player_name,p_throws),mean))
playerpercentagesSC1$pitch_type='SC'
playerpercentagesFC1=with(baseball,aggregate(FC,list(player_name,p_throws),mean))
playerpercentagesFC1$pitch_type='FC'

playerpercentages1=rbind(playerpercentagesFF1,playerpercentagesSL1,playerpercentagesCU1,playerpercentagesCH1,playerpercentagesFT1,playerpercentagesSI1,playerpercentagesFS1,playerpercentagesKC1,playerpercentagesIN1,playerpercentagesPO1,playerpercentagesUN1,playerpercentagesEP1,playerpercentagesSC1,playerpercentagesFC1)
setnames(playerpercentages1,"Group.1","Player")
setnames(playerpercentages1,"Group.2","PitchHand")
setnames(playerpercentages1,"x","HitterPercentage")

RealPitcherBatter=merge(RealPitcherBatter,playerpercentages1,by=c("Player","PitchHand","pitch_type"))
RealPitcherBatter=subset(RealPitcherBatter,HitterPercentage>.08)


is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))

HomeBatterPercentages[is.nan(HomeBatterPercentages)]=0
AwayBatterPercentages[is.nan(AwayBatterPercentages)]=0



#########################works here#######################################

RealPitcherBatterContact=RealPitcherBatter
RealPitcherBatterContactB=RealPitcherBatter
PitcherBatter2=RealPitcherBatter$Pitch_typeHand

kk=subset(baseball,Swing=='1')



pitcherlineup1=subset(RealPitcherBatter,PitcherNumber==1)
extra1PitcherBatter2=pitcherlineup1[,c('Pitch_typeHand',"PitcherNumber")]
DataframePitcherBatter2=as.data.frame(PitcherBatter2)
setnames(DataframePitcherBatter2,"PitcherBatter2","Pitch_typeHand")
extra1PitcherBatter2aa=merge(extra1PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher1=extra1PitcherBatter2aa$Pitch_typeHand
Pitcher1=unique(Pitcher1)
ActualPitcher=unique(pitcherlineup1$Pitcher)
Pitcher1A=as.data.frame(Pitcher1)
Pitcher1B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher1B,"Var1","Pitcher1")
Pitcher1A=merge(Pitcher1A,Pitcher1B,by=c("Pitcher1"))
Pitcher1A=subset(Pitcher1A,Freq > 40)
Pitcher1=as.character(Pitcher1A$Pitcher1)

batterswing1=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing1 <- data.frame(lapply(batterswing1, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher1){ try({batterswing1=rbind(batterswing1,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}





options(warn=0)

batterswing1$Pitcher=ActualPitcher



pitcherlineup2=subset(RealPitcherBatter,PitcherNumber==2)
extra2PitcherBatter2=pitcherlineup2[,c('Pitch_typeHand',"PitcherNumber")]
extra2PitcherBatter2aa=merge(extra2PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher2=extra2PitcherBatter2aa$Pitch_typeHand
Pitcher2=unique(Pitcher2)
ActualPitcher=unique(pitcherlineup2$Pitcher)
Pitcher2A=as.data.frame(Pitcher2)
Pitcher2B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher2B,"Var1","Pitcher2")
Pitcher2A=merge(Pitcher2A,Pitcher2B,by=c("Pitcher2"))
Pitcher2A=subset(Pitcher2A,Freq > 40)
Pitcher2=as.character(Pitcher2A$Pitcher2)

batterswing2=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing2 <- data.frame(lapply(batterswing2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({batterswing2=rbind(batterswing2,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup2,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing2$Pitcher=ActualPitcher


pitcherlineup3=subset(RealPitcherBatter,PitcherNumber==3)
extra3PitcherBatter2=pitcherlineup3[,c('Pitch_typeHand',"PitcherNumber")]
extra3PitcherBatter2aa=merge(extra3PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher3=extra3PitcherBatter2aa$Pitch_typeHand
Pitcher3=unique(Pitcher3)
ActualPitcher=unique(pitcherlineup3$Pitcher)

Pitcher3A=as.data.frame(Pitcher3)
Pitcher3B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher3B,"Var1","Pitcher3")
Pitcher3A=merge(Pitcher3A,Pitcher3B,by=c("Pitcher3"))
Pitcher3A=subset(Pitcher3A,Freq > 40)
Pitcher3=as.character(Pitcher3A$Pitcher3)

batterswing3=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing3 <- data.frame(lapply(batterswing3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({batterswing3=rbind(batterswing3,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing3$Pitcher=ActualPitcher


pitcherlineup4=subset(RealPitcherBatter,PitcherNumber==4)
extra4PitcherBatter2=pitcherlineup4[,c('Pitch_typeHand',"PitcherNumber")]
extra4PitcherBatter2aa=merge(extra4PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher4=extra4PitcherBatter2aa$Pitch_typeHand
Pitcher4=unique(Pitcher4)
ActualPitcher=unique(pitcherlineup4$Pitcher)

Pitcher4A=as.data.frame(Pitcher4)
Pitcher4B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher4B,"Var1","Pitcher4")
Pitcher4A=merge(Pitcher4A,Pitcher4B,by=c("Pitcher4"))
Pitcher4A=subset(Pitcher4A,Freq > 40)
Pitcher4=as.character(Pitcher4A$Pitcher4)

batterswing4=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing4 <- data.frame(lapply(batterswing4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({batterswing4=rbind(batterswing4,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing4$Pitcher=ActualPitcher

pitcherlineup5=subset(RealPitcherBatter,PitcherNumber==5)
extra5PitcherBatter2=pitcherlineup5[,c('Pitch_typeHand',"PitcherNumber")]
extra5PitcherBatter2aa=merge(extra5PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher5=extra5PitcherBatter2aa$Pitch_typeHand
Pitcher5=unique(Pitcher5)
ActualPitcher=unique(pitcherlineup5$Pitcher)

Pitcher5A=as.data.frame(Pitcher5)
Pitcher5B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher5B,"Var1","Pitcher5")
Pitcher5A=merge(Pitcher5A,Pitcher5B,by=c("Pitcher5"))
Pitcher5A=subset(Pitcher5A,Freq > 40)
Pitcher5=as.character(Pitcher5A$Pitcher5)

batterswing5=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing5 <- data.frame(lapply(batterswing5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({batterswing5=rbind(batterswing5,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

batterswing5$Pitcher=ActualPitcher

pitcherlineup6=subset(RealPitcherBatter,PitcherNumber==6)
extra6PitcherBatter2=pitcherlineup6[,c('Pitch_typeHand',"PitcherNumber")]
extra6PitcherBatter2aa=merge(extra6PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher6=extra6PitcherBatter2aa$Pitch_typeHand
Pitcher6=unique(Pitcher6)
ActualPitcher=unique(pitcherlineup6$Pitcher)

Pitcher6A=as.data.frame(Pitcher6)
Pitcher6B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher6B,"Var1","Pitcher6")
Pitcher6A=merge(Pitcher6A,Pitcher6B,by=c("Pitcher6"))
Pitcher6A=subset(Pitcher6A,Freq > 40)
Pitcher6=as.character(Pitcher6A$Pitcher6)

batterswing6=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing6 <- data.frame(lapply(batterswing6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({batterswing6=rbind(batterswing6,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing6$Pitcher=ActualPitcher


pitcherlineup7=subset(RealPitcherBatter,PitcherNumber==7)
extra7PitcherBatter2=pitcherlineup7[,c('Pitch_typeHand',"PitcherNumber")]
extra7PitcherBatter2aa=merge(extra7PitcherBatter2,DataframePitcherBatter2,by="Pitch_typeHand")
Pitcher7=extra7PitcherBatter2aa$Pitch_typeHand
Pitcher7=unique(Pitcher7)
ActualPitcher=unique(pitcherlineup7$Pitcher)

Pitcher7A=as.data.frame(Pitcher7)
Pitcher7B=as.data.frame(table(kk$Pitch_typeHand))
setnames(Pitcher7B,"Var1","Pitcher7")
Pitcher7A=merge(Pitcher7A,Pitcher7B,by=c("Pitcher7"))
Pitcher7A=subset(Pitcher7A,Freq > 40)
Pitcher7=as.character(Pitcher7A$Pitcher7)

batterswing7=data.frame(Pitch_typeHand="Person",Fit=1)
batterswing7 <- data.frame(lapply(batterswing7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({batterswing7=rbind(batterswing7,((c(i,predict(nls(Contact~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(kk,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

batterswing7$Pitcher=ActualPitcher


batterswing=rbind(batterswing1,batterswing2,batterswing3,batterswing4,batterswing5,batterswing6,batterswing7)










batterswing$Fit=ifelse(batterswing$Fit>.92,.8,batterswing$Fit)
batterswing$Fit=ifelse(batterswing$Fit<.5,.5,batterswing$Fit)



ZoneContact=with(kk,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact,"x","averagezone")

kk1=subset(baseball,Swing=='1'& L1=="1")
ZoneContact1=with(kk1,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact1,"x","L1")

kk2=subset(baseball,Swing=='1'& L2=="1")
ZoneContact2=with(kk2,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact2,"x","L2")

kk3=subset(baseball,Swing=='1'& L3=="1")
ZoneContact3=with(kk3,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact3,"x","L3")
kk4=subset(baseball,Swing=='1'& L4=="1")
ZoneContact4=with(kk4,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact4,"x","L4")
kk5=subset(baseball,Swing=='1'& L5=="1")

ZoneContact5=with(kk5,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact5,"x","L5")
kk6=subset(baseball,Swing=='1'& L6=="1")
ZoneContact6=with(kk6,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact6,"x","L6")
kk7=subset(baseball,Swing=='1'& L7=="1")


ZoneContact7=with(kk7,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact7,"x","L7")
kk8=subset(baseball,Swing=='1'& L8=="1")

ZoneContact8=with(kk8,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact8,"x","L8")
kk9=subset(baseball,Swing=='1'& L9=="1")

ZoneContact9=with(kk9,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact9,"x","L9")
kk11=subset(baseball,Swing=='1'& L11=="1")

ZoneContact11=with(kk11,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact11,"x","L11")
kk12=subset(baseball,Swing=='1'& L12=="1")

ZoneContact12=with(kk12,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact12,"x","L12")
kk13=subset(baseball,Swing=='1'& L13=="1")

ZoneContact13=with(kk13,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact13,"x","L13")
kk14=subset(baseball,Swing=='1'& L1=="1")

ZoneContact14=with(kk14,aggregate(Contact,list(player_name,p_throws),mean))
setnames(ZoneContact14,"x","L14")


ZoneContact=merge(ZoneContact,ZoneContact1,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact2,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact3,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact4,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact5,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact6,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact7,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact8,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact9,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact11,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact12,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact13,by=c('Group.1',"Group.2"))
ZoneContact=merge(ZoneContact,ZoneContact14,by=c('Group.1',"Group.2"))

ZoneContact[is.na(ZoneContact)]<-0
ZoneContact$L1=ifelse(ZoneContact$L1==0,0.95*ZoneContact$averagezone,ZoneContact$L1)
ZoneContact$L2=ifelse(ZoneContact$L2==0,0.95*ZoneContact$averagezone,ZoneContact$L2)
ZoneContact$L3=ifelse(ZoneContact$L3==0,0.95*ZoneContact$averagezone,ZoneContact$L3)
ZoneContact$L4=ifelse(ZoneContact$L4==0,0.95*ZoneContact$averagezone,ZoneContact$L4)
ZoneContact$L5=ifelse(ZoneContact$L5==0,0.95*ZoneContact$averagezone,ZoneContact$L5)
ZoneContact$L6=ifelse(ZoneContact$L6==0,0.95*ZoneContact$averagezone,ZoneContact$L6)
ZoneContact$L7=ifelse(ZoneContact$L7==0,0.95*ZoneContact$averagezone,ZoneContact$L7)
ZoneContact$L8=ifelse(ZoneContact$L8==0,0.95*ZoneContact$averagezone,ZoneContact$L8)
ZoneContact$L9=ifelse(ZoneContact$L9==0,0.95*ZoneContact$averagezone,ZoneContact$L9)
ZoneContact$L11=ifelse(ZoneContact$L11==0,0.95*ZoneContact$averagezone,ZoneContact$L11)
ZoneContact$L12=ifelse(ZoneContact$L12==0,0.95*ZoneContact$averagezone,ZoneContact$L12)
ZoneContact$L13=ifelse(ZoneContact$L13==0,0.95*ZoneContact$averagezone,ZoneContact$L13)
ZoneContact$L14=ifelse(ZoneContact$L14==0,0.95*ZoneContact$averagezone,ZoneContact$L14)
setnames(ZoneContact,'Group.1','Player')
setnames(ZoneContact,'Group.2','PitchHand')


#############


CountContact=with(kk,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact,"x","averagecountcontact")

count00=subset(baseball,Swing=='1'& balls=="0" & strikes =="0")
CountContact00=with(count00,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact00,"x","C00")


count10=subset(baseball,Swing=='1'& balls=="1" & strikes =="0")
CountContact10=with(count10,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact10,"x","C10")

count11=subset(baseball,Swing=='1'& balls=="1" & strikes =="1")
CountContact11=with(count11,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact11,"x","C11")

count12=subset(baseball,Swing=='1'& balls=="1" & strikes =="2")
CountContact12=with(count12,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact12,"x","C12")

count20=subset(baseball,Swing=='1'& balls=="2" & strikes =="0")
CountContact20=with(count20,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact20,"x","C20")

count21=subset(baseball,Swing=='1'& balls=="2" & strikes =="1")
CountContact21=with(count21,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact21,"x","C21")

count22=subset(baseball,Swing=='1'& balls=="2" & strikes =="2")
CountContact22=with(count22,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact22,"x","C22")

count30=subset(baseball,Swing=='1'& balls=="3" & strikes =="0")
CountContact30=with(count30,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact30,"x","C30")

count31=subset(baseball,Swing=='1'& balls=="3" & strikes =="1")
CountContact31=with(count31,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact31,"x","C31")

count32=subset(baseball,Swing=='1'& balls=="3" & strikes =="2")
CountContact32=with(count32,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact32,"x","C32")

count01=subset(baseball,Swing=='1'& balls=="0" & strikes =="1")
CountContact01=with(count01,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact01,"x","C01")

count02=subset(baseball,Swing=='1'& balls=="0" & strikes =="2")
CountContact02=with(count02,aggregate(Contact,list(player_name,p_throws),mean))
setnames(CountContact02,"x","C02")



CountContact=merge(CountContact,CountContact00,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact01,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact02,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact10,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact11,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact12,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact20,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact21,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact22,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact30,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact31,by=c('Group.1',"Group.2"))
CountContact=merge(CountContact,CountContact32,by=c('Group.1',"Group.2"))

CountContact[is.na(CountContact)]<-0
CountContact$C00=ifelse(CountContact$C00==0,1*CountContact$averagecountcontact,CountContact$C00)
CountContact$C01=ifelse(CountContact$C01==0,.95*CountContact$averagecountcontact,CountContact$C01)
CountContact$C02=ifelse(CountContact$C02==0,0.90*CountContact$averagecountcontact,CountContact$C02)
CountContact$C10=ifelse(CountContact$C10==0,1*CountContact$averagecountcontact,CountContact$C10)
CountContact$C11=ifelse(CountContact$C11==0,1*CountContact$averagecountcontact,CountContact$C11)
CountContact$C12=ifelse(CountContact$C12==0,.95*CountContact$averagecountcontact,CountContact$C12)
CountContact$C20=ifelse(CountContact$C20==0,1.05*CountContact$averagecountcontact,CountContact$C20)
CountContact$C21=ifelse(CountContact$C21==0,1.02*CountContact$averagecountcontact,CountContact$C21)
CountContact$C22=ifelse(CountContact$C22==0,1*CountContact$averagecountcontact,CountContact$C22)
CountContact$C30=ifelse(CountContact$C30==0,1*CountContact$averagecountcontact,CountContact$C30)
CountContact$C31=ifelse(CountContact$C31==0,1.10*CountContact$averagecountcontact,CountContact$C31)
CountContact$C32=ifelse(CountContact$C32==0,1.10*CountContact$averagecountcontact,CountContact$C32)

setnames(CountContact,'Group.1','Player')
setnames(CountContact,'Group.2','PitchHand')

Contact=merge(ZoneContact,CountContact,by=c("Player","PitchHand"))

RealPitcherBatter=merge(RealPitcherBatter,batterswing,by=c("Pitch_typeHand","Pitcher"))



balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatter=merge(RealPitcherBatter,balls)
setnames(RealPitcherBatter,"y","balls")
RealPitcherBatter=merge(RealPitcherBatter,strikes)
setnames(RealPitcherBatter,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatter=merge(RealPitcherBatter,zone)
setnames(RealPitcherBatter,"y","zone")
RealPitcherBatter=merge(RealPitcherBatter,Contact,by=c("Player","PitchHand"))


RealPitcherBatter$RealL1=RealPitcherBatter$L1/RealPitcherBatter$averagezone
RealPitcherBatter$RealL2=RealPitcherBatter$L2/RealPitcherBatter$averagezone
RealPitcherBatter$RealL3=RealPitcherBatter$L3/RealPitcherBatter$averagezone
RealPitcherBatter$RealL4=RealPitcherBatter$L4/RealPitcherBatter$averagezone
RealPitcherBatter$RealL5=RealPitcherBatter$L5/RealPitcherBatter$averagezone
RealPitcherBatter$RealL6=RealPitcherBatter$L6/RealPitcherBatter$averagezone
RealPitcherBatter$RealL7=RealPitcherBatter$L7/RealPitcherBatter$averagezone
RealPitcherBatter$RealL8=RealPitcherBatter$L8/RealPitcherBatter$averagezone
RealPitcherBatter$RealL9=RealPitcherBatter$L9/RealPitcherBatter$averagezone
RealPitcherBatter$RealL11=RealPitcherBatter$L11/RealPitcherBatter$averagezone
RealPitcherBatter$RealL12=RealPitcherBatter$L12/RealPitcherBatter$averagezone
RealPitcherBatter$RealL13=RealPitcherBatter$L13/RealPitcherBatter$averagezone
RealPitcherBatter$RealL14=RealPitcherBatter$L14/RealPitcherBatter$averagezone

RealPitcherBatter$RealC00=RealPitcherBatter$C00/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC01=RealPitcherBatter$C01/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC02=RealPitcherBatter$C02/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC10=RealPitcherBatter$C10/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC11=RealPitcherBatter$C11/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC12=RealPitcherBatter$C12/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC20=RealPitcherBatter$C20/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC21=RealPitcherBatter$C21/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC22=RealPitcherBatter$C22/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC30=RealPitcherBatter$C30/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC31=RealPitcherBatter$C31/RealPitcherBatter$averagecountcontact
RealPitcherBatter$RealC32=RealPitcherBatter$C32/RealPitcherBatter$averagecountcontact

RealPitcherBatter$Fit=as.numeric(RealPitcherBatter$Fit)
RealPitcherBatter$ZoneFit=ifelse(RealPitcherBatter$zone==1,RealPitcherBatter$Fit*RealPitcherBatter$RealL1,
                                 ifelse(RealPitcherBatter$zone==2,RealPitcherBatter$Fit*RealPitcherBatter$RealL2,
                                        ifelse(RealPitcherBatter$zone==3,RealPitcherBatter$Fit*RealPitcherBatter$RealL3,
                                               ifelse(RealPitcherBatter$zone==4,RealPitcherBatter$Fit*RealPitcherBatter$RealL4,
                                                      ifelse(RealPitcherBatter$zone==5,RealPitcherBatter$Fit*RealPitcherBatter$RealL5,
                                                             ifelse(RealPitcherBatter$zone==6,RealPitcherBatter$Fit*RealPitcherBatter$RealL6,
                                                                    ifelse(RealPitcherBatter$zone==7,RealPitcherBatter$Fit*RealPitcherBatter$RealL7,
                                                                           ifelse(RealPitcherBatter$zone==8,RealPitcherBatter$Fit*RealPitcherBatter$RealL8,
                                                                                  ifelse(RealPitcherBatter$zone==9,RealPitcherBatter$Fit*RealPitcherBatter$RealL9,
                                                                                         ifelse(RealPitcherBatter$zone==11,RealPitcherBatter$Fit*RealPitcherBatter$RealL11,
                                                                                                ifelse(RealPitcherBatter$zone==12,RealPitcherBatter$Fit*RealPitcherBatter$RealL12,
                                                                                                       ifelse(RealPitcherBatter$zone==13,RealPitcherBatter$Fit*RealPitcherBatter$RealL13,
                                                                                                              ifelse(RealPitcherBatter$zone==14,RealPitcherBatter$Fit*RealPitcherBatter$RealL14,.7)))))))))))))


RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC00,
                                  ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC01,
                                         ifelse(RealPitcherBatter$balls==0 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC02,
                                                ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC10,
                                                       ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC11,
                                                              ifelse(RealPitcherBatter$balls==1 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC12,
                                                                     ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC20,
                                                                            ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC21,
                                                                                   ifelse(RealPitcherBatter$balls==2 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC22,
                                                                                          ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==0,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC30,
                                                                                                 ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==1,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC31,
                                                                                                        ifelse(RealPitcherBatter$balls==3 & RealPitcherBatter$strikes==2,RealPitcherBatter$ZoneFit*RealPitcherBatter$RealC32,.9))))))))))))





RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$CountFit>0.97,.97,RealPitcherBatter$CountFit)
RealPitcherBatter$CountFit=ifelse(RealPitcherBatter$CountFit<0.35,.35,RealPitcherBatter$CountFit)


##########################################################################                                 




############################################################################
jj=subset(baseball,Contact=='1')

jj=subset(jj,launch_angle != "null")
jj=subset(jj,launch_angle!="NA")
jj=subset(jj,description!="foul")
jj$launch_angle=as.numeric(jj$launch_angle)
jj$launch_speed=as.numeric(jj$launch_speed)



ActualPitcher=unique(pitcherlineup1$Pitcher)
Pitcher1A1=as.data.frame(Pitcher1)
Pitcher1B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher1B1,"Var1","Pitcher1")
Pitcher1A1=merge(Pitcher1A1,Pitcher1B1,by=c("Pitcher1"))
Pitcher1A1=subset(Pitcher1A1,Freq > 40)
Pitcher1=as.character(Pitcher1A1$Pitcher1)

launch_speed1=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed1 <- data.frame(lapply(launch_speed1, as.character), stringsAsFactors=FALSE)
options(warn=-1)

for(i in Pitcher1){ try({launch_speed1=rbind(launch_speed1,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed1$Pitcher=ActualPitcher

SDFirst=aggregate(jj$launch_speed,by=list(jj$Pitch_typeHand), FUN=sd)

SDSecond=merge(Pitcher1A1,SDFirst, by.x="Pitcher1",by.y="Group.1")

SDThird=SDSecond


ActualPitcher=unique(pitcherlineup2$Pitcher)
Pitcher2A1=as.data.frame(Pitcher2)
Pitcher2B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher2B1,"Var1","Pitcher2")
Pitcher2A1=merge(Pitcher2A1,Pitcher2B1,by=c("Pitcher2"))
Pitcher2A1=subset(Pitcher2A1,Freq > 40)
Pitcher2=as.character(Pitcher2A1$Pitcher2)

launch_speed2=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed2 <- data.frame(lapply(launch_speed2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({launch_speed2=rbind(launch_speed2,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed2$Pitcher=ActualPitcher

SDSecond=merge(Pitcher2A1,SDFirst, by.x="Pitcher2",by.y="Group.1")
setnames(SDSecond,"Pitcher2","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup3$Pitcher)
Pitcher3A1=as.data.frame(Pitcher3)
Pitcher3B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher3B1,"Var1","Pitcher3")
Pitcher3A1=merge(Pitcher3A1,Pitcher3B1,by=c("Pitcher3"))
Pitcher3A1=subset(Pitcher3A1,Freq > 40)
Pitcher3=as.character(Pitcher3A1$Pitcher3)

launch_speed3=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed3 <- data.frame(lapply(launch_speed3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({launch_speed3=rbind(launch_speed3,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed3$Pitcher=ActualPitcher

SDSecond=merge(Pitcher3A1,SDFirst, by.x="Pitcher3",by.y="Group.1")
setnames(SDSecond,"Pitcher3","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup4$Pitcher)
Pitcher4A1=as.data.frame(Pitcher4)
Pitcher4B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher4B1,"Var1","Pitcher4")
Pitcher4A1=merge(Pitcher4A1,Pitcher4B1,by=c("Pitcher4"))
Pitcher4A1=subset(Pitcher4A1,Freq > 40)
Pitcher4=as.character(Pitcher4A1$Pitcher4)

launch_speed4=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed4 <- data.frame(lapply(launch_speed4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({launch_speed4=rbind(launch_speed4,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed4$Pitcher=ActualPitcher

SDSecond=merge(Pitcher4A1,SDFirst, by.x="Pitcher4",by.y="Group.1")
setnames(SDSecond,"Pitcher4","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup5$Pitcher)
Pitcher5A1=as.data.frame(Pitcher5)
Pitcher5B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher5B1,"Var1","Pitcher5")
Pitcher5A1=merge(Pitcher5A1,Pitcher5B1,by=c("Pitcher5"))
Pitcher5A1=subset(Pitcher5A1,Freq > 40)
Pitcher5=as.character(Pitcher5A1$Pitcher5)

launch_speed5=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed5 <- data.frame(lapply(launch_speed5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({launch_speed5=rbind(launch_speed5,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed5$Pitcher=ActualPitcher

SDSecond=merge(Pitcher5A1,SDFirst, by.x="Pitcher5",by.y="Group.1")
setnames(SDSecond,"Pitcher5","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup6$Pitcher)
Pitcher6A1=as.data.frame(Pitcher6)
Pitcher6B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher6B1,"Var1","Pitcher6")
Pitcher6A1=merge(Pitcher6A1,Pitcher6B1,by=c("Pitcher6"))
Pitcher6A1=subset(Pitcher6A1,Freq > 40)
Pitcher6=as.character(Pitcher6A1$Pitcher6)

launch_speed6=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed6 <- data.frame(lapply(launch_speed6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({launch_speed6=rbind(launch_speed6,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed6$Pitcher=ActualPitcher

SDSecond=merge(Pitcher6A1,SDFirst, by.x="Pitcher6",by.y="Group.1")
setnames(SDSecond,"Pitcher6","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

ActualPitcher=unique(pitcherlineup7$Pitcher)
Pitcher7A1=as.data.frame(Pitcher7)
Pitcher7B1=as.data.frame(table(jj$Pitch_typeHand))
setnames(Pitcher7B1,"Var1","Pitcher7")
Pitcher7A1=merge(Pitcher7A1,Pitcher7B1,by=c("Pitcher7"))
Pitcher7A1=subset(Pitcher7A1,Freq > 40)
Pitcher7=as.character(Pitcher7A1$Pitcher7)

launch_speed7=data.frame(Pitch_typeHand="Person",launchspeedFit=1)
launch_speed7 <- data.frame(lapply(launch_speed7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({launch_speed7=rbind(launch_speed7,((c(i,predict(nls(launch_speed~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_speed7$Pitcher=ActualPitcher

SDSecond=merge(Pitcher7A1,SDFirst, by.x="Pitcher7",by.y="Group.1")
setnames(SDSecond,"Pitcher7","Pitcher1")
SDThird=rbind(SDThird,SDSecond)

SDThird=unique(SDThird)
SDFourth=rbind(SDThird,SDFourth)
setnames(SDFourth,"Pitcher1","pitch_typehand")

launch_speed=rbind(launch_speed1,launch_speed2,launch_speed3,launch_speed4,launch_speed5,launch_speed6,launch_speed7)

launch_speed$launch_speedLower=1
launch_speed$launch_speedHigher=200


launch_speed=launch_speed[,c("Pitch_typeHand","launchspeedFit","launch_speedLower","launch_speedHigher","Pitcher")]















#####################





ZoneContactA=with(jj,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContactA,"x","averagezone")

jj1=subset(jj,Contact=='1'& L1=="1")
ZoneContact1A=with(jj1,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact1A,"x","L1")

jj2=subset(jj,Contact=='1'& L2=="1")
ZoneContact2A=with(jj2,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact2A,"x","L2")

jj3=subset(jj,Contact=='1'& L3=="1")
ZoneContact3A=with(jj3,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact3A,"x","L3")
jj4=subset(jj,Contact=='1'& L4=="1")
ZoneContact4A=with(jj4,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact4A,"x","L4")
jj5=subset(jj,Contact=='1'& L5=="1")

ZoneContact5A=with(jj5,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact5A,"x","L5")
jj6=subset(jj,Contact=='1'& L6=="1")
ZoneContact6A=with(jj6,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact6A,"x","L6")
jj7=subset(jj,Contact=='1'& L7=="1")


ZoneContact7A=with(jj7,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact7A,"x","L7")
jj8=subset(jj,Contact=='1'& L8=="1")

ZoneContact8A=with(jj8,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact8A,"x","L8")
jj9=subset(jj,Contact=='1'& L9=="1")

ZoneContact9A=with(jj9,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact9A,"x","L9")
jj11=subset(jj,Contact=='1'& L11=="1")

ZoneContact11A=with(jj11,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact11A,"x","L11")
jj12=subset(jj,Contact=='1'& L12=="1")

ZoneContact12A=with(jj12,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact12A,"x","L12")
jj13=subset(jj,Contact=='1'& L13=="1")

ZoneContact13A=with(jj13,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact13A,"x","L13")
jj14=subset(jj,Contact=='1'& L1=="1")

ZoneContact14A=with(jj14,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(ZoneContact14A,"x","L14")



ZoneContactA=merge(ZoneContactA,ZoneContact1A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact2A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact3A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact4A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact5A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact6A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact7A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact8A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact9A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact11A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact12A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact13A,by=c('Group.1',"Group.2"))
ZoneContactA=merge(ZoneContactA,ZoneContact14A,by=c('Group.1',"Group.2"))

ZoneContactA[is.na(ZoneContactA)]<-0
ZoneContactA$L1=ifelse(ZoneContactA$L1==0,1.0434*ZoneContactA$averagezone,ZoneContactA$L1)
ZoneContactA$L2=ifelse(ZoneContactA$L2==0,1.0592544*ZoneContactA$averagezone,ZoneContactA$L2)
ZoneContactA$L3=ifelse(ZoneContactA$L3==0,0.9868112*ZoneContactA$averagezone,ZoneContactA$L3)
ZoneContactA$L4=ifelse(ZoneContactA$L4==0,1.0789315*ZoneContactA$averagezone,ZoneContactA$L4)
ZoneContactA$L5=ifelse(ZoneContactA$L5==0,1.1102549*ZoneContactA$averagezone,ZoneContactA$L5)
ZoneContactA$L6=ifelse(ZoneContactA$L6==0,1.1157934*ZoneContactA$averagezone,ZoneContactA$L6)
ZoneContactA$L7=ifelse(ZoneContactA$L7==0,0.9487843*ZoneContactA$averagezone,ZoneContactA$L7)
ZoneContactA$L8=ifelse(ZoneContactA$L8==0,1.0132233*ZoneContactA$averagezone,ZoneContactA$L8)
ZoneContactA$L9=ifelse(ZoneContactA$L9==0,0.9304815*ZoneContactA$averagezone,ZoneContactA$L9)
ZoneContactA$L11=ifelse(ZoneContactA$L11==0,.969728*ZoneContactA$averagezone,ZoneContactA$L11)
ZoneContactA$L12=ifelse(ZoneContactA$L12==0,0.9372415*ZoneContactA$averagezone,ZoneContactA$L12)
ZoneContactA$L13=ifelse(ZoneContactA$L13==0,0.748483*ZoneContactA$averagezone,ZoneContactA$L13)
ZoneContactA$L14=ifelse(ZoneContactA$L14==0,0.8235190*ZoneContactA$averagezone,ZoneContactA$L14)
setnames(ZoneContactA,'Group.1','Player')
setnames(ZoneContactA,'Group.2','PitchHand')


#############


CountContactA=with(jj,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContactA,"x","averagecountcontact")

count00A=subset(jj,Contact=='1'& balls=="0" & strikes =="0")
CountContact00A=with(count00A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact00A,"x","C00")


count10A=subset(jj,Contact=='1'& balls=="1" & strikes =="0")
CountContact10A=with(count10A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact10A,"x","C10")

count11A=subset(jj,Contact=='1'& balls=="1" & strikes =="1")
CountContact11A=with(count11A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact11A,"x","C11")

count12A=subset(jj,Contact=='1'& balls=="1" & strikes =="2")
CountContact12A=with(count12A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact12A,"x","C12")

count20A=subset(jj,Contact=='1'& balls=="2" & strikes =="0")
CountContact20A=with(count20A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact20A,"x","C20")

count21A=subset(jj,Contact=='1'& balls=="2" & strikes =="1")
CountContact21A=with(count21A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact21A,"x","C21")

count22A=subset(jj,Contact=='1'& balls=="2" & strikes =="2")
CountContact22A=with(count22A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact22A,"x","C22")

count30A=subset(jj,Contact=='1'& balls=="3" & strikes =="0")
CountContact30A=with(count30A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact30A,"x","C30")

count31A=subset(jj,Contact=='1'& balls=="3" & strikes =="1")
CountContact31A=with(count31A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact31A,"x","C31")

count32A=subset(jj,Contact=='1'& balls=="3" & strikes =="2")
CountContact32A=with(count32A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact32A,"x","C32")

count01A=subset(jj,Contact=='1'& balls=="0" & strikes =="1")
CountContact01A=with(count01A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact01A,"x","C01")

count02A=subset(jj,Contact=='1'& balls=="0" & strikes =="2")
CountContact02A=with(count02A,aggregate(launch_speed,list(player_name,p_throws),mean))
setnames(CountContact02A,"x","C02")



CountContactA=merge(CountContactA,CountContact00A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact01A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact02A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact10A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact11A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact12A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact20A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact21A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact22A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact30A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact31A,by=c('Group.1',"Group.2"))
CountContactA=merge(CountContactA,CountContact32A,by=c('Group.1',"Group.2"))

CountContactA[is.na(CountContactA)]<-0
CountContactA$C00=ifelse(CountContactA$C00==0,1.0054962*CountContactA$averagecountcontact,CountContactA$C00)
CountContactA$C01=ifelse(CountContactA$C01==0,.9873316*CountContactA$averagecountcontact,CountContactA$C01)
CountContactA$C02=ifelse(CountContactA$C02==0,0.9457973*CountContactA$averagecountcontact,CountContactA$C02)
CountContactA$C10=ifelse(CountContactA$C10==0,1.0055785*CountContactA$averagecountcontact,CountContactA$C10)
CountContactA$C11=ifelse(CountContactA$C11==0,.9977059*CountContactA$averagecountcontact,CountContactA$C11)
CountContactA$C12=ifelse(CountContactA$C12==0,.9630723*CountContactA$averagecountcontact,CountContactA$C12)
CountContactA$C20=ifelse(CountContactA$C20==0,1.0476564*CountContactA$averagecountcontact,CountContactA$C20)
CountContactA$C21=ifelse(CountContactA$C21==0,1.0274699*CountContactA$averagecountcontact,CountContactA$C21)
CountContactA$C22=ifelse(CountContactA$C22==0,.9986828*CountContactA$averagecountcontact,CountContactA$C22)
CountContactA$C30=ifelse(CountContactA$C30==0,1.0624326*CountContactA$averagecountcontact,CountContactA$C30)
CountContactA$C31=ifelse(CountContactA$C31==0,1.1139230*CountContactA$averagecountcontact,CountContactA$C31)
CountContactA$C32=ifelse(CountContactA$C32==0,1.0513556*CountContactA$averagecountcontact,CountContactA$C32)

setnames(CountContactA,'Group.1','Player')
setnames(CountContactA,'Group.2','PitchHand')

ContactA=merge(ZoneContactA,CountContactA,by=c("Player","PitchHand"))


RealPitcherBatterContact=merge(RealPitcherBatterContact,launch_speed,by=c("Pitch_typeHand","Pitcher"))


balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatterContact=merge(RealPitcherBatterContact,balls)
setnames(RealPitcherBatterContact,"y","balls")
RealPitcherBatterContact=merge(RealPitcherBatterContact,strikes)
setnames(RealPitcherBatterContact,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatterContact=merge(RealPitcherBatterContact,zone)
setnames(RealPitcherBatterContact,"y","zone")
RealPitcherBatterContact=merge(RealPitcherBatterContact,ContactA,by=c("Player","PitchHand"))


RealPitcherBatterContact$RealL1=RealPitcherBatterContact$L1-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL2=RealPitcherBatterContact$L2-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL3=RealPitcherBatterContact$L3-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL4=RealPitcherBatterContact$L4-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL5=RealPitcherBatterContact$L5-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL6=RealPitcherBatterContact$L6-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL7=RealPitcherBatterContact$L7-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL8=RealPitcherBatterContact$L8-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL9=RealPitcherBatterContact$L9-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL11=RealPitcherBatterContact$L11-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL12=RealPitcherBatterContact$L12-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL13=RealPitcherBatterContact$L13-RealPitcherBatterContact$averagezone
RealPitcherBatterContact$RealL14=RealPitcherBatterContact$L14-RealPitcherBatterContact$averagezone

RealPitcherBatterContact$RealC00=RealPitcherBatterContact$C00-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC01=RealPitcherBatterContact$C01-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC02=RealPitcherBatterContact$C02-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC10=RealPitcherBatterContact$C10-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC11=RealPitcherBatterContact$C11-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC12=RealPitcherBatterContact$C12-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC20=RealPitcherBatterContact$C20-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC21=RealPitcherBatterContact$C21-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC22=RealPitcherBatterContact$C22-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC30=RealPitcherBatterContact$C30-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC31=RealPitcherBatterContact$C31-RealPitcherBatterContact$averagecountcontact
RealPitcherBatterContact$RealC32=RealPitcherBatterContact$C32-RealPitcherBatterContact$averagecountcontact

RealPitcherBatterContact$launchspeedFit=as.numeric(RealPitcherBatterContact$launchspeedFit)
RealPitcherBatterContact$ZoneLaunchspeedFit =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL1,
                                                    ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL2,
                                                           ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL3,
                                                                  ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL4,
                                                                         ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL5,
                                                                                ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL6,
                                                                                       ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL7,
                                                                                              ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL8,
                                                                                                     ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL9,
                                                                                                            ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL11,
                                                                                                                   ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL12,
                                                                                                                          ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL13,
                                                                                                                                 ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launchspeedFit +RealPitcherBatterContact$RealL14,.7)))))))))))))





RealPitcherBatterContact$RealLaunchSpeed=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC00,
                                                ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC01,
                                                       ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC02,
                                                              ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC10,
                                                                     ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC11,
                                                                            ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC12,
                                                                                   ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC20,
                                                                                          ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC21,
                                                                                                 ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC22,
                                                                                                        ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC30,
                                                                                                               ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC31,
                                                                                                                      ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunchspeedFit+RealPitcherBatterContact$RealC32,.9))))))))))))












RealPitcherBatterContact$launch_speedLower=as.numeric(RealPitcherBatterContact$launch_speedLower)
RealPitcherBatterContact$ZoneLaunch_speedLower =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL1,
                                                       ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL2,
                                                              ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL3,
                                                                     ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL4,
                                                                            ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL5,
                                                                                   ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL6,
                                                                                          ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL7,
                                                                                                 ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL8,
                                                                                                        ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL9,
                                                                                                               ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL11,
                                                                                                                      ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL12,
                                                                                                                             ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL13,
                                                                                                                                    ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launch_speedLower +RealPitcherBatterContact$RealL14,.7)))))))))))))


RealPitcherBatterContact$RealLaunch_speedLower=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC00,
                                                      ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC01,
                                                             ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC02,
                                                                    ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC10,
                                                                           ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC11,
                                                                                  ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC12,
                                                                                         ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC20,
                                                                                                ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC21,
                                                                                                       ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC22,
                                                                                                              ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC30,
                                                                                                                     ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC31,
                                                                                                                            ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedLower+RealPitcherBatterContact$RealC32,.9))))))))))))




RealPitcherBatterContact$launch_speedHigher=as.numeric(RealPitcherBatterContact$launch_speedHigher)
RealPitcherBatterContact$ZoneLaunch_speedHigher =ifelse(RealPitcherBatterContact$zone==1,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL1,
                                                        ifelse(RealPitcherBatterContact$zone==2,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL2,
                                                               ifelse(RealPitcherBatterContact$zone==3,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL3,
                                                                      ifelse(RealPitcherBatterContact$zone==4,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL4,
                                                                             ifelse(RealPitcherBatterContact$zone==5,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL5,
                                                                                    ifelse(RealPitcherBatterContact$zone==6,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL6,
                                                                                           ifelse(RealPitcherBatterContact$zone==7,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL7,
                                                                                                  ifelse(RealPitcherBatterContact$zone==8,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL8,
                                                                                                         ifelse(RealPitcherBatterContact$zone==9,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL9,
                                                                                                                ifelse(RealPitcherBatterContact$zone==11,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL11,
                                                                                                                       ifelse(RealPitcherBatterContact$zone==12,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL12,
                                                                                                                              ifelse(RealPitcherBatterContact$zone==13,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL13,
                                                                                                                                     ifelse(RealPitcherBatterContact$zone==14,RealPitcherBatterContact$launch_speedHigher +RealPitcherBatterContact$RealL14,.7)))))))))))))


RealPitcherBatterContact$RealLaunch_speedHigher=ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC00,
                                                       ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC01,
                                                              ifelse(RealPitcherBatterContact$balls==0 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC02,
                                                                     ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC10,
                                                                            ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC11,
                                                                                   ifelse(RealPitcherBatterContact$balls==1 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC12,
                                                                                          ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC20,
                                                                                                 ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC21,
                                                                                                        ifelse(RealPitcherBatterContact$balls==2 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC22,
                                                                                                               ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==0,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC30,
                                                                                                                      ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==1,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC31,
                                                                                                                             ifelse(RealPitcherBatterContact$balls==3 & RealPitcherBatterContact$strikes==2,RealPitcherBatterContact$ZoneLaunch_speedHigher+RealPitcherBatterContact$RealC32,.9))))))))))))









#####################################launch_angle#################################################################

ActualPitcher=unique(pitcherlineup1$Pitcher)


launch_angle1=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle1 <- data.frame(lapply(launch_angle1, as.character), stringsAsFactors=FALSE)
options(warn=-1)

for(i in Pitcher1){ try({launch_angle1=rbind(launch_angle1,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup1,Pitch_typeHand==i))))))},silent=T)}



options(warn=0)

launch_angle1$Pitcher=ActualPitcher




ActualPitcher=unique(pitcherlineup2$Pitcher)


launch_angle2=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle2 <- data.frame(lapply(launch_angle2, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher2){ try({launch_angle2=rbind(launch_angle2,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup2,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle2$Pitcher=ActualPitcher



ActualPitcher=unique(pitcherlineup3$Pitcher)


launch_angle3=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle3 <- data.frame(lapply(launch_angle3, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher3){ try({launch_angle3=rbind(launch_angle3,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup3,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle3$Pitcher=ActualPitcher



ActualPitcher=unique(pitcherlineup4$Pitcher)


launch_angle4=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle4 <- data.frame(lapply(launch_angle4, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher4){ try({launch_angle4=rbind(launch_angle4,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup4,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle4$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup5$Pitcher)


launch_angle5=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle5 <- data.frame(lapply(launch_angle5, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher5){ try({launch_angle5=rbind(launch_angle5,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup5,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle5$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup6$Pitcher)


launch_angle6=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle6 <- data.frame(lapply(launch_angle6, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher6){ try({launch_angle6=rbind(launch_angle6,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup6,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle6$Pitcher=ActualPitcher

ActualPitcher=unique(pitcherlineup7$Pitcher)


launch_angle7=data.frame(Pitch_typeHand="Person",launch_angleFit=1)
launch_angle7 <- data.frame(lapply(launch_angle7, as.character), stringsAsFactors=FALSE)
options(warn=-1)


for(i in Pitcher7){ try({launch_angle7=rbind(launch_angle7,((c(i,predict(nls(launch_angle~effective_speed*a + break_angle*b + break_length*c + spin_rate*d,data=subset(jj,Pitch_typeHand==i),start=c(a=0,b=0,c=0,d=0)),newdata=subset(pitcherlineup7,Pitch_typeHand==i))))))},silent=T)}


options(warn=0)

launch_angle7$Pitcher=ActualPitcher

launch_angle=rbind(launch_angle1,launch_angle2,launch_angle3,launch_angle4,launch_angle5,launch_angle6,launch_angle7)

launch_angle$launch_angleLower=-100
launch_angle$launch_angleHigher=100


launch_angle=launch_angle[,c("Pitch_typeHand","launch_angleFit","launch_angleLower","launch_angleHigher","Pitcher")]




launch_speed$launch_speedLower=ifelse(launch_speed$launch_speedLower=="NaN",0,launch_speed$launch_speedLower)
launch_speed$launch_speedHigher=ifelse(launch_speed$launch_speedHigher=="NaN",0,launch_speed$launch_speedHigher)
launch_angle$launch_angleLower=ifelse(launch_angle$launch_angleLower=="NaN",0,launch_angle$launch_angleLower)
launch_angle$launch_angleHigher=ifelse(launch_angle$launch_angleHigher=="NaN",0,launch_angle$launch_angleHigher)

launch_speed$launch_speedLower=ifelse(launch_speed$launch_speedLower==0,launch_speed$launchspeedFit,launch_speed$launch_speedLower)
launch_speed$launch_speedHigher=ifelse(launch_speed$launch_speedHigher==0,launch_speed$launchspeedFit,launch_speed$launch_speedHigher)
launch_angle$launch_angleLower=ifelse(launch_angle$launch_angleLower==0,launch_angle$launch_angleFit,launch_angle$launch_angleLower)
launch_angle$launch_angleHigher=ifelse(launch_angle$launch_angleHigher==0,launch_angle$launch_angleFit,launch_angle$launch_angleHigher)



ZoneContactB=with(jj,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContactB,"x","averagezone")

jj1B=subset(jj,Contact=='1'& L1=="1")
jj1B$launch_angle=as.numeric(jj1B$launch_angle)
ZoneContact1B=with(jj1B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact1B,"x","L1")

jj2B=subset(jj,Contact=='1'& L2=="1")
jj2B$launch_angle=as.numeric(jj2B$launch_angle)
ZoneContact2B=with(jj2B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact2B,"x","L2")

jj3B=subset(jj,Contact=='1'& L3=="1")
jj3B$launch_angle=as.numeric(jj3B$launch_angle)
ZoneContact3B=with(jj3B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact3B,"x","L3")

jj4B=subset(jj,Contact=='1'& L4=="1")
jj4B$launch_angle=as.numeric(jj4B$launch_angle)
ZoneContact4B=with(jj4B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact4B,"x","L4")

jj5B=subset(jj,Contact=='1'& L5=="1")
jj5B$launch_angle=as.numeric(jj5B$launch_angle)
ZoneContact5B=with(jj5B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact5B,"x","L5")

jj6B=subset(jj,Contact=='1'& L6=="1")
jj6B$launch_angle=as.numeric(jj6B$launch_angle)
ZoneContact6B=with(jj6B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact6B,"x","L6")

jj7B=subset(jj,Contact=='1'& L7=="1")
jj7B$launch_angle=as.numeric(jj7B$launch_angle)

ZoneContact7B=with(jj7B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact7B,"x","L7")

jj8B=subset(jj,Contact=='1'& L8=="1")
jj8B$launch_angle=as.numeric(jj8B$launch_angle)

ZoneContact8B=with(jj8B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact8B,"x","L8")

jj9B=subset(jj,Contact=='1'& L9=="1")
jj9B$launch_angle=as.numeric(jj9B$launch_angle)

ZoneContact9B=with(jj9B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact9B,"x","L9")

jj11B=subset(jj,Contact=='1'& L11=="1")
jj11B$launch_angle=as.numeric(jj11B$launch_angle)

ZoneContact11B=with(jj11B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact11B,"x","L11")

jj12B=subset(jj,Contact=='1'& L12=="1")
jj12B$launch_angle=as.numeric(jj12B$launch_angle)

ZoneContact12B=with(jj12B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact12B,"x","L12")

jj13B=subset(jj,Contact=='1'& L13=="1")
jj13B$launch_angle=as.numeric(jj13B$launch_angle)

ZoneContact13B=with(jj13B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact13B,"x","L13")

jj14B=subset(jj,Contact=='1'& L14=="1")
jj14B$launch_angle=as.numeric(jj14B$launch_angle)

ZoneContact14B=with(jj14B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(ZoneContact14B,"x","L14")



ZoneContactB=merge(ZoneContactB,ZoneContact1B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact2B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact3B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact4B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact5B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact6B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact7B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact8B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact9B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact11B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact12B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact13B,by=c('Group.1',"Group.2"))
ZoneContactB=merge(ZoneContactB,ZoneContact14B,by=c('Group.1',"Group.2"))

ZoneContactB[is.na(ZoneContactB)]<-0
ZoneContactB$L1=ifelse(ZoneContactB$L1==0,1.0434*ZoneContactB$averagezone,ZoneContactB$L1)
ZoneContactB$L2=ifelse(ZoneContactB$L2==0,1.0592544*ZoneContactB$averagezone,ZoneContactB$L2)
ZoneContactB$L3=ifelse(ZoneContactB$L3==0,0.9868112*ZoneContactB$averagezone,ZoneContactB$L3)
ZoneContactB$L4=ifelse(ZoneContactB$L4==0,1.0789315*ZoneContactB$averagezone,ZoneContactB$L4)
ZoneContactB$L5=ifelse(ZoneContactB$L5==0,1.1102549*ZoneContactB$averagezone,ZoneContactB$L5)
ZoneContactB$L6=ifelse(ZoneContactB$L6==0,1.1157934*ZoneContactB$averagezone,ZoneContactB$L6)
ZoneContactB$L7=ifelse(ZoneContactB$L7==0,0.9487843*ZoneContactB$averagezone,ZoneContactB$L7)
ZoneContactB$L8=ifelse(ZoneContactB$L8==0,1.0132233*ZoneContactB$averagezone,ZoneContactB$L8)
ZoneContactB$L9=ifelse(ZoneContactB$L9==0,0.9304815*ZoneContactB$averagezone,ZoneContactB$L9)
ZoneContactB$L11=ifelse(ZoneContactB$L11==0,.969728*ZoneContactB$averagezone,ZoneContactB$L11)
ZoneContactB$L12=ifelse(ZoneContactB$L12==0,0.9372415*ZoneContactB$averagezone,ZoneContactB$L12)
ZoneContactB$L13=ifelse(ZoneContactB$L13==0,0.748483*ZoneContactB$averagezone,ZoneContactB$L13)
ZoneContactB$L14=ifelse(ZoneContactB$L14==0,0.8235190*ZoneContactB$averagezone,ZoneContactB$L14)



#############


CountContactB=with(jj,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContactB,"x","averagecountcontact")

count00B=subset(jj,Contact=='1'& balls=="0" & strikes =="0")
CountContact00B=with(count00B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact00B,"x","C00")


count10B=subset(jj,Contact=='1'& balls=="1" & strikes =="0")
CountContact10B=with(count10B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact10B,"x","C10")

count11B=subset(jj,Contact=='1'& balls=="1" & strikes =="1")
CountContact11B=with(count11B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact11B,"x","C11")

count12B=subset(jj,Contact=='1'& balls=="1" & strikes =="2")
CountContact12B=with(count12B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact12B,"x","C12")

count20B=subset(jj,Contact=='1'& balls=="2" & strikes =="0")
CountContact20B=with(count20B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact20B,"x","C20")

count21B=subset(jj,Contact=='1'& balls=="2" & strikes =="1")
CountContact21B=with(count21B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact21B,"x","C21")

count22B=subset(jj,Contact=='1'& balls=="2" & strikes =="2")
CountContact22B=with(count22B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact22B,"x","C22")

count30B=subset(jj,Contact=='1'& balls=="3" & strikes =="0")
CountContact30B=with(count30B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact30B,"x","C30")

count31B=subset(jj,Contact=='1'& balls=="3" & strikes =="1")
CountContact31B=with(count31B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact31B,"x","C31")

count32B=subset(jj,Contact=='1'& balls=="3" & strikes =="2")
CountContact32B=with(count32B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact32B,"x","C32")

count01B=subset(jj,Contact=='1'& balls=="0" & strikes =="1")
CountContact01B=with(count01B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact01B,"x","C01")

count02B=subset(jj,Contact=='1'& balls=="0" & strikes =="2")
CountContact02B=with(count02B,aggregate(launch_angle,list(player_name,p_throws),mean))
setnames(CountContact02B,"x","C02")



CountContactB=merge(CountContactB,CountContact00B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact01B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact02B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact10B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact11B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact12B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact20B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact21B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact22B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact30B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact31B,by=c('Group.1',"Group.2"))
CountContactB=merge(CountContactB,CountContact32B,by=c('Group.1',"Group.2"))

CountContactB[is.na(CountContactB)]<-0
CountContactB$C00=ifelse(CountContactB$C00==0,1.0054962*CountContactB$averagecountcontact,CountContactB$C00)
CountContactB$C01=ifelse(CountContactB$C01==0,.9873316*CountContactB$averagecountcontact,CountContactB$C01)
CountContactB$C02=ifelse(CountContactB$C02==0,0.9457973*CountContactB$averagecountcontact,CountContactB$C02)
CountContactB$C10=ifelse(CountContactB$C10==0,1.0055785*CountContactB$averagecountcontact,CountContactB$C10)
CountContactB$C11=ifelse(CountContactB$C11==0,.9977059*CountContactB$averagecountcontact,CountContactB$C11)
CountContactB$C12=ifelse(CountContactB$C12==0,.9630723*CountContactB$averagecountcontact,CountContactB$C12)
CountContactB$C20=ifelse(CountContactB$C20==0,1.0476564*CountContactB$averagecountcontact,CountContactB$C20)
CountContactB$C21=ifelse(CountContactB$C21==0,1.0274699*CountContactB$averagecountcontact,CountContactB$C21)
CountContactB$C22=ifelse(CountContactB$C22==0,.9986828*CountContactB$averagecountcontact,CountContactB$C22)
CountContactB$C30=ifelse(CountContactB$C30==0,1.0624326*CountContactB$averagecountcontact,CountContactB$C30)
CountContactB$C31=ifelse(CountContactB$C31==0,1.1139230*CountContactB$averagecountcontact,CountContactB$C31)
CountContactB$C32=ifelse(CountContactB$C32==0,1.0513556*CountContactB$averagecountcontact,CountContactB$C32)

setnames(CountContactB,'Group.1','Player')
setnames(CountContactB,'Group.2','PitchHand')
setnames(ZoneContactB,'Group.1','Player')
setnames(ZoneContactB,'Group.2','PitchHand')
ContactB=merge(ZoneContactB,CountContactB,by=c("Player","PitchHand"))


RealPitcherBatterContactB=merge(RealPitcherBatterContactB,launch_angle,by=c("Pitch_typeHand","Pitcher"))


balls=c(0,1,2,3)
strikes=c(0,1,2)
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,balls)
setnames(RealPitcherBatterContactB,"y","balls")
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,strikes)
setnames(RealPitcherBatterContactB,"y","strikes")
zone=c(1,2,3,4,5,6,7,8,9,11,12,13,14)
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,zone)
setnames(RealPitcherBatterContactB,"y","zone")
#######righthere#######
RealPitcherBatterContactB=merge(RealPitcherBatterContactB,ContactB,by=c("Player","PitchHand"))



RealPitcherBatterContactB$RealL1=(RealPitcherBatterContactB$L1 + 65)/(RealPitcherBatterContactB$averagezone +65)
RealPitcherBatterContactB$RealL2=(RealPitcherBatterContactB$L2 + 65)/(RealPitcherBatterContactB$averagezone +65)
RealPitcherBatterContactB$RealL3=(RealPitcherBatterContactB$L3 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL4=(RealPitcherBatterContactB$L4 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL5=(RealPitcherBatterContactB$L5 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL6=(RealPitcherBatterContactB$L6 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL7=(RealPitcherBatterContactB$L7 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL8=(RealPitcherBatterContactB$L8 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL9=(RealPitcherBatterContactB$L9 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL11=(RealPitcherBatterContactB$L11 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL12=(RealPitcherBatterContactB$L12 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL13=(RealPitcherBatterContactB$L13 + 65)/(RealPitcherBatterContactB$averagezone + 65)
RealPitcherBatterContactB$RealL14=(RealPitcherBatterContactB$L14 + 65)/(RealPitcherBatterContactB$averagezone + 65)

RealPitcherBatterContactB$RealC00=RealPitcherBatterContactB$C00-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC01=RealPitcherBatterContactB$C01-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC02=RealPitcherBatterContactB$C02-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC10=RealPitcherBatterContactB$C10-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC11=RealPitcherBatterContactB$C11-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC12=RealPitcherBatterContactB$C12-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC20=RealPitcherBatterContactB$C20-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC21=RealPitcherBatterContactB$C21-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC22=RealPitcherBatterContactB$C22-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC30=RealPitcherBatterContactB$C30-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC31=RealPitcherBatterContactB$C31-RealPitcherBatterContactB$averagecountcontact
RealPitcherBatterContactB$RealC32=RealPitcherBatterContactB$C32-RealPitcherBatterContactB$averagecountcontact

RealPitcherBatterContactB$launch_angleFit=as.numeric(RealPitcherBatterContactB$launch_angleFit)
RealPitcherBatterContactB$Zonelaunch_angleFit =ifelse(RealPitcherBatterContactB$zone==1,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL1)-65,
                                                      ifelse(RealPitcherBatterContactB$zone==2,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL2)-65,
                                                             ifelse(RealPitcherBatterContactB$zone==3,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL3)-65,
                                                                    ifelse(RealPitcherBatterContactB$zone==4,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL4)-65,
                                                                           ifelse(RealPitcherBatterContactB$zone==5,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL5)-65,
                                                                                  ifelse(RealPitcherBatterContactB$zone==6,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL6)-65,
                                                                                         ifelse(RealPitcherBatterContactB$zone==7,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL7)-65,
                                                                                                ifelse(RealPitcherBatterContactB$zone==8,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL8)-65,
                                                                                                       ifelse(RealPitcherBatterContactB$zone==9,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL9)-65,
                                                                                                              ifelse(RealPitcherBatterContactB$zone==11,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL11)-65,
                                                                                                                     ifelse(RealPitcherBatterContactB$zone==12,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL12)-65,
                                                                                                                            ifelse(RealPitcherBatterContactB$zone==13,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL13)-65,
                                                                                                                                   ifelse(RealPitcherBatterContactB$zone==14,((RealPitcherBatterContactB$launch_angleFit+65) *RealPitcherBatterContactB$RealL14)-65,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angle=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0054962)-65,
                                                  ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*0.9873316)-65,
                                                         ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*0.9457973)-65,
                                                                ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0055785)-65,
                                                                       ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9977059)-65,
                                                                              ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9630723)-65,
                                                                                     ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0476564)-65,
                                                                                            ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0274699)-65,
                                                                                                   ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*.9986828)-65,
                                                                                                          ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0624326)-65,
                                                                                                                 ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.1139230)-65,
                                                                                                                        ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,((RealPitcherBatterContactB$Zonelaunch_angleFit + 65)*1.0513556)-65,.9))))))))))))










RealPitcherBatterContactB$launch_angleLower=as.numeric(RealPitcherBatterContactB$launch_angleLower)
RealPitcherBatterContactB$Zonelaunch_angleLowerFit =ifelse(RealPitcherBatterContactB$zone==1,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL1,
                                                           ifelse(RealPitcherBatterContactB$zone==2,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL2,
                                                                  ifelse(RealPitcherBatterContactB$zone==3,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL3,
                                                                         ifelse(RealPitcherBatterContactB$zone==4,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL4,
                                                                                ifelse(RealPitcherBatterContactB$zone==5,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL5,
                                                                                       ifelse(RealPitcherBatterContactB$zone==6,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL6,
                                                                                              ifelse(RealPitcherBatterContactB$zone==7,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL7,
                                                                                                     ifelse(RealPitcherBatterContactB$zone==8,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL8,
                                                                                                            ifelse(RealPitcherBatterContactB$zone==9,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL9,
                                                                                                                   ifelse(RealPitcherBatterContactB$zone==11,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL11,
                                                                                                                          ifelse(RealPitcherBatterContactB$zone==12,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL12,
                                                                                                                                 ifelse(RealPitcherBatterContactB$zone==13,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL13,
                                                                                                                                        ifelse(RealPitcherBatterContactB$zone==14,RealPitcherBatterContactB$launch_angleLower +RealPitcherBatterContactB$RealL14,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angleLower=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC00,
                                                       ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC01,
                                                              ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC02,
                                                                     ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC10,
                                                                            ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC11,
                                                                                   ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC12,
                                                                                          ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC20,
                                                                                                 ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC21,
                                                                                                        ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC22,
                                                                                                               ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC30,
                                                                                                                      ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC31,
                                                                                                                             ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleLowerFit+RealPitcherBatterContactB$RealC32,.9))))))))))))

RealPitcherBatterContactB$launch_angleHigher=as.numeric(RealPitcherBatterContactB$launch_angleHigher)
RealPitcherBatterContactB$Zonelaunch_angleHigherFit =ifelse(RealPitcherBatterContactB$zone==1,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL1,
                                                            ifelse(RealPitcherBatterContactB$zone==2,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL2,
                                                                   ifelse(RealPitcherBatterContactB$zone==3,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL3,
                                                                          ifelse(RealPitcherBatterContactB$zone==4,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL4,
                                                                                 ifelse(RealPitcherBatterContactB$zone==5,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL5,
                                                                                        ifelse(RealPitcherBatterContactB$zone==6,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL6,
                                                                                               ifelse(RealPitcherBatterContactB$zone==7,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL7,
                                                                                                      ifelse(RealPitcherBatterContactB$zone==8,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL8,
                                                                                                             ifelse(RealPitcherBatterContactB$zone==9,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL9,
                                                                                                                    ifelse(RealPitcherBatterContactB$zone==11,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL11,
                                                                                                                           ifelse(RealPitcherBatterContactB$zone==12,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL12,
                                                                                                                                  ifelse(RealPitcherBatterContactB$zone==13,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL13,
                                                                                                                                         ifelse(RealPitcherBatterContactB$zone==14,RealPitcherBatterContactB$launch_angleHigher +RealPitcherBatterContactB$RealL14,.7)))))))))))))


RealPitcherBatterContactB$Reallaunch_angleHigher=ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC00,
                                                        ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC01,
                                                               ifelse(RealPitcherBatterContactB$balls==0 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC02,
                                                                      ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC10,
                                                                             ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC11,
                                                                                    ifelse(RealPitcherBatterContactB$balls==1 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC12,
                                                                                           ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC20,
                                                                                                  ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC21,
                                                                                                         ifelse(RealPitcherBatterContactB$balls==2 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC22,
                                                                                                                ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==0,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC30,
                                                                                                                       ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==1,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC31,
                                                                                                                              ifelse(RealPitcherBatterContactB$balls==3 & RealPitcherBatterContactB$strikes==2,RealPitcherBatterContactB$Zonelaunch_angleHigherFit+RealPitcherBatterContactB$RealC32,.9))))))))))))






setnames(Awayplayerpercentageszone,"Balls","balls")
setnames(Awayplayerpercentageszone,"Strikes","strikes")



FinalPitcherBatter=RealPitcherBatterContactB[,c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone","Reallaunch_angle","Reallaunch_angleLower","Reallaunch_angleHigher")]
FinalPitcherBattertemp=RealPitcherBatterContact[,c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone","RealLaunchSpeed","RealLaunch_speedLower","RealLaunch_speedHigher")]
FinalPitcherBatter=merge(FinalPitcherBatter,FinalPitcherBattertemp,by=c("Player","PitchHand","Pitch_typeHand","Pitcher","pitch_type","RealHand","Team","MaxPitch","balls","strikes","zone"))





FinalAwayBatterPercentages=AwayBatterPercentages[,c("PitchHand","Balls","Strikes","Pitcher","Player","RealHand","percentageFF","percentageCU","percentageSL","percentageCH","percentageFT","percentageSI","percentageFS","percentageKC","percentageIN","percentagePO","percentageUN","percentageEP","percentageSC","percentageFC")]




PitchThrowType=rbind(PitchThrowType,FinalAwayBatterPercentages)
PitchThrowZone=rbind(PitchThrowZone,AwayBatterPercentageszone)






foul1=foul
cc=subset(baseball,Contact=='1')
cc$foul=ifelse(cc$description=="foul",1,0)
foul=with(cc,aggregate(foul,list(player_name),mean))
setnames(foul,'Group.1',"Player")
setnames(foul,'x',"FoulPercentage")
foul=merge(foul,batterlineup,by="Player")
foul[,3:6]=NULL

foul=rbind(foul,foul1)
Angleone=Angle
jj=subset(jj,hc_x!="null")  
jj=subset(jj,hc_x!="null")
jj$hc_x=as.numeric(jj$hc_x)
jj$hc_y=as.numeric(jj$hc_y)

jj$horangle=(round(tan((jj$hc_x-128)/(208-jj$hc_y))*180/pi*.75))
jj=subset(jj,horangle<45)
jj=subset(jj,horangle>-45)
jj$angle1=ifelse(jj$horangle>=27,1,0)
jj$angle2=ifelse(jj$horangle>=9 & jj$horangle<26.5,1,0)
jj$angle3=ifelse(jj$horangle<8.5 & jj$horangle>-8.5,1,0)
jj$angle4=ifelse(jj$horangle<=-8.5 & jj$horangle>=-27.5,1,0)
jj$angle5=ifelse(jj$horangle<=-27.5,1,0)

Angle1=with(jj,aggregate(angle1,list(player_name),mean))
setnames(Angle1,"x",'Angle1')
Angle2=with(jj,aggregate(angle2,list(player_name),mean))
setnames(Angle2,"x",'Angle2')
Angle3=with(jj,aggregate(angle3,list(player_name),mean))
setnames(Angle3,"x",'Angle3')
Angle4=with(jj,aggregate(angle4,list(player_name),mean))
setnames(Angle4,"x",'Angle4')
Angle5=with(jj,aggregate(angle5,list(player_name),mean))
setnames(Angle5,"x",'Angle5')

Angle=merge(Angle1,Angle2,by="Group.1")
Angle=merge(Angle,Angle3,by="Group.1")
Angle=merge(Angle,Angle4,by="Group.1")
Angle=merge(Angle,Angle5,by="Group.1")
setnames(Angle,"Group.1","Player")
Angle=merge(Angle,batterlineup,by="Player")
Angle[,7:10]=NULL
Angle=rbind(Angle,Angleone)







PitchThrowZone=PitchThrowZone[,c("PitchHand","Balls","Strikes","Pitcher","MaxPitch","Batterhand","Player","RealHand","percentageL1","percentageL2","percentageL3","percentageL4","percentageL5","percentageL6","percentageL7","percentageL8","percentageL9","percentageL11","percentageL12","percentageL13","percentageL14")]
Swing=rbind(Swing,batterdifferences)
ChanceofContact=rbind(ChanceofContact,RealPitcherBatter)
ChanceofContact=ChanceofContact[,c("Player","PitchHand","Pitcher","pitch_type","RealHand","balls","strikes","zone","CountFit")]
hitspeed=rbind(hitspeed,RealPitcherBatterContact)
hitspeed=hitspeed[,c("Player","Pitcher","balls","strikes","zone","pitch_type","RealLaunchSpeed","RealLaunch_speedLower","RealLaunch_speedHigher")]
hitspeed$sd=(hitspeed$RealLaunch_speedHigher-hitspeed$RealLaunchSpeed)/2
launchangle=rbind(launchangle,RealPitcherBatterContactB)
launchangle=launchangle[,c("Player","Pitcher","balls","strikes","zone","pitch_type","Reallaunch_angle","Reallaunch_angleLower","Reallaunch_angleHigher")]

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
hitspeed[is.nan(hitspeed)]=0
hitspeed$RealLaunch_speedLower=ifelse(hitspeed$RealLaunch_speedLower==0,hitspeed$RealLaunch_speedLower-10,hitspeed$RealLaunch_speedLower)
hitspeed$RealLaunch_speedHigher=ifelse(hitspeed$RealLaunch_speedHigher==0,hitspeed$RealLaunch_speedHigher+10,hitspeed$RealLaunch_speedHigher)
launchangle$Reallaunch_angleLower=ifelse(launchangle$Reallaunch_angleLower==launchangle$Reallaunch_angle,launchangle$Reallaunch_angleLower-20,launchangle$Reallaunch_angleLower)
launchangle$Reallaunch_angleHigher=ifelse(launchangle$Reallaunch_angleHigher==launchangle$Reallaunch_angle,launchangle$Reallaunch_angleHigher+20,launchangle$Reallaunch_angleHigher)

launchangle$sd=(launchangle$Reallaunch_angleHigher-launchangle$Reallaunch_angle)/2
hitspeed$sd=16.416468131

launchangle$sd=16.4646546


################################################################stopguess
hitpercentage=read.csv('thisbaseball2.csv',TRUE,',')
CatchProb=read.csv('CatchProb.csv',TRUE,',')
#############################################################






batterlineup=read.csv('baseballlineup.csv',TRUE,',')
batterlineup=merge(batterlineup,CatchProb,by="Player",all=TRUE)
batterlineup=subset(batterlineup,(Team==HomeTeam) | (Team==AwayTeam))
batterlineup$CatchPercent[is.na(batterlineup$CatchPercent)]=0
SpeedScore=read.csv('SpeedScore.csv',TRUE,',')
batterlineup=merge(batterlineup,SpeedScore,by="Player",all=TRUE)
batterlineup=subset(batterlineup,Team==HomeTeam | Team==AwayTeam)
batterlineup$Speed[is.na(batterlineup$Speed)]=0
pitcherlineup=read.csv('pitcherlineup.csv',TRUE,',')
ShiftPercentage=read.csv('ShiftPercentage.csv',TRUE,',')
StolenBaseRunner=read.csv('StolenBaseRunner.csv',TRUE,',')
StolenBaseRunner=merge(batterlineup,StolenBaseRunner,by="Player",all=TRUE)
StolenBaseRunner=subset(StolenBaseRunner,(Team==HomeTeam) | (Team==AwayTeam))
StolenBaseRunner$SBAttempt[is.na(StolenBaseRunner$SBAttempt)]=0
StolenBaseRunner$SB.[is.na(StolenBaseRunner$ SB.)]=0
StolenBaseRunner$SB2nd[is.na(StolenBaseRunner$SB2nd)]=0
StolenBaseRunner$SB3rd[is.na(StolenBaseRunner$SB3rd)]=0
StolenBasePitcher=read.csv('PitcherStolenBase.csv',TRUE,',')
StolenBasePitcher=merge(StolenBasePitcher,pitcherlineup,by='Pitcher', all=TRUE)
StolenBasePitcher=subset(StolenBasePitcher,(Team==HomeTeam) | (Team==AwayTeam))
StolenBasePitcher$SBPercent.AverageAttempt[is.na(StolenBasePitcher$SBPercent.AverageAttempt)]=1
StolenBasePitcher$SBPercent[is.na(StolenBasePitcher$SBPercent)]=1
CatcherStealsPercentages=read.csv('CatcherStealsPercentages.csv',TRUE,',')
Catcher=subset(batterlineup,Position==2)
CatcherStealsPercentages=merge(Catcher,CatcherStealsPercentages,by="Player",all=TRUE)
CatcherStealsPercentages=subset(CatcherStealsPercentages,(Team==HomeTeam) | (Team==AwayTeam))
CatcherStealsPercentages$CatcherSBPercent[is.na(CatcherStealsPercentages$CatcherSBPercent)]=1
StolenBaseDefense=merge(StolenBasePitcher,CatcherStealsPercentages,by='Team')
StolenBaseDefense$PitchHand=NULL
StolenBaseDefense$MaxPitch=NULL
StolenBaseDefense$Hand=NULL
StolenBaseDefense$BattingNumber=NULL
StolenBaseDefense$Position=NULL
StolenBaseDefense$CatchPercent=NULL
StolenBaseDefense$Speed=NULL
setnames(StolenBaseDefense,"Player","Catcher")

StolenBaseRunner$Opponent=ifelse(StolenBaseRunner$Team==HomeTeam,AwayTeam,HomeTeam)
StolenBaseDefense$Opponent=ifelse(StolenBaseDefense$Team==HomeTeam,AwayTeam,HomeTeam)
options(warn=-1)
StolenBase=merge(StolenBaseRunner,StolenBaseDefense,by.x = "Team", by.y = "Opponent")
options(warn=0)
StolenBase[,13]=NULL
StolenBase$realSB2nd=StolenBase$SB2nd*StolenBase$SBPercent.AverageAttempt
StolenBase$realSB3rd=StolenBase$SB3rd*StolenBase$SBPercent.AverageAttempt
StolenBase$RealSBPercent=StolenBase$SB.*StolenBase$SBPercent*StolenBase$CatcherSBPercent
StolenBase$RealSBPercent=ifelse(StolenBase$RealSBPercent>1,1,StolenBase$RealSBPercent)
average=read.csv('average.csv',TRUE,',')
AverageContact=read.csv('AverageContact.csv',TRUE,',')
pitcherlist=pitcherlineup$Pitcher
batterlist=batterlineup$Player
list=merge(pitcherlist,batterlist)
setnames(list,"x","Pitcher")
setnames(list,"y","Player")
average=merge(average,list,by=c("Player","Pitcher"))
AverageContact=merge(AverageContact,list,by=c("Player","Pitcher"))
AverageContact=merge(AverageContact,pitches)
AverageContact=merge(AverageContact,zone1)
balls=as.data.frame(balls)
strikes=as.data.frame(strikes)
AverageContact=merge(AverageContact,balls)
AverageContact=merge(AverageContact,strikes)
RealHandandPitchers=PitchThrowType[,c("Pitcher","Player","RealHand","PitchHand")]
RealHandandPitchers=unique(RealHandandPitchers)
setnames(AverageContact,'c("FF", "CU", "SL", "CH", "FT", "SI", "FS", "KC", "IN", "PO", "UN", "EP", "SC", "FC")',"pitch_type")
setnames(AverageContact,'c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)',"zone")

ChanceofContact=merge(ChanceofContact,AverageContact,by=c("Player","Pitcher","balls","strikes","RealHand","PitchHand","pitch_type","zone"),all=TRUE)
ChanceofContact[is.na(ChanceofContact)]<-0
ChanceofContact$CountFit=ifelse(ChanceofContact$CountFit<.01,ChanceofContact$PlayerContact,ChanceofContact$CountFit)

##############################
average=merge(average,pitches)
average=merge(average,zone1)
average=merge(average,balls)
average=merge(average,strikes)
setnames(average,'c("FF", "CU", "SL", "CH", "FT", "SI", "FS", "KC", "IN", "PO", "UN", "EP", "SC", "FC")',"pitch_type")
setnames(average,'c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)',"zone")





write.csv(PitchThrowType,file="PitchThrowType55.csv")
write.csv(PitchThrowZone,file="PitchThrowZone55.csv")
write.csv(Swing,file="Swing55.csv")
write.csv(ChanceofContact,file="ChanceofContact55.csv")
write.csv(hitspeed,file="hitspeed55.csv")
write.csv(launchangle,file="launchangle55.csv")
write.csv(Angle,file="Angle55.csv")
write.csv(foul,file="foul155.csv")
write.csv(SDFourth,file="SDlaunch.csv")

