rm(list =ls())


options(scipen=999)

library('varhandle')
library('data.table')
library('plyr')
library('dplyr')
library('rvest')
library("splitstackshape")



HomeTeam="Yankees"
AwayTeam="Dodgers"
Stadium="Dodgers"

TotalGames=30
Temperature=64
wind=3
Humidity=82

BaseballTeams=read.csv("BaseballTeams.csv",TRUE,",")

PitchThrowType=read.csv("PitchThrowType55.csv",TRUE,",")
PitchThrowZone=read.csv("PitchThrowZone55.csv",TRUE,",")
Swing=read.csv("Swing55.csv",TRUE,",")
ChanceofContact=read.csv("ChanceofContact55.csv",TRUE,",")
hitspeed=read.csv("hitspeed55.csv",TRUE,",")
launchangle=read.csv("launchangle55.csv",TRUE,",")
Angle=read.csv("Angle55.csv",TRUE,",")
foul=read.csv("foul155.csv",TRUE,",")
SDFourth=read.csv("SDlaunch.csv",TRUE,",")




hitpercentage=read.csv("thisbaseball2.csv",TRUE,",")
batterlineup=read.csv("baseballlineup.csv",TRUE,",")
pitcherlineup=read.csv("pitcherlineup.csv",TRUE,",")
averagepitch=read.csv("averagepitch.csv",TRUE,",")
average=read.csv("average.csv",TRUE,",")
averagecontact=read.csv("AverageContact.csv",TRUE,",")
ParkWallHeights=read.csv("ParkWallHeights.csv",TRUE,",")
ParkFenceDistance=read.csv("ParkFenceDistance.csv",TRUE,",")
CatchProb=read.csv("CatchProb.csv",TRUE,",")
Speed=read.csv("SpeedScore.csv",TRUE,",")
ShiftPercentage=read.csv("ShiftPercentage.csv",TRUE,",")

StolenBaseRunner=read.csv('StolenBaseRunner.csv',TRUE,',')
CatcherStealsPercentages=read.csv('CatcherStealsPercentages.csv',TRUE,',')
ParkFactor=read.csv("ParkFactor.csv",TRUE,",")


Balls=as.data.frame(c(0,1,2,3))
Strikes=as.data.frame(c(0,1,2))
BallsStrikes=merge(Balls,Strikes)
setnames(BallsStrikes,"c(0, 1, 2, 3)","Balls")
setnames(BallsStrikes,"c(0, 1, 2)","Strikes")
zone1=as.data.frame(c(1,2,3,4,5,6,7,8,9,11,12,13,14))
pitches=as.data.frame(c("FF","CU","SL","CH",'FT','SI','FS','KC','IN','PO','UN','EP','SC','FC'))


batterlineupA=subset(batterlineup,Team==AwayTeam)
pitcherlineupA=subset(pitcherlineup,Team==HomeTeam)
pitcherlineupA1=pitcherlineupA[,c("Pitcher","PitchHand","PitcherNumber")]
batterlineupA1=as.data.frame(batterlineupA[,c("Player")])
setnames(batterlineupA1,'batterlineupA[, c("Player")]',"Player")
pitcherlineupA2=as.data.frame(pitcherlineupA$Pitcher)
setnames(pitcherlineupA2,"pitcherlineupA$Pitcher","Pitcher")
pitcherlineupA3=pitcherlineupA[,c("Pitcher","PitchHand","PitcherNumber")]
setnames(pitcherlineupA3,"PitchHand","ExtraPitchHand")
setnames(pitcherlineupA3,"PitcherNumber","ExtraPitcherNumber")
batterlineupA2=batterlineupA[,c("Player","BattingNumber","Hand")]
batterlineupA3=batterlineupA[,c("Player","BattingNumber")]
batterlineupA4=as.data.frame(batterlineupA[,c("Player")])
setnames(batterlineupA4,'batterlineupA[, c("Player")]',"Player")
batterlineupA4=merge(batterlineupA4,zone1)
setnames(batterlineupA4,"c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)","zone")
batterlineupA4=merge(batterlineupA4,pitches)
setnames(batterlineupA4,'c("FF", "CU", "SL", "CH", "FT", "SI", "FS", "KC", "IN", "PO", "UN", "EP", "SC", "FC")',"pitch_type")
batterlineupA4=merge(batterlineupA4,Balls)
batterlineupA4=merge(batterlineupA4,Strikes)
batterlineupA4=merge(batterlineupA4,pitcherlineupA2)
setnames(batterlineupA4,"c(0, 1, 2, 3)","balls")
setnames(batterlineupA4,"c(0, 1, 2)","strikes")

batterlineupB=subset(batterlineup,Team==HomeTeam)
pitcherlineupB=subset(pitcherlineup,Team==AwayTeam)
pitcherlineupB1=pitcherlineupB[,c("Pitcher","PitchHand","PitcherNumber")]
batterlineupB1=as.data.frame(batterlineupB[,c("Player")])
setnames(batterlineupB1,'batterlineupB[, c("Player")]',"Player")
pitcherlineupB2=as.data.frame(pitcherlineupB$Pitcher)
setnames(pitcherlineupB2,"pitcherlineupB$Pitcher","Pitcher")
pitcherlineupB3=pitcherlineupB[,c("Pitcher","PitchHand","PitcherNumber")]
setnames(pitcherlineupB3,"PitchHand","ExtraPitchHand")
setnames(pitcherlineupB3,"PitcherNumber","ExtraPitcherNumber")
batterlineupB2=batterlineupB[,c("Player","BattingNumber","Hand")]
batterlineupB3=batterlineupB[,c("Player","BattingNumber")]
batterlineupB4=as.data.frame(batterlineupB[,c("Player")])
setnames(batterlineupB4,'batterlineupB[, c("Player")]',"Player")
batterlineupB4=merge(batterlineupB4,zone1)
setnames(batterlineupB4,"c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)","zone")
batterlineupB4=merge(batterlineupB4,pitches)
setnames(batterlineupB4,'c("FF", "CU", "SL", "CH", "FT", "SI", "FS", "KC", "IN", "PO", "UN", "EP", "SC", "FC")',"pitch_type")
batterlineupB4=merge(batterlineupB4,Balls)
batterlineupB4=merge(batterlineupB4,Strikes)
batterlineupB4=merge(batterlineupB4,pitcherlineupB2)
setnames(batterlineupB4,"c(0, 1, 2, 3)","balls")
setnames(batterlineupB4,"c(0, 1, 2)","strikes")



PitcherBatterA=merge(batterlineupA1,pitcherlineupA2)
PitcherBatterA=merge(PitcherBatterA,average,by=c("Player","Pitcher"))
PitcherBatterA=PitcherBatterA[,c("Player","Pitcher","RealHand")]

PitcherBatterB=merge(batterlineupB1,pitcherlineupB2)
PitcherBatterB=merge(PitcherBatterB,average,by=c("Player","Pitcher"))
PitcherBatterB=PitcherBatterB[,c("Player","Pitcher","RealHand")]

matchup=merge(BallsStrikes,batterlineupA1)
matchup=merge(matchup,pitcherlineupA2)
matchupA=merge(BallsStrikes,batterlineupA1)
setnames(matchupA,"Balls","balls")
setnames(matchupA,"Strikes","strikes")

matchupB=merge(BallsStrikes,batterlineupB1)
matchupB=merge(matchupB,pitcherlineupB2)
matchupB1=merge(BallsStrikes,batterlineupB1)
setnames(matchupB1,"Balls","balls")
setnames(matchupB1,"Strikes","strikes")

SDFourth$Freq=NULL
SDFourth$X=NULL
setnames(SDFourth,"x","SD")

averagepitch$X=NULL
PitchThrowTypeA=merge(PitchThrowType,pitcherlineupA2,by=c("Pitcher"))
PitchThrowTypeA$X=NULL
PitchThrowTypeA=merge(PitchThrowTypeA,matchup,by=c("Balls","Strikes","Pitcher","Player"),all=TRUE)
PitchThrowTypeA=merge(PitchThrowTypeA,averagepitch,by=c("Pitcher"))
PitchThrowTypeA=unfactor(PitchThrowTypeA)
PitchThrowTypeA[is.na(PitchThrowTypeA)]<-99
PitchThrowTypeA$percentageFF=ifelse(PitchThrowTypeA$percentageFF==99,PitchThrowTypeA$FF,PitchThrowTypeA$percentageFF)
PitchThrowTypeA$percentageCU=ifelse(PitchThrowTypeA$percentageCU==99,PitchThrowTypeA$CU,PitchThrowTypeA$percentageCU)
PitchThrowTypeA$percentageSL=ifelse(PitchThrowTypeA$percentageSL==99,PitchThrowTypeA$SL,PitchThrowTypeA$percentageSL)
PitchThrowTypeA$percentageCH=ifelse(PitchThrowTypeA$percentageCH==99,PitchThrowTypeA$CH,PitchThrowTypeA$percentageCH)
PitchThrowTypeA$percentageFT=ifelse(PitchThrowTypeA$percentageFT==99,PitchThrowTypeA$FT,PitchThrowTypeA$percentageFT)
PitchThrowTypeA$percentageSI=ifelse(PitchThrowTypeA$percentageSI==99,PitchThrowTypeA$SI,PitchThrowTypeA$percentageSI)
PitchThrowTypeA$percentageFS=ifelse(PitchThrowTypeA$percentageFS==99,PitchThrowTypeA$FS,PitchThrowTypeA$percentageFS)
PitchThrowTypeA$percentageKC=ifelse(PitchThrowTypeA$percentageKC==99,PitchThrowTypeA$KC,PitchThrowTypeA$percentageKC)
PitchThrowTypeA$percentageIN=ifelse(PitchThrowTypeA$percentageIN==99,PitchThrowTypeA$IN,PitchThrowTypeA$percentageIN)
PitchThrowTypeA$percentagePO=ifelse(PitchThrowTypeA$percentagePO==99,PitchThrowTypeA$PO,PitchThrowTypeA$percentagePO)
PitchThrowTypeA$percentageUN=ifelse(PitchThrowTypeA$percentageUN==99,PitchThrowTypeA$UN,PitchThrowTypeA$percentageUN)
PitchThrowTypeA$percentageEP=ifelse(PitchThrowTypeA$percentageEP==99,PitchThrowTypeA$EP,PitchThrowTypeA$percentageEP)
PitchThrowTypeA$percentageSC=ifelse(PitchThrowTypeA$percentageSC==99,PitchThrowTypeA$SC,PitchThrowTypeA$percentageSC)
PitchThrowTypeA$percentageFC=ifelse(PitchThrowTypeA$percentageFC==99,PitchThrowTypeA$FC,PitchThrowTypeA$percentageFC)
PitchThrowTypeA=merge(PitchThrowTypeA,pitcherlineupA3,by="Pitcher")
PitchThrowTypeA$ExtraPitchHand=as.character(PitchThrowTypeA$ExtraPitchHand)
PitchThrowTypeA$PitchHand=ifelse(PitchThrowTypeA$PitchHand==99,PitchThrowTypeA$ExtraPitchHand,PitchThrowTypeA$PitchHand)
setnames(PitchThrowTypeA,"ExtraPitcherNumber","PitcherNumber")
PitchThrowTypeA=merge(PitchThrowTypeA,batterlineupA2,by=c("Player"))
PitchThrowTypeA$Hand=as.character(PitchThrowTypeA$Hand)
PitchThrowTypeA$RealHand=ifelse(PitchThrowTypeA$RealHand==99,PitchThrowTypeA$Hand,PitchThrowTypeA$RealHand)
PitchThrowTypeA$RealHand=ifelse(PitchThrowTypeA$RealHand=='S' & PitchThrowTypeA$PitchHand=="R","L",PitchThrowTypeA$RealHand)
PitchThrowTypeA$RealHand=ifelse(PitchThrowTypeA$RealHand=='S' & PitchThrowTypeA$PitchHand=="L","R",PitchThrowTypeA$RealHand)
PitchThrowTypeA[1:2]=NULL
PitchThrowTypeA[19:33]=NULL
PitchThrowTypeA[21]=NULL
PitchThrowTypeA$PitchHand=ifelse(PitchThrowTypeA$PitchHand=="R",1,2)
PitchThrowTypeA$RealHand=ifelse(PitchThrowTypeA$RealHand=="R",1,2)
PitchThrowTypeA$Extra=paste0(PitchThrowTypeA$PitcherNumber,PitchThrowTypeA$BattingNumber,PitchThrowTypeA$Balls,PitchThrowTypeA$Strikes)
PitchThrowTypeA=unique(PitchThrowTypeA)
PitchThrowTypeA=apply(PitchThrowTypeA,2,as.numeric)


PitchThrowTypeB=merge(PitchThrowType,pitcherlineupB2,by=c("Pitcher"))
PitchThrowTypeB$X=NULL
PitchThrowTypeB=merge(PitchThrowTypeB,matchupB,by=c("Balls","Strikes","Pitcher","Player"),all=TRUE)
PitchThrowTypeB=merge(PitchThrowTypeB,averagepitch,by=c("Pitcher"))
PitchThrowTypeB=unfactor(PitchThrowTypeB)
PitchThrowTypeB[is.na(PitchThrowTypeB)]<-99
PitchThrowTypeB$percentageFF=ifelse(PitchThrowTypeB$percentageFF==99,PitchThrowTypeB$FF,PitchThrowTypeB$percentageFF)
PitchThrowTypeB$percentageCU=ifelse(PitchThrowTypeB$percentageCU==99,PitchThrowTypeB$CU,PitchThrowTypeB$percentageCU)
PitchThrowTypeB$percentageSL=ifelse(PitchThrowTypeB$percentageSL==99,PitchThrowTypeB$SL,PitchThrowTypeB$percentageSL)
PitchThrowTypeB$percentageCH=ifelse(PitchThrowTypeB$percentageCH==99,PitchThrowTypeB$CH,PitchThrowTypeB$percentageCH)
PitchThrowTypeB$percentageFT=ifelse(PitchThrowTypeB$percentageFT==99,PitchThrowTypeB$FT,PitchThrowTypeB$percentageFT)
PitchThrowTypeB$percentageSI=ifelse(PitchThrowTypeB$percentageSI==99,PitchThrowTypeB$SI,PitchThrowTypeB$percentageSI)
PitchThrowTypeB$percentageFS=ifelse(PitchThrowTypeB$percentageFS==99,PitchThrowTypeB$FS,PitchThrowTypeB$percentageFS)
PitchThrowTypeB$percentageKC=ifelse(PitchThrowTypeB$percentageKC==99,PitchThrowTypeB$KC,PitchThrowTypeB$percentageKC)
PitchThrowTypeB$percentageIN=ifelse(PitchThrowTypeB$percentageIN==99,PitchThrowTypeB$IN,PitchThrowTypeB$percentageIN)
PitchThrowTypeB$percentagePO=ifelse(PitchThrowTypeB$percentagePO==99,PitchThrowTypeB$PO,PitchThrowTypeB$percentagePO)
PitchThrowTypeB$percentageUN=ifelse(PitchThrowTypeB$percentageUN==99,PitchThrowTypeB$UN,PitchThrowTypeB$percentageUN)
PitchThrowTypeB$percentageEP=ifelse(PitchThrowTypeB$percentageEP==99,PitchThrowTypeB$EP,PitchThrowTypeB$percentageEP)
PitchThrowTypeB$percentageSC=ifelse(PitchThrowTypeB$percentageSC==99,PitchThrowTypeB$SC,PitchThrowTypeB$percentageSC)
PitchThrowTypeB$percentageFC=ifelse(PitchThrowTypeB$percentageFC==99,PitchThrowTypeB$FC,PitchThrowTypeB$percentageFC)
PitchThrowTypeB=merge(PitchThrowTypeB,pitcherlineupB3,by="Pitcher")
PitchThrowTypeB$ExtraPitchHand=as.character(PitchThrowTypeB$ExtraPitchHand)
PitchThrowTypeB$PitchHand=ifelse(PitchThrowTypeB$PitchHand==99,PitchThrowTypeB$ExtraPitchHand,PitchThrowTypeB$PitchHand)
setnames(PitchThrowTypeB,"ExtraPitcherNumber","PitcherNumber")
PitchThrowTypeB=merge(PitchThrowTypeB,batterlineupB2,by=c("Player"))
PitchThrowTypeB$Hand=as.character(PitchThrowTypeB$Hand)
PitchThrowTypeB$RealHand=ifelse(PitchThrowTypeB$RealHand==99,PitchThrowTypeB$Hand,PitchThrowTypeB$RealHand)
PitchThrowTypeB$RealHand=ifelse(PitchThrowTypeB$RealHand=='S' & PitchThrowTypeB$PitchHand=="R","L",PitchThrowTypeB$RealHand)
PitchThrowTypeB$RealHand=ifelse(PitchThrowTypeB$RealHand=='S' & PitchThrowTypeB$PitchHand=="L","R",PitchThrowTypeB$RealHand)
PitchThrowTypeB[1:2]=NULL
PitchThrowTypeB[19:33]=NULL
PitchThrowTypeB[21]=NULL
PitchThrowTypeB$PitchHand=ifelse(PitchThrowTypeB$PitchHand=="R",1,2)
PitchThrowTypeB$RealHand=ifelse(PitchThrowTypeB$RealHand=="R",1,2)
PitchThrowTypeB$Extra=paste0(PitchThrowTypeB$PitcherNumber,PitchThrowTypeB$BattingNumber,PitchThrowTypeB$Balls,PitchThrowTypeB$Strikes)
PitchThrowTypeB=unique(PitchThrowTypeB)
PitchThrowTypeB=apply(PitchThrowTypeB,2,as.numeric)



PitchThrowZoneA=merge(PitchThrowZone,pitcherlineupA1,by=c("Pitcher","PitchHand"))
PitchThrowZoneA$X=NULL
PitchThrowZoneA=merge(PitchThrowZoneA,matchup,by=c("Balls","Strikes","Pitcher","Player"),all=TRUE)
PitchThrowZoneA=unfactor(PitchThrowZoneA)
PitchThrowZoneA[is.na(PitchThrowZoneA)]<-99
PitchThrowZoneA$percentageL1=ifelse(PitchThrowZoneA$percentageL1==99,.1,PitchThrowZoneA$percentageL1)
PitchThrowZoneA$percentageL2=ifelse(PitchThrowZoneA$percentageL2==99,.1,PitchThrowZoneA$percentageL2)
PitchThrowZoneA$percentageL3=ifelse(PitchThrowZoneA$percentageL3==99,.1,PitchThrowZoneA$percentageL3)
PitchThrowZoneA$percentageL4=ifelse(PitchThrowZoneA$percentageL4==99,.1,PitchThrowZoneA$percentageL4)
PitchThrowZoneA$percentageL5=ifelse(PitchThrowZoneA$percentageL5==99,.2,PitchThrowZoneA$percentageL5)
PitchThrowZoneA$percentageL6=ifelse(PitchThrowZoneA$percentageL6==99,.1,PitchThrowZoneA$percentageL6)
PitchThrowZoneA$percentageL7=ifelse(PitchThrowZoneA$percentageL7==99,.1,PitchThrowZoneA$percentageL7)
PitchThrowZoneA$percentageL8=ifelse(PitchThrowZoneA$percentageL8==99,.1,PitchThrowZoneA$percentageL8)
PitchThrowZoneA$percentageL9=ifelse(PitchThrowZoneA$percentageL9==99,.1,PitchThrowZoneA$percentageL9)
PitchThrowZoneA$percentageL11=ifelse(PitchThrowZoneA$percentageL11==99,0,PitchThrowZoneA$percentageL11)
PitchThrowZoneA$percentageL12=ifelse(PitchThrowZoneA$percentageL12==99,0,PitchThrowZoneA$percentageL12)
PitchThrowZoneA$percentageL13=ifelse(PitchThrowZoneA$percentageL13==99,0,PitchThrowZoneA$percentageL13)
PitchThrowZoneA$percentageL14=ifelse(PitchThrowZoneA$percentageL14==99,0,PitchThrowZoneA$percentageL14)
PitchThrowZoneA=merge(PitchThrowZoneA,pitcherlineupA3,by="Pitcher")
PitchThrowZoneA$ExtraPitchHand=as.character(PitchThrowZoneA$ExtraPitchHand)
PitchThrowZoneA$PitchHand=ifelse(PitchThrowZoneA$PitchHand==99,PitchThrowZoneA$ExtraPitchHand,PitchThrowZoneA$PitchHand)
PitchThrowZoneA$PitcherNumber=ifelse(PitchThrowZoneA$PitcherNumber==99,PitchThrowZoneA$ExtraPitcherNumber,PitchThrowZoneA$PitcherNumber)
PitchThrowZoneA=merge(PitchThrowZoneA,batterlineupA2,by=c("Player"))
PitchThrowZoneA$Hand=as.character(PitchThrowZoneA$Hand)
PitchThrowZoneA$RealHand=ifelse(PitchThrowZoneA$RealHand==99,PitchThrowZoneA$Hand,PitchThrowZoneA$RealHand)
PitchThrowZoneA$RealHand=ifelse(PitchThrowZoneA$RealHand=='S' & PitchThrowZoneA$PitchHand=="R","L",PitchThrowZoneA$RealHand)
PitchThrowZoneA$RealHand=ifelse(PitchThrowZoneA$RealHand=='S' & PitchThrowZoneA$PitchHand=="L","R",PitchThrowZoneA$RealHand)
PitchThrowZoneA[1:2]=NULL
PitchThrowZoneA[4:5]=NULL
PitchThrowZoneA[19:20]=NULL
PitchThrowZoneA[20]=NULL
PitchThrowZoneA$PitchHand=ifelse(PitchThrowZoneA$PitchHand=="R",1,2)
PitchThrowZoneA$RealHand=ifelse(PitchThrowZoneA$RealHand=="R",1,2)
PitchThrowZoneA$Extra=paste0(PitchThrowZoneA$PitcherNumber,PitchThrowZoneA$BattingNumber,PitchThrowZoneA$Balls,PitchThrowZoneA$Strikes)
PitchThrowZoneA=unique(PitchThrowZoneA)
PitchThrowZoneA=apply(PitchThrowZoneA,2,as.numeric)


PitchThrowZoneB=merge(PitchThrowZone,pitcherlineupB1,by=c("Pitcher","PitchHand"))
PitchThrowZoneB$X=NULL
PitchThrowZoneB=merge(PitchThrowZoneB,matchupB,by=c("Balls","Strikes","Pitcher","Player"),all=TRUE)
PitchThrowZoneB=unfactor(PitchThrowZoneB)
PitchThrowZoneB[is.na(PitchThrowZoneB)]<-99
PitchThrowZoneB$percentageL1=ifelse(PitchThrowZoneB$percentageL1==99,.1,PitchThrowZoneB$percentageL1)
PitchThrowZoneB$percentageL2=ifelse(PitchThrowZoneB$percentageL2==99,.1,PitchThrowZoneB$percentageL2)
PitchThrowZoneB$percentageL3=ifelse(PitchThrowZoneB$percentageL3==99,.1,PitchThrowZoneB$percentageL3)
PitchThrowZoneB$percentageL4=ifelse(PitchThrowZoneB$percentageL4==99,.1,PitchThrowZoneB$percentageL4)
PitchThrowZoneB$percentageL5=ifelse(PitchThrowZoneB$percentageL5==99,.2,PitchThrowZoneB$percentageL5)
PitchThrowZoneB$percentageL6=ifelse(PitchThrowZoneB$percentageL6==99,.1,PitchThrowZoneB$percentageL6)
PitchThrowZoneB$percentageL7=ifelse(PitchThrowZoneB$percentageL7==99,.1,PitchThrowZoneB$percentageL7)
PitchThrowZoneB$percentageL8=ifelse(PitchThrowZoneB$percentageL8==99,.1,PitchThrowZoneB$percentageL8)
PitchThrowZoneB$percentageL9=ifelse(PitchThrowZoneB$percentageL9==99,.1,PitchThrowZoneB$percentageL9)
PitchThrowZoneB$percentageL11=ifelse(PitchThrowZoneB$percentageL11==99,0,PitchThrowZoneB$percentageL11)
PitchThrowZoneB$percentageL12=ifelse(PitchThrowZoneB$percentageL12==99,0,PitchThrowZoneB$percentageL12)
PitchThrowZoneB$percentageL13=ifelse(PitchThrowZoneB$percentageL13==99,0,PitchThrowZoneB$percentageL13)
PitchThrowZoneB$percentageL14=ifelse(PitchThrowZoneB$percentageL14==99,0,PitchThrowZoneB$percentageL14)
PitchThrowZoneB=merge(PitchThrowZoneB,pitcherlineupB3,by="Pitcher")
PitchThrowZoneB$ExtraPitchHand=as.character(PitchThrowZoneB$ExtraPitchHand)
PitchThrowZoneB$PitchHand=ifelse(PitchThrowZoneB$PitchHand==99,PitchThrowZoneB$ExtraPitchHand,PitchThrowZoneB$PitchHand)
PitchThrowZoneB$PitcherNumber=ifelse(PitchThrowZoneB$PitcherNumber==99,PitchThrowZoneB$ExtraPitcherNumber,PitchThrowZoneB$PitcherNumber)
PitchThrowZoneB=merge(PitchThrowZoneB,batterlineupB2,by=c("Player"))
PitchThrowZoneB$Hand=as.character(PitchThrowZoneB$Hand)
PitchThrowZoneB$RealHand=ifelse(PitchThrowZoneB$RealHand==99,PitchThrowZoneB$Hand,PitchThrowZoneB$RealHand)
PitchThrowZoneB$RealHand=ifelse(PitchThrowZoneB$RealHand=='S' & PitchThrowZoneB$PitchHand=="R","L",PitchThrowZoneB$RealHand)
PitchThrowZoneB$RealHand=ifelse(PitchThrowZoneB$RealHand=='S' & PitchThrowZoneB$PitchHand=="L","R",PitchThrowZoneB$RealHand)
PitchThrowZoneB[1:2]=NULL
PitchThrowZoneB[4:5]=NULL
PitchThrowZoneB[19:20]=NULL
PitchThrowZoneB[20]=NULL
PitchThrowZoneB$PitchHand=ifelse(PitchThrowZoneB$PitchHand=="R",1,2)
PitchThrowZoneB$RealHand=ifelse(PitchThrowZoneB$RealHand=="R",1,2)
PitchThrowZoneB$Extra=paste0(PitchThrowZoneB$PitcherNumber,PitchThrowZoneB$BattingNumber,PitchThrowZoneB$Balls,PitchThrowZoneB$Strikes)
PitchThrowZoneB=unique(PitchThrowZoneB)
PitchThrowZoneB=apply(PitchThrowZoneB,2,as.numeric)


SwingA=merge(Swing,batterlineupA1,by=c("Player"))
SwingA$X=NULL
SwingA$pitch_type=ifelse(SwingA$pitch_type=="FF",101,ifelse(SwingA$pitch_type=="CU",102,ifelse(SwingA$pitch_type=="SL",103,ifelse(SwingA$pitch_type=="CH",104,ifelse(SwingA$pitch_type=="FT",105,ifelse(SwingA$pitch_type=="SI",106,ifelse(SwingA$pitch_type=="FS",107,ifelse(SwingA$pitch_type=="KC",108,ifelse(SwingA$pitch_type=="IN",109,ifelse(SwingA$pitch_type=="PO",110,ifelse(SwingA$pitch_type=="UN",111,ifelse(SwingA$pitch_type=="EP",112,ifelse(SwingA$pitch_type=="SC",113,ifelse(SwingA$pitch_type=="FC",114,ifelse(SwingA$pitch_type=="KN",115,116)))))))))))))))
SwingA$zone=SwingA$zone+100
SwingA=merge(SwingA,batterlineupA3,by=c('Player'))
SwingA$p_throw=ifelse(SwingA$p_throw=="R",1,2)
SwingA[is.na(SwingA)]<-99
SwingA=subset(SwingA,zone !=99)
SwingA$extra=paste0(SwingA$BattingNumber,SwingA$balls,SwingA$strikes,SwingA$zone,SwingA$pitch_type,SwingA$p_throw)
SwingA$Player=NULL
SwingA=unique(SwingA)
SwingA=apply(SwingA,2,as.numeric)

SwingB=merge(Swing,batterlineupB1,by=c("Player"))
SwingB$X=NULL
SwingB$pitch_type=ifelse(SwingB$pitch_type=="FF",101,ifelse(SwingB$pitch_type=="CU",102,ifelse(SwingB$pitch_type=="SL",103,ifelse(SwingB$pitch_type=="CH",104,ifelse(SwingB$pitch_type=="FT",105,ifelse(SwingB$pitch_type=="SI",106,ifelse(SwingB$pitch_type=="FS",107,ifelse(SwingB$pitch_type=="KC",108,ifelse(SwingB$pitch_type=="IN",109,ifelse(SwingB$pitch_type=="PO",110,ifelse(SwingB$pitch_type=="UN",111,ifelse(SwingB$pitch_type=="EP",112,ifelse(SwingB$pitch_type=="SC",113,ifelse(SwingB$pitch_type=="FC",114,ifelse(SwingB$pitch_type=="KN",115,116)))))))))))))))
SwingB$zone=SwingB$zone+100
SwingB=merge(SwingB,batterlineupB3,by=c('Player'))
SwingB$p_throw=ifelse(SwingB$p_throw=="R",1,2)
SwingB[is.na(SwingB)]<-99
SwingB=subset(SwingB,zone !=99)
SwingB$extra=paste0(SwingB$BattingNumber,SwingB$balls,SwingB$strikes,SwingB$zone,SwingB$pitch_type,SwingB$p_throw)
SwingB$Player=NULL
SwingB=unique(SwingB)
SwingB=apply(SwingB,2,as.numeric)



averagecontact$X=NULL
setnames(averagecontact,"PlayerContact","PlayerContact1")


ChanceofContact$X=NULL
ChanceofContact$X.1=NULL
ChanceofContactA=merge(ChanceofContact,PitcherBatterA,by=c("Player","RealHand","Pitcher"))
ChanceofContactA=merge(ChanceofContactA,batterlineupA4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
ChanceofContactA=unfactor(ChanceofContactA)
ChanceofContactA[is.na(ChanceofContactA)]<-99
ChanceofContactA$RealHand=NULL
ChanceofContactA=merge(ChanceofContactA,pitcherlineupA2,by=c("Pitcher"))
ChanceofContactA$PitchHand=NULL
ChanceofContactA=merge(ChanceofContactA,averagecontact,by=c("Player","Pitcher"))
ChanceofContactA$CountFit=ifelse(ChanceofContactA$CountFit==99,ChanceofContactA$PlayerContact1,ChanceofContactA$CountFit)
ChanceofContactA$PlayerContact=ifelse(ChanceofContactA$PlayerContact==99,ChanceofContactA$PlayerContact1,ChanceofContactA$PlayerContact)
ChanceofContactA$pitch_type=ifelse(ChanceofContactA$pitch_type=="FF",101,ifelse(ChanceofContactA$pitch_type=="CU",102,ifelse(ChanceofContactA$pitch_type=="SL",103,ifelse(ChanceofContactA$pitch_type=="CH",104,ifelse(ChanceofContactA$pitch_type=="FT",105,ifelse(ChanceofContactA$pitch_type=="SI",106,ifelse(ChanceofContactA$pitch_type=="FS",107,ifelse(ChanceofContactA$pitch_type=="KC",108,ifelse(ChanceofContactA$pitch_type=="IN",109,ifelse(ChanceofContactA$pitch_type=="PO",110,ifelse(ChanceofContactA$pitch_type=="UN",111,ifelse(ChanceofContactA$pitch_type=="EP",112,ifelse(ChanceofContactA$pitch_type=="SC",113,ifelse(ChanceofContactA$pitch_type=="FC",114,ifelse(ChanceofContactA$pitch_type=="KN",115,116)))))))))))))))
ChanceofContactA$zone=ChanceofContactA$zone+100
ChanceofContactA=merge(ChanceofContactA,pitcherlineupA1,by=c("Pitcher","PitchHand"))
ChanceofContactA=merge(ChanceofContactA,batterlineupA3,by=c("Player"))
ChanceofContactA$PitchHand=ifelse(ChanceofContactA$PitchHand=="R",1,2)
ChanceofContactA$RealHand=ifelse(ChanceofContactA$RealHand=="R",1,2)
ChanceofContactA$Extra=paste0(ChanceofContactA$BattingNumber,ChanceofContactA$PitcherNumber,ChanceofContactA$pitch_type,ChanceofContactA$zone,ChanceofContactA$balls,ChanceofContactA$strikes,ChanceofContactA$RealHand)
ChanceofContactA$Pitcher=NULL
ChanceofContactA$Player=NULL
ChanceofContactA$CountFit=ifelse(ChanceofContactA$CountFit>1,.95,ChanceofContactA$CountFit)
ChanceofContactA=apply(ChanceofContactA,2,as.numeric)
ChanceofContactA=unique(ChanceofContactA)

ChanceofContactB=merge(ChanceofContact,PitcherBatterB,by=c("Player","RealHand","Pitcher"))
ChanceofContactB=merge(ChanceofContactB,batterlineupB4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
ChanceofContactB=unfactor(ChanceofContactB)
ChanceofContactB[is.na(ChanceofContactB)]<-99
ChanceofContactB$RealHand=NULL
ChanceofContactB=merge(ChanceofContactB,pitcherlineupB2,by=c("Pitcher"))
ChanceofContactB$PitchHand=NULL
ChanceofContactB=merge(ChanceofContactB,averagecontact,by=c("Player","Pitcher"))
ChanceofContactB$CountFit=ifelse(ChanceofContactB$CountFit==99,ChanceofContactB$PlayerContact1,ChanceofContactB$CountFit)
ChanceofContactB$PlayerContact=ifelse(ChanceofContactB$PlayerContact==99,ChanceofContactB$PlayerContact1,ChanceofContactB$PlayerContact)
ChanceofContactB$pitch_type=ifelse(ChanceofContactB$pitch_type=="FF",101,ifelse(ChanceofContactB$pitch_type=="CU",102,ifelse(ChanceofContactB$pitch_type=="SL",103,ifelse(ChanceofContactB$pitch_type=="CH",104,ifelse(ChanceofContactB$pitch_type=="FT",105,ifelse(ChanceofContactB$pitch_type=="SI",106,ifelse(ChanceofContactB$pitch_type=="FS",107,ifelse(ChanceofContactB$pitch_type=="KC",108,ifelse(ChanceofContactB$pitch_type=="IN",109,ifelse(ChanceofContactB$pitch_type=="PO",110,ifelse(ChanceofContactB$pitch_type=="UN",111,ifelse(ChanceofContactB$pitch_type=="EP",112,ifelse(ChanceofContactB$pitch_type=="SC",113,ifelse(ChanceofContactB$pitch_type=="FC",114,ifelse(ChanceofContactB$pitch_type=="KN",114,101)))))))))))))))
ChanceofContactB$zone=ChanceofContactB$zone+100
ChanceofContactB=merge(ChanceofContactB,pitcherlineupB1,by=c("Pitcher","PitchHand"))
ChanceofContactB=merge(ChanceofContactB,batterlineupB3,by=c("Player"))
ChanceofContactB$PitchHand=ifelse(ChanceofContactB$PitchHand=="R",1,2)
ChanceofContactB$RealHand=ifelse(ChanceofContactB$RealHand=="R",1,2)
ChanceofContactB$Extra=paste0(ChanceofContactB$BattingNumber,ChanceofContactB$PitcherNumber,ChanceofContactB$pitch_type,ChanceofContactB$zone,ChanceofContactB$balls,ChanceofContactB$strikes,ChanceofContactB$RealHand)
ChanceofContactB$Pitcher=NULL
ChanceofContactB$Player=NULL
ChanceofContactB$CountFit=ifelse(ChanceofContactB$CountFit>1,.95,ChanceofContactB$CountFit)
ChanceofContactB=apply(ChanceofContactB,2,as.numeric)
ChanceofContactB=unique(ChanceofContactB)

foul$X=NULL
foulA=merge(foul,batterlineupA3,by=c("Player"))
foulA$Player=NULL
foulA=as.matrix(foulA)
foulB=merge(foul,batterlineupB3,by=c("Player"))
foulB$Player=NULL
foulB=as.matrix(foulB)

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
StolenBase[,11]=NULL
StolenBase$realSB2nd=StolenBase$SB2nd*StolenBase$SBPercent.AverageAttempt
StolenBase$realSB3rd=StolenBase$SB3rd*StolenBase$SBPercent.AverageAttempt
StolenBase$RealSBPercent=StolenBase$SB.*StolenBase$SBPercent*StolenBase$CatcherSBPercent
StolenBase$RealSBPercent=ifelse(StolenBase$RealSBPercent>1,1,StolenBase$RealSBPercent)

options(warn=-1)

StolenBaseA=merge(StolenBase,batterlineupA1,by="Player")
options(warn=0)
StolenBaseA[1:3]=NULL
StolenBaseA[8:9]=NULL
StolenBaseA[10]=NULL
StolenBaseA[11:23]=NULL
StolenBaseA$Hand=NULL
StolenBaseA=apply(StolenBaseA,2,as.numeric)
options(warn=-1)
StolenBaseB=merge(StolenBase,batterlineupB1,by="Player")
options(warn=0)
StolenBaseB[1:3]=NULL
StolenBaseB[8:9]=NULL
StolenBaseB[10]=NULL
StolenBaseB[11:23]=NULL
StolenBaseB$Hand=NULL
StolenBaseB=apply(StolenBaseB,2,as.numeric)

hitspeedangle=0
launchanglenumber=0


hitspeed$X=NULL
hitspeedA=merge(hitspeed,batterlineupA1,by=c("Player"))
hitspeedA=merge(hitspeedA,batterlineupA4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
hitspeedA=unfactor(hitspeedA)
hitspeedA[is.na(hitspeedA)]<-99
hitspeedA=merge(hitspeedA,average,by=c("Player","Pitcher"))

hitspeedA$pitch_typehand=paste(hitspeedA$Player,hitspeedA$PitchHand,hitspeedA$pitch_type,sep="")
hitspeedA=merge(hitspeedA,SDFourth,by="pitch_typehand",all=TRUE)
hitspeedA[is.na(hitspeedA)]<-99

hitspeedA$sd=13.4
hitspeedA$sd=ifelse(hitspeedA$SD<98,hitspeedA$SD,hitspeedA$sd)
hitspeedA$pitch_typehand=NULL
hitspeedA$SD=NULL

hitspeedA$RealLaunchSpeed=ifelse(hitspeedA$RealLaunchSpeed==99,hitspeedA$launch_speed,hitspeedA$RealLaunchSpeed)
hitspeedA=merge(hitspeedA,batterlineupA2,by=c("Player"))
hitspeedA=merge(hitspeedA,pitcherlineupA1,by=c("Pitcher","PitchHand"))
hitspeedA$PitchHand=NULL
hitspeedA$Hand=NULL
hitspeedA[1:2]=NULL
hitspeedA$RealHand=ifelse(hitspeedA$RealHand=="R",1,2)
hitspeedA$pitch_type=ifelse(hitspeedA$pitch_type=="FF",101,ifelse(hitspeedA$pitch_type=="CU",102,ifelse(hitspeedA$pitch_type=="SL",103,ifelse(hitspeedA$pitch_type=="CH",104,ifelse(hitspeedA$pitch_type=="FT",105,ifelse(hitspeedA$pitch_type=="SI",106,ifelse(hitspeedA$pitch_type=="FS",107,ifelse(hitspeedA$pitch_type=="KC",108,ifelse(hitspeedA$pitch_type=="IN",109,ifelse(hitspeedA$pitch_type=="PO",110,ifelse(hitspeedA$pitch_type=="UN",111,ifelse(hitspeedA$pitch_type=="EP",112,ifelse(hitspeedA$pitch_type=="SC",113,ifelse(hitspeedA$pitch_type=="FC",114,101))))))))))))))
hitspeedA$zone=hitspeedA$zone+100
hitspeedA$Extra=paste0(hitspeedA$BattingNumber,hitspeedA$PitcherNumber,hitspeedA$pitch_type,hitspeedA$zone,hitspeedA$balls,hitspeedA$strikes,hitspeedA$RealHand)
hitspeedA=unique(hitspeedA)
hitspeedA=apply(hitspeedA,2,as.numeric)


hitspeedB=merge(hitspeed,batterlineupB1,by=c("Player"))
hitspeedB=merge(hitspeedB,batterlineupB4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
hitspeedB=unfactor(hitspeedB)
hitspeedB[is.na(hitspeedB)]<-99
hitspeedB=merge(hitspeedB,average,by=c("Player","Pitcher"))

hitspeedB$pitch_typehand=paste(hitspeedB$Player,hitspeedB$PitchHand,hitspeedB$pitch_type,sep="")
hitspeedB=merge(hitspeedB,SDFourth,by="pitch_typehand",all=TRUE)
hitspeedB[is.na(hitspeedB)]<-99

hitspeedB$sd=13.4
hitspeedB$sd=ifelse(hitspeedB$SD<98,hitspeedB$SD,hitspeedB$sd)
hitspeedB$pitch_typehand=NULL
hitspeedB$SD=NULL



hitspeedB$RealLaunchSpeed=ifelse(hitspeedB$RealLaunchSpeed==99,hitspeedB$launch_speed,hitspeedB$RealLaunchSpeed)
hitspeedB=merge(hitspeedB,batterlineupB2,by=c("Player"))
hitspeedB=merge(hitspeedB,pitcherlineupB1,by=c("Pitcher","PitchHand"))
hitspeedB$PitchHand=NULL
hitspeedB$Hand=NULL
hitspeedB[1:2]=NULL
hitspeedB$RealHand=ifelse(hitspeedB$RealHand=="R",1,2)
hitspeedB$pitch_type=ifelse(hitspeedB$pitch_type=="FF",101,ifelse(hitspeedB$pitch_type=="CU",102,ifelse(hitspeedB$pitch_type=="SL",103,ifelse(hitspeedB$pitch_type=="CH",104,ifelse(hitspeedB$pitch_type=="FT",105,ifelse(hitspeedB$pitch_type=="SI",106,ifelse(hitspeedB$pitch_type=="FS",107,ifelse(hitspeedB$pitch_type=="KC",108,ifelse(hitspeedB$pitch_type=="IN",109,ifelse(hitspeedB$pitch_type=="PO",110,ifelse(hitspeedB$pitch_type=="UN",111,ifelse(hitspeedB$pitch_type=="EP",112,ifelse(hitspeedB$pitch_type=="SC",113,ifelse(hitspeedB$pitch_type=="FC",114,101))))))))))))))
hitspeedB$zone=hitspeedB$zone+100
hitspeedB$Extra=paste0(hitspeedB$BattingNumber,hitspeedB$PitcherNumber,hitspeedB$pitch_type,hitspeedB$zone,hitspeedB$balls,hitspeedB$strikes,hitspeedB$RealHand)
hitspeedB=unique(hitspeedB)
hitspeedB=apply(hitspeedB,2,as.numeric)


launchangle$X=NULL
launchangleA=merge(launchangle,batterlineupA1,by=c("Player"))
launchangleA=merge(launchangleA,batterlineupA4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
launchangleA=unfactor(launchangleA)
launchangleA[is.na(launchangleA)]<-99
launchangleA=merge(launchangleA,average,by=c("Player","Pitcher"))
launchangleA$sd=13.261868
launchangleA$Reallaunch_angle=ifelse(launchangleA$Reallaunch_angle==99,launchangleA$launch_angle,launchangleA$Reallaunch_angle)
launchangleA=merge(launchangleA,batterlineupA2,by=c("Player"))
launchangleA=merge(launchangleA,pitcherlineupA1,by=c("Pitcher","PitchHand"))
launchangleA$PitchHand=NULL
launchangleA$Hand=NULL
launchangleA[1:2]=NULL
launchangleA$RealHand=ifelse(launchangleA$RealHand=="R",1,2)
launchangleA$pitch_type=ifelse(launchangleA$pitch_type=="FF",101,ifelse(launchangleA$pitch_type=="CU",102,ifelse(launchangleA$pitch_type=="SL",103,ifelse(launchangleA$pitch_type=="CH",104,ifelse(launchangleA$pitch_type=="FT",105,ifelse(launchangleA$pitch_type=="SI",106,ifelse(launchangleA$pitch_type=="FS",107,ifelse(launchangleA$pitch_type=="KC",108,ifelse(launchangleA$pitch_type=="IN",109,ifelse(launchangleA$pitch_type=="PO",110,ifelse(launchangleA$pitch_type=="UN",111,ifelse(launchangleA$pitch_type=="EP",112,ifelse(launchangleA$pitch_type=="SC",113,ifelse(launchangleA$pitch_type=="FC",114,101))))))))))))))
launchangleA$zone=launchangleA$zone+100
launchangleA$Extra=paste0(launchangleA$BattingNumber,launchangleA$PitcherNumber,launchangleA$pitch_type,launchangleA$zone,launchangleA$balls,launchangleA$strikes,launchangleA$RealHand)
launchangleA=unique(launchangleA)
launchangleA=apply(launchangleA,2,as.numeric)


launchangleB=merge(launchangle,batterlineupB1,by=c("Player"))
launchangleB=merge(launchangleB,batterlineupB4,by=c("Player","Pitcher","zone","pitch_type","balls","strikes"),all=TRUE)
launchangleB=unfactor(launchangleB)
launchangleB[is.na(launchangleB)]<-99
launchangleB=merge(launchangleB,average,by=c("Player","Pitcher"))
launchangleB$sd=13.261868
launchangleB$Reallaunch_angle=ifelse(launchangleB$Reallaunch_angle==99,launchangleB$launch_angle,launchangleB$Reallaunch_angle)
launchangleB=merge(launchangleB,batterlineupB2,by=c("Player"))
launchangleB=merge(launchangleB,pitcherlineupB1,by=c("Pitcher","PitchHand"))
launchangleB$PitchHand=NULL
launchangleB$Hand=NULL
launchangleB[1:2]=NULL
launchangleB$RealHand=ifelse(launchangleB$RealHand=="R",1,2)
launchangleB$pitch_type=ifelse(launchangleB$pitch_type=="FF",101,ifelse(launchangleB$pitch_type=="CU",102,ifelse(launchangleB$pitch_type=="SL",103,ifelse(launchangleB$pitch_type=="CH",104,ifelse(launchangleB$pitch_type=="FT",105,ifelse(launchangleB$pitch_type=="SI",106,ifelse(launchangleB$pitch_type=="FS",107,ifelse(launchangleB$pitch_type=="KC",108,ifelse(launchangleB$pitch_type=="IN",109,ifelse(launchangleB$pitch_type=="PO",110,ifelse(launchangleB$pitch_type=="UN",111,ifelse(launchangleB$pitch_type=="EP",112,ifelse(launchangleB$pitch_type=="SC",113,ifelse(launchangleB$pitch_type=="FC",114,101))))))))))))))
launchangleB$zone=launchangleB$zone+100
launchangleB$Extra=paste0(launchangleB$BattingNumber,launchangleB$PitcherNumber,launchangleB$pitch_type,launchangleB$zone,launchangleB$balls,launchangleB$strikes,launchangleB$RealHand)
launchangleB=unique(launchangleB)
launchangleB=apply(launchangleB,2,as.numeric)


Angle$X=NULL
AngleA=merge(Angle,batterlineupA3,by=c("Player"))
AngleA$Player=NULL
AngleA=as.matrix(AngleA)

AngleB=merge(Angle,batterlineupB3,by=c("Player"))
AngleB$Player=NULL
AngleB=as.matrix(AngleB)



####################################
pitcherlineupAA=pitcherlineupA
pitcherlineupAA=pitcherlineupAA[,-1:-3]
pitcherlineupAA$PitchHand=ifelse(pitcherlineupAA$PitchHand=="R",1,2)
pitcherlineupAA=as.matrix(pitcherlineupAA)

batterlineupA=merge(batterlineupA,CatchProb,by=c("Player"),all=TRUE)
batterlineupA=subset(batterlineupA,Team==AwayTeam)
batterlineupA=merge(batterlineupA,Speed,by=c("Player"),all=TRUE)
batterlineupA=subset(batterlineupA,Team==AwayTeam)
batterlineupA[is.na(batterlineupA)]<-0
batterlineupAA=batterlineupA
batterlineupAA$Team=NULL
batterlineupAA$Player=NULL
batterlineupAA$Hand=ifelse(batterlineupAA$Hand=='R',1,ifelse(batterlineupAA$Hand=="L",2,3))
batterlineupAA=arrange(batterlineupAA,BattingNumber)
batterlineupAA=as.matrix(batterlineupAA)

pitcherlineupBB=pitcherlineupB
pitcherlineupBB=pitcherlineupBB[,-1:-3]
pitcherlineupBB$PitchHand=ifelse(pitcherlineupBB$PitchHand=="R",1,2)
pitcherlineupBB=as.matrix(pitcherlineupBB)

batterlineupB=merge(batterlineupB,CatchProb,by=c("Player"),all=TRUE)
batterlineupB=subset(batterlineupB,Team==HomeTeam)
batterlineupB=merge(batterlineupB,Speed,by=c("Player"),all=TRUE)
batterlineupB=subset(batterlineupB,Team==HomeTeam)
batterlineupB[is.na(batterlineupB)]<-0
batterlineupBB=batterlineupB
batterlineupBB$Team=NULL
batterlineupBB$Player=NULL
batterlineupBB$Hand=ifelse(batterlineupBB$Hand=='R',1,ifelse(batterlineupBB$Hand=="L",2,3))
batterlineupBB=arrange(batterlineupBB,BattingNumber)
batterlineupBB=as.matrix(batterlineupBB)


ParkFactor=subset(ParkFactor,Team==Stadium)
hitpercentage$SingleAdjust=hitpercentage$Single * ParkFactor$Single
hitpercentage$DoubleAdjust=hitpercentage$Double * ParkFactor$Double
hitpercentage$TripleAdjust=hitpercentage$Triple * ParkFactor$Triple
hitpercentage$Totaladjust=hitpercentage$SingleAdjust+hitpercentage$DoubleAdjust+hitpercentage$TripleAdjust+hitpercentage$out

hitpercentage$Single=hitpercentage$SingleAdjust / hitpercentage$Totaladjust
hitpercentage$Double=hitpercentage$DoubleAdjust / hitpercentage$Totaladjust
hitpercentage$Triple=hitpercentage$TripleAdjust / hitpercentage$Totaladjust
hitpercentage$out=hitpercentage$out/hitpercentage$Totaladjust

hitpercentage$SingleAdjust=NULL
hitpercentage$DoubleAdjust=NULL
hitpercentage$TripleAdjust=NULL
hitpercentage$Totaladjust=NULL


trajectory=function(r,t,y,u,w,h){
  
  
  temperature=r
  barometricpressureHg=29.92
  barometericpressuremmHg=barometricpressureHg*1000/39.37
  beta=0.0001217
  elevation=t
  elevationM=elevation/3.2808
  relativehumidty=h
  
  
  mass=5.125
  circball=9.125
  exitspeed=y
  launchangle=u
  direction=15
  cd0=0.4105
  cddot=0.0044
  cdspin=0.2017
  w0=2072
  theta0=7.2
  tau0=25
  tausec=tau0
  wb=2072*(exitspeed/100)*(launchangle-7.2)/(27.5-7.2)
  cd=cd0*(1+cddot*(100-exitspeed))
  backspin=wb
  sidespin=2
  wg=0
  vwind=w
  phiwind=0
  x0=0
  y0=2
  z0=3
  dtsec=0.01
  hwind=1
  
  
  tempC=(5/9)*(temperature-32)
  SVP=4.5841*exp((18.687-tempC/234.5)*tempC/(257.14+tempC))
  rhokg=1.2929*(273/(tempC+273)*(barometericpressuremmHg*exp(-beta*elevationM)-0.3783*relativehumidty*SVP/100)/760)
  
  rholb=rhokg*0.06261
  c0=0.07182*rholb*(5.125/mass)*(circball/9.125)^2
  const=c0
  v0=exitspeed*1.467
  v0x=1.467*exitspeed*cos(launchangle*pi/180)*sin(direction*pi/180)
  v0y=1.467*exitspeed*cos(launchangle*pi/180)*cos(direction*pi/180)
  v0z=1.467*exitspeed*sin(launchangle*pi/180)
  
  wx=(backspin*cos(direction*pi/180)-sidespin*sin(launchangle*pi/180)*sin(direction*pi/180)+wg*v0x/v0)*pi/30
  wy=(-backspin*sin(direction*pi/180)-sidespin*sin(launchangle*pi/180)*cos(direction*pi/180)+wg*v0y/v0)*pi/30
  wz=(sidespin*cos(launchangle*pi/180)+wg*v0z/v0)*pi/30
  omega=sqrt(wx^2+wy^2+wz^2)
  romega=(circball/2/pi)*omega/12
  vxw=vwind*1.467*sin(phiwind*pi/180)
  vyw=vwind*1.467*cos(phiwind*pi/180)
  Re100=rhokg*44.7*(circball/(pi*39.37))*(tempC+273.16+120)/(0.000001512*(tempC+273.16)^1.5)
  
  
  traj=matrix(0,nrow=1127,ncol=9)
  cols1=c("aaa","bbb","t","x","vx","vxw","adragx","aMagx","ax")
  colnames(traj)=cols1
  traj[,1]=dtsec
  traj[1,1]=0
  traj[,"t"]=cumsum(traj[,"aaa"])
  
  
  
  
  
  
  
  y=as.matrix(y0)
  z=as.matrix(z0)
  r=as.matrix(sqrt(traj[1,"x"]^2 ^ y[1,]^2))
  phi=as.matrix(0)
  vy=as.matrix(v0y)
  vz=as.matrix(v0z)
  v=as.matrix(sqrt(traj[1,"vx"] + vy[1,]^2 + vz[1,]^2 ))
  vw=as.matrix(ifelse(z[1,]>=hwind,sqrt((traj[1,"vx"]-vxw)^2 + (vy-vyw)^2 + vz^2),v))
  vmph=as.matrix(v/1.467)
  S=as.matrix((romega/vw[1])*exp(-traj[1,"t"]/(tausec*146.7/v[1])))
  cdfit=as.matrix(cd*(1+cdspin*S[1,]^2))
  CI=as.matrix(1/(2.32+0.4/S[1,]))
  vywfit=as.matrix(ifelse(z[1,]>=hwind,vyw,0))
  adragy=as.matrix(-const*cdfit[1,]*vw[1,]*(vy[1,]-vywfit[1,]))
  adragz=as.matrix(-const*cdfit[1,]*vw[1,]*vz[1,])
  W=as.matrix(omega*exp(-traj[1,"t"]/tausec)*30/pi)
  aMagy=as.matrix(const*(CI[1,]/omega)*vw[1,]*(wz*(traj[1,"vx"]-traj[1,"vxw"])-wx*vz[1,]))
  aMagz=as.matrix(const*(CI[1,]/omega)*vw[1,]*(wx*(vy[1,]-vywfit[1,])-wy*(traj[1,"vx"]-traj[1,"vxw"])))
  ay=as.matrix(adragy[1,]+aMagy[1,])
  az=as.matrix(adragz[1,]+aMagz[1,]-32.174)
  
  
  for (i in 1:1126){
    y=rbind(y,y[i,]+vy[i,]*dtsec+0.5*ay[i,]*dtsec*dtsec)
    z=rbind(z,z[i,]+vz[i,]*dtsec+0.5*az[i,]*dtsec*dtsec)
    r=rbind(r,sqrt(traj[i+1,"x"]^2+y[(i+1),]^2))
    vy=rbind(vy,vy[i,]+ay[i,]*dtsec)
    vz=rbind(vz,(vz[i,]+az[i,]*dtsec))
    v=rbind(v,sqrt(traj[i+1,"vx"]^2+vy[i+1,]^2+vz[i+1,]^2))
    vw=rbind(vw,ifelse(z[i+1,]>=hwind,sqrt((traj[i+1,"vx"]-vxw)^2+(vy[i+1,]-vyw)^2+vz[i+1,]^2),v[i+1,]))
    vmph=rbind(vmph,v[i+1,]/1.467)
    S=rbind(S,(romega/vw[i+1,])*exp(-traj[i+1,"t"]/(tausec*146.7/v[i+1,])))
    cdfit=rbind(cdfit,cd*(1+cdspin*S[i+1,]^2))
    CI=rbind(CI,1/(2.32+0.4/S[i+1,]))
    vywfit=rbind(vywfit,ifelse(z[i+1,]>=hwind,vyw,0))
    adragy=rbind(adragy,-const*cdfit[i+1,]*vw[i+1,]*(vy[i+1,]-vywfit[i+1,]))
    adragz=rbind(adragz,-const*cdfit[i+1,]*vw[i+1,]*vz[i+1,])
    W=rbind(W,omega*exp(-traj[i+1,"t"]/tausec)*30/pi)
    aMagy=rbind(aMagy,const*(CI[i+1,]/omega)*vw[i+1,]*(wz*(traj[i+1,"vx"]-traj[i+1,'vxw'])-wx*vz[i+1,]))
    aMagz=rbind(aMagz,const*(CI[i+1,]/omega)*vw[i+1,]*(wx*(vy[i+1,]-vywfit[i+1,])-wy*(traj[i+1,"vx"]-traj[i+1,"vxw"])))
    ay=rbind(ay,adragy[i+1,]+aMagy[i+1,])
    az=rbind(az,adragz[i+1,]+aMagz[i+1,]-32.174)
  }
  
  
  
  flag=as.matrix(ifelse(z[1,]*z[1+1,]<0,1,0))
  for(i in 2:1126){
    flag=rbind(flag,ifelse(z[i,]*z[i+1,]<0,1,0))
  }
  
  
  flag=as.data.frame(flag)
  flag[1127,1]=0
  flag=as.matrix(flag)
  
  
  first=cbind(y,z,r,vy,vz,v,vw,vmph,cdfit,S,CI,vywfit,adragy,adragz,W,aMagy,aMagz,ay,az,flag,traj[,"t"],traj[,"x"])
  cols2=c('y','z','r','vy','vz','v','vw','vmph','cdfit','S','CI','vywfit','adragy','adragz','W','aMagy','aMagz','ay','az',"flag","t","x")
  colnames(first)=cols2
  
  
  match1=match(1,first[,"flag"])
  match2=match1+1
  match3=first[,"x"][match1]
  match4=first[,"x"][match2]
  match5=first[,"y"][match1]
  match6=first[,"y"][match2]
  match7=first[,"z"][match1]
  match8=first[,"z"][match2]
  match8.5=match8/(match8-match7)
  match9=first[,"t"][match1]
  match10=first[,"t"][match2]
  match11=0
  match12=0
  match13=first[,"r"][match1]
  match14=first[,"r"][match2]
  
  xlandingpoint=match4-match8.5*(match4-match3)
  ylandingpoint=match6-match8.5*(match6-match5)
  
  hangtime=match10-match8.5*(match10-match9)
  distance=sqrt(xlandingpoint^2+ylandingpoint^2)
  return(list(distance,hangtime))
  
  
}

################
AngelsPark=151
DiamondbacksPark=1086
BravesPark=930
OriolesPark=33
RedSoxPark=21
WhiteSoxPark=595
CubsPark=595
RedsPark=535
IndiansPark=653
RockiesPark=5190
TigersPark=600
MarlinsPark=11
AstrosPark=45
RoyalsPark=865
DodgersPark=515
BrewersPark=597
TwinsPark=815
YankeesPark=55
MetsPark=10
AsPark=3
PhilliesPark=20
PiratesPark=780
PadresPark=23
MarinersPark=10
GiantsPark=0
CardinalsPark=460
RaysPark=15
RangersPark=545
BlueJaysPark=270
NationalsPark=35
######################################
FantasyModelAway=batterlineupA1
FantasyModelHome=batterlineupB1
FantasyPitcherModelAway=pitcherlineupB2
FantasyPitcherModelHome=pitcherlineupA2
Score=as.data.frame(rbind(AwayTeam,HomeTeam))
PitcherK=as.data.frame(rbind(as.character(FantasyPitcherModelAway[1,]),as.character(FantasyPitcherModelHome[1,])))
BatterSBAway=batterlineupA1
BatterSBHome=batterlineupB1
BatterHRRAway=batterlineupA1
BatterHRRHome=batterlineupB1
BatterHitsAway=batterlineupA1
BatterHitsHome=batterlineupB1
BatterHRAway=batterlineupA1
BatterHRHome=batterlineupB1
BatterOutAway=batterlineupA1
BatterOutHome=batterlineupB1
BatterWalkAway=batterlineupA1
BatterWalkHome=batterlineupB1
BatterTotalBasesAway=batterlineupA1
BatterTotalBasesHome=batterlineupB1
PitcherRunsAway=pitcherlineupA1[1]
PitcherRunsHome=pitcherlineupB1[1]
PitcherOutsAway=pitcherlineupA1[1]
PitcherOutsHome=pitcherlineupB1[1]

ParkElevation=paste(Stadium,"Park",sep="")


Park=get(ParkElevation)
