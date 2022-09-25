for(i in 1:TotalGames){
  
  TagThird="N"
  ScoreThird="N"
  MoveSecond="N"
  MoveFirst="N"
  TagSecond="N"
  TagFirst="N"
  ScoreThird="N"
  Inning=0
  DoublePlay="N"
  DoublePlayLead="N"
  FoulOut="N"
  Steal2ndAttempt="N"
  Steal2nd="N"
  Steal3rdAttempt="N"
  Steal3rd="N"
  strikeout=2
  out=3
  Runs=4
  Single=5
  Double=6
  Triple=7
  HomeRun=8
  PA=9
  walk=10
  RBI=11
  SB=12
  HomeScore=0
  AwayScore=0
  
  
  Inning=0
  
  
  pitcherlineupA=pitcherlineupAA
  batterlineupA=batterlineupAA
  
  
  boxscore=read.csv('boxscore.csv',TRUE,',')
  boxscoreAway1=batterlineupA[,"BattingNumber"]
  boxscoreAway=merge(boxscoreAway1,boxscore)
  colnames(boxscoreAway)[1]="BattingNumber"
  boxscoreAway=arrange(boxscoreAway,BattingNumber)
  
  
  pitcherboxscore=read.csv("Homeboxscorepitcher.csv",TRUE,",")
  pitcherboxscoreHome1=pitcherlineupA[,c("PitcherNumber")]
  pitcherboxscoreHome=merge(pitcherboxscoreHome1,pitcherboxscore)
  colnames(pitcherboxscoreHome)[1]="PitcherNumber"
  
  
  
  PitchCount=0
  batternumberAway=1
  pitchernumberHome=1
  Pitcher1MaxRuns=10
  Pitcher1Limit=100
  HomePitcherRuns=0
  Pitcher1=1
  Pitcher1=as.numeric(pitcherlineupA[pitchernumberHome,"PitcherNumber"])
  
  HomeScore=0
  AwayScore=0
  HitTo=0
  ###################
  
  
  
  pitcherlineupB=pitcherlineupBB
  batterlineupB=batterlineupBB
  
  
  
  boxscoreHome1=batterlineupB[,"BattingNumber"]
  boxscoreHome=merge(boxscoreHome1,boxscore)
  colnames(boxscoreHome)[1]="BattingNumber"
  boxscoreHome=arrange(boxscoreHome,BattingNumber)
  
  
  pitcherboxscoreAway1=pitcherlineupB[,c("PitcherNumber")]
  pitcherboxscoreAway=merge(pitcherboxscoreAway1,pitcherboxscore)
  colnames(pitcherboxscoreAway)[1]="PitcherNumber"
  
  
  
  PitchCountAway=0
  batternumberHome=1
  pitchernumberAway=1
  Pitcher2MaxRuns=10
  Pitcher2Limit=100
  AwayPitcherRuns=0
  Pitcher2=1
  Pitcher2=as.numeric(pitcherlineupB[pitchernumberAway,"PitcherNumber"])
  
  
  
  repeat{
    Inning=Inning+1
    AwayInningOut=0
    FirstBase=0
    SecondBase=0
    ThirdBase=0
    
    repeat{
      
      Pitcher1Runs=as.matrix(pitcherboxscoreHome[which(pitcherboxscoreHome[,'PitcherNumber']==Pitcher1),])
      Pitcher1Runs=Pitcher1Runs[,c('Runs')]
      
      
      
      ReplaceHomePitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupA[2,Inning+3],pitcherlineupA[3,Inning+3],pitcherlineupA[4,Inning+3],pitcherlineupA[5,Inning+3],pitcherlineupA[6,Inning+3],pitcherlineupA[7,Inning+3]))
      if(PitchCount>Pitcher1Limit){Pitcher1=ReplaceHomePitcher} else if (Pitcher1Runs>Pitcher1MaxRuns){Pitcher1=ReplaceHomePitcher}
      if(PitchCount>Pitcher1Limit){PitchCount=0} else if (Pitcher1Runs>Pitcher1MaxRuns){PitchCount=0}
      pitcherlineupA[Pitcher1,4:13]=0
      pitcherlineupA[,2:14]=as.numeric(pitcherlineupA[,2:14])
      pitcherlineupASum=sum(pitcherlineupA[,Inning+3])
      pitcherlineupAmultiplier=1/pitcherlineupASum
      pitcherlineupA[,Inning+3]=pitcherlineupA[,Inning+3]*pitcherlineupAmultiplier
      
      batternumberAway=ifelse(batternumberAway==10,1,batternumberAway)
      
      
      
      PitcherHand1=pitcherlineupA[,'PitchHand'][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
      PlayerHand1=batterlineupA[,"Hand"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
      Pitcher1Limit=pitcherlineupA[,"MaxPitch"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
      Pitcher1MaxRuns=pitcherlineupA[,"MaxRuns"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
      if(PlayerHand1==3 & PitcherHand1==1){PlayerHand1=2}
      if(PlayerHand1==3 & PitcherHand1==2){PlayerHand1=1}
      
      ballscount=0
      strikescount=0
      DoesSwing="N"
      MakesContact="N"
      MakesFoul="N"
      StolenBaseOut="N"
      FoulOut="N"
      repeat{
        
        PitchThrowTypesubset=PitchThrowTypeA[which(PitchThrowTypeA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
        
        PitchThrowZonesubset=PitchThrowZoneA[which(PitchThrowZoneA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
        
        Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
        
        Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
        
        
        Swingsubset=SwingA[which(SwingA[,'extra']==as.numeric(paste0(batternumberAway,ballscount,strikescount,Zone,Pitch,PitcherHand1))),]
        
        PitchCount=PitchCount+1
        
        
        DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
        
        
        ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
        
        strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
        
        
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubset=ChanceofContactA[which(ChanceofContactA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubset['CountFit'],(1-ChanceofContactsubset['CountFit'])))}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulA[which(foulA[,'BattingNumber']==batternumberAway),]}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==FirstBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
        
        if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
        
        
        if(Steal2nd=="Y"){SecondBase=FirstBase}
        if(Steal2nd=="Y"){FirstBase=0}
        if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
        if(Steal2nd=="Y"){boxscoreAway[SecondBase,SB]=boxscoreAway[SecondBase,SB]+1}
        
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==SecondBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
        
        if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
        
        
        if(Steal3rd=="Y"){ThirdBase=SecondBase}
        if(Steal3rd=="Y"){SecondBase=0}
        if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
        if(Steal3rd=="Y"){boxscoreAway[ThirdBase,SB]=boxscoreAway[ThirdBase,SB]+1}
        
        
        if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
        if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
        
        Steal2ndAttempt="N"
        Steal2nd="N"
        Steal3rdAttempt="N"
        Steal3rd="N"
        
        
        if(StolenBaseOut=="Y" & AwayInningOut + 1==3) break
        if(FoulOut=="Y")break
        if(MakesContact=="Y" & MakesFoul=="N") break
        if(ballscount==4) break
        if(strikescount==3) break
      }
      
      if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
        hitspeedsubset=hitspeedA[which(hitspeedA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
        
        launchanglesubset=launchangleA[which(launchangleA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
        
        
        hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
        hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
        hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
        launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
        hitspeednumber=round(hitspeednumber)
        launchanglenumber=round(launchanglenumber)
        launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
        launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
        
        
        AngleSubset=AngleA[which(AngleA[,'BattingNumber']==batternumberAway),]
        
        
        AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
        Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
        
        
        
        
        
        
        if(launchanglenumber >12 & hitspeednumber >88){
          FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
          
          Distance=as.numeric(FirstDistance[[1]])
          
          Feet2Meter=Distance*0.3048
          hangtime=as.numeric(FirstDistance[[2]])
          hangtime1=as.numeric(hangtime[[1]])
          NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
          
          ballheight=1
          WallHeight=ParkWallHeights[c("Angle",Stadium)]
          WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
          WallHeight$Angle=NULL
          WallHeightMetric=WallHeight*0.3048
          
          WallDistance=ParkFenceDistance[c("Angle",Stadium)]
          WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
          WallDistance$Angle=NULL
          WallDistanceMetric=WallDistance*0.3048
          HeightAtWall=Distance-(WallDistance+10)
        } else{
          Distance=1
          HeightAtWall=-5
        }
        
        
        hitspeednumber=round_any(hitspeednumber,5)
        launchanglenumber=round_any(launchanglenumber,5)
        
        HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
        HitTo1=batterlineupB[which(batterlineupB[,'Position']==HitTo),]
        HitToCatch=HitTo1['CatchPercent']
        
        
        
        hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
        
        
        
        
        hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
        
        
        hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
        
        SpeedAdded=batterlineupA[,"Speed"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
        SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
        
        
        
        hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
        hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
        
        ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==HomeTeam & ShiftPercentage[,"Hand"]==PlayerHand1 ),]
        ShiftSubset1=as.numeric(ShiftSubset["Shift"])
        ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
        ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
        ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
        ShiftNumber=hitpercentagesubset['Single'] *.033784
        hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
        hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
        if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
        
        
        hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
        hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
        hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
        hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
        hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
        
        result=hitpercentagesubset1
      }
      
      
      
      boxscoreAway[batternumberAway,Single]=ifelse(result=="Single",boxscoreAway[batternumberAway,Single]+1,boxscoreAway[batternumberAway,Single]+0)
      boxscoreAway[batternumberAway,Double]=ifelse(result=="Double",boxscoreAway[batternumberAway,Double]+1,boxscoreAway[batternumberAway,Double]+0)
      boxscoreAway[batternumberAway,Triple]=ifelse(result=="Triple",boxscoreAway[batternumberAway,Triple]+1,boxscoreAway[batternumberAway,Triple]+0)
      boxscoreAway[batternumberAway,HomeRun]=ifelse(result=="HomeRun",boxscoreAway[batternumberAway,HomeRun]+1,boxscoreAway[batternumberAway,HomeRun]+0)
      boxscoreAway[batternumberAway,strikeout]=ifelse(result=="strikeout",boxscoreAway[batternumberAway,strikeout]+1,boxscoreAway[batternumberAway,strikeout]+0)
      boxscoreAway[batternumberAway,out]=ifelse(result=="Out",boxscoreAway[batternumberAway,out]+1,boxscoreAway[batternumberAway,out]+0)
      boxscoreAway[batternumberAway,walk]=ifelse(result=="walk",boxscoreAway[batternumberAway,walk]+1,boxscoreAway[batternumberAway,walk]+0)
      boxscoreAway[batternumberAway,PA]=boxscoreAway[batternumberAway,PA]+1
      
      
      if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(ThirdBase!=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){ThirdBase=0}
      
      if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
      
      
      if(DoublePlay=="Y"){FirstBase=0}
      if(DoublePlay=="Y"){AwayInningOut=AwayInningOut+1}
      
      
      if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
      
      
      if(DoublePlayLead=="Second"){FirstBase=batternumberAway}
      
      if(DoublePlayLead=="First"){SecondBase=FirstBase}
      if(DoublePlayLead=="First"){FirstBase=0}
      
      DoublePlay="N"
      DoublePlayLead="N"
      
      
      
      TagThird="N"
      if(ThirdBase !=0 & HitTo==7 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
      if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
      if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
      if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
      if(TagThird=="Y"){ThirdBase=0}
      
      
      if(ThirdBase !=0 & HitTo==3 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
      if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
      if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
      if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
      if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
      if(TagThird=="Y"){ThirdBase=0}
      
      TagSecond="N"
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
      if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
      if(TagSecond=="Y"){SecondBase=0}
      
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & AwayInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
      if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
      if(TagSecond=="Y"){SecondBase=0}
      
      
      
      TagFirst="N"
      if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
      if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
      if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
      if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
      if(TagFirst=="Y"){FirstBase=0}
      
      
      
      
      
      
      
      ################################
      
      
      
      SecondToSingle="N"
      if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
      
      
      if(SecondToSingle=="Third"){ThirdBase=SecondBase}
      if(SecondToSingle=="Home"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(SecondToSingle=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(SecondToSingle=="Out"){SecondBase=0}
      if(SecondBase != 0 & result=="Single"){SecondBase=0}
      if(SecondToSingle!="N"){SecondToSingle="N"}
      
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
      
      ######################
      
      
      FirstToSingle="N"
      if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
      
      if(FirstToSingle=="Second"){SecondBase=FirstBase}
      if(FirstToSingle=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
      if(FirstToSingle!="N"){FirstToSingle="N"}
      
      
      FirstToSingle="N"
      if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
      
      if(FirstToSingle=="Second"){SecondBase=FirstBase}
      if(FirstToSingle=="Third"){ThirdBase=FirstBase}
      if(FirstToSingle=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Single"){FirstBase=0}
      if(FirstToSingle!="N"){FirstToSingle="N"}
      
      FirstToDouble="N"
      if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
      
      if(FirstToDouble=="Third"){ThirdBase=FirstBase}
      if(FirstToDouble=="Home"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(FirstToDouble=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(FirstToDouble=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Double"){FirstBase=0}
      if(FirstToDouble!="N"){FirstToDouble="N"}
      
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
      
      
      if(result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(result=="HomeRun"){boxscoreAway[batternumberAway,Runs]=boxscoreAway[batternumberAway,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      
      if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
      if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
      if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
      if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
      
      if(result=="Single"){FirstBase=batternumberAway}
      if(result=="Double"){SecondBase=batternumberAway}
      if(result=="Triple"){ThirdBase=batternumberAway}
      if(result=="walk"){FirstBase=batternumberAway}
      
      
      
      
      AwayInningOut=ifelse(result=="strikeout" | result=="Out",AwayInningOut+1,AwayInningOut)
      if(result=="Out") {pitcherboxscoreHome[Pitcher1,out]=pitcherboxscoreHome[Pitcher1,out]+1}
      if(result=="strikeout") {pitcherboxscoreHome[Pitcher1,strikeout]=pitcherboxscoreHome[Pitcher1,strikeout]+1}
      
      
      batternumberAway=batternumberAway+1
      
      
      if(AwayInningOut>2) break
    }
    
    
    
    HomeInningOut=0
    FirstBase=0
    SecondBase=0
    ThirdBase=0
    
    repeat{
      Pitcher2Runs=as.matrix(pitcherboxscoreAway[which(pitcherboxscoreAway[,'PitcherNumber']==Pitcher2),])
      Pitcher2Runs=Pitcher2Runs[,'Runs']
      
      
      
      ReplaceAwayPitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupB[2,Inning+3],pitcherlineupB[3,Inning+3],pitcherlineupB[4,Inning+3],pitcherlineupB[5,Inning+3],pitcherlineupB[6,Inning+3],pitcherlineupB[7,Inning+3]))
      if(PitchCountAway>Pitcher2Limit){Pitcher2=ReplaceAwayPitcher} else if (Pitcher2Runs>Pitcher2MaxRuns){Pitcher2=ReplaceAwayPitcher}
      if(PitchCountAway>Pitcher2Limit){PitchCountAway=0} else if (Pitcher2Runs>Pitcher2MaxRuns){PitchCountAway=0}
      pitcherlineupB[Pitcher2,4:13]=0
      pitcherlineupBSum=sum(pitcherlineupB[,Inning+3])
      pitcherlineupBmultiplier=1/pitcherlineupBSum
      pitcherlineupB[,Inning+3]=pitcherlineupB[,Inning+3]*pitcherlineupBmultiplier
      
      batternumberHome=ifelse(batternumberHome==10,1,batternumberHome)
      
      
      
      
      PitcherHand2=pitcherlineupB[,'PitchHand'][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
      PlayerHand2=batterlineupB[,"Hand"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
      Pitcher2Limit=pitcherlineupB[,"MaxPitch"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
      Pitcher2MaxRuns=pitcherlineupB[,"MaxRuns"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
      if(PlayerHand2==3 & PitcherHand2==1){PlayerHand2=2}
      if(PlayerHand2==3 & PitcherHand2==2){PlayerHand2=1}
      
      ballscount=0
      strikescount=0
      DoesSwing="N"
      MakesContact="N"
      MakesFoul="N"
      StolenBaseOut="N"
      FoulOut="N"
      repeat{
        
        PitchThrowTypesubset=PitchThrowTypeB[which(PitchThrowTypeB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
        
        PitchThrowZonesubset=PitchThrowZoneB[which(PitchThrowZoneB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
        
        Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
        
        Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
        
        
        Swingsubset=SwingB[which(SwingB[,'extra']==as.numeric(paste0(batternumberHome,ballscount,strikescount,Zone,Pitch,PitcherHand2))),]
        
        PitchCountAway=PitchCountAway+1
        
        
        DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
        
        
        ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
        
        strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
        
        
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubsetB=ChanceofContactB[which(ChanceofContactB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubsetB['CountFit'],(1-ChanceofContactsubsetB['CountFit'])))}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulB[which(foulB[,'BattingNumber']==batternumberHome),]}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
        
        if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==FirstBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
        
        if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
        
        
        if(Steal2nd=="Y"){SecondBase=FirstBase}
        if(Steal2nd=="Y"){FirstBase=0}
        if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
        if(Steal2nd=="Y"){boxscoreHome[SecondBase,SB]=boxscoreHome[SecondBase,SB]+1}
        
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==SecondBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
        
        if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
        
        if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
        
        
        if(Steal3rd=="Y"){ThirdBase=SecondBase}
        if(Steal3rd=="Y"){SecondBase=0}
        if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
        if(Steal3rd=="Y"){boxscoreHome[ThirdBase,SB]=boxscoreHome[ThirdBase,SB]+1}
        
        
        if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
        if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
        
        Steal2ndAttempt="N"
        Steal2nd="N"
        Steal3rdAttempt="N"
        Steal3rd="N"
        
        
        if(StolenBaseOut=="Y" & HomeInningOut + 1==3) break
        if(FoulOut=="Y")break
        if(MakesContact=="Y" & MakesFoul=="N") break
        if(ballscount==4) break
        if(strikescount==3) break
      }
      
      if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
        hitspeedsubset=hitspeedB[which(hitspeedB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
        
        launchanglesubset=launchangleB[which(launchangleB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
        
        
        hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
        hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
        hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
        launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
        hitspeednumber=round(hitspeednumber)
        launchanglenumber=round(launchanglenumber)
        launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
        launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
        
        
        AngleSubset=AngleB[which(AngleB[,'BattingNumber']==batternumberHome),]
        
        
        AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
        Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
        
        
        if(launchanglenumber >12 & hitspeednumber >88){
          FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
          Distance=as.numeric(FirstDistance[[1]])
          
          Feet2Meter=Distance*0.3048
          hangtime=as.numeric(FirstDistance[[2]])
          hangtime1=as.numeric(hangtime[[1]])
          NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
          
          ballheight=1
          WallHeight=ParkWallHeights[c("Angle",Stadium)]
          WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
          WallHeight$Angle=NULL
          WallHeightMetric=WallHeight*0.3048
          
          WallDistance=ParkFenceDistance[c("Angle",Stadium)]
          WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
          WallDistance$Angle=NULL
          WallDistanceMetric=WallDistance*0.3048
          HeightAtWall=Distance-(WallDistance+10)
        } else{
          Distance=1
          HeightAtWall=-5
        }
        
        
        hitspeednumber=round_any(hitspeednumber,5)
        launchanglenumber=round_any(launchanglenumber,5)
        
        HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
        HitTo1=batterlineupA[which(batterlineupA[,'Position']==HitTo),]
        HitToCatch=HitTo1['CatchPercent']
        
        
        
        hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
        
        
        
        
        hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
        
        
        hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
        
        SpeedAdded=batterlineupB[,"Speed"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
        SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
        
        
        
        hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
        hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
        ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==AwayTeam & ShiftPercentage[,"Hand"]==PlayerHand2),]
        ShiftSubset1=as.numeric(ShiftSubset["Shift"])
        ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
        ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
        ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
        ShiftNumber=hitpercentagesubset['Single'] *.033784
        hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
        hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
        if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
        
        
        hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
        hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
        hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
        hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
        hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
        
        result=hitpercentagesubset1
      }
      
      
      
      boxscoreHome[batternumberHome,Single]=ifelse(result=="Single",boxscoreHome[batternumberHome,Single]+1,boxscoreHome[batternumberHome,Single]+0)
      boxscoreHome[batternumberHome,Double]=ifelse(result=="Double",boxscoreHome[batternumberHome,Double]+1,boxscoreHome[batternumberHome,Double]+0)
      boxscoreHome[batternumberHome,Triple]=ifelse(result=="Triple",boxscoreHome[batternumberHome,Triple]+1,boxscoreHome[batternumberHome,Triple]+0)
      boxscoreHome[batternumberHome,HomeRun]=ifelse(result=="HomeRun",boxscoreHome[batternumberHome,HomeRun]+1,boxscoreHome[batternumberHome,HomeRun]+0)
      boxscoreHome[batternumberHome,strikeout]=ifelse(result=="strikeout",boxscoreHome[batternumberHome,strikeout]+1,boxscoreHome[batternumberHome,strikeout]+0)
      boxscoreHome[batternumberHome,out]=ifelse(result=="Out",boxscoreHome[batternumberHome,out]+1,boxscoreHome[batternumberHome,out]+0)
      boxscoreHome[batternumberHome,walk]=ifelse(result=="walk",boxscoreHome[batternumberHome,walk]+1,boxscoreAway[batternumberHome,walk]+0)
      boxscoreHome[batternumberHome,PA]=boxscoreHome[batternumberHome,PA]+1
      
      
      if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(ThirdBase!=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){ThirdBase=0}
      
      if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
      
      
      if(DoublePlay=="Y"){FirstBase=0}
      if(DoublePlay=="Y"){HomeInningOut=HomeInningOut+1}
      
      
      if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
      
      
      if(DoublePlayLead=="Second"){FirstBase=batternumberHome}
      
      if(DoublePlayLead=="First"){SecondBase=FirstBase}
      if(DoublePlayLead=="First"){FirstBase=0}
      
      
      
      DoublePlay="N"
      DoublePlayLead="N"
      
      TagThird="N"
      if(ThirdBase !=0 & HitTo==7 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
      if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
      if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
      if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
      if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
      if(TagThird=="Y"){ThirdBase=0}
      
      
      if(ThirdBase !=0 & HitTo==3 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
      if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
      if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
      if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
      if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
      if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
      if(TagThird=="Y"){ThirdBase=0}
      
      TagSecond="N"
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
      if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
      if(TagSecond=="Y"){SecondBase=0}
      
      if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & HomeInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
      if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
      if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
      if(TagSecond=="Y"){SecondBase=0}
      
      
      
      
      TagFirst="N"
      if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
      if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
      if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
      if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
      if(TagFirst=="Y"){FirstBase=0}
      
      
      
      
      
      
      
      
      
      
      
      ################################
      
      
      
      SecondToSingle="N"
      if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
      
      
      if(SecondToSingle=="Third"){ThirdBase=SecondBase}
      if(SecondToSingle=="Home"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(SecondToSingle=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(SecondToSingle=="Out"){SecondBase=0}
      if(SecondBase != 0 & result=="Single"){SecondBase=0}
      if(SecondToSingle!="N"){SecondToSingle="N"}
      
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
      
      ######################
      
      
      FirstToSingle="N"
      if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
      
      if(FirstToSingle=="Second"){SecondBase=FirstBase}
      if(FirstToSingle=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
      if(FirstToSingle!="N"){FirstToSingle="N"}
      
      
      FirstToSingle="N"
      if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
      
      if(FirstToSingle=="Second"){SecondBase=FirstBase}
      if(FirstToSingle=="Third"){ThirdBase=FirstBase}
      if(FirstToSingle=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Single"){FirstBase=0}
      if(FirstToSingle!="N"){FirstToSingle="N"}
      
      FirstToDouble="N"
      if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
      
      if(FirstToDouble=="Third"){ThirdBase=FirstBase}
      if(FirstToDouble=="Home"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(FirstToDouble=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(FirstToDouble=="Out"){FirstBase=0}
      if(FirstBase != 0 & result=="Double"){FirstBase=0}
      if(FirstToDouble!="N"){FirstToDouble="N"}
      
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
      
      
      if(result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(result=="HomeRun"){boxscoreHome[batternumberHome,Runs]=boxscoreHome[batternumberHome,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      
      if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
      if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
      if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
      if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
      
      if(result=="Single"){FirstBase=batternumberHome}
      if(result=="Double"){SecondBase=batternumberHome}
      if(result=="Triple"){ThirdBase=batternumberHome}
      if(result=="walk"){FirstBase=batternumberHome}
      
      
      
      
      HomeInningOut=ifelse(result=="strikeout" | result=="Out",HomeInningOut+1,HomeInningOut)
      if(result=="Out") {pitcherboxscoreAway[Pitcher2,out]=pitcherboxscoreAway[Pitcher2,out]+1}
      if(result=="strikeout") {pitcherboxscoreAway[Pitcher2,strikeout]=pitcherboxscoreAway[Pitcher2,strikeout]+1}
      
      
      batternumberHome=batternumberHome+1
      
      
      if(HomeInningOut>2) break
    }
    
    
    if(Inning==8) break
    
  }
  
  Inning=Inning+1
  AwayInningOut=0
  FirstBase=0
  SecondBase=0
  ThirdBase=0
  
  repeat{
    Pitcher1Runs=as.matrix(pitcherboxscoreHome[which(pitcherboxscoreHome[,'PitcherNumber']==Pitcher1),])
    Pitcher1Runs=Pitcher1Runs[,'Runs']
    
    
    
    ReplaceHomePitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupA[2,Inning+3],pitcherlineupA[3,Inning+3],pitcherlineupA[4,Inning+3],pitcherlineupA[5,Inning+3],pitcherlineupA[6,Inning+3],pitcherlineupA[7,Inning+3]))
    if(PitchCount>Pitcher1Limit){Pitcher1=ReplaceHomePitcher} else if (Pitcher1Runs>Pitcher1MaxRuns){Pitcher1=ReplaceHomePitcher}
    if(PitchCount>Pitcher1Limit){PitchCount=0} else if (Pitcher1Runs>Pitcher1MaxRuns){PitchCount=0}
    pitcherlineupA[Pitcher1,4:13]=0
    pitcherlineupASum=sum(pitcherlineupA[,Inning+3])
    pitcherlineupAmultiplier=1/pitcherlineupASum
    pitcherlineupA[,Inning+3]=pitcherlineupA[,Inning+3]*pitcherlineupAmultiplier
    
    batternumberAway=ifelse(batternumberAway==10,1,batternumberAway)
    
    
    
    PitcherHand1=pitcherlineupA[,'PitchHand'][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
    PlayerHand1=batterlineupA[,"Hand"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
    Pitcher1Limit=pitcherlineupA[,"MaxPitch"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
    Pitcher1MaxRuns=pitcherlineupA[,"MaxRuns"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
    if(PlayerHand1==3 & PitcherHand1==1){PlayerHand1=2}
    if(PlayerHand1==3 & PitcherHand1==2){PlayerHand1=1}
    
    ballscount=0
    strikescount=0
    DoesSwing="N"
    MakesContact="N"
    MakesFoul="N"
    StolenBaseOut="N"
    FoulOut="N"
    repeat{
      
      PitchThrowTypesubset=PitchThrowTypeA[which(PitchThrowTypeA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
      
      PitchThrowZonesubset=PitchThrowZoneA[which(PitchThrowZoneA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
      
      Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
      
      Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
      
      
      Swingsubset=SwingA[which(SwingA[,'extra']==as.numeric(paste0(batternumberAway,ballscount,strikescount,Zone,Pitch,PitcherHand1))),]
      
      PitchCount=PitchCount+1
      
      
      DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
      
      
      ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
      
      strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
      
      
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubset=ChanceofContactA[which(ChanceofContactA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubset['CountFit'],(1-ChanceofContactsubset['CountFit'])))}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulA[which(foulA[,'BattingNumber']==batternumberAway),]}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==FirstBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
      
      if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
      
      
      if(Steal2nd=="Y"){SecondBase=FirstBase}
      if(Steal2nd=="Y"){FirstBase=0}
      if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
      if(Steal2nd=="Y"){boxscoreAway[SecondBase,SB]=boxscoreAway[SecondBase,SB]+1}
      
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==SecondBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
      
      if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
      
      
      if(Steal3rd=="Y"){ThirdBase=SecondBase}
      if(Steal3rd=="Y"){SecondBase=0}
      if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
      if(Steal3rd=="Y"){boxscoreAway[ThirdBase,SB]=boxscoreAway[ThirdBase,SB]+1}
      
      
      if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
      if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
      
      Steal2ndAttempt="N"
      Steal2nd="N"
      Steal3rdAttempt="N"
      Steal3rd="N"
      
      
      if(StolenBaseOut=="Y" & AwayInningOut + 1==3) break
      if(FoulOut=="Y")break
      if(MakesContact=="Y" & MakesFoul=="N") break
      if(ballscount==4) break
      if(strikescount==3) break
    }
    
    if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
      hitspeedsubset=hitspeedA[which(hitspeedA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
      
      launchanglesubset=launchangleA[which(launchangleA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
      
      
      hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
      hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
      hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
      launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
      hitspeednumber=round(hitspeednumber)
      launchanglenumber=round(launchanglenumber)
      launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
      launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
      
      
      AngleSubset=AngleA[which(AngleA[,'BattingNumber']==batternumberAway),]
      
      
      AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
      Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
      
      
      if(launchanglenumber >12 & hitspeednumber >88){
        FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
        Distance=as.numeric(FirstDistance[[1]])
        
        Feet2Meter=Distance*0.3048
        hangtime=as.numeric(FirstDistance[[2]])
        hangtime1=as.numeric(hangtime[[1]])
        NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
        
        ballheight=1
        WallHeight=ParkWallHeights[c("Angle",Stadium)]
        WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
        WallHeight$Angle=NULL
        WallHeightMetric=WallHeight*0.3048
        
        WallDistance=ParkFenceDistance[c("Angle",Stadium)]
        WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
        WallDistance$Angle=NULL
        WallDistanceMetric=WallDistance*0.3048
        HeightAtWall=Distance-(WallDistance+10)
      } else{
        Distance=1
        HeightAtWall=-5
      }
      
      
      hitspeednumber=round_any(hitspeednumber,5)
      launchanglenumber=round_any(launchanglenumber,5)
      
      HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
      HitTo1=batterlineupB[which(batterlineupB[,'Position']==HitTo),]
      HitToCatch=HitTo1['CatchPercent']
      
      
      
      hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
      
      
      
      
      hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
      
      
      hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
      
      SpeedAdded=batterlineupA[,"Speed"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
      SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
      
      
      
      hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
      hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
      ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==HomeTeam & ShiftPercentage[,"Hand"]==PlayerHand1),]
      ShiftSubset1=as.numeric(ShiftSubset["Shift"])
      ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
      ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
      ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
      ShiftNumber=hitpercentagesubset['Single'] *.033784
      hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
      hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
      if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
      
      
      hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
      hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
      hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
      hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
      hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
      
      result=hitpercentagesubset1
    }
    
    
    
    boxscoreAway[batternumberAway,Single]=ifelse(result=="Single",boxscoreAway[batternumberAway,Single]+1,boxscoreAway[batternumberAway,Single]+0)
    boxscoreAway[batternumberAway,Double]=ifelse(result=="Double",boxscoreAway[batternumberAway,Double]+1,boxscoreAway[batternumberAway,Double]+0)
    boxscoreAway[batternumberAway,Triple]=ifelse(result=="Triple",boxscoreAway[batternumberAway,Triple]+1,boxscoreAway[batternumberAway,Triple]+0)
    boxscoreAway[batternumberAway,HomeRun]=ifelse(result=="HomeRun",boxscoreAway[batternumberAway,HomeRun]+1,boxscoreAway[batternumberAway,HomeRun]+0)
    boxscoreAway[batternumberAway,strikeout]=ifelse(result=="strikeout",boxscoreAway[batternumberAway,strikeout]+1,boxscoreAway[batternumberAway,strikeout]+0)
    boxscoreAway[batternumberAway,out]=ifelse(result=="Out",boxscoreAway[batternumberAway,out]+1,boxscoreAway[batternumberAway,out]+0)
    boxscoreAway[batternumberAway,walk]=ifelse(result=="walk",boxscoreAway[batternumberAway,walk]+1,boxscoreAway[batternumberAway,walk]+0)
    boxscoreAway[batternumberAway,PA]=boxscoreAway[batternumberAway,PA]+1
    
    
    if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(ThirdBase!=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){ThirdBase=0}
    
    if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
    
    
    if(DoublePlay=="Y"){FirstBase=0}
    if(DoublePlay=="Y"){AwayInningOut=AwayInningOut+1}
    
    
    if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
    
    
    if(DoublePlayLead=="Second"){FirstBase=batternumberAway}
    
    if(DoublePlayLead=="First"){SecondBase=FirstBase}
    if(DoublePlayLead=="First"){FirstBase=0}
    
    DoublePlay="N"
    DoublePlayLead="N"
    
    TagThird="N"
    if(ThirdBase !=0 & HitTo==7 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
    if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
    if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
    if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
    if(TagThird=="Y"){ThirdBase=0}
    
    
    if(ThirdBase !=0 & HitTo==3 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
    if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
    if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
    if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
    if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
    if(TagThird=="Y"){ThirdBase=0}
    
    TagSecond="N"
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
    if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
    if(TagSecond=="Y"){SecondBase=0}
    
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & AwayInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
    if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
    if(TagSecond=="Y"){SecondBase=0}
    
    
    
    
    
    TagFirst="N"
    if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
    if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
    if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
    if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
    if(TagFirst=="Y"){FirstBase=0}
    
    
    
    
    
    ################################
    
    
    
    SecondToSingle="N"
    if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
    
    
    if(SecondToSingle=="Third"){ThirdBase=SecondBase}
    if(SecondToSingle=="Home"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(SecondToSingle=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(SecondToSingle=="Out"){SecondBase=0}
    if(SecondBase != 0 & result=="Single"){SecondBase=0}
    if(SecondToSingle!="N"){SecondToSingle="N"}
    
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
    
    ######################
    
    
    FirstToSingle="N"
    if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
    
    if(FirstToSingle=="Second"){SecondBase=FirstBase}
    if(FirstToSingle=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
    if(FirstToSingle!="N"){FirstToSingle="N"}
    
    
    FirstToSingle="N"
    if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
    
    if(FirstToSingle=="Second"){SecondBase=FirstBase}
    if(FirstToSingle=="Third"){ThirdBase=FirstBase}
    if(FirstToSingle=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Single"){FirstBase=0}
    if(FirstToSingle!="N"){FirstToSingle="N"}
    
    FirstToDouble="N"
    if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
    
    if(FirstToDouble=="Third"){ThirdBase=FirstBase}
    if(FirstToDouble=="Home"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(FirstToDouble=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(FirstToDouble=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Double"){FirstBase=0}
    if(FirstToDouble!="N"){FirstToDouble="N"}
    
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
    
    
    if(result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(result=="HomeRun"){boxscoreAway[batternumberAway,Runs]=boxscoreAway[batternumberAway,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    
    if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
    if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
    if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
    if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
    
    if(result=="Single"){FirstBase=batternumberAway}
    if(result=="Double"){SecondBase=batternumberAway}
    if(result=="Triple"){ThirdBase=batternumberAway}
    if(result=="walk"){FirstBase=batternumberAway}
    
    
    
    
    AwayInningOut=ifelse(result=="strikeout" | result=="Out",AwayInningOut+1,AwayInningOut)
    if(result=="Out") {pitcherboxscoreHome[Pitcher1,out]=pitcherboxscoreHome[Pitcher1,out]+1}
    if(result=="strikeout") {pitcherboxscoreHome[Pitcher1,strikeout]=pitcherboxscoreHome[Pitcher1,strikeout]+1}
    
    
    batternumberAway=batternumberAway+1
    
    
    
    if(AwayInningOut>2) break
  }
  
  HomeScore=sum(boxscoreHome$Runs)
  AwayScore=sum(boxscoreAway$Runs)
  
  if(HomeScore<AwayScore){HomeInningOut=0
  FirstBase=0
  SecondBase=0
  ThirdBase=0
  
  repeat{
    Pitcher2Runs=as.matrix(pitcherboxscoreAway[which(pitcherboxscoreAway[,'PitcherNumber']==Pitcher2),])
    Pitcher2Runs=Pitcher2Runs[,'Runs']
    
    
    
    ReplaceAwayPitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupB[2,Inning+3],pitcherlineupB[3,Inning+3],pitcherlineupB[4,Inning+3],pitcherlineupB[5,Inning+3],pitcherlineupB[6,Inning+3],pitcherlineupB[7,Inning+3]))
    if(PitchCountAway>Pitcher2Limit){Pitcher2=ReplaceAwayPitcher} else if (Pitcher2Runs>Pitcher2MaxRuns){Pitcher2=ReplaceAwayPitcher}
    if(PitchCountAway>Pitcher2Limit){PitchCountAway=0} else if (Pitcher2Runs>Pitcher2MaxRuns){PitchCountAway=0}
    pitcherlineupB[Pitcher2,4:13]=0
    pitcherlineupBSum=sum(pitcherlineupB[,Inning+3])
    pitcherlineupBmultiplier=1/pitcherlineupBSum
    pitcherlineupB[,Inning+3]=pitcherlineupB[,Inning+3]*pitcherlineupBmultiplier
    
    batternumberHome=ifelse(batternumberHome==10,1,batternumberHome)
    
    
    
    
    PitcherHand2=pitcherlineupB[,'PitchHand'][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
    PlayerHand2=batterlineupB[,"Hand"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
    
    Pitcher2Limit=pitcherlineupB[,"MaxPitch"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
    Pitcher2MaxRuns=pitcherlineupB[,"MaxRuns"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
    if(PlayerHand2==3 & PitcherHand2==1){PlayerHand2=2}
    if(PlayerHand2==3 & PitcherHand2==2){PlayerHand2=1}
    
    ballscount=0
    strikescount=0
    DoesSwing="N"
    MakesContact="N"
    MakesFoul="N"
    StolenBaseOut="N"
    FoulOut="N"
    repeat{
      
      PitchThrowTypesubset=PitchThrowTypeB[which(PitchThrowTypeB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
      
      PitchThrowZonesubset=PitchThrowZoneB[which(PitchThrowZoneB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
      
      Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
      
      Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
      
      
      Swingsubset=SwingB[which(SwingB[,'extra']==as.numeric(paste0(batternumberHome,ballscount,strikescount,Zone,Pitch,PitcherHand2))),]
      
      PitchCountAway=PitchCountAway+1
      
      
      DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
      
      
      ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
      
      strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
      
      
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubsetB=ChanceofContactB[which(ChanceofContactB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubsetB['CountFit'],(1-ChanceofContactsubsetB['CountFit'])))}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulB[which(foulB[,'BattingNumber']==batternumberHome),]}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
      
      if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==FirstBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
      
      if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
      
      
      if(Steal2nd=="Y"){SecondBase=FirstBase}
      if(Steal2nd=="Y"){FirstBase=0}
      if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
      if(Steal2nd=="Y"){boxscoreHome[SecondBase,SB]=boxscoreHome[SecondBase,SB]+1}
      
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==SecondBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
      
      if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
      
      if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
      
      
      if(Steal3rd=="Y"){ThirdBase=SecondBase}
      if(Steal3rd=="Y"){SecondBase=0}
      if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
      if(Steal3rd=="Y"){boxscoreHome[ThirdBase,SB]=boxscoreHome[ThirdBase,SB]+1}
      
      
      if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
      if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
      
      Steal2ndAttempt="N"
      Steal2nd="N"
      Steal3rdAttempt="N"
      Steal3rd="N"
      
      
      if(StolenBaseOut=="Y" & HomeInningOut + 1==3) break
      if(FoulOut=="Y")break
      if(MakesContact=="Y" & MakesFoul=="N") break
      if(ballscount==4) break
      if(strikescount==3) break
    }
    
    if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
      hitspeedsubset=hitspeedB[which(hitspeedB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
      
      launchanglesubset=launchangleB[which(launchangleB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
      
      
      hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
      hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
      hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
      launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
      hitspeednumber=round(hitspeednumber)
      launchanglenumber=round(launchanglenumber)
      launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
      launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
      
      
      AngleSubset=AngleB[which(AngleB[,'BattingNumber']==batternumberHome),]
      
      
      AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
      Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
      
      
      if(launchanglenumber >12 & hitspeednumber >88){
        FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
        Distance=as.numeric(FirstDistance[[1]])
        
        Feet2Meter=Distance*0.3048
        hangtime=as.numeric(FirstDistance[[2]])
        hangtime1=as.numeric(hangtime[[1]])
        NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
        
        ballheight=1
        WallHeight=ParkWallHeights[c("Angle",Stadium)]
        WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
        WallHeight$Angle=NULL
        WallHeightMetric=WallHeight*0.3048
        
        WallDistance=ParkFenceDistance[c("Angle",Stadium)]
        WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
        WallDistance$Angle=NULL
        WallDistanceMetric=WallDistance*0.3048
        HeightAtWall=Distance-(WallDistance+10)
      } else{
        Distance=1
        HeightAtWall=-5
      }
      
      hitspeednumber=round_any(hitspeednumber,5)
      launchanglenumber=round_any(launchanglenumber,5)
      
      HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
      HitTo1=batterlineupA[which(batterlineupA[,'Position']==HitTo),]
      HitToCatch=HitTo1['CatchPercent']
      
      
      
      hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
      
      
      
      
      hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
      
      
      hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
      
      SpeedAdded=batterlineupB[,"Speed"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
      SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
      
      
      
      hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
      hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
      ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==AwayTeam & ShiftPercentage[,"Hand"]==PlayerHand2),]
      ShiftSubset1=as.numeric(ShiftSubset["Shift"])
      ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
      ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
      ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
      ShiftNumber=hitpercentagesubset['Single'] *.033784
      hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
      hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
      if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
      
      
      hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
      hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
      hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
      hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
      hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
      
      result=hitpercentagesubset1
    }
    
    
    
    boxscoreHome[batternumberHome,Single]=ifelse(result=="Single",boxscoreHome[batternumberHome,Single]+1,boxscoreHome[batternumberHome,Single]+0)
    boxscoreHome[batternumberHome,Double]=ifelse(result=="Double",boxscoreHome[batternumberHome,Double]+1,boxscoreHome[batternumberHome,Double]+0)
    boxscoreHome[batternumberHome,Triple]=ifelse(result=="Triple",boxscoreHome[batternumberHome,Triple]+1,boxscoreHome[batternumberHome,Triple]+0)
    boxscoreHome[batternumberHome,HomeRun]=ifelse(result=="HomeRun",boxscoreHome[batternumberHome,HomeRun]+1,boxscoreHome[batternumberHome,HomeRun]+0)
    boxscoreHome[batternumberHome,strikeout]=ifelse(result=="strikeout",boxscoreHome[batternumberHome,strikeout]+1,boxscoreHome[batternumberHome,strikeout]+0)
    boxscoreHome[batternumberHome,out]=ifelse(result=="Out",boxscoreHome[batternumberHome,out]+1,boxscoreHome[batternumberHome,out]+0)
    boxscoreHome[batternumberHome,walk]=ifelse(result=="walk",boxscoreHome[batternumberHome,walk]+1,boxscoreAway[batternumberHome,walk]+0)
    boxscoreHome[batternumberHome,PA]=boxscoreHome[batternumberHome,PA]+1
    
    if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
    
    
    if(DoublePlay=="Y"){FirstBase=0}
    if(DoublePlay=="Y"){HomeInningOut=HomeInningOut+1}
    
    
    if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
    
    
    if(DoublePlayLead=="Second"){FirstBase=batternumberHome}
    
    if(DoublePlayLead=="First"){SecondBase=FirstBase}
    if(DoublePlayLead=="First"){FirstBase=0}
    
    DoublePlay="N"
    DoublePlayLead="N"
    
    TagThird="N"
    if(ThirdBase !=0 & HitTo==7 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
    if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
    if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
    if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
    if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
    if(TagThird=="Y"){ThirdBase=0}
    
    
    if(ThirdBase !=0 & HitTo==3 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
    if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
    if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
    if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
    if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
    if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
    if(TagThird=="Y"){ThirdBase=0}
    
    TagSecond="N"
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
    if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
    if(TagSecond=="Y"){SecondBase=0}
    
    if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & HomeInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
    if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
    if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
    if(TagSecond=="Y"){SecondBase=0}
    
    
    
    
    
    TagFirst="N"
    if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
    if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
    if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
    if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
    if(TagFirst=="Y"){FirstBase=0}
    
    
    
    
    
    
    
    
    
    
    ################################
    
    
    
    SecondToSingle="N"
    if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
    
    
    if(SecondToSingle=="Third"){ThirdBase=SecondBase}
    if(SecondToSingle=="Home"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    if(SecondToSingle=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(SecondToSingle=="Out"){SecondBase=0}
    if(SecondBase != 0 & result=="Single"){SecondBase=0}
    if(SecondToSingle!="N"){SecondToSingle="N"}
    
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
    
    ######################
    
    
    FirstToSingle="N"
    if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
    
    if(FirstToSingle=="Second"){SecondBase=FirstBase}
    if(FirstToSingle=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
    if(FirstToSingle!="N"){FirstToSingle="N"}
    
    
    FirstToSingle="N"
    if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
    
    if(FirstToSingle=="Second"){SecondBase=FirstBase}
    if(FirstToSingle=="Third"){ThirdBase=FirstBase}
    if(FirstToSingle=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Single"){FirstBase=0}
    if(FirstToSingle!="N"){FirstToSingle="N"}
    
    FirstToDouble="N"
    if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
    
    if(FirstToDouble=="Third"){ThirdBase=FirstBase}
    if(FirstToDouble=="Home"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    if(FirstToDouble=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(FirstToDouble=="Out"){FirstBase=0}
    if(FirstBase != 0 & result=="Double"){FirstBase=0}
    if(FirstToDouble!="N"){FirstToDouble="N"}
    
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
    
    
    if(result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(result=="HomeRun"){boxscoreHome[batternumberHome,Runs]=boxscoreHome[batternumberHome,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    
    if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
    if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
    if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
    if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
    
    if(result=="Single"){FirstBase=batternumberHome}
    if(result=="Double"){SecondBase=batternumberHome}
    if(result=="Triple"){ThirdBase=batternumberHome}
    if(result=="walk"){FirstBase=batternumberHome}
    
    
    
    
    HomeInningOut=ifelse(result=="strikeout" | result=="Out",HomeInningOut+1,HomeInningOut)
    if(result=="Out") {pitcherboxscoreAway[Pitcher2,out]=pitcherboxscoreAway[Pitcher2,out]+1}
    if(result=="strikeout") {pitcherboxscoreAway[Pitcher2,strikeout]=pitcherboxscoreAway[Pitcher2,strikeout]+1}
    
    
    batternumberHome=batternumberHome+1
    HomeScore=sum(boxscoreHome$Runs)
    if(HomeScore>AwayScore) break
    if(HomeInningOut>2) break
  }
  }
  
  ############
  ##########
  #########
  #########
  
  
  
  
  
  
  if(HomeScore==AwayScore){
    
    repeat{  
      AwayInningOut=0
      FirstBase=0
      SecondBase=0
      ThirdBase=0
      
      repeat{
        Pitcher1Runs=as.matrix(pitcherboxscoreHome[which(pitcherboxscoreHome[,'PitcherNumber']==Pitcher1),])
        Pitcher1Runs=Pitcher1Runs[,'Runs']
        
        
        
        ReplaceHomePitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupA[2,Inning+3],pitcherlineupA[3,Inning+3],pitcherlineupA[4,Inning+3],pitcherlineupA[5,Inning+3],pitcherlineupA[6,Inning+3],pitcherlineupA[7,Inning+3]))
        if(PitchCount>Pitcher1Limit){Pitcher1=ReplaceHomePitcher} else if (Pitcher1Runs>Pitcher1MaxRuns){Pitcher1=ReplaceHomePitcher}
        if(PitchCount>Pitcher1Limit){PitchCount=0} else if (Pitcher1Runs>Pitcher1MaxRuns){PitchCount=0}
        pitcherlineupA[Pitcher1,4:13]=0
        pitcherlineupASum=sum(pitcherlineupA[,Inning+3])
        pitcherlineupAmultiplier=1/pitcherlineupASum
        pitcherlineupA[,Inning+3]=pitcherlineupA[,Inning+3]*pitcherlineupAmultiplier
        
        batternumberAway=ifelse(batternumberAway==10,1,batternumberAway)
        
        
        
        PitcherHand1=pitcherlineupA[,'PitchHand'][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
        PlayerHand1=batterlineupA[,"Hand"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
        Pitcher1Limit=pitcherlineupA[,"MaxPitch"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
        Pitcher1MaxRuns=pitcherlineupA[,"MaxRuns"][which(pitcherlineupA[,"PitcherNumber"]==Pitcher1)]
        if(PlayerHand1==3 & PitcherHand1==1){PlayerHand1=2}
        if(PlayerHand1==3 & PitcherHand1==2){PlayerHand1=1}
        
        ballscount=0
        strikescount=0
        DoesSwing="N"
        MakesContact="N"
        MakesFoul="N"
        StolenBaseOut="N"
        FoulOut="N"
        repeat{
          
          PitchThrowTypesubset=PitchThrowTypeA[which(PitchThrowTypeA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
          
          PitchThrowZonesubset=PitchThrowZoneA[which(PitchThrowZoneA[,'Extra']==as.numeric(paste0(Pitcher1,batternumberAway,ballscount,strikescount))),]
          
          Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
          
          Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
          
          
          Swingsubset=SwingA[which(SwingA[,'extra']==as.numeric(paste0(batternumberAway,ballscount,strikescount,Zone,Pitch,PitcherHand1))),]
          
          PitchCount=PitchCount+1
          
          
          DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
          
          
          ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
          
          strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
          
          
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubset=ChanceofContactA[which(ChanceofContactA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubset['CountFit'],(1-ChanceofContactsubset['CountFit'])))}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulA[which(foulA[,'BattingNumber']==batternumberAway),]}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==FirstBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
          
          if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
          
          
          if(Steal2nd=="Y"){SecondBase=FirstBase}
          if(Steal2nd=="Y"){FirstBase=0}
          if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
          if(Steal2nd=="Y"){boxscoreAway[SecondBase,SB]=boxscoreAway[SecondBase,SB]+1}
          
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseA[which(StolenBaseA[,'BattingNumber']==SecondBase & StolenBaseA[,'PitcherNumber']==Pitcher1),]}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
          
          if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
          
          
          if(Steal3rd=="Y"){ThirdBase=SecondBase}
          if(Steal3rd=="Y"){SecondBase=0}
          if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
          if(Steal3rd=="Y"){boxscoreAway[ThirdBase,SB]=boxscoreAway[ThirdBase,SB]+1}
          
          
          if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
          if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
          
          Steal2ndAttempt="N"
          Steal2nd="N"
          Steal3rdAttempt="N"
          Steal3rd="N"
          
          
          if(StolenBaseOut=="Y" & AwayInningOut + 1==3) break
          if(FoulOut=="Y")break
          if(MakesContact=="Y" & MakesFoul=="N") break
          if(ballscount==4) break
          if(strikescount==3) break
        }
        
        if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
          hitspeedsubset=hitspeedA[which(hitspeedA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
          
          launchanglesubset=launchangleA[which(launchangleA[,'Extra']==as.numeric(paste0(batternumberAway,Pitcher1,Pitch,Zone,ballscount,strikescount,PlayerHand1))),]
          
          
          hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
          hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
          hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
          launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
          hitspeednumber=round(hitspeednumber)
          launchanglenumber=round(launchanglenumber)
          launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
          launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
          
          
          AngleSubset=AngleA[which(AngleA[,'BattingNumber']==batternumberAway),]
          
          
          AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
          Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
          
          
          if(launchanglenumber >12 & hitspeednumber >88){
            FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
            Distance=as.numeric(FirstDistance[[1]])
            
            Feet2Meter=Distance*0.3048
            hangtime=as.numeric(FirstDistance[[2]])
            hangtime1=as.numeric(hangtime[[1]])
            NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
            
            ballheight=1
            WallHeight=ParkWallHeights[c("Angle",Stadium)]
            WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
            WallHeight$Angle=NULL
            WallHeightMetric=WallHeight*0.3048
            
            WallDistance=ParkFenceDistance[c("Angle",Stadium)]
            WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
            WallDistance$Angle=NULL
            WallDistanceMetric=WallDistance*0.3048
            HeightAtWall=Distance-(WallDistance+10)
          } else{
            Distance=1
            HeightAtWall=-5
          }
          
          
          hitspeednumber=round_any(hitspeednumber,5)
          launchanglenumber=round_any(launchanglenumber,5)
          
          HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
          HitTo1=batterlineupB[which(batterlineupB[,'Position']==HitTo),]
          HitToCatch=HitTo1['CatchPercent']
          
          
          
          hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
          
          
          
          
          hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
          
          
          hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
          
          SpeedAdded=batterlineupA[,"Speed"][which(batterlineupA[,"BattingNumber"]==batternumberAway)]
          SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
          
          
          
          hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
          hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
          ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==HomeTeam & ShiftPercentage[,"Hand"]==PlayerHand1),]
          ShiftSubset1=as.numeric(ShiftSubset["Shift"])
          ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
          ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
          ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
          ShiftNumber=hitpercentagesubset['Single'] *.033784
          hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
          hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
          if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
          
          
          hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
          hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
          hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
          hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
          hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
          
          result=hitpercentagesubset1
        }
        
        
        
        boxscoreAway[batternumberAway,Single]=ifelse(result=="Single",boxscoreAway[batternumberAway,Single]+1,boxscoreAway[batternumberAway,Single]+0)
        boxscoreAway[batternumberAway,Double]=ifelse(result=="Double",boxscoreAway[batternumberAway,Double]+1,boxscoreAway[batternumberAway,Double]+0)
        boxscoreAway[batternumberAway,Triple]=ifelse(result=="Triple",boxscoreAway[batternumberAway,Triple]+1,boxscoreAway[batternumberAway,Triple]+0)
        boxscoreAway[batternumberAway,HomeRun]=ifelse(result=="HomeRun",boxscoreAway[batternumberAway,HomeRun]+1,boxscoreAway[batternumberAway,HomeRun]+0)
        boxscoreAway[batternumberAway,strikeout]=ifelse(result=="strikeout",boxscoreAway[batternumberAway,strikeout]+1,boxscoreAway[batternumberAway,strikeout]+0)
        boxscoreAway[batternumberAway,out]=ifelse(result=="Out",boxscoreAway[batternumberAway,out]+1,boxscoreAway[batternumberAway,out]+0)
        boxscoreAway[batternumberAway,walk]=ifelse(result=="walk",boxscoreAway[batternumberAway,walk]+1,boxscoreAway[batternumberAway,walk]+0)
        boxscoreAway[batternumberAway,PA]=boxscoreAway[batternumberAway,PA]+1
        
        
        if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(ThirdBase!=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(ThirdBase !=0 & result=="Single"|ThirdBase !=0 & result=="Double"|ThirdBase !=0 & result=="Triple"|ThirdBase !=0 & result=="HomeRun"){ThirdBase=0}
        
        if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
        
        
        if(DoublePlay=="Y"){FirstBase=0}
        if(DoublePlay=="Y"){AwayInningOut=AwayInningOut+1}
        
        
        if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & AwayInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
        
        
        if(DoublePlayLead=="Second"){FirstBase=batternumberAway}
        
        if(DoublePlayLead=="First"){SecondBase=FirstBase}
        if(DoublePlayLead=="First"){FirstBase=0}
        
        DoublePlay="N"
        DoublePlayLead="N"
        
        TagThird="N"
        if(ThirdBase !=0 & HitTo==7 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
        if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
        if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
        if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
        if(TagThird=="Y"){ThirdBase=0}
        
        
        if(ThirdBase !=0 & HitTo==3 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & AwayInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
        if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
        if(ScoreThird=="Y" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(ScoreThird=="N" & TagThird=="Y"){boxscoreAway[ThirdBase,Runs]=0}
        if(ScoreThird=="N" & TagThird=="Y"){AwayInningOut=AwayInningOut +1}
        if(ScoreThird=="Y"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1} 
        if(TagThird=="Y"){ThirdBase=0}
        
        TagSecond="N"
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
        if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
        if(TagSecond=="Y"){SecondBase=0}
        
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & AwayInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & AwayInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & AwayInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
        if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
        if(TagSecond=="Y"){SecondBase=0}
        
        
        
        
        
        TagFirst="N"
        if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
        if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &AwayInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & AwayInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
        if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
        if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
        if(TagFirst=="Y"){FirstBase=0}
        
        
        
        
        
        ################################
        
        
        
        SecondToSingle="N"
        if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
        
        
        if(SecondToSingle=="Third"){ThirdBase=SecondBase}
        if(SecondToSingle=="Home"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(SecondToSingle=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(SecondToSingle=="Out"){SecondBase=0}
        if(SecondBase != 0 & result=="Single"){SecondBase=0}
        if(SecondToSingle!="N"){SecondToSingle="N"}
        
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[SecondBase,Runs]=boxscoreAway[SecondBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
        
        ######################
        
        
        FirstToSingle="N"
        if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
        
        if(FirstToSingle=="Second"){SecondBase=FirstBase}
        if(FirstToSingle=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
        if(FirstToSingle!="N"){FirstToSingle="N"}
        
        
        FirstToSingle="N"
        if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
        
        if(FirstToSingle=="Second"){SecondBase=FirstBase}
        if(FirstToSingle=="Third"){ThirdBase=FirstBase}
        if(FirstToSingle=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Single"){FirstBase=0}
        if(FirstToSingle!="N"){FirstToSingle="N"}
        
        FirstToDouble="N"
        if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
        
        if(FirstToDouble=="Third"){ThirdBase=FirstBase}
        if(FirstToDouble=="Home"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(FirstToDouble=="Home"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(FirstToDouble=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Double"){FirstBase=0}
        if(FirstToDouble!="N"){FirstToDouble="N"}
        
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[FirstBase,Runs]=boxscoreAway[FirstBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
        
        
        if(result=="HomeRun"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(result=="HomeRun"){boxscoreAway[batternumberAway,Runs]=boxscoreAway[batternumberAway,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        
        if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[ThirdBase,Runs]=boxscoreAway[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher1,Runs]=pitcherboxscoreHome[Pitcher1,Runs]+1}
        if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreAway[batternumberAway,RBI]=boxscoreAway[batternumberAway,RBI]+1}
        if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
        if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
        
        if(result=="Single"){FirstBase=batternumberAway}
        if(result=="Double"){SecondBase=batternumberAway}
        if(result=="Triple"){ThirdBase=batternumberAway}
        if(result=="walk"){FirstBase=batternumberAway}
        
        
        
        
        AwayInningOut=ifelse(result=="strikeout" | result=="Out",AwayInningOut+1,AwayInningOut)
        if(result=="Out") {pitcherboxscoreHome[Pitcher1,out]=pitcherboxscoreHome[Pitcher1,out]+1}
        if(result=="strikeout") {pitcherboxscoreHome[Pitcher1,strikeout]=pitcherboxscoreHome[Pitcher1,strikeout]+1}
        
        
        batternumberAway=batternumberAway+1
        
        
        
        if(AwayInningOut>2) break
      }
      
      HomeScore=sum(boxscoreHome$Runs)
      AwayScore=sum(boxscoreAway$Runs)
      
      if(HomeScore<AwayScore){HomeInningOut=0
      FirstBase=0
      SecondBase=0
      ThirdBase=0
      
      repeat{
        Pitcher2Runs=as.matrix(pitcherboxscoreAway[which(pitcherboxscoreAway[,'PitcherNumber']==Pitcher2),])
        Pitcher2Runs=Pitcher2Runs[,'Runs']
        
        
        
        ReplaceAwayPitcher=sample(c(2,3,4,5,6,7),size=1,replace=TRUE,prob = c(pitcherlineupB[2,Inning+3],pitcherlineupB[3,Inning+3],pitcherlineupB[4,Inning+3],pitcherlineupB[5,Inning+3],pitcherlineupB[6,Inning+3],pitcherlineupB[7,Inning+3]))
        if(PitchCountAway>Pitcher2Limit){Pitcher2=ReplaceAwayPitcher} else if (Pitcher2Runs>Pitcher2MaxRuns){Pitcher2=ReplaceAwayPitcher}
        if(PitchCountAway>Pitcher2Limit){PitchCountAway=0} else if (Pitcher2Runs>Pitcher2MaxRuns){PitchCountAway=0}
        pitcherlineupB[Pitcher2,4:13]=0
        pitcherlineupBSum=sum(pitcherlineupB[,Inning+3])
        pitcherlineupBmultiplier=1/pitcherlineupBSum
        pitcherlineupB[,Inning+3]=pitcherlineupB[,Inning+3]*pitcherlineupBmultiplier
        
        batternumberHome=ifelse(batternumberHome==10,1,batternumberHome)
        
        
        
        
        PitcherHand2=pitcherlineupB[,'PitchHand'][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
        PlayerHand2=batterlineupB[,"Hand"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
        
        Pitcher2Limit=pitcherlineupB[,"MaxPitch"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
        Pitcher2MaxRuns=pitcherlineupB[,"MaxRuns"][which(pitcherlineupB[,"PitcherNumber"]==Pitcher2)]
        if(PlayerHand2==3 & PitcherHand2==1){PlayerHand2=2}
        if(PlayerHand2==3 & PitcherHand2==2){PlayerHand2=1}
        
        ballscount=0
        strikescount=0
        DoesSwing="N"
        MakesContact="N"
        MakesFoul="N"
        StolenBaseOut="N"
        FoulOut="N"
        repeat{
          
          PitchThrowTypesubset=PitchThrowTypeB[which(PitchThrowTypeB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
          
          PitchThrowZonesubset=PitchThrowZoneB[which(PitchThrowZoneB[,'Extra']==as.numeric(paste0(Pitcher2,batternumberHome,ballscount,strikescount))),]
          
          Pitch=sample(c(101,102,103,104,105,106,107,108,109,110,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowTypesubset['percentageFF'],PitchThrowTypesubset['percentageCU'],PitchThrowTypesubset['percentageSL'],PitchThrowTypesubset['percentageCH'],PitchThrowTypesubset['percentageFT'],PitchThrowTypesubset['percentageSI'],PitchThrowTypesubset['percentageFS'],PitchThrowTypesubset['percentageKC'],PitchThrowTypesubset['percentageIN'],PitchThrowTypesubset['percentagePO'],PitchThrowTypesubset['percentageUN'],PitchThrowTypesubset['percentageEP'],PitchThrowTypesubset['percentageSC'],PitchThrowTypesubset['percentageFC']))
          
          Zone=sample(c(101,102,103,104,105,106,107,108,109,111,112,113,114),size=1,replace=TRUE,prob=c(PitchThrowZonesubset['percentageL1'],PitchThrowZonesubset['percentageL2'],PitchThrowZonesubset['percentageL3'],PitchThrowZonesubset['percentageL4'],PitchThrowZonesubset['percentageL5'],PitchThrowZonesubset['percentageL6'],PitchThrowZonesubset['percentageL7'],PitchThrowZonesubset['percentageL8'],PitchThrowZonesubset['percentageL9'],PitchThrowZonesubset['percentageL11'],PitchThrowZonesubset['percentageL12'],PitchThrowZonesubset['percentageL13'],PitchThrowZonesubset['percentageL14']))
          
          
          Swingsubset=SwingB[which(SwingB[,'extra']==as.numeric(paste0(batternumberHome,ballscount,strikescount,Zone,Pitch,PitcherHand2))),]
          
          PitchCountAway=PitchCountAway+1
          
          
          DoesSwing=sample(c("Y","N"),size=1,replace=TRUE,prob=c(Swingsubset['realaveswing'],(1-Swingsubset['realaveswing'])))
          
          
          ballscount=ifelse(DoesSwing=="N" & Zone>109,ballscount+1,ballscount)
          
          strikescount=ifelse(DoesSwing=="N" & Zone<110,strikescount+1,strikescount)
          
          
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){ ChanceofContactsubsetB=ChanceofContactB[which(ChanceofContactB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){MakesContact=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceofContactsubsetB['CountFit'],(1-ChanceofContactsubsetB['CountFit'])))}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (DoesSwing=="Y"){strikescount=ifelse(MakesContact=="N" ,strikescount+1,strikescount)}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){ChanceFoul=foulB[which(foulB[,'BattingNumber']==batternumberHome),]}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){MakesFoul=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ChanceFoul['FoulPercentage'],(1-ChanceFoul['FoulPercentage'])))}
          
          if(strikescount==3){strikescount=3} else if (ballscount==4){ballscount=4} else if (MakesContact=="Y"){strikescount=ifelse(MakesFoul=="Y" & strikescount==0,strikescount+1,ifelse(MakesFoul=="Y" & strikescount==1,strikescount+1,2))}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="Y" & MakesFoul=="Y"){FoulOut=sample(c("Y","N"),size=1,replace = TRUE,prob=c(.2,.8))}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==FirstBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & FirstBase>0 & SecondBase==0){Steal2ndAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB2nd'],(1-StolenBasesubset['realSB2nd'])))}
          
          if(Steal2ndAttempt=='Y'){Steal2nd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
          
          
          if(Steal2nd=="Y"){SecondBase=FirstBase}
          if(Steal2nd=="Y"){FirstBase=0}
          if(Steal2ndAttempt=="Y" & Steal2nd=="N"){FirstBase=0}
          if(Steal2nd=="Y"){boxscoreHome[SecondBase,SB]=boxscoreHome[SecondBase,SB]+1}
          
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){StolenBasesubset=StolenBaseB[which(StolenBaseB[,'BattingNumber']==SecondBase & StolenBaseB[,'PitcherNumber']==Pitcher2),]}
          
          if(strikescount != 3 & ballscount != 4 & MakesContact =="N" & SecondBase>0 & ThirdBase==0){Steal3rdAttempt=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['realSB3rd'],(1-StolenBasesubset['realSB3rd'])))}
          
          if(Steal3rdAttempt=='Y'){Steal3rd=sample(c("Y","N"),size=1,replace = TRUE,prob=c(StolenBasesubset['RealSBPercent'],(1-StolenBasesubset['RealSBPercent'])))}
          
          
          if(Steal3rd=="Y"){ThirdBase=SecondBase}
          if(Steal3rd=="Y"){SecondBase=0}
          if(Steal3rdAttempt=="Y" & Steal3rd=="N"){SecondBase=0}
          if(Steal3rd=="Y"){boxscoreHome[ThirdBase,SB]=boxscoreHome[ThirdBase,SB]+1}
          
          
          if(Steal3rdAttempt=="Y" & Steal3rd=="N"){StolenBaseOut="Y"}
          if(Steal2ndAttempt=="Y" & Steal2nd=="N"){StolenBaseOut="Y"}
          
          Steal2ndAttempt="N"
          Steal2nd="N"
          Steal3rdAttempt="N"
          Steal3rd="N"
          
          
          if(StolenBaseOut=="Y" & HomeInningOut + 1==3) break
          if(FoulOut=="Y")break
          if(MakesContact=="Y" & MakesFoul=="N") break
          if(ballscount==4) break
          if(strikescount==3) break
        }
        
        if(strikescount==3){result="strikeout"} else if (ballscount==4){result="walk"} else if (FoulOut=="Y"){result="Out"}  else if (StolenBaseOut=="Y"){result="Out"}else{
          hitspeedsubset=hitspeedB[which(hitspeedB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
          
          launchanglesubset=launchangleB[which(launchangleB[,'Extra']==as.numeric(paste0(batternumberHome,Pitcher2,Pitch,Zone,ballscount,strikescount,PlayerHand2))),]
          
          
          hitspeednumber=rnorm(1,hitspeedsubset['RealLaunchSpeed'],hitspeedsubset['sd'])
          hitspeednumber=ifelse(hitspeednumber>115,115,hitspeednumber)
          hitspeednumber=ifelse(hitspeednumber<60,60,hitspeednumber)
          launchanglenumber=rnorm(1,launchanglesubset['Reallaunch_angle'],launchanglesubset['sd'])
          hitspeednumber=round(hitspeednumber)
          launchanglenumber=round(launchanglenumber)
          launchanglenumber=ifelse(launchanglenumber<(-65),-65,launchanglenumber)
          launchanglenumber=ifelse(launchanglenumber>(55),55,launchanglenumber)
          
          
          AngleSubset=AngleB[which(AngleB[,'BattingNumber']==batternumberHome),]
          
          
          AngleSubset1=sample(c(1,2,3,4,5),size=1,replace=TRUE,prob=c(AngleSubset['Angle1'],AngleSubset['Angle2'],AngleSubset['Angle3'],AngleSubset['Angle4'],AngleSubset['Angle5']))
          Anglenumber=ifelse(AngleSubset1==1,round(runif(1,27,45)),ifelse(AngleSubset1==2,round(runif(1,9,26)),ifelse(AngleSubset1==3,round(runif(1,-8.5,8.5)),ifelse(AngleSubset1==4,round(runif(1,-27.6,-8.49)),ifelse(AngleSubset1==5,round(runif(1,-45,-28)),1)))))
          
          
          if(launchanglenumber >12 & hitspeednumber >88){
            FirstDistance=trajectory(Temperature,Park,hitspeednumber,launchanglenumber,wind,Humidity)
            Distance=as.numeric(FirstDistance[[1]])
            
            Feet2Meter=Distance*0.3048
            hangtime=as.numeric(FirstDistance[[2]])
            hangtime1=as.numeric(hangtime[[1]])
            NewExitVelocity=sqrt((Feet2Meter/hangtime1)^2+((9.80665*hangtime1)/2)^2)
            
            ballheight=1
            WallHeight=ParkWallHeights[c("Angle",Stadium)]
            WallHeight=WallHeight[which(WallHeight$Angle==Anglenumber),]
            WallHeight$Angle=NULL
            WallHeightMetric=WallHeight*0.3048
            
            WallDistance=ParkFenceDistance[c("Angle",Stadium)]
            WallDistance=WallDistance[which(WallDistance$Angle==Anglenumber),]
            WallDistance$Angle=NULL
            WallDistanceMetric=WallDistance*0.3048
            HeightAtWall=Distance-(WallDistance+10)
          } else{
            Distance=1
            HeightAtWall=-5
          }
          
          hitspeednumber=round_any(hitspeednumber,5)
          launchanglenumber=round_any(launchanglenumber,5)
          
          HitTo=ifelse(Distance>200 & Anglenumber>27.5,9,ifelse(Distance>200 & Anglenumber<27.49999999 & Anglenumber>-27.4999999,8,ifelse(Distance>200  & Anglenumber<=-27.49999999,7,ifelse(Distance<=200  & Anglenumber<=-29.999999999,5,ifelse(Distance<=200  & Anglenumber>=-30 & Anglenumber<=-0.00000000001,6,ifelse(Distance<=200  & Anglenumber>=0  & Anglenumber<=29.99999,4,ifelse(Distance<=200  & Anglenumber>=30,3,1)))))))
          HitTo1=batterlineupA[which(batterlineupA[,'Position']==HitTo),]
          HitToCatch=HitTo1['CatchPercent']
          
          
          
          hitpercentagesubset=hitpercentage[which(hitpercentage[,"launch_angle"]==launchanglenumber & hitpercentage[,"launch_speed"]==hitspeednumber & hitpercentage[,"Angle"]==AngleSubset1),]
          
          
          
          
          hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['out'] + HitToCatch,hitpercentagesubset['out'])
          
          
          hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + HitToCatch > 0 & hitpercentagesubset['out'] + HitToCatch < 1 & hitpercentagesubset['Single'] - HitToCatch < 1& hitpercentagesubset['Single'] - HitToCatch < 1,hitpercentagesubset['Single'] - HitToCatch,hitpercentagesubset['Single'])
          
          SpeedAdded=batterlineupB[,"Speed"][which(batterlineupB[,"BattingNumber"]==batternumberHome)]
          SpeedAdded1=SpeedAdded*hitpercentagesubset['Single']
          
          
          
          hitpercentagesubset['out']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['out'] + SpeedAdded1,hitpercentagesubset['out'])
          hitpercentagesubset['Single']=ifelse(hitpercentagesubset['out'] + SpeedAdded1 > 0 & hitpercentagesubset['out'] + SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1 & hitpercentagesubset['Single'] - SpeedAdded1 < 1,hitpercentagesubset['Single'] - SpeedAdded1,hitpercentagesubset['Single'])
          ShiftSubset=ShiftPercentage[which(ShiftPercentage[,"Team"]==AwayTeam & ShiftPercentage[,"Hand"]==PlayerHand2),]
          ShiftSubset1=as.numeric(ShiftSubset["Shift"])
          ShiftSubset2=sample(c("Y","N"),size=1,replace=TRUE,prob=c(ShiftSubset1,(1-ShiftSubset1)))
          ShiftSubset2=ifelse(PitchThrowTypesubset['RealHand']==1,"N",ShiftSubset2)
          ShiftSubset2=ifelse(FirstBase >0 | SecondBase > 0 | ThirdBase ,"N",ShiftSubset2)
          ShiftNumber=hitpercentagesubset['Single'] *.033784
          hitpercentagesubset['out']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['out']+ShiftNumber,hitpercentagesubset['out'])
          hitpercentagesubset['Single']=ifelse(ShiftSubset2=="Y",hitpercentagesubset['Single']-ShiftNumber,hitpercentagesubset['Single'])
          if(hitpercentagesubset['Single']<0){hitpercentagesubset['Single']=0}
          
          
          hitpercentagesubset['Sum']=sum(hitpercentagesubset[4:7])
          hitpercentagesubset['multiple']=1/hitpercentagesubset['Sum']
          hitpercentagesubset[4:7]=hitpercentagesubset[1,4:7]*hitpercentagesubset[1,'multiple']
          hitpercentagesubset1=sample(c("Single","Double","Triple","Out"),size=1,replace=TRUE,prob=c(hitpercentagesubset['Single'],hitpercentagesubset['Double'],hitpercentagesubset['Triple'],hitpercentagesubset['out']))
          hitpercentagesubset1=ifelse(HeightAtWall>0,"HomeRun",hitpercentagesubset1)
          
          result=hitpercentagesubset1
        }
        
        
        
        boxscoreHome[batternumberHome,Single]=ifelse(result=="Single",boxscoreHome[batternumberHome,Single]+1,boxscoreHome[batternumberHome,Single]+0)
        boxscoreHome[batternumberHome,Double]=ifelse(result=="Double",boxscoreHome[batternumberHome,Double]+1,boxscoreHome[batternumberHome,Double]+0)
        boxscoreHome[batternumberHome,Triple]=ifelse(result=="Triple",boxscoreHome[batternumberHome,Triple]+1,boxscoreHome[batternumberHome,Triple]+0)
        boxscoreHome[batternumberHome,HomeRun]=ifelse(result=="HomeRun",boxscoreHome[batternumberHome,HomeRun]+1,boxscoreHome[batternumberHome,HomeRun]+0)
        boxscoreHome[batternumberHome,strikeout]=ifelse(result=="strikeout",boxscoreHome[batternumberHome,strikeout]+1,boxscoreHome[batternumberHome,strikeout]+0)
        boxscoreHome[batternumberHome,out]=ifelse(result=="Out",boxscoreHome[batternumberHome,out]+1,boxscoreHome[batternumberHome,out]+0)
        boxscoreHome[batternumberHome,walk]=ifelse(result=="walk",boxscoreHome[batternumberHome,walk]+1,boxscoreAway[batternumberHome,walk]+0)
        boxscoreHome[batternumberHome,PA]=boxscoreHome[batternumberHome,PA]+1
        
        if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2){DoublePlay=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.12,.88))}
        
        
        if(DoublePlay=="Y"){FirstBase=0}
        if(DoublePlay=="Y"){HomeInningOut=HomeInningOut+1}
        
        
        if(result=="Out" & HitTo==4 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==3 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==5 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"|result=="Out" & HitTo==6 & launchanglenumber <(3) & FirstBase>0 & HomeInningOut<2 & DoublePlay=="N"){DoublePlayLead=sample(c("First","Second"),size=1,replace=TRUE,prob=c(.5,.5))}
        
        
        if(DoublePlayLead=="Second"){FirstBase=batternumberHome}
        
        if(DoublePlayLead=="First"){SecondBase=FirstBase}
        if(DoublePlayLead=="First"){FirstBase=0}
        
        DoublePlay="N"
        DoublePlayLead="N"
        
        TagThird="N"
        if(ThirdBase !=0 & HitTo==7 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==8 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.92,.08))}
        if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
        if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
        if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
        if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
        if(TagThird=="Y"){ThirdBase=0}
        
        
        if(ThirdBase !=0 & HitTo==3 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==4 & result=="Out" &HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2|ThirdBase !=0 & HitTo==5 & result=="Out" & HomeInningOut <2){TagThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.5,.5))}
        if(TagThird=="Y"){ScoreThird=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.90,.1))}
        if(ScoreThird=="Y" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreHome[Pitcher2,Runs]=pitcherboxscoreHome[Pitcher2,Runs]+1}
        if(ScoreThird=="N" & TagThird=="Y"){boxscoreHome[ThirdBase,Runs]=0}
        if(ScoreThird=="N" & TagThird=="Y"){HomeInningOut=HomeInningOut +1}
        if(ScoreThird=="Y"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1} 
        if(TagThird=="Y"){ThirdBase=0}
        
        TagSecond="N"
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
        if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
        if(TagSecond=="Y"){SecondBase=0}
        
        if(SecondBase !=0 & ThirdBase ==0 & HitTo==3 & result=="Out" & HomeInningOut <2 |SecondBase !=0 & ThirdBase ==0 & HitTo==4 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==5 & result=="Out" & HomeInningOut <2|SecondBase !=0 & ThirdBase ==0 & HitTo==6 & result=="Out" & HomeInningOut <2){TagSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.40,.6))}
        if(TagSecond=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.95,.05))}
        if(MoveSecond=="Y" & TagSecond=="Y"){ThirdBase=SecondBase}
        if(TagSecond=="Y"){SecondBase=0}
        
        
        
        
        
        TagFirst="N"
        if(FirstBase !=0 & ThirdBase ==0 & HitTo==7 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.52,.48))}
        if(FirstBase !=0 & ThirdBase ==0 & HitTo==8 & result=="Out" &HomeInningOut <2|FirstBase !=0 & ThirdBase ==0 & HitTo==9 & result=="Out" & HomeInningOut <2){TagFirst=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.75,.25))}
        if(TagFirst=="Y"){MoveSecond=sample(c("Y","N"),size=1,replace=TRUE,prob=c(.9,.1))}
        if(MoveFirst=="Y" & TagFirst=="Y"){SecondBase=FirstBase}
        if(TagFirst=="Y"){FirstBase=0}
        
        
        
        
        
        
        
        
        
        
        ################################
        
        
        
        SecondToSingle="N"
        if(SecondBase != 0 & result=="Single"){SecondToSingle=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.37,.58,.02))}
        
        
        if(SecondToSingle=="Third"){ThirdBase=SecondBase}
        if(SecondToSingle=="Home"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        if(SecondToSingle=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(SecondToSingle=="Out"){SecondBase=0}
        if(SecondBase != 0 & result=="Single"){SecondBase=0}
        if(SecondToSingle!="N"){SecondToSingle="N"}
        
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[SecondBase,Runs]=boxscoreHome[SecondBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(SecondBase !=0 & result=="Double"|SecondBase !=0 & result=="Triple"|SecondBase !=0 & result=="HomeRun"){SecondBase=0}
        
        ######################
        
        
        FirstToSingle="N"
        if(FirstBase != 0 & result=="Single" & ThirdBase!=0){FirstToSingle=sample(c("Second","Out"),size=1,replace=TRUE,prob=c(.98,.02))}
        
        if(FirstToSingle=="Second"){SecondBase=FirstBase}
        if(FirstToSingle=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Single" & ThirdBase !=0){FirstBase=0}
        if(FirstToSingle!="N"){FirstToSingle="N"}
        
        
        FirstToSingle="N"
        if(FirstBase != 0 & result=="Single" & ThirdBase==0){FirstToSingle=sample(c("Second","Third","Out"),size=1,replace=TRUE,prob=c(.7,.28,.02))}
        
        if(FirstToSingle=="Second"){SecondBase=FirstBase}
        if(FirstToSingle=="Third"){ThirdBase=FirstBase}
        if(FirstToSingle=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Single"){FirstBase=0}
        if(FirstToSingle!="N"){FirstToSingle="N"}
        
        FirstToDouble="N"
        if(FirstBase != 0 & result=="Double" ){FirstToDouble=sample(c("Third","Home","Out"),size=1,replace=TRUE,prob=c(.57,.41,.02))}
        
        if(FirstToDouble=="Third"){ThirdBase=FirstBase}
        if(FirstToDouble=="Home"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        if(FirstToDouble=="Home"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(FirstToDouble=="Out"){FirstBase=0}
        if(FirstBase != 0 & result=="Double"){FirstBase=0}
        if(FirstToDouble!="N"){FirstToDouble="N"}
        
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[FirstBase,Runs]=boxscoreHome[FirstBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(FirstBase !=0 & result=="Triple"|FirstBase !=0 & result=="HomeRun"){FirstBase=0}
        
        
        if(result=="HomeRun"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(result=="HomeRun"){boxscoreHome[batternumberHome,Runs]=boxscoreHome[batternumberHome,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        
        if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[ThirdBase,Runs]=boxscoreHome[ThirdBase,Runs]+1 ; pitcherboxscoreAway[Pitcher2,Runs]=pitcherboxscoreAway[Pitcher2,Runs]+1}
        if(FirstBase !=0 & SecondBase != 0 & ThirdBase !=0 & result=="walk"){boxscoreHome[batternumberHome,RBI]=boxscoreHome[batternumberHome,RBI]+1}
        if(FirstBase !=0 & SecondBase != 0 & result=="walk"){ThirdBase=SecondBase}
        if(FirstBase !=0 & result=="walk"){SecondBase=FirstBase}
        
        if(result=="Single"){FirstBase=batternumberHome}
        if(result=="Double"){SecondBase=batternumberHome}
        if(result=="Triple"){ThirdBase=batternumberHome}
        if(result=="walk"){FirstBase=batternumberHome}
        
        
        
        
        HomeInningOut=ifelse(result=="strikeout" | result=="Out",HomeInningOut+1,HomeInningOut)
        if(result=="Out") {pitcherboxscoreAway[Pitcher2,out]=pitcherboxscoreAway[Pitcher2,out]+1}
        if(result=="strikeout") {pitcherboxscoreAway[Pitcher2,strikeout]=pitcherboxscoreAway[Pitcher2,strikeout]+1}
        
        
        batternumberHome=batternumberHome+1
        HomeScore=sum(boxscoreHome$Runs)
        if(HomeScore>AwayScore) break
        if(HomeInningOut>2) break
        
      }
      if(HomeScore!=AwayScore) break
      }
      
      
      
      
      
      
    }
    
  } 
  
  boxscoreAway$Fantasy=(boxscoreAway$Runs*3.2)+(boxscoreAway$Single * 3)+(boxscoreAway$Double * 6)+(boxscoreAway$Triple*9)+(boxscoreAway$Home.Run * 12)+(boxscoreAway$Walk * 3) + (boxscoreAway$RBI * 3.5)+(boxscoreAway$SB * 6)
  boxscoreAway$Fantasy2=(boxscoreAway$Runs*2)+(boxscoreAway$Single * 3)+(boxscoreAway$Double * 5)+(boxscoreAway$Triple*8)+(boxscoreAway$Home.Run * 10)+(boxscoreAway$Walk * 2) + (boxscoreAway$RBI * 2)+(boxscoreAway$SB * 5)
  
  boxscoreHome$Fantasy=(boxscoreHome$Runs*3.2)+(boxscoreHome$Single * 3)+(boxscoreHome$Double * 6)+(boxscoreHome$Triple*9)+(boxscoreHome$Home.Run * 12)+(boxscoreHome$Walk * 3) + (boxscoreHome$RBI * 3.5)+(boxscoreHome$SB * 6)
  boxscoreHome$Fantasy2=(boxscoreHome$Runs*2)+(boxscoreHome$Single * 3)+(boxscoreHome$Double * 5)+(boxscoreHome$Triple*8)+(boxscoreHome$Home.Run * 10)+(boxscoreHome$Walk * 2) + (boxscoreHome$RBI * 2)+(boxscoreHome$SB * 5)
  
  pitcherboxscoreAway$QS=ifelse((pitcherboxscoreAway$Strike.Out+pitcherboxscoreAway$Out)>18 & pitcherboxscoreAway$Runs<4,1,0)
  pitcherboxscoreHome$QS=ifelse((pitcherboxscoreHome$Strike.Out+pitcherboxscoreHome$Out)>18 & pitcherboxscoreHome$Runs<4,1,0)
  
  pitcherboxscoreAway$Fantasy=(pitcherboxscoreAway$Strike.Out*4)+(pitcherboxscoreAway$Out * 1)+(pitcherboxscoreAway$QS * 4)+(pitcherboxscoreAway$Runs * -3)+(pitcherboxscoreAway$Win * 6)
  pitcherboxscoreHome$Fantasy=(pitcherboxscoreHome$Strike.Out*4)+(pitcherboxscoreHome$Out * 1)+(pitcherboxscoreHome$QS * 4)+(pitcherboxscoreHome$Runs * -3)+(pitcherboxscoreHome$Win * 6)
  
  FantasyModelAway[,(i+1)]=boxscoreAway$Fantasy
  FantasyModelHome[,(i+1)]=boxscoreHome$Fantasy
  
  FantasyPitcherModelAway[,(i+1)]=pitcherboxscoreAway$Fantasy
  FantasyPitcherModelHome[,(i+1)]=pitcherboxscoreHome$Fantasy
  
  Score[1,(i+1)]=AwayScore
  Score[2,(i+1)]=HomeScore
  
  PitcherK[1,(i +1)]=pitcherboxscoreAway[1,2]
  PitcherK[2,(i +1)]=pitcherboxscoreHome[1,2]
  
  BatterSBAway[,(i+1)]=boxscoreAway[,12]
  BatterSBHome[,(i+1)]=boxscoreHome[,12]
  
  BatterHRRAway[,(i+1)]=boxscoreAway[,"Runs"]+boxscoreAway[,"Single"]+boxscoreAway[,"Double"]+boxscoreAway[,"Triple"]+boxscoreAway[,"Home.Run"]+boxscoreAway[,"RBI"]
  BatterHRRHome[,(i+1)]=boxscoreHome[,"Runs"]+boxscoreHome[,"Single"]+boxscoreHome[,"Double"]+boxscoreHome[,"Triple"]+boxscoreHome[,"Home.Run"]+boxscoreHome[,"RBI"]
  
  BatterHitsAway[,(i+1)]=boxscoreAway[,"Single"]+boxscoreAway[,"Double"]+boxscoreAway[,"Triple"]+boxscoreAway[,"Home.Run"]
  BatterHitsHome[,(i+1)]=boxscoreHome[,"Single"]+boxscoreHome[,"Double"]+boxscoreHome[,"Triple"]+boxscoreHome[,"Home.Run"]
  
  BatterHRAway[,(i+1)]=boxscoreAway[,"Home.Run"]
  BatterHRHome[,(i+1)]=boxscoreHome[,"Home.Run"]
  
  BatterOutAway[,(i+1)]=boxscoreAway[,2]+boxscoreAway[,3]
  BatterOutHome[,(i+1)]=boxscoreHome[,2]+boxscoreHome[,3]
  
  BatterWalkAway[,(i+1)]=boxscoreAway[,"Walk"]
  BatterWalkHome[,(i+1)]=boxscoreHome[,"Walk"]
  
  BatterTotalBasesAway[,(i+1)]=boxscoreAway[,"Single"]+(boxscoreAway[,"Double"]*2)+(boxscoreAway[,"Triple"]*3)+(boxscoreAway[,"Home.Run"]*4)
  BatterTotalBasesHome[,(i+1)]=boxscoreHome[,"Single"]+(boxscoreHome[,"Double"]*2)+(boxscoreHome[,"Triple"]*3)+(boxscoreHome[,"Home.Run"]*4)
  
  PitcherRunsAway[,(i+1)]=pitcherboxscoreAway[,"Runs"]
  PitcherRunsHome[,(i+1)]=pitcherboxscoreHome[,"Runs"]
  
  PitcherOutsAway[,(i+1)]=pitcherboxscoreAway[,"Strike.Out"]+pitcherboxscoreAway[1,"Out"]
  PitcherOutsHome[,(i+1)]=pitcherboxscoreHome[,"Strike.Out"]+pitcherboxscoreHome[1,"Out"]
}


BatterAveragesAway=BatterHitsAway[1]
BatterAveragesAway$BA=rowSums(BatterHitsAway[,-1])/(rowSums(BatterOutAway[,-1])+rowSums(BatterHitsAway[,-1]))
BatterAveragesAway$OBP=(rowSums(BatterHitsAway[,-1])+rowSums(BatterWalkAway[,-1]))/(rowSums(BatterOutAway[,-1])+rowSums(BatterHitsAway[,-1])+rowSums(BatterWalkAway[,-1]))


BatterAveragesHome=BatterHitsHome[1]
BatterAveragesHome$BA=rowSums(BatterHitsHome[,-1])/(rowSums(BatterOutHome[,-1])+rowSums(BatterHitsHome[,-1]))
BatterAveragesHome$OBP=(rowSums(BatterHitsHome[,-1])+rowSums(BatterWalkHome[,-1]))/(rowSums(BatterOutHome[,-1])+rowSums(BatterHitsHome[,-1])+rowSums(BatterWalkHome[,-1]))


PitcherERAAway=PitcherOutsAway[1]
PitcherERAAway$ERA=(9*rowSums(PitcherRunsAway[,-1]))/(rowSums(PitcherOutsAway[,-1])/3)


PitcherERAHome=PitcherOutsHome[1]
PitcherERAHome$ERA=(9*rowSums(PitcherRunsHome[,-1]))/(rowSums(PitcherOutsHome[,-1])/3)


BatterAveragesAway
BatterAveragesHome
PitcherERAAway
PitcherERAHome
Score
rowMeans(Score[,-1])
boxscoreAway
boxscoreHome

