#####the above htmlToText function doesn' work
#####get better data format, csv file from moves
###upload the moves data from 3/21/2014 to 8/13/2014
activities <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/Moves/Mike_Moves_Data_iPhone5s/csv/full/activities.csv")
##9 parameters, [1] "Date"     "Activity" "Group"    "Start"    "End"      "Duration"
##[7] "Distance" "Steps"    "Calories", 3,208 observations
##dum code the activities into 1-5 numbers, 0 for no activities(still)
#airplane   cycling   running transport   walking 
#46       185        29       357      2591
#extract dates from 605-6-11 to compare with basis data
#find the beginning position of 605 and the ending position of 611 and 
#extract all of them together. Assuming that dates are continuous
begin605<-which(substr(activities$Start, 6, 10)=="06-05")[1]#the beginning position
#the length of 611 date
end611<-length(which(substr(activities$Start, 6, 10)=="06-11"))
end611<-which(substr(activities$Start, 6, 10)=="06-11")[end611]
moves605_611<-activities[begin605:end611,]
###need to break the start and end time into seconds data
##then fill in the gap with 0, then match with basis data

##run the mikeBasis_plotOptimization code to get the basis data from 605-611
##then get the moves steps, distance and activity data to compare

##make a matrix the same size of the dataM_all
dataM_basis<-dataM_all#get a copy of the basis data matrix from 605-611
dates<-unlist(unique(dataM_basis[,10]))#get all unique date in basis
# dataM_moves<-matrix(0, nrow=dim(dataM_basis)[1], ncol=dim(dataM_basis)[2])
dataM_moves<-matrix(0, nrow=length(dates)*60*24, ncol=dim(dataM_basis)[2])
colnames(dataM_moves)<-colnames(dataM_basis)
# dataM_moves[, 9:10]<-dataM_basis[,9:10]#assign date and min vector to the matirx
#some min points were missing in basis but are in moves, 
#so the matrix need to be complete dimension, 10800 rows
dataM_moves[, 9]<-rep(0:1439, 7)
dataM_moves[, 10]<-rep(dates, each=1440)
#add a column for distance, a parameter measured by moves
dataM_moves<-cbind(dataM_moves, dataM_moves[,8])
colnames(dataM_moves)[11]<-"distance"

#loop all date to break down moves data into mins
#then map into the pre-set dataM_moves matrix
movesDate<-substr(moves605_611[,"Start"], 1, 10)#date in moves
for (i in 1:length(dates)) {
  idDate<-as.numeric(which(movesDate==dates[i]))#row number in moves matrix
  idDate_basis<-as.numeric(which(dataM_moves[,10]==dates[i]))# row number in dataM_moves matrix
  for (j in 1:length(idDate)){#convert hrs and mins, into mins
    rowNum<-idDate[j]#get the row number of each
    startHrs<-as.numeric(as.character(substr(moves605_611[rowNum, "Start"], 12, 13)))
    startMins<-as.numeric(as.character(substr(moves605_611[rowNum, "Start"], 15, 16)))
    startTime<-startHrs*60+startMins
    endHrs<-as.numeric(as.character(substr(moves605_611[rowNum, "End"], 12, 13)))
    endMins<-as.numeric(as.character(substr(moves605_611[rowNum, "End"], 15, 16)))
    endTime<-endHrs*60+endMins
    id<-which(as.numeric(dataM_moves[idDate_basis,9])==startTime)+idDate_basis[1]-1#the starting row to put the moves data in
    if (startTime==endTime){
      dataM_moves[id,"steps"]<-as.numeric(moves605_611[rowNum, "Steps"])
      dataM_moves[id,"distance"]<-as.numeric(moves605_611[rowNum, "Distance"])
      dataM_moves[id,"activity"]<-as.character(moves605_611[rowNum, "Activity"])
    }else {
      minLen<-endTime-startTime
      for (k in id:(id+minLen)) {
        dataM_moves[k,"steps"]<-as.numeric(moves605_611[rowNum, "Steps"])/minLen#break down steps in mins
        dataM_moves[k,"distance"]<-as.numeric(moves605_611[rowNum, "Distance"])/minLen
        dataM_moves[k,"activity"]<-as.character(moves605_611[rowNum, "Activity"])
      }
    }
  }
}
###plot moves steps with basis steps during no moves (0 for moves, inactive for basis)
times<-as.numeric(dataM_moves[,9]) #the vector that has the time values
#get all the unique dates
dates<-unlist(unique(dataM_moves[,10]))
date_time<-0#store all the dateTime plotted
paraAll<-c(2,3,4,5,7,8)#position of parameter in the matrix
para<-paraAll[5]
par(new=FALSE)
par(mfrow=c(1,1), las=1, bty="l", cex=0.8)
for (i in 1:length(dates)) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot_temp<-as.numeric(which(dataM_moves[,10]==dates[i]))
  numPlot<-as.numeric(which(dataM_moves[numPlot_temp, "activity"]=="running"))#get the rows that have corresponding activity
  numPlot<-numPlot_temp[numPlot]
  dateTime<-0
  dateTime<-as.numeric(times[numPlot]+1440*(i-1))
  date_time<-c(date_time, dateTime)
  maxP<-max(as.numeric(dataM_moves[,para]), na.rm=T)
  minP<-min(as.numeric(dataM_moves[,para]), na.rm=T)#add min to stretch the value region
  plot(dateTime, as.numeric(dataM_moves[numPlot,para]), type="p", lty=1,
       xlab="", ylab="", col=i, lwd=2, 
       ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, 10079), cex=0.8)
  par(new=T)
}
title(xlab="Hour", ylab=colnames(dataM_moves)[para])
#give the label name
date.F<-c("2014-06-05","2014-06-06","2014-06-07","2014-06-08", 
          "2014-06-09","2014-06-10","2014-06-11")
legend("topleft",inset=0, date.F, 
       col=1:length(dateName), lty=1, bty="n", cex=0.9, horiz=T, title="Moves Steps During Running")
# lablist<-as.vector(rep(c(0,3,6,9,12,15,18,21),7))
lablist<-as.vector(rep(c(0:11)*2,7))
axis(1, at=seq(1, 10080, by=120), labels=lablist, tick=T, las=3)

##use the date_time plotted in the moves plot
##to plot the corresponding steps from basis
date_time1<-date_time[-1]#minus the first 0 number
par(new=T)
plot(date_time1, as.numeric(dataM_all[date_time1,"steps"]), type="p", lty=1,
     xlab="", ylab="", col="lightgray", lwd=2, 
     ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, 10079), cex=0.8)
legend("top",inset=0.1, "Basis Steps During Moves' Running", 
       col="lightgray", lty=c(rep(1,length(dateName))), bty="n", cex=0.9, horiz=T)

##check correlation between moves steps and basis steps during walking
steps_basis<-as.numeric(dataM_all[date_time1,"steps"])
steps_moves<-as.numeric(dataM_moves[date_time1,"steps"])
plot(steps_moves, steps_basis,las=1, main="Correlation between Moves Steps and Basis Steps during Walking (Kendall Regression, tau=0.11)")
cor.test(steps_moves, steps_basis, method="kendall")
abline(lm(steps_basis~steps_moves), col="red")



#basis steps at inactivity status
times<-as.numeric(dataM_all[,9]) #the vector that has the time values
#get all the unique dates
dates<-unlist(unique(dataM_all[,10]))
paraAll<-c(2,3,4,5,7,8)#position of parameter in the matrix
para<-paraAll[5]
par(new=FALSE)
par(mfrow=c(1,1), las=3, bty="l", cex=0.8)
for (i in 1:length(dates)) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot_temp<-as.numeric(which(dataM_all[,10]==dates[i]))
  numPlot<-as.numeric(which(dataM_all[numPlot_temp, "activity"]=="inactive"))#get the rows that have no moves
  numPlot<-numPlot_temp[numPlot]
  dateTime<-0
  dateTime<-as.numeric(times[numPlot]+1440*(i-1))
  maxP<-max(as.numeric(dataM_all[,para]), na.rm=T)
  minP<-min(as.numeric(dataM_all[,para]), na.rm=T)#add min to stretch the value region
  plot(dateTime, as.numeric(dataM_all[numPlot,para]), type="p", pch=".", lty=1,
       xlab="Hour", ylab=colnames(dataM_all)[para], col=i, lwd=2, 
       ylim=c(minP, maxP+5), xaxt="n", xlim=c(0, 10079), cex=0.8)
  par(new=T)
}
#give the label name
date.F<-c("2014-06-05","2014-06-06","2014-06-07","2014-06-08", 
          "2014-06-09","2014-06-10","2014-06-11")
# date.f<-factor(date.F, levels=date.F)
# legend("bottomleft", inset=.01, date.F, 
#        col=1:length(dateName), lty=c(rep(1,length(dateName))), bty="n", cex=0.8)
#the below labeling, use date, but removed the hour info, so use hour info and 
#use color to show date
# lablist<-as.vector(date.F)
# axis(1, at=seq(1, 10080, by=1440), labels=F, tick=T)
# text(seq(720, 10080, by=1440), par("usr")[3] - 0.2, labels=lablist, pos=1, xpd=T)
legend("topleft",inset=0, date.F, 
       col=1:length(dateName), lty=c(rep(1,length(dateName))), bty="n", cex=0.9, horiz=T)
# lablist<-as.vector(rep(c(0,3,6,9,12,15,18,21),7))
lablist<-as.vector(rep(c(0:11)*2,7))
axis(1, at=seq(1, 10080, by=120), labels=lablist, tick=T, las=3)
# text(seq(1, 10080, by=120), par("usr")[3] , labels=lablist, pos=1, srt=90)
