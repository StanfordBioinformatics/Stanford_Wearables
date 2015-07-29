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

#load moves basis comparison RData
load("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/Moves/moves_basis_comparison.RData")

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

#loop all date to get the duration time and sum basis data into
#then map into the pre-set dataM_moves matrix
movesDate<-substr(moves605_611[,"Start"], 1, 10)#date in moves
for (i in 1:length(dates)) {
  idDate<-as.numeric(which(movesDate==dates[i]))#row number in moves matrix
  idDate_moves<-as.numeric(which(dataM_moves[,10]==dates[i]))# row number in dataM_moves matrix
  idDate_basis<-as.numeric(which(dataM_all[,10]==dates[i]))# row number in dataM_all matrix
  for (j in 1:length(idDate)){#convert hrs and mins, into mins
    rowNum<-idDate[j]#get the row number of each date
    startHrs<-as.numeric(as.character(substr(moves605_611[rowNum, "Start"], 12, 13)))
    startMins<-as.numeric(as.character(substr(moves605_611[rowNum, "Start"], 15, 16)))
    startTime<-startHrs*60+startMins
    endHrs<-as.numeric(as.character(substr(moves605_611[rowNum, "End"], 12, 13)))
    endMins<-as.numeric(as.character(substr(moves605_611[rowNum, "End"], 15, 16)))
    endTime<-endHrs*60+endMins
    #the starting row to put the moves data in dataM_moves matrix
    id<-which(as.numeric(dataM_moves[idDate_moves,9])==startTime)+idDate_moves[1]-1
    #the starting row to put the moves data in dataM_moves matrix
    #     id_basis<-which(as.numeric(dataM_all[idDate_basis,9])==startTime)+idDate_basis[1]-1
    minLen<-endTime-startTime#get the length of duration
    for (k in id:(id+minLen)) {
      #         dataM_moves[k,"steps"]<-as.numeric(moves605_611[rowNum, "Steps"])#/minLen#break down steps in mins
      #         dataM_moves[k,"distance"]<-as.numeric(moves605_611[rowNum, "Distance"])#/minLen
      #         dataM_moves[k,"activity"]<-as.character(moves605_611[rowNum, "Activity"])
      #get the steps distance and activity into the dataM_moves matrix
      dataM_moves[k,"steps"]<-as.numeric(moves605_611[rowNum, "Steps"])
      dataM_moves[k,"distance"]<-as.numeric(moves605_611[rowNum, "Distance"])
      dataM_moves[k,"activity"]<-as.character(moves605_611[rowNum, "Activity"])
    }
    #get the steps distance and activity into the dataM_all matrix,need to count those time that basis doesn't have
    if (length(which(as.numeric(dataM_all[idDate_basis,9])==startTime))!=0){
      id_basis<-which(as.numeric(dataM_all[idDate_basis,9])==startTime)+idDate_basis[1]-1
    }
    for (h in id_basis:(id_basis+minLen)) {
      dataM_all[h,"steps"]<-as.numeric(moves605_611[rowNum, "Steps"])
      #       dataM_all[h,"distance"]<-as.numeric(moves605_611[rowNum, "Distance"])
      #       dataM_all[h,"activity"]<-as.character(moves605_611[rowNum, "Activity"])
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
#set parameter for easy change
numDay<-7
numXlim<-10079
for (i in 1:numDay) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot_temp<-as.numeric(which(dataM_moves[,10]==dates[i]))
  numPlot<-as.numeric(which(dataM_moves[numPlot_temp, "activity"]=="walking"))#get the rows that have corresponding activity
  numPlot<-numPlot_temp[numPlot]
  dateTime<-0
  #dateTime<-as.numeric(times[numPlot]+1440*(i-1)), plot by activity
  dateTime<-as.numeric(times[numPlot_temp]+1440*(i-1))#plot by days
  date_time<-c(date_time, dateTime)
  maxP<-max(as.numeric(dataM_moves[,para]), na.rm=T)
  minP<-min(as.numeric(dataM_moves[,para]), na.rm=T)#add min to stretch the value region
#   plot(dateTime, as.numeric(dataM_moves[numPlot,para]), type="h", lty=1,
#        xlab="", ylab="", col=i, lwd=2, 
#        ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, numXlim), cex=0.8)
plot(dateTime, as.numeric(dataM_moves[numPlot_temp,para]), type="h", lty=1,
     xlab="", ylab="", col=i, lwd=2, 
     ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, numXlim), cex=0.8)#plot by days
par(new=T)
}
title(xlab="Hour", ylab=colnames(dataM_moves)[para])
#give the label name
date.F<-c("2014-06-05","2014-06-06","2014-06-07", "2014-06-08", 
          "2014-06-09","2014-06-10","2014-06-11")
legend("topleft",inset=0, date.F, 
       col=1:length(dateName), lty=1, bty="n", cex=0.9, horiz=T, title="Moves Steps Taken by Days")
# lablist<-as.vector(rep(c(0,3,6,9,12,15,18,21),7))
lablist<-as.vector(rep(c(0:11)*2,numDay))
axis(1, at=seq(1, numXlim+1, by=120), labels=lablist, tick=T, las=3)

##use the date_time plotted in the moves plot
##to plot the corresponding steps from basis
date_time1<-date_time[-1]#minus the first 0 number
par(new=F)
#find the differene between basis time and move time
# diff<-setdiff(date_time1, as.numeric(dataM_all[,9]))
# if (length(which(as.numeric(dataM_all[,9])==startTime))!=0){
plot(date_time1, as.numeric(dataM_all[date_time1,"steps"]), type="h", lty=1,
     xlab="", ylab="", col="red", lwd=2, 
     ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, numXlim), cex=0.8)
legend("top",inset=0.1, "Basis Steps During Moves' Walking", 
       col="red", lty=c(rep(1,length(dateName))), bty="n", cex=0.9, horiz=T)

##check correlation between moves steps and basis steps during walking
steps_basis<-as.numeric(dataM_all[date_time1,"steps"])
steps_moves<-as.numeric(dataM_moves[date_time1,"steps"])
tau<-cor.test(steps_moves, steps_basis, method="kendall")$estimate
plot(steps_moves, steps_basis,las=1, main=paste("Correlation between Moves Steps and Basis Steps during Walking (Kendall Regression, tau=", tau,")"))
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
