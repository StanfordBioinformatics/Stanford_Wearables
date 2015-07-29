###Mike's Basis 1 data analysis, trying different plot type
#to best visualize longitudinal data
## got raw data from Denis in the online box folder
## data from 8/7/14-8/13/14
#each date is in a separate file, so import individually and combine them to fit the pipeline
mpsnyder_20140807 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140807.csv")
mpsnyder_20140808 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140808.csv")
mpsnyder_20140809 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140809.csv")
mpsnyder_20140810 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140810.csv")
mpsnyder_20140811 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140811.csv")
mpsnyder_20140812 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140812.csv")
mpsnyder_20140813 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140813.csv")
#combine all
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807, mpsnyder_20140808)
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807_20140813, mpsnyder_20140809)
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807_20140813, mpsnyder_20140810)
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807_20140813, mpsnyder_20140811)
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807_20140813, mpsnyder_20140812)
mpsnyder_20140807_20140813<-rbind(mpsnyder_20140807_20140813, mpsnyder_20140813)
# mpsnyder_20140807_20140813<-as.matrix(mpsnyder_20140807_20140813)#convert to matrix from data.frame
#total 604,807 data records with 8 variables
write.csv(mpsnyder_20140807_20140813, "mpsnyder_20140807_20140813.csv")
# mpsnyder_20140605_20140612 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/mpsnyder_20140605_20140612.csv")
mpsnyder_20140807_20140813 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/mpsnyder_20140807_20140813.csv")
mikeBasis<-mpsnyder_20140807_20140813#don't coerce to matrix
# mikeBasis<-mikeBasis[,-1]
# attach(mikeBasis)
#convert factor column into numeric
# mB_hr<-as.numeric(as.character(mikeBasis[,"hr"]))
# mB_accelMagnitude<-as.numeric(as.character(mikeBasis[,"accel_magnitude"]))
# mB_gsr<-as.numeric(as.character(mikeBasis[,"gsr"]))
# mB_skinTemp<-as.numeric(as.character(mikeBasis[,"skin_temp"]))
# mB_steps<-as.numeric(as.character(mikeBasis[,"steps"]))
# mB_calories<-as.numeric(as.character(mikeBasis[,"calories"]))
mB_time<-as.character(mikeBasis[,"time"])
# mB_activity<-as.character(mikeBasis[,"activity"])

#extract time by date and get the data points in each date
mB_date<-paste(substr(mB_time, 7, 7), substr(mB_time, 9, 10), sep="")
#convert each parameter into numeric data, not necessary
# mikeBasis[,4]<-as.numeric(as.character(mikeBasis[,4]))
# mikeBasis[,5]<-as.numeric(as.character(mikeBasis[,5]))
# mikeBasis[,7]<-as.numeric(as.character(mikeBasis[,7]))
# mikeBasis[,8]<-as.numeric(as.character(mikeBasis[,8]))

##plot by days
##get the points in each day
##use for loop and paste and assign to assign data to different variables based on date
##assign(paste...) to give value to the paste name
##get(paste...) to acquire value of the paste name
dateName<-c("807", "808", "809", "810", "811", "812", "813")
for (i in 1:length(dateName)){
  #num of data points in that date
  assign(paste("num_", dateName[i], sep=""),length(which(mB_date==dateName[i])))
  #position of the first data point for that date
  assign(paste("begin_", dateName[i], sep=""),which(mB_date==dateName[i])[1])
}
#separate each day's data
for (i in 1:length(dateName)){
  begin<-get(paste("begin_", dateName[i], sep=""))#get the variable value of the begin_(date)
  num<-get(paste("num_", dateName[i], sep=""))#get the variable value of the num_(date)
  assign(paste("data_", dateName[i], sep=""),mikeBasis[begin:(begin+num-1),])
}

##use loop to capture the average of second data for each minute
##for each dataset, data_605, data_606...., sum the seconds into minutes (steps and cal) or average (hr, gsr, skintemp, accel)
##then get the final dataset with a column of time converted to minutes and date factor column for plot group by date
n<-length(dateName)
# n<-1
for (k in 1:n){
  dataM<-get(paste("data_", dateName[k], sep=""))#dataM as the boilplate for each date data
  #get the unique hour number and minute number
  hours<-unlist(unique(as.numeric(as.character(substr(dataM[,1], 12, 13)))))
  minutes<-unlist(unique(as.numeric(as.character(substr(dataM[,1], 15, 16)))))
  ##do loops to average seconds data for each minute
  totalRows<-length(hours)*length(minutes) #total rows of dataset by minute
  dMinute<-matrix(0, nrow=totalRows, ncol=8, dimnames=list(1:totalRows, colnames(mikeBasis)))
  num=1 #counting the loops
  for (i in hours) {
#for (i in 0) {
    dataHours<-dataM[which(as.numeric(as.character(substr(dataM[,1], 12, 13)))==i),]
    for (j in minutes) {
# for (j in 0) {
      dataMins<-dataHours[which(as.numeric(as.character(substr(dataHours[,1], 15, 16)))==j),]
      ids<-which(as.numeric(as.character(substr(dataMins[,1], 15, 16)))==j)
      dMinute[num,2]<-mean(as.numeric(as.character(dataMins[ids,2])), na.rm = TRUE)
      dMinute[num,3]<-mean(as.numeric(as.character(dataMins[ids,3])), na.rm = TRUE)
      dMinute[num,4]<-mean(as.numeric(as.character(dataMins[ids,4])), na.rm = TRUE)
      dMinute[num,5]<-mean(as.numeric(as.character(dataMins[ids,5])), na.rm = TRUE)
      dMinute[num,6]<-as.character(dataMins[1,6])
      dMinute[num,7]<-sum(as.numeric(as.character(dataMins[ids,7])), na.rm = TRUE)
      dMinute[num,8]<-sum(as.numeric(as.character(dataMins[ids,8])), na.rm = TRUE)
      #for the time string, use the sequential name, regardless whether there is a value or not
      #get the date time string regardless of whether there is a minute or not
      if(nchar(as.character(i))<2) {
        hrs<-paste(0, i, sep="")
      }else {
        hrs<-i
      }
      if(nchar(as.character(j))<2) {
        mins<-paste(0, j, sep="")
      }else {
        mins<-j
      }
      dMinute[num,1]<-paste(paste(substr(dataM[1,1], 1, 10), hrs, sep=" "), mins, sep=":")
      num=num+1
    }
  }
  #get the time into minutes count then plot them and label in hrs
  mins<-as.numeric(as.character(substr(dMinute[,1], 15, 16)))
  hrs<-as.numeric(as.character(substr(dMinute[,1], 12, 13)))*60
  mB_time_mins<-hrs+minutes
  ##make a final dataframe with date info as group factor
  dMinute<-cbind(dMinute, mB_time_mins)#add converted mins variable as a column
  dMinute<-cbind(dMinute, as.character(substr(dMinute[,1], 1, 10)))#add dates variable as a column
  assign(paste("dataM_", dateName[k], sep=""), dMinute)
}
##combine all dates
dataM_all<-get(paste("dataM_", dateName[1], sep=""))
for (k in 2:length(dateName)){
  dataM_all<-rbind(dataM_all, get(paste("dataM_", dateName[k], sep="")))
}
save.image("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/basis_data/MyBasis_5-23_to_8-25/newdata807-813.RData")

#plot the time vs. parameter
#group by date, use xyplot in the lattice package
library("lattice", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#plot from early date to later, use factor
dataFactor<-c("2014-06-11", "2014-06-10","2014-06-09","2014-06-08",
              "2014-06-07","2014-06-06","2014-06-05")
dateFactor<-factor(dataM_all[,10], dataFactor)
lablist<-as.vector(c(0:23))
dev.off()
xyplot(as.numeric(dataM_all[,2])~as.numeric(dataM_all[,9])|dateFactor,
       layout=c(1,7), type="l", xlab="Hour", ylab="Heart Rate", lwd=3, 
       scales = list(x = list(alternating=1,at=seq(1, 1440, by=60), labels=lablist)))#alternating to define where to draw the tick
#or plot it by overlay on top of each other to show in one graph
dev.off()
par(mfrow=c(1,1), las=1, bty="l")
for (k in 1:length(dateName)){
  dataPlot<-get(paste("dataM_", dateName[k], sep=""))
  plot(as.numeric(dataPlot[,9]), as.numeric(dataPlot[,2]), type="l", lty=1, 
       xlab="Hour", ylab="Heart Rate", lwd=1, col=k, ylim=c(0, 140), xaxt="n")
  par(new=T) 
}
#give the label name
date.F<-c("2014-06-05","2014-06-06","2014-06-07","2014-06-08", 
          "2014-06-09","2014-06-10","2014-06-11")
# date.f<-factor(date.F, levels=date.F)
legend("bottomleft", inset=.01, date.F, 
       col=1:length(dateName), lty=c(rep(1,length(dateName))), bty="n", cex=0.8)
lablist<-as.vector(c(0:23))
axis(1, at=seq(1, 1440, by=60), labels=F, tick=T)
text(seq(1, 1440, by=60), par("usr")[3] - 0.2, labels=lablist, pos=1, xpd=T)


###########
##or plot all dates in one graph longitudinally
times<-as.numeric(dataM_all[,9]) #the vector that has the time values
#get all the unique dates
dates<-unlist(unique(dataM_all[,10]))
paraAll<-c(2,3,4,5,7,8)#position of parameter in the matrix
para<-paraAll[5]
par(new=FALSE)
par(mfrow=c(1,1), las=3, bty="l", cex=0.8)
for (i in 1:7) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot<-as.numeric(which(dataM_all[,10]==dates[i]))
  dateTime<-0
  dateTime<-as.numeric(times[numPlot]+1440*(i-1))
  maxP<-max(as.numeric(dataM_all[,para]), na.rm=T)
  minP<-min(as.numeric(dataM_all[,para]), na.rm=T)#add min to stretch the value region
  plot(dateTime, as.numeric(dataM_all[numPlot,para]), type="l", lty=1,
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


#####add activity data to the plot
#plot activity, by code the categorical data into numerical data
# > table(dataM_all[,"activity"])
# deep sleep          inactive interrupted sleep    light activity       
#        436              4340                26              2626              
# light sleep moderate activity 
#        1238               548 
# rem sleep     unknown sleep 
#       458                 4 
# code unknown sleep=0, deep sleep=1, light sleep=2, rem sleep=3
# interrupted sleep=4, inactive=5, light activity=6, moderate activity=7
#get the column of dataM_all that are the activity
#recode them with numbers
actM<-dataM_all[, "activity"]
# for (i in 1:length(actM)){
#   if (actM[i]=="unknown sleep") {
#     actM[i]=0
#   }else if (actM[i]=="deep sleep") {
#     actM[i]=1
#   }else if (actM[i]=="light sleep") {
#     actM[i]=2
#   }else if (actM[i]=="rem sleep") {
#     actM[i]=3
#   }else if (actM[i]=="interrupted sleep") {
#     actM[i]=4
#   }else if (actM[i]=="inactive") {
#     actM[i]=5
#   }else if (actM[i]=="light activity") {
#     actM[i]=6
#   }else if (actM[i]=="moderate activity") {
#     actM[i]=7
#   }else {}
# }
#get the rows that have each type of activity
# num0<-as.numeric(which(actM=="unknown sleep"))
# num1<-as.numeric(which(actM=="deep sleep"))
# num2<-as.numeric(which(actM=="light sleep"))
# num3<-as.numeric(which(actM=="rem sleep"))
# num4<-as.numeric(which(actM=="interrupted sleep"))
# num5<-as.numeric(which(actM=="inactive"))
# num6<-as.numeric(which(actM=="light activity"))
# num7<-as.numeric(which(actM=="moderate activity"))
#or  use loop
actName<-c("unknown sleep", "deep sleep", "light sleep", "rem sleep", "interrupted sleep", 
           "inactive", "light activity", "moderate activity")
for (i in 1:length(actName)){
  #assign num0 to num7 with the number of rows that have the same value
  assign(paste("num", i-1, sep=""), as.numeric(which(actM==actName[i])))
  #for each vector of num0 to num7, assign dumb code to it
  ids<-get(paste("num", i-1, sep=""))
  for (j in 1:length(ids)){#loop all rows with the same value and assign the dumb number
    actM[ids[j]]<-i-1
  }
}
#plot all dates' activity
times<-as.numeric(dataM_all[,9]) #the vector that has the time values
#get all the unique dates
dates<-unlist(unique(dataM_all[,10]))
par(new=FALSE)
par(mar=c(5, 10, 4, 8) + 0.1)#increase the margin on the left to 10
par(mfrow=c(1,1), las=3, bty="l", cex=0.8)
for (i in 1:7) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot<-as.numeric(which(dataM_all[,10]==dates[i]))
  dateTime<-0
  dateTime<-as.numeric(times[numPlot]+1440*(i-1))
  maxP<-max(as.numeric(actM), na.rm=T)
  minP<-min(as.numeric(actM), na.rm=T)
  plot(dateTime, as.numeric(actM[numPlot]), type="l", lty=1,
       xlab="Hour", ylab="", col=i, lwd=2, 
       ylim=c(minP, maxP+1), xaxt="n", yaxt="n", xlim=c(0, 10079), cex=0.8)
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
       col=1:length(dateName), lty=c(rep(1,length(dateName))), bty="n", cex=0.8, horiz=T)
# lablist<-as.vector(rep(c(0,3,6,9,12,15,18,21),7))
lablist<-as.vector(rep(c(0:11)*2,7))
axis(1, at=seq(1, 10080, by=120), labels=lablist, tick=T, las=3)
axis(2, at=seq(0, 7, by=1), labels=actName, tick=T, las=2)

# text(seq(1, 10080, by=120), par("usr")[3] , labels=lablist, pos=1, srt=90)












#######################(below not used)
##plot all dates in one graph
# dev.off()
# par(mfrow=c(1,1), las=1, bty="l")
# plot(times, as.numeric(dataM_all[,2]), type="l", lty=1, 
#      xlab="Date", ylab="Heart Rate", lwd=1, ylim=c(0, 140), xaxt="n")

#######using stream and stack plot to show sequential change of a parameter 
#through dates
#code from r blogger, Data mountains and streams – stacked area plots in R
# December 9, 2013
# By Marc in the box, use devtools package
install.packages("devtools")
library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#plot.stacked makes a stacked plot where each y series is plotted on top
#of the each other using filled polygons
#
#Arguments include:
#'x' - a vector of values
#'y' - a matrix of data series (columns) corresponding to x
#'order.method' = c("as.is", "max", "first") 
#  "as.is" - plot in order of y column
#  "max" - plot in order of when each y series reaches maximum value
#  "first" - plot in order of when each y series first value > 0
#'col' - fill colors for polygons corresponding to y columns (will recycle)
#'border' - border colors for polygons corresponding to y columns (will recycle) (see ?polygon for details)
#'lwd' - border line width for polygons corresponding to y columns (will recycle)
#'...' - other plot arguments


####this is a stack graph, in other words, each plot lay on top of each other
##doesn't mean the total value is the sum, but more to show
## the frequency of subject or parameters that have a value on the same date
#code from stackoverflow
plot.stacked <- function(x,y, ylab="", xlab="", ncol=1, xlim=range(x, na.rm=T), ylim=c(0, 1.2*max(rowSums(y), na.rm=T)), border = NULL, col=rainbow(length(y[1,]))){
  
  plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, xaxs="i", yaxs="i", xlim=xlim, t="n")
  bottom=0*y[,1]
  for(i in 1:length(y[1,])){
    top=rowSums(as.matrix(y[,1:i]))
    polygon(c(x, rev(x)), c(top, rev(bottom)), border=border, col=col[i])
    bottom=top
  }
  abline(h=seq(0,200000, 10000), lty=3, col="grey")
  legend("topleft", rev(colnames(y)), ncol=ncol, inset = 0, fill=rev(col), bty="0", bg="white", cex=0.8, col=col)
  box()
}
####an example
set.seed(1)
m<-500
n<-15
x<-seq(m)
y<-matrix(0,nrow=m,ncol=n)
colnames(y)<-seq(n)
for(i in seq(ncol(y))){
  mu<-runif(1,min=0.25*m,max=0.75*m)
  SD<-runif(1,min=5,max=30)
  TMP<-rnorm(1000,mean=mu,sd=SD)
  HIST<- hist(TMP,breaks=c(0,x),plot=FALSE)
  fit<- smooth.spline(HIST$counts~HIST$mids)
  y[,i]<-fit$y
}
plot.stacked(x,y)
###weired y axis limit
####try stack plot on basis data
##need to get all parameters value in one y dataframe
#then plot x=mintime against y
#for date 6-08
data608<-rep(0, length(dataM_605[,1]))#the vector that holds the full range of time points
#for loop to replace NA with data from shortened vector
for (i in 1:length(dataM_605[,1])){
  rowNum<-which(as.numeric(dataM_608[,9])==as.numeric(dataM_605[i,9]))
  if(length(rowNum)!=0){
    data608[i]<-dataM_608[rowNum,2]
  }
}
#for date 6-09
data609<-rep(0, length(dataM_605[,1]))#the vector that holds the full range of time points
#for loop to replace NA with data from shortened vector
for (i in 1:length(dataM_605[,1])){
  rowNum<-which(as.numeric(dataM_609[,9])==as.numeric(dataM_605[i,9]))
  if(length(rowNum)!=0){
    data609[i]<-dataM_609[rowNum,2]
  }
}
#combine y dataframe with the extended vectors
stackY<-data.frame(as.numeric(dataM_605[,2]),as.numeric(dataM_606[,2]),
                   as.numeric(dataM_607[,2]),
                   as.numeric(data608), as.numeric(data609), 
                   as.numeric(dataM_610[,2]),as.numeric(dataM_611[,2]))
stackX<-as.numeric(dataM_605[,9])
stackY<-as.matrix(stackY)
plot.stacked(stackX,stackY)


##try a different function using lattice
#code from stackoverflow
##function of panel.flow and prepanel.flow
##stack together to show which date/time have more parameters land on
panel.flow <- function(x, y, groups, origin, ...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)
  
  ## From long to wide
  yWide <- unstack(dat, y~groups)
  ## Where are the maxima of each variable located? We will use
  ## them to position labels.
  idxMaxes <- apply(yWide, 2, which.max)
  
  ##Origin calculated following Havr.eHetzler.ea2002
  if (origin=='themeRiver') origin= -1/2*rowSums(yWide)
  else origin=0 
  yWide <- cbind(origin=origin, yWide)
  ## Cumulative sums to define the polygon
  yCumSum <- t(apply(yWide, 1, cumsum))
  Y <- as.data.frame(sapply(seq_len(nVars),
                            function(iCol)c(yCumSum[,iCol+1],
                                            rev(yCumSum[,iCol]))))
  names(Y) <- levels(groups)
  ## Back to long format, since xyplot works that way
  y <- stack(Y)$values
  
  ## Similar but easier for x
  xWide <- unstack(dat, x~groups)
  x <- rep(c(xWide[,1], rev(xWide[,1])), nVars)
  ## Groups repeated twice (upper and lower limits of the polygon)
  groups <- rep(groups, each=2)
  
  ## Graphical parameters
  superpose.polygon <- trellis.par.get("superpose.polygon")
  col = superpose.polygon$col
  border = superpose.polygon$border 
  lwd = superpose.polygon$lwd 
  
  ## Draw polygons
  for (i in seq_len(nVars)){
    xi <- x[groups==groupLevels[i]]
    yi <- y[groups==groupLevels[i]]
    panel.polygon(xi, yi, border=border,
                  lwd=lwd, col=col[i])
  }
  
  ## Print labels
  for (i in seq_len(nVars)){
    xi <- x[groups==groupLevels[i]]
    yi <- y[groups==groupLevels[i]]
    N <- length(xi)/2
    ## Height available for the label
    h <- unit(yi[idxMaxes[i]], 'native') -
      unit(yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1], 'native')
    ##...converted to "char" units
    hChar <- convertHeight(h, 'char', TRUE)
    ## If there is enough space and we are not at the first or
    ## last variable, then the label is printed inside the polygon.
    if((hChar >= 1) && !(i %in% c(1, nVars))){
      grid.text(groupLevels[i],
                xi[idxMaxes[i]],
                (yi[idxMaxes[i]] +
                   yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1])/2,
                gp = gpar(col='white', alpha=0.7, cex=0.7),
                default.units='native')
    } else {
      ## Elsewhere, the label is printed outside
      
      grid.text(groupLevels[i],
                xi[N],
                (yi[N] + yi[N+1])/2,
                gp=gpar(col=col[i], cex=0.7),
                just='left', default.units='native')
    }
  }
}

prepanel.flow <- function(x, y, groups, origin,...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)
  yWide <- unstack(dat, y~groups)
  if (origin=='themeRiver') origin= -1/2*rowSums(yWide)
  else origin=0
  yWide <- cbind(origin=origin, yWide)
  yCumSum <- t(apply(yWide, 1, cumsum))
  
  list(xlim=range(x),
       ylim=c(min(yCumSum[,1]), max(yCumSum[,nVars+1])),
       dx=diff(x),
       dy=diff(c(yCumSum[,-1])))
}

###example using the function
library(lattice)
library(zoo)
library(colorspace)

nCols <- ncol(unemployUSA)
pal <- rainbow_hcl(nCols, c=70, l=75, start=30, end=300)
# myTheme <- custom.theme(fill=pal, lwd=0.2)

xyplot(unemployUSA, superpose=TRUE, auto.key=FALSE,
       panel=panel.flow, prepanel=prepanel.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)))


####need to plot longitudinal data for one individual
##so still use xyplot
##example for plot longitudinal data using base graphics and fitting line
##code from http://colbyimaging.duckdns.org:8080/wiki/statistics/longitudinal-data
install.packages("plyr")
install.packages("ggplot2")
library(plyr)
library(ggplot2)

# Simulate data
numSubs = 40

x = runif(numSubs, 0, 30)
x = c(x, x + abs(rnorm(numSubs, 2)))
y = 2 + x - 0.02*x^2 + rnorm(2*numSubs, 0, 1)

data = data.frame(ID = 1:numSubs, Age = x, FA = y, Time = factor(rep(c(1,2), each=numSubs)), Gender = sample(c('Male', 'Female'), numSubs, replace=T))

# Fit quadratic model
quad.fit     = lm(FA ~ Age + I(Age^2), data=data)
pred.ages    = with(data, data.frame(Age = seq(min(Age), max(Age), length.out=100)))
quad.pred    = pred.ages
quad.pred$FA = predict(quad.fit, quad.pred)

# Fit exponential model
exp.fit     = nls(FA ~ SSasymp(Age, Asym, R0, lrc), data=data)
exp.pred    = pred.ages
exp.pred$FA = predict(exp.fit, exp.pred)

# Plot with base graphics
dev.new(width=5, height=5)
colorpicks = c('pink', 'skyblue')
with(data, plot(Age, FA, pch=c(1,19)[Time], col=colorpicks[as.numeric(Gender)], bty='l', main='Longitudinal example', xlab='Age', ylab='FA'))
d_ply(data, 'ID', function(x) lines(x$Age, x$FA, lty=3, col='#666666'))
lines(quad.pred)
lines(exp.pred, lty=2)
legend('bottomright', bty='n', col=c('black', 'black', colorpicks), pch=c(1,19,19,19), c('Time 1', 'Time 2', 'Female', 'Male'))

######
##for one individual, just change the minutes of each date into
#continuous time, i.e. add 1440 to each different date's minutes
#then plot them in one graph with x axis is the continuous minutes 
#but labeled with different date
##first change the minutes

