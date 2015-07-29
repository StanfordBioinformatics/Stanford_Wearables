#load moves basis comparison RData
load("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/Moves/moves_basis_comparison.RData")






times<-as.numeric(dataM_moves[,9]) #the vector that has the time values
#get all the unique dates
dates<-unlist(unique(dataM_moves[,10]))
date_time<-0#store all the dateTime plotted
paraAll<-c(2,3,4,5,7,8)#position of parameter in the matrix
para<-paraAll[5]
par(new=FALSE)
par(mfrow=c(1,1), las=1, bty="l", cex=0.8)
for (i in 1:7) {#use min=min+1440*k (k=0,6) to make continuous time values
  #   times[which(dataM_all[,10]==dates[i])]<-times[which(dataM_all[,10]==dates[i])]+1440*(i-1)
  numPlot_temp<-as.numeric(which(dataM_moves[,10]==dates[i]))
  numPlot<-as.numeric(which(dataM_moves[numPlot_temp, "activity"]=="walking"))#get the rows that have corresponding activity
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
date.F<-c("2014-06-05","2014-06-06","2014-06-07", "2014-06-08", 
          "2014-06-09","2014-06-10","2014-06-11")
legend("topleft",inset=0, date.F, 
       col=1:length(dateName), lty=1, bty="n", cex=0.9, horiz=T, title="Moves Steps During Walking")
# lablist<-as.vector(rep(c(0,3,6,9,12,15,18,21),7))
lablist<-as.vector(rep(c(0:11)*2,7))
axis(1, at=seq(1, 10080, by=120), labels=lablist, tick=T, las=3)

##use the date_time plotted in the moves plot
##to plot the corresponding steps from basis
date_time1<-date_time[-1]#minus the first 0 number
par(new=T)
plot(date_time1, as.numeric(dataM_all[date_time1,"steps"]), type="b", lty=1,
     xlab="", ylab="", col="lightgray", lwd=2, 
     ylim=c(minP, maxP+25), xaxt="n", xlim=c(0, 10079), cex=0.8)
legend("top",inset=0.1, "Basis Steps During Moves' Walking", 
       col="lightgray", lty=c(rep(1,length(dateName))), bty="n", cex=0.9, horiz=T)

##check correlation between moves steps and basis steps during walking
steps_basis<-as.numeric(dataM_all[date_time1,"steps"])
steps_moves<-as.numeric(dataM_moves[date_time1,"steps"])
plot(steps_moves, steps_basis,las=1, main="Correlation between Moves Steps and Basis Steps during Walking (Kendall Regression, tau=0.99)")
cor.test(steps_moves, steps_basis, method="kendall")
abline(lm(steps_basis~steps_moves), col="red")
