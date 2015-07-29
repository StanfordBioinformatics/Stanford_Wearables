###Mike's Neuma data analysis, 6/19-6/20
## got raw data from Denis in the online box folder
#total 254,007 data records with 6 variables for 6/19, and 6/20
# [1] "Time.GMT..Year.Month.Day.Hr.Min.Sec.Ms." "Skin.conductance..microSiemens."        
# [3] "Temperature..degrees.F."                 "X..Gs."                                 
# [5] "Y..Gs."                                  "Z..Gs."    
# DATA_02.204_06_19_14 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Neuma_device/Neuma/DATA_02-204_06_19_14.csv")
DATA_02.204_06_19_14 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/Neuma_device/Neuma/DATA_02-204_06_19_14.csv")
# DATA_02.204_06_20_14 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Neuma_device/Neuma/DATA_02-204_06_20_14.csv")
DATA_02.204_06_20_14 <- read.csv("~/Documents/BMI master/BMI projects/Atul lab project/butteLab temp & postdoc/iHMP_project_iPop/Fitbit project/Neuma_device/Neuma/DATA_02-204_06_20_14.csv")

neuma0619<-DATA_02.204_06_19_14
neuma0620<-DATA_02.204_06_20_14
#change column name to easy symbol
colnames(neuma0619)[1]<-"time"
colnames(neuma0619)[2]<-"gsr"
colnames(neuma0619)[3]<-"temp"
colnames(neuma0619)[4]<-"xGs"
colnames(neuma0619)[5]<-"yGs"
colnames(neuma0619)[6]<-"zGs"
#for 6/20
colnames(neuma0620)[1]<-"time"
colnames(neuma0620)[2]<-"gsr"
colnames(neuma0620)[3]<-"temp"
colnames(neuma0620)[4]<-"xGs"
colnames(neuma0620)[5]<-"yGs"
colnames(neuma0620)[6]<-"zGs"

#get each parameter individually
mNeu_time0619<-as.character(neuma0619$time)
mNeu_gsr0619<-as.numeric(as.character(neuma0619$gsr))
mNeu_temp0619<-as.numeric(as.character(neuma0619$temp))
mNeu_time0620<-as.character(neuma0620$time)
mNeu_gsr0620<-as.numeric(as.character(neuma0620$gsr))
mNeu_temp0620<-as.numeric(as.character(neuma0620$temp))

#extract time by date and get the data points in each date
mNeu_time_date<-substr(mNeu_time0619, 6, 12)#date in the 0619 file
mNeu_time_date2<-substr(mNeu_time0620, 6, 12)#date in the 0620 file
mNeu_time_hr<-substr(mNeu_time0619, 14, 15)
mNeu_time_hr2<-substr(mNeu_time0620, 14, 15)

##find out the last 20408 data points in 0619 file were the same as the 233599-254007 data points
## so combine two days data file into one file, by taking all of 0619 file and 1-233598 data points
##using rbind function
nonrepeatNeuma0620<-neuma0620[1:233598,]
mNeuma<-rbind(neuma0619, nonrepeatNeuma0620)#new matrix with combined nonrepeative data from 0619 and 0620
#get the new time variable and gsr
time_mNeuma<-as.character(mNeuma$time)
time_date_mNeuma<-substr(time_mNeuma, 6, 12)#date in combined file
gsr_mNeuma<-as.numeric(as.character(mNeuma$gsr))
temp_mNeuma<-as.numeric(as.character(mNeuma$temp))

#############initial plotting, not ideal
#plot the raw one demention data of gsr
plot(gsr_mNeuma, xlab="Data points", ylab="gsr")

#get the starting time for 6/20 and add 24hrs to the hr number to continue the count and allow plotting in one plot
# > unique(time_date_mNeuma)
# [1] "June-19" "June-20"
# > which(time_date_mNeuma=="June-20")[1]
# [1] 148181

#convert time into mili second and divide 3,600,000 to get the hr
#for the hours on 6.20, add 24 to the hour number to continue counting
mseconds<-as.numeric(as.character(substr(time_mNeuma, 23, 25)))*1
seconds<-as.numeric(as.character(substr(time_mNeuma, 20, 21)))*1000
minutes<-as.numeric(as.character(substr(time_mNeuma, 17, 18)))*60000
hrs1<-as.numeric(as.character(substr(time_mNeuma[1:148180], 14, 15)))*3600000
hrs2<-(as.numeric(as.character(substr(time_mNeuma[148181:487605], 14, 15)))+24)*3600000
#bind hrs1 and hrs2
hrs<-c(hrs1, hrs2)
mNeuma_time_msecond<-hrs+minutes+seconds+mseconds
#so the x axis is from 18*3600000 ms to 35*3600000 ms
#plot into hrs against gsr(skin conductance)
par(bty="l", mfrow=c(2,1))
par(xaxt="n")

plot(mNeuma_time_msecond, gsr_mNeuma,  xlab="Hour", ylab="skin conductance", xlim=c(64800000,  122400000), main="Neuma watch data 6/19 11am to 6/20 2am")
lablist<-as.vector(c(11:23, 0:2))
axis(1, at=seq(64800001,  122400000, by=3600000), labels=F)
text(seq(64800001,  122400000, by=3600000), par("usr")[3] - 0.2, labels=lablist, pos=1, xpd=T)
#plot temp vs. time
plot(mNeuma_time_msecond, temp_mNeuma,  xlab="Hour", ylab="Temperature", xlim=c(64800000,  122400000), main="Neuma watch data 6/19 11am to 6/20 2am")
lablist<-as.vector(c(11:23, 0:2))
axis(1, at=seq(64800001,  122400000, by=3600000), labels=F)
text(seq(64800001,  122400000, by=3600000), par("usr")[3] - 0.2, labels=lablist, pos=1, xpd=T)
################the end of initial plotting


#####plot by minutes
##use loop to capture the average of second data for each minute
##for each dataset, data_605, data_606...., sum the seconds into minutes (steps and cal) or average (hr, gsr, skintemp, accel)
##then get the final dataset with a column of time converted to minutes and date factor column for plot group by date

##analyze by days
##get the points in each day
##use for loop and paste and assign to assign data to different variables based on date
##assign(paste...) to give value to the paste name
##get(paste...) to acquire value of the paste name
#change date to no hyphen
mB_date<-paste(substr(time_date_mNeuma, 1, 4), substr(time_date_mNeuma, 6, 7), sep="")
dateName<-c("June19", "June20")
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
  assign(paste("data_", dateName[i], sep=""),mNeuma[begin:(begin+num-1),])
}
for (k in 1:length(dateName)){
  dataM<-get(paste("data_", dateName[k], sep=""))#dataM as the boilplate for each date data
  #get the unique hour number and minute number and second number, 
  #then sum the milli second in each second, then average seconds in each minute
  hours<-unlist(unique(as.numeric(as.character(substr(dataM[,1], 14, 15)))))
  minutes<-unlist(unique(as.numeric(as.character(substr(dataM[,1], 17, 18)))))
# seconds<-unlist(unique(as.numeric(as.character(substr(dataM[,1], 20, 21)))))
  ##do loops to average seconds data for each minute
  totalRows<-length(hours)*length(minutes) #total rows of dataset by minute
  dMinute<-matrix(0, nrow=totalRows, ncol=6, dimnames=list(1:totalRows, colnames(mNeuma)))
  num=1 #counting the loops
  for (i in hours) {
    dataHours<-dataM[which(as.numeric(as.character(substr(dataM[,1], 14, 15)))==i),]
    for (j in minutes) { #ignore the millisecond separation, just get the mean of all milli second in each minute
      dataMins<-dataHours[which(as.numeric(as.character(substr(dataHours[,1], 17, 18)))==j),]
      ids<-which(as.numeric(as.character(substr(dataMins[,1], 17, 18)))==j)
      dMinute[num,2]<-mean(dataMins[ids,2], na.rm = TRUE)
      dMinute[num,3]<-mean(dataMins[ids,3], na.rm = TRUE)
      dMinute[num,4]<-mean(dataMins[ids,4], na.rm = TRUE)
      dMinute[num,5]<-mean(dataMins[ids,5], na.rm = TRUE)
      dMinute[num,6]<-mean(dataMins[ids,6], na.rm = TRUE)
    
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
      dMinute[num,1]<-paste(paste(substr(dataM[1,1], 1, 12), hrs, sep=" "), mins, sep=":")
      num=num+1
    }
  }
  #get the time into minutes count then plot them and label in hrs
  mins<-as.numeric(as.character(substr(dMinute[,1], 17, 18)))
  hrs<-as.numeric(as.character(substr(dMinute[,1], 14, 15)))*60
  mB_time_mins<-hrs+minutes
  ##make a final dataframe with date info as group factor
  dMinute<-cbind(dMinute, mB_time_mins)#add converted mins variable as a column
  dMinute<-cbind(dMinute, as.character(substr(dMinute[,1], 1, 12)))
  assign(paste("dataM_", dateName[k], sep=""), dMinute)
}
#make the dataM_June20 hours add 24 hrs or add 24*60 minutes
dataM_June20[,7]<-as.numeric(dataM_June20[,7])+24*60
##combine all dates
dataM_all<-get(paste("dataM_", dateName[1], sep=""))
for (k in 2:length(dateName)){
  dataM_all<-rbind(dataM_all, get(paste("dataM_", dateName[k], sep="")))
}
dataM_all_neuma<-dataM_all
#plot 
par(bty="l", mfrow=c(1,1))
par(xaxt="n")
plot(dataM_all[,7], dataM_all[,2], type="l")
plot(dataM_all[,7], dataM_all[,3], type="l")
