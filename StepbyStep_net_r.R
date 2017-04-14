##read in package
install.packages("lubridate")
library(lubridate)

#set working drive
setwd("/Users/jacobwasserman/Downloads/Loranty Research/net_radiometer/")

#####name all dataframes#####
##convert dates to day of year, using timestamp##

nr1hd <- read.csv("CR1000_nr1_Highdensity.csv")
datenr1hd <- as.Date(nr1hd$TIMESTAMP, "%m/%d/%y")
nr1hd$doy <- yday(datenr1hd)

nr1ld <- read.csv("CR1000_nr1_Lowdensity.csv")
datenr1ld <- as.Date(nr1ld$TIMESTAMP, "%m/%d/%y")
nr1ld$doy <- yday(datenr1ld)

nr2hd <- read.csv("CR1000_nr2_Highdensity.csv")
datenr2hd <- as.Date(nr2hd$TIMESTAMP, "%m/%d/%y")
nr2hd$doy <- yday(datenr2hd)

nr2ld <- read.csv("CR1000_nr2_Lowdensity.csv")
datenr2ld <- as.Date(nr2ld$TIMESTAMP, "%m/%d/%y")
nr2ld$doy <- yday(datenr2ld)

#plot(nr1hd$Albedo_Avg, ylim=c(0,1), xlim=c(500,550), type="l")
#lines(nr1hd$IR01UpCo_Avg)   


##plot abledo vs time##
#plot(nr2hd$Albedo_Avg, ylim=c(0,1), xlim=c(500,550), type="l")

##albedo vs time for the other two as well## 
#read in other files and look at graphs for the same plot#

#plot(nr2hd$Albedo_Avg, ylim=c(0,1), xlim=c(500,550), type="l")

#plot(nr2ld$Albedo_Avg, ylim=c(0,1), xlim=c(500,550), type="l")

##try to merge dataframes by timestamp, the columnn they have in common##
#nr2Comb <- merge(nr2hd, nr2ld,by="TIMESTAMP")
#name combination#

#plot(nr2Comb$Albedo_Avg.x, ylim=c(0,.5), type="l", col="red", xlab="timestamp entry", ylab="albedo")

#points(nr2Comb$Albedo_Avg.y, col="blue", type="l")

##Try to make a legend##
legend(nr2Comb, c("high density", "low density"), col=c("red", "blue"), bty="n")

##Try to combine all four data.frames via the TIMESTAMP column. *Note. nr1ld is lowercase.
#nr1Comb <- merge(nr1hd,nr1ld,by="TIMESTAMP")
#the above step just merged the 1's for hd and ld

#################   NOTE #############
##  COPIED TIMES ON MERGE ## TRY AGAIN ##

#Attempt Combination##
#mergea

#should see if there's a way to merge all 4 at some point

#add the (1) dataframes to the plot
#points(nr1Comb$Albedo_Avg.x, col="green", type="b")
#points(nr1Comb$Albedo_Avg.y, col="black", type="b")
#or if needed, can put in plot of just nr1's

##Try to merge nr1Comb with nr2Comb##
#NetRadComb <- merge(nr1Comb, nr2Comb,by="TIMESTAMP")

#plot(nr1Comb$Albedo_Avg.x, ylim=c(0,1), xlim=c(500,550), type="l", col="green")
#observations--comparing the 4 albedos, the observations from nr1 look lower than from nr2

#############this time plot all four from the new merged NetRadComb#################
#plot(NetRadComb$Albedo_Avg.x.x, ylim=c(0,.5), xlim=c(0,2000), type="l", col="black")
#points(NetRadComb$Albedo_Avg.x.y, type="l", col="red")
#points(NetRadComb$Albedo_Avg.y.x, type="l", col="green")
#points(NetRadComb$Albedo_Avg.y.y,type="l", col="blue")

##analysis: nr1ld, nr2hd mirror each other very closely.##

### try to take standard deviation. then, remove values that exceed###
### 2 standard deviations#####
#the original standard deviation should match the standard deviation#
#in the new NetRadComb file. It does not. (Magnitudes different)## 

#sd(nr1hd$Albedo_Avg, na.rm = TRUE)

#nr1hd["correctAlb"] <- NA
#for(i in 1:length(nr1hd$Albedo_Avg)){
#  if(nr1hd$Albedo_Avg[i] < (average of column + 2sd of column) && ){
 #   nr1hd$correctAlb[i] <- nr1hd$Albedo_Avg[i]
#  } #(replace with function))
#}


## need to change column names in one dataframe since ##
## all of them are the same, and this is causing problems ##
## only do first 16 columns to leave the last three the same ##
## to join the two dataframes ##
colnames(nr2hd) [1:16] <- paste0(colnames(nr2hd) [1:16], "HD2")
colnames(nr1hd) [1:16] <- paste0(colnames(nr1hd) [1:16], "HD1")
##try the join function, using plyr installation##

# INSTALL PLYR #
install.packages("plyr")
library(plyr)

##using multiple columns between the tables##
#type options are inner, left, right, or full, here we want full#

HDjoin <- join(nr1hd, nr2hd, by=c("Hour", "Minute", "doy"), type="full")

##now do the same thing with the low density stands##

colnames(nr2ld) [1:16] <- paste0(colnames(nr2ld) [1:16], "LD2")
colnames(nr1ld) [1:16] <- paste0(colnames(nr1ld) [1:16], "LD1")

LDjoin <- join(nr1ld, nr2ld, by=c("Hour", "Minute", "doy"), type="full")

##join all 4 dataframes##
NetRadJoin <- join(LDjoin, HDjoin, by=c("Hour", "Minute", "doy"), type="full")

## SD test---see if original DF is same as new combined DF ## 
#sd(nr1hd$Albedo_AvgHD1, na.rm = TRUE)
#sd(NetRadJoin$Albedo_AvgHD1, na.rm = TRUE)
#GOOD#
#sd(nr1ld$Albedo_AvgLD1, na.rm = TRUE)
#sd(NetRadJoin$Albedo_AvgLD1, na.rm = TRUE)
#GOOD#
#sd(nr2hd$Albedo_AvgHD2, na.rm = TRUE)
#sd(NetRadJoin$Albedo_AvgHD2, na.rm = TRUE)
#GOOD#
#sd(nr2ld$Albedo_AvgLD2, na.rm = TRUE)
#sd(NetRadJoin$Albedo_AvgLD2, na.rm = TRUE)
#GOOD#
#DATAFRAMES MATCH UP#

##Steps below used to assess problems from 3/29/17##
#length.test <- aggregate(NetRadJoin$doy, by=list(NetRadJoin$doy), FUN = "length")
#length(nr1hd$doy [nr1hd$doy==197])
#length.test2 <- aggregate(nr2hd$doy, by=list(nr2hd$doy), FUN = "length")

### restrict column conditions for albedo: between 0 and 1 ##
## this action will create a new VECTOR ##
## Add this new vector with good albedos to the dataframe ##
GoodAlbHD1 <- ifelse(NetRadJoin$Albedo_AvgHD1<0|NetRadJoin$Albedo_AvgHD1>1,NA,NetRadJoin$Albedo_AvgHD1)
NetRadJoin["GoodAlbHD1"] <- GoodAlbHD1
##now we have a new column#
##do the same with the albedos from the other 3 dataframes as well##

GoodAlbHD2 <- ifelse(NetRadJoin$Albedo_AvgHD2<0|NetRadJoin$Albedo_AvgHD2>1,NA,NetRadJoin$Albedo_AvgHD2)
NetRadJoin["GoodAlbHD2"] <- GoodAlbHD2

GoodAlbLD1 <- ifelse(NetRadJoin$Albedo_AvgLD1<0|NetRadJoin$Albedo_AvgLD1>1,NA,NetRadJoin$Albedo_AvgLD1)
NetRadJoin["GoodAlbLD1"] <- GoodAlbLD1

GoodAlbLD2 <- ifelse(NetRadJoin$Albedo_AvgLD2<0|NetRadJoin$Albedo_AvgLD2>1,NA,NetRadJoin$Albedo_AvgLD2)
NetRadJoin["GoodAlbLD2"] <- GoodAlbLD2

##Convert time and day to doy and fraction of day.
#First, convert hour to fraction out of 24h
HourFraction <- NetRadJoin$Hour*(1/24)
NetRadJoin["HourFraction"] <- HourFraction
#now minute fraction, out of (1/(24*60)) to weigh its value as proportional to day
MinFraction <- NetRadJoin$Minute*(1/(24*60))
NetRadJoin["MinFraction"] <- MinFraction

#sum doy, HourFraction, and Minute Fraction#
#and add as column
DayTimeComb <- NetRadJoin$doy+NetRadJoin$HourFraction+NetRadJoin$MinFraction
NetRadJoin["DayTimeComb"] <- DayTimeComb

### add the conditions of short wave radiation as new columns ###
## take out SR_UP less than 0 and SR_DOWN less than 0 ##
A_HD1_limSR <- ifelse(NetRadJoin$SR01Up_AvgHD1<0|NetRadJoin$SR01Dn_AvgHD1<0,NA,NetRadJoin$GoodAlbHD1)
NetRadJoin["A_HD1_limSR"] <- A_HD1_limSR

A_HD2_limSR <- ifelse(NetRadJoin$SR01Up_AvgHD2<0|NetRadJoin$SR01Dn_AvgHD2<0,NA,NetRadJoin$GoodAlbHD2)
NetRadJoin["A_HD2_limSR"] <- A_HD2_limSR

A_LD1_limSR <- ifelse(NetRadJoin$SR01Up_AvgLD1<0|NetRadJoin$SR01Dn_AvgLD1<0,NA,NetRadJoin$GoodAlbLD1)
NetRadJoin["A_LD1_limSR"] <- A_LD1_limSR

A_LD2_limSR <- ifelse(NetRadJoin$SR01Up_AvgLD2<0|NetRadJoin$SR01Dn_AvgLD2<0,NA,NetRadJoin$GoodAlbLD2)
NetRadJoin["A_LD2_limSR"] <- A_LD2_limSR

##NOW PLOT## albedo on y-axis, DayTimeComb on x-axis
plot(NetRadJoin$DayTimeComb, NetRadJoin$A_HD1_limSR, pch=".", type="l", xlab = "Day of Year", ylab = "Albedo")
points(NetRadJoin$DayTimeComb, NetRadJoin$A_HD2_limSR, pch=".", type = "l", col="darkgreen")
points(NetRadJoin$DayTimeComb, NetRadJoin$A_LD1_limSR, pch=".", type = "l", col="blue")
points(NetRadJoin$DayTimeComb, NetRadJoin$A_LD2_limSR, pch=".", type = "l", col="red")

#LD2 and HD1 seem to match prety closely
########### QUESTION ###########
## should the date/time column filter out low radiation values too, or just on albedo?
## Also, why do the curves get less "noisy" when I plot against date/time,
# but not when I plot against record#

#now same with low short wave radiation greater than 8, then 5#
#plot(NetRadJoin$Albedo_AvgHD1[NetRadJoin$SR01Dn_AvgHD1>8], type="l", ylim = c(0,1))
#points(NetRadJoin$Albedo_AvgHD2[NetRadJoin$SR01Dn_AvgHD2>8], type = "l", col="green")
#points(NetRadJoin$Albedo_AvgLD1[NetRadJoin$SR01Dn_AvgLD1>8], type = "l", col="blue")
#points(NetRadJoin$Albedo_AvgLD2[NetRadJoin$SR01Dn_AvgLD2>8], type = "l", col="red")

#plot(NetRadJoin$Albedo_AvgHD1[NetRadJoin$SR01Dn_AvgHD1>5], type="l")
#points(NetRadJoin$Albedo_AvgHD2[NetRadJoin$SR01Dn_AvgHD2>5], type = "l", col="green")
#points(NetRadJoin$Albedo_AvgLD1[NetRadJoin$SR01Dn_AvgLD1>5], type = "l", col="blue")
#points(NetRadJoin$Albedo_AvgLD2[NetRadJoin$SR01Dn_AvgLD2>5], type = "l", col="red")

##DAILY MEAN ALBEDO VAULES##
#try by the aggregate function#
dailyM_HD1 <- aggregate(A_HD1_limSR, by=list(NetRadJoin$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
dailyM_HD2 <- aggregate(A_HD2_limSR, by=list(NetRadJoin$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
dailyM_LD1 <- aggregate(A_LD1_limSR, by=list(NetRadJoin$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
dailyM_LD2 <- aggregate(A_LD2_limSR, by=list(NetRadJoin$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)

##Have 4 new vectors, Try to rename the columns#
colnames(dailyM_HD1) [1] <- "Doy"
colnames(dailyM_HD1) [2] <- "Daily_Mean_HD1"

colnames(dailyM_HD2) [1] <- "Doy"
colnames(dailyM_HD2) [2] <- "Daily_Mean_HD2"

colnames(dailyM_LD1) [1] <- "Doy"
colnames(dailyM_LD1) [2] <- "Daily_Mean_LD1"

colnames(dailyM_LD2) [1] <- "Doy"
colnames(dailyM_LD2) [2] <- "Daily_Mean_LD2"

#Join 4 dataframes#
JoinDailyMHD <- join(dailyM_HD1, dailyM_HD2, by=c("Doy"), type="full")
JoinDailyMLD <- join(dailyM_LD1, dailyM_LD2, by=c("Doy"), type="full")
JoinDailyM <- join(JoinDailyMHD, JoinDailyMLD, by=c("Doy"), type="full")

#plot daily means on one graph#
plot(JoinDailyM$Doy, JoinDailyM$Daily_Mean_HD1, type="l", col="black", xlab = "Day of Year", ylab = "Daily Average Albedo", ylim=c(0.05,.43))
points(JoinDailyM$Doy, JoinDailyM$Daily_Mean_HD2, type="l", col="darkgreen")
points(JoinDailyM$Doy, JoinDailyM$Daily_Mean_LD1, type="l", col="blue")
points(JoinDailyM$Doy, JoinDailyM$Daily_Mean_LD2, type="l", col="red")

#make a legend#
legend("topright", c("HD1","HD2","LD1", "LD2"), lty = c(1,1,1,1), col=c("black", "darkgreen", "blue", "red"), cex = .7)
##need to fix legend##

##NEW STEP##
#INVESTIGATE STEFAN BOLZMANN RELATIONSHIP#

#Create new dataframe with IR radiation, Temp^4, (Temp^4)*S-B constant, doy, day/time comb#
#add existing columns to new DF#
#StefBoltz <- NetRadJoin[,c(7,14,15,27,34,35,43,50,51,59,66,67,74,19)]
StefBoltz <- NetRadJoin[,c(15,35,51,67,74,19)]

#T to the 4th power (add as vectors, then add to DF)
#HD1_T4 <- StefBoltz$NR01TK_AvgHD1^4
#HD2_T4 <- StefBoltz$NR01TK_AvgHD2^4
#LD1_T4 <- StefBoltz$NR01TK_AvgLD1^4
#LD2_T4 <- StefBoltz$NR01TK_AvgLD2^4

#StefBoltz["HD1_T4"] <- HD1_T4
#StefBoltz["HD2_T4"] <- HD2_T4
#StefBoltz["LD1_T4"] <- LD1_T4
#StefBoltz["LD2_T4"] <- LD2_T4

#multiply these vectors by the S-B constant, (5.67*10^-8) to get one side of the equation
# add ass new vector, then to DF
#HD1_T4xConst <- HD1_T4*(5.67*10^-8)
#HD2_T4xConst <- HD2_T4*(5.67*10^-8)
#LD1_T4xConst <- LD1_T4*(5.67*10^-8)
#LD2_T4xConst <- LD2_T4*(5.67*10^-8)

#StefBoltz["HD1_T4xConst"] <- HD1_T4xConst
#StefBoltz["HD2_T4xConst"] <- HD2_T4xConst
#StefBoltz["LD1_T4xConst"] <- LD1_T4xConst
#StefBoltz["LD2_T4xConst"] <- LD2_T4xConst

#From downward sensor, half hourly and daily avereages, solving for true ground temperature, 

#solve for Ground Temp.# Half hourly data
TempHD1 <- StefBoltz$IR01DnCo_AvgHD1^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))
TempHD2 <- StefBoltz$IR01DnCo_AvgHD2^(1/4)*(1/(.97*5.67*(10^-8))^(1/4)) 
TempLD1 <- StefBoltz$IR01DnCo_AvgLD1^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))
TempLD2 <- StefBoltz$IR01DnCo_AvgLD2^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))  
#add vectors as column in dataframe
StefBoltz["TempHD1"] <- TempHD1
StefBoltz["TempHD2"] <- TempHD2
StefBoltz["TempLD1"] <- TempLD1
StefBoltz["TempLD2"] <- TempLD2

#Try with daily averages
IRDoyAvgHD1 <- aggregate(StefBoltz$IR01DnCo_AvgHD1, by=list(StefBoltz$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
IRDoyAvgHD2 <- aggregate(StefBoltz$IR01DnCo_AvgHD2, by=list(StefBoltz$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
IRDoyAvgLD1 <- aggregate(StefBoltz$IR01DnCo_AvgLD1, by=list(StefBoltz$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)
IRDoyAvgLD2 <- aggregate(StefBoltz$IR01DnCo_AvgLD2, by=list(StefBoltz$doy),FUN="mean", na.action=na.omit, na.rm=TRUE)  

#rename columns
colnames(IRDoyAvgHD1) [1] <- "Doy"
colnames(IRDoyAvgHD1) [2] <- "IR_Doy_Mean_HD1"

colnames(IRDoyAvgHD2) [1] <- "Doy"
colnames(IRDoyAvgHD2) [2] <- "IR_Doy_Mean_HD2"

colnames(IRDoyAvgLD1) [1] <- "Doy"
colnames(IRDoyAvgLD1) [2] <- "IR_Doy_Mean_LD1"

colnames(IRDoyAvgLD2) [1] <- "Doy"
colnames(IRDoyAvgLD2) [2] <- "IR_Doy_Mean_LD2"

#Merge new DFs
IRdoyJoinHD <- join(IRDoyAvgHD1, IRDoyAvgHD2, by=c("Doy"), type="full")
IRdoyJoinLD <- join(IRDoyAvgLD1, IRDoyAvgLD2, by=c("Doy"), type="full")
doyAvgIrTEMP <- join(IRdoyJoinHD, IRdoyJoinLD, by=c("Doy"), type="full")

#Stef_Bolz EQ to find daily average ground temperature
AvgTempHD1 <- doyAvgIrTEMP$IR_Doy_Mean_HD1^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))
AvgTempHD2 <- doyAvgIrTEMP$IR_Doy_Mean_HD2^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))
AvgTempLD1 <- doyAvgIrTEMP$IR_Doy_Mean_LD1^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))
AvgTempLD2 <- doyAvgIrTEMP$IR_Doy_Mean_LD2^(1/4)*(1/(.97*5.67*(10^-8))^(1/4))

#add vectors to doyAvgIrTemp as columns
doyAvgIrTEMP["AvgTempHD1"] <- AvgTempHD1
doyAvgIrTEMP["AvgTempHD2"] <- AvgTempHD2
doyAvgIrTEMP["AvgTempLD1"] <- AvgTempLD1
doyAvgIrTEMP["AvgTempLD2"] <- AvgTempLD2

#plot daily average ground temperature and Day of Year
plot(doyAvgIrTEMP$Doy, doyAvgIrTEMP$IR_Doy_Mean_HD1, type="l", col="black", xlab = "Day of Year", ylab = "Daily Average Temp")
points(doyAvgIrTEMP$Doy, doyAvgIrTEMP$IR_Doy_Mean_HD2, type = "l", col="darkgreen")
points(doyAvgIrTEMP$Doy, doyAvgIrTEMP$IR_Doy_Mean_LD1, type = "l", col="blue")
points(doyAvgIrTEMP$Doy, doyAvgIrTEMP$IR_Doy_Mean_LD2, type = "l", col="red")

#Plot Summary: all daily temp averages seem to be very similar, ##
#but low density stands lag high density stands by a day or so. ##
#Here, Sensor's above and below the canopy should be the same, right?
#HD1 should equal HD2

#Legend
legend("topright", c("HD1","HD2","LD1", "LD2"), lty = c(1,1,1,1), col=c("black", "darkgreen", "blue", "red"), cex = .7)

#now do the same with the half hourly data
plot(StefBoltz$DayTimeComb, StefBoltz$TempHD1, type="l", col="black", xlab = "Day of Year", ylab = "Daily Average Temp")
points(StefBoltz$DayTimeComb, StefBoltz$TempHD2, type = "l", col="darkgreen")
points(StefBoltz$DayTimeComb, StefBoltz$TempLD1, type = "l", col="blue")
points(StefBoltz$DayTimeComb, StefBoltz$TempLD2, type = "l", col="red")

#try to plot albedo and daily average temperature next to each other
plot(doyAvgIrTEMP$Doy, doyAvgIrTEMP$AvgTempHD1*.0007, type = "l", ylim = c(0,.4))
points(doyAvgIrTEMP$Doy, JoinDailyM$Daily_Mean_HD1, type = "l")
##THINK OF BETTER WAY TO PUT THEM SIDE BY SIDE