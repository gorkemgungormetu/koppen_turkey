# Read precipitation and temperature data and couple data with stations having both precipitation and temperature data
precipitation<-read.csv("precipitation.csv",sep=",",header=TRUE)
precipitation<-subset(precipitation,P...T=="Yes")
temperature<-read.csv("temperature.csv",sep=",",header=TRUE)
temperature<-subset(temperature,P...T=="Yes")

# Create Köppen classification columns with factor values
temperature$first<-NA
levels(temperature[,"first"])<-c("C","D")
temperature$second<-NA
levels(temperature[,"second"])<-c("s","w","f")
temperature$third<-NA
levels(temperature[,"third"])<-c("a","b","c","d")

# Identify climate classes for each region only for C and D classes
for (i in 1:nrow(temperature))
{
  if(temperature[i,"HotMonth"]>18 & temperature[i,"ColdMonth"]>0 & temperature[i,"ColdMonth"]<18)
  {
    temperature[i,"first"]<-"C"
    if(precipitation[i,"AMJJAS_DryM"]<40 & precipitation[i,"AMJJAS_DryM"]<(precipitation[i,"ONDJFM_WetM"]/3))temperature[i,"second"]<-"s"
    else if(precipitation[i,"ONDJFM_DryM"]<(precipitation[i,"AMJJAS_WetM"]/10))temperature[i,"second"]<-"w"
    else temperature[i,"second"]<-"f"
    if (temperature[i,"HotMonth"]>=22)temperature[i,"third"]<-"a"
    else if(temperature[i,"MonthsAbove10"]>=4) temperature[i,"third"]<-"b"
    else if(temperature[i,"MonthsAbove10"]>=1 & temperature[i,"MonthsAbove10"]<4)temperature[i,"third"]<-"c"
  }
  if(temperature[i,"HotMonth"]>10 & temperature[i,"ColdMonth"]<=0) 
  {
    temperature[i,"first"]<-"D"
    if(precipitation[i,"AMJJAS_DryM"]<40 & precipitation[i,"AMJJAS_DryM"]<(precipitation[i,"ONDJFM_WetM"]/3))temperature[i,"second"]<-"s"
    else if(precipitation[i,"ONDJFM_DryM"]<(precipitation[i,"AMJJAS_WetM"]/10))temperature[i,"second"]<-"w"
    else if(temperature[i,"MonthsAbove10"]>=1 & temperature[i,"MonthsAbove10"]<4)temperature[i,"third"]<-"c"
    else temperature[i,"second"]<-"f"
    if (temperature[i,"HotMonth"]>=22)temperature[i,"third"]<-"a"
    else if(temperature[i,"MonthsAbove10"]>=4) temperature[i,"third"]<-"b"
    else if(temperature[i,"ColdMonth"]<-38) temperature[i,"third"]<-"d"
    else temperature[i,"third"]<-"c"
  }
}

# Factorization of climate classes
temperature$first<-as.factor(temperature$first)
temperature$second<-as.factor(temperature$second)
temperature$third<-as.factor(temperature$third)

# Initialization of graphics display area and plotting of results
par(mfrow=c(2,2),mar=c(2,4,2,1))
boxplot(Elevation~second*first,data=temperature,names=c('Cf','Cs','Df','Ds'),main='Figure 1 - Elevation',ylab='Elevation (m)')
temperature$sea<-as.integer(c("1","1","1","1","1","1","1","1","1","0","1","0","0","0","0","0","0","0","0","0","1","1","0","0","0","0","0","1","0","0","0","0","0","0","0","0","1","0","0","0","0","0","1","1","0","1"))
boxplot(sea~second*first,data=temperature,names=c('Cf','Cs','Df','Ds'),main='Figure 2 - Land vs. water')
legend("topright",bty = "n", xpd = TRUE,legend=c('1 = coastal','0 = inner'))
boxplot(Latitude~second*first,data=temperature,names=c('Cf','Cs','Df','Ds'),main='Figure 3 - Latitude',ylab='Degrees Latitude')
boxplot(Longitude~second*first,data=temperature,names=c('Cf','Cs','Df','Ds'),main='Figure 4 - Longitude',ylab='Degrees Longitude')

