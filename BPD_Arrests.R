
setwd("E:/SS/AV/OnlineHack/Kaggle/Baltimore Police Detention")

# Load The Data
data <- read.csv("BPD_Arrests.csv")

# Summary Of Data
summary(data)

# Exploratory Data Analysis
hist(data$Age)

# Age Distribution
library(ggplot2)
ggplot(data=data, aes(data$Age))+geom_histogram(breaks=seq(0, 100, by = 3),col="red", fill="green",alpha = .2)+
    labs(title="Histogram for Age")+labs(x="Age", y="Count")+xlim(c(0,100)) + ylim(c(0,20000))

# Sex Distribution
ggplot(data,aes(Sex))+geom_bar(col="red", fill="green",alpha = .5)

# Race Distribution
ggplot(data,aes(Race))+geom_bar(col="blue", fill="green", alpha=0.3)


#ArrestDate
data$ArrestDate  <- as.Date(data$ArrestDate, "%m/%d/%Y")
data$Arr_Month   <- as.factor(months(data$ArrestDate))
data$Arr_Quarter <- as.factor(quarters(data$ArrestDate))
data$Arr_Year    <- as.factor(substr(data$ArrestDate,1,4))
data$Weekday     <- as.factor(weekdays(data$ArrestDate))

## Distribution of arrest WeekDay  basis
ggplot(data, aes(Weekday))+geom_bar(col="red", fill="green", alpha=0.2)+
    theme(axis.text.y = element_text(face="bold", color="#993333", size=8),
          axis.text.x = element_text(face="bold", color="#993333", size=6, angle=45))+labs(x="WeekDay Of Arrest")



## Distribution of arrest month basis
ggplot(data, aes(Arr_Month))+geom_bar(col="red", fill="green", alpha=0.2)+
     theme(axis.text.y = element_text(face="bold", color="#993333", size=8),
           axis.text.x = element_text(face="bold", color="#993333", size=6, angle=45))+labs(x="Month Of Arrest")


## Distribution of arrest Quarter basis
ggplot(data, aes(Arr_Quarter))+geom_bar(col="red", fill="green", alpha=0.2)+
    theme(axis.text.y = element_text(face="bold", color="#993333", size=8),
          axis.text.x = element_text(face="bold", color="#993333", size=6, angle=45))+labs(x="Quarter Of Arrest")


## Distribution of arrest Year basis
ggplot(data, aes(Arr_Year))+geom_bar(col="red", fill="green", alpha=0.2)+
    theme(axis.text.y = element_text(face="bold", color="#993333", size=8),
          axis.text.x = element_text(face="bold", color="#993333", size=6, angle=45))+labs(x="Year Of Arrest")


## Distribution Of ArrestTime 
time  <- data$ArrestTime
time1 <- gsub('[.:]', '', time , perl = TRUE)
time1 <- as.character(time1)
time1 <- ifelse(nchar(time1)==3,paste0("0",time1),time1)
time2 <- as.factor(substr(time1,1,2))

ggplot(data, aes(time2))+geom_bar(col="red", fill="green", alpha=0.2)+labs(x="Time Of Arrest")


# Distribution of ArrestLocation
data$ArrestLocation <- as.character(data$ArrestLocation)
data$ArrestLocation[data$ArrestLocation==""] <- "Unknown"
ArrestLocation <- data.frame(sort(table(data$ArrestLocation),decreasing=TRUE)[1:15])

ArrestLocation
#                   Var1  Freq
#1                Unknown 52118
#2         200 N EUTAW ST   447
#3       1600 W NORTH AVE   344
#4        1500 RUSSELL ST   306
#5     400 E LEXINGTON ST   297
#6         300 N EUTAW ST   237
#7     400 E BALTIMORE ST   230
#8      400 W SARATOGA ST   219
#9   5100 REISTERSTOWN RD   211
#10        600 LAURENS ST   200
#11      1500 W NORTH AVE   199
#12    2400 FREDERICK AVE   196
#13 5100 PARK HEIGHTS AVE   185
#14    600 CHERRY HILL RD   155
#15       1800 W PRATT ST   154

##     Distribution Of    IncidentOffense
# Top 15 incident Offence
sort(table(data$IncidentOffense),decreasing=T)[1:15]

#Unknown Offense             87-Narcotics        4E-Common Assault          UNKNOWN OFFENSE 
#82046                    12294                     6304                     3209 

#87O-Narcotics (Outside)  6C-Larceny- Shoplifting                 79-Other         24-Towed Vehicle 
#2602                     2222                     1959                     1581 

#97-Search & Seizure     4C-Agg. Asslt.- Oth.      4B-Agg. Asslt.- Cut    5A-Burg. Res. (Force) 
#1544                     1466                     1302                      852 

#55-Disorderly Person         55A-Prostitution 75-Destruct. Of Property 
#817                      666                      640 


##    Distribution Of    IncidentLocation
IncidentLocation <- data$IncidentLocation
IncidentLocation <- as.character(IncidentLocation)
IncidentLocation[IncidentLocation==""] <- "Unknown"
sort(table(IncidentLocation),decreasing=T)[1:15]


## Distribution Of  Charge
charge <- data$Charge
charge <- as.character(charge)
charge[charge==""] <- "Unknown"
sort(table(charge),decreasing=T)[1:15]

# charge
#4 3550  1 1415 Unknown  1 0077  1 1635  1 0573  3 0233  1 0088  2A0696  1 1420  1 0521  1 0621  2 0050 
#19116   18138   16458   16269    5255    4247    4115    3334    2411    2398    2231    2073    1682 

#1 1093  2 0696 
#1646    1400 

##   Distribution Of   ChargeDescription

ChargeDescription <- data$ChargeDescription
sort(table(ChargeDescription),decreasing=T)[1:15]


## Distribution Of District
data$District <- as.character(data$District)
data$District[data$District==""] <- "Unknown"
sort(table(data$District), decreasing=T)

ggplot(data,aes(District))+geom_bar(col="red",fill="green", alpha=0.3)+
    labs(title="District Distribution")+
    xlab("Name Of District") + theme(axis.text.x = element_text(face="bold", color="black", angle=45, size=8))


## Distribution Of Post  (Top - 15)
sort(table(data$Post), decreasing=T)[1:15]
#733  223  224  732  111  212  221  113  911  634  933  912  935  913  333 
#1812 1721 1645 1457 1270 1239 1199 1141 1133 1117 1065 1059 1024 1023 1022 

## Distribution Of Neighborhood
Neighborhood <- as.character(data$Neighborhood)
Neighborhood[Neighborhood ==""] <- "Unknown"
sort(table(Neighborhood), decreasing=T)[1:15]

## Distribution Of Location.1
data$Location.1
lat <- as.numeric(substr(as.character(data$Location.1),2,13)) #lat
lon <- (as.numeric(substr(as.character(data$Location.1),16,27))) #lon
df <- as.data.frame(cbind(lat,lon))
df1 <- na.omit(df)

#plot(df1$lon, df1$lat, xlab = "Longitude", ylab = "Latitude", main = "Arrests in Baltimore")

library(ggmap)
#map1 <- get_map(location = "baltimore", zoom = 4)
map <- get_map(location = c(lon = mean(df1$lon), lat = mean(df1$lat)), zoom = 12,maptype = "terrain", scale = 2)
ggmap(map)+geom_point(data=df1,aes(x = df1$lon, y = df1$lat,fill="red", alpha=0.6),size=0.4,shape=21)+
    guides(fill=FALSE, alpha=FALSE, size=FALSE)



##  Distribution of Arrest Year & Arrest month 
ggplot(data,aes(x=Arr_Year, fill=Arr_Month))+geom_bar(stat="count")

##  Distribution of Weekdays & Time 
time_cat <- as.integer(time2)
time_cat[time_cat==1|time_cat==2 |time_cat==3] <- "0-3"
time_cat[time_cat==4|time_cat==5 |time_cat==6] <- "3-6"
time_cat[time_cat==7|time_cat==8 |time_cat==9] <- "6-9"
time_cat[time_cat==10|time_cat==11|time_cat==12] <- "9-12"
time_cat[time_cat==13|time_cat==14 |time_cat==15] <- "12-15"
time_cat[time_cat==16|time_cat==17 |time_cat==18] <- "15-18"
time_cat[time_cat==19|time_cat==20 |time_cat==21] <- "18-21"
time_cat[time_cat==21|time_cat==22|time_cat==23] <- "21-24"

ggplot(data,aes(x=Weekday, fill=time_cat))+geom_bar(stat="count")

##  Distribution of Year & Quarter
ggplot(data,aes(x=Arr_Year, fill=Arr_Quarter))+geom_bar(stat="count")

## Distribution of Age & Sex
age_cat <- cut(data$Age,5)
ggplot(data,aes(x=Sex, fill=age_cat))+geom_bar(stat="count")


## Distribution of Age & Race
age_cat <- cut(data$Age,5)
ggplot(data,aes(x=Race, fill=age_cat))+geom_bar(stat="count")

## Distribution of  Race & Sex
ggplot(data,aes(x=Race, fill=Sex))+geom_bar(stat="count")

## Distribution of  Year & Sex
ggplot(data,aes(x=Arr_Year, fill=Sex))+geom_bar(stat="count")

## Distribution of  Year & Race
ggplot(data,aes(x=Arr_Year, fill=Race))+geom_bar(stat="count")

## Distribution of  Year & Time
ggplot(data,aes(x=Arr_Year, fill=time_cat))+geom_bar(stat="count")

## Distribution of  Year & District
ggplot(data,aes(x=Arr_Year, fill=District))+geom_bar(stat="count")


# Frequency of Arrest on location
library(plyr)
df2 <- df1
df2 <- ddply(df2,.(df2$lat,df2$lon),nrow)
names(df2) <- c("lat","lon","Freq")

ggmap(map) + geom_tile(data = df2, aes(x = lon, y = lat, alpha = (Freq/400))) 






























































































