#-------------------------------------------------------
rm(list= ls())
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("lubridate")
install.packages("janitor")
install.packages("tidyverse")
install.packages("maps")
install.packages("ggforce")
install.packages("data.table")
install.packages("skimr")
install.packages("maps")
install.packages("DataExplorer")
install.packages("plotly")
install.packages("ggmap")
install.packages("ggthemes")
#----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(maps)
library(janitor)
library(ggforce)
library(data.table)
library(skimr)
library(mapview)
library(DataExplorer)
library(plotly)
library(ggmap)
library(ggthemes)
#--------------------------------------------------------------------------------
getwd()
setwd(choose.dir())
accidents <- read.csv("Accidents.csv")
accidents<-distinct(accidents)#removed 1 duplicate row
accidents$Date <-as.Date(accidents$Date)
str(accidents)
summary(accidents)
accident.cleaned <- na.omit(accidents)# number of observations reduced from 129981 to 129962
#Mode_Function
mode <- function(x) {
  uniquevalue <- unique(x)
  uniquevalue[which.max(tabulate(match(x, uniquevalue)))]
}

#-------------------------------------------------------------------------------
I1 <- accident.cleaned%>%tabyl(Police_Force) 
I1%>%arrange(I1, desc(n))
str(I1)
I1<- data.table(I1)
str(I1)
names(I1)[names(I1)=="n"] <- "Number_of_Accidents"
View(I1)
mode(I1$Police_Force)
I1_PF<- I1[order(I1$Number_of_Accidents, decreasing = TRUE) ]
I1_PF<-top_n(I1_PF,10)
I1_PF <- data.frame(I1_PF)
ggplot(I1_PF,aes(x=factor(Police_Force),y=Number_of_Accidents))+geom_col(color='black',fill='cyan3') + geom_point(color = "red") + geom_text(aes(label = Number_of_Accidents), vjust= -1 , hjust = .5, color="BLACK", size=3, angle = 0) + xlab('Police Forces')+ ylab('Accidents')+labs(title = "Accidents across different Police Forces", subtitle = "Plot of Police Forces Versus Number of Accidents",caption = "Data source: Accident.csv")+theme_classic() 
#-------------------------------------------------------------------------------

I1_PF_AS<-accident.cleaned%>%tabyl(Police_Force, Accident_Severity)%>%drop_na()
I1_PF_AS<-top_n(I1_PF_AS,20)
I1_PF_AS<- data.frame(I1_PF_AS)
I1_PF_AS$Police_Force<-as.factor(I1_PF_AS$Police_Force)
names(I1_PF_AS)[names(I1_PF_AS) == "X3"] <- "Severity 3"
names(I1_PF_AS)[names(I1_PF_AS) == "X2"] <- "Severity 2"
names(I1_PF_AS)[names(I1_PF_AS) == "X1"] <- "Severity 1"
Plot_PFAS <- plot_ly(I1_PF_AS, x = I1_PF_AS$Police_Force, y = I1_PF_AS$Severity.1, type = 'scatter', name = 'Severity 1', marker = list(color = 'green'))
Plot_PFAS<- Plot_PFAS %>% add_trace(y =I1_PF_AS$Severity.2, name = 'Severity 2', marker = list(color = 'yellow'))
Plot_PFAS <- Plot_PFAS%>% add_trace(y =I1_PF_AS$Severity.3, name = 'Severity 3', marker = list(color = 'red'))
#Plot_PFAS <- Plot_PFAS %>% layout(xaxis = list(title = "Police Forces", tickangle = 0),yaxis = list(title = "Number of Accidents"),margin = list(b = 100),barmode = 'group')
Plot_PFAS<- Plot_PFAS%>%layout(title = "Number of accidents classified by Severity ")
Plot_PFAS
#-------------------------------------------------------------------------------

I2<-accident.cleaned$Day_of_Week
I2<-tabyl(I2)
names(I2)[names(I2)=="n"] <- "Number_of_Accidents"
names(I2)[names(I2)=="I2"] <- "Day_of_the_Week"
I2<-data.table(I2)
ggplot(I2,aes(x=factor(Day_of_the_Week),y=Number_of_Accidents))+geom_col(color='black',fill= "grey")+xlab('Day of The Week')+ ylab('Accidents') + coord_flip() + geom_text(aes(label = Number_of_Accidents), vjust= .5 , hjust = 1.25, color="BLACK", size=4, angle = 0)+ labs(title = "Accidents across different Days of the Week", subtitle = "Plot of Days of the Week Versus Number of Accidents",caption = "Data source: Accident.csv")+theme_classic()
#-------------------------------------------------------------------------------

I3 <- select(accidents,Accident_Severity,Time)
#separate time
I3 <- separate(data = I3,col = Time,into = c("Hour", "Minutes"),sep=':')
#remove NA
I3<-na.omit(I3)
#Convert data type
I3$Time_slot <-as.numeric(I3$Hour) 
#Calculate the number
num_sev <- c(count(I3,I3$Accident_Severity))
hour_slot <- c(count(I3,I3$Time_slot))
#Classification morning and evening
I3$DayOrNight <- ifelse(I3$Time_slot<= 6 | I3$Time_slot>= 18,"night","day")
DayFatal <- length((which(I3$DayOrNight=="day" & I3$Accident_Severity==1)))
DayUnfatal <- length((which(I3$DayOrNight=="day" & I3$Accident_Severity==2 | I3$Accident_Severity==3)))
NightFatal <- length((which(I3$DayOrNight=="night" & I3$Accident_Severity==1)))
NightUnfatal <- length((which(I3$DayOrNight=="night" & I3$Accident_Severity==2 | I3$Accident_Severity==3)))
num_Day <- c(DayFatal,DayUnfatal)
num_night <- c(NightFatal,NightUnfatal)
num_all <- c(num_Day,num_night)
m <- matrix(num_all,nrow=2)
chisq.test(m)
#Plotting for I3
ggplot(I3, aes(x = DayOrNight,fill=Accident_Severity)) + geom_bar() +xlab('Part of the Day')+ ylab('Number of Accidents')+labs(title = "Accident Severity across Day and Night", subtitle = "Plot of Three accident Severities Versus Number of Accidents",caption = "Data source: Accident.csv")+theme_classic() 

I3$Accident_Severity <-as.factor(I3$Accident_Severity)
I3_AS1 <- I3%>%filter(as.integer(Accident_Severity) == 1)
ggplot(I3_AS1, aes(x = DayOrNight,fill= Accident_Severity )) + geom_bar() +xlab('Part of the Day')+ ylab('Number of Accidents')+labs(title = "Accident Severity across Day and Night", subtitle = "Plot of  Severity 1 Versus Number of Accidents",caption = "Data source: Accident.csv")+theme_classic() 

#-------------------------------------------------------------------------------

I4<-accident.cleaned$Road_Type
I4<-tabyl(I4)
names(I4)[names(I4)=="n"] <- "Number_of_Accidents"
names(I4)[names(I4)=="I4"] <- "Road_Type"
View(I4)
I4<-data.table(I4)
ggplot(I4,aes(x=factor(Road_Type),y=Number_of_Accidents))+geom_col(color='black',fill= "grey")+xlab('Road Type')+ ylab('Accidents')  + geom_text(aes(label = Number_of_Accidents), vjust= - 0.6 , hjust = 0.5, color="BLACK", size=3, angle = 0) + labs(title = "Number of Accidents across Different Road types", subtitle = "Plot of Road types Versus Number of Accidents",caption = "Data source: Accident.csv") + scale_x_discrete(labels= c("Roundabout","One way street", "Dual carriageway", "Single carriageway","Slip road","Unknown")) + theme_classic()
#-------------------------------------------------------------------------------

#1. Plots relating to the Data in the Dataset.
plot_str(accidents)
introduce(accidents)
plot_intro(accidents)
plot_missing(accidents)
str(accident.cleaned)
I6<- subset( accident.cleaned, select = c(Time, Date, Light_Conditions, Weather_Conditions, Speed_limit,Local_Authority_District, Accident_Severity))
I6$Date<- as.Date(I6$Date)
I6 <- I6[I6$Light_Conditions >= 0, ] 
I6 <- I6[I6$Weather_Conditions >= 0, ]
I6 <- I6[I6$Speed_limit >= 0, ]
summarise(I6)
I6<-data.frame(I6)
I6<-I6%>%arrange(Speed_limit)%>%group_by(Speed_limit)

#Plotting Speed limit vs accidents
I6$Speed_limit <-as.factor(I6$Speed_limit)
I6%>%ggplot(aes(x =(I6$Speed_limit) )) + geom_bar() +labs( x = "Speed Limits", y = "Number of Accidents",title ="Speed Limits vs Accidents")+ labs(title = "Number of Accidents across Different Speed Limits", subtitle = "Plot of Speed Limits Versus Number of Accidents",caption = "Data source: Accident.csv") + theme_classic()

#Plotting Weather vs Accidents
I6%>%ggplot(aes(x = as.factor( Weather_Conditions) )) + geom_bar() +labs( x = "Weather Conditions", y = "Number of Accidents") + labs(title = "Number of Accidents during Different Weather Conditions", subtitle = "Plot of Weather Conditions Versus Number of Accidents",caption = "Data source: Accident.csv") +theme_classic()

#Plotting Accident Severity VS Weather Conditions
I6_AS_SL<-tabyl(I6,Accident_Severity, Speed_limit)
I6_AS_SL<-data.table(I6_AS_SL)
str(I6_AS_SL)
View(I6_AS_SL)
ggplot(I6, aes(x= Accident_Severity, y = Speed_limit)) + geom_count(colour = "Purple")+ labs( x= "Accident Severity", y = "Speed Limit")+ labs(title = "Relationship between Speed limit and the Severity of the accident", subtitle = "Plot of Speed limit Versus Severity",caption = "Data source: Accident.csv") +theme_classic()

tabyl(I6$Speed_limit)
tabyl(I6$Light_Conditions)
tabyl(I6$Weather_Conditions)

# Plotting Month vs Number of Accidents
I6_M <- separate(data = accident.cleaned,col = Date,into = c("Year", "Month","Date"),sep='-')
I6_M<-table(I6_M$Month)
I6_M <- data.frame(I6_M)
View(I6_M)
I6_M<-rename(I6_M, month = Var1, number_of_accidents = Freq)
View(I6_M)
PlotMN<-plot_ly(x = I6_M$month, y = I6_M$number_of_accidents,type = "bar",text = I6_M$number_of_accidents, textposition = 'auto',marker = list(color = 'rgb(158,202,225)',line = list(color = 'rgb(8,48,107)', width = 1.5))) 
PlotMN%>%layout(title = "Number of accidents grouped month wise", xaxis = list(title = "Months"), yaxis = list(title = "Number of Accidents"))

#------------------------------------------------------------------
#Plotting Accident Hotspots across UK, where Severity >= 5
accident1<-accident.cleaned%>%filter( Number_of_Casualties >= 5)
accident1$Longitude<- as.numeric(accident1$Longitude)
accident1$Latitude<- as.numeric(accident1$Latitude)
accident1$Accident_Severity<- as.factor(accident1$Accident_Severity)
str(accident1)
uk_map <-get_stamenmap(bbox = c(left= -14.8, bottom = 48.96 ,right = 9.339, top = 57.445), maptype = "terrain", zoom = 9)
ggmap(uk_map)+ geom_point(data = accident1, aes(x =Longitude, y= Latitude,alpha = .01 ,color = Accident_Severity))+labs(title = "Accident Hotspots in United Kingdom", subtitle = "Where the number of casualities is greater than or equal to five",caption = "Data source: Accident.csv") +theme_map()

#-----------------------------------------------------------------
#Additional Plots 
#special conditions at site vs severity
accident_SCAS <-accident.cleaned%>%filter(Special_Conditions_at_Site != -1)
accident_SCAS%>%ggplot(aes( x= as.factor(Special_Conditions_at_Site), y = Accident_Severity ))+ geom_count( colour = "steelblue") + labs( x= "Special Conditions at Site", y = "Accident Severity")+ labs(title = "Relationship between Special Site Conditions and the Severity of the accident", subtitle = "Plot of Special Conditions Versus Severity",caption = "Data source: Accident.csv") + theme_classic()
accident_RSA<-accident.cleaned%>%filter( Road_Surface_Conditions >= 0 )
accident_RSA%>%ggplot(aes( x= as.factor(Road_Surface_Conditions)))+ geom_bar( fill = "steelblue") + labs( x= "Road Surface Conditions", y = "Number of Accidents")+ labs(title = "Relationship between Road's Surface Conditions and the Number of the accident", subtitle = "Plot of Surface Conditions Versus Number of Accidents",caption = "Data source: Accident.csv") +theme_classic() + scale_x_discrete(labels= c("Dry","Wet or damp","Snow","Frost or ice","Flood over 3cm. deep","Oil or diesel","Mud")) 
#-----------------------------------------------------------------
#Plot of Local Authority District
I6_LAD <- accident.cleaned%>%tabyl(Local_Authority_District)
I6_LAD%>%arrange(I6_LAD, desc(n))
I6_LAD<- data.table(I6_LAD)
names(I6_LAD)[names(I6_LAD)=="n"] <- "Number_of_Accidents"
I6_LAD<-top_n(I6_LAD,10)
I6_LAD <- data.frame(I6_LAD)
ggplot(I6_LAD,aes(x=factor(Local_Authority_District),y=Number_of_Accidents))+geom_col(color='black',fill='grey25')  + geom_text(aes(label = Number_of_Accidents), vjust= -1 , hjust = .5, color="BLACK", size=3, angle = 0) + xlab('Local Authority(District)')+ ylab('Accidents')+labs(title = "Accidents across different Local Authority", subtitle = "Plot of Local Authority (District) Versus Number of Accidents",caption = "Data source: Accident.csv")+theme_classic() 