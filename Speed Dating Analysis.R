###Speed Dating Exploration 
#This dataset was compiled by Columbia Business School professors Ray Fisman and Sheena Iyengar for 
#their paper Gender Differences in Mate Selection: Evidence From a Speed Dating Experiment.

#Data was gathered from participants in experimental speed dating events from 2002-2004. During the 
#events, the attendees would have a four minute "first date" with every other participant of the opposite 
#sex. At the end of their four minutes, participants were asked if they would like to see their date again. 
#They were also asked to rate their date on six attributes: Attractiveness, Sincerity, Intelligence, Fun, 
#Ambition, and Shared Interests.

##Prepare Data

#load libraries
library(ggplot2)
library(wesanderson) #for color palletes
library(plyr)

#set working directory
setwd("C:/Users/smcnish/Documents/Vizs/Dating")

#read data
allData = read.csv(file="Speed Dating Data.csv", header=TRUE)

#keep only variables you want
keepvars<- c("iid", "gender", "wave", "position", "pf_o_att", 
             "pf_o_sin","pf_o_int","pf_o_fun","pf_o_amb","pf_o_sha",
             "dec_o","attr_o", "sinc_o", "intel_o", "fun_o", "amb_o",
             "shar_o","date_3")

data<-allData[keepvars]

#which variables do you want to factor
fact<- c("gender", "dec_o","date_3") 

#make vars a factor
data[fact] <- lapply(data[fact] , factor) 

#name levels of factors
levels(data[,"gender"])<- c("Female","Male")
levels(data[,"dec_o"])<- c("No","Yes")
data$dec_o <- factor(data$dec_o, c("Yes", "No"))
levels(data[,"date_3"])<- c("No","Yes")
data$date_3 <- factor(data$date_3, c("Yes", "No"))

##Did men or women recieve more yes's?
ggplot(data, aes(gender, fill=dec_o)) + geom_bar() + 
    ggtitle("Did Men or Women Receive more Yes's?") + labs(x="Gender",y="Count") + 
    scale_fill_manual(name = "Decision", values=wes_palette(n=2, name="GrandBudapest"))
#it looks like women recieved more yes's than men. Perhaps women are more picky?

##How many of the Yes decisions led to a date?
#get data where decision was yes (and they responded to the survey 3 weeks later)
data_yes2<-subset(data, subset=(dec_o=="Yes")&(!is.na(date_3)))

ggplot(data_yes2, aes(gender, fill=date_3)) + geom_bar() + 
    ggtitle("Are Men or Women More likely to follow through and go on a date?") + labs(x="Gender",y="Count") +
    scale_fill_manual(name = "Follow up Date", values=wes_palette(n=2, name="GrandBudapest"))
#it looks like more women followed through with dates, however the respose rate was greater for women than men

##What position were people more likely to get a yes

ggplot(data, aes(position, fill=dec_o)) + geom_bar() +
  ggtitle("What position faired most Sucessful in a Yes decision?") + labs(x="Wave",y="Count") +
  scale_fill_manual(name = "Decision", values=wes_palette(n=2, name="GrandBudapest")) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) + 
  scale_y_continuous(expand = c(0, 0))
#this is hard to compare since differnt waves had a different number of positions. 
#Let's get just the last position of each wave

#compare first position to last position of wave

#no position 1 in wave 17, so we will code position 2 as for that wave for easy comparison
data17<- within(data, position[position == 2 & wave == 17] <- 1)
pos1 <- subset(data17, subset = (position ==1))

#get max position in each wave
maxpos<- setNames(aggregate(data$position, by = list(data$wave), max),c("wave","position"))
#get data for only last position
pos.last <-  subset(merge(data,maxpos), position == position & wave == wave)        
pos.last$position <- "Last"
#append data for first and liast postiions into one data frame
data1.last<-rbind(pos1, pos.last)
data1.last$position <- as.factor(data1.last$position)


ggplot(data1.last, aes(position, fill=dec_o)) + geom_bar() +
  ggtitle("Was there a difference in Yes decisions between the first and last position?") + labs(x="Position",y="Count") +
  scale_fill_manual(name = "Decision", values=wes_palette(n=2, name="GrandBudapest")) 
#looks like an equal distribution between those in the first vs last position
