library(gganimate)
library(animation)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)

##########################
# Step 1: Clean the Data #
##########################

#get data
dataRaw <-read.csv("C:/Users/mayab/OneDrive/Desktop/DataMining/week7_Lab/openpowerlifting.csv")

#clean data
from2018 <-dataRaw[dataRaw$Date >= "2018-01-01",]
only2018 <-from2018[from2018$Date <= "2018-12-31",]
head(only2018)

#remove name
only2018 <-only2018[,-1]
#remove non-best attempts
only2018 <-only2018[,-9:-12]
only2018 <-only2018[,-10:-13]
only2018 <-only2018[,-11:-14]
only2018 <-only2018[,-14:-20]
head(only2018)

noNeg <- only2018[only2018$Best3SquatKg >= 0, ]
noNeg <- noNeg[noNeg$Best3BenchKg >= 0, ]
noNeg <- noNeg[noNeg$Best3DeadliftKg >= 0, ]
noNeg <- noNeg[noNeg$MeetName >= "Nationals", ]
noNeg <- noNeg[noNeg$Age >= 12.1, ]
noNeg <- na.omit(noNeg)

##########################
# Step 2: Compare Lifts  #
##########################

#scatter plot
s <-ggplot(noNeg,aes(x=Best3SquatKg ,y=Best3DeadliftKg,color=Best3BenchKg)) +
  geom_point(size=1) +
  scale_color_gradientn(colors=c("red","yellow","green","cyan", "magenta")) +
  theme(panel.background = element_rect(fill = "black")) +
  geom_jitter(size = 0.5, width = 0.5) +
  labs(title = "Squat vs Deadlift vs Bench", x = "Squat (kg)", y= "Deadlift (kg)", color = "Bench (kg)")
s

###########################################
# Step 3: Total Lift Range by Age Class   #
###########################################

#boxplot
b <- ggplot(noNeg, aes(x=AgeClass, y=TotalKg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(title = "Lift Ranges by Age",x = "Age Class", y= "Avg Total Kg")
b
##########################
# Step 4: Compare Sex    #
##########################

#histogram
mu <- ddply(noNeg, "Sex", summarise, grp.mean=mean(TotalKg))
head(mu)
p<-ggplot(noNeg, aes(x=TotalKg, fill=Sex, color=Sex)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 50) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Sex),linetype="dashed") +
  labs(title = "Female vs Male Total Lift Distribution", x = "Total Lift (kg)",
       y= "Count")
p

##################
# Step 5: Time   #
##################

#make row for average kg on each day
noNeg <- ddply(noNeg, "Date", mutate, mean.TotalKg = mean(TotalKg))
#take out duplicates
#noNeg <- noNeg %>% distinct(Date, .keep_all = T)
noNeg$mean.TotalKg <- as.factor(noNeg$mean.TotalKg)
noNeg$mean.TotalKg <- as.numeric(as.character(noNeg$mean.TotalKg))
noNeg$Date <- as.Date(noNeg$Date)

#make plot
t <- ggplot(data = noNeg, aes(x=Date, y=mean.TotalKg, group=2))+
  geom_point() +
  geom_line() +
  labs(title = "Total Lifted Kg Throughout the Year", y= "Avg Total Kg")+
  transition_reveal(Date)
t

#save as gif
anim_save("visuals_Final.gif", t)