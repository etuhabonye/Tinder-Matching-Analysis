#The purpose of this research was to compare Tinder matches from different cities around the world.
#In each city the data observed was,the Predominant language, the number of matches and messages per match, 
#The average type of text and and average characters per opening text. This study was done swiping across the world for women and men.
#The data observed for women was majoritively collected in North America.
#In each City the Tinder profile was the same. There was no bio and there were no pictures that would indicate where the researchers were from.
#The location for each researcher was turned off so swipers being observed would not be deterred from swiping someone so far away.
#One hundred people were swiped on. The researchers would not look who they were swiping on so there would be no bias.
#The researchers waited 24 hours to give each person a chance to see if there was a match and message the researcher.
#The researchers would never message first. The people swiped on were in the age group 18-25 and in each city a 50 mile radius
#was set to avoid only looking at one side or specific part of the city. 
#The collection of this data is inspired by a TikTok by Amanda Jagg (@amandajagg)
#The data observed is interesting because it will give insight to the standards of beauty and online dating culture around the world. 
#One of the researchers is a black women swiping with men around the world, named Emma, and the other is a white blonde women swiping with women around the world, named Jane.
#Since the researchers are dealing with different languages, to collect the average characters per text message, the messages were translated to English and the translated sentence was counted.

require(rgdal)
require(leaflet)
require(geojsonio)
require(readr)
require(data.table)
require(dplyr)
require(RColorBrewer)
require(shiny)
require(ggplot2)

getwd()

library(readr)
research_emma <- read.csv("research_emma.csv")
View(research_emma)

names(research_emma)<-c("", "City", "Continient","PredomLang", "Matches","Inapp","Messages","Superlikes","TypeofText","AvgChar", "lat","long")
str(research_emma$AvgChar)
str(research_emma$lat)
str(research_emma$long)
str(research_emma$Matches)
str(research_emma$Inapp)
str(research_emma$Messages)
str(research_emma$Superlikes)

research_emma<-research_emma[-c(20:999),-c(13:24)]
research_emma<-research_emma[,-c(1,13:14)]

research_emma$messrate<-NA
research_emma$messrate<-(research_emma$Messages/research_emma$Matches)


#Emma
ggplot(data=research_emma)+
  geom_tile(aes(x=TypeofText, y=City, fill=AvgChar))+
  scale_fill_distiller("Average Characters",palette="RdPu", direction = 1)+
  ylab("City")+
  xlab("Type of Text Messages")+
  ggtitle("Average Type of Text per City based on Average Characters per Message")

#In this visualization, the Average Type of Text in each city is shown for Emma. 
#The average amount of characters is looked in each tile. The higher the average character of text could mean the more effort someone has put into a conversation.
#This effort could be translated as how meaning the match was to a person. Places with higher characters 
#may have more people looking for meaningful connections on their online dating interactions.
#France has the longest messages while Tbilisi has the shortest. 
#Most of the Cities use greetings in their opening messages while the amount of cities that start with conversation starters and compliments are equal.
#while France and Tbilisi both have majoritively more compliments,
#France had lengthier compliments, while Tbilisi had shorter compliments
#The average characters per text appear to be tied to the type of  message. 

#-----------------------------------------------------------------------------

#graph 2

pal<-colorQuantile("RdYlBu",domain = NULL, n=5)

leaflet(data=research_emma) %>%
  addTiles() %>%
  addCircles(lng=~long,
             lat=~lat,
             popup=~paste(City,"<br>Matches:",Matches,
                          "<br>Messages:", Messages),
             radius=500000,
             color= ~pal(messrate),
             fillOpacity = 0.8) %>%
  addLegend("bottomright", colors=brewer.pal(5,"RdYlBu"),
            labels = c("low", "","","","high"),
            title = "Relative Messages per Match")


#Emma averaged around swipes in the 40s and 50 around the world.
#The area that she reached the most amount of matches was Uganda. This could be because Emma looks more like the people who live there.
#There is less of a Eurocentric standard of beauty. 
#The area that she had the least amount of matches was Vancouver 
#and Los Angeles, California. Uganda had the most amount of messages and Tokyo had the least amount of messages.
#The closer to the equator Emma is the more messages she received. 
#_________________________________________________________________________________________________________________________________

getwd()

library(readr)
research_jane <- read_csv("research_jane.csv")
View(research_jane)

names(research_jane)<-c("", "City", "Continient","PredomLang", "Matches","Inapp","Messages","Superlikes","TypeofText","AvgChar", "lat","long")
research_jane<-research_jane[,-13]
research_jane$messrate<-NA
research_jane$messrate<-(research_jane$Messages/research_jane$Matches)

research_jane<-research_jane[-c(10:998),]

jane_US<-research_jane[-c(8:10),]


pal<-colorQuantile("RdYlBu",domain = NULL, n=5)

leaflet(data=jane_US) %>%
  addTiles() %>%
  addCircles(lng=~long,
             lat=~lat,
             popup=~paste(City,"<br>Matches:",Matches,
                          "<br>Messages:", Messages),
             radius=~Matches*10000,
             color= ~pal(messrate),
             fillOpacity = 0.8) %>%
  addLegend("bottomright", colors=brewer.pal(5,"RdYlBu"),
            labels = c("low", "","","","high"),
            title = "Relative Messages per Match")

#the data from this visualization was collected from Jane. 
#The rate of messages is an interesting variable because could mean how many people are willing to seek an actual 
#relationship or willing to engage with the researcher. 
#The area with the most amount of matches is from Mexico City. Mexico City also has the most amount of messages, showing that people were looking for meaningful connections.
#While the amount of matches is fairly consistent across the US, the amount of messages varies much more. 
#Atlanta has very low message rate while Mexico City and Austin have higher message rates. 
#The West and Southwest areas of the US have a higher amount of messages.
#-----------------------------------------------------------------------------------------

#graph 4

jane_US$City[jane_US$City == "Mamaroneck, New York"]<-"New York, New York"

emma_US <- research_emma[-c(8:19),]


emma_US$person<-"Emma"
jane_US$person<-"Jane"
jane_US<-jane_US[,-1]
setdiff(emma_US$City, jane_US$City)

e_j<-rbind(emma_US, jane_US)
View(e_j)

e_j$Inapp[e_j$Inapp==1]<-'yes'
e_j$Inapp[e_j$Inapp==0]<-'no'

ggplot(e_j)+
  stat_summary(aes(x =City, y=messrate, fill = Inapp), geom="bar", fun = "mean")+
  facet_grid(~person)+
  theme(axis.text = element_text(angle = 45, hjust=1))+
  scale_fill_manual("Inappropriate Message?",
                    values = c("maroon","blue"))+
  ylab("Messages")+
  ggtitle("Comparison of Inappropriate Messages in Each City for Emma and Jane")

#The cities in the visualization are only focused on the data points observed in North America.
#Both Emma and Jane only received Inappropriate messages from one city. 
#It appears that the amount of messages a research receives has not played into how many
#Inappropriate messages they received.
#The messages were from opposite sides of the country so there doesn't seem to be a correlation with location either. 

#---------------------------------------------------------------

#graph 5

e_j$TypeofText[e_j$TypeofText=='greeting ']<-"greeting"

ggplot(data = e_j)+
  geom_point(aes(x=City, y=Matches,size = messrate, color = TypeofText))+
  scale_color_brewer("Type of Text",
                     palette = "Accent")+
  theme(axis.text = element_text(angle = 45, hjust=1))+
  ggtitle("Matches per City based on Messages")+
  facet_grid(~person)+
  labs(size = "Message Rates")

#Each of these data visualizations are only looking at the data observed in North America. 
#Jane received more compliments than greetings from her swipes.
#This is may indicate a more consistent alignment of the standard of beauty around the US.
#Emma received mostly greeting texts in the US. 
#Emma had more variance in her number of swipes across the US indicating a less consistent alignment with the standard of beauty across the US
#There wasn't much variance in the message rate for each of the cities in North America
#____________________________________________________________________________________________________________________

#Conclusion

#If given the opportunity to do this project again, there would be a count of each type of text from each city. 
#There would also be many more cities observed. 
#I would focus on the a specific area,like the US and get as many data points as possible from the one place. 
#I would wait more than 24 hours for someone to respond because there were sometimes people who matched after the 24 hours.
#Each researcher would compare their matches and observed data from matching with both girls and boys.
#There would also be a focus on age. 
#I would try to include race in each city and have a breakdown on which people tended to swipe on each researcher based on their race.
#There is the possibility that quarantine affected the results of the experiments so I would conduct this again when people are no longer social distancing.  
#Overall, there would be more variables and cities to give more insight on dating culture around the world.
