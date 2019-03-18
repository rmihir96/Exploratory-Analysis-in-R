#install.packages("rtweet")
library(ggmap)
library(ggplot2)
library(rtweet)
library(maps)
library(stringr)

file.remove(".httr-oauth")
rm(list = ls())
library(rtweet)
create_token(
  "",
  "",
  "",
  "") #Enter your API keys here

############################################################
### Code to mine tweets
#############################################################

temp <- c()
rt <- search_tweets("flu", n=18000, since= "2019-01-01")
temp <- rbind(temp,rt)  
Sys.sleep(60*15)

geocoded_tweets <- lat_lng(temp)
dim(unique(geocoded_tweets))

plot_tweets <-unique(geocoded_tweets) 

typeof(plot_tweets)
tweets_data <- as.data.frame(plot_tweets)

typeof(tweets_data)

tweets_data <- apply(tweets_data,2,as.character)
write.csv(tweets_data, "tweets.csv")

##############################################################
### Load tweets from csv
################################################################


database <- read.csv("D:/Study Material/DIC/part3/tweets.csv")
dim(database)
#Removing retweets
database <- database[database$is_retweet == 'FALSE',]
dim(database)
#Removing duplicate tweets
new_database <- database[!duplicated(database$status_id), ]
dim(new_database)
#getting latitude and longitude for all the rows
location_database<-lat_lng(new_database)
dim(location_database)

#Since all the tweets dont have a geocode location, we get geocordinates for these tweets.
library(ggmap)
register_google(key = "") #Your google key

geo<-geocode(as.character(location_database$location[1:10000]), output = "latlona")
dim(geo)

statenames = c()
for (i in geo$address) {
  statenames <- c(statenames, str_extract(i,"\\b[a-z]{1,4}\\b"))
}

length(statenames)
write.csv(geo, "geo.csv")

statenames<- as.list.data.frame(statenames)
length(statenames)
US_state_abv <- c()
US_state_abv <- datasets::state.abb
head(US_state_abv)

US_state <- c()
US_state <- datasets::state.name
head(US_state)
temp_loc <-c()
temp_loc$abv <- (as.list.data.frame(US_state_abv))
temp_loc$us_states <- (as.list.data.frame(US_state))
temp_loc<- as.data.frame(temp_loc)

head(temp_loc)

temp_location_database <- location_database[1:10000,]
temp_location_database$abv <- as.list.data.frame(toupper(statenames))
temp_location_database<- subset(temp_location_database, select =c(user_id, status_id, abv))

temp_US_states <- merge(temp_location_database, temp_loc, by = "abv")

final_data <- subset(temp_US_states, select = c(user_id, status_id, us_states))
final_data$region <- tolower(final_data$us_states)

states <- map_data("state")
final_data$level <- 1

state_data<-aggregate(level~region,FUN=length,data=final_data)
all_tweets <- merge(states, state_data, by="region", all.x = T)
all_tweets <- all_tweets[order(all_tweets$order),]


#Lets plot the heat map 
library(ggplot2)
x11()
ggplot(all_tweets, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
  scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
  label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
  geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019(Using Twitter Data)")

  


##################################################################################
### Tweets for keyword = "flu"
##################################################################################

flu_tweets <- location_database[grep("flu", location_database$text),]

temp_flu_tweets <- flu_tweets[1:10000,]
temp_flu_tweets$abv <- as.list.data.frame(toupper(statenames))
flu_tweets<- subset(temp_flu_tweets, select =c(user_id, status_id, abv))

temp_US_states <- merge(flu_tweets, temp_loc, by = "abv")

flu_tweets_data <- subset(temp_US_states, select = c(user_id, status_id, us_states))
flu_tweets_data$region <- tolower(flu_tweets_data$us_states)
flu_tweets_data$statename <- NULL


states <- map_data("state")
flu_tweets_data$level <- 1

state_data<-aggregate(level~region,FUN=length,data=flu_tweets_data)
df_flu <- merge(states, state_data, by="region", all.x = T)
df_flu <- df_flu[order(df_flu$order),]
#Lets plot the heat map 
library(ggplot2)
x11()
ggplot(df_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
  scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                       label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
  geom_path()+coord_map()+ggtitle("Tweets with keyword flu")



##################################################################################
### Tweets for keyword = "hashtag flu"
##################################################################################
hash_flu_tweets <- location_database[grep("#flu", location_database$text),]

temp_flu_tweets <- hash_flu_tweets
temp_flu_tweets$abv <- as.list.data.frame(toupper(statenames)[1:1100])
hash_flu_tweets<- subset(temp_flu_tweets, select =c(user_id, status_id, abv))

temp_US_states <- merge(hash_flu_tweets, temp_loc, by = "abv")

hash_flu_tweets_data <- subset(temp_US_states, select = c(user_id, status_id, us_states))
hash_flu_tweets_data$region <- tolower(hash_flu_tweets_data$us_states[1:293])
hash_flu_tweets_data$statename<-NULL


states <- map_data("state")
hash_flu_tweets_data$level <- 1

state_data<-aggregate(level~region,FUN=length,data=hash_flu_tweets_data)
df_hash_flu <- merge(states, state_data, by="region", all.x = T)
df_hash_flu <- df_hash_flu[order(df_hash_flu$order),]


#Lets plot the heat map 
library(ggplot2)
x11()
ggplot(df_hash_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
  scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                       label=c("minimal","low","moderate","High"),breaks=c(0, 5, 20 , 50), na.value="grey50")+
  geom_path()+coord_map()+ggtitle("Tweets with keyword #flu")

write.csv(all_tweets,"all_tweets.csv")
write.csv(df_flu,"flu.csv")
write.csv(df_hash_flu, "hash_flu.csv")

#save(all_tweets, df_flu, df_hash_flu, file="output.RData")
