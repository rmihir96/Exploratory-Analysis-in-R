#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(gridExtra)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  heatmap_data <- read.csv("D:/Study Material/DIC/part2/StateDataforMap_2018-19week6.csv")
  heatmap_data$region<-tolower(heatmap_data$STATENAME)
  US_heatmap_data <- subset(heatmap_data, select = c(region, ACTIVITY.LEVEL.LABEL))
  states <- map_data("state")
  #unique(US_heatmap_data)
  US_plot_data <- merge(states, US_heatmap_data, by="region")
  
  all_tweets <- read.csv("D:/Study Material/DIC/part3/all_tweets.csv")
  df_flu <- read.csv("D:/Study Material/DIC/part3/flu.csv")
  df_hash_flu <-read.csv("D:/Study Material/DIC/part3/hash_flu.csv")
   
  plot1 <- reactive({
    # this should be a complete plot image,
    # e.g. ggplot(data, aes(x=x, y=y)) + geom_line()
    ggplot(US_plot_data, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=ACTIVITY.LEVEL.LABEL))+
      scale_fill_manual(values=c("red","orange","yellow","green"),
                        name = "Activity Level",
                        breaks= c("High","Moderate","Low", "Minimal"),
                        label=c("High","Moderate","Low", "Minimal"))+
      geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019")
  })
  
  plot2 <- reactive({
    # this should be a complete plot image,
    # e.g. ggplot(data, aes(x=x, y=y)) + geom_line()
    ggplot(all_tweets, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019(Using Twitter Data)")
    
    
  })
  
  plot3 <- reactive({
    # this should be a complete plot image,
    # e.g. ggplot(data, aes(x=x, y=y)) + geom_line()
    ggplot(df_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("Tweets with keyword flu")
    
  })
  
  plot4 <- reactive({
    # this should be a complete plot image,
    # e.g. ggplot(data, aes(x=x, y=y)) + geom_line()
    ggplot(df_hash_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 5, 20 , 50), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("Tweets with keyword #flu")
    
  })
  plot5<- reactive(({
   gg1<-ggplot(US_plot_data, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=ACTIVITY.LEVEL.LABEL))+
      scale_fill_manual(values=c("red","orange","yellow","green"),
                        name = "Activity Level",
                        breaks= c("High","Moderate","Low", "Minimal"),
                        label=c("High","Moderate","Low", "Minimal"))+
      geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019")
    
  gg2<-ggplot(all_tweets, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019(Using Twitter Data)") 
    
  grid.arrange(gg1,gg2, ncol=2, top = "CDC vs Twitter")
  }))
  
  plot6<- reactive({
    x1<-ggplot(df_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 40, 150 , 200), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("Tweets with keyword flu")
    
    x2<-ggplot(df_hash_flu, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=level))+
      scale_fill_gradientn(colours=c("green","yellow","orange","orangered","tomato","red","firebrick"),
                           label=c("minimal","low","moderate","High"),breaks=c(0, 5, 20 , 50), na.value="grey50")+
      geom_path()+coord_map()+ggtitle("Tweets with keyword #flu")
    
    grid.arrange(x1,x2, ncol=2, top = "keyword:flu vs keyword: #flu")
  })
  # Return the requested graph
  graphInput <- reactive({
    switch(input$graph,
           "US Heat Map" = plot1(), 
           "All tweets" = plot2(),
           "Keyword:Flu" = plot3(),  
           "Keyword:#flu" = plot4(),
           "CDC vs Tweets" = plot5(),
           "flu vs #flu" = plot6()
    )
  })
  
  output$selected_graph <- renderPlot({ 
    graphInput()
  })
  
})


#Reference : https://stackoverflow.com/questions/48312392/shiny-allow-users-to-choose-which-plot-outputs-to-display