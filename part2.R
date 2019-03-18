CL_data <-read.csv("/WHO_NREVSS_Clinical_Labs.csv") 
sapply(CL_data, class)
total_data <- CL_data[CL_data$YEAR>2017,]
row.names(total_data) <- NULL
task1_data <- total_data[c(40:57),]
task1_data <- subset(task1_data, select = c(WEEK,YEAR, TOTAL.SPECIMENS, TOTAL.A, TOTAL.B, PERCENT.POSITIVE, PERCENT.A, PERCENT.B))

task1_bars <- subset(task1_data, select = c(TOTAL.A, TOTAL.B, WEEK))
axis_year <- paste(task1_data$YEAR,task1_data$WEEK,sep="")
task1_bars$xlabel <- as.list.data.frame(axis_year)
length(task1_bars$xlabel)
#<----Plot 1 ------>
x11()
par(mar = c(5,6,4,5) + 0.1)
barplot(task1_data$TOTAL.A, ylim=c(0,8000),col = "yellow", xlab="Week", ylab="Number of Positive Specimen", xaxt="n",yaxt="n",cex.axis = 0.5, main = "Influenza Positive Tests Reported to CDC by 
U.S. Clinical Laboratories
        2018-2019 Season")

bar<-barplot(task1_data$TOTAL.B, col = "green", add=TRUE, yaxt="n", xaxt="n",cex.axis = 0.5)
axis(2)
#xticks <-c('201840', '201841', '201842', '201843', '201844', '201845', 
 #          '201846', '201847', '201848', '201849', '2018450', '201851', '201852',
  #         '201901', '201902', '201903', '201904', '201905', '201906' )
#axis(1,labels = FALSE)

axis(side=1, at =bar, labels=task1_bars$xlabel, las=2)
par(new=T)
plot(task1_data$PERCENT.POSITIVE, axes = FALSE, type = "l",xlab = NA, ylab = NA, yaxt="n")
axis(side=4, at=seq(0, 35,5))
axis(4)
mtext("Percent Positive", side = 4, line=3)
par(new=T)
plot(task1_data$PERCENT.A, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "yellow")
par(new=T)
plot(task1_data$PERCENT.B, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "green", ylim = c(0,10000))
legend("topleft", inset=0.01, c("A","B", "Percent Positive", "%A", "%B"), fill=c("Yellow","Green", "Black", "Yellow", "Green"),  lty = c(1,1,1,2,3,3))

#<-----END----->

#<----Plot 2---->
PL_data <-read.csv("/WHO_NREVSS_Public_Health_Labs.csv") 

PHL_data <- PL_data[PL_data$YEAR>2017,]
row.names(PHL_data) <- NULL
PHL_data <- PHL_data[c(40:57),]


v<-rbind(PHL_data$BYam, PHL_data$BVic, PHL_data$B, PHL_data$H3N2v, PHL_data$A..H3.,
         PHL_data$A..2009.H1N1., PHL_data$A..Subtyping.not.Performed.)
bar<-barplot(v,beside=FALSE,
        col = c("Light Green","Green", "Dark Green", "Purple", "Red", "Orange", "Yellow"),xlab = "week" ,ylab = "Number of Positive Specimen", main = "Influenza Positive Tests Reported to CDC by
U.S. Public Health Laboratories 
        2018-2019 Season" )
#axis(side=2, pos=-0.2)
axis(side=1, at =bar, labels=task1_bars$xlabel, las=2)
legend("topleft", inset = 0.01, 
       c("BYam","BVic", "B", "H3N2v", "A..H3. A.", "A..2009.H1N1.", "A(Subtyping not Performed)"), 
       fill = c("Light Green","Green", "Dark Green", "Purple", "Red", "Orange", "Yellow"))
#Reference https://stackoverflow.com/questions/7583432/plot-stacked-bar-plot-in-r
 #<----END----->

#<---- plot 3 ------->

piedata <- read.csv("/Genetic06_pie.csv")

par(mar = c(5,6,4,5) + 0.1)
b_vict_slices <- piedata[1:3,"Distinct.count.of.Cdc.Id.."];
b_vict_lbls_1 <- piedata[1:3,"Sequence.Genetic.Group"];
b_vict_lbls_2 <- piedata[1:3,"Distinct.count.of.Cdc.Id.."];
b_vict_lbls_3 <- piedata[1:3,"X..of.Total.Distinct.count.of.Cdc.Id.."];
b_vict_lbls <- paste(b_vict_lbls_1,b_vict_lbls_2,b_vict_lbls_3,sep=" | ")
pie(b_vict_slices, labels = b_vict_lbls, main="Influenza B Victoria");

b_yamagata_slices <- piedata[4:4,"Distinct.count.of.Cdc.Id.."];
b_yamagata_lbls_1 <- piedata[4:4, "Sequence.Genetic.Group"];
b_yamagata_lbls_2 <- piedata[4:4, "Distinct.count.of.Cdc.Id.."];
b_yamagata_lbls_3 <- piedata[4:4, "X..of.Total.Distinct.count.of.Cdc.Id.."];
b_yamagata_lbls <- paste(b_yamagata_lbls_1,b_yamagata_lbls_2,b_yamagata_lbls_3,sep=" | ")
pie(b_yamagata_slices, labels = b_yamagata_lbls, main="Influenza B Yamagata");

a_h1_slices <- piedata[5:5,"Distinct.count.of.Cdc.Id.."];
a_h1_lbls_1 <- piedata[5:5, "Sequence.Genetic.Group"];
a_h1_lbls_2 <- piedata[5:5, "Distinct.count.of.Cdc.Id.."];
a_h1_lbls_3 <- piedata[5:5, "X..of.Total.Distinct.count.of.Cdc.Id.."];
a_h1_lbls <- paste(a_h1_lbls_1,a_h1_lbls_2,a_h1_lbls_3 ,sep=" | ")
pie(a_h1_slices, labels = a_h1_lbls, main="Influenza A(H1N1)pdm09");

a_h3_slices <- piedata[6:8,"Distinct.count.of.Cdc.Id.."];
a_h3_lbls_1 <- piedata[6:8, "Sequence.Genetic.Group"];
a_h3_lbls_2 <- piedata[6:8, "Distinct.count.of.Cdc.Id.."];
a_h3_lbls_3 <- piedata[6:8, "X..of.Total.Distinct.count.of.Cdc.Id.."];
a_h3_lbls <- paste(a_h3_lbls_1,a_h3_lbls_2,a_h3_lbls_3 ,sep=" | ")
pie(a_h3_slices, labels = a_h3_lbls, main="Influenza A(H3N2)");



#
#<---Task 5 part 1--->
weeks_52_data <- total_data[c(6:57),]

axis_year5 <- paste(weeks_52_data$YEAR,weeks_52_data$WEEK,sep="")

weeks_52_data <- subset(weeks_52_data, select = c(WEEK, TOTAL.SPECIMENS, TOTAL.A, TOTAL.B, PERCENT.POSITIVE, PERCENT.A, PERCENT.B))
lines_data <- weeks_52_data
lines_data
weeks_52_data <- subset(weeks_52_data, select = c(TOTAL.A, TOTAL.B, WEEK))
weeks_52_data$x_labels <- as.list.data.frame(axis_year5)
length(weeks_52_data$x_labels)
barplot(weeks_52_data$TOTAL.A,ylim = c(0,20000),col = "yellow", xlab="Week", ylab="Number of Positive Specimen", xaxt="n",yaxt="n",main = "Influenza Positive Tests Reported to CDC by 
U.S. Clinical Laboratories
        For 52 Weeks")


bar5 <- barplot(weeks_52_data$TOTAL.B, col = "green", add=TRUE, yaxt="n", xaxt="n",cex.axis = 0.5)
lines(x = bar5, y=lines_data$PERCENT.POSITIVE*400, lwd=2)
lines(x = bar5, y=lines_data$PERCENT.A*400, col="yellow",  lwd=2, lty=2)
lines(x = bar5, y=lines_data$PERCENT.B*400, col="green",  lwd=2, lty=3)
axis(2)
#xticks <-c('201840', '201841', '201842', '201843', '201844', '201845', 
#          '201846', '201847', '201848', '201849', '2018450', '201851', '201852',
#         '201901', '201902', '201903', '201904', '201905', '201906' )
axis(side=1, at =bar5, labels=weeks_52_data$x_labels, las=2,cex.axis = 0.5)
par(new=T)
plot(weeks_52_data$PERCENT.POSITIVE, axes = FALSE, type = "l",xlab = NA, ylab = NA, yaxt="n", xlim = c(0,60), ylim = c(0, 15000), lwd=10)
axis(side=4)
box()
par(new=T)
plot(weeks_52_data$PERCENT.A*400, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "yellow", xlim = c(0,60), ylim = c(0,15000))
par(new=T)
plot(weeks_52_data$PERCENT.B*400, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "green", ylim = c(0,15000), xlim = c(0,60))
legend("topright", inset=0.01, c("A","B", "Percent Positive", "%A", "%B"), fill=c("Yellow","Green", "Black", "Yellow", "Green"),  lty = c(1,1,1,2,3,3))

#<----Task 5 Part 2--->
PL_52_data <-read.csv("/WHO_NREVSS_Public_Health_Labs.csv") 

PHL_52_data <- PL_52_data[PL_data$YEAR>2017,]
PHL_52_data
row.names(PHL_52_data) <- NULL
PHL_52_data <- PHL_52_data[c(6:57),]
PHL_52_data

v_52<-rbind(PHL_52_data$BYam, PHL_52_data$BVic, PHL_52_data$B, PHL_52_data$H3N2v, PHL_52_data$A..H3.,
         PHL_52_data$A..2009.H1N1., PHL_52_data$A..Subtyping.not.Performed.)
bar52 <- barplot(v_52,beside=FALSE,
        col = c("Light Green","Green", "Dark Green", "Purple", "Red", "Orange", "Yellow"),xlab = "week" ,ylab = "Number of Positive Specimen", main = "Influenza Positive Tests Reported to CDC by
U.S. Public Health Laboratories 
       For 52 Weeks")
axis(side=1, at =bar52, labels=weeks_52_data$x_labels, las=2,cex.axis = 0.5)

legend("topright", inset = 0.01, 
       c("BYam","BVic", "B", "H3N2v", "A..H3. A.", "A..2009.H1N1.", "A(Subtyping not Performed)"), 
       fill = c("Light Green","Green", "Dark Green", "Purple", "Red", "Orange", "Yellow"))

#< --- Task 6 (New York State) --->
weeks_52_data <- read.csv("/WHO_NREVSS_Clinical_Labs_NY.csv")
axis_year5 <- paste(weeks_52_data$YEAR,weeks_52_data$WEEK,sep="")

weeks_52_data <- subset(weeks_52_data, select = c(WEEK, TOTAL.SPECIMENS, TOTAL.A, TOTAL.B, PERCENT.POSITIVE, PERCENT.A, PERCENT.B))
lines_data <- weeks_52_data
weeks_52_data <- subset(weeks_52_data, select = c(TOTAL.A, TOTAL.B, WEEK))
weeks_52_data$x_labels <- as.list.data.frame(axis_year5)
length(weeks_52_data$x_labels)
weeks_52_data$x_labels
barplot(weeks_52_data$TOTAL.A,ylim = c(0,20000),col = "yellow", xlab="Week", ylab="Number of Positive Specimen", xaxt="n",yaxt="n",main = "Influenza Positive Tests Reported to CDC by 
        U.S. Clinical Laboratories
        For 52 Weeks in the New York State ")
bar5 <- barplot(weeks_52_data$TOTAL.B, col = "green", add=TRUE, yaxt="n", xaxt="n",cex.axis = 0.5)
lines(x = bar5, y=lines_data$PERCENT.POSITIVE*400, lwd=2)
lines(x = bar5, y=lines_data$PERCENT.A*400, col="yellow",  lwd=2, lty=2)
lines(x = bar5, y=lines_data$PERCENT.B*400, col="green",  lwd=2, lty=3)
axis(2)
#xticks <-c('201840', '201841', '201842', '201843', '201844', '201845', 
#          '201846', '201847', '201848', '201849', '2018450', '201851', '201852',
#         '201901', '201902', '201903', '201904', '201905', '201906' )
axis(side=1, at =bar5, labels=weeks_52_data$x_labels, las=2,cex.axis = 0.5)
par(new=T)
plot(weeks_52_data$PERCENT.POSITIVE, axes = FALSE, type = "l",xlab = NA, ylab = NA, yaxt="n", xlim = c(0,60), ylim = c(0, 15000), lwd=10)
axis(side=4)
box()
par(new=T)
plot(weeks_52_data$PERCENT.A*400, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "yellow", xlim = c(0,60), ylim = c(0,15000))
par(new=T)
plot(weeks_52_data$PERCENT.B*400, axes = FALSE, type = "b",xlab = NA, ylab = NA, col = "green", ylim = c(0,15000), xlim = c(0,60))
legend("topright", inset=0.01, c("A","B", "Percent Positive", "%A", "%B"), fill=c("Yellow","Green", "Black", "Yellow", "Green"),  lty = c(1,1,1,2,3,3))


#----US Heat Map----
library(ggplot2)
heatmap_data <- read.csv("/StateDataforMap_2018-19week6.csv")
#statenames <-unique(heatmap_data$STATENAME)
heatmap_data$region<-tolower(heatmap_data$STATENAME)
US_heatmap_data <- subset(heatmap_data, select = c(region, ACTIVITY.LEVEL.LABEL))
states <- map_data("state")
#unique(US_heatmap_data)
US_plot_data <- merge(states, US_heatmap_data, by="region")

typeof(US_plot_data$group)
qplot(long, lat, data=US_plot_data, geom="polygon", fill=ACTIVITY.LEVEL.LABEL, group=group)
x11()
ggplot(US_plot_data, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=ACTIVITY.LEVEL.LABEL))+
  scale_fill_manual(values=c("red","orange","yellow","green"),
                    name = "Activity Level",
                    breaks= c("High","Moderate","Low", "Minimal"),
                    label=c("High","Moderate","Low", "Minimal"))+
geom_path()+coord_map()+ggtitle("2018-2019 Season Week 4 ending Feb 28,2019")

#<----plot 5 pediatric deaths--->
pediatric_data = read.csv("/weekly.csv")
x11()
barplot(pediatric_data$NO..OF.DEATHS,col = "dark green", ylim = c(0,30), xlim = c(0,210), xaxt = "n", yaxt='n',xlab = "Week of Death" ,ylab = "Number of Deaths", main= "Number of Influenza-Associated Pediatric 
        Deaths: 2015-2016 to present")
barplot(pediatric_data$CURRENT.WEEK.DEATHS,add=TRUE,col = "light Blue", yaxt="n", xaxt="n",cex.axis = 0.5)

legend("topright", inset=0.01, c("Deaths Reported Previous Week","Deaths Reported Current Week"), fill=c("Green", "light Blue"),  lty = c(1,1))
axis(2)
axis(1, at=pediatric_data$WEEK.NUMBER, labels = pediatric_data$WEEK.NUMBER)

text(20, 13, "2015-16
     Number of Deaths
     Reported = 94",cex=0.8)
text(90, 15, "2016-17
     Number of Deaths
     Reported = 110",cex=0.8)
text(140, 21, "2017-18
     Number of Deaths
     Reported = 185",cex=0.8)
text(190, 13, "2018-19
     Number of Deaths
     Reported = 56",cex=0.8)
range(pediatric_data$WEEK.NUMBER)
nrow(pediatric_data)

?map_data
