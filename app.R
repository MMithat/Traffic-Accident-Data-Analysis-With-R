library(shiny)
library(stringr)
library(tidyverse)
library(shinythemes)
library(maps)
library(sp)
library("ggmap")
library(mapproj)
library(leaflet)



data<-read.csv("2009-9.csv", header = T)

##We created new columns and organized some columns for analysis.
data$Easting%<>%as.character()%>%
  str_pad(3, side="left", pad="0")%>%
  str_replace("(\\w{2})", "\\1\\.") %>%
  as.numeric() 

data$Northing%<>%as.character()%>%
  str_pad(3, side="left", pad="0")%>%
  str_replace("(\\w{2})", "\\1\\.") %>%
  as.numeric()

for (i in (1:nrow(data))) {
  a<-word(data$Accident.Date[i],2,2,sep = "-")
  data$Mounth[i]<-a
}

for (i in (1:nrow(data))) {
  if (data$Mounth[i]=="Jan" | data$Mounth[i]=="Feb" | data$Mounth[i]=="Dec"){
    data$season[i]<-"Winter"
  } else if (data$Mounth[i]=="Mar" | data$Mounth[i]=="Apr" | data$Mounth[i]=="May"){
    data$season[i]<-"Spring"
  } else if (data$Mounth[i]=="Jun" | data$Mounth[i]=="Jul" | data$Mounth[i]=="Aug"){
    data$season[i]<-"Summer"
  } else {
    data$season[i]<-"Fall"
  }
}

datas1<-as.data.frame(table(data$Reference.Number))
news<-data.frame()
for (i in (1:nrow(datas1))){
  news<-rbind(news,data[data$Reference.Number==datas1[i,1],])
}


news2<-news%>%select(c(Reference.Number, Easting, Northing, Number.of.Vehicles, 
                       Accident.Date, Time..24hr., Road.Surface, 
                       Lighting.Conditions, Weather.Conditions))


news3<-news2[!duplicated(news2),]
news3$Number.of.casualty<-datas1[,2]

for (i in (1:nrow(data))){
  data$Number.of.Csualty[i]<-news3$Number.of.casualty[news3$Reference.Number==data$Reference.Number[i]]
}


##This is ui for our application.
ui <- navbarPage("ATENCA", ##We create navbar.
                
  mainPanel(
    shinythemes::themeSelector(),## This' purpose is selecting theme in the application  
    tabsetPanel( 
      tabPanel("Info", ##Creating tab panel called "Info"
               h1(strong("ATENCA")),
               br(),
               p("Many people injured in traffic accidents die because they can not get treated early. The main reason is insufficient ambulances for casualties are called to the scene. Therefore, determining the number of injuries in a traffic accident and how many ambulances should be sent according to this number on time; is vital in traffic accidents. Suppose that we determine the number of injured casualties earlier. In that case, we can determine the number of ambulances earlier; thus, we can call sufficient ambulances in time to provide early intervention to the wounded. Following this, ATENCA was created to estimate the number of casualties sustained in an accident. ATENCA can calculate this number using the dataset from 2009. You can access this dataset and information about the dataset from the data bar."),
               p("ATENCA estimates the number with its linear model for four variables. These variables are; the number of vehicles, weather conditions, lighting conditions, and road class. "),
               br(),
               br(),
               h3(strong("How does ATENCA work?")),
               br(),
               p("Go to the prediction bar and enter four pieces of information about the accident. The result will come in both with and without decimal. The section under 'Approximately' shows how many ambulances should be called."),
               br(),
               h3(strong("How is ATENCA used?")),
               br(),
               p("Go to the prediction bar and enter 4 information about the accident. The result will come in both with and without decimal. The section under 'Approximately' shows how many ambulances will be called.")
               
        
      ),
      tabPanel("Prediction", sidebarLayout( 
        sidebarPanel( ##Creating side bar in the right of the page in the application
          textInput("Number.of.Vehicles", "Number of Vehicles:"), 
          selectInput("X1st.Road.Class", "Road Class:",
                      choices = c("A",
                                  "A(M)",
                                  "B",
                                  "Motorway",
                                  "Unclassified")),
          selectInput("Weather.Conditions", "Weather Conditions:",
                      choices = c("Fine with high winds",
                                  "Fine without high winds",
                                  "Other",
                                  "Raining with high winds",
                                  "Raining without high winds",
                                  "Snowing with high winds",
                                  "Snowing without high winds",
                                  "Unknown")),
          selectInput("Lighting.Conditions", "Lighting Conditions:",
                      choices = c("Darkness: no street lighting",
                                  "Darkness: street lighting unknown",
                                  "Darkness: street lights present and lit",
                                  "Darkness: street lights present but unlit",
                                  "Daylight: street lights present"))),
        mainPanel(h3(strong("The expected number of casualty in the accident is:")), 
                  br(),
                  h3(textOutput("text")), br(),
                  br(),
                  h3("Approximately"),
                  br(),
                  h3(textOutput("approx")),
                  br(),
                  h3("For this traffic accident, the number of ambulances needed :"),
                  br(),
                  h3(textOutput("text06")),)
      )),
      navbarMenu("Statistics", ##Creating some tabs in statistics bar.
                  
                 tabPanel("Tab 1",
                          plotOutput("barPlot2"),
                          h3(strong("According to the bar chart of 'The Number of Casualty by Mounths in 2009':")),
                          p("The highest number of casualty in traffic accidents is in July and it is 296 people. 
                 The minimum number of casualty is in February and 196 people."),
                          p("713 people were casualty in traffic accidents in winter, 769 in spring, 
                 801 in summer and 774 in autumn. According to these numbers, while the most casualty were in summer, the minimum casualty were in winter."),
                          p("According to the seasons, the average of casualty in a month was 237.66 in winter, 256.33 in spring, 267 in summer, and 258 in autumn."),
                          br(),
                          br(),
                          plotOutput("linechart"),
                          selectInput("Mounth2", "Select a mounth to see average number
                           \n of casualty in an accident in that mounth:",
                                      choices = c("Jan",
                                                  "Feb",
                                                  "Mar",
                                                  "Apr",
                                                  "May",
                                                  "Jun",
                                                  "Jul",
                                                  "Aug",
                                                  "Sep",
                                                  "Oct",
                                                  "Nov",
                                                  "Dec")),
                          textOutput("Mounth2"),
                          h3(strong("According to the line graph of 'Average Number of Casualty
                         in an Accident by Mounth' :")),
                          p("In the line graph, months werre represented by numbers (1:January, 2:February,..., 11:November, 12:December)." ),
                          p("Although the maximum average number of casualty in an accident was
                 in April with 1.52, the minimum average number of casualty in an accident
                 was June with 1.28."),
                          p("According to the seasons, the average of casualty in a an accident was 1.40 in winter, 1.48 in spring, 1.36 in summer, and 1.36 in autumn."),
                          br(),
                          br(),
                          plotOutput("lines"),
                          br(),
                          br(),
                          p("The graphs ware created by using a dataset from https://data.gov.uk. .You can reach this dataset from 'Data' button above.")),
                 tabPanel("Tab 2", sidebarLayout(
                   sidebarPanel(
                     selectInput("Mounth", "Mounth:",
                                 choices=c("Jan",
                                           "Feb",
                                           "Mar",
                                           "Apr",
                                           "May",
                                           "Jun",
                                           "Jul",
                                           "Aug",
                                           "Sep",
                                           "Oct",
                                           "Nov",
                                           "Dec")),
                     selectInput("Sv", "Casualty Class:", 
                                 choices = c( "Pedestrian",
                                              "Driver",
                                              "Passenger")),
                     sliderInput("Bins", "The number of bins in the histogram:",
                                 min = 1, max=100, value=10)),
                   mainPanel(plotOutput("barPlot"),
                             br(),
                             plotOutput("histogram"),
                             br(),
                             p("The bar chart and histogram were created by using a dataset from https://data.gov.uk. 
                           You can reach this dataset from 'Data' button above.")))),
                 tabPanel("Tab 3", sidebarLayout(
                   sidebarPanel(selectInput("select6", "Casualty Class:",
                                            choices = c("Driver",
                                                        "Passenger",
                                                        "Pedestrian")),
                                selectInput("select7", "Type of Vehicle:",
                                            choices = c("Bus or coach (17 or more passenger seats)",
                                                        "Car",
                                                        "Goods vehicle 3.5 tonnes mgw and under",
                                                        "Goods vehicle 7.5 tonnes mgw and over",
                                                        "Goods vehicle over 3.5 tonnes and under 7.5 tonnes mgw",
                                                        "Motorcycle over 500cc",
                                                        "Motorcycle over 50cc and up to 125cc" ,
                                                        "Other Vehicle ",
                                                        "Pedal cycle",
                                                        "Taxi/Private hire car"))),
                   mainPanel(plotOutput("pie"),
                             textOutput("Fatif"),
                             textOutput("Fatif2"),
                             textOutput("Fatif3"),
                             textOutput(("Fatif4")),
                             br(),
                             br(),
                             p("The pie chart was created by using a dataset from https://data.gov.uk. 
                           You can reach this dataset from 'Data' button above.")
                          )
                 ))),
      
      tabPanel("McTatus", sidebarLayout(
        sidebarPanel(selectInput("select", h3("Mounths"), 
                                 choices = c("Jan",
                                             "Feb",
                                             "Mar",
                                             "Apr",
                                             "May",
                                             "Jun",
                                             "Jul",
                                             "Aug",
                                             "Sep",
                                             "Oct",
                                             "Nov",
                                             "Dec"))),
        mainPanel(leafletOutput("mymap"),
                  br(),
                  p("The mapping was created by using a dataset from https://data.gov.uk. 
                           You can reach this dataset from 'Data' button above.")))),
      navbarMenu("Data",
                 tabPanel("Dataset",
                          tableOutput("dataset")),
                 tabPanel("Dataset Info",
                          h3(strong("Dataset Information")),
                          br(),
                          p("Information on accidents across Leeds. Data includes location, number of people and vehicles involved, road surface, weather conditions and severity of any casualties. Please note that The Eastings and Northings are generated at the roadside where the accident occurred."),
                          p("Due to the format of the report a number of figures in the columns are repeated, these are: * Reference Number * Easting * Northing * Number of Vehicles * Accident Date * Time (24hr) * 1st Road Class * Road Surface * Lighting Conditions * Weather Conditions * Casualty Class * Casualty Severity * Sex of Casualty * Age of Casualty *    Type of Vehicle."),
                          p("Source: https://data.gov.uk/road-traffic-accidents"),
                          p("Last updated at https://ckan.publishing.service.gov.uk/dataset : 2019-12-21"),
                          p("Reference Number : Reference number in the police report"),
                          p("Easting  and Northing gives us the coordinates of the place that accident occurred."),
                          p("Number of Vehicles: Number of vehicles that involved in accident"),
                          p("Accident Date: Represents the date of the accident as year, month and day, respectively."),
                          p("Time (24hr) : The hour of the accident"),
                          p("1st Road Class : The road hierarchy categorizes roads according to their functions and capacities"),
                          p("Road Surface : The road surface variable specifies the amount of wetness of the surface and keywords such as ice, wet, dry are used for this."),
                          p("Lighting Conditions : The characteristics of the road's light, dark, and lighting system where the vehicles involved in the accident were at the time of the accident."),
                          p("Weather Conditions : Weather conditions at the time of the accident "),
                          p("Casualty Class : It states whether the individuals involved in the accident were pedestrians, drivers or passengers."),
                          p("Casualty Severity : The classification of the severity of an accident (as fatal, serious or slight)"),
                          p("Sex of Casualty : Gender of the people that were injured in the accident"),
                          p("Age of Casualty : Age of the person that injured in the accident "),
                          p("Type of Vehicle.: Vehicle type means vehicles which do not differ in such essential respects as the structure, dimensions, shape and materials in areas to which the mechanical coupling device or component is affixed.(such as; car, bus etc.)"))),
      tabPanel("Contributors and References",
               h2(strong("References")),
               br(),
               h5(p("data.gov.uk. (2020, January 12). Road traffic accidents - dataset by datagov-UK. data.world. Retrieved June 15, 2022, from https://data.world/datagov-uk/6efe5505-941f-45bf-b576-4c1e09b579a1/workspace/file?filename=2009-9.csv")),
               br(),
               br(),
               h2(strong(p("Contributors :"))),
               br(),
               h5(p("Arif Yigit APAYDIN (2428878)")),
               h5(p("Sayen ALTINDISLI (2361087)")),
               h5(p("Ismail Fatih BOZ (2428928)")),
               h5(p("Mustafa Mithat Can EVCI (2429033)")),
               h5(p("Zehra Eylul YILDIZ (2429413)")))
      )
      
    ))



##This is server of our application.
server <- function(input, output){
  output$barPlot<- renderPlot({##creating bar graph in tab2 in statistics. 
    Mounth<-input$Mounth
    Class<-input$Sv
    data2<-data.frame()
    for (i in (1:nrow(data))){
      a<-word(data$Accident.Date[i],2,2,sep = "-")
      if (a==Mounth){
        data2<-rbind(data2, data[i,])
      }
    }
    data3<-subset(data2, Casualty.Class==Class)
    data4<-table(data3$Sex.of.Casualty, data3$Casualty.Severity)
    library(RColorBrewer)
    coul <- brewer.pal(2, "Set2")
    barplot(data4, main=paste("The Number of Female and Male casualty as", Class,
                              "\nby Casual Severity in", Mounth,", 2009"),
            xlab="Casual Severity", ylab="The number of female and male", 
            col=c(coul[1],coul[2]),
            legend = rownames(data4), beside=T,
            legend.text = T,
            args.legend = list(x = "topleft"))
    
  })
  output$histogram<-renderPlot({##Creating histogrom in tab2 in statistics.
    Mounth<-input$Mounth
    Bins<-input$Bins
    datas2<-data[data$Mounth==Mounth,]
    hist(datas2$Age.of.Casualty, breaks=Bins, col="skyblue3", xlab = "Age",
         main=paste("Histogram of Age of Casualty", Mounth, ", 2009"))
    
  })
  output$barPlot2<-renderPlot({##Creating bar graph in tab1 in statistics.
    Jan<-0
    Feb<-0
    Mar<-0
    Apr<-0
    May<-0
    Jun<-0
    Jul<-0
    Aug<-0
    Sep<-0
    Oct<-0
    Nov<-0
    Dec<-0
    for (i in data$Accident.Date) {
      a<-word(i,2,2,sep = "-")
      if (a=="Jan"){
        Jan<-Jan+1
      } else if (a=="Feb"){
        Feb<-Feb+1
      } else if (a=="Mar"){
        Mar<-Mar+1
      } else if (a=="Apr"){
        Apr<-Apr+1
      } else if (a=="May"){
        May<-May+1
      } else if (a=="Aug"){
        Aug<-Aug+1
      } else if (a=="Jun"){
        Jun<-Jun+1
      } else if (a=="Jul"){
        Jul<-Jul+1
      } else if (a=="Sep"){
        Sep<-Sep+1
      } else if (a=="Oct"){
        Oct<-Oct+1
      } else if (a=="Nov"){
        Nov<-Nov+1
      } else if (a=="Dec"){
        Dec<-Dec+1
      }
    }
    
    
    b<-c(Jan, Feb, Mar, Apr, May, Jun,
         Jul, Aug, Sep, Oct, Nov, Dec)
    n<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    library(RColorBrewer)
    coul <- brewer.pal(12, "Set2")
    my_bar<-barplot(b, names.arg = n, ylim = c(0,370), col = coul, xlab="Mounths",
                    ylab="The number of Casualty", main="The Number of Casualty by Mounths in 2009")
    text(my_bar, b+50 , b ,cex=1, col = coul)
  })
  output$linechart<-renderPlot({##Creating first line graph in tab1 in statistics.
    average<-c()
    n<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    for (i in (1:nrow(news3))) {
      a<-word(news3$Accident.Date[i],2,2,sep = "-")
      news3$Mounth[i]<-a
    }
    for (i in (1:12)){
      z<-news3[news3$Mounth==n[i],]
      average[i]<-mean(z$Number.of.casualty)
    }
    plot(x=(1:12), y=average, main="Average Number of Casualty \nin an Accident by Mounth in 2009",
         ylab = "Average Number of Casualty",
         xlab = "Mounths", type = "b", pch=20, lty=2, col="red", cex=2, ylim = c(1,1.6))
    
  })
  output$text<-renderText({##This for output text in prediction tab.
    
    mlodel3<-lm(data=data, Number.of.Csualty ~  Number.of.Vehicles +
                  X1st.Road.Class + Weather.Conditions+ Lighting.Conditions)
    N<-as.numeric(input$Number.of.Vehicles)
    X<-input$X1st.Road.Class
    W<-input$Weather.Conditions
    L<-input$Lighting.Conditions
    new_obs<-data.frame(Number.of.Vehicles=N,
                        X1st.Road.Class = X,  Weather.Conditions= W, 
                        Lighting.Conditions= L)
    predict(mlodel3, newdata=new_obs)
    
    
  })
  output$dataset<-renderTable({ ##This is for adding our dataset to data.
    a<-read.csv("2009-9.csv", header = T)
    a
  })
  output$mymap <- renderLeaflet({##Creating map for mapping in McTatus.
    
    
    b <- input$select
    
    data8<-data.frame()
    for (i in (1:nrow(data))){
      a<-word(data$Accident.Date[i],2,2,sep = "-")
      if (a == b){                              
        data8<-rbind(data8, data[i,])
      }
    }
    
    
    
    
    pal <- colorFactor(c(" red", " orange","light green"), 
                       domain = c("Fatal", "Serious","Slight"))
    
    leaflet(data8) %>% 
      addProviderTiles("OpenStreetMap") %>% 
      addLegend("bottomright", opacity = 0.6, pal = pal, values = data8$Casualty.Severity) %>% 
      addCircleMarkers(lng = ~Northing , lat = ~Easting, weight = 2,
                       color = ~pal(Casualty.Severity), fill = T, fillOpacity = 1,
                       popup = paste("Date:", data8$Accident.Date, "<br>",
                                     "Surface:", data8$Road.Surface, "<br>",
                                     "Severity:", data8$Casualty.Severity))
  })
  output$Mounth2<-renderText({##Create a value for text below the first line graph in tab1 in statistics.  
    k<-input$Mounth2
    average<-c()
    n<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    for (i in (1:nrow(news3))) {
      a<-word(news3$Accident.Date[i],2,2,sep = "-")
      news3$Mounth[i]<-a
    }
    for (i in (1:12)){
      z<-news3[news3$Mounth==n[i],]
      average[i]<-mean(z$Number.of.casualty)
    }
    j<-data.frame(n,average)
    paste("Average Number of Casualty in an Accident in", k, 
          "in 2009 :",round(j$average[j$n==k],2))
    
  })
  output$approx<-renderText({##Creating an approximate value for the prediction. This is in the line4 in main page.
    mlodel3<-lm(data=data, Number.of.Csualty ~  Number.of.Vehicles +
                  X1st.Road.Class + Weather.Conditions+ Lighting.Conditions)
    N<-as.numeric(input$Number.of.Vehicles)
    X<-input$X1st.Road.Class
    W<-input$Weather.Conditions
    L<-input$Lighting.Conditions
    new_obs<-data.frame(Number.of.Vehicles=N,
                        X1st.Road.Class = X,  Weather.Conditions= W, 
                        Lighting.Conditions= L)
    round(predict(mlodel3, newdata=new_obs), digits = 0)
    
  })
  output$pie<-renderPlot({##Creating pie chart in the tab3 in statistics.
    Mou<-input$select6
    Typ<-input$select7
    x <- data[data$Casualty.Class == Mou & data$Type.of.Vehicle == Typ,]
    X <- select(x,Age.of.Casualty)
    
    for(i in (1:nrow(X))){
      if (X[i,1]<=17){
        X$groupage[i] <- "Children"
      } else if (18<=X[i,1] & X[i,1]<=33){
        X$groupage[i] <- "Young Adults"
      } else if (34<=X[i,1] & X[i,1]<=47){
        X$groupage[i] <- "Middle-Aged Adults"
      } else if (48<=X[i,1] & X[i,1]<=64){
        X$groupage[i] <- "Old-Adults"
      } else{
        X$groupage[i] <- "Older"
      }
    }
    t <-table(X$groupage)
    
    slices <- t
    lbls <- c("Children","Middle-Aged Adults","Old-Adults","Older","Young Adults")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct)
    lbls <- paste(lbls,"%",sep="")
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
        main=paste("The Percentage of Age Groups of Casualty as", Mou))
    legend("bottomright", c("Children(0-17)","Middle-Aged Adults(34-47)","Old-Adults(48-64)","Older(64+)","Young Adults(18-33)"),
           cex = 0.5, fill = rainbow(length(lbls)))
  })
  output$Fatif<-renderText({##Creating a value for minimum in tab3 in statistics
    Mou<-input$select6
    Typ<-input$select7
    x <- data[data$Casualty.Class == Mou & data$Type.of.Vehicle == Typ,]
    X <- select(x,Age.of.Casualty)
    
    for(i in (1:nrow(X))){
      if (X[i,1]<=17){
        X$groupage[i] <- "Children"
      } else if (18<=X[i,1] & X[i,1]<=33){
        X$groupage[i] <- "Young Adults"
      } else if (34<=X[i,1] & X[i,1]<=47){
        X$groupage[i] <- "Middle-Aged Adults"
      } else if (48<=X[i,1] & X[i,1]<=64){
        X$groupage[i] <- "Old-Adults"
      } else{
        X$groupage[i] <- "Older"
      }
    }
    paste("Minimum Age Casualty :", min(X$Age.of.Casualty))
  })
  output$Fatif2<-renderText({##Creating a maximum value for maximum in tab3 in statistics.
    Mou<-input$select6
    Typ<-input$select7
    x <- data[data$Casualty.Class == Mou & data$Type.of.Vehicle == Typ,]
    X <- select(x,Age.of.Casualty)
    
    for(i in (1:nrow(X))){
      if (X[i,1]<=17){
        X$groupage[i] <- "Children"
      } else if (18<=X[i,1] & X[i,1]<=33){
        X$groupage[i] <- "Young Adults"
      } else if (34<=X[i,1] & X[i,1]<=47){
        X$groupage[i] <- "Middle-Aged Adults"
      } else if (48<=X[i,1] & X[i,1]<=64){
        X$groupage[i] <- "Old-Adults"
      } else{
        X$groupage[i] <- "Older"
      }
    }
    paste("Maximum Age Casualty :", max(X$Age.of.Casualty))
  })
  output$Fatif3<-renderText({##Creating a mean value for mean in tab3 in statistics.
    Mou<-input$select6
    Typ<-input$select7
    x <- data[data$Casualty.Class == Mou & data$Type.of.Vehicle == Typ,]
    X <- select(x,Age.of.Casualty)
    
    for(i in (1:nrow(X))){
      if (X[i,1]<=17){
        X$groupage[i] <- "Children"
      } else if (18<=X[i,1] & X[i,1]<=33){
        X$groupage[i] <- "Young Adults"
      } else if (34<=X[i,1] & X[i,1]<=47){
        X$groupage[i] <- "Middle-Aged Adults"
      } else if (48<=X[i,1] & X[i,1]<=64){
        X$groupage[i] <- "Old-Adults"
      } else{
        X$groupage[i] <- "Older"
      }
    }
    paste("Mean Age Casualty:", round(mean(X$Age.of.Casualty),digit=2))
  })
  output$Fatif4<-renderText({##Creating median value for median in tab3 in statistics.
    Mou<-input$select6
    Typ<-input$select7
    x <- data[data$Casualty.Class == Mou & data$Type.of.Vehicle == Typ,]
    X <- select(x,Age.of.Casualty)
    
    for(i in (1:nrow(X))){
      if (X[i,1]<=17){
        X$groupage[i] <- "Children"
      } else if (18<=X[i,1] & X[i,1]<=33){
        X$groupage[i] <- "Young Adults"
      } else if (34<=X[i,1] & X[i,1]<=47){
        X$groupage[i] <- "Middle-Aged Adults"
      } else if (48<=X[i,1] & X[i,1]<=64){
        X$groupage[i] <- "Old-Adults"
      } else{
        X$groupage[i] <- "Older"
      }
    }
    paste("Median Age Casualty:", median(X$Age.of.Casualty))
  })
  output$lines<-renderPlot({ ##Creating the second line graph in tab1 in statistics.
    m<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
    mins<-c()
    maxs<-c()
    for (i in(1:12)){
      mins[i]<-min(data[data$Mounth==m[i],]$Number.of.Csualty)
      maxs[i]<-max(data[data$Mounth==m[i],]$Number.of.Csualty)
      
    }
    
    plot(x=c(1:12), y=maxs, ylim = c(0,10), pch=16, col="purple", type = "b", 
         xlab="Mounths", ylab="The number of casualty", main="The Maximum and Minimum Number of Casualty in an Accident by Mounth in 2009")
    lines(x=c(1:12), y=mins, pch = 16, col = "green", type = "b", lty = 1)
    legend("topleft", legend=c("The maximum number", "The minimum number"),
           col=c("purple", "green"), lty = 1, cex=1)
  })
  output$text06<-renderText({##Creating a value for sixth line in main page in prediction.
    mlodel3<-lm(data=data, Number.of.Csualty ~  Number.of.Vehicles +
                  X1st.Road.Class + Weather.Conditions+ Lighting.Conditions)
    N<-as.numeric(input$Number.of.Vehicles)
    X<-input$X1st.Road.Class
    W<-input$Weather.Conditions
    L<-input$Lighting.Conditions
    new_obs<-data.frame(Number.of.Vehicles=N,
                        X1st.Road.Class = X,  Weather.Conditions= W, 
                        Lighting.Conditions= L)
    round(predict(mlodel3, newdata=new_obs), digits = 0)
  })
  

}

##This is shinnApp for our application.
shinyApp(ui, server)


