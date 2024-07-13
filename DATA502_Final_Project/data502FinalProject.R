#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(tidycensus)
library(colorspace)
library(scales)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(ggplot2)



df_usa = read_csv("final_ufo_data.csv")
df_usa = df_usa %>% filter(!is.na(shape) & !is.na(day_part))
df_usa$state[df_usa$state == "Fl"] <- "FL"

#df_usa = df %>% mutate(part_of_day = case_when(reported_24_hour >= 5 & reported_24_hour <= 17 ~ "day", 
#                                               TRUE ~ "night"))
states = unique(df_usa$state) %>% str_sort()
stateCounts = df_usa %>% group_by(state) %>% summarize(sightings = n()) %>% arrange(desc(sightings))
statePerCapita = df_usa %>% group_by(state) %>% summarize(pop = sum(population), 
                                                          sightings = n(), 
                                                          perCapita = sightings / pop) %>% arrange(desc(perCapita))

#states = c("None", states)

generate_word_cloud <- function(clusterSightingsOnDate) {
  text <- clusterSightingsOnDate$summary
  # Create a corpus  
  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  
  # set.seed(1234) # for reproducibility 
  #  wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
  #            max.words=200, random.order=FALSE, rot.per=0.35,            
  #            colors=brewer.pal(8, "Dark2"))
  
  # wordcloud2(data=df, size=1.6, color='random-dark')
  wordcloud2(data=df, size = 0.7, shape = 'pentagon')
}

get_summary_stats <- function(sightingsByState) {
  numRows = sightingsByState %>% summarize(rowCount = n())
  numNight = sightingsByState %>% filter(day_part == "night" | day_part == "astronomial dusk" | day_part == "civil dusk" | day_part == "nautical dusk") %>% summarize(nightCount = n())
  numShape = sightingsByState %>% filter(shape == "light" | shape == "fireball") %>% summarize(nightCount = n())
  
  #percents
  ratioNumNight = numNight[,1]/numRows[,1]
  ratioNumShape = numShape[,1]/numRows[,1]
  percentNight = label_percent()(ratioNumNight[,1])
  percentLightShape = label_percent()(ratioNumShape[,1])
  
  #rank
  rankState = which(stateCounts$state == sightingsByState$state[1])
  rankStateOrdinal = label_ordinal()(rankState)
  
  rankPerCapita = which(statePerCapita$state == sightingsByState$state[1])
  rankPerCapitaOrdinal = label_ordinal()(rankPerCapita)
  
  #most common shapes
  commonShapes = sightingsByState %>% group_by(shape) %>% summarize(shapeNumbers = n()) %>% arrange(desc(shapeNumbers)) %>% head(3)
  
  list(percentNight = percentNight, rankStateOrdinal = rankStateOrdinal, rankPerCapitaOrdinal = rankPerCapitaOrdinal, commonShapes = commonShapes)
}

get_daytime_sightings <- function(sightingsByState) {
  fullCurve = sightingsByState %>%
    count(reported_24_hour)%>%
    mutate(freq_hour = n/sum(n),
           cum_prob_hour = cumsum(freq_hour))
  minDiff = which.min(abs(diff(fullCurve$cum_prob_hour)))
  minTime = minDiff-2
  maxTime = minDiff+4
  fullCurveDiffs = abs(diff(fullCurve$cum_prob_hour))
  dayPercent =(fullCurve[maxTime,]$cum_prob_hour - fullCurve[minTime,]$cum_prob_hour)
  dayCurve = fullCurve %>% slice(minTime:maxTime)
  minTime12hour = str_remove(format(strptime(minTime, '%H'), '%I %p'), "^0+")
  maxTime12hour = str_remove(format(strptime(maxTime, '%H'), '%I %p'), "^0+")
  sightingsByStateAndDaylight = sightingsByState %>% filter(reported_24_hour >= minTime & reported_24_hour <= maxTime)
  commonShapes = sightingsByStateAndDaylight %>% group_by(shape) %>% summarize(shapeNumbers = n()) %>% arrange(desc(shapeNumbers)) %>% head(3)
  numAnomalyRows = sightingsByStateAndDaylight %>% summarize(rowCount = n())
  return(list(sightingsByStateAndDaylight = sightingsByStateAndDaylight,
              minTime12hour = minTime12hour,
              maxTime12hour = maxTime12hour,
              dayPercent = dayPercent,
              dayCurve = dayCurve,
              fullCurve = fullCurve,
              numAnomalyRows = numAnomalyRows,
              commonShapes = commonShapes))
}


ufoIcon <- makeIcon(
  iconUrl = "ufo.png",
  iconWidth = 38, iconHeight = 65,
  iconAnchorX = 22, iconAnchorY = 64)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: #2F327D;
        color: white;
      }
      rcorners1 {
        border-radius: 25px;
        background: #73AD21;
        padding: 20px;
        width: 200px;
        height: 150px;
      } 
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #2F327D;
      }"))
  ),
  
  # Application title
  titlePanel("NUFORC Anomoly Detector"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("state", "Choose a state:", choices = states),
      selectInput("anomaly", "Look For Anomalies:", choices = c("-", "Cluster Sightings", "Daytime Sightings")),
      conditionalPanel("input.anomaly == 'Cluster Sightings'", textInput("date", "Date:"))
    ),
    
    # Show plots in main panel 
    mainPanel(
      fluidRow(
        column(12, leafletOutput("leafletPlot", width = '100%', height = 400),
               fluidRow(
                 column(12, conditionalPanel("input.anomaly == '-'", uiOutput("unfilteredStats"))),
               ),
     
               
               fluidRow(
                 column(6, conditionalPanel("input.anomaly == 'Daytime Sightings'", plotOutput(outputId="daytimePlot",height = 300,width = '100%'), style='padding:10px')),
                 column(6, conditionalPanel("input.anomaly == 'Daytime Sightings'", uiOutput("daytimeStats", height = 200, width = '100%'), style='padding:10px')),
                 column(7, conditionalPanel("input.anomaly == 'Cluster Sightings'", plotOutput(outputId="clusterPlot",height = 500,width = '100%'), style='padding:10px')),
                 column(5, conditionalPanel("input.anomaly == 'Cluster Sightings'", plotOutput(outputId="wordcloud", height = 400,width ='100%'), style='padding:10px')),
                 #   column(4, plotOutput(outputId="shapePlot", height = 400,width = '100%'), style='padding:10px')
               )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Filter dataframe to feed to graphs and leaflet
  rvSightingsByState = reactive({ df_usa %>% filter(state == input$state) })
  
  #Prepare data for most rare sightings during day
  rvSightingsByStateAndDaylight = reactive({
    get_daytime_sightings(rvSightingsByState())
  })
  
  #Get clusters of viewings for the state
  rvClusterSightingsByState = reactive({
    sightingsByState = rvSightingsByState()
    if (input$date != "") {
      sightingsByState = sightingsByState %>% filter(reported_date == input$date)
    }
    
    clusterSightingsByState = sightingsByState %>%
      group_by(reported_date) %>% add_count(name="numSightings") %>% filter(numSightings >3)
    
    clusterSummary = clusterSightingsByState %>% group_by(reported_date) %>% summarize(numSightings = n())
    
    list(clusterSightingsByState = clusterSightingsByState, clusterSummary = clusterSummary)
  })
  
  rvGetStateSummary = reactive({get_summary_stats(rvSightingsByState())})
  
  rvCreateWordCloud = reactive({
    
    text <- rvClusterSightingsByState()$clusterSightingsByState$summary
    # Create a corpus  
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myDTM = TermDocumentMatrix(myCorpus)
    #m = as.matrix(myDTM) ##this can cause an memory error if your corpus is even moderately sized.
    #row_sums from the slam package is much more efficient as it is designed to operate on sparse matrices and TDM/DTMs from the tm package are sparse matrices which are constructed using slam
    v = sort(slam::row_sums(myDTM),decreasing = TRUE)
    data.frame(word=names(v),freq=v) #Since you've already calculated frequency here there is no need for the freq reactive. We can just access the freq from this data.frame
  })
  
  #remove duplicate rows across entire data frame 
  # df %>%
  #    distinct(.keep_all = TRUE)
  
  
  output$unfilteredStats <- renderUI({ 
    percentNight = rvGetStateSummary()$percentNight
    rankStateOrdinal = rvGetStateSummary()$rankStateOrdinal
    rankPerCapitaOrdinal = rvGetStateSummary()$rankPerCapitaOrdinal
    commonShapes = commonShapes = rvGetStateSummary()$commonShapes
    
    HTML(paste0(
      h1(state.name[which(state.abb == input$state)]),
      h3("Ranks", rankStateOrdinal, "in total sightings."),
      h3("Ranks", rankPerCapitaOrdinal, "in total sightings per capita."),
      h3(percentNight, "of sightings are at night."),
      '<h3>The most common shapes are', commonShapes[1,]$shape,', ', commonShapes[2,]$shape,', ', commonShapes[3,]$shape,'.',
      sep=""))
  })
  
  output$daytimeStats <- renderUI({ 
    minTime12hour = rvSightingsByStateAndDaylight()$minTime12hour
    maxTime12hour = rvSightingsByStateAndDaylight()$maxTime12hour
    numRows = rvSightingsByStateAndDaylight()$numAnomalyRows
    commonShapes = rvSightingsByStateAndDaylight()$commonShapes
    
    HTML(paste0(
      h1(state.name[which(state.abb == input$state)]),
      h3(numRows, "anomalous sightings found between", minTime12hour, "and", maxTime12hour,"."),
      '<h3>The most common shapes during these daytime hours are ', commonShapes[1,]$shape,', ', commonShapes[2,]$shape,', ', commonShapes[3,]$shape,".",
      sep=""))
  })
  
  output$leafletPlot <- renderLeaflet({
    
    sightingsByState = rvSightingsByState()
    
    if (input$anomaly == "Cluster Sightings") {
      sightingsByState = rvClusterSightingsByState()$clusterSightingsByState
    } else if (input$anomaly == "Daytime Sightings") {
      sightingsByState = rvSightingsByStateAndDaylight()$sightingsByStateAndDaylight
    }
    
    
    countyData = get_acs(geography = "county", year=2021,
                         state = input$state,
                         variables = "B25077_001E", 
                         geometry = TRUE)
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data=countyData,
                  fillOpacity = .7,
                  weight =.5,
                  smoothFactor = 0.2) %>%
      addMarkers(data=sightingsByState,
                 lng=sightingsByState$longitude, lat=sightingsByState$latitude,
                 clusterOptions = markerClusterOptions(),
                 icon = ufoIcon,
                 popup = paste0("<b>City: ", sightingsByState$city, "</b><hr>",
                                "Date of Sighting: ", sightingsByState$reported_date, "<br>",
                                "Shape: ", sightingsByState$shape, "<br>",
                                "Summary: ", sightingsByState$summary, "<br>",
                                "Duration in seconds: ", sightingsByState$duration_seconds, "<br>",
                                "Time of Day:", sightingsByState$day_part))
  })
  
  output$clusterPlot <- renderPlot ({
    
   test <- rvClusterSightingsByState()$clusterSummary %>% arrange(desc(numSightings)) %>% head(20) %>%
      ggplot() +
      geom_col(aes(numSightings, reorder(reported_date, -numSightings, sum), fill=numSightings)) +
      scale_fill_continuous_sequential("GnBu") +
      labs(title = paste("Top Clusters of Sightings found in", input$state),
           x = "Number of Sightings in a Cluster",
           y ="",
           caption="NUFORC UFO Data (https://nuforc.org/)") + 
      theme_classic() +
      scale_x_continuous(expand = c(0, 0)) +
      theme(legend.position="none")
   
   print(test)
   ggplotly(test)
  })
  
  output$daytimePlot <- renderPlot ({
    minTime12hour = rvSightingsByStateAndDaylight()$minTime12hour
    maxTime12hour = rvSightingsByStateAndDaylight()$maxTime12hour
    dayPercent = rvSightingsByStateAndDaylight()$dayPercent
    dayCurve = rvSightingsByStateAndDaylight()$dayCurve
    fullCurve = rvSightingsByStateAndDaylight()$fullCurve
    
    ggplot()+
      geom_line(data=fullCurve, aes(reported_24_hour, cum_prob_hour),alpha = .5)+
      geom_area(data=dayCurve, aes(reported_24_hour, cum_prob_hour),alpha = .5, fill="#8CDBC2") +
      geom_point(data=fullCurve, aes(reported_24_hour, cum_prob_hour)) +
      scale_x_continuous(breaks=c(0, 5, 10, 15, 20), labels= c("12AM", "5AM", "10AM", "3PM", "8PM")) +
      labs(title=paste('Only',label_percent()(dayPercent),'of sightings occur between the hours of', minTime12hour, 'and', maxTime12hour),
           y="Probability of a Sighting",
           x="Hour of the Day",
           caption="NUFORC UFO Data (https://nuforc.org/)") +
      theme_minimal()
  })
  
  
  output$wordcloud <- renderPlot({
    wordcloud(words = rvCreateWordCloud()$word,
              freq = rvCreateWordCloud()$freq, 
              max.words=200, min.freq = 1,
              random.order=FALSE, rot.per=0.35,   
              scale=c(4,.5), 
              colors=brewer.pal(8,"Dark2"))
  })
  
  
  
  # output$timeOfDayPlot <- renderPlot ({
  #   
  #   sightingsByState %>% filter(!is.na(day_part)) %>%
  #   group_by(day_part) %>% summarize(count=n()) %>%
  #   
  #   ggplot() +
  #   geom_col(aes(y=fct_reorder(day_part, count), x=count, fill=day_part)) +
  #   ggtitle(paste("When to view UFOs in", state.name[match(input$state,state.abb)])) +
  #   labs(y ="") +
  #     theme(legend.position = "none")
  # })
  # 
  # output$durationPlot <- renderPlot({
  #   sightingsByState %>% filter(duration_seconds < 3600) %>%
  #     ggplot() +
  #     geom_density(aes(x=duration_seconds), fill="purple", alpha=.3, color="black") +
  #     ggtitle(paste("How long UFOs stick around in", state.name[match(input$state,state.abb)])) +
  #     labs(y ="") +
  #     theme(legend.position = "none")
  # })
  # 
  # output$shapePlot <- renderPlot({
  #     # generate bins based on input$bins from ui.R
  #   sightingsByState %>% filter(!is.na(shape)) %>%
  #       group_by(shape) %>% summarize(count=n()) %>% arrange(desc(count)) %>%
  #       slice_head(n=10) %>%
  #     ggplot() + geom_col(aes(y=fct_reorder(shape, count), x=count, fill=shape)) +
  #       ggtitle(paste("10 most common shapes in", state.name[match(input$state,state.abb)])) +
  #     labs(y ="") +
  #     theme(legend.position = "none")
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)