#----> This is an All In One Presentation and Interactive Customer Data Analysis Application-----------


# Badass function that checks to see if the package is installed, installs it or loads the library...Totally Awesome
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


#---------Using the usePackage Function instead of library-----------------------------

usePackage("shiny")
usePackage("DT")
usePackage("data.table")
#usePackage("arules")
#usePackage("arulesViz")
usePackage("crosstalk")
usePackage("caret")
usePackage("ggplot2")
usePackage("dplyr")
usePackage("tidyr")
usePackage("Hmisc")
usePackage("pastecs")
usePackage("psych")
usePackage("mice")
usePackage("VIM")
#usePackage(remove.na)
#usePackage(lubridate)
#usePackage(data.table)
#usePackage(tibble)
usePackage("rlang")
usePackage("gridExtra")
usePackage("lubridate")
usePackage("chron")
usePackage("zoo")
usePackage("forecast")
usePackage("reshape")
usePackage("prophet")
usePackage("xts")
usePackage("grid")
usePackage("ggfortify")

#--------------------------Time to get this party started!!!!!!   WOOOOOOOOOOO
# autoplot
#   _____            _         __          ___                   _       __  __         _   _            ___  
#  |  __ \          | |        \ \        / / |                 ( )     |  \/  |       | \ | |   /\     |__ \ 
#  | |  | |_   _  __| | ___     \ \  /\  / /| |__   ___ _ __ ___|/ ___  | \  / |_   _  |  \| |  /  \       ) |
#  | |  | | | | |/ _` |/ _ \     \ \/  \/ / | '_ \ / _ \ '__/ _ \ / __| | |\/| | | | | | . ` | / /\ \     / / 
#  | |__| | |_| | (_| |  __/_     \  /\  /  | | | |  __/ | |  __/ \__ \ | |  | | |_| | | |\  |/ ____ \   |_|  
#  |_____/ \__,_|\__,_|\___( )     \/  \/   |_| |_|\___|_|  \___| |___/ |_|  |_|\__, | |_| \_/_/    \_\  (_)  
#                          |/                                                    __/ |                        
#                                                                                |___/                         
#     

# ---------------> Define some critical variables first....These are Default Values---------------------------------
dateStart = as.POSIXct("2006-12-16 17:24:00","%Y-%m-%d %H:%M:%S", tz = "") #start day and time
dateEnd = as.POSIXct("2010-11-26 21:02:00","%Y-%m-%d %H:%M:%S",  tz = "") #end day and time
supp=0.1
conf=0.5
vars=50



########------------> Use the following if you want to browse for a file (set as default)

#import = read.csv(file.choose(), sep=";")

#######-------------> Use the following if you want to lock in a file (disabled by default)

#----> Testing Dataset--------
import = read.csv("https://drive.google.com/uc?id=1a1yWld2tiv6Aw0lE3nhH_aRWM54r75AN", sep=";")




####-------->  Change the stupid column names to something actually readable-------------------------------

setnames(import, old=c("Global_active_power", "Global_reactive_power", "Global_intensity","Sub_metering_1","Sub_metering_2", "Sub_metering_3"), new=c("Active", "Reactive", "Amps", "Kitchen", "Laundry", "HVAC"))

#--------->  Convert Date ---------------------------------------------------------------------------------#######

import$Date <- as.Date(import$Date, "%d/%m/%Y", tz = "")

######------> Creating one date+time column called DateTime############################################################

import <-cbind(import,paste(import$Date,import$Time), stringsAsFactors=FALSE)
colnames(import)[10] <-"DateTime"
import <- import[,c(ncol(import), 1:(ncol(import)-1))]

######------> Converting date and Time format #########################################################################

import$DateTime <- strptime(import$DateTime, "%Y-%m-%d %H:%M:%S", tz ="") # Converts the string to a Date/Time Object
import$DateTime =  as.POSIXct(import$DateTime, tz = "") #Does some Voodoo shit to fix the DST problem

#--------> Convert 3,4,5,6,7,8,9 to numeric (And Normalize Some of them)------- They are not Factors  ##############################################

import$Active = as.numeric(import$Active)
import$Reactive = as.numeric(import$Reactive)
import$Voltage = as.numeric(import$Voltage)
import$Amps = as.numeric(import$Amps)
import$Kitchen = as.numeric(import$Kitchen)
import$Kitchen = import$Kitchen/1000
import$Laundry = as.numeric(import$Laundry)
import$Laundry = import$Laundry/1000
import$HVAC = as.numeric(import$HVAC)
import$HVAC = import$HVAC/1000

#-------> Remove NA's ---------------------------

cleaned = na.omit(import) #removed all of the NA's

#cleaned = rm.outlier(cleaned$Active, fill = FALSE, median = FALSE, opposite = FALSE)

cleaned$day = weekdays(as.Date(cleaned$DateTime))

startDS = paste("Start of Data: ",as.character(cleaned$Date[1]))
endDS = paste("End of Data: ", as.character(cleaned$Date[as.numeric(length(cleaned$Date))]))

# Define first 1/2 of Shiny app....The User Interface!!! Awwwwww Yeah!

ui <- fluidPage(theme = "bootstrap.css",
  
#---------> Creating a NavBar for the top of the page-------------------------------------------------------------------------------------------------
  
  navbarPage(
    
#---------> Insert the Company Logo at the top left....Like a Boss!    -------------------------------------------------------------------------------------------------   
    
    img(src='http://res.cloudinary.com/x78163/image/upload/v1510921813/IOT_p5bgjo.png', align = "left", width="150", height="30"),

#---------> Creating a Home Tabs for the top of the page---------------------------------------------------------------------------
             tabPanel("Home" , 
                      h1("Ubiqum Data Science Consultants", align = "center"),
                      HTML('<center><img src="http://cultofthepartyparrot.com/assets/sirocco.gif" style ="width="300", height="300"></center>'),
                      #HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1512060481/partyparrot_lcjgj2.gif" style ="width="300", height="300"></center>'),
                    #  HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907256/DS_logo_rmmtbo.png" style ="width="300", height="300"></center>'),
                      h3("Time to make your data party like a parrot!!!!!", align = "center"),
                      HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907481/headshot_foglex.png" style ="width="100", height="100"></center>')
                      
             ),

#---------> Creating a Presentation  Tabs for the top of the page-------------------------------------------------------------------------------------------------  

             tabPanel("Presentation", 
                     
#---------> Code to Insert a Powerpoint Presentation-----------------------------------------------------------------------------------------------------------------------
                      
                       tags$iframe(style="height:50vw; width:90vw; scrolling=no", 
                                  src="https://onedrive.live.com/embed?cid=D091F528EDB75B0A&resid=D091F528EDB75B0A%2111092&authkey=AJlOeVwrPeQJKDc&em=2")),
            # <iframe src="https://onedrive.live.com/embed?cid=D091F528EDB75B0A&resid=D091F528EDB75B0A%2111092&authkey=AJlOeVwrPeQJKDc&em=2" width="402" height="327" frameborder="0" scrolling="no"></iframe>

#---------> Creating an Analysis  Tabs for the top of the page---------------------------------------------------------------------------------------------------------------------
          
             tabPanel("Our Analysis",
                      
                      sidebarPanel(
                        
                      #  img(src='http://res.cloudinary.com/x78163/image/upload/v1510825161/DS_logo_rsoctl.png', align = "center", width="250", height="250"),
                        tags$br(),
                        
                        conditionalPanel(
                          condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
                          # radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
                          #uiOutput("choose_columns"), br(),
                          
                          HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510908400/Calendar_f5yruq.png" style ="width="150", height="100"></center>'),
                          tags$br(),
                          startDS,
                          tags$br(),
                          endDS,
                          tags$br(),
                          dateRangeInput("daterange", "Date range:",
                                         start = "2007-01-01",
                                         end   = "2007-01-31"),
                          
                          # sliderInput("supp", "Support:", min = "2007-12-05 01:00:00", max = "2007-11-05 01:00:00", value = c(dateStart,dateEnd) ), br(),
                          tags$br(),
                         
                          HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510908508/Clock_l3ktu9.png" style ="width="75", height="100"></center>'),
                          tags$br(),
                          tags$br(),
                          sliderInput("time", "Time (0000-2359):",
                                      min = 0, max = 2359, step = 100,
                                      value = c(200,500)), 
                          #sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/100), br(),
                          tags$br(),
                          HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510908579/sunmoon_ybnl63.png" style ="width="100", height="100"></center>'),
                          
                          tags$br(),
                          tags$br(),
                          selectInput('light', label='Filter by Day/Night', choices = c('All Day','Day', 'Night')), br(), br()
                      #       selectInput('week', label='Filter by Weekend/Weekday', choices = c('All','Weekend', 'Weekday')), br(), br(),
                          # numericInput("minL", "Min. items per set:", 2), br(), 
                          # numericInput("maxL", "Max. items per set::", 3), br(),
                          # radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
                          # radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
                        #  downloadButton('downloadPlot', 'Download Charts')
                        )
                        
                      ),
                      
                      
                      mainPanel(
                        
                        
                        
                        tabsetPanel(
                          tabPanel("Dataset", DT::dataTableOutput("rules")),
                          
                          tabPanel("Combined Plot", plotOutput("combined"),downloadButton('downloadCombined', 'Download Charts')),
                          tabPanel("Histograms", plotOutput("hist"),downloadButton('downloadHist', 'Download Charts')),
                          navbarMenu("Active/Reactive Plots",
                                     
                                     tabPanel("Overview", plotOutput("activeoverview", click = "plot1_click", brush = brushOpts(id = "plot1_brush"))),
                                     tabPanel("Costs", plotOutput("activecosts"),downloadButton('downloadActiveCosts', 'Download Charts'))),
                          navbarMenu("Submeter Plots",
                                     tabPanel("Overview", plotOutput("suboverview"),downloadButton('downloadSubOverview', 'Download Charts')),
                                     tabPanel("Costs", plotOutput("subcost"),downloadButton('downloadSubCost', 'Download Charts'))),
                          tabPanel("Dashboard", textOutput("dash"))
                        )
                      )
                      
             ),
             
tabPanel("Predictions",  
         sidebarPanel(
           
           #  img(src='http://res.cloudinary.com/x78163/image/upload/v1510825161/DS_logo_rsoctl.png', align = "center", width="250", height="250"),
           tags$br(),
           
           conditionalPanel(
             condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
             HTML('<center><img src="http://www.freepngimg.com/download/calendar/4-2-calendar-png-hd.png" style ="width="150", height="100"></center>'),
             tags$br(),
             tags$br(),
            startDS,
            tags$br(),
            endDS,
            tags$br(),
             dateRangeInput("dateSelect", "Select Date Range:",
                            start = "2007-01-01",
                            end   = "2007-01-31"),
             
           
             tags$br(),
             HTML('<center><img src="http://iconbug.com/data/95/256/8696325e0e7407823058632e68fb5970.png" style ="width="75", height="100"></center>'),
             tags$br(),
             tags$br(),
             sliderInput("predict", "Number of Days to Predict:",
                         min = 0, max = 60, step = 1,
                         value = 30), 
             #sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/100), br(),
             tags$br(),
             HTML('<center><img src="http://www.clker.com/cliparts/e/0/7/1/1197123164299010126JPortugall_icon_bus_bar.svg.med.png" style ="width="100", height="100"></center>'),
             
             tags$br(),
             tags$br(),
             selectInput('series', label='Choose a Data Series to Predict', choices = c('Global Active','Global Reactive', 'Amps', 'Kitchen', 'Laundry', 'HVAC')), br(), br(),
             selectInput('interval', label='Choose a time Interval', choices = c('Every Day (Default)','Every Second (please no)','Every Minute (poor computer)', 'Every Hour',  'Every Week')), br()
           )
           
         ),
         
         mainPanel(
           
           
           
           tabsetPanel(
             tabPanel("Linear Forecasts", plotOutput("linearForecast")),
             tabPanel("Time Series Decomposition", plotOutput("tsDecomp")),
             tabPanel("Holt-Winters Predictions", plotOutput("hwPrediction")),
             tabPanel("Prophet Predictions", plotOutput("prophetPrediction"),downloadButton('downloadprophet', 'Download Charts'))
           )
         )
         
         ),

tabPanel("Take Aways",  
                     
                      h1("Recommendation: Install The Submeters In The Housing Project", align = "center"),
                      tags$br(),
                      tags$br(),
                       tags$ol(margin= "20%",
               tags$li("Housing Community Meets 'Renewable Energy' Reactive Power Guidelines"), 
               tags$br(),
               tags$li("Builder Can Re-Allocate Equipment to Optimize Material Costs"), 
               tags$br(),
               tags$li("Home Owners Can Save Money With Smart Home Appliances (Off-Peak Power Usage)"),
               tags$br(),
               tags$li("Home Owners Can Remotely Monitor Their Home (Identify Failing Systems)") 
             ),
             tags$br(),
             tags$br(),
             HTML('<center><img src="http://res.cloudinary.com/x78163/image/upload/v1510907256/DS_logo_rmmtbo.png" style ="width="300", height="300"></center>')
             )
            
             
          #   titlePanel("Recommendation: Install The Submeters In The Housing Project")
             )
 
  
  
)




# Mandatory 2/2 Shiny App requirement---> server call
server <- function(input, output) {
  
  ###----> Do some awesome magic to make all of the charts reactive.  Basically, run Apriori when you move the slider!   WOOOOOOO!!  
  #apriori (import, parameter = list(supp=input$supp,conf = input$conf, minlen= input$minL, maxlen=input$maxL))
  dataInput <- reactive({
    results = cleaned
    #-----------------------Date Filter Function...There is some crazy wonky shit happening here....Enable only if it makes sense to you------
    # if(input$week=="All")
    # {
       dates = filter(cleaned, cleaned$DateTime >= input$daterange[1] & cleaned$DateTime <= input$daterange[2] )
    # }
    # 
    # if(input$week =="Weekend")
    # {
    #   
    #   dates = is.weekend(results$DateTime)  
    # }
    # 
    # if(input$week == "Weekday")
    # {
    #   
    #   dates = filter(results, wday(cleaned$DateTime, label=FALSE) )
    # }
    
    
    #-----------------------Hour Range Filter Function --------------------------------------------------------------
    # Moved into the Night/Day Filter Function
    #----------------------Night/Day Filter Function ----------------------------------------------------------------
    if(input$light == "All Day")
    {
      results = filter(dates, as.numeric(dates$Time) >= input$time[1] & as.numeric(dates$Time) <= input$time[2] )
    }
    if(input$light == "Day")
    {
      nightStart = as.POSIXct("18:30:00","%H:%M:%S", tz = "") #start night and time
      nightEnd = as.POSIXct("06:30:00","%H:%M:%S",  tz = "") #end night and time
      dates$Time = as.POSIXct(dates$Time,"%H:%M:%S", tz = "")
      results = filter(dates, dates$Time > nightStart | dates$Time <= nightEnd )
    }
    if(input$light == "Night")
    {
      nightStart = as.POSIXct("18:30:00","%H:%M:%S", tz = "") #start night and time
      nightEnd = as.POSIXct("06:30:00","%H:%M:%S",  tz = "") #end night and time
      dates$Time = as.POSIXct(dates$Time,"%H:%M:%S", tz = "")
      results = filter(dates, dates$Time < nightStart | dates$Time >= nightEnd )
    }
    
    
    #----------------------Weekend/Week Filter Function   NOT WORKING AWWWWWWWWW :(  --------------------------------------------------------------
    
    # # To avoid displaying dates as integers in outputted table
    #results$EndDate = as.character(results$DateTime) 
    
    # 
    #  #---------> Create Day Select Filter -----------------------
    # dateStart = as.POSIXct(input$daterange[1],"%Y-%m-%d", tz = "") #start day and time
    # dateEnd = as.POSIXct(input$daterange[2],"%Y-%m-%d",  tz = "") #end day and time
    # 
    # filtered = filter(cleaned, cleaned$DateTime>dateStart & cleaned$DateTime < dateEnd) #filters between start and end selected
    # filtered$Active
    
    #------------------Output results.  Call this for plots with: dataInput()$results
    
    results
  })
  
  ##############################################  Prediction Space ################################################
  predictInput <- reactive({
    
    predictRange = input$predict
    dateSelect = input$dateSelect
    series = input$series
    interval = input$interval
    intervalSelected = "days"
    seriesSelected= "Active"
    y = cleanTail$Active
   if(interval == "Every Day (Default)")
   {
     intervalSelected = "days"
   }
    if(interval == "Every Second (please no)")
    {
      intervalSelected = "seconds"
    }
    if(interval == "Every Minute (please no)")
    {
      intervalSelected = "minutes"
    }
    if(interval == "Every Hour")
    {
      intervalSelected = "hours"
    }
    if(interval == "Every Week")
    {
      intervalSelected = "weeks"
    }
    if(series == "Global Active")
    { 
      y = cleanTail$Active
    }
    if(series == "Global Reactive")
    {
      y = cleanTail$Reactive
    }
    if(series == "Amps")
    {
      y = cleanTail$Amps
    }
    if(series == "Kitchen")
    {
      y = cleanTail$Kitchen
    }
    if(series == "Laundry")
    {
      y = cleanTail$Laundry
    }
    if(series == "HVAC")
    {
      y = cleanTail$HVAC
    }
    
    
  
    ds = cleanTail$DateTime
    dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
    head(dat.xts) #quick systems check to make sure it is working
    ep <- endpoints(dat.xts,intervalSelected) #our endpoints will be hours (so hourly) and using endpoint function
    mean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
    mean = as.data.frame(mean) #converting to dataframe
    meanDF = data.matrix(as.data.frame(mean)) #shifting to matrix
    meanDF = as.data.frame(meanDF) #Pulling back into dataframe
    meanDF = setDT(meanDF, keep.rownames = TRUE)[]  #making the row names into a column
    colnames(meanDF) = c("ds", "y") #Applying labels from above
   # demand4 <- ts(meanDF$y, frequency = 4)
   # demand12 <- ts(meanDF$y, frequency = 12)
    
    
    
  })
  #--------------------------------->  <----------------------------

  #--------------------------------->  <----------------------------
  
  # output$tsDecomp =  renderPlot({ 
  #   
  #   #---------------------> Moving Average Additive---------------------------------------------
  #   
  #  
  # 
  #   
  #   
  #   #---------------------> Moving Average Multiplicative---------------------------------------------
  #   
  #   
  #   demand = predictInput()$demand12
  #   
  #   trend_air = ma(demand, order = 12, centre = T)
  #   plot(as.ts(demand))
  #   lines(trend_air)
  #   plot(as.ts(trend_air))
  #   
  #   
  #   
  #   #---------------------> Seasonal decomposed Data Addititive--------------------------
  #   cleanTail = cleaned
  #   y = cleanTail$Active
  #   ds = cleanTail$DateTime
  #   dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
  #   head(dat.xts) #quick systems check to make sure it is working
  #   ep <- endpoints(dat.xts,'days') #our endpoints will be hours (so hourly) and using endpoint function
  #   dailyMean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
  #   dailyMean = as.data.frame(dailyMean) #converting to dataframe
  #   dailyMeanDF = data.matrix(as.data.frame(dailyMean)) #shifting to matrix
  #   dailyMeanDF = as.data.frame(dailyMeanDF) #Pulling back into dataframe
  #   dailyMeanDF = setDT(dailyMeanDF, keep.rownames = TRUE)[]  #making the row names into a column
  #   colnames(dailyMeanDF) = c("ds", "y") #Applying labels from above
  #   demand <- ts(dailyMeanDF$y, frequency = 4)
  #   
  #  
  #   decompose_beer = decompose(demand, "additive")
  #   
  #   plot(as.ts(decompose_beer$seasonal))
  #   plot(as.ts(decompose_beer$trend))
  #   plot(as.ts(decompose_beer$random))
  #   plot(decompose_beer)
  #   
  #   
  #   #---------------------> Seasonal decomposed Data Multiplicative--------------------------
  #   cleanTail = cleaned
  #   y = cleanTail$Active
  #   ds = cleanTail$DateTime
  #   dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
  #   head(dat.xts) #quick systems check to make sure it is working
  #   ep <- endpoints(dat.xts,'days') #our endpoints will be hours (so hourly) and using endpoint function
  #   dailyMean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
  #   dailyMean = as.data.frame(dailyMean) #converting to dataframe
  #   dailyMeanDF = data.matrix(as.data.frame(dailyMean)) #shifting to matrix
  #   dailyMeanDF = as.data.frame(dailyMeanDF) #Pulling back into dataframe
  #   dailyMeanDF = setDT(dailyMeanDF, keep.rownames = TRUE)[]  #making the row names into a column
  #   colnames(dailyMeanDF) = c("ds", "y") #Applying labels from above
  #   demand <- ts(dailyMeanDF$y, frequency = 12)
  #  
  #   
  #   
  #   decompose_air = decompose(demand, "multiplicative")
  #   
  #   plot(as.ts(decompose_air$seasonal))
  #   plot(as.ts(decompose_air$trend))
  #   plot(as.ts(decompose_air$random))
  #   plot(decompose_air)
  #   
  #   
  #   #---------------------> Using STL ------------------------------------------------------
  #   
  #   cleanTail = cleaned
  #   y = cleanTail$Active
  #   ds = cleanTail$DateTime
  #   dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
  #   head(dat.xts) #quick systems check to make sure it is working
  #   ep <- endpoints(dat.xts,'days') #our endpoints will be hours (so hourly) and using endpoint function
  #   dailyMean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
  #   dailyMean = as.data.frame(dailyMean) #converting to dataframe
  #   dailyMeanDF = data.matrix(as.data.frame(dailyMean)) #shifting to matrix
  #   dailyMeanDF = as.data.frame(dailyMeanDF) #Pulling back into dataframe
  #   dailyMeanDF = setDT(dailyMeanDF, keep.rownames = TRUE)[]  #making the row names into a column
  #   colnames(dailyMeanDF) = c("ds", "y") #Applying labels from above
  #   demand <- ts(dailyMeanDF$y, frequency = 4)
  #   
  #   
  #   
  #   
  #   stl_beer = stl(demand, "periodic")
  #   seasonal_stl_beer   <- stl_beer$time.series[,1]
  #   trend_stl_beer     <- stl_beer$time.series[,2]
  #   random_stl_beer  <- stl_beer$time.series[,3]
  #   
  #   plot(demand)
  #   plot(as.ts(seasonal_stl_beer))
  #   plot(trend_stl_beer)
  #   plot(random_stl_beer)
  #   plot(stl_beer)
  #   
  #   
  # })
  
  #--------------------------------->  <----------------------------
  linearInput = reactive({
    
     cleanTail = filter(cleaned, cleaned$DateTime >= input$dateSelect[1] & cleaned$DateTime <= input$dateSelect[2] )
    predictRange = 30
    predictRange = as.numeric(input$predict)
    
    series = input$series
    interval = input$interval
    intervalSelected = "hours"
    seriesSelected= "Active"
    y = cleanTail$Active
    if(interval == "Every Day (Default)")
    {
      intervalSelected = "days"
    }
    if(interval == "Every Second (please no)")
    {
      intervalSelected = "seconds"
    }
    if(interval == "Every Minute (please no)")
    {
      intervalSelected = "minutes"
    }
    if(interval == "Every Hour")
    {
      intervalSelected = "hours"
    }
    if(interval == "Every Week")
    {
      intervalSelected = "weeks"
    }
    if(series == "Global Active")
    { 
      y = cleanTail$Active
    }
    if(series == "Global Reactive")
    {
      y = cleanTail$Reactive
    }
    if(series == "Amps")
    {
      y = cleanTail$Amps
    }
    if(series == "Kitchen")
    {
      y = cleanTail$Kitchen
    }
    if(series == "Laundry")
    {
      y = cleanTail$Laundry
    }
    if(series == "HVAC")
    {
      y = cleanTail$HVAC
    }
    ds = cleanTail$DateTime
    dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
    ep <- endpoints(dat.xts,intervalSelected) #our endpoints will be hours (so hourly) and using endpoint function
    mean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
    mean = as.data.frame(mean) #converting to dataframe
    meanDF = data.matrix(as.data.frame(mean)) #shifting to matrix
    meanDF = as.data.frame(meanDF) #Pulling back into dataframe
    meanDF = setDT(meanDF, keep.rownames = TRUE)[]  #making the row names into a column
    colnames(meanDF) = c("ds", "y") #Applying labels from above
    
    demand <- ts(meanDF$y, frequency = 12)


      decompose_air = decompose(demand, "multiplicative")

      plot(as.ts(decompose_air$seasonal))
      plot(as.ts(decompose_air$trend))
      plot(as.ts(decompose_air$random))
      plot(decompose_air)
    
  })
  
  
  output$linearForecast =  renderPlot({ 
 linearInput()
  })
  
  
  
  stlInput <- reactive({
    cleanTail = filter(cleaned, cleaned$DateTime >= input$dateSelect[1] & cleaned$DateTime <= input$dateSelect[2] )
    predictRange = 30
    predictRange = as.numeric(input$predict)
    
    series = input$series
    interval = input$interval
    intervalSelected = "hours"
    seriesSelected= "Active"
    y = cleanTail$Active
    if(interval == "Every Day (Default)")
    {
      intervalSelected = "days"
    }
    if(interval == "Every Second (please no)")
    {
      intervalSelected = "seconds"
    }
    if(interval == "Every Minute (please no)")
    {
      intervalSelected = "minutes"
    }
    if(interval == "Every Hour")
    {
      intervalSelected = "hours"
    }
    if(interval == "Every Week")
    {
      intervalSelected = "weeks"
    }
    if(series == "Global Active")
    { 
      y = cleanTail$Active
    }
    if(series == "Global Reactive")
    {
      y = cleanTail$Reactive
    }
    if(series == "Amps")
    {
      y = cleanTail$Amps
    }
    if(series == "Kitchen")
    {
      y = cleanTail$Kitchen
    }
    if(series == "Laundry")
    {
      y = cleanTail$Laundry
    }
    if(series == "HVAC")
    {
      y = cleanTail$HVAC
    }
    ds = cleanTail$DateTime
    dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
    ep <- endpoints(dat.xts,intervalSelected) #our endpoints will be hours (so hourly) and using endpoint function
    mean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
    mean = as.data.frame(mean) #converting to dataframe
    meanDF = data.matrix(as.data.frame(mean)) #shifting to matrix
    meanDF = as.data.frame(meanDF) #Pulling back into dataframe
    meanDF = setDT(meanDF, keep.rownames = TRUE)[]  #making the row names into a column
    colnames(meanDF) = c("ds", "y") #Applying labels from above
    
      
      demand <- ts(meanDF$y, frequency = 4)
      stl_beer = stl(demand, "periodic")
      seasonal_stl_beer   <- stl_beer$time.series[,1]
      trend_stl_beer     <- stl_beer$time.series[,2]
      random_stl_beer  <- stl_beer$time.series[,3]

      plot(demand)
      plot(as.ts(seasonal_stl_beer))
      plot(trend_stl_beer)
      plot(random_stl_beer)
      plot(stl_beer)

  })
  
  output$tsDecomp =  renderPlot({
    stlInput()
  })
  
  hwInput <- reactive({
    cleanTail = cleaned#filter(cleaned, cleaned$DateTime >= input$dateSelect[1] & cleaned$DateTime <= input$dateSelect[2] )
    predictRange = 30
    predictRange = as.numeric(input$predict)
    
    series = input$series
    interval = input$interval
    intervalSelected = "hours"
    seriesSelected= "Active"
    y = cleanTail$Active
    if(interval == "Every Day (Default)")
    {
      intervalSelected = "days"
    }
    if(interval == "Every Second (please no)")
    {
      intervalSelected = "seconds"
    }
    if(interval == "Every Minute (please no)")
    {
      intervalSelected = "minutes"
    }
    if(interval == "Every Hour")
    {
      intervalSelected = "hours"
    }
    if(interval == "Every Week")
    {
      intervalSelected = "weeks"
    }
    if(series == "Global Active")
    { 
      y = cleanTail$Active
    }
    if(series == "Global Reactive")
    {
      y = cleanTail$Reactive
    }
    if(series == "Amps")
    {
      y = cleanTail$Amps
    }
    if(series == "Kitchen")
    {
      y = cleanTail$Kitchen
    }
    if(series == "Laundry")
    {
      y = cleanTail$Laundry
    }
    if(series == "HVAC")
    {
      y = cleanTail$HVAC
    }
    ds = cleanTail$DateTime
    dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
    ep <- endpoints(dat.xts,intervalSelected) #our endpoints will be hours (so hourly) and using endpoint function
    mean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
    mean = as.data.frame(mean) #converting to dataframe
    meanDF = data.matrix(as.data.frame(mean)) #shifting to matrix
    meanDF = as.data.frame(meanDF) #Pulling back into dataframe
    meanDF = setDT(meanDF, keep.rownames = TRUE)[]  #making the row names into a column
    colnames(meanDF) = c("ds", "y") #Applying labels from above
    
    demand <- ts(meanDF$y, frequency = 4)
    hw <- HoltWinters(demand)
    forecast <- predict(hw, n.ahead = predictRange, prediction.interval = T, level = 0.95)
    autoplot(forecast)
    
  
  })
  
  output$hwPrediction =  renderPlot({ 
    
    hwInput()
    
  })
  
  prophetInput <- reactive({
    
    
    cleanTail = filter(cleaned, cleaned$DateTime >= input$dateSelect[1] & cleaned$DateTime <= input$dateSelect[2] )
    predictRange = 30
    predictRange = as.numeric(input$predict)
  
    series = input$series
    interval = input$interval
   intervalSelected = "hours"
    seriesSelected= "Active"
    y = cleanTail$Active
    if(interval == "Every Day (Default)")
    {
      intervalSelected = "days"
    }
    if(interval == "Every Second (please no)")
    {
      intervalSelected = "seconds"
    }
    if(interval == "Every Minute (please no)")
    {
      intervalSelected = "minutes"
    }
    if(interval == "Every Hour")
    {
      intervalSelected = "hours"
    }
    if(interval == "Every Week")
    {
      intervalSelected = "weeks"
    }
    if(series == "Global Active")
    { 
      y = cleanTail$Active
    }
    if(series == "Global Reactive")
    {
      y = cleanTail$Reactive
    }
    if(series == "Amps")
    {
      y = cleanTail$Amps
    }
    if(series == "Kitchen")
    {
      y = cleanTail$Kitchen
    }
    if(series == "Laundry")
    {
      y = cleanTail$Laundry
    }
    if(series == "HVAC")
    {
      y = cleanTail$HVAC
    }
    ds = cleanTail$DateTime
    dat.xts <- xts(x = y,as.POSIXct(ds)) #convert to xts format for maximum awesomeness
     ep <- endpoints(dat.xts,intervalSelected) #our endpoints will be hours (so hourly) and using endpoint function
    mean = period.apply(dat.xts,ep,mean) #applying the mean function to the defined period (hourly)
    mean = as.data.frame(mean) #converting to dataframe
    meanDF = data.matrix(as.data.frame(mean)) #shifting to matrix
    meanDF = as.data.frame(meanDF) #Pulling back into dataframe
    meanDF = setDT(meanDF, keep.rownames = TRUE)[]  #making the row names into a column
    colnames(meanDF) = c("ds", "y") #Applying labels from above
    y = meanDF$y
    ds = meanDF$ds
    forecasting = data.frame(ds, y)  #set up our variables in a data frame
    prophetPredictions = prophet(forecasting)  #This step releases the wizard (generates model)
    future = make_future_dataframe(prophetPredictions, periods=predictRange) #Set the number of days (periods) you want to predict
    forecast = predict(prophetPredictions, future) # Unleash the wizard on the data dragon (applies model)
   plot(prophetPredictions, forecast, ylab = "Active Watt Hours", xlab = "Date", main = "30 Day Prediction with Prophet")
    
    
    
    
  })
  #--------------------------------->  <----------------------------
  
  output$prophetPrediction =  renderPlot({ 
    
prophetInput()
    
  })
 
  
  ############################################### End of Prediction Space #################################################
  
  ###-----------> Output a table showing the frequency of the items. 
  output$dash =  renderText({ 
    
    price = dataInput()
    pricekwpeak = .159
    pricekwoffpeak = .1252
    price$price1 = price$Kitchen*pricekwpeak
    price$price2 = price$Laundry*pricekwpeak
    price$price3 = price$HVAC*pricekwpeak 
    price$priceTot = (price$Active)/10000*pricekwpeak 
    
    #-------Total amounts of power for each region
    sumKW1 = sum( price$Kitchen)
    sumKW2 = sum( price$Laundry)
    sumKW3 = sum( price$HVAC)
    sumKWTot = sum( price$Active)/100000
    
    #--------Totalcosts for each region
    sumP1 = sum( price$price1)
    sumP2 = sum( price$price2)
    sumP3 = sum( price$price3)
    sumPTot = sum( price$priceTot)
    
    #------------Totaled submetered
    sumSubKW = sum(sumKW1,sumKW2,sumKW3)
    sumSubP = sum(sumP1,sumP2,sumP3)
    
    totPercent = (sumSubKW/sumKWTot)*10
    
    # price$sumP1 <- cumsum(price$price1)
    # price$sumP2 <- cumsum(price$price2)
    # price$sumP3 <- cumsum(price$price3)
    
    paste("For the selected date range of",input$daterange[1], "to", input$daterange[2], " a total of  ", sumKWTot,  "(kW) consumed.  This cost approximately ", sumPTot , " dollars.  The Kitchen used: ", sumKW1,  " Watts and cost:  ", sumP1, " dollars. The Laundry Room used: ", sumKW2,  " Watts and cost:  ", sumP2, " dollars.  The HVAC used: ", sumKW3,  " Watts and cost:  ", sumP3, " dollars. In total, the sub metered areas cost" , sumSubP, " dollars, and represented ", totPercent, "% of the total house power consumption.")
    
    
    
  })
  
  output$combined =  renderPlot({ 
    
    plot(dataInput()$DateTime, dataInput()$Kitchen, type="l", ylab="KiloWatts", xlab="Date Range", main = "Normalized Plot of Entire Dataset")
    lines(dataInput()$DateTime, dataInput()$Laundry, type="l", col="red")
    lines(dataInput()$DateTime, dataInput()$HVAC, type="l", col="blue")
    lines(dataInput()$DateTime, dataInput()$Reactive/10000, type="l", col="green")
    lines(dataInput()$DateTime, dataInput()$Reactive/10000, type="l", col="brown")
    lines(dataInput()$DateTime, dataInput()$Voltage/1000, type="l", col="purple")
    lines(dataInput()$DateTime, dataInput()$Amps, type="l", col="orange")
    legend("topright", c("Active","Reactive","Volts", "Amps","Kitchen", "Laundry", "HVAC"   ), lty=1, lwd=2.5, col=c("brown", "green", "purple", "orange", "black", "red", "blue"))
    
    
  })
  
  
  ###------------> output the rules, and make them sortable by the drop down magic box thingy
  output$rules = DT:: renderDataTable({
    
    dataInput()
    
  })
  ###------------> Output the cool interactive scatterplot
  output$activeoverview = renderPlot({
    
    plot(dataInput()$DateTime, dataInput()$Active/1000, type="l", ylab="KiloWatt Hours", xlab="Date Range", main = "Normalized Plot of Daily Power Usage")
    lines(dataInput()$DateTime, dataInput()$Reactive/100, type="l", col="green")
    lines(dataInput()$DateTime, dataInput()$Voltage/1000, type="l", col="purple")
    lines(dataInput()$DateTime, dataInput()$Amps/1000, type="l", col="orange")
    legend("topright", c("Active","Reactive","Volts", "Amps"   ), lty=1, lwd=2.5, col=c("black", "green", "purple", "orange"))
    
  })
  
  ###------------------> Submeter Overview Chart-------------------
  output$suboverview = renderPlot({
    
    plot(dataInput()$DateTime, dataInput()$Kitchen, type="l", ylab="Kilowatt Hours", xlab="Date Range", main ="Plot of Kitchen, Laundry and HVAC Requirements")
    lines(dataInput()$DateTime, dataInput()$Laundry, type="l", col="red")
    lines(dataInput()$DateTime, dataInput()$HVAC, type="l", col="blue")
    legend("topright", c("Kitchen", "Laundry", "HVAC"   ), lty=1, lwd=2.5, col=c( "black", "red", "blue"))
    
  })
  ###------------> Boring Histograms
  output$hist = renderPlot({
    sampled = dataInput()
    actReact = ggplot(sampled, aes( sampled$Active/1000), fill = "blue") + 
      geom_histogram() +
      geom_histogram(data = sampled, aes( sampled$Reactive/1000), fill = "red")    + labs(title = "Most Frequent Power Level (Home) ", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), x = "KiloWatts (Normalized)", y = "Number of Observations")
    
    sub = ggplot(sampled, aes( sampled$Kitchen), fill = "purple") + 
      geom_histogram() +
      geom_histogram(data = sampled, aes( sampled$Laundry), fill = "red")    +
      geom_histogram(data = sampled, aes( sampled$HVAC), fill = "green")+ labs(title = "Most Frequent Power Levels (Submetered Rooms)", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), x = "KiloWatts (Normalized)", y = "Number of Observations")
    
    
    # plot1 = hist(dataInput()$Active/100, col=rgb(1,0,0,0.5), main="Histogram of Active and Reactive Power", xlab="KiloWatts")
    #  hist(dataInput()$Reactive/10, col=rgb(0,0,1,0.5), add=T)
    grid.arrange(actReact,sub ,ncol = 1, nrow = 2)
    
  })
  ##---------> Check Active and Reactive Power 
  output$activecosts = renderPlot({
    
    sampled = dataInput()
    sampled$plott = sampled$Reactive/((sampled$Active+sampled$Reactive)*100)
    sampled$sum1 <- cumsum((sampled$Active+sampled$Reactive)/1000)
    percentReactive = ggplot(sampled, aes(y = plott, x = DateTime))+   geom_point()+ geom_smooth() +labs(y="% Reactive Power", x = "Date Range", title = "Active/Reactive Power Ratio" , subtitle = paste("5% is Maximum for a Green Community. Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ))+ theme_bw()
    
    actReact = ggplot(sampled, aes(sampled$DateTime, sampled$Active)) + 
      geom_line() +
      geom_line(data = sampled, aes(sampled$DateTime, sampled$Reactive), color = "red")    +
      
      geom_line(data = sampled, aes(sampled$DateTime, sampled$sum1), color = "green")+ labs(title = "Total Instant Demand and Cumulative Power Requirement", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), x = "Date Range", y = "KiloWatts")
    grid.arrange(actReact,percentReactive ,ncol = 1, nrow = 2)
  })
  ########----------------Calculate the Costs associated with Each Room
  output$subcost = renderPlot({
    price = dataInput()
    pricekwpeak = .159
    pricekwoffpeak = .1252
    price$price1 = price$Kitchen*pricekwpeak
    price$price2 = price$Laundry*pricekwpeak
    price$price3 = price$HVAC*pricekwpeak  
    
    sum1 = sum( price$price1)
    sum2 = sum( price$price2)
    sum3 = sum( price$price3)
    
    price$sum1 <- cumsum(price$price1)
    price$sum2 <- cumsum(price$price2)
    price$sum3 <- cumsum(price$price3)
    
    df <- data.frame(
      group = c("Kitchen", "Laundry", "HVAC"),
      value = c(sum1,sum2, sum3)
    )
    
    polar = ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+ labs(title = "Pie Chart of Consumption", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), y = "Percent", x = "")
    
    bar = ggplot(df, aes(group, value)) +
      geom_col()+ labs(title = "Relative Magnitutes of Power Consumed", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "SubMetered Area", y = "KiloWatt Hours")
    
    instaPrice = ggplot(price, aes(price$DateTime, price$price1, fill = price$price1)) + 
      geom_line() +
      geom_line(data = price, aes(price$DateTime, price$price2), color = "red") +  #the damn plus must be on the same line
      geom_line(data = price, aes(price$DateTime, price$price3), color = "green")+ labs(title = "Instant Submeter Cost", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "Date Range", y = "Total Cost ($)")
    
    totprice = ggplot(price, aes(price$DateTime, price$sum1)) + 
      geom_line() +
      geom_line(data = price, aes(price$DateTime, price$sum2), color = "red") +  #the damn plus must be on the same line
      geom_line(data = price, aes(price$DateTime, price$sum3), color = "green")+ labs(title = "Aggregated Submeter Cost", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "Date Range", y = "Total Cost ($)")
    
    grid.arrange(polar, bar, instaPrice, totprice,ncol = 2, nrow = 2)
  })
  
  ###############------------> Code to Histogram Chart  ** Now with Extra Dates!
  plotInput = reactive({
    sampled = dataInput()
    actReact = ggplot(sampled, aes( sampled$Active/1000), fill = "blue") + 
      geom_histogram() +
      geom_histogram(data = sampled, aes( sampled$Reactive/1000), fill = "red")    + labs(title = "Most Frequent Power Level (Home)", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), x = "KiloWatts (Normalized)", y = "Number of Observations")
    
    sub = ggplot(sampled, aes( sampled$Kitchen), fill = "purple") + 
      geom_histogram() +
      geom_histogram(data = sampled, aes( sampled$Laundry), fill = "red")    +
      geom_histogram(data = sampled, aes( sampled$HVAC), fill = "green")+ labs(title = "Most Frequent Power Levels (Submetered Rooms)", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), y = "Number of Observations")
    
    
    # plot1 = hist(dataInput()$Active/100, col=rgb(1,0,0,0.5), main="Histogram of Active and Reactive Power", xlab="KiloWatts")
    #  hist(dataInput()$Reactive/10, col=rgb(0,0,1,0.5), add=T)
    grid.arrange(actReact,sub ,ncol = 1, nrow = 2)
  })
  
  
  plotInput2 = reactive({
    price = dataInput()
    pricekwpeak = .159
    pricekwoffpeak = .1252
    price$price1 = price$Kitchen*pricekwpeak
    price$price2 = price$Laundry*pricekwpeak
    price$price3 = price$HVAC*pricekwpeak  
    
    sum1 = sum( price$price1)
    sum2 = sum( price$price2)
    sum3 = sum( price$price3)
    
    price$sum1 <- cumsum(price$price1)
    price$sum2 <- cumsum(price$price2)
    price$sum3 <- cumsum(price$price3)
    
    df <- data.frame(
      group = c("Kitchen", "Laundry", "HVAC"),
      value = c(sum1,sum2, sum3)
    )
    
    polar = ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+ labs(title = "Pie Chart of Consumption", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), y = "Percent", x = "")
    
    bar = ggplot(df, aes(group, value)) +
      geom_col()+ labs(title = "Relative Magnitutes of Power Consumed", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "SubMetered Area", y = "KiloWatt Hours")
    
    instaPrice = ggplot(price, aes(price$DateTime, price$price1, fill = price$price1)) + 
      geom_line() +
      geom_line(data = price, aes(price$DateTime, price$price2), color = "red") +  #the damn plus must be on the same line
      geom_line(data = price, aes(price$DateTime, price$price3), color = "green")+ labs(title = "Instant Submeter Cost", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "Date Range", y = "Total Cost ($)")
    
    totprice = ggplot(price, aes(price$DateTime, price$sum1)) + 
      geom_line() +
      geom_line(data = price, aes(price$DateTime, price$sum2), color = "red") +  #the damn plus must be on the same line
      geom_line(data = price, aes(price$DateTime, price$sum3), color = "green")+ labs(title = "Aggregated Submeter Cost", subtitle = paste(input$daterange[1], "to",  input$daterange[2], "(",  input$time[1], "to", input$time[2],")", sep=" " ), x = "Date Range", y = "Total Cost ($)")
    
    grid.arrange(polar, bar, instaPrice, totprice,ncol = 2, nrow = 2)
  })
  
  
  plotInput3 = reactive({
    sampled = dataInput()
    sampled$plott = sampled$Reactive/((sampled$Active+sampled$Reactive)*100)
    sampled$sum1 <- cumsum((sampled$Active+sampled$Reactive)/1000)
    percentReactive = ggplot(sampled, aes(y = plott, x = DateTime))+   geom_point()+ geom_smooth() +labs(y="% Reactive Power", x = "Date Range", title = "Active/Reactive Power Ratio" , subtitle = paste("5% is Maximum for a Green Community. Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ))+ theme_bw()
    
    actReact = ggplot(sampled, aes(sampled$DateTime, sampled$Active)) + 
      geom_line() +
      geom_line(data = sampled, aes(sampled$DateTime, sampled$Reactive), color = "red")    +
      
      geom_line(data = sampled, aes(sampled$DateTime, sampled$sum1), color = "green")+ labs(title = "Total Instant Demand and Cumulative Power Requirement", subtitle = paste("Dates from:",input$daterange[1], "to",  input$daterange[2], "And Time From:",  input$time[1], "to", input$time[2], sep=" " ), x = "Date Range", y = "KiloWatts")
    grid.arrange(actReact,percentReactive ,ncol = 1, nrow = 2)
  })
  
  
  plotInput4 = reactive({
    prophetInput()
  })
  
  plotInput4 = reactive({
    
  })
  
  
  output$downloadCombined <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  
  output$downloadHist <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  
  output$downloadActiveCosts <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput3(), device = "png")
    }
  )
  
  output$downloadSubOverview <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  output$downloadSubCost <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput2(), device = "png")
    }
  )
  output$downloadprophet <- downloadHandler(
    filename = function() { paste("inputdataset", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput4(), device = "png")
    }
  )
  #  output$downloadData <- downloadHandler(
  #   filename = 'My_Rules.csv',
  #   content = function(file) {
  #     write.csv(rules2df(dataInput()), filename)
  #   }
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)

