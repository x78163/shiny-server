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



