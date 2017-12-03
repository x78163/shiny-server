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



