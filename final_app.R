## app.R ##
#Importing libraries
library(shinydashboard)
library(maps)
library(shiny)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(plotly)
library(dygraphs)
library(shinyWidgets)
library(leaflet.extras)
library(xts)
library(RColorBrewer)
library(formattable)
library(dplyr)
library(repmis)
library(highcharter)

#reading the files
melbourne_housing_data <- read.csv('Melbourne_data_final_cleaned_new.csv')

#creating the dashboard with different menu options and the different icons on the left panel.
ui <- dashboardPage(skin = "purple",
                    
                    dashboardHeader(title = "HOUSING MARKET"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Home", tabName = "dashboard", icon = icon("home")),
                            menuItem("Regions", tabName = "Regions", icon = icon("door-open")),
                            menuItem("Suburbs", tabName = "Suburbs", icon = icon("door-open")),
                            menuItem("Prices", tabName = "Prices", icon = icon("dollar-sign")),
                            menuItem(
                                HTML('<marquee behavior="scroll" direction="left">MELBOURNE HOUSING MARKET</marquee>'),
                                 tabName = "coal_forecasting",
                                icon = icon("line-chart", class = "fa-spin",lib="font-awesome"))
                        )
                    ),
                    
                    ## Body content
                    dashboardBody(
                        tags$head(tags$style(HTML('
                          .box {margin: 5px;}'
                        ))),
                        
                        tabItems(
                            
                            # First tab content
                            
                            tabItem(tabName = "dashboard",
                                    fluidRow(
                                        box(title=strong("Project Introduction"),width = 12,
                                            p(style="text-align: justify;" , 'The Melbourne housing market Project is done based on the open data 
                                    set available in Kaggle, where the data was obtained by web scrapping from Domain.com.au, 
                                    which consists of 34,855 rows and 21 columns. It consists of a csv file namely complete housing data,
                                    which is taken over three years i.e., 2016, 2017 and 2018, where the data deals about on which date 
                                    the sale of house has happened, and other parameters, that are related to, when the house had been 
                                    saled. The main objective of my visualization is to help user to look up at the kind of properties
                                    like house, townhouse, unit in specific region (or) in specific suburb. From these, they can filter
                                    out, if they consider any region (or) suburb, what kind of properties(house, townhouse, unit) are 
                                    more popular  there. What will be the average price of the property? Why specific suburb (or) region 
                                    has more number of houses? Is it because they have more number of facilities like convenient stores,
                                    educational institutions likewise and let users to know the price varying for each type of house over
                                    the years in a particular suburb (or) region? This project tells a story to a user that, 
                                    after exploring a house in a particular suburb (or) region, the kind of property i.e. more popular
                                    there, the price through which they can render to buy a house, the price varying for that house 
                                    in that particular place over the years, the other properties (or) facilities that are present near
                                    to it(properties (or) facilities present near can be explored by searching in google, why it has
                                    more price?). Letting him know the different options that are available to him, based on his 
                                    interested (or) affordable price range.It gives the user a good narration (or) decision making 
                                    if he wants to purchase a house at one specific place')
                                        )
                                        
                                    ),
                                    titlePanel(title=div(strong("Melbourne Housing Market"),img(src="MelHousing.jpeg")))
                            ),
                            tabItem(tabName = "Regions",
                                    h1("Interactive visualization of map based on selected region"),
                                    fluidRow(
                                        box(title = "Information", solidHeader = T,
                                            width = 12, collapsible = T,
                                            p(style="text-align: justify;" ,'The', strong("User Story"), 'here is as follows, 
                                        This page will let the users to tell a story about different regions. 
                                        Which type of property is more popular in a specific region he selected. 
                                        It also helps them to find the price in the popular property of the region. 
                                        After finding the particular place, even though the price in that place is more and 
                                        more no.of houses are being sold out there, means , there is something interesting in that place.
                                        i.e. it may consists of many convenient stores, schools, colleges likewise, which the user could 
                                        find about the nearby places through google."'),
                                            
                                            p(style="text-align: justify;" ,'Based on the particular property type that we have selected above via', 
                                              strong('radio buttons'), 'i.e. either "All", "Unit", "Townhouse" , "House"
                                      and based on the ',strong('regions'),'we have selected below, the map is being highlighted with the positions
                                      of latitude, longitude which consists of the selected property type in a particular region. The total number of properties
                                      in the specific selected region is also shown. i.e."House", "Unit", "Townhouse". 
                                      Based on the count of properties we can say which kind of property is more popular in a selected region.',
                                              
                                              br(),
                                              strong('On Hovering'),  'on the points in the map, the price is shown. On clicking on that price, address 
                                      in which that price is present is shown. The radius of the sites is taken based on the average price
                                      present in that location',
                                              br(),
                                              'By default', strong("Eastern Metropolitan"), 'region and property type "All" is taken, if no selection is based by the user.' ,
                                              br()) 
                                        )),
                                    
                                    fluidRow(
                                        #a box for selecting a region
                                        box(title =HTML("<font color='#800080', size = '5px'>Select Region</font>"),icon=icon('home'),
                                            solidHeader = F,
                                            width = 6,
                                            collapsible = T,
                                            pickerInput(
                                                label = '',
                                                inputId = "region", 
                                                choices = c('Eastern Metropolitan',
                                                            'Southern Metropolitan',
                                                            'Northern Metropolitan',
                                                            'South-Eastern Metropolitan',
                                                            'Western Metropolitan','All')
                                            )),
                                        
                                        #a box for selecting a property type
                                        box(title = HTML("<font color='#800080', size = '5px'>Select a Property Type</font>"), solidHeader = F,
                                            width = 6, collapsible = T,
                                            prettyRadioButtons(
                                                label = '',
                                                inputId = "type",
                                                choices = c("All", "Unit", "Townhouse", 'House'),
                                                icon = icon("check"), 
                                                inline =TRUE
                                                
                                            )),
                                        box( which = 'plot',width = 12,collapsible = T, 
                                             htmlOutput('text'),
                                             valueBoxOutput("unit"),
                                             valueBoxOutput("townhouse"),
                                             valueBoxOutput("house")
                                        )
                                    ),
                                    fluidRow(
                                        box( which='plot',lty='solid',
                                             width = 12,
                                             leafletOutput('region_map',height = 600),
                                             hr() ))       
                            ),
                            tabItem(tabName = "Suburbs",
                                    h1("Interactive visualization of map based on selected suburb"),
                                    fluidRow(
                                        box(title = "Information", solidHeader = T,
                                            width = 12, collapsible = T,
                                            p(style="text-align: justify;" ,
                                              'The', strong('user story'), 'here is as follows, it will let the users to tell a story about 
                                      different suburbs. Which type of property is more popular in a specific suburb he selected.
                                      It also helps them to find the price in the popular property of the suburb. After finding the 
                                      particular place, even though the price in that place is more and more no.of houses are being sold 
                                      there, means , there is something interesting in that place. i.e. it may consists of many 
                                      convenient stores, schools, colleges likewise, good water facilities, which the user could find 
                                      about the nearby places through google.')
                                            
                                        )),
                                    
                                    # Second tab content
                                    fluidRow(
                                        
                                        box(title =HTML("<font color='#800080', size = '5px'>Select Suburb</font>"),icon=icon('home'),
                                            solidHeader = F,
                                            width = 4,
                                            collapsible = T,
                                            icon=icon("filter"),
                                            uiOutput('suburb_selecting'),
                                            height = "100%"
                                        ),
                                        box(which = 'plot',
                                            solidHeader = F,
                                            width = 8,
                                            dygraphOutput('suburb_dygraph',height = 325)
                                        ),
                                        box(solidHeader = F,
                                            width = 14, collapsible = T,
                                            
                                            p(style="text-align: justify;",'The above dygraph shows the time series graph of the selected suburb over the years 2016, 2017 and 2018 for different months. If no suburb is selected,
                                              by default as mentioned above, that the map of "Reservoir" suburb is shown. Similiarly, "Reservoir" suburb dygraph is shown ',
                                              br()))
                                    ),
                                    fluidRow(
                                        box( which='plot',lty='solid',
                                             width = 12,
                                             leafletOutput('suburb_map',height = 600),
                                             p(style = "text-align: justify;", 'The above map shows the properties present in the selected suburb.
                                         On hovering of the location, it will give the user a price. On clicking on that price it gives the user
                                         the location in which that price is present. The radius of the sites is taken based on the average price
                                      present in that location.'),
                                             hr()
                                             
                                        ))
                            ),
                            
                            #third tab content          
                            tabItem(tabName = "Prices",
                                    h2("Based on price range"),
                                    fluidRow(
                                        box(title = "Information", solidHeader = T,
                                            width = 13, collapsible = T,
                                            p(style="text-align: justify;",
                                              
                                              ' This "Prices" page helps user to get some story based on the price he selected.
                                         It tells us a story with the help of three kinds of graphs. 
                                         Donut chart indicates what kind of property proportion are maximum present 
                                         across all the regions over the Melbourne. Bar chart is animated to know how many number
                                         of rooms could be maximum obtained (or) how many number of rooms is maximum present in the selected price range. 
                                         Third graph i.e. circular bar plot indicates in which region the maximum number of houses are present, 
                                         for the user selected price range. It narrates some of the properties to the user. On hovering on the bar graph
                                         gives the count of rooms. 
                                         This page helps the users a lot when they want to buy the house in a particular price range.
                                         The price slider here can be moved for selecting the prices.')
                                        )),
                                    fluidRow(
                                        
                                        infoBox(title =HTML("<font color='#800080', size = '5px'>Price slider</font>"),
                                                width = 12,
                                                icon=icon("money"),
                                                uiOutput('creating_price_slider')
                                                
                                        )),
                                    
                                    fluidRow(
                                        box(which = 'plot',
                                            solidHeader = F,
                                            width = 12,
                                            highchartOutput("rooms_high_chart",height = "500px")
                                        ),
                                        box(width = 13, collapsed = T,
                                            p(style="text-align: justify;",'The above bar chart shows the count of no.of rooms present in the selected price range. This chart is drawn so that the users knows,
                                    what could be the maximum number of rooms they can get in the selected price range. The higher the count of number of rooms, in the
                                      selected price range means the higher those many number of rooms are present in that range. ',
                                              'On hovering on that particular bar chart gives you the room number and its count.')
                                            
                                        )
                                    ), 
                                    fluidRow(
                                        box(which = 'plot',
                                            solidHeader = F,
                                            width = 12,
                                            plotlyOutput('donut_chart'),
                                            p(style="text-align: justify;", 'The above donut chart indicates what kind of property
                                        proportion are maximum present across all the regions over the Melbourne.
                                        On hovering on it, gives the type of property and the percentage proportionate of the region.')
                                        )
                                    ),
                                    
                                    fluidRow(
                                        box(which = 'plot',
                                            solidHeader = F,
                                            width = 12,
                                            plotOutput('circular_bar_plot_region',height=600)
                                            
                                        ),
                                        
                                        box(width = 12, collapsible = T,
                                            
                                            p(style="text-align: justify;",'The above circular bar plot shows the regions that consists of houses in the particular price selected, The higher the circular bar
                                            curve indicates that the higher number of houses are present in that region of selected price range')
                                        )))
                        )))

server = function(input, output) {
    
# Selecting a suburb
output$suburb_selecting <- renderUI ({
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(melbourne_housing_data$Type)))
    data <- melbourne_housing_data %>% 
        filter(Type %in% c(type))
    #sorting the suburbs based on its price
    suburb <- melbourne_housing_data %>% 
        group_by(Suburb) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count))
    multiInput(
        inputId = "suburb",
        label = "Select Suburbs", 
        choiceNames = lapply(seq_along(suburb$Suburb), function(i) tagList(suburb$Suburb[i])),
        choiceValues = suburb$Suburb
    )
})

#Creating a price slider
output$creating_price_slider <- renderUI({
    sliderInput('slider',
                ' ',
                width='100%',
                min=100000,
                max=4000000,
                step=100000,
                value=c(100000, 500000))
})

output$priceTitle <- renderUI({
    titlePanel(paste0('Properties Priced Between $', input$slider[1], '-$', input$slider[2]))
})

#creating a leaflet map
mapp <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("CartoDB.Positron")
#creating an empty render box, which contains the values later for unit type of property.
output$unit <- renderValueBox({
})
output$text <- renderUI({
    titlePanel(paste0('Total number of Properties available in ', input$region,' region:'))
})
output$unit <- renderValueBox({
    if(input$region == 'All'){
        region = c('Eastern Metropolitan',
                   'Southern Metropolitan',
                   'Northern Metropolitan',
                   'South-Eastern Metropolitan',
                   'Western Metropolitan')
    }
    else{
        region = input$region
    }
    
    data <- melbourne_housing_data %>% 
        filter(Regionname %in% region) %>% 
        filter(Type == 'u')
    valueBox(paste0(nrow(data)),
             "Units", icon = icon("building"), color = "red")
})
#creating a render box, which contains the values for town house type of property.
output$townhouse <- renderValueBox({
    if(input$region == 'All'){
        region = c('Eastern Metropolitan',
                   'Southern Metropolitan',
                   'Northern Metropolitan',
                   'South-Eastern Metropolitan',
                   'Western Metropolitan')
    }
    else{
        region = input$region
    }
    data <- melbourne_housing_data %>% 
        filter(Regionname %in% region) %>% 
        filter(Type == 't')
    valueBox(paste0(nrow(data)), 
             "Townhouses", icon = icon("building"), color = "blue")
})
#creating a render box, which contains the values for house type of property
output$house <- renderValueBox({
    if(input$region == 'All'){
        region = c('Eastern Metropolitan',
                   'Southern Metropolitan',
                   'Northern Metropolitan',
                   'South-Eastern Metropolitan',
                   'Western Metropolitan')
    }
    else{
        region = input$region
    }
    data <- melbourne_housing_data %>% 
        filter(Regionname %in% region) %>% 
        filter(Type == 'h')
    valueBox(paste0(nrow(data)), 
             "Houses", icon = icon("building"), color = "yellow")
})


#creating a leaflet map for regions
output$region_map <- renderLeaflet({
    
    # get the kind of property to filter the data
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(melbourne_housing_data$Type)))
    
    if(input$region == 'All'){
        region = c('Eastern Metropolitan',
                   'Southern Metropolitan',
                   'Northern Metropolitan',
                   'South-Eastern Metropolitan',
                   'Western Metropolitan')
    }
    else{
        region = input$region
    }
    
    #filter the data
    data <- melbourne_housing_data %>% 
        filter(Regionname %in% region) %>% 
        filter(Type %in% c(type)) 
    
    #fix the radius based on the prices 
    radius <- findInterval(data$Price,c(100000, 200000, 300000, 500000, 1000000, 1300000, 1700000, 2000000)) * 8
    
    #give the colors for each property type
    getColor <- function(data) 
    {
        sapply(data$Type, function(Type) {
            if(Type == "h") {
                "#FF8C00"
            } 
            else if(Type == "t") {
                "#0078ff"
            }
            else {
                "Red"
            } }) }
    
    #draw the leaflet map based on the filtered data, colours, view e.t.c.
    mapp %>% 
        setView(lng = 144.98, lat = -37.84 , zoom = 30) %>% 
        addCircles(data = data, ~Longtitude, ~Lattitude,
                   radius, 
                   stroke = T, 
                   color = getColor(data),
                   fillOpacity = 1, 
                   popup = ~paste("<b>","<h4>",as.character(Address), ',',Suburb,"</h4>"),
                   options = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                   label = ~paste0('$', as.character(Price)),
                   labelOptions = labelOptions(noHide = F,style =list("font-size" = "18px"))) %>%
        
        leaflet::addLegend("topright", colors = c("#FF8C00", "#0078ff", "Red"), labels = c("House", "Townhouse", "Unit"), title = "Type of Property") %>%
        fitBounds(lng1 = max(melbourne_housing_data$Longtitude)-0.001,lat1 = max(melbourne_housing_data$Lattitude)-0.001,
                  lng2 = min(melbourne_housing_data$Longtitude)+0.001,lat2 = min(melbourne_housing_data$Lattitude)+0.001)
})

#drawing the leaflet map for suburbs.
output$suburb_map <- renderLeaflet({
    #by default if no selection is made for the suburb type. Set it to "Reservoir.". Because it is the suburb that is found to have maximum number of properties.
    if(is.null(input$suburb)) 
        suburb_data <- melbourne_housing_data %>% 
            filter(Suburb %in% c('Reservoir'))
    else
        suburb_data <- melbourne_housing_data %>% 
            filter(Suburb == as.character(input$suburb))
    #set the radius for the different locations of suburbs based on the prices.
    radius <- findInterval(suburb_data$Price,c(300000, 1000000, 1300000, 1700000, 2000000)) *20
    #get the color for the different type of properties in each suburb.
    getColor <- function(suburb_data) 
    {
        sapply(suburb_data$Type, function(Type) {
            if(Type == "h") {
                "#FF8C00"
            } 
            else if(Type == "t") {
                "#0078ff"
            }
            else {
                "Red"
            } }) }
    
    #draw the leaflet map based on the filtered data, colours, view e.t.c.
    mapp %>% 
        setView(lng = 144.98, lat = -37.84 , zoom=11) %>% 
        addCircles(data=suburb_data, ~Longtitude, ~Lattitude,
                   radius, 
                   color = getColor(suburb_data),
                   fillOpacity = 1,
                   popup = ~paste("<b>","<h4>", as.character(Address), ',',Suburb,"</h4>"), 
                   label = ~paste0('$', as.character(Price)),
                   labelOptions = labelOptions(noHide = F,style =list("font-size" = "18px"))) %>% 
        leaflet::addLegend("topright", colors = c("#FF8C00", "#0078ff", "Red"), labels = c("House", "Townhouse", "Unit"), title = "Type of Property") %>%
        fitBounds(lng1 = max(suburb_data$Longtitude),lat1 = max(suburb_data$Lattitude)+0.001,
                  lng2 = min(suburb_data$Longtitude)+0.001,lat2 = min(suburb_data$Lattitude))
    
})

#creating a dygraph for a selected suburb
output$suburb_dygraph <- renderDygraph({
    #if no selection of suburb is made, reservoir is selected.
    if(is.null(input$suburb))
        data <- melbourne_housing_data %>% 
            filter(Suburb == c('Reservoir'))
    else
        data <- melbourne_housing_data %>% 
            filter(Suburb == as.character(input$suburb))
    
    priceTrend <- data %>% 
        group_by(Date, Date) %>% 
        summarize_at(vars(Price), funs(median(., na.rm=TRUE))) %>% 
        mutate(Date = as.Date(.$Date, '%d-%m-%Y')) %>% 
        xts(order.by = .$Date)
    
    # converting to the date object.
    dygraph(priceTrend,paste('Time Series graph for', toString(ifelse(is.null(input$suburb), toString(c('Reservoir')), toString(input$suburb))))) %>% 
        dySeries("Price", label = "Price") %>%
        dyOptions(drawPoints = TRUE, strokeWidth = 3) %>% 
        dyRangeSelector()
})

#creating a donut chart for the selected price range through price slider.
output$donut_chart <- renderPlotly({
    melbourne_housing_datahouse <- na.omit(melbourne_housing_data)
    data <- melbourne_housing_datahouse %>% 
        filter(between(Price, input$slider[1], input$slider[2])) %>% 
        group_by(Type) %>% 
        summarize(count=n()) 
    data$Type <- as.character(data$Type)
    data$Type[data$Type == "u"] <-"Unit"
    data$Type[data$Type == "t"] <-"Townhall"
    data$Type[data$Type == "h"] <-"House"
    data %>% 
        plot_ly(labels = ~Type, values = ~count,
                marker = list(colors = c("#00AFBB", "Yellow", "#FC4E07")), opacity = 1) %>%
        add_pie(hole = 0.8) %>%
        layout(title = "Donut graph showing the type of properties",showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
               yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE)) 
    
}) 

#creating a high chart to show which kind of rooms are present more , in the selected price range.
output$rooms_high_chart <- renderHighchart({
    data <- melbourne_housing_data %>% 
        filter(between(Price, input$slider[1], input$slider[2])) %>% 
        mutate(Rooms = as.character(Rooms)) %>% 
        filter(Rooms %in% c('0', '1', '2', '3', '4', '5', '6')) %>% 
        group_by(Rooms) %>% 
        summarize(count=n())
    highchart() %>% 
        hc_title(text = "The bar chart showing the no.of bed rooms present in the selected price range") %>%
        hc_subtitle(text = paste0("Price range from ", input$slider[1],' to ', input$slider[2] )) %>% 
        hc_add_series(data$count, type = "column",name = " ", colorByPoint = FALSE, center = c('35%', '10%'),
                      size = 50,tooltip = list(pointFormat = "property with number of rooms: {point.y}")) %>%
      hc_yAxis(title = list(text = 'number of property'),
                 categories = data$count) %>% 
    hc_xAxis(title = list(text = 'Number of Rooms'),
             categories = data$Rooms)
})

#creating a circular bar chart to show which region has more number of properties present based on the selected price range
output$circular_bar_plot_region <- renderPlot({
    data <- melbourne_housing_data %>% 
        filter(between(Price, input$slider[1], input$slider[2])) %>% 
        group_by(Regionname) %>% 
        summarize(count=n())
    data$pos <- c(1,2,3,4,7,8,5,6)
    ggplot(data, aes(x = reorder(Regionname,pos), y = count, fill = Regionname)) + geom_line()+
        geom_col(position = "dodge") +
        coord_polar(start = 4) +
        xlab('Regionname')+
        scale_fill_manual(values = c("Northern Metropolitan" = "blue",
                                     "Northern Victoria" = "yellow",
                                     "Eastern Metropolitan" = "orange",
                                     "Eastern Victoria" = "red",
                                     "South-Eastern Metropolitan" = "green",
                                     "Southern Metropolitan" = "#800000",
                                     "Western Metropolitan" = "Navy",
                                     "Western Victoria" = "dark slate gray")) +
        ggtitle("Regions Proportions in which the selected property is present") +
        theme(plot.title = element_text(size = 10, face = "bold")) 
})

}
shinyApp(ui, server)