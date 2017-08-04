#List of required packages: UNCOMMENT and RUN for first time execution
# install.packages("devtools")
# library(devtools)
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("RColorBrewer")
# install.packages("rgdal")
# install.packages("leaflet")
# install.packages("tidyverse")
# install.packages("treemap")
# install.packages("d3Tree")
# install_github("timelyportfolio/d3treeR")
# install.packages("fmsb")
# install.packages("plotly")
# install_github("timelyportfolio/parcoords")
# install.packages("geofacet")
# install.packages("Hmisc")
# install.packages("formattable")
# install.packages("reshape2")

require(shiny)
require(shinydashboard)
require(RColorBrewer)
require(rgdal)
require(leaflet)
require(tidyverse)
require(treemap)
require(d3Tree)
require(d3treeR)
require(fmsb)
require(plotly)
require(parcoords)
require(geofacet)
require(Hmisc)
require(formattable)
require(reshape2)


#---------------------------------------Data Preparation & Global Variable Creation----------------------------------------

#Set working directories, read datasets & add viz-based columns
# setwd("C:/Users/Angad Srivastava/Desktop/VP/GlobalWarmingApp")


#Read the datasets into the application
geofacet.Dataset <- read.csv("Datasets/TemperatureForecastPlot.csv")
consolidated.Dataset <- read.csv("Datasets/FinalStandardisedData.csv")
consolidated.Dataset$Abs.Average.Annual.Temperature <- abs(consolidated.Dataset$Average.Annual.Temperature)



#Tooltip for Electricity Bubble Plot
consolidated.Dataset$ToolTip.Text.Electricity=paste("Country: ",consolidated.Dataset$Country,"\n",
                                        "Temperature: " , format(round(consolidated.Dataset$Average.Annual.Temperature,2),big.mark=",",scientific=FALSE),"\n",
                                        "Electricity consumption (kwH per capita): ", format(round(consolidated.Dataset$Electric.Power.kWh.per.Capita,2),big.mark=",",scientific=FALSE),"\n",
                                        "Greenhouse gas emissions (kt of CO2): ", format(round(consolidated.Dataset$Greenhouse.Gases.KT.CO2,2),big.mark=",",scientific=FALSE),sep="")

#Tooltip for Forest Bubble Plot
consolidated.Dataset$ToolTip.Text.Forest=paste("Country: ",consolidated.Dataset$Country,"\n",
                                                    "Temperature: " , format(round(consolidated.Dataset$Average.Annual.Temperature,2),big.mark=",",scientific=FALSE),"\n",
                                                    "Forest Cover (% of total land area): ", format(round(consolidated.Dataset$Forest.Area.Percentage,2),big.mark=",",scientific=FALSE),"\n",
                                                    "Greenhouse gas emissions (kt of CO2): ", format(round(consolidated.Dataset$Greenhouse.Gases.KT.CO2,2),big.mark=",",scientific=FALSE),sep="")

#Tooltip for Fossil Fuel Bubble Plot
consolidated.Dataset$ToolTip.Text.Fossil=paste("Country: ",consolidated.Dataset$Country,"\n",
                                               "Temperature: " , format(round(consolidated.Dataset$Average.Annual.Temperature,2),big.mark=",",scientific=FALSE),"\n",
                                               "Fossil Fuel Consumption (% of energy): ", format(round(consolidated.Dataset$Fossil.Fuel.Percentage,2),big.mark=",",scientific=FALSE),"\n",
                                               "Greenhouse gas emissions (kt of CO2): ", format(round(consolidated.Dataset$Greenhouse.Gases.KT.CO2,2),big.mark=",",scientific=FALSE),sep="")

#Tooltip for Population Bubble Plot
consolidated.Dataset$ToolTip.Text.Population=paste("Country: ",consolidated.Dataset$Country,"\n",
                                               "Temperature: " , format(round(consolidated.Dataset$Average.Annual.Temperature,2),big.mark=",",scientific=FALSE),"\n",
                                               "Total population: ", format(round(consolidated.Dataset$Total.Population,0),big.mark=",",scientific=FALSE),"\n",
                                               "Greenhouse gas emissions (kt of CO2): ", format(round(consolidated.Dataset$Greenhouse.Gases.KT.CO2,2),big.mark=",",scientific=FALSE),sep="")

#Tooltip for Renewable Energy Bubble Plot
consolidated.Dataset$ToolTip.Text.Renewable=paste("Country: ",consolidated.Dataset$Country,"\n",
                                               "Temperature: " , format(round(consolidated.Dataset$Average.Annual.Temperature,2),big.mark=",",scientific=FALSE),"\n",
                                               "Renewable energy adoption (% of energy): ", format(round(consolidated.Dataset$Renewable.Percentage,2),big.mark=",",scientific=FALSE),"\n",
                                               "Greenhouse gas emissions (kt of CO2): ", format(round(consolidated.Dataset$Greenhouse.Gases.KT.CO2,2),big.mark=",",scientific=FALSE),sep="")



#Continent specific geofacet forecast tables
americasgeofacetdata <- subset(geofacet.Dataset, Continent == "Americas")
europegeofacetdata <- subset(geofacet.Dataset, Continent == "Europe")
africageofacetdata <- subset(geofacet.Dataset, Continent == "Africa")
asiageofacetdata <- subset(geofacet.Dataset, Continent == "Asia")
oceaniageofacetdata <- subset(geofacet.Dataset, Continent == "Oceania")

#Continent specific tables
americasconsolidated <- subset(consolidated.Dataset, Continent == "Americas")
europeconsolidated <- subset(consolidated.Dataset, Continent == "Europe")
africaconsolidated <- subset(consolidated.Dataset, Continent == "Africa")
asiaconsolidated <- subset(consolidated.Dataset, Continent == "Asia")
oceaniaconsolidated <- subset(consolidated.Dataset, Continent == "Oceania")


#Grids for Geofacet Plots
americasgrid <- data.frame(
  code = c("CAN", "USA", "CUB", "DOM", "MEX", "GTM", "HND", "JAM", "SLV", "NIC", "CRI", "PAN", "COL", "BRA", "ECU", "BOL", "PER", "CHL", "PRY", "URY", "ARG"),
  name = c("Canada", "United States", "Cuba", "Dominican Republic", "Mexico", "Guatemala", "Honduras", "Jamaica", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Colombia", "Brazil", "Ecuador", "Bolivia", "Peru", "Chile", "Paraguay", "Uruguay", "Argentina"),
  row = c(1, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 7, 8, 9, 9, 10, 10, 11, 11, 11, 13),
  col = c(1, 1, 3, 5, 2, 2, 3, 4, 2, 3, 3, 4, 5, 7, 4, 5, 4, 4, 5, 6, 5),
  stringsAsFactors = FALSE
)

europegrid <- data.frame(
  code = c("FIN", "ISL", "IRL", "NOR", "SWE", "DNK", "EST", "GBR", "BLR", "DEU", "LVA", "NLD", "POL", "CZE", "FRA", "LTU", "CHE", "UKR", "AUT", "HUN", "ROU", "ESP", "BGR", "HRV", "ITA", "PRT", "ALB", "GRC"),
  name = c("Finland", "Iceland", "Ireland", "Norway", "Sweden", "Denmark", "Estonia", "United Kingdom", "Belarus", "Germany", "Latvia", "Netherlands", "Poland", "Czech Republic", "France", "Lithuania", "Switzerland", "Ukraine", "Austria", "Hungary", "Romania", "Spain", "Bulgaria", "Croatia", "Italy", "Portugal", "Albania", "Greece"),
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
  col = c(7, 1, 2, 5, 6, 4, 7, 2, 8, 5, 7, 4, 6, 5, 3, 7, 4, 8, 5, 6, 7, 2, 7, 6, 4, 1, 6, 7),
  stringsAsFactors = FALSE
)

africagrid <- data.frame(
  code = c("MAR", "TUN", "SEN", "SDN", "BEN", "GHA", "NGA", "TGO", "GAB", "KEN", "AGO", "ZMB", "MOZ", "ZWE"),
  name = c("Morocco", "Tunisia", "Senegal", "Sudan", "Benin", "Ghana", "Nigeria", "Togo", "Gabon", "Kenya", "Angola", "Zambia", "Mozambique", "Zimbabwe"),
  row = c(1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6),
  col = c(2, 5, 1, 8, 4, 2, 5, 3, 5, 8, 5, 6, 8, 7),
  stringsAsFactors = FALSE
)

asiagrid <- data.frame(
  code = c("GEO", "JPN", "KAZ", "ARM", "TUR", "UZB", "IRQ", "ISR", "TJK", "CHN", "JOR", "IND", "NPL", "PAK", "BGD", "VNM", "IDN", "PHL", "LKA", "THA", "MYS"),
  name = c("Georgia", "Japan", "Kazakhstan", "Armenia", "Turkey", "Uzbekistan", "Iraq", "Israel", "Tajikistan", "China", "Jordan", "India", "Nepal", "Pakistan", "Bangladesh", "Vietnam", "Indonesia", "Philippines", "Sri Lanka", "Thailand", "Malaysia"),
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 7, 8),
  col = c(2, 10, 4, 3, 2, 4, 3, 1, 4, 8, 2, 5, 6, 4, 7, 9, 10, 9, 6, 8, 8),
  stringsAsFactors = FALSE
)

oceaniagrid <- data.frame(
  code = c("AUS", "NZL"),
  name = c("Australia", "New Zealand"),
  row = c(1, 1),
  col = c(1, 2),
  stringsAsFactors = FALSE
)


#Setting global label variables
label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]) <- "Electricity Consumption"
label(consolidated.Dataset[["Forest.Area.Percentage"]]) <- "Forest Cover"
label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]) <- "Fossil Fuel Consumption"
label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]) <- "Greenhouse Emissions"
label(consolidated.Dataset[["Total.Population"]]) <- "Population"
label(consolidated.Dataset[["Renewable.Percentage"]]) <- "Renewable Energy Adoption"


#Description Text variables
lbl.IntroPageText = "The purpose of this dashboard is to provide an application interface which facilitates an interactive analysis of the rise in global temperatures and its potential influencers which include <i>Electricity Consumption, Forest Cover, Fossil Fuel Consumption, Population</i> and <i>Renewable Energy Adoption</i>.  
Users are encouraged to interact with dynamic visualizations contained in this application and analytically determine the potential influencers and effects of global warming at any geographical level such as continent or country. 
<br/><br/>
Some potential areas of exploration are:
<br/><ul>
<li>Global overview of changes in temperature using a Chloropleth world map.</li>
<li>Continental analysis of change in temperature trends from 1990 to 2022 using the Geofacet plot.</li>
<li>Country-level overview of the relationship between Average temperature and its influencers using a Parallel Coordinates Plot.</li> 
<li>Interactive Treemap and reactive Bubble plots to deterministically analyse the relationship with between Average temperature of a country and its influencer over a 22 year time period.</li> 
</ul><br/>
<a href='https://wiki.smu.edu.sg/1617t3isss608g1/ADA_Overview' style='text-decoration:underline'>Please click on our Wikipedia page</a> 
for more information about the scope, usability, visualization creation or authors of this application."
lbl.WorldMapText = "<span style='font-size: 10pt;'><b>Slide</b> through each year to analyse the changes in average global temperature.</span>"
lbl.ParallelCoordText = "<span style='font-size: 10pt;'><b>Slide</b> through each year to analyse the relationship between the average temperature and its influencers.<br/> 
<b>Filter</b> the axis of each measure by brushing a section of any axis. </span>"
lbl.TreeBubbleText = "<span style='padding-top:5px;'><b>Click</b> on the treemap to filter the adjacent Bubble Plot for each continent/country.<br/>
Additionally, the slider below provides a yearly transition of all measured variables.<br/><br/></span>"
lbl.GeoFacet.Americas = "<span style='font-size: 10pt; padding-left:0px;'>The adjacent Geofacet plot shows an overview of the trends in Average temperature for countries in North America and South America. Temperatures from <b>2013</b> to <b>2022</b> have been forecasted based on annual readings from <b>1990</b> to <b>2012</b>.<br/>
The datatable below shows the deviation in average temperature to highlight the fluctuations and variations in temperatures.</span>"
lbl.GeoFacet.Europe = "<span style='font-size: 10pt; padding-left:0px;'>The adjacent Geofacet plot shows an overview of the trends in Average temperature for countries in Europe. Temperatures from <b>2013</b> to <b>2022</b> have been forecasted based on annual readings from <b>1990</b> to <b>2012</b>.<br/>
The datatable below shows the deviation in average temperature to highlight the fluctuations and variations in temperatures.</span>"
lbl.GeoFacet.Africa = "<span style='font-size: 10pt; padding-left:0px;'>The adjacent Geofacet plot shows an overview of the trends in Average temperature for countries in Africa. Temperatures from <b>2013</b> to <b>2022</b> have been forecasted based on annual readings from <b>1990</b> to <b>2012</b>.<br/>
The datatable below shows the deviation in average temperature to highlight the fluctuations and variations in temperatures.</span>"
lbl.GeoFacet.Asia = "<span style='font-size: 10pt; padding-left:0px;'>The adjacent Geofacet plot shows an overview of the trends in Average temperature for countries in Asia Temperatures from <b>2013</b> to <b>2022</b> have been forecasted based on annual readings from <b>1990</b> to <b>2012</b>.<br/>
The datatable below shows the deviation in average temperature to highlight the fluctuations and variations in temperatures.</span>"
lbl.GeoFacet.Oceania = "<span style='font-size: 10pt; padding-left:0px;'>The adjacent Geofacet plot shows an overview of the trends in Average temperature for countries in Oceania. Temperatures from <b>2013</b> to <b>2022</b> have been forecasted based on annual readings from <b>1990</b> to <b>2012</b>.<br/>
The datatable below shows the deviation in average temperature to highlight the fluctuations and variations in temperatures.</span>"



#Downloading shape files for Map
url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
tmp <- tempdir()
file <- basename(url)
download.file(url, file)
unzip("ne_50m_admin_0_countries.zip", exdir = tmp)
countries <- readOGR(dsn = tmp,
                     layer = "ne_50m_admin_0_countries",
                     encoding = "UTF-8",
                     verbose = FALSE)







#--------------------------------------------Application Code-------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Global Warming: An Analysis of Rise in Global Temperatures",
    titleWidth = 1100
  ),
  dashboardSidebar(
    sidebarMenu(width = 100,
                menuItem("Overview", icon = icon("home"), tabName = "tab_overview"),
                menuItem("Choropleth World Map", icon = icon("globe"), tabName = "tab_choroplethmap"),
                menuItem("Continental Geofacet Maps", icon = icon("map"), tabName = "tab_geofacetmap"),
                menuItem("Country-level Relational Synopsis", icon = icon("line-chart"), tabName = "tab_parallelplot"),
                menuItem("Country-level Trends Analysis", icon = icon("table"), tabName = "tab_treemapplot_bubbleplot"),
                menuItem("About Us", icon = icon("external-link"), href = "https://wiki.smu.edu.sg/1617t3isss608g1/ADA_Overview")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("tab_overview",
              fluidRow(br(),
                valueBox("5 continents", icon = icon("globe"), subtitle = "Analyse global temperatures and its causal factors across Africa, Americas, Europe, Asia and Oceania", color = "light-blue", width = 4),
                valueBox("86 countries", icon = icon("flag-o"), subtitle = "Inspect temperature changes for 86 countries including USA, UK, China, Thailand, Nepal, India and France", color = "red", width = 4),
                valueBox("Years: 1990 - 2022", icon = icon("calendar"), subtitle = "Visualize changes in temperature and its causal factors from 1990 - 2022 based on actual and forecasted values for over 30 years", color = "light-blue", width = 4)
              ),
              mainPanel(HTML(lbl.IntroPageText)
                        , width = 12, style = "font-size: 12pt; padding-left: 0; text-align:left;")
      ),
      tabItem("tab_choroplethmap",
              h2("Choropleth World Map", style="text-align:center;"),
              fluidRow(
                box(
                  width=12,
                  solidHeader = TRUE,
                  leafletOutput("op_WorldMap")
                )
              ),
              fluidRow(
                column(1),
                column(10,
                       sliderInput(inputId = "inp_choropleth_year",
                                   label = "Select a Year:",
                                   min = min(consolidated.Dataset$Year),
                                   max = max(consolidated.Dataset$Year),
                                   value=min(consolidated.Dataset$Year),
                                   step = 1,
                                   width='100%',
                                   ticks=FALSE,
                                   sep=""
                       )
                )
              ),
              mainPanel(HTML(lbl.WorldMapText), width = 12, style = "font-size: 12pt; padding-left: 0; text-align:center;")
      ),
      tabItem("tab_geofacetmap",
              h2("Continental Geofacet Map", style="text-align:center;"),
              fluidRow(
                tabBox(
                  width=12, height = 1250,
                  tabPanel("Americas",
                           fluidRow(
                             width = 12, 
                             column(7,plotOutput("op_GeofacetPlotAmericas")),
                             column(5,HTML(lbl.GeoFacet.Americas), br(), br(), tableOutput("op_TableAmericas"))
                           )),
                  tabPanel("Europe",
                           fluidRow(
                             width = 12,
                             column(7,plotOutput("op_GeofacetPlotEurope")),
                             column(5,HTML(lbl.GeoFacet.Europe), br(), br(), tableOutput("op_TableEurope"))
                           )),
                  tabPanel("Africa",
                           fluidRow(
                             width = 12,
                             column(7,plotOutput("op_GeofacetPlotAfrica")),
                             column(5,HTML(lbl.GeoFacet.Africa), br(), br(), tableOutput("op_TableAfrica"))
                           )),
                  tabPanel("Asia",
                           fluidRow(
                             width = 12,
                             column(7,plotOutput("op_GeofacetPlotAsia")),
                             column(5,HTML(lbl.GeoFacet.Asia), br(), br(), tableOutput("op_TableAsia"))
                           )),
                  tabPanel("Oceania",
                           fluidRow(
                             width = 12,
                             column(7,plotOutput("op_GeofacetPlotOceania")),
                             column(5,HTML(lbl.GeoFacet.Oceania), br(), br(), tableOutput("op_TableOceania"))
                           ))
                )
              )
      ),
      tabItem("tab_parallelplot",
              h2("Country-level Relational Synopsis: Parallel Coordinates Plot", style="text-align:center;"),
              fluidRow(
                tabBox(
                  width=12, height=520,
                  tabPanel("Americas",parcoordsOutput("op_ParallelCoordPlotAmericas")),
                  tabPanel("Europe",parcoordsOutput("op_ParallelCoordPlotEurope")),
                  tabPanel("Africa",parcoordsOutput("op_ParallelCoordPlotAfrica")),
                  tabPanel("Asia",parcoordsOutput("op_ParallelCoordPlotAsia")),
                  tabPanel("Oceania",parcoordsOutput("op_ParallelCoordPlotOceania"))
                )
              ),
              fluidRow(
                column(1),
                column(10,
                       sliderInput(inputId = "inp_parallel_year",
                                   label = "Select a Year:",
                                   min = min(consolidated.Dataset$Year),
                                   max = max(consolidated.Dataset$Year),
                                   value=min(consolidated.Dataset$Year),
                                   step = 1,
                                   width='100%',
                                   ticks=FALSE,
                                   sep=""
                       )
                )
              ),
              fluidRow(
                mainPanel(HTML(lbl.ParallelCoordText), width = 12, style = "font-size: 12pt; padding-left: 0; text-align:center;")
              )
      ),
      tabItem("tab_treemapplot_bubbleplot",
              h2("Country-level Trends Analysis", style="text-align:center;"),
              fluidRow(
                column(
                  width = 6,
                  height = 600,
                  mainPanel(HTML(lbl.TreeBubbleText), width=12, style = "font-size: 10pt; padding-left: 0;"),
                  d3tree2Output("op_tree")
                ),
                tabBox(id = "tab_BubblePlots",
                  width=6,
                  height = 500,
                  tabPanel("Electricity",plotlyOutput("op_BubblePlotElectricity")),
                  tabPanel("Forest Cover",plotlyOutput("op_BubblePlotForest")),
                  tabPanel("Fossil Fuel",plotlyOutput("op_BubblePlotFossil")),
                  tabPanel("Population",plotlyOutput("op_BubblePlotPopulation")),
                  tabPanel("Renewable Energy",plotlyOutput("op_BubblePlotRenewable"))
                )
              ),
              fluidRow(
                column(1),
                column(10,
                       sliderInput(inputId = "inp_treemap_bubbleplot_year",
                                   label = "Select a Year:",
                                   min = min(consolidated.Dataset$Year),
                                   max = max(consolidated.Dataset$Year),
                                   value=min(consolidated.Dataset$Year),
                                   step = 1,
                                   width='100%',
                                   ticks=FALSE,
                                   sep=""
                       )
                )
              )
      )
    )
  )
)

server <- function( input, output, session ){
  
  #WORLD CHOROPLETH MAP RENDERING - Subset data based on year:
  selected.WorldMapData <- reactive(subset(consolidated.Dataset[c("Year", "Country", "Country.Code", "Average.Annual.Temperature")], Year == input$inp_choropleth_year))
  
  #Code for rendering world choropleth map
  output$op_WorldMap <- renderLeaflet({
    #reference: https://rpubs.com/walkerke/wdi_legend
    mapped.Countries <- merge(countries, 
                              selected.WorldMapData(),
                              by.x = "iso_a3", 
                              by.y = "Country.Code",                    
                              sort = FALSE)
    pal <- colorQuantile("Reds", NULL, n = 9)
    country_popup <- paste0("<strong>Country: </strong>", 
                            mapped.Countries$Country,
                            "<br><strong>Temperature: </strong>",
                            format(round(mapped.Countries$Average.Annual.Temperature, 2), nsmall = 2))
    
    leaflet(data = mapped.Countries) %>%
      setView(0, 0, zoom = 2) %>%
      addPolygons(fillColor = ~pal(mapped.Countries[["Average.Annual.Temperature"]]), 
                  fillOpacity = 0.8, 
                  color = "#e3e3e3", 
                  weight = 1, 
                  popup = country_popup)
  })
  
  
  
  
  
  #GEO FACET PLOTS:
  #Code for rendering Geofacet Plot - Americas
  output$op_GeofacetPlotAmericas <- renderPlot({
    ggplot(americasgeofacetdata,aes(Year, AverageAnnualTemperature)) +
      geom_line(color="Blue") +
      geom_ribbon(aes(x = Year, ymin = Lo.95, ymax = Hi.95), fill = "red", alpha = 0.3) +
      facet_geo(~name,grid =americasgrid,label ="name") +
      scale_x_continuous(labels = function(x) paste0("'",substr(x,3,4))) +
      labs(x = "Years 1990-2022",y = "Temperature") 
  }, height = 1000)
  
  #Code for rendering Geofacet Plot - Europe
  output$op_GeofacetPlotEurope <- renderPlot({
    ggplot(europegeofacetdata,aes(Year, AverageAnnualTemperature)) +
      geom_line(color="Blue") +
      geom_ribbon(aes(x = Year, ymin = Lo.95, ymax = Hi.95), fill = "red", alpha = 0.3) +
      facet_geo(~name,grid =europegrid,label ="name") +
      scale_x_continuous(labels = function(x) paste0("'",substr(x,3,4))) +
      labs(x = "Years 1990-2022",y = "Temperature") 
  }, height = 700)
  
  #Code for rendering Geofacet Plot - Africa
  output$op_GeofacetPlotAfrica <- renderPlot({
    ggplot(africageofacetdata,aes(Year, AverageAnnualTemperature)) +
      geom_line(color="Blue") +
      geom_ribbon(aes(x = Year, ymin = Lo.95, ymax = Hi.95), fill = "red", alpha = 0.3) +
      facet_geo(~name,grid =africagrid,label ="name") +
      scale_x_continuous(labels = function(x) paste0("'",substr(x,3,4))) +
      labs(x = "Years 1990-2022",y = "Temperature") 
  }, height = 600)
  
  #Code for rendering Geofacet Plot - Asia
  output$op_GeofacetPlotAsia <- renderPlot({
    ggplot(asiageofacetdata,aes(Year, AverageAnnualTemperature)) +
      geom_line(color="Blue") +
      geom_ribbon(aes(x = Year, ymin = Lo.95, ymax = Hi.95), fill = "red", alpha = 0.3) +
      facet_geo(~name,grid =asiagrid,label ="name") +
      scale_x_continuous(labels = function(x) paste0("'",substr(x,3,4))) +
      labs(x = "Years 1990-2022",y = "Temperature") 
  }, height = 600)
  
  #Code for rendering Geofacet Plot - Oceania
  output$op_GeofacetPlotOceania <- renderPlot({
    ggplot(oceaniageofacetdata,aes(Year, AverageAnnualTemperature)) +
      geom_line(color="Blue") +
      geom_ribbon(aes(x = Year, ymin = Lo.95, ymax = Hi.95), fill = "red", alpha = 0.3) +
      facet_geo(~name,grid =oceaniagrid,label ="name") +
      scale_x_continuous(labels = function(x) paste0("'",substr(x,3,4))) +
      labs(x = "Years 1990-2022",y = "Temperature") 
  }, height = 400)
  
  
  
  
  
  
  #DATA TABLES FOR GEOFACET PLOTS:
  #Code for rendering Datatable for Americas
  output$op_TableAmericas <- renderTable({
    formattable({
      transposedtable <-  subset(consolidated.Dataset, Continent == "Americas")[c("Country", "Year", "Average.Annual.Temperature")]
      transposedtable <-  as.data.frame(acast(transposedtable, Country ~ Year, value.var = "Average.Annual.Temperature"))
      transposedtable$`Minimum Temperature` <- apply(transposedtable[, 2:33], 1, min)
      transposedtable$`Maximum Temperature` <- apply(transposedtable[, 2:33], 1, max)
      transposedtable$`Temperature Deviation` <-   transposedtable$`Maximum Temperature` - transposedtable$`Minimum Temperature`
      transposedtable$`Minimum Temperature` <- format(round(transposedtable$`Minimum Temperature`, 2), nsmall = 2)
      transposedtable$`Maximum Temperature` <- format(round(transposedtable$`Maximum Temperature`, 2), nsmall = 2)
      transposedtable$`Temperature Deviation` <-   format(round(transposedtable$`Temperature Deviation`, 2), nsmall = 2)
      transposedtable[c("Minimum Temperature","Maximum Temperature","Temperature Deviation")]
    })
  }, bordered = TRUE, spacing = c("xs"), rownames = TRUE)
  
  #Code for rendering Datatable for Europe
  output$op_TableEurope <- renderTable({
    formattable({
      transposedtable <-  subset(consolidated.Dataset, Continent == "Europe")[c("Country", "Year", "Average.Annual.Temperature")]
      transposedtable <-  as.data.frame(acast(transposedtable, Country ~ Year, value.var = "Average.Annual.Temperature"))
      transposedtable$`Minimum Temperature` <- apply(transposedtable[, 2:33], 1, min)
      transposedtable$`Maximum Temperature` <- apply(transposedtable[, 2:33], 1, max)
      transposedtable$`Temperature Deviation` <-   transposedtable$`Maximum Temperature` - transposedtable$`Minimum Temperature`
      transposedtable$`Minimum Temperature` <- format(round(transposedtable$`Minimum Temperature`, 2), nsmall = 2)
      transposedtable$`Maximum Temperature` <- format(round(transposedtable$`Maximum Temperature`, 2), nsmall = 2)
      transposedtable$`Temperature Deviation` <-   format(round(transposedtable$`Temperature Deviation`, 2), nsmall = 2)
      transposedtable[c("Minimum Temperature","Maximum Temperature","Temperature Deviation")]
    })
  }, bordered = TRUE, spacing = c("xs"), rownames = TRUE)
  
  #Code for rendering Datatable for Africa
  output$op_TableAfrica <- renderTable({
    formattable({
      transposedtable <-  subset(consolidated.Dataset, Continent == "Africa")[c("Country", "Year", "Average.Annual.Temperature")]
      transposedtable <-  as.data.frame(acast(transposedtable, Country ~ Year, value.var = "Average.Annual.Temperature"))
      transposedtable$`Minimum Temperature` <- apply(transposedtable[, 2:33], 1, min)
      transposedtable$`Maximum Temperature` <- apply(transposedtable[, 2:33], 1, max)
      transposedtable$`Temperature Deviation` <-   transposedtable$`Maximum Temperature` - transposedtable$`Minimum Temperature`
      transposedtable$`Minimum Temperature` <- format(round(transposedtable$`Minimum Temperature`, 2), nsmall = 2)
      transposedtable$`Maximum Temperature` <- format(round(transposedtable$`Maximum Temperature`, 2), nsmall = 2)
      transposedtable$`Temperature Deviation` <-   format(round(transposedtable$`Temperature Deviation`, 2), nsmall = 2)
      transposedtable[c("Minimum Temperature","Maximum Temperature","Temperature Deviation")]
    })
  }, bordered = TRUE, spacing = c("xs"), rownames = TRUE)

  #Code for rendering Datatable for Asia
  output$op_TableAsia <- renderTable({
    formattable({
      transposedtable <-  subset(consolidated.Dataset, Continent == "Asia")[c("Country", "Year", "Average.Annual.Temperature")]
      transposedtable <-  as.data.frame(acast(transposedtable, Country ~ Year, value.var = "Average.Annual.Temperature"))
      transposedtable$`Minimum Temperature` <- apply(transposedtable[, 2:33], 1, min)
      transposedtable$`Maximum Temperature` <- apply(transposedtable[, 2:33], 1, max)
      transposedtable$`Temperature Deviation` <-   transposedtable$`Maximum Temperature` - transposedtable$`Minimum Temperature`
      transposedtable$`Minimum Temperature` <- format(round(transposedtable$`Minimum Temperature`, 2), nsmall = 2)
      transposedtable$`Maximum Temperature` <- format(round(transposedtable$`Maximum Temperature`, 2), nsmall = 2)
      transposedtable$`Temperature Deviation` <-   format(round(transposedtable$`Temperature Deviation`, 2), nsmall = 2)
      transposedtable[c("Minimum Temperature","Maximum Temperature","Temperature Deviation")]
    })
  }, bordered = TRUE, spacing = c("xs"), rownames = TRUE)  
  
  #Code for rendering Datatable for Oceania
  output$op_TableOceania <- renderTable({
    formattable({
      transposedtable <-  subset(consolidated.Dataset, Continent == "Oceania")[c("Country", "Year", "Average.Annual.Temperature")]
      transposedtable <-  as.data.frame(acast(transposedtable, Country ~ Year, value.var = "Average.Annual.Temperature"))
      transposedtable$`Minimum Temperature` <- apply(transposedtable[, 2:33], 1, min)
      transposedtable$`Maximum Temperature` <- apply(transposedtable[, 2:33], 1, max)
      transposedtable$`Temperature Deviation` <-   transposedtable$`Maximum Temperature` - transposedtable$`Minimum Temperature`
      transposedtable$`Minimum Temperature` <- format(round(transposedtable$`Minimum Temperature`, 2), nsmall = 2)
      transposedtable$`Maximum Temperature` <- format(round(transposedtable$`Maximum Temperature`, 2), nsmall = 2)
      transposedtable$`Temperature Deviation` <-   format(round(transposedtable$`Temperature Deviation`, 2), nsmall = 2)
      transposedtable[c("Minimum Temperature","Maximum Temperature","Temperature Deviation")]
    })
  }, bordered = TRUE, spacing = c("xs"), rownames = TRUE)  
  
  
  
  
  
  
  #PARALLEL PLOT RENDERING - Subset data based on year:
  #Americas year subset
  selected.ParallelCoordDataAmericas <- reactive({
    tempData <- subset(americasconsolidated, Year == input$inp_parallel_year)[c("Country","Average.Annual.Temperature","Electric.Power.kWh.per.Capita",
                                                                                "Forest.Area.Percentage","Fossil.Fuel.Percentage","Greenhouse.Gases.KT.CO2",
                                                                                "Total.Population","Renewable.Percentage")]
    colnames(tempData) = c("Country","Temperature",
                           label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]),
                           label(consolidated.Dataset[["Forest.Area.Percentage"]]),
                           label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]),
                           label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]),
                           label(consolidated.Dataset[["Total.Population"]]),
                           label(consolidated.Dataset[["Renewable.Percentage"]]))
    tempData
  })
  
  #Europe year subset
  selected.ParallelCoordDataEurope <- reactive({
    tempData <- subset(europeconsolidated, Year == input$inp_parallel_year)[c("Country","Average.Annual.Temperature","Electric.Power.kWh.per.Capita",
                                                                              "Forest.Area.Percentage","Fossil.Fuel.Percentage","Greenhouse.Gases.KT.CO2",
                                                                              "Total.Population","Renewable.Percentage")]
    colnames(tempData) = c("Country","Temperature",
                           label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]),
                           label(consolidated.Dataset[["Forest.Area.Percentage"]]),
                           label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]),
                           label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]),
                           label(consolidated.Dataset[["Total.Population"]]),
                           label(consolidated.Dataset[["Renewable.Percentage"]]))
    tempData
  })
  
  #Africa year subset
  selected.ParallelCoordDataAfrica <- reactive({
    tempData <- subset(africaconsolidated, Year == input$inp_parallel_year)[c("Country","Average.Annual.Temperature","Electric.Power.kWh.per.Capita",
                                                                              "Forest.Area.Percentage","Fossil.Fuel.Percentage","Greenhouse.Gases.KT.CO2",
                                                                              "Total.Population","Renewable.Percentage")]
    colnames(tempData) = c("Country","Temperature",
                           label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]),
                           label(consolidated.Dataset[["Forest.Area.Percentage"]]),
                           label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]),
                           label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]),
                           label(consolidated.Dataset[["Total.Population"]]),
                           label(consolidated.Dataset[["Renewable.Percentage"]]))
    tempData
  })
  
  #Asia year subset
  selected.ParallelCoordDataAsia <- reactive({
    tempData <- subset(asiaconsolidated, Year == input$inp_parallel_year)[c("Country","Average.Annual.Temperature","Electric.Power.kWh.per.Capita",
                                                                            "Forest.Area.Percentage","Fossil.Fuel.Percentage","Greenhouse.Gases.KT.CO2",
                                                                            "Total.Population","Renewable.Percentage")]
    colnames(tempData) = c("Country","Temperature",
                           label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]),
                           label(consolidated.Dataset[["Forest.Area.Percentage"]]),
                           label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]),
                           label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]),
                           label(consolidated.Dataset[["Total.Population"]]),
                           label(consolidated.Dataset[["Renewable.Percentage"]]))
    tempData
  })
  
  #Oceania year subset
  selected.ParallelCoordDataOceania <- reactive({
    tempData <- subset(oceaniaconsolidated, Year == input$inp_parallel_year)[c("Country","Average.Annual.Temperature","Electric.Power.kWh.per.Capita",
                                                                               "Forest.Area.Percentage","Fossil.Fuel.Percentage","Greenhouse.Gases.KT.CO2",
                                                                               "Total.Population","Renewable.Percentage")]
    colnames(tempData) = c("Country","Temperature",
                           label(consolidated.Dataset[["Electric.Power.kWh.per.Capita"]]),
                           label(consolidated.Dataset[["Forest.Area.Percentage"]]),
                           label(consolidated.Dataset[["Fossil.Fuel.Percentage"]]),
                           label(consolidated.Dataset[["Greenhouse.Gases.KT.CO2"]]),
                           label(consolidated.Dataset[["Total.Population"]]),
                           label(consolidated.Dataset[["Renewable.Percentage"]]))
    tempData
  })
  
  #Code for rendering Parallel Coord Plot - Americas
  output$op_ParallelCoordPlotAmericas <- renderParcoords({
    parcoords(rownames = F, selected.ParallelCoordDataAmericas(), brushMode = "1D-axes",color=list(colorBy="Country"), height=500, alphaOnBrushed=0.1,queue = TRUE)
  })
  
  #Code for rendering Parallel Coord Plot - Europe
  output$op_ParallelCoordPlotEurope <- renderParcoords({
    parcoords(rownames = F, selected.ParallelCoordDataEurope(), brushMode = "1D-axes",color=list(colorBy="Country"), height=500, alphaOnBrushed=0.1,queue = TRUE)
  })
  
  #Code for rendering Parallel Coord Plot - Africa
  output$op_ParallelCoordPlotAfrica <- renderParcoords({
    parcoords(rownames = F, selected.ParallelCoordDataAfrica(), brushMode = "1D-axes",color=list(colorBy="Country"), height=500, alphaOnBrushed=0.1,queue = TRUE)
  })
  
  #Code for rendering Parallel Coord Plot - Asia
  output$op_ParallelCoordPlotAsia <- renderParcoords({
    parcoords(rownames = F, selected.ParallelCoordDataAsia(), brushMode = "1D-axes",color=list(colorBy="Country"), height=500, alphaOnBrushed=0.1,queue = TRUE)
  })
  
  #Code for rendering Parallel Coord Plot - Asia
  output$op_ParallelCoordPlotOceania <- renderParcoords({
    parcoords(rownames = F, selected.ParallelCoordDataOceania(), brushMode = "1D-axes",color=list(colorBy="Country"), height=500, alphaOnBrushed=0.1,queue = TRUE)
  })
  
  
  
  
  
  #TREEMAP RENDERING:
  selected.TreeMapData <- reactive(subset(consolidated.Dataset, Year == input$inp_treemap_bubbleplot_year))

  output$op_tree <- renderD3tree2({
    rendered.Treemap <- treemap(
      selected.TreeMapData(),
      index=c("Continent","Country"),
      vSize="Abs.Average.Annual.Temperature",
      vColor="Average.Annual.Temperature",
      type="value",
      palette="Reds",
      fun.aggregate = "mean",
      position.legend = "none"
    )
    
    d3tree2(rendered.Treemap,rootname = "World")
  })
  
  
  
  
  
  
  #BUBBLE PLOT RENDERING - Subset data based on year & selection on TreeMap:
  #Code to read click on Treemap 
  treemap.Click <- reactiveValues(msg = "")
  observeEvent(input$op_tree_click, {
    treemap.Click$msg <- input$op_tree_click$name
  })
  
  #Selection of subset based on Treemap click
  selected.BubblePlotData <- reactive({
      if(treemap.Click$msg != "" & treemap.Click$msg != "World") {
        subset(consolidated.Dataset, Year == input$inp_treemap_bubbleplot_year
               & (Continent == treemap.Click$msg | Country == treemap.Click$msg))
      }
      else {
        subset(consolidated.Dataset, Year == input$inp_treemap_bubbleplot_year)
      }
    })
  
  #Code for rendering Bubble Plot - Electricity
  output$op_BubblePlotElectricity <- renderPlotly({
    x_mean<-mean(consolidated.Dataset$StandardisedElectric)
    y_mean<-mean(consolidated.Dataset$StandardisedTemp)

    plot_ly(selected.BubblePlotData(),x=selected.BubblePlotData()$StandardisedElectric,
            y=selected.BubblePlotData()$StandardisedTemp, mode="markers"
            ,color=as.factor(selected.BubblePlotData()$Country),text=selected.BubblePlotData()$ToolTip.Text.Electricity,
            hoverinfo="text",size= selected.BubblePlotData()$StandardisedGreenhouse,
            sizes=c(10,50),showlegend=FALSE) %>%

      layout(xaxis = list(title = "Electricity Consumption",showticklabels = FALSE,zeroline = FALSE, showline = FALSE,showgrid=FALSE),
             yaxis = list(title = "Temperature",zeroline = FALSE, showline = FALSE,showticklabels = FALSE,showgrid = FALSE ),
             shapes = list(list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,
                                x0 =x_mean, x1 =x_mean, y0=0,y1=1),
                           list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,x0 =0, x1 =1, y0=y_mean,y1=y_mean)))
  })
  
  #Code for rendering Bubble Plot - Forest
  output$op_BubblePlotForest <- renderPlotly({
    x_mean<-mean(consolidated.Dataset$StandardisedForest)
    y_mean<-mean(consolidated.Dataset$StandardisedTemp)
    
    plot_ly(selected.BubblePlotData(),x=selected.BubblePlotData()$StandardisedForest,
            y=selected.BubblePlotData()$StandardisedTemp, mode="markers"
            ,color=as.factor(selected.BubblePlotData()$Country),text=selected.BubblePlotData()$ToolTip.Text.Forest,
            hoverinfo="text",size= selected.BubblePlotData()$StandardisedGreenhouse,
            sizes=c(10,50),showlegend=FALSE) %>%
      
      layout(xaxis = list(title = "Forest Cover",showticklabels = FALSE,zeroline = FALSE, showline = FALSE,showgrid=FALSE),
             yaxis = list(title = "Temperature",zeroline = FALSE, showline = FALSE,showticklabels = FALSE,showgrid = FALSE ),
             shapes = list(list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,
                                x0 =x_mean, x1 =x_mean, y0=0,y1=1),
                           list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,x0 =0, x1 =1, y0=y_mean,y1=y_mean)))
  })
  
  #Code for rendering Bubble Plot - Fossil Fuel
  output$op_BubblePlotFossil <- renderPlotly({
    x_mean<-mean(consolidated.Dataset$StandardisedFossil)
    y_mean<-mean(consolidated.Dataset$StandardisedTemp)
    
    plot_ly(selected.BubblePlotData(),x=selected.BubblePlotData()$StandardisedFossil,
            y=selected.BubblePlotData()$StandardisedTemp, mode="markers"
            ,color=as.factor(selected.BubblePlotData()$Country),text=selected.BubblePlotData()$ToolTip.Text.Fossil,
            hoverinfo="text",size= selected.BubblePlotData()$StandardisedGreenhouse,
            sizes=c(10,50),showlegend=FALSE) %>%
      
      layout(xaxis = list(title = "Fossil Fuel Consumption",showticklabels = FALSE,zeroline = FALSE, showline = FALSE,showgrid=FALSE),
             yaxis = list(title = "Temperature",zeroline = FALSE, showline = FALSE,showticklabels = FALSE,showgrid = FALSE ),
             shapes = list(list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,
                                x0 =x_mean, x1 =x_mean, y0=0,y1=1),
                           list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,x0 =0, x1 =1, y0=y_mean,y1=y_mean)))
  })
  
  #Code for rendering Bubble Plot - Population
  output$op_BubblePlotPopulation <- renderPlotly({
    x_mean<-mean(consolidated.Dataset$StandardisedPopulation)
    y_mean<-mean(consolidated.Dataset$StandardisedTemp)
    
    plot_ly(selected.BubblePlotData(),x=selected.BubblePlotData()$StandardisedPopulation,
            y=selected.BubblePlotData()$StandardisedTemp, mode="markers"
            ,color=as.factor(selected.BubblePlotData()$Country),text=selected.BubblePlotData()$ToolTip.Text.Population,
            hoverinfo="text",size= selected.BubblePlotData()$StandardisedGreenhouse,
            sizes=c(10,50),showlegend=FALSE) %>%
      
      layout(xaxis = list(title = "Total Population",showticklabels = FALSE,zeroline = FALSE, showline = FALSE,showgrid=FALSE),
             yaxis = list(title = "Temperature",zeroline = FALSE, showline = FALSE,showticklabels = FALSE,showgrid = FALSE ),
             shapes = list(list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,
                                x0 =x_mean, x1 =x_mean, y0=0,y1=1),
                           list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,x0 =0, x1 =1, y0=y_mean,y1=y_mean)))
  })
  
  #Code for rendering Bubble Plot - Renewable
  output$op_BubblePlotRenewable <- renderPlotly({
    x_mean<-mean(consolidated.Dataset$StandardisedRenewable)
    y_mean<-mean(consolidated.Dataset$StandardisedTemp)
    
    plot_ly(selected.BubblePlotData(),x=selected.BubblePlotData()$StandardisedRenewable,
            y=selected.BubblePlotData()$StandardisedTemp, mode="markers"
            ,color=as.factor(selected.BubblePlotData()$Country),text=selected.BubblePlotData()$ToolTip.Text.Renewable,
            hoverinfo="text",size= selected.BubblePlotData()$StandardisedGreenhouse,
            sizes=c(10,50),showlegend=FALSE) %>%
      
      layout(xaxis = list(title = "Renewable Energy Adoption",showticklabels = FALSE,zeroline = FALSE, showline = FALSE,showgrid=FALSE),
             yaxis = list(title = "Temperature",zeroline = FALSE, showline = FALSE,showticklabels = FALSE,showgrid = FALSE ),
             shapes = list(list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,
                                x0 =x_mean, x1 =x_mean, y0=0,y1=1),
                           list(type = "line",fillcolor = "Red", line = list(color = "Red"), opacity = 0.3,x0 =0, x1 =1, y0=y_mean,y1=y_mean)))
  })
}

shinyApp( ui, server )