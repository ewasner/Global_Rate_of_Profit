## Libraries 
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(shiny)
library(shinyWidgets)
library(shinydashboard, warn.conflicts = FALSE)
library(readxl)

## Clear workplace
rm(list = ls())

## Set working directory - This is for Evan 
## Needs to be changed for server
## setwd("I:/Evan/Documents/Umass/RA - Deepankar Fall 2021/Global Rate of Profit/Global_Rate_of_Profit")

###################### Import Exchange Rates, PPP Indices, & Income Group Classes #############

## Exchange rate data
XR <- read_excel("Nominal_Exchange_Rates.xls",
                 sheet = "Data",
                 skip = 3) %>%
  pivot_longer("1960":"2020","year", values_to="value") %>%
  select(`Country Code`, year, value) %>%
  rename(countrycode = `Country Code`, XR = value)

## PPP Data
OECD_PPP <- read.csv("OECD_PPP.csv", fileEncoding = 'UTF-8-BOM') %>%
  select(LOCATION,TIME,Value) %>%
  rename(countrycode=LOCATION,year=TIME,PPP=Value)

## World Bank Income Group Classifications
class <- read.csv("dh_country_class.csv") %>% 
  select(countrycode,wb_income_group) 

###################### Collect & Manipulate Data From EPWT Spreadsheet ######################

## Read from .csv file
EPWT <- read_excel("EPWT 7.0 Preliminary.xlsx",
                  sheet = "EPWT7.0")

## Remove na values from EPWT, select relevant variables, and
## calculate Profit share (PS), Output-Capital Ratio (OCR), and Rate of Profit (ROP)
EPWT <- na.omit(EPWT %>% select(countrycode, country, year, LabShare, rhonatcur, Knatcur, XGDPnatcur) %>%
                 filter(year > 1949)) %>%
  rename(Y=XGDPnatcur, K=Knatcur, OCR=rhonatcur) %>%
  mutate(PS = 1-LabShare, Profit=PS*Y, ROP = 100 * PS * OCR)

## Merge EPWT with WB income group categories, PPPs, and exchange rates (XR)
EPWT <- merge(EPWT, class, all.x=TRUE) %>%
  merge(OECD_PPP, all.x=TRUE) %>%
  merge(XR, all.x=TRUE)


###################### Collect & Manipulate Data From WIOD Spreadsheet ######################

### List of variables and description

## GO	Gross output by industry at current basic prices (in millions of national currency)
## II	Intermediate inputs at current purchasers' prices (in millions of national currency)
## VA	Gross value added at current basic prices (in millions of national currency)
## EMP	Number of persons engaged (thousands)
## EMPE	Number of employees (thousands)
## H_EMPE	Total hours worked by employees (millions)
## COMP	Compensation of employees (in millions of national currency)
## LAB	Labour compensation (in millions of national currency)
## CAP	Capital compensation (in millions of national currency)
## K	Nominal capital stock (in millions of national currency)

### Prices	
## GO_PI	Price levels gross output, 2010=100
## II_PI	Price levels of intermediate inputs, 2010=100
## VA_PI	Price levels of gross value added, 2010=100

### Volumes	
## GO_QI	Gross output, volume indices, 2010=100
## II_QI	Intermediate inputs, volume indices, 2010=100
## VA_QI	Gross value added, volume indices, 2010=100


### OUR VARIABLES:
### Variable construction

## 2 measures of surplus value: CAP and (VA-LAB)
### Rate of profit_1= CAP/K = (Output-Capital ratio)(Profit share)=(VA/K)*(CAP/VA)
### Rate of profit_2= (VA-LAB)/K

### Rate of surplus value= CAP/LAB
### Organic composition of capital= K/LAB

## Import Socioeconomic accounts, include extended country names from Notes sheet
WIOD <- merge(read_excel("WIOD_SEA_Nov16.xlsx", 
                   sheet = "DATA") %>% 
                rename(countrycode=country) %>% 
                suppressWarnings(),
              read_excel("WIOD_SEA_Nov16.xlsx", 
                         sheet = "Notes",
                         skip=6)[,1:2] %>% 
                rename(countrycode=Acronym, country=Name) %>% 
                suppressMessages())

## Change name for United Kingdom to be consistent with EPWT
WIOD[WIOD$countrycode=="GBR","country"] <- "United Kingdom"

## Get rid of Taiwan, since there is no PPP data available
WIOD <- WIOD[WIOD$countrycode!="TWN",]

## Melt year columns into one
WIOD <- WIOD %>% pivot_longer("2000":"2014","year", values_to="value") 

## Merge PPP data into WIOD, convert values, and drop the PPP column
WIOD <- merge(WIOD, 
              OECD_PPP) %>%
  merge(XR)

## Unmelt variable column into several columns
WIOD <- WIOD %>% pivot_wider(names_from = 'variable') %>%
  rename(Y=VA,
         Profit=CAP)

## Calculate relevant variables at the industry level:
WIOD <- WIOD %>% mutate(OCR=Y/K,
                        PS=Profit/Y,
                        ROP=100*PS*OCR)

###################### Define Necessary Functions ########################################## 

## Function which finds the average annual rate of growth
avg_GR <- function(x){
  # x is a vector of observations
  if(length(x)>2){
    # If 3 or more observations, estimate regression
    d <- cbind(x,1:length(x))
    g <- coef(lm(log(d[,1]) ~ d[,2]))[2]
    gwth <- 100*g
  }else{
    # If fewer than 3 observations, compute growth rate directly
    gwth <- 100*((x[2]-x[1])/abs(x[1]))
  }
  # Return value
  return(gwth)
}

## Function which converts capital stocks and value added to USD using either PPPs or XRs
currencyConversion <- function(data, conversionFactor){
  if(conversionFactor=="PPP"){
    mutate(data[!is.na(data$PPP),],
           K=K/PPP,
           Y=Y/PPP,
           Profit=Profit/PPP)
  } else{
    mutate(data[!is.na(data$XR),],
           K=K/XR,
           Y=Y/XR,
           Profit=Profit/XR)
  }
}

## Function which creates a graph for rate of profit (Plot 1)
plot1 <- function(data, numCountries, aggregateLevel, dataSource, aggregateType, trendLine){
  ggplot(data=data,
         aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
    geom_line() + 
    {if(trendLine!="None")
      geom_smooth(method=ui.trendLineList[[trendLine]][1],
                  formula=ui.trendLineList[[trendLine]][2],
                  se=FALSE,
                  linetype="dashed")} +
    labs(x="Year",
         y="Percentage",
         title=paste0(aggregateLevel,
                      " Annual Rate of Profit"),
         subtitle=if(aggregateType %in% c("All","LimitCountries")){
           paste0(dataSource,
                  ": ",
                  as.character(numCountries),
                  " countries")
         }
         else if(aggregateType=="Country"){dataSource}
         else if(aggregateLevel=="Global"){
           paste0(aggregateType,
                  ": ",
                  as.character(numCountries),
                  " countries")} else{aggregateType}) +
    theme_minimal()
}

## Function which creates plot for ROP decomposition (Plot 2)
plot2 <- function(plotType,data,dateStart,dateEnd,trendLine){
  if(plotType=="histogram"){
    ggplot(data = data.frame(gr_OCR = avg_GR(data[with(data,order(year)),]$OCR),
                             gr_PS = avg_GR(data[with(data,order(year)),]$PS),
                             gr_ROP=avg_GR(data[with(data,order(year)),]$ROP)) %>%
             gather("Measure",
                    "Value"), 
           aes(x=Measure, y=Value, fill=Measure)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme_minimal() +
      theme(axis.text.x=element_blank()) +
      scale_fill_manual("Measure",
                        values = c("navajowhite1",
                                   "lightsalmon1",
                                   "lightsalmon4"),
                        labels = c("Output-Capital Ratio",
                                   "Profit Share",
                                   "Rate of Profit")) +
      labs(x="",
           y="Average Growth Rate (%)",
           title="Rate of Profit Decomposition",
           subtitle=paste0("Average Rates of Growth: ",format(dateStart,format="%Y")," - ",format(dateEnd,format="%Y")))
  } else{
    ggplot(data = data %>%
             select(year,OCR,PS) %>%
             gather("Measure",
                    "Value",
                    -year),
           aes(x=as.Date(as.character(year), "%Y"), y=Value, color=Measure)) +
      geom_line() +
      {if(trendLine!="None")
        geom_smooth(method=ui.trendLineList[[trendLine]][1],
                    formula=ui.trendLineList[[trendLine]][2],
                    se=FALSE,
                    linetype="dashed")} +
      labs(x="Year",
           y="Ratio",
           title=paste0("Rate of Profit Decomposition")) +
      scale_color_discrete(labels = c("Output-Capital Ratio",
                                      "Profit Share")) +
      theme_minimal()
  }
}

######################### Shiny Code ####################################

## List used to generate plot subtitle
ui.CL_GlobalPlot1Subtitle <- list(All = "Using all available data observations",
                               LimitCountries = "Using data from countries with observations in all selected years")

## This list is used for geom_smooth() function arguments for a trend line for the main plot 
ui.trendLineList <- list("None" = c("None","None"),
                         "Linear" = c("lm", "y~x"),
                         "Loess" = c("loess", "y~x"),
                         "Quadratic" = c("lm", "y~poly(x,2)"),
                         "Cubic" = c("lm", "y~poly(x,3)"))

## Table which displays the countries within each data source
ui.CL_CountriesTable <- data.frame(Country = sort(unique(EPWT$country))) %>%
  mutate(EPWT = Country %in% unique(EPWT$country),
         WIOD = Country %in% unique(WIOD$country)) %>%
  mutate_all(list(~ str_replace(.,"TRUE","&#10004;"))) %>%
  mutate_all(list(~ str_replace(.,"FALSE","	&#10060;")))

# ## Generate list of industries with descriptions
# ui.IL_IndustriesList.df <- unique(WIOD[,c("code","description")])[order(unique(WIOD$description)),]
# ui.IL_IndustriesList <- as.list(ui.IL_IndustriesList.df$code)
# names(ui.IL_IndustriesList) <- ui.IL_IndustriesList.df$description

############### UI #################

## Create UI
ui <- dashboardPage(
  dashboardHeader(title = "Marxian Rates of Profit",
                  titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Country-Level", tabName="CL"),
      menuItem("Industry-Level", tabName="IL")
    )
  ),
  dashboardBody(
    tabItems(
      
      ############################################# Country-Level #################################
      
      tabItem(tabName="CL",
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput(inputId = "CL_dataSource",
                              label = "Data Source:",
                              choices = c("EPWT",
                                          "WIOD")),
                  
                  airDatepickerInput("CL_dateStart",
                                     label = "Start Year",
                                     value = "1960-01-02",
                                     maxDate = Sys.Date()-365,
                                     minDate = "1960-01-02",
                                     view = "years", #editing what the popup calendar shows when it opens
                                     minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                     dateFormat = "yyyy"
                  ),
                  
                  airDatepickerInput("CL_dateEnd",
                                     label = "End Year",
                                     value = Sys.Date()-365,
                                     maxDate = Sys.Date()-365,
                                     minDate = "1960-01-02",
                                     view = "years", #editing what the popup calendar shows when it opens
                                     minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                     dateFormat = "yyyy"
                  ),
                  
                  selectInput(inputId = "CL_currencyConversion",
                              label = "Convert current local currencies to current USD using:",
                              choices = c("Purchasing Power Parities (PPP)" = "PPP",
                                          "Exchange Rates" = "XR")),
                  
                  uiOutput("CL_conditionalPanel1"),
                  
                  uiOutput("CL_conditionalPanel2"),
                  
                  selectInput(inputId = "CL_trendLine", ## Choice of Trend Line for ROP graph
                              label = "Trend Line:",
                              c("None" = "None",
                                "Linear Trend" = "Linear",
                                "Loess Trend" = "Loess",
                                "Quadratic Trend" = "Quadratic",
                                "Cubic Trend" = "Cubic")),
                  
                  selectInput(inputId = "CL_plot2Type",
                              label = "Display ROP Decomposition as:",
                              choices = c("Histogram of Average Rates" = "histogram",
                                          "Time-Series" = "timeSeries")),
                  
                  wellPanel(
                    selectInput("CL_fformat", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                    uiOutput("CL_downloadData")
                  )
                ), ## sideBarPanel
                
                mainPanel(
                  tabsetPanel(
                    
                    ###################### Global Profit Rate ########################
                    
                    tabPanel("Global",
                             plotOutput("CL_plotGlobal1"),
                             div(downloadButton("CL_downloadPlot1Global", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("CL_plotGlobal2"),
                             div(downloadButton("CL_downloadPlot2Global", "Download Plot 2"),style="float:right")
                    ), ## tabPanel (Global)
                    
                    ###################### Group Profit Rate ########################
                    
                    tabPanel("By Group",
                             uiOutput("CL_groupMainPanel")
                    ), ## tabPanel (Group)
                    
                    ###################### Country Profit Rate ########################
                    
                    tabPanel("By Country",
                             uiOutput("CL_countryMainPanel")
                    ), ## tabPanel (Country)
                    
                    id="CL_tab" 
                  ) ## tabSetPanel
                ) ## mainPanel
              ), ## sideBarLayout
              br(),
              uiOutput("CL_text"),
              downloadButton("CL_downloadExplanationFile", "More Information")
      ), ## tabItem (CL)
      
      ############################################# Industry-Level #################################
      
      tabItem(tabName="IL",
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("IL_industry",
                              label = "Select Industry:",
                              choices = sort(unique(WIOD$description)),
                              selectize=FALSE),
                  
                  airDatepickerInput("IL_dateStart",
                                     label = "Start Year",
                                     value = "2000-01-02",
                                     maxDate = Sys.Date()-365,
                                     minDate = "2000-01-02",
                                     view = "years", #editing what the popup calendar shows when it opens
                                     minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                     dateFormat = "yyyy"
                  ),
                  
                  airDatepickerInput("IL_dateEnd",
                                     label = "End Year",
                                     value = Sys.Date()-365,
                                     maxDate = Sys.Date()-365,
                                     minDate = "2000-01-02",
                                     view = "years", #editing what the popup calendar shows when it opens
                                     minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                     dateFormat = "yyyy"
                  ),
                  
                  selectInput(inputId = "IL_currencyConversion",
                              label = "Convert current local currencies to current USD using:",
                              choices = c("Purchasing Power Parities (PPP)" = "PPP",
                                          "Exchange Rates" = "XR")),
                  
                  uiOutput("IL_conditionalPanel"),
                  
                  selectInput(inputId = "IL_trendLine", ## Choice of Trend Line for ROP graph
                              label = "Trend line:",
                              c("None" = "None",
                                "Linear Trend" = "Linear",
                                "Loess Trend" = "Loess",
                                "Quadratic Trend" = "Quadratic",
                                "Cubic Trend" = "Cubic")),
                  
                  selectInput(inputId = "IL_plot2Type",
                              label = "Display ROP Decomposition as:",
                              choices = c("Histogram of Average Rates" = "histogram",
                                          "Time-Series" = "timeSeries")),
                  
                  wellPanel(
                    selectInput("IL_fformat", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                    uiOutput("IL_downloadData")
                  )
                ), ## sideBarPanel
                
                mainPanel(
                  tabsetPanel(
                    
                    ###################### Global Profit Rate ########################
                    
                    tabPanel("Global",
                             plotOutput("IL_plotGlobal1"),
                             div(downloadButton("IL_downloadPlot1Global", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("IL_plotGlobal2"),
                             div(downloadButton("IL_downloadPlot2Global", "Download Plot 2"),style="float:right")
                    ),
                    
                    ###################### Country Profit Rate ########################
                    
                    tabPanel("By Country",
                             plotOutput("IL_plotCountry1"),
                             div(downloadButton("IL_downloadPlot1Country", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("IL_plotCountry2"),
                             div(downloadButton("IL_downloadPlot2Country", "Download Plot 2"),style="float:right")
                    ),
                    
                    id="IL_tab"
                  ) ## tabSetPanel
                ) ## mainPanel
              ), ## sideBarLayout
              br(),
              uiOutput("IL_text"),
              downloadButton("IL_downloadExplanationFile", "More Information") 
      ) ## tabItem (IL)
    )## tabItems
  ) ## dashboardBody
) ## dashboardPage


############### Server #################

server <- function(input, output) {
  
  ############################################# Country-Level ###############################################
  
  ## Conditional Panel to display
  output$CL_conditionalPanel1 <- renderUI({
    if((input$CL_tab=="Global" | input$CL_tab=="By Group") & input$CL_dataSource=="EPWT"){
      selectInput(inputId = "CL_aggregate", ## Choose method for aggregating data
                  label = "Method to compute Aggregates:",
                  choices = c("Use only countries with data observations in all selected years" = "LimitCountries",
                              "Use all available data observations" = "All"),
                  selectize=FALSE)
    }
  })
  output$CL_conditionalPanel2 <- renderUI({
    if(input$CL_tab=="By Group"){
      selectInput(inputId = "CL_group", ## Income Group
                  label = "Income Group:",
                  choices = sort(unique(EPWT$wb_income_group)[2:6]), # First value is NA, 
                  selected = "High income: OECD")
    } else if(input$CL_tab=="By Country"){
      selectInput(inputId = "CL_country", ## Country
                  label = "Country:",
                  choices = sort(unique(EPWT$country)), 
                  selected = "United States")
    }
  })
  
  ## Download button for sidePanel
  output$CL_downloadData <- renderUI({
    if(input$CL_tab=="Global"){
      downloadButton("CL_downloadDataGlobal", "Download Data")
    } else if(input$CL_tab=="By Group"){
      downloadButton("CL_downloadDataGroup", "Download Data")
    } else if(input$CL_tab=="By Country"){
      downloadButton("CL_downloadDataCountry", "Download Data")
    }
  })
  
  ## Download Handler for downloading explanatory pdf
  output$CL_downloadExplanationFile <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  
  ## Text to display at bottom of page
  description <- tagList("Data for the profitability analysis reported on this dashboard comes from the ",
                         a("Extended Penn World Table",href="https://sites.google.com/a/newschool.edu/duncan-foley-homepage/home/EPWT"),
                         "and the ",
                         HTML(paste0(a("World Input-Output Database",href="https://www.rug.nl/ggdc/valuechain/wiod/?lang=en"),".")),
                         " More details can be found in the documentation file available for download below. This dashboard has been created and is maintained by Evan Wasner ",
                         HTML(paste0("(",a("ewasner@umass.edu",href="mailto:@ewasner@umass.edu"),"),")),
                         " Jesus Lara Jauregui ",
                         HTML(paste0("(",a("jlarajauregu@umass.edu",href="mailto:@jlarajauregu@umass.edu"),"),")),
                         " Julio Huato ",
                         HTML(paste0("(",a("jhuato@sfc.edu",href="mailto:@jhuato@sfc.edu"),"),")),
                         " and Deepankar Basu ",
                         HTML(paste0("(",a("dbasu@econs.umass.edu",href="mailto:@dbasu@econs.umass.edu"),")."))) 
  
  ## Shiny cannot use the same output multiple times --> therefore must define three separate outputs to reuse text
  output$CL_text <- renderUI({description})
  
  ###################### Global Profit Rate ########################
  
  ## Global EPWT data - Using all available data observations from each year
  data.CL.EPWT.Global.All <- reactive({
    data <- currencyConversion(EPWT,input$CL_currencyConversion) %>% 
      filter(format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y")) 
    list(data %>%
           group_by(year) %>% 
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  ## Global EPWT data - Using only countries which have observations in every year in the user's time selection
  data.CL.EPWT.Global.LimitCountries <- reactive({
    data <- currencyConversion(EPWT,input$CL_currencyConversion) %>% 
      filter(format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y"))
    ## Create a list of countries which have data in all years selected
    validCountries <- group_by(data,countrycode) %>% summarise(length=n()) %>% filter(length==max(length))
    data <- data %>% filter(countrycode %in% validCountries$countrycode) 
    list(data %>%
           group_by(year) %>% 
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  ## Create list with both All and LimitCountries dfs for EPWT
  data.CL.EPWT.Global <- reactive({
    list(All=data.CL.EPWT.Global.All(),
         LimitCountries=data.CL.EPWT.Global.LimitCountries())
  })
  
  ## Create WIOD data
  data.CL.WIOD.Global <- reactive({
    data <- currencyConversion(WIOD,input$CL_currencyConversion) %>%
      filter(format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y")) 
    list(data %>%
           group_by(year, country) %>% summarize(across(c(K, Profit, Y), sum)) %>%
           group_by(year) %>%
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  ## Create a list with both EPWT and WIOD data for user selection by token input$CL_dataSource
  data.CL.Global <- reactive({
    list("EPWT" = data.CL.EPWT.Global()[[if(is.null(input$CL_aggregate)){"LimitCountries"}else{input$CL_aggregate}]],
         "WIOD" = data.CL.WIOD.Global())
  })
  
  # Plot Global ROP
  CL_GlobalPlot1 <- reactive({
    plot1(data.CL.Global()[[input$CL_dataSource]][[1]],
          data.CL.Global()[[input$CL_dataSource]][[2]],
          "Global",
          input$CL_dataSource,
          if(is.null(input$CL_aggregate)){"LimitCountries"}else{input$CL_aggregate},
          input$CL_trendLine)
  })
  
  ## Plot Global ROP - output
  output$CL_plotGlobal1 <- renderPlot({
    CL_GlobalPlot1()
  })
  
  ## Plot Global ROP Decomposition
  CL_GlobalPlot2 <- reactive({
    plot2(input$CL_plot2Type,
          data.CL.Global()[[input$CL_dataSource]][[1]],
          min(data.CL.Global()[[input$CL_dataSource]][[1]]$year),
          max(data.CL.Global()[[input$CL_dataSource]][[1]]$year),
          input$CL_trendLine)
  })
  
  ## Plot Global ROP Decomposition - output
  output$CL_plotGlobal2 <- renderPlot({
    CL_GlobalPlot2()
  })
  
  ## Download Handler for Plot 1
  output$CL_downloadPlot1Global <- downloadHandler(
    filename = function(){paste0("RateOfProfit - Global - ",
                                 input$CL_dataSource,
                                 ".",
                                 input$CL_fformat)},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_GlobalPlot1())
      dev.off()
    })
  
  ## Download Handler for Plot 2
  output$CL_downloadPlot2Global <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - Global - ",
                                 input$CL_dataSource,
                                 ".",
                                 input$CL_fformat)},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_GlobalPlot2())
      dev.off()
    })
  
  ## Download Handler for Global ROP data
  output$CL_downloadDataGlobal <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - Global - ",
                                 input$CL_dataSource,
                                 ".csv")},
    content = function(file){
      write.table(data.CL.Global()[[input$CL_dataSource]][[1]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Income Group Profit Rate ########################
  
  ## Group EPWT data - Using all available data observations from each year
  data.CL.EPWT.Group.All <- reactive({
    data <- currencyConversion(EPWT,input$CL_currencyConversion) %>% 
      filter(wb_income_group==if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
             format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y")) 
    list(data %>%
           group_by(year) %>% 
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  ## Group EPWT data - Using only countries which have observations in every year in the user's time selection
  data.CL.EPWT.Group.LimitCountries <- reactive({
    ## Filter for countries which have observations for every year in selected date range
    data <- currencyConversion(EPWT,input$CL_currencyConversion) %>% 
      filter(wb_income_group==if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
             format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y"))
    ## Create a list of countries which have data in all years selected
    validCountries <- group_by(data,countrycode) %>% summarise(length=n()) %>% filter(length==max(length))
    data <- data %>% filter(countrycode %in% validCountries$countrycode) 
    list(data %>%
           group_by(year) %>% 
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  ## Create list with both All and LimitCountries dfs
  data.CL.EPWT.Group <- reactive({
    list(All=data.CL.EPWT.Group.All(),
         LimitCountries=data.CL.EPWT.Group.LimitCountries())
  })
  
  ## Create a list with both EPWT and WIOD data for user selection by token input$CL_dataSource
  data.CL.Group <- reactive({
    data.CL.EPWT.Group()[[if(is.null(input$CL_aggregate)){"LimitCountries"}else{input$CL_aggregate}]]
  })
  
  ## Plot Group ROP
  CL_GroupPlot1 <- reactive({
    plot1(data.CL.Group()[[1]],
          data.CL.Group()[[2]],
          if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
          input$CL_dataSource,
          if(is.null(input$CL_aggregate)){"LimitCountries"}else{input$CL_aggregate},
          input$CL_trendLine)
  })
  
  ## Plot Group ROP - output
  output$CL_plotGroup1 <- renderPlot({
    CL_GroupPlot1()
  })
  
  ## Plot Global ROP Decomposition
  CL_GroupPlot2 <- reactive({
    plot2(input$CL_plot2Type,
          data.CL.Group()[[1]],
          min(data.CL.Group()[[1]]$year),
          max(data.CL.Group()[[1]]$year),
          input$CL_trendLine)
  })
  
  ## Plot Group ROP Decomposition
  output$CL_plotGroup2 <- renderPlot({
    CL_GroupPlot2()
  })
  
  ## Download Handler for Plot 1
  output$CL_downloadPlot1Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfit - ",
                                             if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
                                             " - ",
                                             input$CL_dataSource,
                                             ".",
                                             input$CL_fformat),
                                      ":"," -")},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_GroupPlot1())
      dev.off()
    })
  
  ## Download Handler for Plot 2
  output$CL_downloadPlot2Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfitDecomposition - ",
                                             if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
                                             " - ",
                                             input$CL_dataSource,
                                             ".",
                                             input$CL_fformat),
                                      ":"," -")},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_GroupPlot2())
      dev.off()
    })
  
  ## Download Handler for Data
  output$CL_downloadDataGroup <- downloadHandler(
    filename = function(){str_replace(paste0("RateofProfit Data - ",
                                             if(is.null(input$CL_group)){"High income: OECD"}else{input$CL_group},
                                             " - ",
                                             input$CL_dataSource,
                                             ".csv"),
                                      ":"," -")},
    content = function(file){
      write.table(data.CL.Group()[[1]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ## If the user has selected the WIOD data source, display text which explains
  ## aggregation by income group is only available for the EPWT. 
  ## Otherwise display the proper plots and download buttons
  output$CL_WIODgroupText <- renderText({"Aggregation by income group is only available for the EPWT data set due to the limited number of countries contained within the WIOD data set."})
  output$CL_groupMainPanel <- renderUI({
    if(input$CL_dataSource == "WIOD"){
      textOutput("CL_WIODgroupText")
    } else{
      list(
        plotOutput("CL_plotGroup1"),
        div(downloadButton("CL_downloadPlot1Group", "Download Plot 1"),style="float:right"),
        br(),
        br(),
        plotOutput("CL_plotGroup2"),
        div(downloadButton("CL_downloadPlot2Group", "Download Plot 2"),style="float:right"))
    }
  })
  
  ###################### Individual Country Profit Rate ########################

  ## EPWT Data for individual country selection
  data.CL.EPWT.Country <- reactive({
    EPWT %>% 
      filter(country==if(is.null(input$CL_country)){"United States"}else{input$CL_country},
             format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y")) %>%
      select(year,OCR,PS,ROP)
  })
  
  ## WIOD Data for individual country selection
  data.CL.WIOD.Country <- reactive({
    WIOD %>% 
      filter(format(input$CL_dateStart,format="%Y") <= year & year <= format(input$CL_dateEnd,format="%Y"),
             country==if(is.null(input$CL_country)){"United States"}else{input$CL_country}) %>%
      group_by(year, country) %>% 
      summarize(across(c(K, Profit, Y), sum)) %>%
      mutate(OCR=Y/K,
             PS=Profit/Y,
             ROP=100*OCR*PS)
  })
  
  ## Create a list with both EPWT and WIOD data for user selection by token input$CL_dataSource
  data.CL.Country <- reactive({
    list("EPWT" = data.CL.EPWT.Country()[with(data.CL.EPWT.Country(),order(year)),],
         "WIOD" = data.CL.WIOD.Country()[with(data.CL.WIOD.Country(),order(year)),])
  })
  
  ## Plot Group ROP
  CL_CountryPlot1 <- reactive({
    plot1(data.CL.Country()[[input$CL_dataSource]],
          NA,
          if(is.null(input$CL_country)){"United States"}else{input$CL_country},
          input$CL_dataSource,
          "Country",
          input$CL_trendLine)
  })
  
  ## Plot Country ROP - output
  output$CL_plotCountry1 <- renderPlot({
    CL_CountryPlot1()
  })
  
  ## Plot Global ROP Decomposition
  CL_CountryPlot2 <- reactive({
    plot2(input$CL_plot2Type,
          data.CL.Country()[[input$CL_dataSource]],
          min(data.CL.Country()[[input$CL_dataSource]]$year),
          max(data.CL.Country()[[input$CL_dataSource]]$year),
          input$CL_trendLine)
  })

  ## Plot Country ROP Decomposition - output
  output$CL_plotCountry2 <- renderPlot({
    CL_CountryPlot2()
  })
  
  ## Download Handler for Plot 1
  output$CL_downloadPlot1Country <- downloadHandler(
    filename = function(){paste0("RateOfProfit - ",
                                 if(is.null(input$CL_country)){"United States"}else{input$CL_country},
                                 " - ",
                                 input$CL_dataSource,
                                 ".",
                                 input$CL_fformat)},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_CountryPlot1())
      dev.off()
    })
  
  ## Download Handler for Plot 2
  output$CL_downloadPlot2Country <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - ",
                                 if(is.null(input$CL_country)){"United States"}else{input$CL_country},
                                 " - ",
                                 input$CL_dataSource,
                                 ".",
                                 input$CL_fformat)},
    content = function(file) {
      do.call(input$CL_fformat,list(file))
      print(CL_CountryPlot2())
      dev.off()
    })
  
  ## Download Handler for Data
  output$CL_downloadDataCountry <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - ",
                                 if(is.null(input$CL_country)){"United States"}else{input$CL_country},
                                 " - ",
                                 input$CL_dataSource,
                                 ".csv")},
    content = function(file){
      write.table(data.CL.Country()[[input$CL_dataSource]], 
                  file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ## If the user has selected the WIOD data source and a country which is not included in WIOD,
  ## then display a table which shows which countries are available in each data source. 
  ## Otherwise display the proper plots and download buttons
  output$CL_WIODcountryText <- renderText({"The selected country is not available in the WIOD data set. The table below displays the countries available within each data source:"})
  output$CL_WIODcountryTable <- renderTable({ui.CL_CountriesTable},sanitize.text.function = identity)
  output$CL_countryMainPanel <- renderUI({
    if(input$CL_dataSource == "WIOD" & !((if(is.null(input$CL_country)){"United States"}else{input$CL_country}) %in% unique(WIOD$country))){
      list(
        textOutput("CL_WIODcountryText"),
        br(),
        tableOutput("CL_WIODcountryTable"),
        tags$head(tags$style("#CL_WIODcountryTable table {background-color: white; }", media="screen", type="text/css")))
    } else{
      list(
        plotOutput("CL_plotCountry1"),
        div(downloadButton("CL_downloadPlot1Country", "Download Plot 1"),style="float:right"),
        br(),
        br(),
        plotOutput("CL_plotCountry2"),
        div(downloadButton("CL_downloadPlot2Country", "Download Plot 2"),style="float:right"))
    }
  })
  
  
  ############################################# Industry-Level ###############################################
  
  ## Conditional Panel to display
  output$IL_conditionalPanel <- renderUI({
    if(input$IL_tab=="By Group"){
      selectInput(inputId = "IL_group", ## Income Group
                  label = "Income Group:",
                  choices = sort(unique(WIOD$wb_income_group)), 
                  selected = "High income: OECD")
    } else if(input$IL_tab=="By Country"){
      selectInput(inputId = "IL_country", ## Country
                  label = "Country:",
                  choices = sort(unique(WIOD$country)), 
                  selected = "United States")
    }
  })
  
  ## Download button for sidePanel
  output$IL_downloadData <- renderUI({
    if(input$IL_tab=="Global"){
      downloadButton("IL_downloadDataGlobal", "Download Data")
    } else if(input$IL_tab=="By Group"){
      downloadButton("IL_downloadDataGroup", "Download Data")
    } else if(input$IL_tab=="By Country"){
      downloadButton("IL_downloadDataCountry", "Download Data")
    }
  })
  
  ## Download Handler for downloading explanatory pdf
  output$IL_downloadExplanationFile <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  
  ## Shiny cannot use the same output multiple times --> therefore must define three separate outputs to reuse text
  output$IL_text <- renderUI({description})
  
  ###################### Global Profit Rate ########################
  
  data.IL.WIOD.Global <- reactive({
    data <- na.omit(currencyConversion(WIOD,input$IL_currencyConversion)) %>% 
      filter(format(input$IL_dateStart,format="%Y") <= year & year <= format(input$IL_dateEnd,format="%Y"),
             description == input$IL_industry,
             K!=0) 
    list(data %>%
           group_by(year, country) %>% summarize(across(c(K, Profit, Y), sum)) %>%
           group_by(year) %>%
           ## Compute ROP, PS, OCR with global sums of profits, capital stocks, and value added
           summarise(ROP=100*sum(Profit)/sum(K),
                     PS=sum(Profit)/sum(Y),
                     OCR=sum(Y)/sum(K)),
         length(unique(data$countrycode)))
  })
  
  # Plot Global ROP
  IL_GlobalPlot1 <- reactive({
    plot1(data.IL.WIOD.Global()[[1]],
          data.IL.WIOD.Global()[[2]],
          "Global",
          "WIOD",
          input$IL_industry,
          input$IL_trendLine)
  })
  
  ## Plot Global ROP - output
  output$IL_plotGlobal1 <- renderPlot({
    IL_GlobalPlot1()
  })
  
  ## Plot Global ROP Decomposition
  IL_GlobalPlot2 <- reactive({
    plot2(input$IL_plot2Type,
          data.IL.WIOD.Global()[[1]],
          min(data.IL.WIOD.Global()[[1]]$year),
          max(data.IL.WIOD.Global()[[1]]$year),
          input$IL_trendLine)
  })
  
  ## Plot Global ROP Decomposition - output
  output$IL_plotGlobal2 <- renderPlot({
    IL_GlobalPlot2()
  })
  
  ## Download Handler for Plot 1
  output$IL_downloadPlot1Global <- downloadHandler(
    filename = function(){paste0("RateOfProfit - Global - ",
                                 input$IL_industry,
                                 ".",
                                 input$IL_fformat)},
    content = function(file) {
      do.call(input$IL_fformat,list(file))
      print(IL_GlobalPlot1())
      dev.off()
    })
  
  ## Download Handler for Plot 2
  output$IL_downloadPlot2Global <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - Global - ",
                                 input$IL_industry,
                                 ".",
                                 input$IL_fformat)},
    content = function(file) {
      do.call(input$IL_fformat,list(file))
      print(IL_GlobalPlot2())
      dev.off()
    })
  
  ## Download Handler for Data
  output$IL_downloadDataGlobal <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - Global - ",
                                 input$IL_industry,
                                 ".csv")},
    content = function(file){
      write.table(data.IL.WIOD.Global()[[1]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Country Profit Rate ########################
  
  data.IL.WIOD.Country <- reactive({
    na.omit(WIOD) %>% 
      filter(format(input$IL_dateStart,format="%Y") <= year & year <= format(input$IL_dateEnd,format="%Y"),
             description == input$IL_industry,
             K!=0,
             country == if(is.null(input$IL_country)){"United States"}else{input$IL_country}) %>%
      select(year,OCR,PS,ROP)
  })
  
  # Plot Country ROP
  IL_CountryPlot1 <- reactive({
    plot1(data.IL.WIOD.Country(),
          NA,
          if(is.null(input$IL_country)){"United States"}else{input$IL_country},
          "WIOD",
          input$IL_industry,
          input$IL_trendLine)
  })
  
  ## Plot Country ROP - output
  output$IL_plotCountry1 <- renderPlot({
    IL_CountryPlot1()
  })
  
  ## Plot Global ROP Decomposition
  IL_CountryPlot2 <- reactive({
    plot2(input$IL_plot2Type,
          data.IL.WIOD.Country(),
          min(data.IL.WIOD.Country()$year),
          max(data.IL.WIOD.Country()$year),
          input$IL_trendLine)
  })
  
  ## Plot Country ROP Decomposition - output
  output$IL_plotCountry2 <- renderPlot({
    IL_CountryPlot2()
  })
  
  ## Download Handler for Plot 1
  output$IL_downloadPlot1Country <- downloadHandler(
    filename = function(){paste0("RateOfProfit - ",
                                 input$IL_industry,
                                 " - ",
                                 if(is.null(input$IL_country)){"United States"}else{input$IL_country},
                                 ".",
                                 input$IL_fformat)},
    content = function(file) {
      do.call(input$IL_fformat,list(file))
      print(IL_CountryPlot1())
      dev.off()
    })
  
  ## Download Handler for Plot 2
  output$IL_downloadPlot2Country <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - ",
                                 input$IL_industry,
                                 " - ",
                                 if(is.null(input$IL_country)){"United States"}else{input$IL_country},
                                 ".",
                                 input$IL_fformat)},
    content = function(file) {
      do.call(input$IL_fformat,list(file))
      print(IL_CountryPlot2())
      dev.off()
    })
  
  ## Download Handler for Data
  output$IL_downloadDataCountry <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - ",
                                 input$IL_industry,
                                 " - ",
                                 if(is.null(input$IL_country)){"United States"}else{input$IL_country},
                                 ".csv")},
    content = function(file){
      write.table(data.IL.WIOD.Country(), file = file, sep = ",", row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
