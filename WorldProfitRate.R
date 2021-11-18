## Libraries 
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard, warn.conflicts = FALSE)
library(readxl)

## Clear workplace
rm(list = ls())

## Set working directory - This is for Evan 
## Needs to be changed for server
## setwd("I:/Evan/Documents/Umass/RA - Deepankar Fall 2021/Global Rate of Profit/Global_Rate_of_Profit")


###################### Collect & Manipulate Data From PWT Spreadsheet ######################
## Read from .csv file
PWT <- read_excel("EPWT 7.0 Preliminary.xlsx",
                  sheet = "EPWT7.0")
class <- read.csv("dh_country_class.csv") # For World Bank Income Group Categorization

## Remove na values from PWT, select relevant variables, and
## calculate Profit share (PS), Output-Capital Ratio (OCR), and Rate of Profit (r)
PWT <- na.omit(PWT %>% select(countrycode, country, year, LabShare, rhonatcur, Kppp2017, XGDPppp2017) %>%
                 filter(year > 1949)) %>%
  rename(Y=XGDPppp2017, K=Kppp2017, OCR=rhonatcur) %>%
  mutate(PS = 1-LabShare, r = 100 * PS * OCR)

## Merge PWT with WB income group categories
PWT <- merge(PWT, class %>% select(countrycode,wb_income_group), by="countrycode", all.x=TRUE)


###################### Collect & Manipulate Data From WIOT Spreadsheet ######################

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

## Import Socioeconomic accounts
WIOT <- merge(read_excel("WIOD_SEA_Nov16.xlsx", 
                   sheet = "DATA") %>% suppressWarnings(),
              read_excel("WIOD_SEA_Nov16.xlsx", 
                         sheet = "Notes",
                         skip=6)[,1:2],
              by.x="country",
              by.y="Acronym")

## Get it in tidy format 
WIOT <- WIOT %>% pivot_longer("2000":"2014","year", values_to="value")
WIOT <- WIOT %>% pivot_wider(names_from = 'variable')

## Calculate relevant variables at the industry level:
WIOT <- WIOT %>% mutate(OCR=VA/K,
                        PS1=CAP/VA,
                        PS2=(VA-LAB)/VA,
                        ROP1=OCR*PS1,
                        ROP2=OCR*PS2,
                        labor.share=LAB/VA,
                        r.surplus.1=CAP/LAB,
                        r.surplus.2=(VA-LAB)/LAB,
                        org.comp=K/LAB)


###################### Define Necessary Functions ########################################## 

## Function which finds the average annual rate of growth
avg_GR <- function(x){
  # x is a vector of observations
  if(length(x)>2){
    # If 3 or more observations, estimate regression
    d <- cbind(x,1:length(x))
    g <- coef(lm(d[,1] ~ d[,2]))[2]
    gwth <- ifelse(mean(x,na.rm = TRUE)>0,
                   100*(g/mean(x,na.rm = TRUE)),
                   NA)
  }else{
    # If fewer than 3 observations, compute growth rate directly
    gwth <- 100*((x[2]-x[1])/abs(x[1]))
  }
  # Return value
  return(gwth)
}


######################### Shiny Code ####################################

## List used to generate plot subtitle
ui.EPWT_GlobalPlot1Subtitle <- list(All = "Using all available data observations",
                               LimitCountries = "Using data from countries with observations in all selected years")

## This list is used for geom_smooth() function arguments for a trend line for the main plot 
ui.trendLineList <- list("None" = c("",""),
                         "Linear" = c("lm", "y~x"),
                         "Loess" = c("loess", "y~x"),
                         "Quadratic" = c("lm", "y~poly(x,2)"),
                         "Cubic" = c("lm", "y~poly(x,3)"))

## Create UI
ui <- dashboardPage(
  dashboardHeader(title = "Marxian Rates of Profit",
                  titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Extended World Penn Table", tabName="EPWT"),
      menuItem("World Input-Output Data", tabName="WIOT")
    )),
  dashboardBody(
    tabItems(
      
      ############################################# EPWT #################################
      
      tabItem(tabName="EPWT",
              tabsetPanel(
                
                ###################### Global Profit Rate ########################
                
                tabPanel("Global",
                         sidebarLayout(
                           sidebarPanel(
                             airDatepickerInput("EPWT_dateStartGlobal",
                                                label = "Start Year",
                                                value = "1950-01-02",
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             airDatepickerInput("EPWT_dateEndGlobal",
                                                label = "End Year",
                                                value = Sys.Date()-365,
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             selectInput(inputId = "EPWT_globalAggregate", ## Choose method for aggregating data
                                         label = "Choose Method to compute Global Aggregates:",
                                         choices = c("Use all available data observations" = "All",
                                                     "Use only countries with data observations in all selected years" = "LimitCountries")),
                             
                             selectInput(inputId = "EPWT_trendLineGlobal", ## Choice of Trend Line for ROP graph
                                         label = "Choose a trend line to display:",
                                         c("None" = "None",
                                           "Linear Trend" = "Linear",
                                           "Loess Trend" = "Loess",
                                           "Quadratic Trend" = "Quadratic",
                                           "Cubic Trend" = "Cubic")),
                             
                             wellPanel(
                               selectInput("EPWT_fformatGlobal", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                               downloadButton("EPWT_downloadDataGlobal", "Download Data"),
                               downloadButton("EPWT_downloadExplanationFileGlobal", "More Information")
                             )),
                           
                           mainPanel(
                             plotOutput("EPWT_plotGlobal1"),
                             div(downloadButton("EPWT_downloadPlot1Global", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("EPWT_plotGlobal2"),
                             div(downloadButton("EPWT_downloadPlot2Global", "Download Plot 2"),style="float:right"),
                             br(),
                             br(),
                             uiOutput("EPWT_textGlobal")
                           ))),
                
                ###################### Group Profit Rate ########################
                
                tabPanel("By Group",
                         sidebarLayout(
                           sidebarPanel(
                             airDatepickerInput("EPWT_dateStartGroup",
                                                label = "Start Year",
                                                value = "1950-01-02",
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             airDatepickerInput("EPWT_dateEndGroup",
                                                label = "End Year",
                                                value = Sys.Date()-365,
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             selectInput(inputId = "EPWT_group", ## Income Group
                                         label = "Choose Income Group:",
                                         choices = sort(unique(PWT$wb_income_group)[2:6]), # First value is NA
                                         selected = "High income: OECD"),
                             
                             selectInput(inputId = "EPWT_groupAggregate", ## Choose method for aggregating data
                                         label = "Choose Method to compute Group Aggregates:",
                                         choices = c("Use all available data observations" = "All",
                                                     "Use only countries with data observations in all selected years" = "LimitCountries")),
                             
                             selectInput(inputId = "EPWT_trendLineGroup", ## Choice of Trend Line for ROP graph
                                         label = "Choose a trend line to display:",
                                         c("None" = "None",
                                           "Linear Trend" = "Linear",
                                           "Loess Trend" = "Loess",
                                           "Quadratic Trend" = "Quadratic",
                                           "Cubic Trend" = "Cubic")),
                             
                             wellPanel(
                               selectInput("EPWT_fformatGroup", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                               downloadButton("EPWT_downloadDataGroup", "Download Data"),
                               downloadButton("EPWT_downloadExplanationFileGroup", "More Information")
                             )),
                           
                           mainPanel(
                             plotOutput("EPWT_plotGroup1"),
                             div(downloadButton("EPWT_downloadPlot1Group", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("EPWT_plotGroup2"),
                             div(downloadButton("EPWT_downloadPlot2Group", "Download Plot 2"),style="float:right"),
                             br(),
                             br(),
                             uiOutput("EPWT_textGroup")
                           ))),
                
                ###################### Country Profit Rate ########################
                
                tabPanel("By Country",
                         sidebarLayout(
                           sidebarPanel(
                             airDatepickerInput("EPWT_dateStartCountry",
                                                label = "Start Year",
                                                value = "1950-01-02",
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             airDatepickerInput("EPWT_dateEndCountry",
                                                label = "End Year",
                                                value = Sys.Date()-365,
                                                maxDate = Sys.Date()-365,
                                                minDate = "1950-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             selectInput(inputId = "EPWT_country", ## Country
                                         label = "Choose Country:",
                                         choices = sort(unique(PWT$country)),
                                         selected = "United States"),
                             
                             selectInput(inputId = "EPWT_trendLineCountry", ## Choice of Trend Line for ROP graph
                                         label = "Choose a trend line to display:",
                                         c("None" = "None",
                                           "Linear Trend" = "Linear",
                                           "Loess Trend" = "Loess",
                                           "Quadratic Trend" = "Quadratic",
                                           "Cubic Trend" = "Cubic")),
                             
                             wellPanel(
                               selectInput("EPWT_fformatCountry", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                               downloadButton("EPWT_downloadDataCountry", "Download Data"),
                               downloadButton("EPWT_downloadExplanationFileCountry", "More Information")
                             )),
                           
                           mainPanel(
                             plotOutput("EPWT_plotCountry1"),
                             div(downloadButton("EPWT_downloadPlot1Country", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("EPWT_plotCountry2"),
                             div(downloadButton("EPWT_downloadPlot2Country", "Download Plot 2"),style="float:right"),
                             br(),
                             br(),
                             uiOutput("EPWT_textCountry")
                           )
                         )
                )
              )
      ),
      
      ############################################# WIOT #################################
      
      tabItem(tabName="WIOT",
              tabsetPanel(
                
                ###################### Global Profit Rate ########################
                
                tabPanel("Global",
                         sidebarLayout(
                           sidebarPanel(
                             airDatepickerInput("WIOT_dateStartGlobal",
                                                label = "Start Year",
                                                value = "2000-01-02",
                                                maxDate = Sys.Date()-365,
                                                minDate = "2000-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             airDatepickerInput("WIOT_dateEndGlobal",
                                                label = "End Year",
                                                value = Sys.Date()-365,
                                                maxDate = Sys.Date()-365,
                                                minDate = "2000-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             selectInput(inputId = "WIOT_trendLineGlobal", ## Choice of Trend Line for ROP graph
                                         label = "Choose a trend line to display:",
                                         c("None" = "None",
                                           "Linear Trend" = "Linear",
                                           "Loess Trend" = "Loess",
                                           "Quadratic Trend" = "Quadratic",
                                           "Cubic Trend" = "Cubic")),
                             
                             wellPanel(
                               selectInput("WIOT_fformatGlobal", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                               downloadButton("WIOT_downloadDataGlobal", "Download Data"),
                               downloadButton("WIOT_downloadExplanationFileGlobal", "More Information")
                             )),
                           
                           mainPanel(
                             plotOutput("WIOT_plotGlobal1"),
                             div(downloadButton("WIOT_downloadPlot1Global", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("WIOT_plotGlobal2"),
                             div(downloadButton("WIOT_downloadPlot2Global", "Download Plot 2"),style="float:right"),
                             br(),
                             br(),
                             uiOutput("WIOT_textGlobal")
                           ))),
                
                ###################### Industry Profit Rate ########################
                
                tabPanel("By Industry",
                         br()),
                
                ###################### Country Profit Rate ########################
                
                tabPanel("By Country",
                         sidebarLayout(
                           sidebarPanel(
                             airDatepickerInput("WIOT_dateStartCountry",
                                                label = "Start Year",
                                                value = "2000-01-02",
                                                maxDate = Sys.Date()-365,
                                                minDate = "2000-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             airDatepickerInput("WIOT_dateEndCountry",
                                                label = "End Year",
                                                value = Sys.Date()-365,
                                                maxDate = Sys.Date()-365,
                                                minDate = "2000-01-02",
                                                view = "years", #editing what the popup calendar shows when it opens
                                                minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                                dateFormat = "yyyy"
                             ),
                             
                             selectInput(inputId = "WIOT_country", ## Country
                                         label = "Choose Country:",
                                         choices = sort(unique(WIOT$Name)),
                                         selected = "United States"),
                             
                             selectInput(inputId = "WIOT_trendLineCountry", ## Choice of Trend Line for ROP graph
                                         label = "Choose a trend line to display:",
                                         c("None" = "None",
                                           "Linear Trend" = "Linear",
                                           "Loess Trend" = "Loess",
                                           "Quadratic Trend" = "Quadratic",
                                           "Cubic Trend" = "Cubic")),
                             
                             wellPanel(
                               selectInput("WIOT_fformatCountry", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                               downloadButton("WIOT_downloadDataCountry", "Download Data"),
                               downloadButton("WIOT_downloadExplanationFileCountry", "More Information")
                             )),
                           
                           mainPanel(
                             plotOutput("WIOT_plotCountry1"),
                             div(downloadButton("WIOT_downloadPlot1Country", "Download Plot 1"),style="float:right"),
                             br(),
                             br(),
                             plotOutput("WIOT_plotCountry2"),
                             div(downloadButton("WIOT_downloadPlot2Country", "Download Plot 2"),style="float:right"),
                             br(),
                             br(),
                             uiOutput("WIOT_textCountry")
                           )
                         )
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  ############################################# EPWT ###############################################
  
  ###################### Global Profit Rate ########################
  
  ## Global data - Using all available data observations from each year
  data.EPWT.Global.All <- reactive({
    PWT %>% filter(format(input$EPWT_dateStartGlobal,format="%Y") <= year & year <= format(input$EPWT_dateEndGlobal,format="%Y")) %>%
      group_by(year) %>% 
      ## Calculate each observation's share in global capital stock and global output
      mutate(Kshare=K/sum(K),
             Yshare=Y/sum(Y)) %>%
      ## Compute weighted average of ROP, PS, and OCR
      summarise(ROP=sum(r*Kshare),
                PS=sum(PS*Yshare),
                OCR=sum(OCR*Kshare))
  })
  
  ## Global data - Using only countries which have observations in every year in the user's time selection
  data.EPWT.Global.LimitCountries <- reactive({
    data <- PWT %>% filter(format(input$EPWT_dateStartGlobal,format="%Y") <= year & year <= format(input$EPWT_dateEndGlobal,format="%Y"))
    ## Create a list of countries which have data in all years selected
    validCountries <- group_by(data,countrycode) %>% summarise(length=n()) %>% filter(length==max(length))
    data %>% filter(countrycode %in% validCountries$countrycode) %>%
      group_by(year) %>% 
      ## Calculate each observation's share in global capital stock and global output
      mutate(Kshare=K/sum(K),
             Yshare=Y/sum(Y)) %>%
      ## Compute weighted average of ROP, PS, and OCR
      summarise(ROP=sum(r*Kshare),
                PS=sum(PS*Yshare),
                OCR=sum(OCR*Kshare))
  })
  
  ## Create list with both All and LimitCountries dfs
  data.EPWT.Global <- reactive({
    list(All=data.EPWT.Global.All(),
         LimitCountries=data.EPWT.Global.LimitCountries())
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.EPWT.Global.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.EPWT.Global()[[input$EPWT_globalAggregate]]$OCR),
               gr_PS = avg_GR(data.EPWT.Global()[[input$EPWT_globalAggregate]]$PS),
               gr_ROP=avg_GR(data.EPWT.Global()[[input$EPWT_globalAggregate]]$ROP))
  })
  
  EPWT_GlobalPlot1 <- reactive({
    ggplot(data = data.EPWT.Global()[[input$EPWT_globalAggregate]]  %>%
             select(year, ROP), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$EPWT_trendLineGlobal]][1],
                  formula=ui.trendLineList[[input$EPWT_trendLineGlobal]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0("Global Annual Rate of Profit"),
           subtitle=ui.EPWT_GlobalPlot1Subtitle[[input$EPWT_globalAggregate]]) +
      theme_minimal()
  })
  
  ## Plot Global ROP
  output$EPWT_plotGlobal1 <- renderPlot({
    EPWT_GlobalPlot1()
  })
  
  EPWT_GlobalPlot2 <- reactive({
    ggplot(data = data.EPWT.Global.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",format(input$EPWT_dateStartGlobal,format="%Y")," - ",format(input$EPWT_dateEndGlobal,format="%Y")))
  })
  
  ## Plot Global ROP Decomposition
  output$EPWT_plotGlobal2 <- renderPlot({
    EPWT_GlobalPlot2()
  })
  
  output$EPWT_downloadPlot1Global <- downloadHandler(
    filename = function(){paste0("RateOfProfit - Global.",input$EPWT_fformatGlobal)},
    content = function(file) {
      do.call(input$EPWT_fformatGlobal,list(file))
      print(EPWT_GlobalPlot1())
      dev.off()
    })
  
  output$EPWT_downloadPlot2Global <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - Global.",input$EPWT_fformatGlobal)},
    content = function(file) {
      do.call(input$EPWT_fformatGlobal,list(file))
      print(EPWT_GlobalPlot2())
      dev.off()
    })
  
  output$EPWT_downloadDataGlobal <- downloadHandler(
    filename = "RateofProfit Data - Global.csv",
    content = function(file){
      write.table(data.EPWT.Global()[[input$EPWT_globalAggregate]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Income Group Profit Rate ########################
  
  ## Group data - Using all available data observations from each year
  data.EPWT.Group.All <- reactive({
    PWT %>% filter(wb_income_group==input$EPWT_group,
                   format(input$EPWT_dateStartGroup,format="%Y") <= year & year <= format(input$EPWT_dateEndGroup,format="%Y")) %>%
      group_by(year) %>% 
      ## Calculate each observation's share in global capital stock and global output
      mutate(Kshare=K/sum(K),
             Yshare=Y/sum(Y)) %>%
      ## Compute weighted average of ROP, PS, and OCR
      summarise(ROP=sum(r*Kshare),
                PS=sum(PS*Yshare),
                OCR=sum(OCR*Kshare))
  })
  
  ## Group data - Using only countries which have observations in every year in the user's time selection
  data.EPWT.Group.LimitCountries <- reactive({
    ## Filter for countries which have observations for every year in selected date range
    data <- PWT %>% filter(wb_income_group==input$EPWT_group,
                           format(input$EPWT_dateStartGroup,format="%Y") <= year & year <= format(input$EPWT_dateEndGroup,format="%Y"))
    ## Create a list of countries which have data in all years selected
    validCountries <- group_by(data,countrycode) %>% summarise(length=n()) %>% filter(length==max(length))
    data %>% filter(countrycode %in% validCountries$countrycode) %>%
      group_by(year) %>% 
      ## Calculate each observation's share in global capital stock and global output
      mutate(Kshare=K/sum(K),
             Yshare=Y/sum(Y)) %>%
      ## Compute weighted average of ROP, PS, and OCR
      summarise(ROP=sum(r*Kshare),
                PS=sum(PS*Yshare),
                OCR=sum(OCR*Kshare))
  })
  
  ## Create list with both All and LimitCountries dfs
  data.EPWT.Group <- reactive({
    list(All=data.EPWT.Group.All(),
         LimitCountries=data.EPWT.Group.LimitCountries())
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.EPWT.Group.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.EPWT.Group()[[input$EPWT_groupAggregate]]$OCR),
               gr_PS = avg_GR(data.EPWT.Group()[[input$EPWT_groupAggregate]]$PS),
               gr_ROP=avg_GR(data.EPWT.Group()[[input$EPWT_groupAggregate]]$ROP))
  })
  
  EPWT_GroupPlot1 <- reactive({
    ggplot(data = data.EPWT.Group()[[input$EPWT_groupAggregate]] %>%
             select(year, ROP), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$EPWT_trendLineGroup]][1],
                  formula=ui.trendLineList[[input$EPWT_trendLineGroup]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0(input$EPWT_group," Annual Rate of Profit"),
           subtitle=ui.EPWT_GlobalPlot1Subtitle[[input$EPWT_groupAggregate]]) +
      theme_minimal()
  })
  
  ## Plot Group ROP
  output$EPWT_plotGroup1 <- renderPlot({
    EPWT_GroupPlot1()
  })
  
  EPWT_GroupPlot2 <- reactive({
    ggplot(data = data.EPWT.Group.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",
                           format(input$EPWT_dateStartGroup,format="%Y"),
                           " - ",
                           format(input$EPWT_dateEndGroup,format="%Y")))
  })
  
  ## Plot Group ROP Decomposition
  output$EPWT_plotGroup2 <- renderPlot({
    EPWT_GroupPlot2()
  })
  
  output$EPWT_downloadPlot1Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfit - ",input$EPWT_group,".",input$EPWT_fformatGroup),":"," -")},
    content = function(file) {
      do.call(input$EPWT_fformatGroup,list(file))
      print(EPWT_GroupPlot1())
      dev.off()
    })
  
  output$EPWT_downloadPlot2Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfitDecomposition - ",input$EPWT_group,".",input$EPWT_fformatGroup),":"," -")},
    content = function(file) {
      do.call(input$EPWT_fformatGroup,list(file))
      print(EPWT_GroupPlot2())
      dev.off()
    })
  
  output$EPWT_downloadDataGroup <- downloadHandler(
    filename = function(){str_replace(paste0("RateofProfit Data - ",input$EPWT_group,".csv"),":"," -")},
    content = function(file){
      write.table(data.EPWT.Group()[[input$EPWT_groupAggregate]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Individual Country Profit Rate ########################
  
  ## Data for individual country selection
  data.EPWT.Country <- reactive({
    PWT %>% filter(country==input$EPWT_country,
                   format(input$EPWT_dateStartCountry,format="%Y") <= year & year <= format(input$EPWT_dateEndCountry,format="%Y"))
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.EPWT.Country.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.EPWT.Country()[with(data.EPWT.Country(),order(year)),]$OCR),
               gr_PS = avg_GR(data.EPWT.Country()[with(data.EPWT.Country(),order(year)),]$PS),
               gr_ROP=avg_GR(data.EPWT.Country()[with(data.EPWT.Country(),order(year)),]$r))
  })
  
  EPWT_CountryPlot1 <- reactive({
    ggplot(data = data.EPWT.Country(), 
           aes(x=as.Date(as.character(year), "%Y"), y=r)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$EPWT_trendLineCountry]][1],
                  formula=ui.trendLineList[[input$EPWT_trendLineCountry]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0("Annual Rate of Profit"),
           subtitle=input$EPWT_country) +
      theme_minimal()
  })
  
  ## Plot Country ROP
  output$EPWT_plotCountry1 <- renderPlot({
    EPWT_CountryPlot1()
  })
  
  EPWT_CountryPlot2 <- reactive({
    ggplot(data = data.EPWT.Country.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",
                           format(input$EPWT_dateStartCountry,format="%Y"),
                           " - ",
                           format(input$EPWT_dateEndCountry,format="%Y")))
  })

  ## Plot Country ROP Decomposition
  output$EPWT_plotCountry2 <- renderPlot({
    EPWT_CountryPlot2()
  })
  
  output$EPWT_downloadPlot1Country <- downloadHandler(
    filename = function(){paste0("RateOfProfit - ",input$EPWT_country,".",input$EPWT_fformatCountry)},
    content = function(file) {
      do.call(input$EPWT_fformatCountry,list(file))
      print(EPWT_CountryPlot1())
      dev.off()
    })
  
  output$EPWT_downloadPlot2Country <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - ",input$EPWT_country,".",input$EPWT_fformatCountry)},
    content = function(file) {
      do.call(input$EPWT_fformatCountry,list(file))
      print(EPWT_CountryPlot2())
      dev.off()
    })
  
  output$EPWT_downloadDataCountry <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - ",input$EPWT_country,".csv")},
    content = function(file){
      write.table(data.EPWT.Country() %>%
                    select(year,OCR,PS,r), 
                  file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ######################## Common Functions ################################
  
  ## Download Handler for downloading explanatory pdf
  output$EPWT_downloadExplanationFileGlobal <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  output$EPWT_downloadExplanationFileGroup <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  output$EPWT_downloadExplanationFileCountry <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  
  ## Text to display at bottom of page
  description <- tagList("Data for the profitability analysis reported on this dashboard comes from the ",
                         a("Extended Penn World Table",href="https://sites.google.com/a/newschool.edu/duncan-foley-homepage/home/EPWT"),
                         ". We thank Adalmir Marquetti for sharing the data with us. More details can be found on the downloaded data file. This dashboard has been created and is maintained by Evan Wasner (",
                         a("ewasner@umass.edu",href="mailto:@ewasner@umass.edu"),
                         ") and Deepankar Basu (",
                         a("dbasu@econs.umass.edu",href="mailto:@dbasu@econs.umass.edu"),
                         ").")
  
  ## Shiny cannot use the same output multiple times --> therefore must define three separate outputs to reuse text
  output$EPWT_textGlobal <- renderUI({description})
  output$EPWT_textGroup <- renderUI({description})
  output$EPWT_textCountry <- renderUI({description})
  
  ############################################# WIOT ###############################################
  
  ###################### Global Profit Rate ########################
  
  data.WIOT.Global <- reactive({
    WIOT %>% 
      group_by(year, country) %>% summarize(across(c(LAB, K, CAP, VA), sum)) %>%
      filter(format(input$WIOT_dateStartGlobal,format="%Y") <= year & year <= format(input$WIOT_dateEndGlobal,format="%Y")) %>%
      mutate(OCR=VA/K,
             PS1=CAP/VA,
             ROP1=OCR*PS1,
             Kshare=K/sum(K),
             Yshare=VA/sum(VA)) %>% group_by(year) %>%
      ## Compute weighted average of ROP, PS, and OCR
      summarise(ROP=sum(ROP1*Kshare),
                PS=sum(PS1*Yshare),
                OCR=sum(OCR*Kshare))
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.WIOT.Global.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.WIOT.Global()$OCR),
               gr_PS = avg_GR(data.WIOT.Global()$PS),
               gr_ROP=avg_GR(data.WIOT.Global()$ROP))
  })
  
  WIOT_GlobalPlot1 <- reactive({
    ggplot(data = data.WIOT.Global() %>%
             select(year, ROP), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$WIOT_trendLineGlobal]][1],
                  formula=ui.trendLineList[[input$WIOT_trendLineGlobal]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0("Global Annual Rate of Profit")) +
      theme_minimal()
  })
  
  ## Plot Global ROP
  output$WIOT_plotGlobal1 <- renderPlot({
    WIOT_GlobalPlot1()
  })
  
  WIOT_GlobalPlot2 <- reactive({
    ggplot(data = data.WIOT.Global.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",format(input$WIOT_dateStartGlobal,format="%Y")," - ",format(input$WIOT_dateEndGlobal,format="%Y")))
  })
  
  ## Plot Global ROP Decomposition
  output$WIOT_plotGlobal2 <- renderPlot({
    WIOT_GlobalPlot2()
  })
  
  output$WIOT_downloadPlot1Global <- downloadHandler(
    filename = function(){paste0("RateOfProfit - Global.",input$WIOT_fformatGlobal)},
    content = function(file) {
      do.call(input$WIOT_fformatGlobal,list(file))
      print(WIOT_GlobalPlot1())
      dev.off()
    })
  
  output$WIOT_downloadPlot2Global <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - Global.",input$WIOT_fformatGlobal)},
    content = function(file) {
      do.call(input$WIOT_fformatGlobal,list(file))
      print(WIOT_GlobalPlot2())
      dev.off()
    })
  
  output$WIOT_downloadDataGlobal <- downloadHandler(
    filename = "RateofProfit Data - Global.csv",
    content = function(file){
      write.table(data.WIOT.Global(), file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Industry Profit Rate ########################
  
  output$WIOT_plot <- renderPlot({
    ggplot(data=WIOT%>%filter(country=="USA"),
           aes(x=as.Date(as.character(year), "%Y"), y=K)) +
      geom_line()
  })
  
  ###################### Country Profit Rate ########################
  
  #At the national level: just add the capital stock, profits, labor compensation, etc. into national aggregates
  data.WIOT.Country <- reactive({
    WIOT %>% 
      group_by(year, Name) %>% summarize(across(c(LAB, K, CAP, VA), sum)) %>%
      filter(format(input$WIOT_dateStartCountry,format="%Y") <= year & year <= format(input$WIOT_dateEndCountry,format="%Y"),
             Name==input$WIOT_country) %>%
      mutate(OCR=VA/K,
             PS1=CAP/VA,
             PS2=(VA-LAB)/VA,
             ROP1=OCR*PS1,
             ROP2=OCR*PS2,
             labor.share=LAB/VA,
             r.surplus.1=CAP/LAB,
             r.surplus.2=(VA-LAB)/LAB,
             org.comp=K/LAB,
             Kshare=K/sum(K),
             Yshare=VA/sum(VA))
    
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.WIOT.Country.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.WIOT.Country()$OCR),
               gr_PS = avg_GR(data.WIOT.Country()$PS1),
               gr_ROP=avg_GR(data.WIOT.Country()$ROP1))
  })
  
  WIOT_CountryPlot1 <- reactive({
    ggplot(data = data.WIOT.Country() %>%
             select(year, ROP1), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP1)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$WIOT_trendLineCountry]][1],
                  formula=ui.trendLineList[[input$WIOT_trendLineCountry]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title="Annual Rate of Profit",
           subtitle=input$WIOT_country) +
      theme_minimal()
  })
  
  ## Plot Country ROP
  output$WIOT_plotCountry1 <- renderPlot({
    WIOT_CountryPlot1()
  })
  
  WIOT_CountryPlot2 <- reactive({
    ggplot(data = data.WIOT.Country.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",format(input$WIOT_dateStartCountry,format="%Y")," - ",format(input$WIOT_dateEndCountry,format="%Y")))
  })
  
  ## Plot Country ROP Decomposition
  output$WIOT_plotCountry2 <- renderPlot({
    WIOT_CountryPlot2()
  })
  
  output$WIOT_downloadPlot1Country <- downloadHandler(
    filename = function(){paste0("RateOfProfit - ",input$WIOT_country,".",input$WIOT_fformatCountry)},
    content = function(file) {
      do.call(input$WIOT_fformatCountry,list(file))
      print(WIOT_CountryPlot1())
      dev.off()
    })
  
  output$WIOT_downloadPlot2Country <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - ",input$WIOT_country,".",input$WIOT_fformatCountry)},
    content = function(file) {
      do.call(input$WIOT_fformatCountry,list(file))
      print(WIOT_CountryPlot2())
      dev.off()
    })
  
  output$WIOT_downloadDataCountry <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - ",input$WIOT_country,".csv")},
    content = function(file){
      write.table(data.WIOT.Country() %>%
                    select(year,OCR,PS1,ROP1), 
                  file = file, sep = ",", row.names = FALSE)
    })
  
}

shinyApp(ui = ui, server = server)
