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


###################### Collect Data From PWT Spreadsheet ######################
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
ui.GlobalPlot1Subtitle <- list(All = "Using all available data observations",
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
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      
      ###################### Global Profit Rate ########################
      
      tabPanel("Global",
               sidebarLayout(
                 sidebarPanel(
                   airDatepickerInput("dateStartGlobal",
                                      label = "Start Year",
                                      value = "1950-01-02",
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   airDatepickerInput("dateEndGlobal",
                                      label = "End Year",
                                      value = Sys.Date()-365,
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   selectInput(inputId = "globalAggregate", ## Choose method for aggregating data
                               label = "Choose Method to compute Global Aggregates:",
                               choices = c("Use all available data observations" = "All",
                                           "Use only countries with data observations in all selected years" = "LimitCountries")),
                   
                   selectInput(inputId = "trendLineGlobal", ## Choice of Trend Line for ROP graph
                               label = "Choose a trend line to display:",
                               c("None" = "None",
                                 "Linear Trend" = "Linear",
                                 "Loess Trend" = "Loess",
                                 "Quadratic Trend" = "Quadratic",
                                 "Cubic Trend" = "Cubic")),
                   
                   wellPanel(
                     selectInput("fformatGlobal", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                     downloadButton("downloadDataGlobal", "Download Data"),
                     downloadButton("downloadExplanationFileGlobal", "More Information")
                   )),
                 
                 mainPanel(
                   plotOutput("plotGlobal1"),
                   div(downloadButton("downloadPlot1Global", "Download Plot 1"),style="float:right"),
                   br(),
                   br(),
                   plotOutput("plotGlobal2"),
                   div(downloadButton("downloadPlot2Global", "Download Plot 2"),style="float:right"),
                   br(),
                   br(),
                   uiOutput("textGlobal")
                 ))),
      
      ###################### Group Profit Rate ########################
      
      tabPanel("By Group",
               sidebarLayout(
                 sidebarPanel(
                   airDatepickerInput("dateStartGroup",
                                      label = "Start Year",
                                      value = "1950-01-02",
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   airDatepickerInput("dateEndGroup",
                                      label = "End Year",
                                      value = Sys.Date()-365,
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   selectInput(inputId = "group", ## Income Group
                               label = "Choose Income Group:",
                               choices = sort(unique(PWT$wb_income_group)[2:6]), # First value is NA
                               selected = "High income: OECD"),
                   
                   selectInput(inputId = "groupAggregate", ## Choose method for aggregating data
                               label = "Choose Method to compute Group Aggregates:",
                               choices = c("Use all available data observations" = "All",
                                           "Use only countries with data observations in all selected years" = "LimitCountries")),
                   
                   selectInput(inputId = "trendLineGroup", ## Choice of Trend Line for ROP graph
                               label = "Choose a trend line to display:",
                               c("None" = "None",
                                 "Linear Trend" = "Linear",
                                 "Loess Trend" = "Loess",
                                 "Quadratic Trend" = "Quadratic",
                                 "Cubic Trend" = "Cubic")),
                   
                   wellPanel(
                     selectInput("fformatGroup", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                     downloadButton("downloadDataGroup", "Download Data"),
                     downloadButton("downloadExplanationFileGroup", "More Information")
                   )),
                 
                 mainPanel(
                   plotOutput("plotGroup1"),
                   div(downloadButton("downloadPlot1Group", "Download Plot 1"),style="float:right"),
                   br(),
                   br(),
                   plotOutput("plotGroup2"),
                   div(downloadButton("downloadPlot2Group", "Download Plot 2"),style="float:right"),
                   br(),
                   br(),
                   uiOutput("textGroup")
                 ))),
      
      ###################### Country Profit Rate ########################
      
      tabPanel("By Country",
               sidebarLayout(
                 sidebarPanel(
                   airDatepickerInput("dateStartCountry",
                                      label = "Start Year",
                                      value = "1950-01-02",
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   airDatepickerInput("dateEndCountry",
                                      label = "End Year",
                                      value = Sys.Date()-365,
                                      maxDate = Sys.Date()-365,
                                      minDate = "1950-01-02",
                                      view = "years", #editing what the popup calendar shows when it opens
                                      minView = "years", #making it not possible to go down to a "days" view and pick the wrong date
                                      dateFormat = "yyyy"
                   ),
                   
                   selectInput(inputId = "country", ## Country
                               label = "Choose Country:",
                               choices = sort(unique(PWT$country)),
                               selected = "United States"),
                   
                   selectInput(inputId = "trendLineCountry", ## Choice of Trend Line for ROP graph
                               label = "Choose a trend line to display:",
                               c("None" = "None",
                                 "Linear Trend" = "Linear",
                                 "Loess Trend" = "Loess",
                                 "Quadratic Trend" = "Quadratic",
                                 "Cubic Trend" = "Cubic")),
                   
                   wellPanel(
                     selectInput("fformatCountry", "Download Plot File Type", choices=c("png","tiff","jpeg","pdf")),
                     downloadButton("downloadDataCountry", "Download Data"),
                     downloadButton("downloadExplanationFileCountry", "More Information")
                   )),
                 
                 mainPanel(
                   plotOutput("plotCountry1"),
                   div(downloadButton("downloadPlot1Country", "Download Plot 1"),style="float:right"),
                   br(),
                   br(),
                   plotOutput("plotCountry2"),
                   div(downloadButton("downloadPlot2Country", "Download Plot 2"),style="float:right"),
                   br(),
                   br(),
                   uiOutput("textCountry")
                 )
               )
      )
    )
  )
)

server <- function(input, output) {
  
  ###################### Global Profit Rate ########################
  
  ## Global data - Using all available data observations from each year
  data.Global.All <- reactive({
    PWT %>% filter(format(input$dateStartGlobal,format="%Y") <= year & year <= format(input$dateEndGlobal,format="%Y")) %>%
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
  data.Global.LimitCountries <- reactive({
    data <- PWT %>% filter(format(input$dateStartGlobal,format="%Y") <= year & year <= format(input$dateEndGlobal,format="%Y"))
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
  data.Global <- reactive({
    list(All=data.Global.All(),
         LimitCountries=data.Global.LimitCountries())
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.Global.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.Global()[[input$globalAggregate]]$OCR),
               gr_PS = avg_GR(data.Global()[[input$globalAggregate]]$PS),
               gr_ROP=avg_GR(data.Global()[[input$globalAggregate]]$ROP))
  })
  
  GlobalPlot1 <- reactive({
    ggplot(data = data.Global()[[input$globalAggregate]]  %>%
             select(year, ROP), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$trendLineGlobal]][1],
                  formula=ui.trendLineList[[input$trendLineGlobal]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0("Global Annual Rate of Profit"),
           subtitle=ui.GlobalPlot1Subtitle[[input$globalAggregate]]) +
      theme_minimal()
  })
  
  ## Plot Global ROP
  output$plotGlobal1 <- renderPlot({
    GlobalPlot1()
  })
  
  GlobalPlot2 <- reactive({
    ggplot(data = data.Global.GR() %>%
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
           subtitle=paste0("Average Rates of Growth: ",format(input$dateStartGlobal,format="%Y")," - ",format(input$dateEndGlobal,format="%Y")))
  })
  
  ## Plot Global ROP Decomposition
  output$plotGlobal2 <- renderPlot({
    GlobalPlot2()
  })
  
  output$downloadPlot1Global <- downloadHandler(
    filename = function(){paste0("RateOfProfit - Global.",input$fformatGlobal)},
    content = function(file) {
      do.call(input$fformatGlobal,list(file))
      print(GlobalPlot1())
      dev.off()
    })
  
  output$downloadPlot2Global <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - Global.",input$fformatGlobal)},
    content = function(file) {
      do.call(input$fformatGlobal,list(file))
      print(GlobalPlot2())
      dev.off()
    })
  
  output$downloadDataGlobal <- downloadHandler(
    filename = "RateofProfit Data - Global.csv",
    content = function(file){
      write.table(data.Global()[[input$globalAggregate]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Income Group Profit Rate ########################
  
  ## Group data - Using all available data observations from each year
  data.Group.All <- reactive({
    PWT %>% filter(wb_income_group==input$group,
                   format(input$dateStartGroup,format="%Y") <= year & year <= format(input$dateEndGroup,format="%Y")) %>%
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
  data.Group.LimitCountries <- reactive({
    ## Filter for countries which have observations for every year in selected date range
    data <- PWT %>% filter(wb_income_group==input$group,
                           format(input$dateStartGroup,format="%Y") <= year & year <= format(input$dateEndGroup,format="%Y"))
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
  data.Group <- reactive({
    list(All=data.Group.All(),
         LimitCountries=data.Group.LimitCountries())
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.Group.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.Group()[[input$groupAggregate]]$OCR),
               gr_PS = avg_GR(data.Group()[[input$groupAggregate]]$PS),
               gr_ROP=avg_GR(data.Group()[[input$groupAggregate]]$ROP))
  })
  
  GroupPlot1 <- reactive({
    ggplot(data = data.Group()[[input$groupAggregate]] %>%
             select(year, ROP), 
           aes(x=as.Date(as.character(year), "%Y"), y=ROP)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$trendLineGroup]][1],
                  formula=ui.trendLineList[[input$trendLineGroup]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0(input$group," Annual Rate of Profit"),
           subtitle=ui.GlobalPlot1Subtitle[[input$groupAggregate]]) +
      theme_minimal()
  })
  
  ## Plot Group ROP
  output$plotGroup1 <- renderPlot({
    GroupPlot1()
  })
  
  GroupPlot2 <- reactive({
    ggplot(data = data.Group.GR() %>%
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
                           format(input$dateStartGroup,format="%Y"),
                           " - ",
                           format(input$dateEndGroup,format="%Y")))
  })
  
  ## Plot Group ROP Decomposition
  output$plotGroup2 <- renderPlot({
    GroupPlot2()
  })
  
  output$downloadPlot1Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfit - ",input$group,".",input$fformatGroup),":"," -")},
    content = function(file) {
      do.call(input$fformatGroup,list(file))
      print(GroupPlot1())
      dev.off()
    })
  
  output$downloadPlot2Group <- downloadHandler(
    filename = function(){str_replace(paste0("RateOfProfitDecomposition - ",input$group,".",input$fformatGroup),":"," -")},
    content = function(file) {
      do.call(input$fformatGroup,list(file))
      print(GroupPlot2())
      dev.off()
    })
  
  output$downloadDataGroup <- downloadHandler(
    filename = function(){str_replace(paste0("RateofProfit Data - ",input$group,".csv"),":"," -")},
    content = function(file){
      write.table(data.Group()[[input$groupAggregate]], file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ###################### Individual Country Profit Rate ########################
  
  ## Data for individual country selection
  data.Country <- reactive({
    PWT %>% filter(country==input$country,
                   format(input$dateStartCountry,format="%Y") <= year & year <= format(input$dateEndCountry,format="%Y"))
  })
  
  ## Create df with average annual growth rates for OCR, PS, and ROP
  data.Country.GR <- reactive({
    data.frame(gr_OCR = avg_GR(data.Country()[with(data.Country(),order(year)),]$OCR),
               gr_PS = avg_GR(data.Country()[with(data.Country(),order(year)),]$PS),
               gr_ROP=avg_GR(data.Country()[with(data.Country(),order(year)),]$r))
  })
  
  CountryPlot1 <- reactive({
    ggplot(data = data.Country(), 
           aes(x=as.Date(as.character(year), "%Y"), y=r)) + 
      geom_line() +
      geom_smooth(method=ui.trendLineList[[input$trendLineCountry]][1],
                  formula=ui.trendLineList[[input$trendLineCountry]][2],
                  se=FALSE,
                  linetype="dashed") +
      labs(x="Year",
           y="Percentage",
           title=paste0("Annual Rate of Profit"),
           subtitle=input$country) +
      theme_minimal()
  })
  
  ## Plot Country ROP
  output$plotCountry1 <- renderPlot({
    CountryPlot1()
  })
  
  CountryPlot2 <- reactive({
    ggplot(data = data.Country.GR() %>%
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
                           format(input$dateStartCountry,format="%Y"),
                           " - ",
                           format(input$dateEndCountry,format="%Y")))
  })

  ## Plot Country ROP Decomposition
  output$plotCountry2 <- renderPlot({
    CountryPlot2()
  })
  
  output$downloadPlot1Country <- downloadHandler(
    filename = function(){paste0("RateOfProfit - ",input$country,".",input$fformatCountry)},
    content = function(file) {
      do.call(input$fformatCountry,list(file))
      print(CountryPlot1())
      dev.off()
    })
  
  output$downloadPlot2Country <- downloadHandler(
    filename = function(){paste0("RateOfProfitDecomposition - ",input$country,".",input$fformatCountry)},
    content = function(file) {
      do.call(input$fformatCountry,list(file))
      print(CountryPlot2())
      dev.off()
    })
  
  output$downloadDataCountry <- downloadHandler(
    filename = function(){paste0("RateofProfit Data - ",input$country,".csv")},
    content = function(file){
      write.table(data.Country() %>%
                    select(year,OCR,PS,r), 
                  file = file, sep = ",", row.names = FALSE)
    }
  )
  
  ######################## Common Functions ################################
  
  ## Download Handler for downloading explanatory pdf
  output$downloadExplanationFileGlobal <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  output$downloadExplanationFileGroup <- downloadHandler(
    filename = "world-profitability.pdf",
    content = function(file) {
      file.copy("world-profitability.pdf", file)
    })
  output$downloadExplanationFileCountry <- downloadHandler(
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
  output$textGlobal <- renderUI({description})
  output$textGroup <- renderUI({description})
  output$textCountry <- renderUI({description})
  
}

shinyApp(ui = ui, server = server)
