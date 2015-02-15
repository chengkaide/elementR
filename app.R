##############################################################>))))°>
#
# element-R - 14/02/2015
#
# shinydashboard / object version
# 
# charlottesirot@free.fr
# francois.guilhaumon@ird.fr
#
#####################################################################


######################
############ LIBRARIES
######################
library(shiny)
library(shinydashboard)
#library(shinyFiles)
library(shinyBS)

library(dygraphs)
library(datasets)

library(tseries)
library(xts)


######################
############### GLOBAL
######################


#skyn
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "") skin <- "blue"

######################
############ FUNCTIONS
######################


getFolders <- function(d){dir(d)}

getData <- function(p,header=T,sep=";",dec=","){cat("getData : ",p,sep="");t<-read.table(p,header=header,sep=sep,dec=dec);cat("exiting getData ...\n");t}


######################
######### DATA EXAMPLE
######################

nist1 <- read.csv("Data/example_1/calibration/NIST_1.csv",header=T,sep=";",dec=",")

nist1.irts <- irts(nist1$Temps, as.matrix(nist1[,-c(1,4)]))



######################
################### UI
######################

sidebar <- dashboardSidebar(
  #sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Start Project", tabName = "start", icon = icon("home")),
    menuItem("Calibration", icon = icon("bullseye"), tabName = "calibration", badgeLabel = "100%",
             badgeColor = "green"
    ),
    menuItem("Samples", icon = icon("circle"), tabName = "samples", badgeLabel = "0%",
             badgeColor = "yellow"
    ),
    menuItem("Configuration", icon = icon("sliders"),
      menuSubItem("Graphic options", tabName = "graphOptions"),
      menuSubItem("Language", tabName = "lang",icon = icon("flag"))
    ),
    menuItem("Source code for app", icon = icon("file-code-o"),
      href = "https://github.com/charlottesirot/elementR"
    )
  )#,
#sliderInput("slider", "Slider input:", 1, 100, 50)
)

body <- dashboardBody(
tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
	##START
    tabItem("start",
    # Solid backgrounds
      fluidRow(
        box(
          title = list(icon("folder-o"),"New Project"),
          width = 6,
          #background = "aqua",
	  status="primary",
	  solidHeader = TRUE,
	  height=500,
          h4("1. Choose the project folder"),
	  selectInput("folderProjectIn", NULL ,  as.matrix(getFolders("Data")),multiple = FALSE),
	  checkboxInput("projNameIsfolderName", "The project name is the folder name", TRUE)
	),#box
	box(
          title = list(icon("folder"),"Load Project"),
          width = 6,
          #background = "green",
	  solidHeader = TRUE,
	  status="warning",
	  height=500,
          "the 'Load Project' Box"#,getwd()
        )
      )#fluidRow
    ),#tabItem("start"

	##CALIBRATION
   tabItem("calibration",
      fluidRow(
	column(3,
	valueBox("Calibration", "Deal with your calibration files first", icon = icon("bullseye"),width=12,color="green"),#valueBox
	box(
          title = list(icon("file"),"Calibration file selection"),
	  solidHeader = TRUE,
          status = "success",
          width = 12,
          #height = 150,
	  uiOutput("calibrationSelect")
        ),#box
	box(
          title = list(icon("eyedropper"),"Elements selection"),
	  solidHeader = TRUE,
          status = "success",
          width = 12,
          height = 150
        )#box
	),#column
	column(9,
        box(
          title = list(icon("image"),"Profile plot"),
	  solidHeader = TRUE,
          status = "success",
          width = 12,
          dygraphOutput("dygraph"),
          height = 500
        )#box
	)#column
      )#fuildRow
   )#tabItem
    
  )#tabItems,
)#dashboardBody

header <- dashboardHeader(
  title = list(icon("star-half-o"),"element-R")
  #messages,
  #notifications,
  #tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

######################
############### SERVER
######################

server <- function(input, output) {

  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    if (is.null(input$count) || is.null(input$fill))
      return()

    data <- histdata[seq(1, input$count)]
    color <- input$fill
    if (color == "none")
      color <- NULL
    hist(data, col = color, main = NULL)
  })

  output$scatter1 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "blue")
  })

  output$scatter2 <- renderPlot({
    spread <- as.numeric(input$spread) / 100
    x <- rnorm(1000)
    y <- x + rnorm(1000) * spread
    plot(x, y, pch = ".", col = "red")
  })

output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count2, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })

output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })

output$calibrationSelect <- renderUI({

	fold <- input$folderProjectIn
	files <- dir(paste("Data/",fold,"/calibration",sep=""))
	selectInput("calibrationIn", NULL ,  as.matrix(files),multiple = FALSE)

})#output$calibrationSelect


currentcalibrationIRTS <- reactive({
	if (is.null(input$calibrationIn)) return()
	fPath <- paste0("Data/",input$folderProjectIn,"/calibration/",input$calibrationIn)
	f <- getData(fPath)
	irts(f[,"Temps"], as.matrix(f[,-c(1,4)]))
})

  output$dygraph <- renderDygraph({

	#if (is.null(input$calibrationIn)) return()

	#cat(input$calibrationIn,"\n")
	#fPath <- paste0("Data/",input$folderProjectIn,"/calibration/",input$calibrationIn)
	#cat(fPath,"\n")
	#f <- getData(fPath)
	#cat(names(f),"\n")
	#f.irts <- getIRTS(f)
	

    dygraph(currentcalibrationIRTS(), main = "Observed CPS",height=500) %>%
    dyLegend(show = "onmouseover", width = 400) %>%
      #dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      #dyOptions(drawGrid = input$showgrid) %>% 
      dyRangeSelector(dateWindow = NULL, height = 80,
       fillColor = "#1C93A6", strokeColor = "#ACC128", keepMouseZoom = TRUE)
  })
  
  output$from <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(input$dygraph_date_window[[1]], "%d %b %Y")      
  })
  
  output$to <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(input$dygraph_date_window[[2]], "%d %b %Y")
  })

}#eo server





######################
######## CALL shinyApp
######################
shinyApp(ui, server)
