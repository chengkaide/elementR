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

library(dygraphs)

#imported from classElementR_R6.R
#library(R6) #shiny depends
#library(tseries)
#library(xts)

######################
###### CLASSES IMPORTS
######################
source("classElementR_R6.R")


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
	fluidRow(
		box(
			#title = list(icon("home"),"Welcome to element-R"),
			width=12,
			background = "light-blue",
			#height=100,
			h3(icon("flask"),"Choose to create a new project or load an existing one")
		)#box
	),#fluidRow
      fluidRow(
        box(
          title = list(icon("folder-o"),"New Project"),
          width = 6,
          #background = "aqua",
	  status="primary",
	  solidHeader = TRUE,
	  height=300,
		h4("1. Choose the project folder"),
		selectInput("folderProjectIn", NULL ,  as.matrix(dir("Data")),multiple = FALSE),
		#checkboxInput("projNameIsfolderName", "The project name is the folder name", TRUE),
		h4("2. Create the project"),
		actionButton("createProjButton", "Create project !")
	),#box
	box(
          title = list(icon("folder"),"Load Project"),
          width = 6,
          #background = "green",
	  solidHeader = TRUE,
	  status="primary",
	  height=300,
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
          dygraphOutput("dygraph", height = 500),
          height = 1000
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

output$calibrationSelect <- renderUI({
	if (is.null(currentProject())) return()
	files <- currentProject()$calibrationsFiles
	selectInput("calibrationIn", NULL ,  as.matrix(files),multiple = FALSE)

})#output$calibrationSelect


currentProject <- reactive({

	input$createProjButton

	if (input$createProjButton==0) return(NULL)

	isolate({
		input$createProjButton
		cat("you pressed the button")
		elementR_project$new(paste("Data/",input$folderProjectIn,sep=""))
	})
	
})

#currentProjectCreate()

output$dygraph <- renderDygraph({

	if (is.null(currentProject())) return()
	if (is.null(input$calibrationIn)) return()

	#cat(input$calibrationIn,"\n")
	#fPath <- paste0("Data/",input$folderProjectIn,"/calibration/",input$calibrationIn)
	#cat(fPath,"\n")
	#f <- getData(fPath)
	#cat(names(f),"\n")
	#f.irts <- getIRTS(f)


	dygraph(currentProject()$calibrations[[input$calibrationIn]]$dataIRTS, main = "Observed CPS",height=700) %>%
	dyLegend(show = "onmouseover", width = 400) %>%
	#dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
	#dyOptions(drawGrid = input$showgrid) %>% 
	dyRangeSelector(dateWindow = NULL, height = 300,
	fillColor = "#1C93A6", strokeColor = "#ACC128", keepMouseZoom = TRUE)
})


  
}#eo server





######################
######## CALL shinyApp
######################
shinyApp(ui, server)
