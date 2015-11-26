##############################################################>))))°>
#
# element-R - 12/11/2015
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
library(devtools)
library(R6)
library(shinydashboard)
library(tseries)
library(xts)
library("graphics")
library("gplots")
library("stringr")
library("abind")
library(stringr)
library(lmtest)

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

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-up:auto;margin-down:auto;"
  )
}

PlotIC <- function(nom, Mean,SD, lengthSeg, xlim, ylim, type = "p", xlab, ylab){
  plot(as.factor(nom), rep(-1,length(nom)), ylim = ylim, xlim = xlim, type = type, xlab = xlab, ylab = ylab)
  points(1:length(Mean),Mean)
  segments(1:length(Mean), Mean-SD, 1:length(Mean), Mean+SD)
  segments((1:length(Mean))-lengthSeg,Mean+SD,(1:length(Mean))+lengthSeg,Mean+SD)
  segments((1:length(Mean))-lengthSeg,Mean-SD,(1:length(Mean))+lengthSeg,Mean-SD)
}

# Choix = vecteur d'élements à choisir, longueurDesirer = longueur du mot, combinaisonDesirer = nombre de combinaisons désirées
geneR = function(choix, longueurDesirer, combinaisonDesirer){
  
  temp <- vector()
  
  nombreMax = length(choix)^longueurDesirer
  
  if(combinaisonDesirer > nombreMax){print("Impossible")}
  else{
    while(length(temp) != combinaisonDesirer){
      
      nom <- paste(sample(choix, longueurDesirer, replace = T), collapse = "")
      
      if(length(which(temp == nom) != 0) ){}
      else{ temp <- c(temp, nom)}
      
    }
    
    return(temp)
  }
  
  
}

letterChoice <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")

######################
################### UI
######################

sidebar <- dashboardSidebar(
  #sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Start Project", tabName = "start", icon = icon("home")),
    menuItem("Calibration", icon = icon("bullseye"), 
             
             menuSubItem("Filtering data", tabName = "Standards", icon = icon("flask")),
             menuSubItem("Concentration conversion", tabName = "Etalon", icon = icon("folder-open"))
    ),
    menuItem("Samples", icon = icon("circle"), tabName = "Samples", 
             menuSubItem("Filtering data", tabName = "Samples", icon = icon("flask")),
             menuSubItem("Realignement", tabName = "realign", icon = icon("folder-open"))
    ),
    menuItem("Configuration", icon = icon("sliders"),
             menuSubItem("Project export", tabName = "projectExport"),
             menuSubItem("Graphic options", tabName = "graphOptions"),
             menuSubItem("Language", tabName = "lang",icon = icon("flag"))
    ),
    menuItem("Source code for app", icon = icon("file-code-o"),
             href = "https://github.com/charlottesirot/elementR"
    )
  ),
  br(),
  br(),
  column(width = 1, offset = 2,
         uiOutput("Export")
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    ##START
    tabItem("start",
            uiOutput("premier"),
            
            fluidRow(
              
              uiOutput("ChoixDonne1"),
              uiOutput("ChoixDonne2")
            )
            
            
            
    ),#tabItem("start"
    
    ##CALIBRATION
    tabItem("Standards",
            uiOutput("Nist"),            
            uiOutput("Nist2")
            
            
            
    ),#tabItem
    tabItem("Etalon",
            uiOutput("Text1"),
            uiOutput("Text2"),
            uiOutput("Text2Bis"),
            br(),            
            fluidRow(
              uiOutput("Text3")
            ),  #fuildRow
            fluidRow(
              tableOutput("tableEtalon")
              
            )  #fuildRow
            
            
    ), #tabItem
    
    tabItem("Samples",
            box(width = 12,background = "navy", height = 100,
                
                column(3,                 
                       uiOutput("sample1")
                ), # Column
                
                column(2,
                       br(),
                       uiOutput("sample2")
                ), # Column
                column(2,
                       br(),
                       uiOutput("sample3")
                ), # Column
                column(1,
                       br(),
                       uiOutput("sample4")
                ),
                column(1,
                       uiOutput("sample6")
                )
                
            ),
            column(12,
                   uiOutput("Sample5"))
    ), # tabItem
    
    tabItem("realign",
            fluidRow(
              box(background = "black", width = 12, height = 100,
                  column(4,
                         uiOutput("textRealign")
                  ), # column 
                  column(3, 
                         uiOutput("textRealignBis")
                  ), # column
                  column(3,
                         uiOutput("textRealignBis2")
                  )               
              ) # box              
            ), # fluidRow
            fluidRow(
              uiOutput("textRealign2"),
              uiOutput("textRealign7"),
              
              div(style='overflow-y: scroll',
                  tableOutput("textRealign3")
              ), # div
              div(style='overflow-y: scroll',
                  plotOutput("textRealign5")
              ) # div       
            ) # fluidRow
    )
  )#tabItem
)#dashboardBody

header <- dashboardHeader(
  title = list(icon("star-half-o"),"element-R")
  #messages,
  #notifications,
  #tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output, session) {  
  
  observe({
    if(dataChoisies$temp == 0){
      output$Export = renderUI({
        NULL
      })      
    }
    else{
      output$Export = renderUI({
        actionButton("export","Export Project")
      })
    }
  })
  
  observe({
    if(is.null(input$export)){}
    else{
      if(input$export > 0){
        
        espace1 = getwd()
        dir.create(paste0("Data/",input$folderProjectIn,"/Done"))
        setwd(paste0("Data/",input$folderProjectIn,"/Done"))
        
        myProject <- currentProject()
        save(myProject, file = paste0(input$folderProjectIn, ".RData"))
        
        if(is.na(currentProject()$SummaryNist)){}
        else{
          write.csv(currentProject()$SummaryNist, file = "Summary.csv")
        }
        
        setwd(espace1)
        dir.create(paste0("Data/",input$folderProjectIn,"/Done/NIST"))
        setwd(paste0("Data/",input$folderProjectIn,"/Done/NIST"))
        
        lapply(1:length(currentProject()$calibrations[[1]]$rep_Files),function(x){
          if(currentProject()$flag_Calib[x] == 0){}
          else{
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataBlanc, file = paste0("data_blanc_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataplateau, file = paste0("data_plateau_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataPlateauMoinsBlanc, file = paste0("dataPlateauMoinsBlanc_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataPlateauMoinsBlancSupLOD, file = paste0("dataPlateauMoinsBlancSupLOD_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataPlateauMoinsBlancSupLODNorm, file = paste0("dataPlateauMoinsBlancSupLODNorm_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))
            write.csv(currentProject()$calibrations[[1]]$rep_data[[x]]$dataPlateauMoinsBlancSupLODNormSansAnom, file = paste0("dataPlateauMoinsBlancSupLODNormSansAnom_",currentProject()$calibrations[[1]]$rep_Files[x],".csv"))            
          }
        }) # eo lapply
        
        lapply(1:length(currentProject()$samplesFiles), function(x){
          setwd(espace1)
          dir.create(paste0("Data/",input$folderProjectIn,"/Done/",currentProject()$samplesFiles[x]))
          
          lapply(1:length(currentProject()$samples[[x]]$rep_Files), function(y){
            setwd(espace1)
            temporaire <- strsplit(currentProject()$samples[[x]]$rep_Files[y],".csv")
            dir.create(paste0("Data/",input$folderProjectIn,"/Done/",currentProject()$samplesFiles[x],"/",temporaire))
            setwd(paste0("Data/",input$folderProjectIn,"/Done/",currentProject()$samplesFiles[x],"/",temporaire))
            if(currentProject()$flag_Sample[[x]][y] == 0){}
            else{
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataBlanc, file = paste0("data_blanc_",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataplateau, file = paste0("dataplateau_",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateauMoinsBlanc, file = paste0("dataPlateauMoinsBlanc_",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateauMoinsBlancSupLOD, file = paste0("dataPlateauMoinsBlancSupLOD_",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateauMoinsBlancSupLODNorm, file = paste0("dataPlateauMoinsBlancSupLODNorm__",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateauMoinsBlancSupLODNormConc, file = paste0("dataPlateauMoinsBlancSupLODNormConc_",temporaire,".csv"))
              write.csv(currentProject()$samples[[x]]$rep_data[[y]]$dataPlateauMoinsBlancSupLODNormConcCorr, file = paste0("dataPlateauMoinsBlancSupLODNormConcCorr_",temporaire,".csv"))
            }
          }) #eo lapply
          
          setwd(paste0(espace1,"/","Data/",input$folderProjectIn,"/Done/"))
          if(!is.na(currentProject()$machineCorrection)){
            tempo <- matrix(unlist(currentProject()$machineCorrection), ncol = 2, byrow = T)
            rownames(tempo) <- currentProject()$listeElem
            colnames(tempo) <- c("intercept","slope")
            write.csv(tempo, file = "regression parameters.csv")                      
          }
          
          setwd(paste0(espace1,"/","Data/",input$folderProjectIn,"/Done/",currentProject()$samplesFiles[x]))
          if(is.na(currentProject()$samples[[x]]$rep_type2)){}
          else{
            if(currentProject()$samples[[x]]$rep_type2 == "spot"){
              write.csv(currentProject()$samples[[x]]$rep_dataIntermSpotBis, file = paste0("final_",currentProject()$samplesFiles[x],".csv"))
            }
            if(currentProject()$samples[[x]]$rep_type2 == "raster"){
              write.csv(currentProject()$samples[[x]]$rep_dataFinaleCorrel, file = paste0("finalCorrel_",currentProject()$samplesFiles[x],".csv"))
#               write.csv(currentProject()$samples[[x]]$rep_dataNonCorrel, file = paste0("finalNonCorrel_",currentProject()$samplesFiles[x],".csv"))
            } 
          }
          
        }) #eo lapply
        
        setwd(espace1)
      } # if
    } #else
  }) #observe
  
  observe({ 
    
    input$createProjButton
    
    if(is.null(data$temp)){
      output$premier = renderUI({
        fluidRow(
          box(
            background = "light-blue",
            height = 100,
            width = 12,
            column(9,                       
                   h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one")
            )
          )#box
        )#fluidRow
      })
    }
    else{
      if(dataChoisies$temp == 0 & currentProject()$elementChecking[1] == 1){
        output$premier = renderUI({
          fluidRow(
            box(
              background = "light-blue",
              height = 100,
              width = 12,
              column(9,                       
                     h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one")
              )
            )#box
          )#fluidRow
        })
      }
      if(dataChoisies$temp == 1){
        output$premier = renderUI({
          fluidRow(
            box(
              background = "light-blue",
              height = 100,
              width = 12,
              column(9,                       
                     h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one")
              ),
              column(3, 
                     br(),
                     actionButton("SuppDonne","Delete Data")
              ) 
            )#box
          )#fluidRow
        })
      }
      if(dataChoisies$temp == 0 & currentProject()$elementChecking[1] == 0 & is.null(currentProject()$errorSession)){
        output$premier = renderUI({
          fluidRow(
            box(
              background = "light-blue",
              height = 100,
              width = 12,
              column(9,                       
                     h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one")
              ),
              column(3, 
                     br(),
                     actionButton("validDonne","Go filtering !")
              ) 
            )#box
          )#fluidRow
        })
      }
      
    }
    
    
  })
  
  dataChoisies <- reactiveValues(temp = 0)
  
  observe({
    if(is.null(input$validDonne)){}
    else{
      if(input$validDonne != 0){
        dataChoisies$temp <- 1
      }
    }
  })
  
  observe({
    if(is.null(input$SuppDonne)){}
    else{
      if(input$SuppDonne != 0){
        dataChoisies$temp <- 0
      }
    }
    
  })
  
  observe({  
    
    if(dataChoisies$temp == 0){
      
      output$ChoixDonne1 <- renderUI({
        
        box(
          title = list(icon("folder-o"),"New Project"),
          width = 6,
          #background = "aqua",
          status="primary",
          solidHeader = TRUE,
          height=250,
          h4("1. Choose the project folder"),
          selectInput("folderProjectIn", NULL ,  as.matrix(dir("Data")),multiple = FALSE),
          h4("2. Create the project"),
          actionButton("createProjButton", "Create project !")
        )
        
      })
      output$ChoixDonne2 <- renderUI({
        
        box(
          title = list(icon("folder"),"Load Project"),
          width = 6,
          #background = "green",
          solidHeader = TRUE,
          status="primary",
          height=200,
          
          h4("1. Choose a project to load"),
          actionButton("loadProjButton","Load Project")
          
        )
        
      })
      
      observe({
        if(is.null(input$loadProjButton)){}
        else{
          
          output$ChoixDonne2 <- renderUI({
            
            box(
              title = list(icon("folder"),"Load Project"),
              width = 6,
              #background = "green",
              solidHeader = TRUE,
              status="primary",
              height=200,
              
              h4("1. Choose a project to load"),
              actionButton("loadProjButton","Load Project"),
              br(),
              br(),
              h4(paste0("Fichier chargé : ",nomData$temp))
            )
            
          })
          
        }
        
      })
      
    } # if
    
    if(dataChoisies$temp == 1){ 
      
      observe({
        if(isolate(temoin$temp)[[1]] == 1){
          
          output$ChoixDonne1 <- renderUI({
            
            box(
              title = list(icon("folder-o"),"Selected Data"),
              width = 12,
              #background = "aqua",
              status="primary",
              solidHeader = TRUE,
              height=400,
              h3(icon("check"), temoin$temp[[2]]),
              h3(icon("check"), paste0("Name: ", temoin$temp[[3]])),
              h3(icon("check"), paste0("Path: ", temoin$temp[[4]])),
              h3(icon("check"), paste0("Calibration: ", paste0(temoin$temp[[5]], collapse = " "))),
              h3(icon("check"), paste0("Sample: ", paste0(temoin$temp[[6]], collapse = " "))),
              h3(icon("check"), paste0("Considered elements: ", paste0(temoin$temp[[7]], collapse = " ")))
            )
            
          })
          
          output$ChoixDonne2 <- renderUI({})
          
        }
        if(isolate(temoin$temp)[[1]] == 2){
          
          output$ChoixDonne1 <- renderUI({
            
            box(
              title = list(icon("folder-o"),"Selected Data"),
              width = 12,
              #background = "aqua",
              status="primary",
              solidHeader = TRUE,
              height=450,
              h3(icon("check"), temoin$temp[[2]]),
              h3(icon("check"), paste0("Save name: ", temoin$temp[[3]])),
              h3(icon("check"), paste0("Session name: ", temoin$temp[[4]])),
              h3(icon("check"), paste0("Path: ", temoin$temp[[5]])),
              h3(icon("check"), paste0("Calibration: ", paste0(temoin$temp[[6]], collapse = " "))),
              h3(icon("check"), paste0("Sample: ", paste0(temoin$temp[[7]], collapse = " "))),
              h3(icon("check"), paste0("Considered elements: ", paste0(temoin$temp[[9]], collapse = " ")))
            )
            
          })
          
          output$ChoixDonne2 <- renderUI({})
          
        }
      })
      
    }
    
  })
  
  #################################
  ########### Création Données ####
  #################################
  
  data = reactiveValues(temp = NULL)
  nomData = reactiveValues(temp = NULL)
  temoin = reactiveValues(temp = NULL)
  
  observe({
    if(is.null(input$createProjButton)){}
    else{
      input$createProjButton
      
      if (input$createProjButton!=0) {sauvegarde = getwd()
                                      
                                      data$temp <- elementR_project$new(paste("Data/",input$folderProjectIn,sep=""))
                                      
                                      temoin$temp = list(1, "Création projet", input$folderProjectIn, paste("Data/",input$folderProjectIn,sep=""), dir(paste("Data/",input$folderProjectIn,"/calibrations",sep="")), dir(paste("Data/",input$folderProjectIn,"/samples",sep="")), data$temp$listeElem)
                                      
                                      if(currentProject()$elementChecking[1] == 0 & is.null(currentProject()$errorSession)){ 
                                        
                                        
                                        
                                        output$ChoixDonne1 <- renderUI({
                                          
                                          box(
                                            title = list(icon("folder-o"),"New Project"),
                                            width = 6,
                                            #background = "aqua",
                                            status="primary",
                                            solidHeader = TRUE,
                                            height=520,
                                            h4("1. Choose the project folder"),
                                            selectInput("folderProjectIn", NULL ,  as.matrix(dir(paste0(sauvegarde,"/Data/"))),multiple = FALSE),
                                            h4("2. Create the project"),
                                            actionButton("createProjButton", "Create project !"),
                                            br(),
                                            br(),
                                            h4("3. Checking elements"),
                                            h4(icon("check"), "Elements checked"),
                                            br(),
                                            h4("4. Choose elements to consider"),
                                            checkboxGroupInput("ElementGroup", label = "", 
                                                               choices = colnames(read.csv(paste(getwd(),"Data/",input$folderProjectIn, "/calibrations",dir(paste0("Data/",input$folderProjectIn, "/calibrations"))[1],sep="/"), sep = ";", h = T, dec ="."))[-1],
                                                               selected = colnames(read.csv(paste(getwd(),"Data/",input$folderProjectIn, "/calibrations",dir(paste0("Data/",input$folderProjectIn, "/calibrations"))[1],sep="/"), sep = ";", h = T, dec ="."))[-1], inline = T),                                            
                                            br(),
                                            h4("5. Verification of the non-numeric character of the data"),
                                            h4(icon("check"), "Data checked")
                                          )
                                          
                                        })
                                      }
                                      if(currentProject()$elementChecking[1] == 1 & is.null(currentProject()$errorSession)){
                                        
                                        output$ChoixDonne1 <- renderUI({
                                          box(
                                          title = list(icon("folder-o"),"New Project"),
                                          width = 6,
                                          #background = "aqua",
                                          status="primary",
                                          solidHeader = TRUE,
                                          height=380,
                                          h4("1. Choose the project folder"),
                                          selectInput("folderProjectIn", NULL ,  as.matrix(dir(paste0(sauvegarde,"/Data/"))),multiple = FALSE),
                                          h4("2. Create the project"),
                                          actionButton("createProjButton", "Create project !"),
                                          h4("3. Checking elements"),
                                          h4(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[2]))),
                                          br(),
                                          h4("5. Verification of the non-numeric character of the data"),
                                          h4(icon("check"), "Data checked")
                                          )
                                        })
                                        
                                      }
                                      if(currentProject()$elementChecking[1] == 1 & !is.null(currentProject()$errorSession)){
                                        
                                        output$ChoixDonne1 <- renderUI({
                                          box(
                                          title = list(icon("folder-o"),"New Project"),
                                          width = 6,
                                          #background = "aqua",
                                          status="primary",
                                          solidHeader = TRUE,
                                          height=380,
                                          h4("1. Choose the project folder"),
                                          selectInput("folderProjectIn", NULL ,  as.matrix(dir(paste0(sauvegarde,"/Data/"))),multiple = FALSE),
                                          h4("2. Create the project"),
                                          actionButton("createProjButton", "Create project !"),
                                          h4("3. Checking elements"),
                                          h4(icon("times"), paste0("Problem in ", paste(currentProject()$elementChecking[2]))),
                                          br(),
                                          h4("5. Verification of the non-numeric character of the data"),
                                          h4(icon("times"), paste0("Problem in ", paste0(currentProject()$errorSession, collapse = " ")))
                                          )
                                        })
                                        
                                      }
                                      if(currentProject()$elementChecking[1] == 0 & !is.null(currentProject()$errorSession)){
                                        output$ChoixDonne1 <- renderUI({
                                          
                                          box(
                                            title = list(icon("folder-o"),"New Project"),
                                            width = 6,
                                            #background = "aqua",
                                            status="primary",
                                            solidHeader = TRUE,
                                            height=520,
                                            h4("1. Choose the project folder"),
                                            selectInput("folderProjectIn", NULL ,  as.matrix(dir(paste0(sauvegarde,"/Data/"))),multiple = FALSE),
                                            h4("2. Create the project"),
                                            actionButton("createProjButton", "Create project !"),
                                            br(),
                                            br(),
                                            h4("3. Checking elements"),
                                            h4(icon("check"), "Elements checked"),
                                            br(),
                                            h4("4. Choose elements to consider"),
                                            checkboxGroupInput("ElementGroup", label = "", 
                                                               choices = colnames(read.csv(paste(getwd(),"Data/",input$folderProjectIn, "/calibrations",dir(paste0("Data/",input$folderProjectIn, "/calibrations"))[1],sep="/"), sep = ";", h = T, dec ="."))[-1],
                                                               selected = colnames(read.csv(paste(getwd(),"Data/",input$folderProjectIn, "/calibrations",dir(paste0("Data/",input$folderProjectIn, "/calibrations"))[1],sep="/"), sep = ";", h = T, dec ="."))[-1], inline = T),                                            
                                            br(),
                                            h4("5. Verification of the non-numeric character of the data"),
                                            h4(icon("times"), paste0("Problem in ", paste0(currentProject()$errorSession, collapse = " ")))
                                          )
                                          
                                        })
                                      }

                                      
      }
      
      updateSelectInput(session, "folderProjectIn", selected = input$folderProjectIn)
      
    }
    
  })
  
  observe({
    if(is.null(input$loadProjButton)){}
    else{
      input$loadProjButton
      
      if (input$loadProjButton!=0) {
        
        nomData$temp <- file.choose()
        
        load(nomData$temp)
        
        data$temp <- myProject
        
        temoin$temp = list(2, "Chargement d'une session déjà traitée", nomData$temp, data$temp$name, data$temp$folderPath, data$temp$calibrationsFiles, data$temp$samplesFiles, data$temp$EtalonName, data$temp$listeElem)
        
      }  
    }
  })
  
  currentProject <- reactive({
    if(is.null(input$createProjButton)){}
    if(is.null(input$loadProjButton)){}
    else{
      input$create
      input$load
      
      data$temp
    }    
  })
  
  #######################
  ##### NISTS ###########
  #######################
  
  doneSample <- reactiveValues(temp = NULL)
  
  observe({
    if(is.null(currentProject())){}
    if(is.null(input$validDonne)){}
    else{
      if(input$validDonne == 0){
        
        output$Nist = renderUI({NULL})
        
        output$Nist2 = renderUI({NULL})
        
      }
      if(input$validDonne > 0 ){
        
        Temp0 <- reactiveValues(t = NULL)
        Temp1 <- reactiveValues(t = NULL)
        Temp2 <- reactiveValues(t = NULL)       
        
        currentProject()$setelem(input$ElementGroup)
        
        dataPlot2 <- reactiveValues(dat = NULL)
        
        observe({ 
          if(is.null(currentProject())){}
          if(is.null(input$CourbeNIST)){}
          if(is.null(input$bins)){}
          if(is.null(input$plat)){}
          else{
            currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setVector(bins =currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], plat = input$plat)
            dataPlot2$dat <- currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$getData(input$CourbeNIST, bins = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], plat = c(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1],currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1])) 
          }  
        })
        
        flagTemp <- reactiveValues(temp = rep(0,length(currentProject()$calibrationsFiles)))
        valid <- reactiveValues(temp = rep(0,length(currentProject()$calibrationsFiles)))
        
        observe({
          if(is.null(input$bins)){}
          if(is.null(input$plat)){}
          else{
            if(valid$temp[which(currentProject()$calibrationsFiles == input$calibrationIn)] > 0){
              isolate({
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setBins(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1])     
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setPlat(c(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1],currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1]))
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setDataDesanomalise(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1],c(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1],currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1]))
                currentProject()$calibrations[[1]]$setRep_pas()
                updateSelectInput(session, "calibrationIn", selected = input$calibrationIn)
              })                              
              
            }
          }
          
        })
        
        observe({
          if(is.null(input$calibrationIn)){
            
            output$Nist <- renderUI({
              
              fluidRow(
                box(
                  width=12,
                  background = "green",            
                  height=100,
                  
                  column(4, 
                         h2(icon("flask"),"Step 2. Filtering data from standards")
                  ),
                  column(3,
                         br(),
                         selectInput("calibrationIn", NULL ,  as.matrix(currentProject()$calibrationsFiles),multiple = FALSE, width = '100%') 
                  ),
                  column(3,
                         br(),
                         actionButton("saveNists", "Save")
                  )
                )#box
              ) 
              
            })
            
          }
          else{
            if((flagTemp$temp[which(as.matrix(currentProject()$calibrationsFiles) == input$calibrationIn)] %%2) == 0){
              
              output$Nist <- renderUI({
                
                fluidRow(
                  box(
                    width=12,
                    background = "green",            
                    height=100,
                    
                    column(4, 
                           h2(icon("flask"),"Step 2. Filtering data from standards")
                    ),
                    column(3,
                           br(),
                           selectInput("calibrationIn", NULL ,  as.matrix(currentProject()$calibrationsFiles),multiple = FALSE, width = '100%') 
                    ),
                    column(3,
                           br(),
                           actionButton("saveNists", "Save")
                    )
                  )#box
                ) 
                
              })
              
              output$Nist2 = renderUI({
                
                minB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                maxB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]
                
                minP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                maxP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]           
                
                
                if(temoin$temp[[1]] == 1){
                  
                  value1 = (maxB - minB)/6
                  value2 = c((maxP - minP)*2/6,(maxP - minP)*4/6)
                  step = currentProject()$calibrations[[1]]$setRep_pas()
                  
                }
                if(temoin$temp[[1]] == 2){
                  
                  value1 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$bins
                  value2 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$plat
                  step = currentProject()$calibrations[[1]]$rep_pas 
                }   
                
                
                fluidRow(
                  column(8, plotOutput("distPlot", height = '600px'),
                         br(),
                         column(1),
                         column(11,
                                sliderInput("bins","Machine noise limits", value = value1, min = minB, max = maxB, step = step, width = '95%', round = T),
                                sliderInput("plat","Plateau limits", value = value2, min = minP, max = maxP,step = step, width = '95%')
                         )
                  ),
                  column(4,plotOutput("distPlot2", height = '400px'),
                         br(),
                         box(
                           background = "green",
                           #title = list(icon("home"),"Welcome to element-R"),               
                           height=150,
                           width  = 15,
                           column(6,
                                  h4(icon("cubes"),"Choose Element to consider"),
                                  selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = "Ca43", width = '100%') 
                           ), # column
                           column(6,
                                  h4(icon("area-chart"),"Choose Curve to plot"),
                                  selectInput("CourbeNIST", label = "", choices =  c("Blanc","Brute", "Plateau","- Moyenne Blanc","> LOD", "Normalisé", "Sans Anomalie"), selected  = "Plateau", width = '100%') 
                           )# column 
                         ) # box
                         
                         
                  ) #column  
                  
                ) #fluidRow
              })
              
              output$distPlot <- renderPlot({
                
                maxY <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data, na.rm = T)
                
                minX <- min(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1], na.rm = T)
                maxX <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1], na.rm = T)
                
                color <- rainbow(length(currentProject()$listeElem))
                
                plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,2],type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)", main = "Raw data", col = color[2], xlim = c(minX, maxX), ylim =c(0,maxY))
                lapply(3:length(currentProject()$listeElem), function(x){
                  par(new = T)
                  plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,x],type ="b", ylab = "", xlab = "", main = "", col = color[x], xlim = c(minX, maxX), ylim =c(0,maxY), axes = F)
                })
                legend((1-10/100)*maxX,(1+50/1000)*maxY, currentProject()$listeElem, color)
                
                # ici j'utilise une fonction des samples pour ne pas a la réécrire dans le script global
                Temp0$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$bins)[[2]]
                Temp1$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$plat[[1]])[[2]]
                Temp2$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$plat[[2]])[[2]]
                
                rect(-maxX,-maxY,currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#FF000064", border = NA)
                
                rect(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#8B735564", border = NA)
                
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "red", lwd = 2)
                
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "burlywood4", lwd = 2)
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "burlywood4", lwd = 2)
                
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,x], cex = 3, col ="red")})
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,x], cex = 3, col ="#A6760F")})
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,x], cex = 3, col ="#A6760F")})
              })
              
              output$distPlot2 <- renderPlot({
                plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)")  
              })
              
            }
            if((flagTemp$temp[which(as.matrix(currentProject()$calibrationsFiles) == input$calibrationIn)] %%2) == 1){
              
              output$Nist <- renderUI({
                
                fluidRow(
                  box(
                    width=12,
                    background = "green",            
                    height=100,
                    
                    column(4, 
                           h2(icon("flask"),"Step 2. Filtering data from standards")
                    ),
                    column(3,
                           br(),
                           selectInput("calibrationIn", NULL ,  as.matrix(currentProject()$calibrationsFiles),multiple = FALSE, width = '100%') 
                    ),
                    column(3,
                           br(),
                           actionButton("saveNists", "Delete")
                    )
                  )#box
                ) 
                
              })
              
              output$Nist2 = renderUI({
                
                minB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                maxB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]
                
                minP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                maxP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]           
                
                
                if(temoin$temp[[1]] == 1){
                  
                  value1 = (maxB - minB)/6
                  value2 = c((maxP - minP)*2/6,(maxP - minP)*4/6)
                  step = currentProject()$calibrations[[1]]$setRep_pas()
                  
                }
                if(temoin$temp[[1]] == 2){
                  
                  value1 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$bins
                  value2 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$plat
                  step = currentProject()$calibrations[[1]]$rep_pas 
                }   
                
                
                fluidRow(
                  column(8, plotOutput("distPlot", height = '600px')
                  ),
                  column(4,plotOutput("distPlot2", height = '400px'),
                         br(),
                         box(
                           background = "green",
                           #title = list(icon("home"),"Welcome to element-R"),               
                           height=150,
                           width  = 15,
                           column(6,
                                  h4(icon("cubes"),"Choose Element to consider"),
                                  selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = "Ca43", width = '100%') 
                           ), # column
                           column(6,
                                  h4(icon("area-chart"),"Choose Curve to plot"),
                                  selectInput("CourbeNIST", label = "", choices =  c("Blanc","Brute", "Plateau","- Moyenne Blanc","> LOD", "Normalisé", "Sans Anomalie"), selected  = "Plateau", width = '100%') 
                           )# column 
                         ) # box
                         
                         
                  ) #column  
                  
                ) #fluidRow
              })
              
              output$distPlot <- renderPlot({
                
                maxY <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data, na.rm = T)
                
                minX <- min(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1],na.rm = T)
                maxX <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1], na.rm = T)
                
                color <- rainbow(length(currentProject()$listeElem))
                
                plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,2],type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)", main = "Raw data", col = color[2], xlim = c(minX, maxX), ylim =c(0,maxY))
                lapply(3:length(currentProject()$listeElem), function(x){
                  par(new = T)
                  plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,x],type ="b", ylab = "", xlab = "", main = "", col = color[x], xlim = c(minX, maxX), ylim =c(0,maxY), axes = F)
                })
                legend((1-10/100)*maxX,(1+50/1000)*maxY, currentProject()$listeElem, color)
                
                # ici j'utilise une fonction des samples pour ne pas a la réécrire dans le script global
                Temp0$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$bins)[[2]]
                Temp1$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$plat[[1]])[[2]]
                Temp2$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1],input$plat[[2]])[[2]]
                
                rect(-maxX,-maxY,currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1],(1+10/100)*maxY, col = "#FF000064", border = NA)
                
                rect(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1],-maxY,currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1],(1+10/100)*maxY, col ="#8B735564", border = NA)
                
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], lty = "dashed", col = "red", lwd = 2)
                
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1], lty = "dashed", col = "burlywood4", lwd = 2)
                abline(v = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1], lty = "dashed", col = "burlywood4", lwd = 2)
                
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp0$t,x], cex = 3, col ="red")})
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp1$t,x], cex = 3, col ="#A6760F")})
                lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[Temp2$t,x], cex = 3, col ="#A6760F")})
              })
              
              output$distPlot2 <- renderPlot({
                plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)")  
              })
              
            }
          }
          
          
        })              
        
        observe({
          
          if(is.null(input$saveNists)){}
          else{
            if(input$saveNists > 0){
              isolate(flagTemp$temp[which(currentProject()$calibrationsFiles == input$calibrationIn)] <- flagTemp$temp[which(currentProject()$calibrationsFiles == input$calibrationIn)] + 1)
            }
          }
          updateSelectInput(session, "calibrationIn", selected = input$calibrationIn)
          
        })
        
        observe({
          sapply(1:length(currentProject()$calibrationsFiles), function(x){
            if((flagTemp$temp[x] %% 2) == 1){
              valid$temp[x] <- 1
              currentProject()$setflagCalib(x,1)
            }
            if((flagTemp$temp[x] %% 2) == 0){
              valid$temp[x] <- 0
              currentProject()$setflagCalib(x,0)
            }
          })
        })
        
      }
    }
    
  })
  
  #######################
  ## VERIF STANDARDS ####
  #######################
  
  Stand <- reactiveValues(dat = NULL)
  validCorrection <- reactiveValues(temp = 0)
  
  correction <- reactiveValues(val = NULL)
  tableauStat <- reactiveValues(temp = NULL)
  
  observe({
    input$saveNists
    input$DeleteNists
    if(is.null(currentProject())){}
    if(is.null(input$calibrationIn)){}    
    else{
      observe({
        if(length(which(currentProject()$flag_Calib != 1)) == 0){  
          
          lapply(1:length(currentProject()$flag_Calib), function(x){currentProject()$calibrations[[1]]$rep_data[[x]]$setdata_calibFinal()})
          
          currentProject()$calibrations[[1]]$setrep_dataFinale() 
          
          tab = reactiveValues(dat = currentProject()$calibrations[[1]]$setRep_table(currentProject()$listeElem))
          
          currentProject()$setSummaryNist(tab$dat)
          
          tableauStat$temp <- currentProject()$correction()  
          
          output$Text1 <- renderUI({
            
            fluidRow(
              box(width = 12,background = "olive", height = 100, 
                  column(4, 
                         h2(icon("plug"),"Step 3. Machine drift verification")
                  )
                  
              ) # box
            )
          })#output$Text1
          
          NomNist <- reactiveValues(temp = geneR(choix = letterChoice, longueurDesirer = 5, combinaisonDesirer = length(currentProject()$listeElem)))
          correction$val <- rep(F, length(currentProject()$listeElem))
          preview <- reactiveValues(temp = geneR(choix = letterChoice, longueurDesirer = 6, combinaisonDesirer = length(currentProject()$listeElem)))
          previewAction <- reactiveValues(temp = rep(0, length(currentProject()$listeElem)))
          
          observe({
            if(length(currentProject()$flag_Calib) == 1){
              
              output$Text2 <- renderUI({
                box(width = 12,background = NULL, height = 100, 
                    fluidRow(
                      column(1,
                             h4("Element")),
                      column(6,
                             h4("Regression parameters")),
                      column(1,
                             actionButton("validDrift", "Valider")
                      )
                      
                    )
                )
                
              })
              
            }
            if(length(currentProject()$flag_Calib) == 2){
              
              if((validCorrection$temp%%2) == 0){
                output$Text2 <- renderUI({
                  box(width = 12,background = NULL, height = 100, 
                      fluidRow(
                        column(1,
                               h4("Element")),
                        column(6,
                               h4("Regression parameters")),
                        column(1,
                               checkboxInput("CorrectAll",label = "Correct all", value = F)),
                        column(1,
                               actionButton("validDrift", "Valider")
                        )
                        
                      )
                  )
                  
                })
              }
              if((validCorrection$temp%%2) == 1){
                output$Text2 <- renderUI({
                  box(width = 12,background = NULL, height = 100, 
                      fluidRow(
                        column(1,
                               h4("Element")),
                        column(6,
                               h4("Regression parameters")),
                        column(1,
                               actionButton("validDrift", "Delete")
                        )
                        
                      )
                  )
                  
                })
              }
              
            }
            if(length(currentProject()$flag_Calib) > 2){
              if((validCorrection$temp%%2) == 0){
                output$Text2 <- renderUI({
                  box(width = 12,background = NULL, height = 100, 
                      fluidRow(
                        column(1,
                               h4("Element")),
                        column(1,
                               h4("Norm.")),
                        column(1,
                               h4("Homosc.")),
                        column(1,
                               h4("Independance")),
                        column(1,
                               h4("Regression")),
                        column(1,
                               checkboxInput("CorrectAll",label = "Correct all", value = F)
                        ),
                        column(1,
                               actionButton("validDrift", "Valider")
                        )
                        
                      )
                  )
                  
                })
              }
              if((validCorrection$temp%%2) == 1){
                output$Text2 <- renderUI({
                  box(width = 12,background = NULL, height = 100, 
                      fluidRow(
                        column(1,
                               h4("Element")),
                        column(1,
                               h4("Norm.")),
                        column(1,
                               h4("Homosc.")),
                        column(1,
                               h4("Independance")),
                        column(1,
                               h4("Regression")),
                        column(1
                        ),
                        column(1,
                               actionButton("validDrift", "Delete")
                        )
                        
                      )
                  )
                  
                })
              }
              
            }
          })          
          
          observe({
            if(is.null(input$CorrectAll)){
              
              if(length(currentProject()$flag_Calib) == 1){
                
                
                output$Text2Bis <- renderUI({
                  
                  lapply(1:length(currentProject()$listeElem), function(x){
                    
                    plotname2 <- paste("plotSession", x, sep="")
                    
                    output[[plotname2]] <- renderPlot({
                      
                      min <- tab$dat[1,x]- 2*tab$dat[2,x]
                      
                      max <- tab$dat[1,x] + 2*tab$dat[2,x]
                      
                        PlotIC(currentProject()$calibrationsFiles,tab$dat[1,x],tab$dat[2,x],lengthSeg = 0.1, xlim = c(0,2), ylim=c(min, max), ylab = "cps", xlab = currentProject()$calibrationsFiles)
                      
                    })
                    
                    if((previewAction$temp[x]%%2) ==1 ){
                      
                      box(width = 12,background = NULL, height = 500,
                          column(8,
                                 fluidRow(
                                   column(1,
                                          h4(currentProject()$listeElem[x], style = couleur[1])),
                                   column(4,
                                          h4("No correction possible: Only one calibration files", style = couleur[1])),
                                   column(3,
                                          actionButton(preview$temp[x],"Hide plot"))
                                 ) 
                          ),
                          column(4,align = "center",
                                 plotOutput(plotname2)
                          )
                          
                      )
                      
                    }
                    else{
                      
                      box(width = 12,background = NULL, height = 100,
                          column(8,
                                 fluidRow(
                                   column(1,
                                          h4(currentProject()$listeElem[x], style = couleur[1])),
                                   column(4,
                                          h4("No correction possible: Only one calibration files", style = couleur[1])),
                                   column(3,
                                          actionButton(preview$temp[x],"Plot preview"))
                                 ) 
                          )                          
                      )
                      
                    }
                    
                    
                    
                  })
                  
                })                  
                
                
              }
              else{}
              
            }
            else{                 
              if(length(currentProject()$flag_Calib) == 2){
                
                if((validCorrection$temp%%2) == 0){
                  output$Text2Bis <- renderUI({
                    
                    lapply(1:length(currentProject()$listeElem), function(x){
                      
                      plotname2 <- paste("plotSession", x, sep="")
                      
                      output[[plotname2]] <- renderPlot({
                        
                        min <- (max(tab$dat[1:length(currentProject()$flag_Calib),x], na.rm = T) - max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],na.rm = T))*0.5
                        
                        max <- (max(tab$dat[1:length(currentProject()$flag_Calib),x],na.rm = T) + max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],na.rm = T))*1.5
                        
                        PlotIC(currentProject()$calibrationsFiles,tab$dat[1:length(currentProject()$flag_Calib),x],tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],lengthSeg = 0.1, xlim =c(1,length(currentProject()$flag_Calib)),ylim=c(min, max), ylab = "cps", xlab = currentProject()$calibrationsFiles)
                        
                        abline(a = currentProject()$regressionModel[[x]][1], b= currentProject()$regressionModel[[x]][2], col ="red", lty = 2)
                      })   
                      
                      if(input$CorrectAll == T){
                        if((previewAction$temp[x]%%2) ==1){
                          taille <- 500
                          correction$val[x] <- T
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("Regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Hide settings preview")),
                                       column(2, 
                                              checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                     ) 
                              ),
                              column(4,align = "center",
                                     plotOutput(plotname2)
                                     
                              )
                              
                          )
                        }
                        else{
                          taille <- 100
                          correction$val[x] <- T
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Regression settings preview")),
                                       column(2, 
                                              checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                     ) 
                              )                              
                          )
                        }
                      }
                      else{
                        if((previewAction$temp[x]%%2) ==1 ){
                          
                          taille <- 500
                          correction$val[x] <- F
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Hide settings preview")),
                                       column(2, 
                                              checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                     ) 
                              ),
                              column(4,align = "center",
                                     plotOutput(plotname2)
                              )
                              
                          )
                          
                        }
                        else{
                          taille <- 100
                          correction$val[x] <- F
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),                                       
                                       column(3,
                                              actionButton(preview$temp[x],"Regression settings preview")),
                                       column(2, 
                                              checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                     ) 
                              )                              
                          )                         
                        }
                      }                    
                      
                    }) # eo lapply       
                  }) # eo output$Text2Bis 
                }
                if((validCorrection$temp%%2) == 1){
                  output$Text2Bis <- renderUI({
                    
                    lapply(1:length(currentProject()$listeElem), function(x){
                      
                      plotname2 <- paste("plotSession", x, sep="")
                      
                      output[[plotname2]] <- renderPlot({
                        
                        min <- (max(tab$dat[1:length(currentProject()$flag_Calib),x],na.rm = T) - max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],na.rm = T))*0.5
                        
                        max <- (max(tab$dat[1:length(currentProject()$flag_Calib),x],na.rm = T) + max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],na.rm = T))*1.5
                        
                        PlotIC(currentProject()$calibrationsFiles,tab$dat[1:length(currentProject()$flag_Calib),x],tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],lengthSeg = 0.1, xlim =c(1,length(currentProject()$flag_Calib)),ylim=c(min, max),ylab = "cps", xlab = currentProject()$calibrationsFiles)
                        
                        abline(a = currentProject()$regressionModel[[x]][1], b= currentProject()$regressionModel[[x]][2], col ="red", lty = 2)
                      })   
                      
                      if(input$CorrectAll == T){
                        if((previewAction$temp[x]%%2) ==1){
                          taille <- 500
                          correction$val[x] <- T
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Hide settings preview"))
                                     ) 
                              ),
                              column(4,align = "center",
                                     plotOutput(plotname2)
                              )
                              
                          )
                        }
                        else{
                          taille <- 100
                          correction$val[x] <- T
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Regression settings preview"))
                                     ) 
                              )                              
                          )
                        }
                      }
                      else{
                        if((previewAction$temp[x]%%2) ==1 ){
                          
                          taille <- 500
                          correction$val[x] <- F
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Hide settings preview"))
                                     ) 
                              ),
                              column(4,align = "center",
                                     plotOutput(plotname2)
                              )
                              
                          )
                          
                        }
                        else{
                          taille <- 100
                          correction$val[x] <- F
                          couleur <- c("color:black","color:red")
                          
                          box(width = 12,background = NULL, height = taille,
                              column(8,
                                     fluidRow(
                                       column(1,
                                              h4(currentProject()$listeElem[x], style = couleur[1])),
                                       column(4,
                                              h4("No parameters: Only two calibration files", style = couleur[1])),
                                       column(2,
                                              h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]][1],3), " + X * ", round(currentProject()$regressionModel[[x]][2],3)))),
                                       column(3,
                                              actionButton(preview$temp[x],"Regression settings preview"))
                                     ) 
                              )                              
                          )                         
                        }
                      }                    
                      
                    }) # eo lapply       
                  }) # eo output$Text2Bis 
                }
                
              }
              if(length(currentProject()$flag_Calib) > 2){
                if((validCorrection$temp%%2) == 0){
                  output$Text2Bis <- renderUI({
                    
                    lapply(1:length(currentProject()$listeElem), function(x){
                      
                      plotname <- paste("plot", x, sep="")                  
                      
                      output[[plotname]] <- renderPlot({
                        par(mfrow = c(1,4))
                        plot(currentProject()$regressionModel[[x]])
                      })
                      
                      plotname2 <- paste("plotSession", x, sep="")
                      
                      output[[plotname2]] <- renderPlot({
                        
                        min <- (max(tab$dat[1:length(currentProject()$flag_Calib),x], na.rm = T) - max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x], na.rm = T))*0.5
                        
                        max <- (max(tab$dat[1:length(currentProject()$flag_Calib),x], na.rm = T) + max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x], na.rm = T))*1.5
                        
                        PlotIC(currentProject()$calibrationsFiles,tab$dat[1:length(currentProject()$flag_Calib),x],tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],lengthSeg = 0.1, xlim =c(1,length(currentProject()$flag_Calib)),ylim=c(min, max), ylab = "cps", xlab = "Calibration files")
                        
                        abline(a = currentProject()$regressionModel[[x]]$coefficients[1], b= currentProject()$regressionModel[[x]]$coefficients[2], col ="red", lty = 2)
                      })   
                      
                      if(is.na(tableauStat$temp[x,1]) | is.na(tableauStat$temp[x,2]) | is.na(tableauStat$temp[x,3]) | is.na(tableauStat$temp[x,4])){
                        taille <- 100
                        correction$val[x] <- F
                        couleur <- c("color:black","color:black")
                        
                        box(width = 12,background = NULL, height = taille,
                            column(8,
                                   fluidRow(
                                     column(1,
                                            h4(currentProject()$listeElem[x], style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                     column(3,
                                            actionButton(preview$temp[x],"Regression settings preview"))
                                   )
                            )
                            
                        )
                        
                      }
                      else{         
                        if(tableauStat$temp[x,1] < 0.05 | tableauStat$temp[x,2] <0.05 | tableauStat$temp[x,3] < 0.05){
                          if(tableauStat$temp[x,4] < 0.05){
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                correction$val[x] <- T
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- T
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ) 
                                    )
                                    
                                )
                                
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                correction$val[x] <- F
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- F
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                          else{
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                correction$val[x] <- T
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- T
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                correction$val[x] <- F
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- F
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                        }
                        else{
                          if(tableauStat$temp[x,4] < 0.05){
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                correction$val[x] <- T
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                                
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- T
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ) 
                                    )
                                    
                                )
                                
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                correction$val[x] <- F
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                                
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- F
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview")),
                                             column(1, 
                                                    checkboxInput(NomNist$temp[x],label = "Correct", value = correction$val[x]))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                          else{
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                correction$val[x] <- T
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4, align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- T
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           )
                                    )
                                    
                                )
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1){  
                                taille <- 500
                                correction$val[x] <- F
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4, align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                correction$val[x] <- F
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           )
                                    )
                                    
                                )
                              }
                            }
                          }
                        }
                        
                      }
                      
                      
                      
                      
                    }) # eo lapply       
                  }) # eo output$Text2Bis 
                }
                if((validCorrection$temp%%2) == 1){
                  output$Text2Bis <- renderUI({
                    
                    lapply(1:length(currentProject()$listeElem), function(x){
                      
                      plotname <- paste("plot", x, sep="")                  
                      
                      output[[plotname]] <- renderPlot({
                        par(mfrow = c(1,4))
                        plot(currentProject()$regressionModel[[x]])
                      })
                      
                      plotname2 <- paste("plotSession", x, sep="")
                      
                      output[[plotname2]] <- renderPlot({
                        
                        min <- (max(tab$dat[1:length(currentProject()$flag_Calib),x], na.rm = T) - max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x], na.rm = T))*0.5
                        
                        max <- (max(tab$dat[1:length(currentProject()$flag_Calib),x], na.rm = T) + max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x], na.rm = T))*1.5
                        
                        PlotIC(currentProject()$calibrationsFiles,tab$dat[1:length(currentProject()$flag_Calib),x],tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),x],lengthSeg = 0.1, xlim =c(1,length(currentProject()$flag_Calib)),ylim=c(min, max), ylab = "cps", xlab = "Calibration files")
                        
                        abline(a = currentProject()$regressionModel[[x]]$coefficients[1], b= currentProject()$regressionModel[[x]]$coefficients[2], col ="red", lty = 2)
                      })  
                      
                      if(is.na(tableauStat$temp[x,1]) | is.na(tableauStat$temp[x,2]) | is.na(tableauStat$temp[x,3]) | is.na(tableauStat$temp[x,4])){
                        taille <- 100
                        couleur <- c("color:black","color:black")
                        
                        box(width = 12,background = NULL, height = taille,
                            column(8,
                                   fluidRow(
                                     column(1,
                                            h4(currentProject()$listeElem[x], style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                     column(1,
                                            h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                     column(3,
                                            actionButton(preview$temp[x],"Regression settings preview"))
                                   )
                            )
                            
                        )
                        
                      }
                      else{         
                        if(tableauStat$temp[x,1] < 0.05 | tableauStat$temp[x,2] <0.05 | tableauStat$temp[x,3] < 0.05){
                          if(tableauStat$temp[x,4] < 0.05){
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                                
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:red","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                          else{
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:red","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                        }
                        else{
                          if(tableauStat$temp[x,4] < 0.05){
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1 ){
                                taille <- 500
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                                
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                                
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4,align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                                
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:black","color:red")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[2])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           ) 
                                    )
                                    
                                )
                              }
                            }
                          }
                          else{
                            if(input$CorrectAll == T){
                              if((previewAction$temp[x]%%2) ==1){
                                taille <- 500
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4, align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           )
                                    )
                                    
                                )
                              }
                            }
                            else{
                              if((previewAction$temp[x]%%2) ==1){  
                                taille <- 500
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Hide settings preview"))
                                           ),
                                           plotOutput(plotname)  
                                    ),
                                    column(4, align = "center",
                                           plotOutput(plotname2),
                                           h4(paste0("regression: Y = ", round(currentProject()$regressionModel[[x]]$coefficients[1],3), " + X * ", round(currentProject()$regressionModel[[x]]$coefficients[2],3)))
                                    )
                                    
                                )
                              }
                              else{
                                taille <- 100
                                couleur <- c("color:black","color:black")
                                
                                box(width = 12,background = NULL, height = taille,
                                    column(8,
                                           fluidRow(
                                             column(1,
                                                    h4(currentProject()$listeElem[x], style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,1],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,2],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,3],2), style = couleur[1])),
                                             column(1,
                                                    h4(round(tableauStat$temp[x,4],2)), style = couleur[2]),
                                             column(3,
                                                    actionButton(preview$temp[x],"Regression settings preview"))
                                           )
                                    )
                                    
                                )
                              }
                            }
                          }
                        }
                        
                      }
                      
                    })
                    
                  })
                }
              }
            } 
          })       
          
          observe({
            
            lapply(1:length(currentProject()$listeElem), function(x){
              
              if(is.null(eval(parse(text = paste0("input$",preview$temp[x]))))){}
              else{
                if(eval(parse(text = paste0("input$",preview$temp[x])))>0){
                  isolate({                
                    previewAction$temp[x] <- previewAction$temp[x]+1
                  })
                }
              }
              
            })
            
          })
          
          observe({
            if(is.null(input$validDrift)){}
            else{
              isolate({
                if(input$validDrift > 0){
                  
                  for (i in 1: length(currentProject()$listeElem)){
                    
                    if(is.null(eval(parse(text = paste0("input$",NomNist$temp[i]))))){correction$val[i] <- FALSE}
                    else(
                      
                      isolate(correction$val[i] <- eval(parse(text = paste0("input$",NomNist$temp[i]))))
                      
                      
                    )
                  }                  
                }
              })
              
            }
          })
          
          observe({
            if(is.null(input$validDrift)){}
            else{
              isolate({
                if((input$validDrift%%2) == 0){
                  validCorrection$temp <- validCorrection$temp
                }
                if((input$validDrift%%2) == 1){
                  validCorrection$temp <- validCorrection$temp + 1
                }
              })
              
            }
          })     
          
          
          
        }
        else{
          
          output$Text1 <- renderUI({NULL})#output$Text1
          
          output$Text2 <- renderUI({NULL})#output$Text2
          
          output$Text2Bis <- renderUI({NULL}) # output$Text2Bis
          
        }
      })
      
      
    }
    
  })  
  
  machineCorrection <- reactiveValues(temp = list())
  observe({
      if((validCorrection$temp%%2) == 1){
        lapply(1:length(currentProject()$samplesFiles), function(x){lapply(1:length(currentProject()$samples[[x]]$rep_data), function(t){currentProject()$samples[[x]]$rep_data[[t]]$setstandard(currentProject()$EtalonData)})})
        
        lapply(1:length(currentProject()$listeElem), function(x){
          if(is.na(tableauStat$temp[x,4])){
            machineCorrection$temp[[x]] <- c(currentProject()$SummaryNist[(nrow(currentProject()$SummaryNist)-1),x],0)
          }
          else{
            if(tableauStat$temp[x,4] < 0.05 & correction$val[x] == T){
              machineCorrection$temp[[x]] <-  currentProject()$regressionModel[[x]]$coefficients[1:2]
            }
            else{
              machineCorrection$temp[[x]] <- c(currentProject()$SummaryNist[(nrow(currentProject()$SummaryNist)-1),x],0)
            }
            
          }
          
          
        })
        currentProject()$setCorrection(machineCorrection$temp)
      }    
  })
  
  #######################
  ##### SAMPLES #########
  #######################
  
  temoinSample <- reactiveValues(temp = NULL)
  
  observe({
      if((validCorrection$temp%%2) == 0){
        
        output$sample1 <- renderUI({NULL})
        
        output$sample2 <- renderUI({NULL})  
        
        output$sample3 <- renderUI({NULL})
        
        output$sample4 <- renderUI({NULL})   
        
        output$Sample5 = renderUI({NULL})      
        
      }
      if((validCorrection$temp%%2) == 1){
        
        output$sample1 <- renderUI({
          h2("Step 5. Filtering data from samples")       
        })
        
        output$sample2 <- renderUI({
          selectInput("SampleIn", NULL,  as.matrix(currentProject()$samplesFiles), selected = as.matrix(currentProject()$samplesFiles)[1], multiple = FALSE, width = '100%')       
        })      
        
        output$sample3 <- renderUI({
          selectInput("SampleIn2", NULL, as.matrix(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files), multiple = F, width = '100%' )
        })        
        
        output$sample4 <- renderUI({
          actionButton("ValiderSample", "Save")       
        })
        
        values <- reactiveValues(tac = 0)
        
        output$sample6 <- renderUI({
          totalActionCount <- as.numeric(input$ValiderSample)
          if (input$ValiderSample > 0) {
            if(isolate(values$tac < totalActionCount)){
              isolate(values$tac <- totalActionCount)
              invalidateLater(3000, session)
              return(h2("Validated !!"))
            }else{
            }
          }
        })
        
        Temp0S <- reactiveValues(t = NULL)
        Temp1S <- reactiveValues(t = NULL)
        Temp2S <- reactiveValues(t = NULL)
        
        dataPlot2Sample <- reactiveValues(datS = NULL) 
        
        output$Sample5 = renderUI({
          
          minBS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
          maxBS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
          
          minPS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
          maxPS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
          
          
          if(temoin$temp[[1]] == 1){
            
            value1S = (maxBS - minBS)/6
            value2S = c((maxPS - minPS)*2/6,(maxPS - minPS)*4/6)
            step = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$setRep_pas()
          }
          if(temoin$temp[[1]] == 2){
            
            value1S = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$binsSample
            value2S = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$platSample
            step = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_pas 
          }   
          
          
          fluidRow(
            column(8, plotOutput("distPlotSample", height = '600px'),
                   br(),
                   column(1),
                   column(11,
                          sliderInput("binsSample","Machine noise limits", value = value1S, min = minBS, max = maxBS, step = step, width = '95%'),
                          sliderInput("platSample","Plateau limits", value = value2S, min = minPS, max = maxPS, step = step, width = '95%')
                   )
            ),
            column(4,plotOutput("distPlot2Sample", height = '400px'),
                   br(),
                   box(
                     background = "navy",            
                     height=150,
                     width  = 15,
                     column(6,
                            h4(icon("cubes"),"Choose Element to consider"),
                            selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = "Ca43", width = '100%') 
                     ), # column
                     column(6,
                            h4(icon("area-chart"),"Choose Curve to plot"),
                            selectInput("CourbeSample", label = "", choices =  c("Blanc","Brute", "Plateau","- Moyenne Blanc","> LOD", "Normalisé","Concentration", "Conc. corrected"), selected  = "Plateau", width = '100%') 
                     )# column 
                   ) # box
                   
                   
            ) #column  
            
          ) #fluidRow
        })
        
        output$distPlotSample <- renderPlot({
          
          maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data, na.rm = T) 

          minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = T)
          maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], na.rm = T)
          
          color <- rainbow(length(currentProject()$listeElem))
          
          plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,2],type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)", main = "Raw data", col = color[2], xlim = c(minX, maxX), ylim =c(0,maxY))
          lapply(3:length(currentProject()$listeElem), function(x){
            par(new = T)
            plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,x],type ="b", ylab = "", xlab = "", main = "", col = color[x], xlim = c(minX, maxX), ylim =c(0,maxY), axes = F)
          })
          legend((1-10/100)*maxX,(1+50/1000)*maxY, currentProject()$listeElem, color)
          
          Temp0S$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],input$binsSample)[[2]]
          Temp1S$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],input$platSample[[1]])[[2]]
          Temp2S$t <- currentProject()$samples[[1]]$lePlusProche(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1],input$platSample[[2]])[[2]]
          
          rect(-maxX,-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1],(1+10/100)*maxY, col = "#FF000064", border = NA)
          
          rect(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],-maxY,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1],(1+10/100)*maxY, col ="#8B735564", border = NA)
          
          abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], lty = "dashed", col = ("red"), lwd = 2)
          
          abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], lty = "dashed", col = ("burlywood4"), lwd = 2)
          abline(v = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], lty = "dashed", col = ("burlywood4"), lwd = 2)
          
          lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,x], cex = 3, col ="red")})
          lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,x], cex = 3, col ="#A6760F")})
          lapply(1:length(currentProject()$listeElem), function(x){points(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,x], cex = 3, col ="#A6760F")})
          
        })
        
        output$distPlot2Sample <- renderPlot({
          if(length(which(!is.na(dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))]))) == 0){
            plot(-1,-1, xlim = c(0,2), ylim = c(0,1),xlab = "", ylab = "")
            text(1,0.5, labels = "No data different from NA", cex = 2)
          }
          else{
            plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "Signal intensity (cps)", xlab = "Time (s)")  
          }
          
        })
        
        observe({
          if(is.null(currentProject())){}
          if(is.null(input$SampleIn)){}
          if(is.null(input$SampleIn2)){}
          if(is.null(input$CourbeSample)){}
          else{
            if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){}
            else{
              currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setVector(bins = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], plat = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]))
              dataPlot2Sample$datS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(input$CourbeSample,  bins = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], plat = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]), nom = input$SampleIn2, SimNist = currentProject()$SummaryNist[(nrow(currentProject()$SummaryNist)-1),], summarySession = currentProject()$sessionSummary, model = currentProject()$machineCorrection)        
              
            }
          } 
        }) # observe
        
        observe({
          if(is.null(input$ValiderSample)){}
          else{      
            input$ValiderSample
            isolate({
              if(input$ValiderSample >0){
                currentProject()$setflagSample(grep(input$SampleIn,currentProject()$samplesFiles), grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files),TRUE)
                currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setBins(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1])
                currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setPlat(c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]))
                currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setDataConcCorr(bins = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp0S$t,1], plat = c(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp1S$t,1],currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[Temp2S$t,1]), nom = input$SampleIn2,  SimNist = currentProject()$SummaryNist[(nrow(currentProject()$SummaryNist)-1),], summarySession = currentProject()$sessionSummary, model = currentProject()$machineCorrection)
              }  
            })
          } 
        }) # observe
        
        observe({
          input$ValiderSample
          input$DeleteSample 
          
          temoinSample$temp <- sapply(1:length(currentProject()$flag_Sample), function(x){if(length(which(currentProject()$flag_Sample[[x]] == F)) != 0){F}
                                                                                          else{T}
          })
        })
        
      }
    
  })
  
  #######################
  ##### REALIGNEMENT ####
  #######################

  deplace <- reactiveValues(val = NULL)
  vectResults <- reactiveValues(temp = c("a","b","c","d","e","f","g","h","i","j"))
  correl <- reactiveValues(temp = NULL)
  
  observe({
    input$ValiderSample
    if(length(which(temoinSample$temp == T)) != 0){
      
      output$textRealign <- renderUI({
        h2("Step 6. Curves realignment & averaging")
      })
      
      output$textRealignBis <- renderUI({
        selectInput("selectRealign",label = "", choices = isolate(currentProject()$samplesFiles[which(temoinSample$temp == T)]), multiple = F )
      })
      
      output$textRealignBis2 <- renderUI({
        radioButtons("typeTraitement", label = "", choices = isolate(c("spot","raster")), inline = T)
      })
      
      observe({
        if(is.null(currentProject())){}
        if(is.null(input$selectRealign)){}
        else{
          currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFiltre()
          
        }
      })
      
      ######## SPOT ##########
      observe({
        if(is.null(input$MoyenneSpot)){}
        else{
          isolate({
            if(input$MoyenneSpot > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "spot", position = 1, valeur = 1)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            }
          })                 
        }
      }) # observe
        
      observe({
        if(is.null(input$SauvegarderSpot)){}
        else{
          isolate({
            if(input$SauvegarderSpot > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "spot", position = 2, valeur = 1)
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2("spot")
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })                
        }
      }) # observe
      
      observe({
        if(is.null(input$SupprSpot)){}
        else{
          isolate({
            input$SupprSpot
            if(input$SupprSpot > 0){                    
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "spot", position = c(1,2,3,4,5), valeur = 0)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })                
        }
      }) # observe
      
      ###### RASTER ##########
      observe({
        if(is.null(input$MoyenneRaster)){}
        else{
          isolate({
            if(input$MoyenneRaster > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 1, valeur = 1)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            }
          })                 
        }
      }) # observe
      
      observe({
        if(is.null(input$DemoyennerRaster)){}
        else{
          isolate({
            if(input$DemoyennerRaster > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(1,2), valeur = 0)
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$initial("rep_dataNonCorrel")
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })
        }
      }) # observe
      
      observe({
        if(is.null(input$SauvegarderReal)){}
        else{
          isolate({
            if(input$SauvegarderReal > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 2, valeur = 1)
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_type2("raster")
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })                
        }
      }) # observe
      
      observe({
        if(is.null(input$Suppr)){}
        else{
          isolate({
            input$Suppr
            if(input$Suppr > 0){                    
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(1,2), valeur = 0)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })                
        }
      }) # observe
      

          
      
      
    }
    if(length(which(temoinSample$temp == T)) == 0){
      output$textRealign <- renderUI({NULL})
      output$textRealignBis <- renderUI({NULL})
      output$textRealignBis2 <- renderUI({NULL})
      output$textRealign2 <- renderUI({NULL})
      output$textRealign7 <- renderUI({NULL})
      output$textRealign3 <- renderUI({NULL})
      
    }
    
  })

  observe({
  input$ValiderSample
  
    if(is.null(input$typeTraitement)){}
    if(is.null(input$selectRealign)){}
    else{ 
      
      if(length(which(temoinSample$temp == T)) != 0){
      input$MoyenneSpot
      input$DemoyennerSpot
      input$SupprSpot
      input$MoyenneRaster
      input$DemoyennerRaster
      input$SauvegarderReal
      input$Suppr
      input$SauvegarderSpot
      
      if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[1] == 0 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[1] != 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[2] != 1){   
        
        currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataInterm(type = "spot")
        
        output$textRealign2 <- renderUI({
          box(background = "black", width = 3, height = 200,
              column(12,
                     fluidRow(
                       h3("Spot averaging :"),
                       actionButton("MoyenneSpot", "Mean"))
              )                    
          ) # box                                                                             
        })
        
        output$textRealign3 <- renderTable({  
          
          tableau <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpot
          
          return(tableau)
          
        }, digits = 5)
        
        output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
        
      } # if
      
      if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[1] == 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] != 1){  
        
        currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_dataFinale(type = "spot") 
        
        output$textRealign2 <- renderUI({
          box(background = "black", width = 3, height = 200,
              column(12,
                     fluidRow(
                       h3("Spot averaging :"),
                       p(actionButton("MoyenneSpot", "Mean"),actionButton("SauvegarderSpot", "Save averaging"))
                       
                     )                               
              )                   
          ) # box                                                                             
        })
        
        output$textRealign3 <- renderTable({  
          
          tableau <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis
          
          return(tableau)
          
        }, digits = 5)
        
        output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
      } # if
      
      if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] == 1){
        
        output$textRealign2 <- renderUI({
          box(background = "black", width = 3, height = 100,
              column(12,
                     actionButton("SupprSpot", "Delete mean")
              )                               
              
          ) # box                                                                             
        })
        
        output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
        
      } # if
      
      if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[2] == 1){
        
        output$textRealign2 <- renderUI({
          box(background = "black", width = 3, height = 100,
              column(12,
                     h3("Already validated with the raster protocole")
              )                               
              
          ) # box                                                                             
        })            
      } # if
      
      if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[1] == 0 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[1] != 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] != 1){
        
        
        
        
        output$textRealign3 <- renderTable({NULL})  
        
        output$textRealign2 <- renderUI({
          
          box(background = "black", width = 3, height = 250 + 200 * (length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)-1),
              column(12,                         
                     h3("Rasters realignment :"),
                     br(),
                     selectInput("elemRaster","Element to realign",choices = currentProject()$listeElem, selected = "Ba138"),
                     plot_output_list <- lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i) {
                       plotname <- paste("plot", i, sep="")
                       numericInput(vectResults$temp[i],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files[i],value = 0)
                       
                     }),
                     br(),
                     actionButton("MoyenneRaster", "Mean")
              ) # column                   
          ) # box
        }) # textRealign2
        
        for (i in 1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            
            output[[plotname]] <- renderUI({
              numericInput(vectResults$temp[i],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files[i],value = 0)
            })
          })
        }
        
        observe({
          ## François ??
          for (i in 1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)){
            deplace$val[i] <- eval(parse(text = paste("input$",vectResults$temp[i],sep="")))
          }
        }) # observe
        
        observe({
          
          if(is.null(deplace$val)){}
          else{
            
            deplace$val 
            
            if(temoin$temp[[1]] == 1){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataInterm(type = "raster", decalage = deplace$val, temoin = 1, data = NULL)
            }
            if(temoin$temp[[1]] == 2){                  
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataInterm(type = "raster", decalage = deplace$val, temoin = 2, data = currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFiltre)
            }
            
            output$textRealign5 <- renderPlot({
              
              deplace$val
              
              ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
              
              xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))))
              
              lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(x){
                
                #                     print(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]])
                
                plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", main = "", col = rainbow(length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files))[x])
                
                par(new = T)
                
              })
              
              
              legend("topright", legend = currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files, col = rainbow(length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files)), lty = c(1,1))
              
              
            })
            
            
          }
        }) 
        
      } # if
      
      if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[1] == 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[2] != 1){
        
        currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_dataFinale(type = "raster")                    
        
        currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_deplacement(deplace$val)
        
        if(temoin$temp[[1]] == 1){value3 <- 15; value4 <- 60}
        if(temoin$temp[[1]] == 2){value3 <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_vitesse; value4 <- currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_diam}         
        
        output$textRealign3 <- renderTable({NULL})  
        
        output$textRealign2 <- renderUI({
          
          box(background = "black", width = 3, height = 175,
              column(12,  
                     fluidRow(
                       column(6, h3("Rasters realignment :")), 
                       column(6, h3(icon("check"), ""))
                     ),
                     br(),
                     fluidRow(
                       column(3, actionButton("DemoyennerRaster","Delete averaging")), 
                       column(6, actionButton("SauvegarderReal","Save averaging"))
                     )
                     
              ) # column                   
          ) # box
        }) 
        
        output$textRealign5 <- renderPlot({
          
          ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
          
          xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]})), na.rm = T))
          
          lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(x){
            
            plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "Time (s)", ylab = "Concentrations", type = "b", col = rainbow(length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files))[x])
            
            par(new = T)
            
          })
          
          plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinaleCorrel[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataFinaleCorrel[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b")
          
        })  
        
        
        
      } # if
      
      if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[2] == 1){
        
        output$textRealign2 <- renderUI({
          
          box(background = "black", width = 3, height = 100,
              column(12,
                     actionButton("Suppr","Delete Realignment")
              )
          )
          
        }) # textRealign2
      } # if
      
      if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] == 1){
        
        output$textRealign2 <- renderUI({
          box(background = "black", width = 3, height = 100,
              column(12,
                     h3("Already validated with the spot protocole")
              )                               
              
          ) # box                                                                             
        })
        
        output$textRealign3 <- renderTable({  
          
          tableau <- rbind(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis, currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis)
          
          return(tableau)
          
        }, digits = 5)
        
        output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
      } # if
      
      }
      
    }
    
  })

  
}#eo server


######################
######## CALL shinyApp
######################
shinyApp(ui, server)