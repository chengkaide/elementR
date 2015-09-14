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
                br(),
                column(2,
                       uiOutput("sample2")
                ), # Column
                column(2,
                       uiOutput("sample3")
                ), # Column
                column(2,
                       uiOutput("sample4")
                ), # column
                column(3,
                       uiOutput("sample4Bis")
                ) # column
                
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
                  ), # column
                  br(),
                  column(2,
                         uiOutput("textRealignTer")
                  ) # column                
              ) # box              
            ), # fluidRow
            fluidRow(
              uiOutput("textRealign2"),
              uiOutput("textRealign7"),
              
              div(style='overflow-y: scroll',
                  tableOutput("textRealign3")
              ), # div
              div(style='overflow-y: scroll',
                  uiOutput("textRealign5_ui")
              ) # div       
            ) # fluidRow
    ), # tabItem
    tabItem('projectExport',
            fluidRow(
              uiOutput('export')
            )
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

######################
############### SERVER
######################

server <- function(input, output, session) {  
  
  observe({
    if(is.null(data$temp)){
      output$premier = renderUI({
      fluidRow(
        box(
          background = "light-blue",
          height = 100,
          width = 12,
          column(9,                       
                 h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one"),
                 uiOutput("dataAtraiter")
          )
        )#box
      )#fluidRow
    })
    }
    else{
      if(dataChoisies$temp == 1){
        output$premier = renderUI({
          fluidRow(
            box(
              background = "light-blue",
              height = 100,
              width = 12,
              column(9,                       
                     h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one"),
                     uiOutput("dataAtraiter")
              ),
              column(3, 
                     br(),
                     actionButton("SuppDonne","Delete Data")
              ) 
            )#box
          )#fluidRow
        })
      }
      if(dataChoisies$temp == 0){
        output$premier = renderUI({
          fluidRow(
            box(
              background = "light-blue",
              height = 100,
              width = 12,
              column(9,                       
                     h2(icon("flask"),"Step 1. Choose to create a new project or load an existing one"),
                     uiOutput("dataAtraiter")
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
          height=400,
          h4("1. Choose the project folder"),
          selectInput("folderProjectIn", NULL ,  as.matrix(dir("Data")),multiple = FALSE),
          br(),
          h4("2. Choose elements to consider"),
          checkboxGroupInput("ElementGroup", label = "", 
                             choices = colnames(read.csv(paste(getwd(),"Data/example_1/calibrations",dir("Data/example_1/calibrations")[1],sep="/"), sep = ";", h = T, dec =","))[-1],
                             selected = colnames(read.csv(paste(getwd(),"Data/example_1/calibrations",dir("Data/example_1/calibrations")[1],sep="/"), sep = ";", h = T, dec =","))[-1], inline = T),
          br(),
          h4("3. Create the project"),
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
      
      output$export <- renderUI({
        actionButton("exprt","export")
      })
      
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
      
      if (input$createProjButton!=0) {data$temp <- elementR_project$new(paste("Data/",input$folderProjectIn,sep=""), elem = input$ElementGroup)
                                      
                                      temoin$temp = list(1, "Création projet", input$folderProjectIn, paste("Data/",input$folderProjectIn,sep=""), dir(paste("Data/",input$folderProjectIn,"/calibrations",sep="")), dir(paste("Data/",input$folderProjectIn,"/samples",sep="")), data$temp$listeElem)

      }
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
  ##### Sauvegardes #####
  #######################
  
  observe({    
    if(is.null(input$S1)){} 
    else{
      if(input$S1 != 0){
        myProject <- currentProject()
        save(myProject, file = paste0(input$folderProjectIn, ".RData"))

      }
    }
  })
  
  observe({    
    if(is.null(input$S2)){} 
    else{
      if(input$S2 != 0){
        myProject <- currentProject()
        save(myProject, file = paste0(input$folderProjectIn, ".RData"))
        
      }
    }
  })
  
  observe({    
    if(is.null(input$S3)){} 
    else{
      if(input$S3 != 0){
        myProject <- currentProject()
        save(myProject, file = paste0(input$folderProjectIn, ".RData"))
        
      }
    }
  })
  
  observe({    
    if(is.null(input$S4)){} 
    else{
      if(input$S4 != 0){
        myProject <- currentProject()
        save(myProject, file = paste0(input$folderProjectIn, ".RData"))
        
      }
    }
  })
  
  #######################
  ##### NISTS ###########
  #######################
  
  observe({
    if(is.null(currentProject())){}
    if(is.null(input$validDonne)){}
    else{
      if(input$validDonne == 0){
        
        output$Nist = renderUI({NULL})
        
        output$Nist2 = renderUI({NULL})
        
      }
      if(input$validDonne > 0 ){
        
        dataPlot2 <- reactiveValues(dat = NULL)
        
        observe({ 
          if(is.null(currentProject())){}
          if(is.null(input$CourbeNIST)){}
          if(is.null(input$bins)){}
          if(is.null(input$plat)){}
          else{
            currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setVector(bins = input$bins, plat = input$plat)
            dataPlot2$dat <- currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$getData(input$CourbeNIST, bins = input$bins, plat = input$plat) 
          }  
        })
        
        observe({
          if(is.null(input$bins)){}
          if(is.null(input$plat)){}
          else{
            if(input$saveNists > 0){
              isolate({
                currentProject()$setflagCalib(grep(input$calibrationIn, currentProject()$calibrationsFiles),1)
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setBins(input$bins)     
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setPlat(input$plat)
                currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$setDataDesanomalise(input$bins,input$plat)
              })                              
              
            }
          }
        })
        
        observe({
          if(is.null(input$DeleteNists)){}
          else{
            if(input$DeleteNists > 0 ){isolate(currentProject()$setflagCalib(grep(input$calibrationIn, currentProject()$calibrationsFiles),0) )
            }
          }
         })
        
        output$Nist = renderUI({        
          
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
                     actionButton("saveNists", "Save"),
                     actionButton("DeleteNists", "Delete")
              ),
              column(2,
                     br(),
                     actionButton("S1", "Save Project")
              )
            )#box
          )
        })  
        
        observe({          
          if(is.null(input$calibrationIn)){}
          else{
              input$saveNists
              input$DeleteNists
              observe({
                if(currentProject()$flag_Calib[grep(input$calibrationIn, currentProject()$calibrationsFiles)] == 0){
                  
                  output$Nist2 = renderUI({
                    
                    minB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                    maxB = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]
                    
                    minP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[1,1]
                    maxP = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[dim(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data)[1],1]           
                    
                    
                    if(temoin$temp[[1]] == 1){
                      
                      value1 = c((maxB - minB)/6,(maxB - minB)*5/6)
                      value2 = c((maxP - minP)*2/6,(maxP - minP)*4/6)
                      
                    }
                    if(temoin$temp[[1]] == 2){
                      
                      value1 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$bins
                      value2 = currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$plat
                    }   
                    
                    
                    fluidRow(
                      column(8, plotOutput("distPlot", height = '600px'),
                             br(),
                             column(1),
                             column(11,
                                    sliderInput("bins","Machine noise limits", value = value1, min = minB, max = maxB, width = '95%'),
                                    sliderInput("plat","Plateau limits", value = value2, min = minP, max = maxP, width = '95%')
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
                                      selectInput("listeElem", label = "", choices =  currentProject()$listeElem, selected  = "Li7", width = '100%') 
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
                    
                    maxY <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data)
                    
                    minX <- min(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1])
                    maxX <- max(currentProject()$calibrations[[1]]$rep_data[[1]]$data[,1])
                    
                    color <- rainbow(length(currentProject()$listeElem))
                    
                    plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,2],type ="b", ylab = "Nombre de coups", xlab = "Temps (s)", main = "Raw data", col = color[2], xlim = c(minX, maxX), ylim =c(0,maxY))
                    lapply(3:length(currentProject()$listeElem), function(x){
                      par(new = T)
                      plot(currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,1], currentProject()$calibrations[[1]]$rep_data[[grep(input$calibrationIn, currentProject()$calibrationsFiles)]]$data[,x],type ="b", ylab = "", xlab = "", main = "", col = color[x], xlim = c(minX, maxX), ylim =c(0,maxY), axes = F)
                    })
                    legend((1-10/100)*maxX,(1+50/1000)*maxY, currentProject()$listeElem, color)
                    
                    rect(-maxX,-maxY,input$bins[[1]],(1+10/100)*maxY, col = "#FF000064", border = NA)
                    rect(input$bins[[2]],-maxY,(1+10/100)*maxX,(1+10/100)*maxY, col = "#FF000064", border = NA)
                    
                    rect(input$plat[[1]],-maxY,input$plat[[2]],(1+10/100)*maxY, col ="#8B735564", border = NA)
                    
                    abline(v = input$bins[[1]], lty = "dashed", col = ("red"), lwd = 2)
                    abline(v = input$bins[[2]], lty = "dashed", col = ("red"), lwd = 2)
                    
                    abline(v = input$plat[[1]], lty = "dashed", col = ("burlywood4"), lwd = 2)
                    abline(v = input$plat[[2]], lty = "dashed", col = ("burlywood4"), lwd = 2)
                  })
                  
                  output$distPlot2 <- renderPlot({          
                    plot(dataPlot2$dat[,1], dataPlot2$dat[,grep(input$listeElem, colnames(dataPlot2$dat))],  type ="b", ylab = "Nombre de coups", xlab = "Temps (s)")  
                  })
                  
                }
                if(currentProject()$flag_Calib[grep(input$calibrationIn, currentProject()$calibrationsFiles)] == 1 ){                
                  output$Nist2 = renderUI({NULL})
                  
                }
              })

          }
          
        })
        
          

        
        }
    }

    })
     
  #######################
  ## VERIF STANDARDS ####
  #######################
               
  Stand <- reactiveValues(dat = NULL)
  
  observe({
    if(is.null(currentProject())){}
    if(is.null(input$calibrationIn)){}
    else{
      input$saveNists
      input$DeleteNists

      if(length(which(currentProject()$flag_Calib != 1)) == 0){
        
        
        Stand <- reactiveValues(val = list.files(paste("Data/",input$folderProjectIn, "/Standart", sep=""))[1])      
        
        lapply(1:length(currentProject()$flag_Calib), function(x){currentProject()$calibrations[[1]]$rep_data[[x]]$setdata_calibFinal()})
        
        currentProject()$calibrations[[1]]$setrep_dataFinale() 
        
        tab = reactiveValues(dat = currentProject()$calibrations[[1]]$setRep_table(currentProject()$listeElem))
        
        output$Text1 <- renderUI({
          
          fluidRow(
            box(width = 12,background = "olive", height = 100, 
                column(4, 
                       h2(icon("plug"),"Step 3. Verification derive machine")
                ), # column
                column(3,
                       selectInput("choixElem", label = "", choices =  currentProject()$listeElem, selected  = "Li7", width = '100%')
                ), # column
                column(3,
                       br(),
                       actionButton("valid", "Valider")
                ), # column                   
                column(2,
                       actionButton("S2", "Save Project") 
                ) # column
            ) # box
          )
        })#output$Text1
        
        output$Text2 <- renderUI({
        fluidRow(
          column(10,
                 plotOutput("graph1")
          ), # column
          column(2,
                 tableOutput('Tab')
          )
        )
        })
       
        output$Tab <- renderTable({
          
          tableau <- as.matrix(tab$dat[,grep(input$choixElem, colnames(tab$dat))])
          
          colnames(tableau) <- input$choixElem
          
          return(tableau)
          
        },digits = 4) 
        
        output$graph1 <- renderPlot({
          
          min <- (max(tab$dat[1:length(currentProject()$flag_Calib),grep(input$choixElem, colnames(tab$dat))]) - max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),grep(input$choixElem, colnames(tab$dat))]))*0.5
          
          max <- (max(tab$dat[1:length(currentProject()$flag_Calib),grep(input$choixElem, colnames(tab$dat))]) + max(tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),grep(input$choixElem, colnames(tab$dat))]))*1.5
          
          PlotIC(currentProject()$calibrationsFiles,tab$dat[1:length(currentProject()$flag_Calib),grep(input$choixElem, colnames(tab$dat))],tab$dat[(length(currentProject()$flag_Calib)+1):(2*length(currentProject()$flag_Calib)),grep(input$choixElem, colnames(tab$dat))],lengthSeg = 0.1, xlim =c(1,length(currentProject()$flag_Calib)),ylim=c(min, max))
          
        })
        
        observe({
          if(is.null(input$valid)){}
          else{
            if(input$valid > 0){
              
              output$Text3 <- renderUI({
                box(background = "olive", width = 12, height = 100,
                    column(4,
                           h2("Step 4. conversion coups/concentrations")
                    ), # column 
                    column(3, 
                           h2(paste0("Standard chargé : ", currentProject()$EtalonName))
                    ),
                    column(3,
                           br(),
                           actionButton("validEtalon", "Save"))
                    # column
                ) # box
              })
              
              output$tableEtalon <- renderTable({
                return(currentProject()$EtalonData)
              }, digits = 5)
            }
          }
        })
        
      }
      
      else{
        
        output$Text1 <- renderUI({NULL})#output$Text1
        
        output$Text2 <- renderUI({NULL})#output$Text2
        
      }
      
    }
    
  })
  
  observe({
    if(is.null(input$validEtalon)){}
    else{
      if(input$validEtalon > 0){
        lapply(1:length(currentProject()$samplesFiles), function(x){lapply(1:length(currentProject()$samples[[x]]$rep_data), function(t){currentProject()$samples[[x]]$rep_data[[t]]$setstandard(currentProject()$EtalonData)})})
      }
    }    
  })
  
  #######################
  ##### SAMPLES #########
  #######################
  
  temoinSample <- reactiveValues(temp = NULL)
  
  observe({    
    if(is.null(input$validEtalon)){}
    
    else{
      if(input$validEtalon == 0){
      
      output$sample1 <- renderUI({NULL})
      
      output$sample2 <- renderUI({NULL})  
      
      output$sample3 <- renderUI({NULL})
      
      output$sample4 <- renderUI({NULL})         

      output$sample4Bis <- renderUI({NULL})  
      
      output$Sample5 = renderUI({NULL})      
      
    }
      if(input$validEtalon > 0){
        
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
          p(actionButton("ValiderSample", "Save"),
            actionButton("DeleteSample", "Delete")
          )          
        })
        
        output$sample4Bis <- renderUI({
          actionButton("S3", "Save Project") 
        })
        
        dataPlot2Sample <- reactiveValues(datS = NULL) 
        
        observe({
          if(is.null(currentProject())){}
          if(is.null(input$SampleIn)){}
          if(is.null(input$SampleIn2)){}
          if(is.null(input$CourbeSample)){}
          else{
            if(length(grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)) == 0){}
            else{
              currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setVector(bins = input$binsSample, plat = input$platSample)
              dataPlot2Sample$datS <- currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$getData(input$CourbeSample,  bins = input$binsSample, plat = input$platSample)        
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
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setBins(input$binsSample)
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setPlat(input$platSample)
                  currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$setDataDesanomaliseConc(input$binsSample, input$platSample)
               }  
            })
            } 
        }) # observe
        
        observe({
          if(is.null(input$DeleteSample)){}
          else{
            input$DeleteSample
            isolate({             
              if(input$DeleteSample >0){
                  currentProject()$setflagSample(grep(isolate(input$SampleIn),currentProject()$samplesFiles), grep(isolate(input$SampleIn2), currentProject()$samples[[grep(isolate(input$SampleIn),currentProject()$samplesFiles)]]$rep_Files),FALSE)
                  }
            })

          }

          }) # observe

        observe({
          if(is.null(currentProject())){}
          if(is.null(input$SampleIn)){}
          if(is.null(input$SampleIn2)){}
          
          else{
            input$ValiderSample
            input$DeleteSample
            input$SampleIn2
            input$SampleIn
            observe({
              if(length(currentProject()$flag_Sample[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]) == 0){}
              else{
                if(currentProject()$flag_Sample[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] == FALSE){
                  
                  output$Sample5 = renderUI({
                    
                    minBS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                    maxBS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                    
                    minPS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[1,1]
                    maxPS = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[dim(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)[1],1]
                    
                    
                    if(temoin$temp[[1]] == 1){
                      
                      value1S = c((maxBS - minBS)/6,(maxBS - minBS)*5/6)
                      value2S = c((maxPS - minPS)*2/6,(maxPS - minPS)*4/6)
                      
                    }
                    if(temoin$temp[[1]] == 2){
                      
                      value1S = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$binsSample
                      value2S = currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$platSample
                    }   
                    
                    
                    fluidRow(
                      column(8, plotOutput("distPlotSample", height = '600px'),
                             br(),
                             column(1),
                             column(11,
                                    sliderInput("binsSample","Limites du Blanc", value = value1S, min = minBS, max = maxBS, width = '95%'),
                                    sliderInput("platSample","Limites du plateau", value = value2S, min = minPS, max = maxPS, width = '95%')
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
                                      selectInput("listeElemSample", label = "", choices =  currentProject()$listeElem, selected  = "Li7", width = '100%') 
                               ), # column
                               column(6,
                                      h4(icon("area-chart"),"Choose Curve to plot"),
                                      selectInput("CourbeSample", label = "", choices =  c("Blanc","Brute", "Plateau","- Moyenne Blanc","> LOD", "Normalisé", "Sans Anomalie", "Concentration"), selected  = "Plateau", width = '100%') 
                               )# column 
                             ) # box
                             
                             
                      ) #column  
                      
                    ) #fluidRow
                  })
                  
                  output$distPlotSample <- renderPlot({
                    
                    maxY <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data)
                    
                    minX <- min(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1])
                    maxX <- max(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1])
                    
                    color <- rainbow(length(currentProject()$listeElem))
                    
                    plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,2],type ="b", ylab = "Nombre de coups", xlab = "Temps (s)", main = "Raw data", col = color[2], xlim = c(minX, maxX), ylim =c(0,maxY))
                    lapply(3:length(currentProject()$listeElem), function(x){
                      par(new = T)
                      plot(currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,1], currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_data[[grep(input$SampleIn2,currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)]]$data[,x],type ="b", ylab = "", xlab = "", main = "", col = color[x], xlim = c(minX, maxX), ylim =c(0,maxY), axes = F)
                    })
                    legend((1-10/100)*maxX,(1+50/1000)*maxY, currentProject()$listeElem, color)
                    
                    rect(-maxX,-maxY,input$binsSample[[1]],(1+10/100)*maxY, col = "#FF000064", border = NA)
                    rect(input$binsSample[[2]],-maxY,(1+10/100)*maxX,(1+10/100)*maxY, col = "#FF000064", border = NA)
                    
                    rect(input$platSample[[1]],-maxY,input$platSample[[2]],(1+10/100)*maxY, col ="#8B735564", border = NA)
                    
                    abline(v = input$binsSample[[1]], lty = "dashed", col = ("red"), lwd = 2)
                    abline(v = input$binsSample[[2]], lty = "dashed", col = ("red"), lwd = 2)
                    
                    abline(v = input$platSample[[1]], lty = "dashed", col = ("burlywood4"), lwd = 2)
                    abline(v = input$platSample[[2]], lty = "dashed", col = ("burlywood4"), lwd = 2)
                  })
                  
                  output$distPlot2Sample <- renderPlot({
                    plot(dataPlot2Sample$datS[,1], dataPlot2Sample$datS[,grep(input$listeElemSample, colnames(dataPlot2Sample$datS))],  type ="b", ylab = "Nombre de coups", xlab = "Temps (s)")  
                  })
                  
                }
                if(currentProject()$flag_Sample[[grep(input$SampleIn,currentProject()$samplesFiles)]][grep(input$SampleIn2, currentProject()$samples[[grep(input$SampleIn,currentProject()$samplesFiles)]]$rep_Files)] == TRUE){
                  
                  output$Sample5 = renderUI({NULL})
                  
                }
                
              
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
          
        }) # observe
        
      }
    }
  })
  
  #######################
  ##### REALIGNEMENT ####
  #######################
  
  observe({
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
      
      output$textRealignTer <- renderUI({
        
        actionButton("S4", "Save Project")
        
      })
      
      observe({
        if(is.null(currentProject())){}
        if(is.null(input$selectRealign)){}
        else{
          currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataFiltre()
          
        }
      })
      
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
        if(is.null(input$DemoyennerSpot)){}
        else{
          input$DemoyennerSpot
          isolate({
            if(input$DemoyennerSpot > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "spot", position = 1, valeur = 0)
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$initial("rep_dataFinale")
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
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(1,2,3,4,5), valeur = 0)
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$initial("rep_dataNonCorrel")
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })
        }
      }) # observe
      
      observe({
        if(is.null(input$calcul)){}
        else{
          isolate({
            input$calcul
            if(input$calcul > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 2, valeur = 1)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })
        }
      }) # observe
      
      observe({
        if(is.null(input$recalculer)){}
        else{
          isolate({
            input$recalculer
            if(input$recalculer > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(2,3,4,5), valeur = 0)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })
        }
      }) # observe            
      
      observe({
        if(is.null(input$choix)){}
        else{
          isolate({
            if(input$choix > 0){
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 3, valeur = 1)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })               
        }
      }) # observe
      
      observe({
        if(is.null(input$myplot_click)){}
        else{
          isolate({
            input$myplot_click
            if(input$myplot_click > 0){                  
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 4, valeur = 1)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
              
            } 
          })
        }
      }) # observe
      
      observe({
        if(is.null(input$deletePoint)){}
        else{
          isolate({
            input$deletePoint
            if(input$deletePoint > 0){                  
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(4,5), valeur = 0)
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
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = 5, valeur = 1)
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
              currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_flag(type = "raster", position = c(1,2,3,4,5), valeur = 0)
              updateSelectInput(session, "selectRealign", selected = input$selectRealign)
              updateSelectInput(session, "typeTraitement", selected = input$typeTraitement)
            } 
          })                
        }
      }) # observe
      
      deplace <- reactiveValues(val = NULL)
      vectResults <- reactiveValues(temp = c("a","b","c","d","e","f","g","h","i","j"))
      correl <- reactiveValues(temp = NULL)
      
      observe({
        if(is.null(input$typeTraitement)){}
        if(is.null(input$selectRealign)){}
        else{ 
          input$MoyenneSpot
          input$DemoyennerSpot
          input$SupprSpot
          input$MoyenneRaster
          input$DemoyennerRaster
          input$recalculer
          input$choix
          input$deletePoint
          input$SauvegarderReal
          input$Suppr
          input$calcul
          input$myplot_click
          input$SauvegarderSpot
                    
          if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[1] == 0 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[1] != 1){   
            
            currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_dataInterm(type = "spot")
            
            output$textRealign2 <- renderUI({
              box(background = "black", width = 3, height = 200,
                  column(12,
                         fluidRow(
                           h3("Moyenne des spots :"),
                           p(actionButton("MoyenneSpot", "Moyenner"),actionButton("DemoyennerSpot","Demoyenner"))
                         )                               
                  )                   
              ) # box                                                                             
            })
            
            output$textRealign3 <- renderTable({  
              
              tableau <- rbind(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpot, rep(NA,ncol(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpot)), rep(NA,ncol(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpot)))
              
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
                           h3("Moyenne des spots :"),
                           p(actionButton("MoyenneSpot", "Moyenner"),actionButton("DemoyennerSpot","Demoyenner")),
                           actionButton("SauvegarderSpot", "Sauvegarder Moyenne")
                         )                               
                  )                   
              ) # box                                                                             
            })
            
            output$textRealign3 <- renderTable({  
              
              tableau <- rbind(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis, currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis)
              
              return(tableau)
              
            }, digits = 5)
            
            output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
          } # if
          
          if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] == 1){
                        
            output$textRealign2 <- renderUI({
              box(background = "black", width = 3, height = 200,
                  column(12,
                         actionButton("SupprSpot", "Supprimer Moyenne")
                  )                               
                  
              ) # box                                                                             
            })
          } # if
          
          if(input$typeTraitement == "spot" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[5] == 1){
                        
            output$textRealign2 <- renderUI({
              box(background = "black", width = 3, height = 100,
                  column(12,
                         h3("validé en raster")
                  )                               
                  
              ) # box                                                                             
            })            
            
            output$textRealign5 <- renderPlot({NULL
            })
            
            output$textRealign5_ui = renderUI({
              
              plotOutput("textRealign5")
              
            })
            
            output$textRealign3 <- renderTable({ NULL }, digits = 5)
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[1] == 0 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[1] != 1){
            
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
                    
                    plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "temps (s)", ylab = "Concentrations", type = "b", main = "bla", col = rainbow(length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files))[x])
                    
                    par(new = T)
                    
                  })
                  
                  
                })
                
                output$textRealign5_ui = renderUI({
                  
                  plotOutput("textRealign5")
                  
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
              
              box(background = "black", width = 3, height = 400,
                  column(12,  
                         fluidRow(
                           column(6, h3("Rasters realignment :")), 
                           column(6, h3(icon("check"), ""))
                         ),
                         br(),
                         actionButton("DemoyennerRaster","Delete averaging"), 
                         br(),br(),
                         h3("Elimination points correles"),
                         br(),
                         fluidRow(
                           column(6,numericInput("vitesse", "vitesse du raster", value = value3,step = 1)), 
                           column(6,numericInput("Dimraster", "Taille du laser", value = value4,step = 1)))
                         ,
                         br(),
                         actionButton("calcul", "calculer")
                         
                  ) # column                   
              ) # box
            }) 
            
            output$textRealign5 <- renderPlot({
              
              ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
              
              xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))))
              
              lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(x){
                
                plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[x]][,input$elemRaster] , xlim = xlim, ylim = ylim, xlab = "temps (s)", ylab = "Concentrations", type = "b", col = rainbow(length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files))[x])
                
                par(new = T)
                
              })
              
              plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b")
              
            })
            
            output$textRealign5_ui = renderUI({
              
              plotOutput("textRealign5")
              
            }) 
            
            
            
            
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[2] == 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[3] != 1){
                        
            currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$set_repParam(input$vitesse,input$Dimraster)
                        
            correl$temp <- round(input$Dimraster/input$vitesse, digits = 0)
            
            output$textRealign2 <- renderUI({
              
              box(background = "black", width = 3, height = 450,
                  column(12,  
                         fluidRow(
                           column(6, h3("Rasters realignment :")), 
                           column(6, h3(icon("check"), ""))
                         ),
                         br(),
                         actionButton("DemoyennerRaster","Delete averaging"), 
                         br(),br(),
                         fluidRow(
                           column(8, h3("Elimination points correles")), 
                           column(4, h3(icon("check"), ""))
                         ), 
                         br(),
                         actionButton("recalculer", "recalculer"),
                         br(),
                         br(),
                         h3("choix du premier point"),
                         br(),
                         actionButton("choix","selection premier point analyse")
                         
                  ) # column                   
              ) # box
              
              
            }) # textRealign2
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[3] == 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[4] != 1){
            
            correl$temp <- round(input$Dimraster/input$vitesse, digits = 0)
            
            if(temoin$temp[[1]] == 1){       
                            
              output$textRealign2 <- renderUI({
                
                box(background = "black", width = 3, height = 450,
                    column(12,  
                           fluidRow(
                             column(6, h3("Rasters realignment :")), 
                             column(6, h3(icon("check"), ""))
                           ),
                           br(),
                           actionButton("DemoyennerRaster","Delete averaging"), 
                           br(),br(),
                           fluidRow(
                             column(8, h3("Elimination points correles")), 
                             column(4, h3(icon("check"), ""))
                           ), 
                           br(),
                           actionButton("recalculer", "recalculer"),
                           br(),
                           br(),
                           h3("choix du premier point"),
                           br(),
                           actionButton("choix","selection premier point analyse")
                           
                    ) # column                   
                ) # box
                
                
              }) # textRealign2
              
              output$textRealign5 <- renderPlot({
                
                ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
                
                xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))))
                
                plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b")
                
              })
              
              output$textRealign5_ui = renderUI({
                
                plotOutput("textRealign5", clickId = "myplot_click")
                
              })
              
            }
                        
            if(temoin$temp[[1]] == 2){
                            
              output$textRealign5 <- renderPlot({
                                
                ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
                
                xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))))
                
                plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b")
                
                points(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_coord[1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_coord[input$elemRaster] ,pch = 16, cex = 2.5, col = "violet")
                                
              })
              
              output$textRealign5_ui = renderUI({
                
                plotOutput("textRealign5", clickId = "myplot_click")
                
              })
              
            }
            
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[4] == 1 & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[5] != 1 & !is.null(input$myplot_click)){        
                         
            currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setrep_dataNonCorrel(correl$temp, input$myplot_click)
            
            currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$setRep_coord(input$myplot_click)
            
            output$textRealign2 <- renderUI({
              
              box(background = "black", width = 3, height = 550,
                  column(12,  
                         fluidRow(
                           column(6, h3("Rasters realignment :")), 
                           column(6, h3(icon("check"), ""))
                         ),
                         br(),
                         actionButton("DemoyennerRaster","Delete averaging"), 
                         br(),br(),
                         fluidRow(
                           column(8, h3("Elimination points correles")), 
                           column(4, h3(icon("check"), ""))
                         ), 
                         br(),
                         actionButton("recalculer", "recalculer"),
                         br(),
                         br(),
                         fluidRow(
                           column(8, h3("choix du premier point")), 
                           column(4, h3(icon("check"), ""))
                         ),
                         br(),
                         actionButton("deletePoint","delete"),
                         br(),
                         br(),
                         br(),
                         fluidRow(
                           column(8, h3("Sauvegarde Realignment")), 
                           column(4, actionButton("SauvegarderReal","Save"))
                         )
                         
                  ) # column                   
              ) # box
              
              
            }) # textRealign
            
            output$textRealign5 <- renderPlot({
              
              ylim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,input$elemRaster]})), na.rm = T))
              
              xlim <- c(min(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))),max(unlist(lapply(1:length(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_Files), function(i){currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRaster[[i]][,1]}))))
              
              plot(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,1],currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermRasterBis[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "b")
              
              points(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataNonCorrel[,1], currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataNonCorrel[,input$elemRaster], xlim = xlim, ylim = ylim, xlab = "", ylab = "", pch = 16, cex = 2.5, col = "red")
              
            })
            
            output$textRealign5_ui = renderUI({
              
              plotOutput("textRealign5")
              
            })
            
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagRaster[5] == 1){
                        
            output$textRealign2 <- renderUI({
              
              box(background = "black", width = 3, height = 100,
                  column(12,
                         actionButton("Suppr","Supprimer Realignement")
                  )
              )
              
            }) # textRealign2
          } # if
          
          if(input$typeTraitement == "raster" & currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_flagSpot[2] == 1){
            
            output$textRealign2 <- renderUI({
              box(background = "black", width = 3, height = 100,
                  column(12,
                         h3("validé en spot")
                  )                               
                  
              ) # box                                                                             
            })
            
            output$textRealign3 <- renderTable({  
              
              tableau <- rbind(currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis, currentProject()$samples[[grep(input$selectRealign,currentProject()$samplesFiles)]]$rep_dataIntermSpotBis)
              
              return(tableau)
              
            }, digits = 5)
            
            output$textRealign5 <- renderPlot({NULL}, bg = "transparent")
            output$textRealign5_ui = renderUI({
              
              plotOutput("textRealign5")
              
            })
          } # if
          
          
          
          
        } # else
        
      }) # observe               
      
    } # if
    
    if(length(which(temoinSample$temp == T)) == 0){
      output$textRealign <- renderUI({NULL})
      output$textRealignBis <- renderUI({NULL})
      output$textRealignBis2 <- renderUI({NULL})
      output$textRealignTer <- renderUI({NULL})
      output$textRealign5_ui <- renderUI({NULL})
      output$textRealign2 <- renderUI({NULL})
      output$textRealign7 <- renderUI({NULL})
      output$textRealign3 <- renderUI({NULL})
      
    }
  }) 
  
}#eo server

lePlusProche = function(x,y){
  temp = list()
  
  temp[[2]]= which(abs(x-y) == min(abs(x-y), na.rm = T))
  temp[[1]] = x[temp[[2]]]
  if (length(temp[[1]])!=1){temp[[2]] = min(temp[[2]], na.rm = T)
                            temp[[1]] = x[temp[[2]]]
  }
  
  names(temp) <- c("le_plus_proche", "place")
  
  return(temp)
}

PlotIC <- function(nom, Mean,SD, lengthSeg, xlim, ylim, type = "p"){
  plot(as.factor(nom), rep(-1,length(nom)), ylim = ylim, xlim = xlim, type = type)
  points(1:length(Mean),Mean)
  segments(1:length(Mean), Mean-SD, 1:length(Mean), Mean+SD)
  segments((1:length(Mean))-lengthSeg,Mean+SD,(1:length(Mean))+lengthSeg,Mean+SD)
  segments((1:length(Mean))-lengthSeg,Mean-SD,(1:length(Mean))+lengthSeg,Mean-SD)
}

######################
######## CALL shinyApp
######################
shinyApp(ui, server)