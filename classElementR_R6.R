#' Otholith data preparation Web Application for R
#'6 object implementation Blabla Blablabla Blablabla BlablablaBlabl
#' Blablabla Blabla Blablabla Blablabla BlablablaBlablablaBlablabla
#' Blablabla Blabla Blablabla Blablabla BlablablaBlablablaBlablabla
#' Blablabla Blabla Blablabla Blablabla BlablablaBlablablaBlablabla
#' Blablabla Blabla Blablabla Blablabla BlablablaBlablablaBlablabla
#'
#' @seealso \link{elementR-options} for documentation about global options.
#'
#' @name elementR_Classes
#' @aliases shiny
#' @docType package
#' @import htmltools httpuv xtable digest R6 mime
#' @importFrom RJSONIO fromJSON
NULL


library(R6)

library(tseries)
library(xts)

############################################################
############################################################
##################################### elementR_data Class
############################################################
############################################################


#test external function (it works ;) but don't know if it's good)
aaa <- function(x) x


elementR_data <- R6Class("elementR_data",
                         public = list(
                           name = NA,
                           data = NA,
                           dataBlanc = NA,
                           dataplateau = NA,
                           dataPlateauMoinsBlanc = NA,
                           dataPlateauMoinsBlancSupLOD = NA,
                           dataPlateauMoinsBlancSupLODNorm = NA,
                           dataPlateauMoinsBlancSupLODNormSansAnom = NA,
                           fPath = NA,
                           gg = NA,
                           platBins = NA,
                           initialize = function(fPath=NULL) {
                             if(is.null(fPath)) stop("error, fPath missing !")
                             charStrings <- unlist(lapply(strsplit(fPath,"[.]"),strsplit,split="/"))
                             self$name <- charStrings[length(charStrings)-1] 
                             self$fPath <- fPath
                             self$gg <- aaa(1)
                             #####dealing with data
                             d <- read.csv(fPath,sep=";",dec=",",header=TRUE)
                             self$data <- d
                             self$greet()
                           },#initialize
                           
                           ######################################## Data Blanc
                           
                           setVector = function(bins, plat) {
                             
                             vect <- vector()  
                             
                             vect[which(self$data[,1] < bins)] <- "B"
                             
                             vect[which(self$data[,1] >= plat[1] & self$data[,1] <= plat[2])] <- "P"
                             
                             vect[which(is.na(vect))] <- "R"
                             
                             self$platBins <- vect
                             
                           }, #setVector
                           
                           setDataBlanc = function(bins, plat) {
                             
                             self$setVector(bins, plat)
                             
                             subDat <- self$data[which(self$platBins=="B"),]       
                             
                             self$dataBlanc <- subDat
                             
                           }, #setDataBlanc
                           
                           ######################################## Data Plateau
                           
                           setDataPlateau = function(bins, plat) {
                             
                             self$setDataBlanc(bins, plat)
                             
                             subDat <- self$data[which(self$platBins=="P"),]
                             
                             self$dataplateau <- subDat
                             
                           }, # setDataPlateau
                           
                           ######################################## Data plateau moins blanc
                           
                           setDataPlateauMoinsBlanc = function(bins, plat) {
                             
                             self$setDataPlateau(bins, plat)
                             
                             tempo <- apply(self$dataBlanc[,-1], 2, mean, na.rm = T)
                                                          
                             subDat <- sapply(1:length(apply(self$dataBlanc[,-1], 2, mean, na.rm = T)),function(x){                              
                               
                               self$dataplateau[,x+1] - tempo[x]  
                               
                             })
                                                          
                             subDat <- cbind(as.matrix(self$dataplateau[,1]), subDat)
                             
                             colnames(subDat) <- colnames(self$dataplateau)
                             
                             self$dataPlateauMoinsBlanc <- subDat
                             
                           }, # setDataPlateauMoinsBlanc
                           
                           ####################################### data plateau moins blanc > LOD
                           
                           setDataPlateauMoinsBlancSupLOD = function(bins, plat) {
                             
                             self$setDataPlateauMoinsBlanc(bins, plat)
                             
                             LOD <- 3*apply(self$dataBlanc[,-1], 2, sd, na.rm =T)
                             
                             subDat <- do.call(rbind,lapply(1:nrow(self$dataPlateauMoinsBlanc),function(x){ l <- self$dataPlateauMoinsBlanc[x,-1]
                                                                                                            
                                                                                                            l[ l< LOD ] <- NA
                                                                                                            
                                                                                                            l
                                                                                                            
                             })) 
                             
                             subDat <- cbind(as.matrix(self$dataPlateauMoinsBlanc[,1]),subDat)
                             
                             colnames(subDat) <- colnames(self$dataPlateauMoinsBlanc)
                             
                             self$dataPlateauMoinsBlancSupLOD <- subDat
                             
                           }, # setDataPlateauMoinsBlancSupLOD
                           
                           ####################################### data plateau moins blanc > LOD Normalisé
                           
                           setDataNorm = function(bins, plat) {
                             
                             self$setDataPlateauMoinsBlancSupLOD(bins, plat)
                             
                             subDat <- sapply(2:ncol(self$dataPlateauMoinsBlancSupLOD),function(x){ 
                               
                               self$dataPlateauMoinsBlancSupLOD[,x]/self$dataPlateauMoinsBlancSupLOD[,grep("Ca",colnames(self$dataPlateauMoinsBlancSupLOD))]
                               
                             })
                                                          
                             subDat <- cbind(as.matrix(self$dataPlateauMoinsBlancSupLOD[,1]),subDat)                             
                             
                             colnames(subDat) <- colnames(self$dataPlateauMoinsBlancSupLOD)
                             
                             self$dataPlateauMoinsBlancSupLODNorm <- subDat
                             
                           },#setDataNorm
                           
                           ############################################ Data sans anomalie moyenne +/- 2 ecart-types
                           
                           setDataDesanomalise = function(bins, plat){
                             
                             print("step4")
                             
                             self$setDataNorm(bins, plat)
                             
                             print("step5")
                             
                             ValMax <- apply(self$dataPlateauMoinsBlancSupLODNorm[,-1], 2, function(k){mean(k, na.rm = T) + 2*sd(k,na.rm = T)})
                             
                             print("step6")
                             
                             ValMin <- apply(self$dataPlateauMoinsBlancSupLODNorm[,-1], 2, function(k){mean(k, na.rm = T) - 2*sd(k,na.rm = T)})
                             
                             print("step7")
                             
                             subDat <- do.call(rbind,lapply(1:dim(self$dataPlateauMoinsBlancSupLODNorm[,-1])[1], function(z){
                               
                               l <- self$dataPlateauMoinsBlancSupLODNorm[z,-1]
                               l[l < ValMin | l > ValMax] <- NA
                               l
                               
                             }))
                             
                             print("step8")
                             
                             self$dataPlateauMoinsBlancSupLODNormSansAnom <- cbind(as.matrix(self$dataPlateauMoinsBlancSupLODNorm[,1]),subDat)
                             
                           }, #setDataDesanomalise
                           
                           ############################################
                           ############################################
                           ############################################
                           
                           getData = function(CourbeNIST, bins, plat){
                             
                             if(CourbeNIST =="Blanc") {self$setDataBlanc(bins, plat)
                                                       return(self$dataBlanc)}
                             
                             if(CourbeNIST =="Brute") {return(self$data) }
                             
                             if(CourbeNIST =="Plateau") {self$setDataPlateau(bins, plat)
                                                         return(self$dataplateau)}
                             
                             if(CourbeNIST =="- Moyenne Blanc") {self$setDataPlateauMoinsBlanc(bins, plat)
                                                                 return(self$dataPlateauMoinsBlanc) }
                             
                             if(CourbeNIST =="> LOD") {self$setDataPlateauMoinsBlancSupLOD(bins, plat)
                                                       return(self$dataPlateauMoinsBlancSupLOD) }
                             
                             if(CourbeNIST =="Normalisé") {self$setDataNorm(bins, plat)
                                                           return(self$dataPlateauMoinsBlancSupLODNorm) }
                             
                             if(CourbeNIST =="Sans Anomalie") {self$setDataDesanomalise(bins, plat)
                                                               return(self$dataPlateauMoinsBlancSupLODNormSansAnom) } 
                           },
                           
                           greet = function() {
                             cat("###\n")
                             cat("Hello I'm an elementR data object\n")
                             cat(paste0("My file is ", self$fPath, "\n"))  
                             cat(paste0("My name is ", self$name, "\n"))
                             dims <- dim(self$data)
                             cat(paste0("My data dim is ", dims[1], " X ",dims[2],"\n"))
                             cat("And my names are ", names(self$data),"\n")
                             cat("###\n")
                           }#greet
                           
                         ),#public
                         private = list(
                           aMethod = function() self$name
                         )#private
)#elementR_data

#nR1 <- function(){
#  require(dygraphs)
#  d <- elementR_data$new("Data/example_1/calibrations/NIST_1.csv")
#  dygraph(d$dataIRTS)
#}#eo notRun

############################################################
############################################################
################################# elementR_calibration Class
############################################################
############################################################

elementR_calibration <- R6Class("elementR_calibration",
                                inherit = elementR_data,
                                public = list(
                                  data_calibFinalMean = NA,
                                  data_calibFinalSD = NA,
                                  type = "calibration",
                                  plat = c(NA,NA),
                                  bins = NA,
                                  
                                  ##########################################################
                                  
                                  setdata_calibFinal = function(){
                                    self$data_calibFinalMean <- apply(self$dataPlateauMoinsBlancSupLODNormSansAnom,2,mean, na.rm = T)[-1]  
                                    self$data_calibFinalSD <- apply(self$dataPlateauMoinsBlancSupLODNormSansAnom,2,sd, na.rm = T)[-1]
                                  }, # setdata_calibFinal
                                  
                                  setBins = function(x){
                                    
                                    self$bins <- x
                                    
                                  },
                                  
                                  setPlat = function(x){
                                    
                                    self$plat <- x
                                    
                                  },
                                  
                                  ##########################################################
                                                                    
                                  greet = function() {
                                    cat("###\n")
                                    cat("Hello I'm an elementR calibration data object\n")
                                    cat(paste0("My file is ", self$fPath, "\n"))    	
                                    cat(paste0("My name is ", self$name, "\n"))
                                    dims <- dim(self$data)
                                    cat(paste0("My data dim is ", dims[1], " X ",dims[2],"\n"))
                                    cat("And my names are ", names(self$data),"\n")
                                    cat("###\n")
                                  }#greet
                                  
                                )#public
)#elementR_calibration

#elementR_calibration$new("Data/example_1/calibrations/NIST_1.csv")

############################################################
############################################################
################################# elementR_sample Class
############################################################
############################################################

elementR_sample <- R6Class("elementR_sample",
                           inherit = elementR_data,
                           public = list(
                             type = "sample",
                             standard = NA,
                             dataPlateauMoinsBlancSupLODNormSansAnomConc = NA,
                             platSample = c(NA,NA),
                             binsSample = NA,
                             
                             setBins = function(x){
                               
                               self$binsSample <- x
                               
                             },
                             
                             setPlat = function(x){
                               
                               self$platSample <- x
                               
                             },
                             
                             setstandard = function(stand){
                               
                               self$standard <- stand
                                                              
                             },                          
                         
                             ##########################################################
                             
                             setDataDesanomaliseConc = function(bins, plat, SimNist){
                               
                               print("step1")
                               
                               self$setDataDesanomalise(bins, plat)    
                               
                               print("step2")
                                                           
                               temp <- sapply(2:ncol(self$dataPlateauMoinsBlancSupLODNormSansAnom), function(x){
                                 
                                 print(x)
                                 
                                 self$dataPlateauMoinsBlancSupLODNormSansAnom[,x] * self$standard[1,x-1]/ SimNist[x-1]
                                 
                                 })
                               
                               self$dataPlateauMoinsBlancSupLODNormSansAnomConc <- cbind(as.matrix(self$dataPlateauMoinsBlancSupLODNormSansAnom[,1]),temp)
                               
                               print("step3")
                               
                               colnames(self$dataPlateauMoinsBlancSupLODNormSansAnomConc) <- colnames(self$dataPlateauMoinsBlancSupLODNormSansAnom)
                               
                             }, #setDataDesanomaliseConc
                             
                             ##########################################################
                             
                             getData = function(CourbeNIST, bins, plat, SimNist){
                              
                               if(CourbeNIST =="Blanc") {self$setDataBlanc(bins, plat)
                                                         return(self$dataBlanc)}
                               
                               if(CourbeNIST =="Brute") {return(self$data) }
                               
                               if(CourbeNIST =="Plateau") {self$setDataPlateau(bins, plat)
                                                           return(self$dataplateau)}
                                                              
                               if(CourbeNIST =="- Moyenne Blanc") {self$setDataPlateauMoinsBlanc(bins, plat)
                                                                   return(self$dataPlateauMoinsBlanc) }
                                                              
                               if(CourbeNIST =="> LOD") {self$setDataPlateauMoinsBlancSupLOD(bins, plat)
                                                         return(self$dataPlateauMoinsBlancSupLOD) }
                                                              
                               if(CourbeNIST =="Normalisé") {self$setDataNorm(bins, plat)
                                                             return(self$dataPlateauMoinsBlancSupLODNorm) }
                                                              
                               if(CourbeNIST =="Sans Anomalie") {self$setDataDesanomalise(bins, plat)
                                                                 return(self$dataPlateauMoinsBlancSupLODNormSansAnom) } 
                                                              
                               if(CourbeNIST =="Concentration") {self$setDataDesanomaliseConc(bins, plat, SimNist)
                                                                 return(self$dataPlateauMoinsBlancSupLODNormSansAnomConc) }
                             },
                             
                             greet = function() {
                               cat("###\n")
                               cat("Hello I'm an elementR sample data object\n")
                               cat(paste0("My file is ", self$fPath, "\n"))  		
                               cat(paste0("My name is ", self$name, "\n"))
                               dims <- dim(self$data)
                               cat(paste0("My data dim is ", dims[1], " X ",dims[2],"\n"))
                               cat("And my names are ", names(self$data),"\n")
                               cat("###\n")
                             }#greet
                           )#public
)#elementR_Sample

#d <- elementR_sample$new("Data/example_1/samples/Sample1.csv")

############################################################
############################################################
################################# elementR_project Class
############################################################
############################################################

elementR_project <- R6Class("elementR_project",
                            public = list(
                              name = NA,                           
                              folderPath = NA,
                              calibrationsPath = NA,
                              calibrationsFiles = NA,
                              calibrations = NA,
                              samplesPath = NA,
                              samplesFiles = NA,
                              samples = NA,
                              EtalonPath = NA,
                              EtalonName = NA,
                              EtalonData = NA,
                              listeElem = NA,
                              flag_Calib = NA,
                              flag_Sample = NA,
                              SummaryNist = NA,
                              
                              setflagCalib = function(place, valeur){
                                
                                self$flag_Calib[place] <- valeur
                                
                                return(self$flag_Calib)
                                
                              },#setflagCalib
                              
                              setflagSample = function(sample, replicat, valeur){
                                
                                self$flag_Sample[[sample]][replicat] <- valeur
                                
                                return(self$flag_Sample)
                                
                              }, #setflagSample
                              
                              setSummaryNist = function(x){
                                
                                self$SummaryNist <- x
                                
                              },
                              
                              initialize = function(folderPath=NULL, elem = NULL) {   
                                if(is.null(folderPath)) stop("\n #### A folder path is required to create an elementR project '[^_-]'")
                                if(sum(c("calibrations","samples","Standart")%in%dir(folderPath))!=3) stop("\n #### A folder should contain two subfolder 'calibrations' and 'samples', as well as a file named standards.csv to create an elementR project '[^_-]'")
                                self$folderPath <- folderPath
                                charStrings <- unlist(strsplit(folderPath,"/"))
                                self$name <- charStrings[length(charStrings)]
                                                                
                                # Etalon
                                self$EtalonPath <- paste0(folderPath,"/Standart")
                                self$EtalonName <- dir(self$EtalonPath)[1]
                                temp <- read.csv(paste0(folderPath,"/Standart/", self$EtalonName), sep = ";", dec = ",", h = T)
                                self$EtalonData <- t(as.matrix(sapply(2: ncol(temp), function(x){as.numeric(as.character(temp[1,x]))})))
                                colnames(self$EtalonData) <- colnames(temp)[2:length(colnames(temp))]
                                
                                #calibrations
                                self$calibrationsPath <- paste0(folderPath,"/calibrations")
                                calFiles <- dir(self$calibrationsPath)                                
                                self$calibrationsFiles <- calFiles
                                
                                calList <- lapply(paste0(self$calibrationsPath, sep=""),function(f){elementR_repCalib$new(f, stand = self$EtalonData)})
                                names(calList) <- "Rep_calibration"                          
                                self$calibrations <- calList
                                
                                #samples
                                self$samplesPath <- paste0(folderPath,"/samples")
                                sampFiles <- dir(self$samplesPath)
                                self$samplesFiles <- sampFiles
                                
                                
                                sampList <- lapply(paste0(self$samplesPath,"/",sampFiles),function(f){elementR_repSample$new(f, stand = self$EtalonData)})
                                names(sampList) <- sampFiles
                                self$samples <- sampList  
                                
                                self$listeElem <- elem
                                
                                self$greet()

                                
                                # Flags
                                self$flag_Calib <- rep(0, length(self$calibrationsFiles))
                                names(self$flag_Calib) <- self$calibrationsFiles
                                
                                flagTemp <- lapply(1:length(self$samplesFiles), function(x){dir(paste0(folderPath,"/samples/",self$samplesFiles[x]))})
                                self$flag_Sample <- lapply(1: length(flagTemp), function(x){ r <- rep(0, length(flagTemp[[x]])) ; names(r) <- flagTemp[[x]] ; r})                                
                                
                                self$greet()
                              },#initialize
                              
                              greet = function() {
                                cat("######\n")
                                cat("Hello my name is ", self$name, "\n")
                                cat("I'm an elementR project object\n")
                                cat("My folder is ", self$folderPath, "\n")  		
                                cat("My calibrations folder is ", self$calibrationsPath, "\n")
                                cat("My calibration files are ", self$calibrationsFiles, "\n")
                                cat("My samples folder is ", self$samplesPath, "\n")
                                cat("My samples files are ", self$samplesFiles, "\n")
                                cat("My Elements are ", self$listeElem, "\n")
                                #cat("flag sample is ", self$flag_Sample, "\n")
                                cat("######\n")     
                              }#greet
                            ),#public
                            private = list(
                              aMethod = function() self$name
                            )#private
)#elementR_project

####################################################################
####################################################################
#################################### ElementR repertoire class ####
####################################################################

elementR_rep <- R6Class("elementR_rep",
                        public = list(
                          rep_name = NA,
                          rep_folderPath = NA,
                          rep_Files = NA,
                          rep_data = NA,
                          rep_table = NA,
                          rep_Stand = NA,                          
                          rep_pas = NA, # moyenne du pas de temps entre deux analyses
                          
                          initialize = function(rep_folderPath=NULL, stand = NULL) {
                            
                            self$rep_Stand <- stand
                            
                            charStrings <- unlist(strsplit(rep_folderPath,"/"))
                            self$rep_name <- charStrings[length(charStrings)]
                            self$rep_folderPath <- rep_folderPath
                            
                            calFiles <- dir(self$rep_folderPath)
                            self$rep_Files <- calFiles 
                            
                            self$create()
                            
                            self$greet()
                          },#initialize              
                          
                          setRep_pas = function(){
                            
                            self$rep_pas <- round(mean(unlist(lapply(1:length(self$rep_data),function(x){sapply(1:(length(self$rep_data[[x]])-1), function(i){self$rep_data[[x]]$data[i+1,1]-self$rep_data[[x]]$data[i,1]})}))),4)
                            
                          },
                          
                          greet = function() {
                            cat("######\n")
                            cat("Hello my name is ", self$rep_name, "\n")
                            cat("I'm an elementR", self$rep_type ,"repertoire object\n")  
                            cat("My folder is ", self$rep_folderPath, "\n")      
                            cat("My files are ", self$rep_Files, "\n")
                            #cat("My flag is ", self$rep_flag, "\n")
                            cat("######\n")
                          }#greet
                          
                        )
)

elementR_repCalib <- R6Class("elementR_repCalib",
                             inherit = elementR_rep,
                             public = list(
                               rep_dataFinaleMean = NA,
                               rep_dataFinaleSD = NA,
                               rep_type = "Calibration",
                               
                               setrep_dataFinale = function(){
                                                                  
                                 listTemp <- list()
                                 
                                 #listTemp2 <- lapply(1:length(self$rep_flag), function(x){self$rep_data[[x]]$data_calibFinal}) ## François
                                 for(i in 1:length(self$rep_Files)){listTemp[[i]] <- self$rep_data[[i]]$data_calibFinalMean}
                                                                  
                                 dataTemp <- do.call(rbind,listTemp)
                                 
                                 self$rep_dataFinaleMean <- apply(dataTemp,2, mean, na.rm = T)
                                 self$rep_dataFinaleSD <- apply(dataTemp,2, sd, na.rm = T)
                                 
                               }, # setrep_dataFinale
                               
                               setRep_table = function(nelem) {
                                 
                                 tab = matrix(0, length(self$rep_Files)*2+2, length(nelem))
                                 
                                 colnames(tab) <- nelem
                                 
                                 rownames(tab) <- c(paste(self$rep_Files, "Mean"),paste(self$rep_Files, "SD"),"Total Mean", "Total SD")
                                 
                                 for(i in 1:length(self$rep_Files)){tab[i,] <- self$rep_data[[i]]$data_calibFinalMean } # François
                                 
                                 for(i in 1:length(self$rep_Files)){tab[i+length(self$rep_Files),] <- self$rep_data[[i]]$data_calibFinalSD }
                                 
                                 tab[2*length(self$rep_Files)+1,] <-self$rep_dataFinaleMean
                                 
                                 tab[2*length(self$rep_Files)+2,] <-self$rep_dataFinaleSD
                                 
                                 return(tab)
                                 
                                 
                                 
                               }, # setRep_table
                               
                               create = function(){
                                 
                                 temp <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_calibration$new(f)})
                                 
                                 names(temp) <- self$rep_Files
                                 
                                 self$rep_data <- temp
                                 
                               }
                             ) # list
) # elementR_repCalib

elementR_repSample <- R6Class("elementR_repSample",
                             inherit = elementR_rep,
                             public = list(
                               rep_type = "Sample",
                               rep_type2 = NA,
                               rep_dataFiltre = NA, # liste de dataPlateauMoinsBlancSupLODNormSansAnom de chaque replicat
                               rep_deplacement = NA, # vecteur delta d'aligneent
                               rep_dataIntermSpot = NA, # chaque replicat moyenné pour les spots
                               rep_dataIntermRaster = NA, # chaque replicat avec son delta d'alignement                               
                               rep_dataIntermSpotBis = NA, # tableau avec la moyenne des moyennes
                               rep_dataFinaleCorrel = NA, # moyenne des courbes                               
                               rep_dataNonCorrel = NA, # moyenne sans points adjacents   
                               rep_coord = NA, # coordonnée point choisi
                               rep_flagSpot = c(0,0), # temoin du réalignement spot
                               rep_flagRaster =  c(0,0,0,0,0), # temoin du réalignement raster
                               rep_vitesse = NA, #vitesse raster
                               rep_diam = NA, # diamètre raster
                               
                               setrep_type2 = function(x){
                                 self$rep_type2 <- x
                               },                          
                             
                               create = function(){
                                 
                                 temp <- self$rep_data <- lapply(paste0(self$rep_folderPath, "/", self$rep_Files),function(f){elementR_sample$new(f)})
                                 
                                 names(temp) <- self$rep_Files
                                 
                                 self$rep_data <- temp
                                 
                               },
                               
                               Realign1 = function(liste, pas){
                                 
                                 for (i in 2:length(liste)){
                                   A <- liste[[1]]
                                   B <- liste[[i]]
                                   
                                   if(round(A[1,1])<round(B[1,1])){while(round(B[1,1]) > round(A[1,1])){B = rbind(c(B[1,1]-pas,rep(NA,dim(A)[2]-1)),B)}}#
                                   if(round(A[1,1])>round(B[1,1])){while(round(A[1,1]) > round(B[1,1])){A = rbind(c(A[1,1]-pas,rep(NA,dim(A)[2]-1)),A)}}
                                   
                                   if(round(A[dim(A)[1],1])<round(B[dim(B)[1],1])){while(round(B[dim(B)[1],1]) > round(A[dim(A)[1],1])){A = rbind(A,c(A[dim(A)[1],1] + pas,rep(NA,dim(A)[2]-1)))}} 
                                   if(round(A[dim(A)[1],1])>round(B[dim(B)[1],1])){while(round(A[dim(A)[1],1]) > round(B[dim(B)[1],1])){B = rbind(B,c(B[dim(B)[1],1] + pas,rep(NA,dim(A)[2]-1)))}}#
                                   
                                   liste[[1]] <- A
                                   liste[[i]] <- B
                                   
                                 }  
                                 return(liste)
                               },
                               
                               Realign2 = function(liste, pas){
                                 
                                 min = min(do.call(rbind,liste)[,1]) ; minPlace = NA; minPlace = which(sapply(1:length(liste), function(x){
                                   if(length(which(liste[[x]][,1] == min)) == 1) {T}
                                   else {F}
                                 }) == T)
                                 
                                 if(length(minPlace) != 1){minPlace = minPlace[1]}
                                 
                                 max = max(do.call(rbind,liste)[,1]) ; maxPlace = NA; maxPlace = which(sapply(1:length(liste), function(x){
                                   if(length(which(liste[[x]][,1] == max)) == 1) {T}
                                   else {F}
                                 }) == T)
                                 
                                 if(length(maxPlace) != 1){maxPlace = maxPlace[length(maxPlace)]}
                                 
                                 A <- liste[[minPlace]]
                                 
                                 B <- liste[[maxPlace]]
                                 
                                 if(round(A[1,1]) < round(B[1,1])){while(round(B[1,1]) > round(A[1,1])){B = rbind(c(B[1,1]-pas,rep(NA,dim(A)[2]-1)),B)}}#
                                 if(round(A[1,1]) > round(B[1,1])){while(round(A[1,1]) > round(B[1,1])){A = rbind(c(A[1,1]-pas,rep(NA,dim(A)[2]-1)),A)}}
                                 
                                 if(round(A[dim(A)[1],1]) < round(B[dim(B)[1],1])){while(round(B[dim(B)[1],1]) > round(A[dim(A)[1],1])){A = rbind(A,c(A[dim(A)[1],1] + pas,rep(NA,dim(A)[2]-1)))}} 
                                 if(round(A[dim(A)[1],1]) > round(B[dim(B)[1],1])){while(round(A[dim(A)[1],1]) > round(B[dim(B)[1],1])){B = rbind(B,c(B[dim(B)[1],1] + pas,rep(NA,dim(A)[2]-1)))}}#
                                 
                                 liste[[minPlace]] <- A
                                 liste[[maxPlace]] <- B
                                                                                               
                                 self$Realign1(liste, pas)                                 
                               },
                               
                               setRep_dataFiltre = function(){
                                 
                                 self$rep_dataFiltre <- lapply(1:length(self$rep_Files),function(x){self$rep_data[[x]]$dataPlateauMoinsBlancSupLODNormSansAnomConc})
                                                                  
                               },
                               
                               setrep_deplacement = function(x){
                                 
                                 self$rep_deplacement <- x
                                 
                               },
                               
                               setRep_dataInterm = function(type, decalage  = rep(0,length(self$rep_Files)), temoin, data){
                                                                                                   
                                 if(!is.na(self$rep_dataFiltre) & type == "spot"){
                                   
                                   self$rep_dataIntermSpot <- t(as.matrix(sapply(1:length(self$rep_Files), function(x){apply(self$rep_dataFiltre[[x]][,-1],2, mean,na.rm = T)})))
                                   
                                 }
                                 
                                 if(!is.na(self$rep_dataFiltre) & type == "raster"){
                                   
                                   if(temoin == 1){
                                     
                                     self$setRep_pas()
                                     
                                     self$rep_dataIntermRaster <-lapply(1:length(self$rep_Files), function(x){
                                       
                                       temp <- self$rep_dataFiltre[[x]]
                                       
                                       temp[,1] <- temp[,1] + decalage[x] * self$rep_pas
                                       
                                       return(temp)                                   
                                       
                                     })
                                     
                                   }
                                   
                                   if(temoin == 2){
                                     
                                     self$rep_dataIntermRaster <-lapply(1:length(self$rep_Files), function(x){
                                       
                                       temp <- data[[x]]
                                       
                                       temp[,1] <- temp[,1] + self$rep_deplacement[x] + decalage[x] * self$rep_pas
                                       
                                       return(temp)                                   
                                       
                                     })
                                     
                                   }
                                     

                                 }
                               },
                               
                               setRep_coord = function(z){
                                 
                                 temp <- self$lePlusProche(self$rep_dataFinaleCorrel[,1], z$x)
                                 
                                 self$rep_coord <- self$rep_dataFinaleCorrel[temp$place, ]
                                 
                               },                               
                               
                               setrep_dataFinale = function(type){
                                 
                                 if(!is.na(self$rep_dataFiltre) & type == "spot"){
                                   
                                   self$rep_dataIntermSpotBis <- rbind(apply(do.call(rbind,self$rep_dataFiltre)[,-1], 2, mean, na.rm = T), apply(do.call(rbind,self$rep_dataFiltre)[,-1], 2, sd, na.rm = T))
                                   
                                 }  
                                 
                                 if(type == "raster"){                          
                                   
                                   MatTemp <- self$Realign2(self$rep_dataIntermRaster, pas = self$rep_pas)
                                                                                                        
                                   MatTemp <- abind(MatTemp,along=3)                                   
                                                                 
                                   MatTemp <- apply(MatTemp,c(1,2),mean,na.rm=TRUE)
                                   
                                   colnames(MatTemp) <- colnames(self$rep_data[[1]]$data)
                                   
                                   self$rep_dataFinaleCorrel <- MatTemp
                                   
                                 }
                                 
                               },
                               
                               set_repParam = function(x,y){
                                 
                                 self$rep_vitesse <- x
                                 
                                 self$rep_diam <- y
                               },
                               
                               setrep_flag = function(type, position,valeur){
                                 
                                 if(type == "spot"){
                                   
                                   self$rep_flagSpot[position] <- valeur
                                   
                                 }
                                 
                                 if(type == "raster"){ 
                                 
                                  self$rep_flagRaster[position] <-  valeur
                                 
                                 }
                                   
                               },
                               
                               lePlusProche = function(x,y){
                                 temp = list()
                                 
                                 temp[[2]] = which(abs(x-y) == min(abs(x-y), na.rm = T))
                                 temp[[1]] = x[temp[[2]]]
                                 
                                 if (length(temp[[1]])!=1){temp[[2]] = min(temp[[2]], na.rm = T)
                                                           temp[[1]] = x[temp[[2]]]
                                 }
                                 
                                 names(temp) <- c("le_plus_proche", "place")
                                 
                                 return(temp)
                               },
                               
                               reverseMatrix = function(matrice){
                                 Dim <- dim(matrice)[2]
                                 
                                 temp <- sapply(1:Dim, function(x){
                                   rev(matrice[,x])
                                 })
                                 colnames(temp) <- colnames(matrice)
                                 return(temp)
                               },
                               
                               setrep_dataNonCorrel = function(enlever, point){
                                                                  
                                 tempApres <- self$rep_dataFinaleCorrel[self$lePlusProche(self$rep_dataFinaleCorrel[,1], point[[1]])[[2]]:dim(self$rep_dataFinaleCorrel)[1],]
                                 
                                 if(is.null(dim(tempApres)[1])){
                                   tempApres <- t(as.matrix(tempApres))                                   
                                 }   
                                 
                                 tempAvant <- self$reverseMatrix(self$rep_dataFinaleCorrel[1:self$lePlusProche(self$rep_dataFinaleCorrel[,1], point[[1]])[[2]],])
                                                                                                                                   
                                 if(dim(tempApres)[1] %% enlever == 0){
                                   nAv <- dim(tempApres)[1]/enlever
                                 }
                                 if(dim(tempApres)[1] %% enlever != 0){
                                   nAv <- floor(dim(tempApres)[1]/enlever) + 1 
                                 } 
                                 
                                 tempApresB <- t(sapply(0:(nAv-1), function(x){tempApres[x*enlever+1,]}))
                                 
                                 if(dim(tempAvant)[1] %% enlever == 0){
                                   nAp <- dim(tempAvant)[1]/enlever                             
                                  }
                                 if(dim(tempAvant)[1] %% enlever != 0){
                                   nAp <- floor(dim(tempAvant)[1]/enlever) + 1 
                                 }
                                 
                                 if(nAp < 2){tempAvantB <- tempAvant[1,]}
                                 if(nAp >=2){tempAvantB <- self$reverseMatrix(t(sapply(0:(nAp-1), function(x){tempAvant[x*enlever+1,]})))}                                                               
                                                                  
                                 self$rep_dataNonCorrel <- rbind(tempAvantB, tempApresB[-1,])
 
                               },
                               
                               initial = function(x){
                                 
                                 eval(parse(text = paste0("self$",x,"<- NA")))
                                            
                               }
                               
                             ) # list
) # elementR_repCalib