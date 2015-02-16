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
		dataIRTS = NA,
		fPath = NA,
		gg = NA,
		initialize = function(fPath=NULL) {
			if(is.null(fPath)) stop("error, fPath missing !")
			charStrings <- unlist(lapply(strsplit(fPath,"[.]"),strsplit,split="/"))
			self$name <- charStrings[length(charStrings)-1] 
			self$fPath <- fPath
			self$gg <- aaa(1)

			#####dealing with data
			d <- read.csv(fPath,sep=";",dec=",",header=TRUE)
			self$data <- d
			self$dataIRTS <- irts(d[,1], as.matrix(d[,-1]))
			self$greet()
		},#initialize
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
#	require(dygraphs)
#	d <- elementR_data$new("Data/example_1/calibrations/NIST_1.csv")
#	dygraph(d$dataIRTS)
#}#eo notRun

############################################################
############################################################
################################# elementR_calibration Class
############################################################
############################################################

elementR_calibration <- R6Class("elementR_calibration",
	inherit = elementR_data,
	public = list(
		type = "calibration",
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
)#elementR_data

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
)#elementR_data

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
		initialize = function(folderPath=NULL) {
			if(is.null(folderPath)) stop("\n #### A folder path is required to create an elementR project '[^_-]'")
			if(sum(c("calibrations","samples","standards.csv")%in%dir(folderPath))!=3) stop("\n #### A folder should contain two subfolder 'calibrations' and 'samples', as well as a file named standards.csv to create an elementR project '[^_-]'")
			self$folderPath <- folderPath
			charStrings <- unlist(strsplit(folderPath,"/"))
			self$name <- charStrings[length(charStrings)]
			#calibrations
			self$calibrationsPath <- paste0(folderPath,"/calibrations")
			calFiles <- dir(self$calibrationsPath)
			self$calibrationsFiles <- calFiles
			calList <- lapply(paste0(self$calibrationsPath,"/",calFiles),function(f){elementR_calibration$new(f)})
			names(calList) <- calFiles
			self$calibrations <- calList
			#samples
			self$samplesPath <- paste0(folderPath,"/samples")
			sampFiles <- dir(self$samplesPath)
			self$samplesFiles <- sampFiles
			sampList <- lapply(paste0(self$samplesPath,"/",sampFiles),function(f){elementR_sample$new(f)})
			names(sampList) <- sampFiles
			self$samples <- sampList
			self$greet()
		},#initialize
		greet = function() {
			cat("######\n")
			cat("Hello my name is ", self$name, "\n")
			cat("I'm an elementR project object\n")
			cat("My folder is ", self$folderPath, "\n")			
			cat("My calibrations folder is ", self$calibrationsPath, "\n")
			cat("My calibration files ara ", self$calibrationsFiles, "\n")
			cat("My samples folder is ", self$samplesPath, "\n")
			cat("My samples files are ", self$samplesFiles, "\n")
			cat("######\n")
		}#greet
	),#public
	private = list(
		aMethod = function() self$name
	)#private
)#elementR_project

#elementR_project$new("Data/example_1")


