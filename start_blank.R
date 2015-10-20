######################################################################################
# COSvis 0.1 
# authors: Klotz, Wesemann, Herrnegger; 10/2015
# mantainer: Klotz Daniel, daniel.klotz@boku.ac.at
# info: COSvis_Notes.pdf

######################################################################################
# Set Controls (explanations are given in: COSvis_Notes.pdf)
######################################################################################
rm(list=ls())  # removes all the variables in the enviorments 
ctrl <- list()  # variable pre-set for the conntrols
#************************************************************************************
# Paths:
  ctrl$pathtoCosero <- "path" 
  ctrl$pathtoApp <- "path/App" 
# folder names:
  ctrl$ofoldername <- "output_foldername"
# Interactive Overview: 
  ctrl$ctrl_span  	<- c(2003,2003) 
# OF plot options:
  # naming:
  ctrl$yearName   	<- "Jahr" 
  # color-settings:
  ctrl$colors      		<- c('#FF3300','#f6f3a1','#1dcc3a',"purple4") 
  ctrl$clr_NSEmid  	<- 0.7 

######################################################################################
# run COSvis
######################################################################################
setwd(ctrl$pathtoCosero ) 
source(paste(ctrl$pathtoApp,"/calculations.R",sep="")) # executes calculation file
require(dygraphs)
runApp(ctrl$pathtoApp) # executes shinyApp


