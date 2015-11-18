######################################################################################
# COSvis 0.2 
# authors: Klotz, Wesemann, Herrnegger; 10/2015
# mantainer: Klotz Daniel, daniel.klotz@boku.ac.at
# info: COSvis_Notes.pdf
l
######################################################################################
# Set Controls (explanations are given in: COSvis_Notes.pdf)
######################################################################################
 rm(list=ls())  # removes all the variables in the enviorments 
 gc(verbose = FALSE)
 ctrl <- list()  # variable pre-set for the conntrols
#************************************************************************************
# Paths:
 ctrl$pathtoCosero <- "C:/Users/H0740147/Cosero_Mur/COSERO/MitExcel" 
 ctrl$pathtoApp <- "C:/Users/H0740147/Cosero_Mur/COSERO/Auswertung/COSvis_DJ/COSvis/App" 
#  ctrl$pathtoCosero <- "/Users/ido87/Dropbox/Arbeit-Anderes/scripts_evalCOSwithR" 
#  ctrl$pathtoApp <- "/Users/ido87/Documents/COSvis/App" 
# folder names:
  ctrl$ofoldername <- "output_60min"
# Interactive Overview: 
  ctrl$ctrl_span  	<- c(2009,2012) 
# OF plot options:
  # naming:
  ctrl$yearName   	<- "Jahr" 
  # color-settings:
  ctrl$colors      		<- c('#FF3300','#f6f3a1','#005900',"purple4") 
  ctrl$clr_NSEmid  	<- 0.5 
  ctrl$OFsize       <- 5.5

######################################################################################
# run COSvis
######################################################################################
setwd(ctrl$pathtoCosero ) 
source(paste(ctrl$pathtoApp,"/calculations.R",sep="")) # executes calculation file

require(dygraphs)
runApp(ctrl$pathtoApp) # executes shinyApp


