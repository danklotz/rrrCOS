
# temporary solution:: 
#************************************************************************************
rm(list=ls())  # removes all the variables in the enviorments 
ctrl <- list()  # variable pre-set for the conntrols
require(magrittr)

# Paths:
#  ctrl$pathtoCosero <- "C:/Users/H0740147/Cosero_Mur/COSERO/MitExcel" 
# ctrl$pathtoApp <- file.choose() %>% strsplit("/") %>% .[[1]]%>% .[1:(length(.)-1)] %>% paste(.,collapse = "/")
# folder names:
ctrl$ofoldername <- "test"
# Interactive Overview: 
ctrl$ctrl_span  	<- c(2009,2012) 
# OF plot options:
# naming:
ctrl$yearName   	<- "Jahr" 
# color-settings:
ctrl$colors      		<- c('#FF3300','#f6f3a1','#005900',"purple4") 
ctrl$clr_NSEmid  	<- 0.5 
ctrl$OFsize       <- 5.5
#************************************************************************************


source("files/calculations.R", local = FALSE) 