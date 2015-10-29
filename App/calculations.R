require("shiny")
require("hydroGOF")
require("ggplot2")
require("xts")
require("dplyr")
require("grid")
require("gridExtra")
oldw <- getOption("warn")
######################################################################################
# read in 
######################################################################################
# load runoff files
d_raw <- read.table(paste( ctrl$ofoldername, "/output.runoff", sep=""), header = TRUE, skip = 22)
#
colmax <- function(x) lapply(X = d_raw,FUN = max) # 
idx_temp <- which(colmax(d_raw) == -999)
idx_slct <- sort(c(idx_temp,idx_temp+1,idx_temp+2))
d_runoff <- d_raw[-idx_slct]
# get amount of used basins and their respective names
temp_names <- names(d_runoff)
temp_names <- gsub('.*_', '' ,temp_names) %>% unique(.) # replaces everything before "_" and gets the unique names
eval_size <- length(temp_names)-5 
d_nums <- temp_names[6:(5+eval_size)] %>% as.integer(.)
d_raw_names <- names(d_raw)[6:length(d_raw)]
# remove spinup-time
tmp <- readLines( paste(  ctrl$ofoldername, "/Statistics.txt", sep="") )
lngth_spinup <- grep("start time-step of evaluation",tmp) %>% tmp[.] %>% sub('.*:', '',.) %>% as.integer(.)
lngth_sim <- dim(d_runoff)[1] 
d_runoff <- slice(d_runoff,lngth_spinup:lngth_sim)
#add a nice formated date to the variables
rdate  <- paste(d_runoff$yyyy,
                sprintf("%02d",d_runoff$mm),
                sprintf("%02d",d_runoff$dd),
                sprintf("%02d",d_runoff$hh),
                sprintf("%02d",d_runoff$mm.1),
                sep= "" ) %>% 
  as.POSIXct(format = "%Y%m%d%H%M",tz="UTC")
d_runoff$rdate <- rdate
#
lngth_sim <- dim(d_runoff)[1] 
years_in_data <- unique(d_runoff$yyyy)
years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
num_years = length(years_in_data)
d_runoff$hydyyyy <- as.character(rdate)
# convert d_runoff to time series (i.e. "xts")
d_xts <- select(d_runoff,-starts_with("QOSI_")) %>%
  filter(.,yyyy >= ctrl$ctrl_span[1],yyyy <= ctrl$ctrl_span[2])
#********************************
# calculate hydrological years:
#********************************
num_hydyears <- length(years_in_data_shrt)
# cut months before first year & after last year : 
if (d_runoff$mm[1] < 9) num_hydyears <- num_hydyears - 1
if (d_runoff$mm[lngth_sim] >= 9) num_hydyears <- num_hydyears - 1
hydyears_in_d <- years_in_data_shrt[1:num_hydyears]
# get hydrological years
cnt <- 0
for (i in 1:(num_hydyears)) 
{
  hydyears_in_d[i] <- paste(years_in_data_shrt[i],years_in_data_shrt[i+1], sep = "/")
  tmp_d_YearX <- filter(d_runoff, yyyy == years_in_data[i] | yyyy == years_in_data[i+1])  %>% 
    filter(., (yyyy == years_in_data[i] & mm >= 9 ) | (yyyy == years_in_data[i+1] & mm < 9 ) ) %>%
    select(.,hydyyyy) %>%
    transform(.,hydyyyy = hydyears_in_d[i])
  tmp_lngth <- dim(tmp_d_YearX)[1]
  d_runoff$hydyyyy[(cnt+1):(cnt+tmp_lngth)] <- tmp_d_YearX$hydyyyy
  cnt = cnt + tmp_lngth
}
rm(tmp,idx_temp,tmp_lngth,tmp_d_YearX)


######################################################################################
# total & yearly (hydyears) evalution of the NSE, KGE,  percentage Bias & Correlation
######################################################################################
# calculations:
NSE_hydyearly <- matrix(nrow = num_hydyears, ncol = as.integer(eval_size), data = NA)
KGE_hydyearly <- NSE_hydyearly 
pBias_hydyearly <- NSE_hydyearly
cor_hydyearly <- NSE_hydyearly
for (k in 1:num_hydyears) 
{
  tempOBS <- filter(d_runoff,hydyyyy == hydyears_in_d[k]) %>% select(.,starts_with("QOBS_"))
  tempSIM <- filter(d_runoff,hydyyyy == hydyears_in_d[k]) %>% select(.,starts_with("QSIM_"))
  NSE_hydyearly[k,1:eval_size] <- hydroGOF::NSE(tempSIM,tempOBS)
  KGE_hydyearly[k,1:eval_size] <- hydroGOF::KGE(tempSIM,tempOBS)
  pBias_hydyearly[k,1:eval_size] <-  hydroGOF::pbias(tempSIM,tempOBS)
  cor_hydyearly[k,1:eval_size] <- cor(tempSIM,tempOBS) %>% diag(.)
}
tempOBS <- select(d_runoff,starts_with("QOBS_"))
tempSIM <- select(d_runoff,starts_with("QSIM_"))
NSE_total <- hydroGOF::NSE(tempSIM,tempOBS)
KGE_total <- hydroGOF::KGE(tempSIM,tempOBS)
pBIAS_total <- hydroGOF::pbias(tempSIM,tempOBS)
cor_total <- cor(tempSIM,tempOBS) %>% diag(.)
# some cleaning
rm(tempOBS,tempSIM)
# write out .txt files (still curde !! )
write.table(cbind(d_nums,t(NSE_hydyearly), NSE_total), 
            file = paste(substr(ctrl$pathtoApp, 1, nchar(ctrl$pathtoApp)-3), "NSE_Hydyear.csv", sep=""),
            row.names = FALSE, col.names = c("#",paste("HY",hydyears_in_d),"TOTAL"), quote = FALSE, sep = ";")
#
# define plot function for hyd-yearly plots:
plt_yOF <- function(OF_hydyearly,hydyears_in_d,eval_size,plt_ctrl) {
  require("reshape2")
  of_y <- expand.grid(hydyears = hydyears_in_d, numberBasins = 1:eval_size) 
  temp <- OF_hydyearly; 
  temp[temp < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut
  temp <- melt(temp)[3]
  of_y$OFvalue = temp$value
  #
  plt_out <- ggplot(of_y, aes(hydyears,numberBasins, fill = OFvalue),environmnet = environment()) + 
    ggtitle(plt_ctrl$gtitle) + 
    geom_raster(position = "identity") + 
    ylab(plt_ctrl$ylab) + 
    xlab(plt_ctrl$xlab) + 
    scale_y_reverse(breaks = 1:eval_size, labels = d_nums) + 
    scale_x_discrete( breaks = hydyears_in_d) +
    scale_fill_gradient2(space = "Lab", 
                         low = plt_ctrl$clr1 , mid= plt_ctrl$clr2 , high = plt_ctrl$clr3 ,
                         midpoint = plt_ctrl$midpoint, 
                         limits= plt_ctrl$limits ,
                         na.value = plt_ctrl$clr4) +
    theme_bw(base_size = 15) +
    theme( legend.position="none" )  + 
    geom_tile(color = "white", size = 0.25 ) + 
    geom_text(aes(hydyears,numberBasins, label = round(OFvalue,2)), size = ctrl$OFsize , color= "black")
  return(plt_out)
}
  






######################################################################################
# pre calculation  objective functions (of)
######################################################################################

#********************************
# Yearly NSE
#********************************
plt_ctrl <- list() # reset list 
plt_ctrl$gtitle <- "Yearly NSE"
plt_ctrl$ylab <- "basin number"
plt_ctrl$xlab <- ctrl$yearName
plt_ctrl$clr1 <- ctrl$colors[1]
plt_ctrl$clr2 <- ctrl$colors[2]
plt_ctrl$clr3 <- ctrl$colors[3]
plt_ctrl$clr4 <- ctrl$colors[4]
plt_ctrl$midpoint <- ctrl$clr_NSEmid
plt_ctrl$limits <- c(0,1)
plt_ctrl$lb_cut <- 0.0
#
plt_ynse <- plt_yOF(NSE_hydyearly,hydyears_in_d,eval_size,plt_ctrl)

#********************************
# Total NSE
#********************************
of_tnse <- expand.grid(total = 1, numberBasins = 1:eval_size) 
temp <- NSE_total; 
temp[temp < -0.0] <- -0.0
temp <- melt(temp)
of_tnse$NSEvalue = temp$value
#
options(warn = -1)
plt_tnse <- ggplot(of_tnse , aes(total,numberBasins, fill = NSEvalue)) + geom_raster(position = "identity") +
  ggtitle("Total NSE") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = grid::unit(c(0.5,2,0.8,-0.5), "cm") ) +
  geom_tile(color="white", size = 0.25) + 
  geom_text(aes( total, numberBasins ,label = round(NSEvalue,2) ), size = ctrl$OFsize ,color="black") +
  scale_y_reverse() +
  scale_fill_gradient2(low = ctrl$colors[1],
                       mid= ctrl$colors[2], 
                       high = ctrl$colors[3],
                       midpoint = ctrl$clr_NSEmid, 
                       limits = c(0,1),
                       na.value = ctrl$colors[3])
options(warn = oldw)
######################################################################################
#********************************
# Plot: Yearly %Bias
#********************************
plt_ctrl <- list() # reset list 
plt_ctrl$gtitle <- "Yearly %-Bias"
plt_ctrl$ylab <- "basin number"
plt_ctrl$xlab <- ctrl$yearName
plt_ctrl$clr1 <- ctrl$colors[1]
plt_ctrl$clr2 <- ctrl$colors[3]
plt_ctrl$clr3 <- ctrl$colors[1]
plt_ctrl$clr4 <- ctrl$colors[1]
plt_ctrl$midpoint <- ctrl$clr_NSEmid
plt_ctrl$limits <- c(-100,100)
plt_ctrl$lb_cut <- -1000.0
#
plt_ypbias <- plt_yOF(pBias_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
#********************************
# Plot: Total %Bias
#********************************
of_tpbias <- expand.grid(total = 1, numberBasins = 1:eval_size) 
temp <- melt(pBIAS_total)
of_tpbias$biasValue = temp$value
#
options(warn = -1)
plt_tpbias <- ggplot(of_tpbias , aes(total,numberBasins, fill = biasValue)) + geom_raster(position = "identity") +
  ggtitle("Total %-Bias") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 12),
        axis.ticks= element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = grid::unit(c(0.5,2,0.8,-0.5), "cm") ) +
  geom_tile(color = "white", size = 0.25) + 
  geom_text(aes( total,numberBasins,label= round(biasValue,2) ), size = ctrl$OFsize ,color = "black") +
  scale_y_reverse() +
  scale_fill_gradient2(low = ctrl$colors[4],
                       mid= ctrl$colors[3],  
                       high = ctrl$colors[1],
                       midpoint = 0, 
                       limits = c(-100,100) )
options(warn = oldw)
######################################################################################
#********************************
# Plot: Yearly KGE
#********************************
plt_ctrl <- list() # reset list 
plt_ctrl$gtitle <- "Yearly KGE"
plt_ctrl$ylab <- "basin number"
plt_ctrl$xlab <- ctrl$yearName
plt_ctrl$clr1 <- ctrl$colors[1]
plt_ctrl$clr2 <- ctrl$colors[2]
plt_ctrl$clr3 <- ctrl$colors[3]
plt_ctrl$clr4 <- ctrl$colors[4]
plt_ctrl$midpoint <- ctrl$clr_NSEmid
plt_ctrl$limits <- c(0,1)
plt_ctrl$lb_cut <- 0.0
#
plt_ykge <- plt_yOF(KGE_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
#********************************
# Plot: Total KGE
#********************************
of_tkge <- expand.grid(total = 1, numberBasins = 1:eval_size) 
temp <- KGE_total; 
temp[temp < -0.0] <- -0.0
temp <- melt(temp)
of_tkge$KGEvalue = temp$value
#
options(warn = -1)
plt_tkge <- ggplot(of_tkge , aes(total,numberBasins, fill = KGEvalue)) + geom_raster(position = "identity") +
  ggtitle("Total KGE") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 12),
        axis.ticks= element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = grid::unit(c(0.5,2,0.8,-0.5), "cm") ) +
  geom_tile(color = "white", size = 0.25) + 
  geom_text(aes( total,numberBasins,label = round(KGEvalue,2) ), size = ctrl$OFsize ,color = "black") +
  scale_y_reverse() +
  scale_fill_gradient2(low = ctrl$colors[1],
                       mid= ctrl$colors[2], 
                       high = ctrl$colors[3],
                       midpoint = ctrl$clr_NSEmid, 
                       limits = c(0,1) )
options(warn = oldw)
######################################################################################
#********************************
# Plot: Yearly Correlation
#********************************
plt_ctrl <- list() # reset list 
plt_ctrl$gtitle <- "Yearly Correlation"
plt_ctrl$ylab <- "basin number"
plt_ctrl$xlab <- ctrl$yearName
plt_ctrl$clr1 <- ctrl$colors[1]
plt_ctrl$clr2 <- ctrl$colors[2]
plt_ctrl$clr3 <- ctrl$colors[3]
plt_ctrl$clr4 <- ctrl$colors[4]
plt_ctrl$midpoint <- ctrl$clr_NSEmid
plt_ctrl$limits <- c(0,1)
plt_ctrl$lb_cut <- -1.0
#
plt_ycor <- plt_yOF(KGE_hydyearly,hydyears_in_d,eval_size,plt_ctrl)

#********************************
# Plot: Total Correlation
#********************************
of_tcor <- expand.grid(total = 1, numberBasins = 1:eval_size) 
temp <- cor_total; 
temp <- melt(temp)
of_tcor$Correlation = temp$value
#
options(warn = -1)
plt_tcor <- ggplot(of_tcor, aes(total,numberBasins, fill = Correlation)) + geom_raster(position = "identity") +
  ggtitle("Total Correlation") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = grid::unit(c(0.5,2,0.8,-0.5), "cm") ) +
  geom_tile(color = "white", size = 0.25) + 
  geom_text(aes( total,numberBasins,label = round(Correlation,2) ), size = ctrl$OFsize ,color = "black") +
  scale_y_reverse() +
  scale_fill_gradient2(low = ctrl$colors[1],
                       mid= ctrl$colors[2], 
                       high = ctrl$colors[3],
                       midpoint = 0.6, 
                       limits = c(0,1) )
options(warn = oldw)

######################################################################################
# Expanded Plots
######################################################################################
#********************************
# NSE
#********************************
# 1. NSE calculations
temp <- NSE_hydyearly;
temp[temp < 0.0] <- 0.0
d_NSEyearly <- as.data.frame(temp)
newNames <- paste("basin",d_nums,sep="") #old: names(d_NSEyearly) %>% gsub("V","basin",.)
names(d_NSEyearly)  <- newNames
years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
d_NSEyearly$hydyear <- hydyears_in_d
# 2. NSE plots
plt_exp_NSE <- list()
plt_exp_NSE <- lapply(1:eval_size, 
                      function(k) ggplot(data = d_NSEyearly) +
                        geom_bar(stat="identity",
                                 position = "identity",
                                 aes_string(x="hydyear", y=newNames[k],fill=newNames[k])) +
                        theme_bw(base_size = 15) +
                        ggtitle(newNames[k]) +
                        xlab(ctrl$yearName) +
                        ylab("nse") +
                        scale_y_continuous( limits = c(0,1) ) + 
                        theme(legend.position= "none", 
                              axis.text.x = element_text(angle = 50, hjust = 1), 
                              plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) +
                        scale_fill_gradient2(low = ctrl$colors[1],
                                             mid = ctrl$colors[2] , 
                                             high = ctrl$colors[3], 
                                             midpoint = ctrl$clr_NSEmid, 
                                             limits = c(0,1) )
)
# 3. htmlFile KGE (cause shiny does not like multiple graphics)
# some pre sets for calculations:
num_plots <- ceiling(eval_size/9)
g <- seq(from = 1, to = (eval_size-9) , by=9)
max_plots <- (length(g)+1)
NSE_sepplots <- list()
fileConn <- file(paste(ctrl$pathtoApp,"/www/expnd_nse.html",sep=""),"w")
writeLines(text = '<!DOCTYPE html>',fileConn)
writeLines(text = '<html>',fileConn)
writeLines(text = '<body>',fileConn)
# save everything
for ( i in 1:(max_plots-1) )
{
  j = g[i]
  plt_name <- paste("expnd_nse",i,".jpg", sep = "")
  plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep = "")
  plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
  writeLines(text = plt_hmtlInfos,fileConn )
  #
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(plt_exp_NSE[j:(j+8)],list(ncol = 3, nrow = 3) ))
  dev.off()
  
}
plt_name <- paste("expnd_nse",i+1,".jpg", sep="")
plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep="")
plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
#
writeLines( plt_hmtlInfos,fileConn)
close(fileConn)
jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
do.call("grid.arrange",c(plt_exp_NSE[(j+9):eval_size],list(ncol = 3, nrow = 3) )) 
dev.off()
#********************************
# KGE
#********************************
# 1. KGE calculations
  temp <- KGE_hydyearly; temp[temp < 0.0] <- 0.0
  d_KGEyearly <- as.data.frame(temp)
  newNames <- paste("basin",d_nums,sep = "")
  names(d_KGEyearly)  <- newNames
  years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
  d_KGEyearly$hydyear <- hydyears_in_d
# 2. KGE plots
  options(warn = -1)
  plt_exp_KGE <- list()
  plt_exp_KGE <- lapply(1:eval_size, 
                        function(k) ggplot(data = d_KGEyearly) +
                          geom_bar(stat= "identity",
                                   position = "identity",
                                   aes_string(x="hydyear", y=newNames[k],fill=newNames[k])) +
                          theme_bw(base_size = 15) +
                          ggtitle(newNames[k]) +
                          xlab(ctrl$yearName) +
                          ylab("kge") +
                          scale_y_continuous( limits = c(0,1) ) + 
                          theme(legend.position= "none", 
                                axis.text.x = element_text(angle = 50, hjust = 1), 
                                plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) +
                          scale_fill_gradient2(low = ctrl$colors[1],
                                               mid = ctrl$colors[2] , 
                                               high = ctrl$colors[3], 
                                               midpoint = ctrl$clr_NSEmid, 
                                               limits = c(0,1) )
  )
  options(warn = oldw)
# 3. htmlFile KGE (cause shiny does not like multiple graphics)
  # some pre sets for calculations:
  num_plots <- ceiling(eval_size/9)
  g <- seq(from = 1, to = (eval_size-9) , by=9)
  max_plots <- (length(g)+1)
  KGE_sepplots <- list()
  fileConn <- file(paste(ctrl$pathtoApp,"/www/expnd_kge.html",sep=""),"w")
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # save everything
  for ( i in 1:(max_plots-1) )
  {
    j = g[i]
    plt_name <- paste("expnd_kge",i,".jpg", sep = "")
    plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep = "")
    plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
    writeLines(text = plt_hmtlInfos,fileConn )
    #
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
    do.call("grid.arrange",c(plt_exp_KGE[j:(j+8)],list(ncol = 3, nrow = 3) ))
    dev.off()
    
  }
  plt_name <- paste("expnd_kge",i+1,".jpg", sep="")
  plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep="")
  plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
  #
  writeLines( plt_hmtlInfos,fileConn)
  close(fileConn)
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(plt_exp_KGE[(j+9):eval_size],list(ncol = 3, nrow = 3) )) 
  dev.off()
#********************************
# pBias
#********************************
# 1. pBias calculations
  temp <- pBias_hydyearly; 
  d_pBiasyearly <- as.data.frame(temp)
  newNames <- paste("basin",d_nums,sep = "")
  names(d_pBiasyearly)  <- newNames
  years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
  d_pBiasyearly$hydyear <- hydyears_in_d
# 2. pBias plots
  options(warn = -1)
  plt_exp_pBias <- list()
  plt_exp_pBias <- lapply(1:eval_size, 
                          function(k) ggplot(data = d_pBiasyearly) +
                            geom_bar(stat= "identity",
                                     position = "identity",
                                     aes_string(x="hydyear", y=newNames[k],fill=newNames[k])) +
                            theme_bw(base_size = 15) +
                            ggtitle(newNames[k]) +
                            xlab(ctrl$yearName) +
                            ylab("%-Bias") +
                            scale_y_continuous( limits = c(-200,200) ) + 
                            theme(legend.position= "none", 
                                  axis.text.x = element_text(angle = 50, hjust = 1), 
                                  plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) +
                            scale_fill_gradient2(low = ctrl$colors[4],
                                                 mid = ctrl$colors[3],  
                                                 high = ctrl$colors[1],
                                                 midpoint = 0, 
                                                 limits = c(-200,200) )
  )
  options(warn = oldw)
# 3. htmlFile pBias (cause shiny does not like multiple graphics)
  # some pre sets for calculations:
  num_plots <- ceiling(eval_size/9)
  g <- seq(from = 1, to = (eval_size-9) , by=9)
  max_plots <- (length(g)+1)
  pBias_sepplots <- list()
  fileConn <- file(paste(ctrl$pathtoApp,"/www/expnd_pbias.html",sep=""),"w")
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # save everything
  for ( i in 1:(max_plots-1) )
  {
    j = g[i]
    plt_name <- paste("expnd_pbias",i,".jpg", sep = "")
    plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep = "")
    plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
    writeLines(text = plt_hmtlInfos,fileConn )
    #
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
    do.call("grid.arrange",c(plt_exp_pBias[j:(j+8)],list(ncol = 3, nrow = 3) ))
    dev.off()
    
  }
  plt_name <- paste("expnd_pbias",i+1,".jpg", sep="")
  plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep="")
  plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
  #
  writeLines( plt_hmtlInfos,fileConn)
  close(fileConn)
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(plt_exp_pBias[(j+9):eval_size],list(ncol = 3, nrow = 3) )) 
  dev.off()
#********************************
# CORR
#********************************
# 1. cor calculations
  temp <- cor_hydyearly
  d_coryearly <- as.data.frame(temp)
  newNames <- paste("basin",d_nums,sep = "")
  names(d_coryearly)  <- newNames
  years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
  d_coryearly$hydyear <- hydyears_in_d
# 2. cor plots
  options(warn = -1)
  plt_exp_cor <- list()
  plt_exp_cor <- lapply(1:eval_size, 
                        function(k) ggplot(data = d_coryearly) +
                          geom_bar(stat= "identity",
                                   position = "identity",
                                   aes_string(x="hydyear", y=newNames[k],fill=newNames[k])) +
                          theme_bw(base_size = 15) +
                          ggtitle(newNames[k]) +
                          xlab(ctrl$yearName) +
                          ylab("correlation") +
                          scale_y_continuous( limits = c(0,1) ) + 
                          theme(legend.position= "none", 
                                axis.text.x = element_text(angle = 50, hjust = 1), 
                                plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) +
                          scale_fill_gradient2(low = ctrl$colors[1],
                                               mid = ctrl$colors[2] , 
                                               high = ctrl$colors[3], 
                                               midpoint = 0.7, 
                                               limits = c(0,1) )
  )
  options(warn = oldw)
# 3. htmlFile cor (cause shiny does not like multiple graphics)
  # some pre sets for calculations:
  num_plots <- ceiling(eval_size/9)
  g <- seq(from = 1, to = (eval_size-9) , by=9)
  max_plots <- (length(g)+1)
  cor_sepplots <- list()
  fileConn <- file(paste(ctrl$pathtoApp,"/www/expnd_cor.html",sep=""),"w")
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # save everything
  for ( i in 1:(max_plots-1) )
  {
    j = g[i]
    plt_name <- paste("expnd_cor",i,".jpg", sep = "")
    plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep = "")
    plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
    writeLines(text = plt_hmtlInfos,fileConn )
    #
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
    do.call("grid.arrange",c(plt_exp_cor[j:(j+8)],list(ncol = 3, nrow = 3) ))
    dev.off()
    
  }
  plt_name <- paste("expnd_cor",i+1,".jpg", sep="")
  plt_pathANDname <- paste(ctrl$pathtoApp,"/www/",plt_name,sep="")
  plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
  #
  writeLines( plt_hmtlInfos,fileConn)
  close(fileConn)
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(plt_exp_cor[(j+9):eval_size],list(ncol = 3, nrow = 3) )) 
  dev.off()