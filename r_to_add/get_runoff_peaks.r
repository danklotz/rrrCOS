
## script to get runoff peaks

# read data
 setwd("D:/Arbeit/COSERO/in_viscos")
# 
# file <- "QsimQobs_60min_Kalibrierung_MS1.txt"
 file <- "QsimQobs_60min_KalibrierungValidierung_MS1.txt"
 Data.read <- read.table(file, header=TRUE, sep="\t", dec=".", na.strings="-999", stringsAsFactors=FALSE)

# Data.read <- visCOS::get_runoff_example()
pracma::tic()
test <- visCOS::plotlist_runoffpeaks(Data.read,n_events = 5, window_size = 48)
pracma::toc()

## User definitions
pracma::tic()
NB <- 1 # define subbasin (not subbasin nr from cosero, but column)
wdw <- 2 # wdw = days before and after a maximum peak which are to be ignored, since it is assumed that they are part of the same flood event.
HQcount <- 5 # HQcount = number of flood peaks to calculate
for (NB in 1:46) {
  result <- ExtractAndPlotQmax.func(Data.read, NB, wdw, HQcount)
  
  if (NB == 1) {
      r <- result
    } else
      {
        r <- rbind(r, result)
    }
}
pracma::toc()


write.table(as.matrix(r), file="results.csv", sep=";", row.names = FALSE)


#### function to perform calculation ####
ExtractAndPlotQmax.func <- function(Data.read, NB, wdw, HQcount) {
  
Date <- strptime(Data.read$Date, format ="%d.%m.%Y %H:%M")

# prepare some variables

x.1 <- seq(7, ncol(Data.read),2)
ND_60min <- 1:nrow(Data.read)

Qobs <- cbind(ND_60min, Data.read[,x.1[NB]])
colnames(Qobs) <- c("ND_60min","Qobs")

Qsim <- cbind(ND_60min, Data.read[,x.1[NB]+1])
colnames(Qsim) <- c("ND_60min","Qsim")


x <- seq(from=1, to= nrow(Data.read)+1, by=24)
Qmax.daily <- matrix(1, nrow=(length(x)-1), ncol=4)
colnames(Qmax.daily) <- c("ND_ObsMax","ObsMax", "ND_SimMax", "SimMax")

# loop to get maximum values per day of Qobs and Qsim
for (ND in seq(from=1, to = length(x)-1, by=1)){ 
  r1 <- x[ND]
  r2 <- x[ND+1]-1
  t <- Qobs[r1:r2,][Qobs[r1:r2,2] == max(Qobs[r1:r2,2], na.rm=TRUE)]
  Qmax.daily[ND,1:2] <- t[c(1, length(t)/2+1)] #if Q on a day is identical, use first occurence
  t <- Qsim[r1:r2,][Qsim[r1:r2,2] == max(Qsim[r1:r2,2], na.rm=TRUE)]
  Qmax.daily[ND,3:4] <- t[c(1, length(t)/2+1)] #if Q on a day is identical, use first occurence
}


# loop to extract maximum daily discharge values

# prepare data and sort according to size of Qobs
Q <- as.data.frame(Qmax.daily[,1:2])
Q <- cbind(1:nrow(Q), Q)
colnames(Q)[1] <- "ND_day"
Q <- Q[order(Q$ObsMax, decreasing = TRUE),] 


for (i in 1:HQcount) {

  if (i == 1) {
    Qobs.fin <- Q[1,]
   } else
  {
    Qobs.fin <- rbind(Qobs.fin, Q[1,])
  }
  t <- Q$ND_day[1]
  t.1 <- Q[Q$ND_day <= (t+wdw) & Q$ND_day >= (t-wdw),]
  Q <- Q[Q$ND_day != t.1$ND_day[1], ]
  if (nrow(t.1)>1) {
      for (j in 2:nrow(t.1)){
        Q <- Q[Q$ND_day != t.1$ND_day[j], ]
      }
    }
    
  #print(i)
}


# extract maximum simulated runoff values in a window of +/- "wdw"
Q <- as.data.frame(Qmax.daily[,3:4])
Q <- cbind(1:nrow(Q), Q)
colnames(Q)[1] <- "ND_day"

for (ND in 1:nrow(Qobs.fin)) {
  
  x <- seq(Qobs.fin$ND_day[ND]-wdw, Qobs.fin$ND_day[ND]+wdw)
  for (i in 1:(2*wdw+1)){ 

    t <- Q[Q$ND_day == x[i],]
      if (i==1) {
        Qmax <- max(t$SimMax)
        Qsim.temp <- t
      }
      if (Qmax<max(t$SimMax)) {
      Qsim.temp <- t
      Qmax <- max(t$SimMax)
      }
   }
  
  if (ND == 1) {
    Qsim.fin <- Qsim.temp
  } else
  {
    Qsim.fin <- rbind(Qsim.fin, Qsim.temp)
  }

}


## generate plots and save
print (paste("plots saved to ", getwd() , sep=""))

NB.1 <- paste(formatC(NB, width=3, flag="0"))

# Scatterplot
title <- paste(colnames(Data.read[x.1[NB]]),colnames(Data.read[x.1[NB]+1]), sep="-")
save.plot <- paste(NB.1,"scatterplot",title,".png", sep="_")
png(save.plot, width=11, height=10, units="cm", res=300)

#layout(matrix(c(1:6), 2, 3, byrow=TRUE))
par(mar = c(2.0, 2.0, 1.7, 0.5), oma=c(1,1,0,0), tcl=0.2, mgp = c(0.5,0.2,0))
#par(cex = 0.9)

t <- max(Qobs.fin$ObsMax, Qsim.fin$SimMax)
ymax <- round(t*1.2,digits = -1)

if (ymax < 15) {
  base <- 2.5
  x <-  ymax/2.5 
  } else if (ymax < 50) {
  base <- 5
  x <-  ymax/5
} else if(ymax < 500) {
  base <- 10
  x <-  ymax/10
} else if (ymax > 500){
  base <- 5
  x <-  ymax/5
}

at.y.by <- base*max(round(x/base, 1))
at.y <- seq(0, ymax, at.y.by)

pch.cex.obs <- 1.
cex.1 <- 1.
x.val <- Qobs.fin$ObsMax
y.val <-Qsim.fin$SimMax



plot(x.val , y.val, pch=19, col=Data.read$Calib1_Valid2[Qobs.fin$ND_ObsMax], ylim=c(0.0,ymax), xlim=c(0.0,ymax),
     cex=pch.cex.obs, xaxt="n", yaxt="n", xlab="", ylab="")
abline(0, 1, col="darkgrey", lty=1, lwd=0.8)
points(x.val , y.val, pch=19, col=Data.read$Calib1_Valid2[Qobs.fin$ND_ObsMax], ylim=c(0.0,ymax), xlim=c(0.0,ymax),
     cex=pch.cex.obs, xaxt="n", yaxt="n", xlab="", ylab="")
## regression line
#abline(lm(x.val~y.val), col="red", lwd=0.8, lty=2)
#x.pos <- 25
#y.pos <- 15
#exp.label <- paste("R?=", round(summary(lm(x.val~y.val))$r.squared,3), sep="")
#text(x.pos, y.pos, label=exp.label, cex= 0.8, offset=0,pos=c(3))
#legend("bottomright", exp.label, cex=0.6, bty="n")

# x und Y-Achse

axis(1, las=0, cex.axis=cex.1, at=at.y, labels=at.y)
mtext("max. Qobs", side=1, line=1.3, cex=cex.1)
axis(2, las=2, cex.axis=cex.1, at=at.y, labels=at.y)
mtext("max. Qsim", side=2, line=2, cex=cex.1)

legend("topleft", title, text.col="black" , box.lwd=0, box.col="white", bg="white", cex=0.7)
legend("bottomright", c("Kalibrierung", "Validierung"), col=c(1,2), pch=c(19,19), box.lwd=0.5, box.col="black", cex=0.7)
box()

dev.off()

## plot Qobs hydrographs inkl. max Qobs

#  data manipulation
Q <- as.data.frame(Qmax.daily[,1:2])
Q <- cbind(1:nrow(Q), Q)
colnames(Q)[1] <- "ND_day"

#plot (Q$ND_day[2000:2500], Q$ObsMax[2000:2500], type="l", col="blue", xlab="ND", ylab="Gr??ter Tagesabfluss[m?/s]")

title <- colnames(Data.read[x.1[NB]])
save.plot <- paste(NB.1,"hydrograph",title,".png", sep="_")
png(save.plot, width=18, height=9, units="cm", res=300)

ymax <- max(Qobs.fin$ObsMax, Qsim.fin$SimMax)
pch.cex.obs <- 0.4

par(cex = 0.9)
par(mar = c(2, 3, 2, 0.125), mgp = c(0.5,0.2,0), tcl=0.5)

plot(Date[Q$ND_ObsMax], Q$ObsMax, type="l", col="blue", ylim=c(0.0,ymax*1.1), 
     cex=pch.cex.obs, ylab="", xlab="")
points(Date[Qobs.fin$ND_ObsMax], Qobs.fin$ObsMax, type="p", col=Data.read$Calib1_Valid2[Qobs.fin$ND_ObsMax], pch=14)
mtext("Max. Tagesabfluss [m?/s]", side=2, line=1.5, cex=0.8)

legend("topright", title, text.col="black" , box.lwd=0, box.col="white", bg="white", cex=0.7)

loc.1 <- sum(Data.read$Calib1_Valid2==1)+1
#loc.2 <- sum(Data.read$Calib1_Valid2==2)/2
#arrows(as.numeric(Date[loc.1]), 0.1, x1=as.numeric(Date[loc.1/1.5]), 0.1, lty=3, length = 0.25)
#text(x=as.numeric(Date[loc.1-loc.1/2.8]), y=0.1,labels = "Kalibrierung", cex=0.7,pos = 2)
#arrows(as.numeric(Date[loc.1]), 0.1, x1=as.numeric(Date[loc.1+(loc.2/1.5)]), 0.1, lty=3, length = 0.25)
#text(x=as.numeric(Date[loc.1+loc.2/0.6]), y=0.1,labels = "Validierung", cex=0.7,pos = 2)

abline(v=as.numeric(Date[loc.1]),  col = "gray", lty = 2)
box()

dev.off()

result <- cbind(rep(NB,times=length(Date[Qobs.fin$ND_ObsMax])), Qobs.fin, Date[Qobs.fin$ND_ObsMax],Qsim.fin,Date[Qsim.fin$ND_SimMax])
return(result)

}