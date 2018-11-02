##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)
library(zoo)
library(scales)
library(RColorBrewer)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)

choose.break.point <- function(map.class.breaks,bp=""){

  if (bp == "") { bp <- "median"}
  if (bp == "median"){
    bp <- median(map.class.breaks)
  } else {
    bp <- as.numeric(bp)
  }
  class.pairs <- rbind(map.class.breaks[1:length(map.class.breaks)-1], map.class.breaks[2:length(map.class.breaks)])
  ## DB: Changed code here to only look at the 1st class break of the pair.
  right <- apply(class.pairs, 2, function(x) { x[1] >= bp})
  left <- !right
  left.length <- length(which(left == T))#ncol(class.pairs[,left])
  right.length <- length(which(right == T))#ncol(class.pairs[,right])
  return(list(left.length, right.length))
}


colour.map <- function(ramp.lengths) {
  left.len <- ramp.lengths[[1]]
  right.len <- ramp.lengths[[2]]
  print(ramp.lengths)
  colour.brewer.light.red.to.dark.red <- c( "#FF9999","#FF6666","#FF3333","#FF0000","#CC0000","#990000")
  light.red.to.dark.red <- colorRampPalette(colors=colour.brewer.light.red.to.dark.red, bias=1, space = "Lab", interpolate = "linear")

  colour.brewer.dark.blue.to.light.blue <- c("#000099","#0000CC","#0000FF","#3333FF","#6666FF","#9999FF")
  dark.blue.to.light.blue <- colorRampPalette(colors=colour.brewer.dark.blue.to.light.blue, bias=1, space = "Lab", interpolate = "linear")  
  rv <- c(dark.blue.to.light.blue(left.len),light.red.to.dark.red(right.len))
  return(rv)  
}

get.class.breaks <- function(map.range, manual.breaks=""){
  print('Class Breaks')
  print(map.range)
  map.diff <- diff(map.range)
  class.width <- (map.diff/11)

  if (class.width < 5 & class.width >= 1) {
     class.rv <- ceiling(class.width/1)*1
  }
  if (class.width < 1) {
     class.rv <- ceiling(class.width/0.1)*0.1
     class.rv <- max(c(0.1,class.rv))
  }
  if (class.width < 0.251) {
     class.rv <- ceiling(class.width/0.05)*0.05
     class.rv <- max(c(0.05,class.rv))
  }
  if (class.width >= 5) {
     class.rv <- round(class.width/5)*5
     class.rv <- max(c(5,class.rv))
  }

  if (manual.breaks[1] != ""){
    class.breaks <- manual.breaks
  } else {
      map.min <- floor(map.range[1]/class.rv) * class.rv
      map.max <- ceiling(map.range[2]/class.rv) * class.rv
      class.breaks <-seq(from=map.min, to=map.max, by=class.rv)
  }
  print(class.breaks)
  return(class.breaks)
}

get.legend.colourbar <- function(map.range,my.bp='median',manual.breaks="") {

  class.breaks <- get.class.breaks(map.range, manual.breaks)

  divisions <- choose.break.point(class.breaks,my.bp)
  st <- divisions[[1]]
  en <- divisions[[2]]

  other.map <- colour.map(ramp.lengths=divisions)
  return(other.map)
}


##---------------------------------------------------------------------
##PCDS Data

area <- 'KOOTENAYS'
area.title <- 'Kootenay' ##'Central Interior'
##regions <- list(c('BULKLEY-NECHAKO','0.6'),c('FRASER-FORT GEORGE','0.4'))
regions <- list(c('CENTRAL KOOTENAY','0.39'),c('EAST KOOTENAY','0.47'),c('KOOTENAY BOUNDARY','0.14'))

seasons <- 'annual' ##c('winter','spring','summer','fall')
season.titles <- 'Annual' ##c('Winter','Spring','Summer','Fall')
type <- 'ann'

for (s in seq_along(seasons)) {
  season <- seasons[s]
  season.title <- season.titles[s]
  obs.dir <- paste0('/storage/data/projects/rci/data/assessments/bc/pcds/',season,'/')

  obs.data <- read.csv(paste0(obs.dir,season,'_tx_anoms_tseries_0017_',regions[[1]][1],'.csv'),header=TRUE,as.is=TRUE)
  obs.years <- obs.data[,1]

  tx.anoms <- matrix(0,nrow=length(obs.years),ncol=length(regions))
  tn.anoms <- matrix(0,nrow=length(obs.years),ncol=length(regions))

  for (i in seq_along(regions)) {
    obs.tx <- read.csv(paste0(obs.dir,season,'_tx_anoms_tseries_0017_',regions[[i]][1],'.csv'),header=TRUE,as.is=TRUE)
    tx.anoms[,i] <- obs.tx[,3]*as.numeric(regions[[i]][2])
    obs.tn <- read.csv(paste0(obs.dir,season,'_tn_anoms_tseries_0017_',regions[[i]][1],'.csv'),header=TRUE,as.is=TRUE)
    tn.anoms[,i] <- obs.tn[,3]*as.numeric(regions[[i]][2])
  
  }

  obs.anoms <- (apply(tx.anoms,1,sum) + apply(tn.anoms,1,sum))/2
  obs.anoms <- obs.anoms - mean(obs.anoms[72:101])

  obs.colours <- get.legend.colourbar(range(obs.anoms),my.bp=0,manual.breaks="")
  class.breaks <- get.class.breaks(range(obs.anoms))
  class.diff <- diff(class.breaks)[1]   
  rounded.anoms <- floor(obs.anoms/class.diff)*class.diff
  col.ix <- sapply(rounded.anoms,function(x,y){which(y %in% x)},class.breaks)               

  map.class.breaks.labels <- sapply(1:(length(class.breaks)-1), function(x){
    paste(class.breaks[x], "to", class.breaks[x+1])})

  plot.dir <- '/storage/data/projects/rci/data/assessments/bc/'
  plot.file <- paste0(plot.dir,tolower(area),'.',season,'.tas.2017.warmings.stripes.png')

  png(plot.file,width=1200,height=700)
  par(mar=c(5,3,5,12))
  barplot(height=rep(1,length(obs.anoms)),xaxs='i',yaxs='i',ylim=c(0,1),col=obs.colours[col.ix],space=0,names=obs.years,border=NA,
  main=paste0(season.title,' Average Temperature Anomalies in ',area.title),xlab='Year',ylab='',
  cex.axis=2,cex.lab=2,cex.main=2.5,axes=F)
  axis(1,at=obs.years,labels=obs.years,cex.axis=2)
  box(which='plot')
  par(xpd=TRUE)
  legend('topright',inset=c(-0.165,0),legend=rev(map.class.breaks.labels),col=rev(obs.colours),cex=2,pch=15,title='\u00B0C')
  dev.off()

}

