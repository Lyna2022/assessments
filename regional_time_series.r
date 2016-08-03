##Script to plot Plant2Adapt style time series of projections
library(zoo)

read.data <- function(read.dir,var.name) {

  rcp26.file <- paste(read.dir,var.name,'_okanagan_rcp26_seasonal_time_series.RData',sep='')
  load(rcp26.file)
  rcp45.file <- paste(read.dir,var.name,'_okanagan_rcp45_seasonal_time_series.RData',sep='')
  load(rcp45.file)
  rcp85.file <- paste(read.dir,var.name,'_okanagan_rcp85_seasonal_time_series.RData',sep='')
  load(rcp85.file)

  rv <- list(rcp26=data.rcp26,
             rcp45=data.rcp45,
             rcp85=data.rcp85)
  return(rv)  
}

convert.tas.data <- function(read.dir) {

  tasmax.data <- read.data(read.dir,var.name='tasmax')
  tasmin.data <- read.data(read.dir,var.name='tasmin')
  tas.data <- vector(mode='list',length=3)
  for (i in 1:3) {
    tasmax.sub <- tasmax.data[[i]]
    tasmin.sub <- tasmin.data[[i]]    
    tas.data[[i]] <- mapply(FUN=function(x,y){(x+y)/2},tasmax.sub,tasmin.sub,SIMPLIFY=FALSE)
  }
  names(tas.data) <- c('rcp26','rcp45','rcp85')
  data.rcp26 <- tas.data[[1]]
  save(data.rcp26,file=paste(read.dir,'tas_okanagan_rcp26_seasonal_time_series.RData',sep=''))
  data.rcp45 <- tas.data[[2]]
  save(data.rcp45,file=paste(read.dir,'tas_okanagan_rcp45_seasonal_time_series.RData',sep=''))
  data.rcp85 <- tas.data[[3]]
  save(data.rcp85,file=paste(read.dir,'tas_okanagan_rcp85_seasonal_time_series.RData',sep=''))
  
}

                                                 


plot.data <- function(var.name,seas) {
  read.dir <- '/home/data/projects/rci/data/assessments/okanagan/data_files/'
  series.data <- read.data(read.dir,var.name)
  if (grepl('(gdd|ffd|pas)',var.name)) {
    seas.data <- series.data
  } else {
    seas.data <- lapply(series.data,function(x){return(x[[seas]])})
  }
  seas.matrix <- rbind(t(seas.data$rcp26),t(seas.data$rcp45),t(seas.data$rcp85))

  base <- apply(seas.matrix[,21:50],1,median,na.rm=T)

  base.means <- matrix(base,nrow=nrow(seas.matrix),ncol=ncol(seas.matrix),byrow=F)

  seas.anoms <- seas.matrix - base.means

  if (grepl('(pr|pas)', var.name))
    seas.anoms <- (seas.matrix - base.means)/base.means*100

  seas.rolled <- apply(seas.anoms,1,rollapply,30,mean,partial=T,na.rm=T)

  seas.test <- seas.rolled - matrix(seas.rolled[21,],nrow=ncol(seas.matrix),ncol=nrow(seas.matrix),byrow=T)
  seas.rolled <- seas.test
  seas.mean <- apply(seas.rolled,1,quantile,0.5,na.rm=T)
  seas.quantiles <- cbind(apply(seas.rolled,1,quantile,0.1,na.rm=T),
                          apply(seas.rolled,1,quantile,0.25,na.rm=T),
                          apply(seas.rolled,1,quantile,0.75,na.rm=T),
                          apply(seas.rolled,1,quantile,0.9,na.rm=T))
  yrs <- 1951:2100
  print(range(seas.quantiles,na.rm=T))

  rv <- list(avg=seas.mean,
             spread=seas.quantiles,
             all=seas.anoms)
  return(rv)
}

make.plot <- function(var.name,seas,
                      yvals) {
  yrs <- 1971:2100
  print(var.name)
  print(seas)
  vals <- plot.data(var.name,seas)

  mean.anoms <- vals$avg[21:150]
  mean.quantiles <- vals$spread[21:150,]
  y.label <- 'Anomalies (\u00B0C)'
  if (var.name=='pr')
    y.label <- 'Anomalies (%)'
  if (var.name=='ffd')
    y.label <- 'Anomalies (Days)'
  if (var.name=='gdd')
    y.label <- 'Anomalies (Degree Days)'
  if (var.name=='pas')
    y.label <- 'Anomalies (%)'
  
  ##Precip Plot
  png(file=paste('/home/data/projects/rci/data/assessments/okanagan/plots/okanagan/',var.name,'_',seas,'_series_plot.png',sep=''),
        width=900,height=800)
  par(mar=c(5,5,2,2))
  plot(c(),ylim=c(yvals[1],yvals[2]),xlim=c(1970,2100),axes=FALSE,
       xlab='Years',ylab=y.label,cex.axis=2,cex.lab=2) ##
  axis(1,seq(1960,2100,20),cex.axis=2,cex.lab=2)
  axis(2,seq(yvals[1],yvals[2],yvals[3]),cex.axis=2,cex.lab=2)
  
  lines(yrs,mean.quantiles[,2],lwd=3,col='lightgray')
  lines(yrs,mean.quantiles[,3],lwd=3,col='lightgray')
  polygon(c(yrs,rev(yrs)),c(mean.quantiles[,1],rev(mean.quantiles[,4])),col='lightgray',border='lightgray')
  polygon(c(yrs,rev(yrs)),c(mean.quantiles[,2],rev(mean.quantiles[,3])),col='gray',border='gray')
  # for(i in 1:33)
  #   lines(yrs,vals$all[i,],col='lightgray')
  
  lines(yrs,mean.anoms,lwd=3)
  
  abline(h=seq(yvals[1],yvals[2],yvals[3]),col='darkgray',lty=2,lwd=2)
  box(which='plot')
  dev.off()

}

make.plot('pr','winter', yvals=c(-20,40,10))
make.plot('pr','spring', yvals=c(-20,40,10))
make.plot('pr','summer', yvals=c(-75,30,15))
make.plot('pr','fall', yvals=c(-20,40,10))
make.plot('pr','annual', yvals=c(-20,40,10))

make.plot('tas','winter', yvals=c(-2,10,1))
make.plot('tas','spring', yvals=c(-2,10,1))
make.plot('tas','summer', yvals=c(-2,10,1))
make.plot('tas','fall', yvals=c(-2,10,1))
make.plot('tas','annual', yvals=c(-2,10,1))


#make.plot('tasmax','winter', yvals=c(-1,6,1))
#make.plot('tasmax','spring', yvals=c(-2,7,1))
#make.plot('tasmax','summer', yvals=c(-1,10,1))
#make.plot('tasmax','fall', yvals=c(-1,7,1))
#make.plot('tasmax','annual', yvals=c(-1,7,1))

#make.plot('tasmin','winter', yvals=c(-2,7,1))
#make.plot('tasmin','spring', yvals=c(-2,7,1))
#make.plot('tasmin','summer', yvals=c(-1,10,1))
#make.plot('tasmin','fall', yvals=c(-1,7,1))
#make.plot('tasmin','annual', yvals=c(-1,7,1))

make.plot('ffd','annual',yvals=c(-20,120,20))
make.plot('gdd','annual',yvals=c(-200,1600,200))

make.plot('pas','annual',yvals=c(-60,20,10))
