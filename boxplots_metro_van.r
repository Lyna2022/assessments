##Script to plot Plant2Adapt style time series of projections
library(zoo)

read.data <- function(read.dir,var.name,region) {
  
  rcp26.file <- paste(read.dir,var.name,'_',region,'_rcp26_seasonal_time_series.RData',sep='')
  load(rcp26.file)
  names(data.rcp26) <- toupper(c(month.abb,'ANN')) 
  rcp45.file <- paste(read.dir,var.name,'_',region,'_rcp45_seasonal_time_series.RData',sep='')
  load(rcp45.file)
  names(data.rcp45) <- toupper(c(month.abb,'ANN'))  
  rcp85.file <- paste(read.dir,var.name,'_',region,'_rcp85_seasonal_time_series.RData',sep='')
  load(rcp85.file)
  names(data.rcp85) <- toupper(c(month.abb,'ANN')) 

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
  save(data.rcp26,file=paste(read.dir,'tas_metro_van_rcp26_seasonal_time_series.RData',sep=''))
  data.rcp45 <- tas.data[[2]]
  save(data.rcp45,file=paste(read.dir,'tas_metro_van_rcp45_seasonal_time_series.RData',sep=''))
  data.rcp85 <- tas.data[[3]]
  save(data.rcp85,file=paste(read.dir,'tas_metro_van_rcp85_seasonal_time_series.RData',sep=''))
  
}

                                                 


plot.data <- function(var.name,seas,region,scenario,anomalies,percent=FALSE) {
  read.dir <- '/home/data/projects/rci/data/assessments/metro_van/data_files/'
  series.data <- read.data(read.dir,var.name,region)
  if (grepl('(gdd|ffd|pas)',var.name)) {
    seas.data <- series.data
  } else {
    seas.data <- lapply(series.data,function(x){return(x[[seas]])})
  }

  seas.matrix <- t(seas.data[[scenario]]) ## rbind(t(seas.data$rcp26),t(seas.data$rcp45),t(seas.data$rcp85))##

  base <- apply(seas.matrix[,21:50],1,median,na.rm=T)
  base.means <- matrix(base,nrow=nrow(seas.matrix),ncol=ncol(seas.matrix),byrow=F)
  if (anomalies) {
    seas.anoms <- seas.matrix - base.means   
    if (percent)
      seas.anoms <- (seas.matrix - base.means)/base.means*100
  } else {
    seas.anoms <- seas.matrix
  }

#  seas.rolled <- apply(seas.anoms,1,rollapply,20,mean,partial=T,na.rm=T)
#  seas.mean <- apply(seas.rolled,1,quantile,0.5,na.rm=T)
#  seas.quantiles <- cbind(apply(seas.rolled,1,quantile,0.1,na.rm=T),
#                          apply(seas.rolled,1,quantile,0.25,na.rm=T),
#                          apply(seas.rolled,1,quantile,0.75,na.rm=T),
#                          apply(seas.rolled,1,quantile,0.9,na.rm=T))
#  yrs <- 1951:2100
#  print(range(seas.quantiles,na.rm=T))
#
#  rv <- list(avg=seas.mean,
#             spread=seas.quantiles,
#             all=seas.anoms)
  return(seas.anoms)
}

box.avgs <- function(var.name,seas,region,scenario,anomalies,percent) {

  yrs <- 1951:2100
  
  ix.base <- yrs %in% 1971:2000
  ix.2020s <- yrs %in% 2011:2040
  ix.2050s <- yrs %in% 2041:2070
  ix.2080s <- yrs %in% 2071:2100
  
  print(var.name)
  print(seas)
  vals <- plot.data(var.name,seas,region,scenario,anomalies,percent)

  base.vals <- vals[,ix.base]
  vals.2020s <- vals[,ix.2020s]
  vals.2050s <- vals[,ix.2050s]
  vals.2080s <- vals[,ix.2080s]

  clim.hist <- base.vals ##apply(base.vals,1,mean,na.rm=T)
  if (anomalies) {
    clim.past <- clim.hist - mean(clim.hist)
  } else {
    clim.past <- clim.hist
  }
  clim.2020s <- vals.2020s ##apply(vals.2020s,1,mean,na.rm=T)
  clim.2050s <- vals.2050s ##apply(vals.2050s,1,mean,na.rm=T)
  clim.2080s <- vals.2080s ##apply(vals.2080s,1,mean,na.rm=T)

  rv <- list(past=clim.past,
             c2020=clim.2020s,
             c2050=clim.2050s,
             c2080=clim.2080s)

  return(rv)
}

new.box <- function(at,data,add,boxwex,axes) { #     new.box(at=i+0.2,seas.2080s[[i]],add=T,boxwex=0.25,axes=F)##col='red',
  bb <- boxplot(data,plot=FALSE)
  bb$stats[c(1,5),] <- quantile(data,c(0.1,0.9),na.rm=TRUE)
  bxp(z=bb,at=at,add=add,boxwex=boxwex,axes=axes)
}
  
make.plot <- function(var.name,seasons,
                      yvals,region,scenario,
                      anomalies,percent) {

  plot.title <- switch(var.name,
                       pr='Precipitation Totals',
                       tasmax='Maximum Temperature',
                       tasmin='Minimum Temperature')
  yrs <- 1951:2100
  seas.past <- vector(mode='list',length=length(seasons))
  seas.2020s <- vector(mode='list',length=length(seasons))
  seas.2050s <- vector(mode='list',length=length(seasons))
  seas.2080s <- vector(mode='list',length=length(seasons))

  for (s in seq_along(seasons)){
    seas.anoms <- box.avgs(var.name,seasons[s],region,scenario,anomalies,percent)
    seas.past[[s]] <- as.vector(seas.anoms$past)
    seas.2020s[[s]] <- as.vector(seas.anoms$c2020)
    seas.2050s[[s]] <- as.vector(seas.anoms$c2050)
    seas.2080s[[s]] <- as.vector(seas.anoms$c2080)
  }
  y.label <- 'Temperature (\u00B0C)'
  if (anomalies)
    y.label <- 'Anomalies (\u00B0C)'
  if (var.name=='pr') {
    y.label <- 'Precipitation Totals (mm)'
    if (anomalies) 
      y.label <- 'Anomalies (mm)'
    if (percent)
      y.label <- 'Anomalies (%)'
  }
  
  ##Precip Plot
  if (anomalies) {
    plot.file <- paste('/home/data/projects/rci/data/assessments/metro_van/plots/',region,'/boxplots/anomalies/',var.name,'_anoms_',region,'_boxplots_',scenario,'.png',sep='')    
    if (percent)
      plot.file <- paste('/home/data/projects/rci/data/assessments/metro_van/plots/',region,'/boxplots/anomalies/',var.name,'_percent_',region,'_boxplots_',scenario,'.png',sep='')      
  } else {
    plot.file <- paste('/home/data/projects/rci/data/assessments/metro_van/plots/',region,'/boxplots/averages/',var.name,'_avgs_',region,'_boxplots_',scenario,'.png',sep='')    
  }
  png(file=plot.file,width=1500,height=800)
  par(mar=c(10,5,5,5))
  if (anomalies) {
    plot(c(),xlim=c(1,length(seasons)),ylim=c(yvals[1],yvals[2]),main=paste('Projected Change of ',plot.title,'\n in Metro Vancouver',sep=''),
         xlab='',ylab=y.label,cex.axis=2,cex.lab=2,cex.main=2.5,axes=F,yaxs='i')         
  } else if (var.name=='pr') {
    plot(c(),xlim=c(1,length(seasons)),ylim=c(yvals[1],yvals[2]),main=paste(plot.title,'\n in Metro Vancouver',sep=''),
         xlab='',ylab=y.label,cex.axis=2,cex.lab=2,cex.main=2.5,axes=F,yaxs='i')         
  } else {
    plot(c(),xlim=c(1,length(seasons)),ylim=c(yvals[1],yvals[2]),main=paste(plot.title,'\n in Metro Vancouver',sep=''),
         xlab='',ylab=y.label,cex.axis=2,cex.lab=2,cex.main=2.5,axes=F,yaxs='i')
  }
  axis(1,at=1:length(seasons),seasons,cex.axis=2)
  axis(2,at=seq(yvals[1],yvals[2],by=yvals[3]),seq(yvals[1],yvals[2],by=yvals[3]),cex.axis=2)
  for (i in seq_along(seasons)) {
    if (anomalies) {
      ##boxplot(at=i-0.3,seas.past[[i]],add=T,boxwex=0.25,axes=F)
      ##boxplot(at=i-0.1,seas.2020s[[i]],add=T,boxwex=0.25,axes=F) ##col='yellow',
      ##boxplot(at=i+0.1,seas.2050s[[i]],add=T,boxwex=0.25,axes=F) ##col='orange',
      ##boxplot(at=i+0.3,seas.2080s[[i]],add=T,boxwex=0.25,axes=F) ##col='red',
      new.box(at=i-0.3,seas.past[[i]],add=T,boxwex=0.25,axes=F)
      new.box(at=i-0.1,seas.2020s[[i]],add=T,boxwex=0.25,axes=F) ##col='yellow',
      new.box(at=i+0.1,seas.2050s[[i]],add=T,boxwex=0.25,axes=F) ##col='orange',
      new.box(at=i+0.3,seas.2080s[[i]],add=T,boxwex=0.25,axes=F) ##col='red',      
    } else {
#      boxplot(at=i-0.2,seas.past[[i]],add=T,boxwex=0.25,axes=F)
#      boxplot(at=i,seas.2050s[[i]],add=T,boxwex=0.25,axes=F) ##,col='orange'
#      boxplot(at=i+0.2,seas.2080s[[i]],add=T,boxwex=0.25,axes=F)##col='red',
      new.box(at=i-0.2,seas.past[[i]],add=T,boxwex=0.25,axes=F)
      new.box(at=i,seas.2050s[[i]],add=T,boxwex=0.25,axes=F) ##,col='orange'
      new.box(at=i+0.2,seas.2080s[[i]],add=T,boxwex=0.25,axes=F)##col='red',
    }
  }
  abline(v=seq(0.5,13.5,by=1))
  grid(ny=NULL,nx=NA,col='gray',lwd=1)
  box(which='plot')
  if (anomalies) {
#    legend('topright',legend=c('Past','2020s','2050s','2080s'),col=c('black','yellow','orange','red'),cex=2,pch=15)
  } else {
    if (var.name=='pr') {
#      legend('bottomright',legend=c('Past','2050s','2080s'),col=c('black','orange','red'),cex=2,pch=15)
    } else {
#      legend('topright',legend=c('Past','2050s','2080s'),col=c('black','orange','red'),cex=2,pch=15)
    }
  }
  dev.off()  
}


##North Vancouver
if (1==0) {
region <- 'north_van'
rcps <- c('rcp26','rcp45','rcp85')
for (rcp in rcps) {
  ##Anomalies  
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-400,700,100),region,rcp,TRUE,FALSE)
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-150,650,150),region,rcp,TRUE,TRUE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-10,20,5),region,rcp,TRUE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,15,5),region,rcp,TRUE,FALSE)
  ##Averages
  make.plot('pr',seasons=toupper(month.abb),yvals=c(0,1000,200),region,rcp,FALSE,FALSE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-5,40,5),region,rcp,FALSE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,25,5),region,rcp,FALSE,FALSE)
}
}

if (1==0) {
region <- 'van_city'
rcps <- c('rcp26','rcp45','rcp85')
for (rcp in rcps) {
  ##Anomalies  
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-300,700,100),region,rcp,TRUE,FALSE)
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-150,600,150),region,rcp,TRUE,TRUE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-10,20,5),region,rcp,TRUE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,15,5),region,rcp,TRUE,FALSE)
  ##Averages
  make.plot('pr',seasons=toupper(month.abb),yvals=c(0,900,200),region,rcp,FALSE,FALSE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-5,40,5),region,rcp,FALSE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,25,5),region,rcp,FALSE,FALSE)
}
}

if (1==1) {
region <- 'metro_van'
rcps <- c('rcp26','rcp45','rcp85')
for (rcp in rcps) {
  ##Anomalies  
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-300,700,100),region,rcp,TRUE,FALSE)
  make.plot('pr',seasons=toupper(month.abb),yvals=c(-150,600,150),region,rcp,TRUE,TRUE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-10,20,5),region,rcp,TRUE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,15,5),region,rcp,TRUE,FALSE)
  ##Averages
  make.plot('pr',seasons=toupper(month.abb),yvals=c(0,900,200),region,rcp,FALSE,FALSE)
  make.plot('tasmax',seasons=toupper(month.abb),yvals=c(-5,40,5),region,rcp,FALSE,FALSE)
  make.plot('tasmin',seasons=toupper(month.abb),yvals=c(-15,25,5),region,rcp,FALSE,FALSE)
}
}




