##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(zoo)
library(scales)
library(raster)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)


##---------------------------------------------------------------------
##PCDS Data - for T and P plots

get_tas_pcds_series <- function(obs.regions,seas,rd.shp) {

   tx.dir <- paste0('/storage/data/projects/crmp/bc_trends_indicators/',seas,'_tx_anoms/')
   tn.dir <- paste0('/storage/data/projects/crmp/bc_trends_indicators/',seas,'_tn_anoms/')
   rd.names <- rd.shp[[2]]

   if (length(obs.regions) > 1) {
      areas <- rep(NA,length(obs.regions))
      for (i in seq_along(obs.regions)) {
         rd.ix <- which(toupper(rd.names) %in% obs.regions[i])
         district <- rd.shp[rd.ix,]
         areas[i] <- area(district)/10E6
      }
      total.area <- sum(areas)
      weights <- areas/total.area
   } else {     
      weights <- 1
   }

   obs.anoms <- c()
   for (i in seq_along(obs.regions)) {
      obs.tx <- read.csv(paste0(tx.dir,seas,'_tx_anoms_tseries_0018_',obs.regions[i],'.csv'),header=TRUE,as.is=TRUE)
      obs.years <- obs.tx[,1]
      tx.anoms <- obs.tx[,3]
      obs.tn <- read.csv(paste0(tn.dir,seas,'_tn_anoms_tseries_0018_',obs.regions[i],'.csv'),header=TRUE,as.is=TRUE)
      tn.anoms <- obs.tn[,3]
      rd.anoms <- (tx.anoms + tn.anoms)/2
      rd.anoms <- rd.anoms - mean(rd.anoms[72:101])
      obs.anoms <- rbind(obs.anoms,weights[i]*rd.anoms)
   }
   if (length(obs.regions) > 1) {
      obs.anoms <- apply(obs.anoms,2,sum)  
   }
   return(list(years=obs.years,anoms=obs.anoms))
}

###--------------------------------------------------

get_pr_pcds_series <- function(obs.regions,seas,rd.shp) {

   pr.dir <- paste0('/storage/data/projects/crmp/bc_trends_indicators/',seas,'_ppt_anoms/')
   rd.names <- rd.shp[[2]]
   if (length(obs.regions > 1)) {
      areas <- rep(NA,length(obs.regions))
      for (i in seq_along(obs.regions)) {
         rd.ix <- which(toupper(rd.names) %in% obs.regions[i])
         district <- rd.shp[rd.ix,]
         areas[i] <- area(district)/10E6
      }
      total.area <- sum(areas)
      weights <- areas/total.area
   } else {
      weights <- 1
   }

   obs.anoms <- c()
   for (i in seq_along(obs.regions)) {
      obs.pr <- read.csv(paste0(pr.dir,seas,'_ppt_anoms_tseries_0018_',obs.regions[i],'.csv'),header=TRUE,as.is=TRUE)
      obs.years <- obs.pr[,1]
      pr.anoms <- obs.pr[,3]
      pr.anoms <- pr.anoms - mean(pr.anoms[72:101])
      obs.anoms <- rbind(obs.anoms,weights[i]*pr.anoms)
   }
   if (length(obs.regions) > 1) {
      obs.anoms <- apply(obs.anoms,2,sum)  
   }
   return(list(years=obs.years,anoms=obs.anoms*100))
}

##------------------------------------------------------------

get_season <- function(gcm.data,seas) {

   ix <- switch(seas,
                annual='-*-',      
                winter='-01-',
                spring='-04-',
                summer='-07-',
                fall='-10-')
   seas.subset <- lapply(gcm.data,function(x,ix){
                         return(list(series=x$series[grepl(ix,x$time)],
                                     time=x$time[grepl(ix,x$time)]))},ix)
   return(seas.subset)                                        
}

##------------------------------------------------------------

calculate_anomalies <- function(var.name,gcm.data,yst,yen) {

   bounds <- lapply(gcm.data,function(x,yst,yen){return(list(yst=head(grep(yst,x$time),1),yen=tail(grep(yen,x$time),1)))},yst,yen)
   if (var.name=='tas' | grepl('dd',var.name)) {
      anomalies <- mapply(function(x,y){return(x$series - mean(x$series[y$yst:y$yen],na.rm=T))},gcm.data,bounds,SIMPLIFY=FALSE)
   } else {
      anomalies <- mapply(function(x,y){
                          return( (x$series - mean(x$series[y$yst:y$yen],na.rm=T)) / mean(x$series[y$yst:y$yen],na.rm=T) * 100)
                                       },gcm.data,bounds,SIMPLIFY=FALSE)
   }

   return(anomalies)                                       
}


##------------------------------------------------------------

plot_tas_or_pr_series <- function(var.name,seas,yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.title) {

   seas.name <- seas
   if (seas %in% c('winter','spring','summer','fall')) {
      seas.name <- 'seasonal'
   }

   if (var.name=='tas') {
      load(paste0(series.dir,region,'.tasmax.',seas.name,'.rcp45.bccaq.series.RData'))
      tasmax.rcp45 <- gcm.data 
      load(paste0(series.dir,region,'.tasmin.',seas.name,'.rcp45.bccaq.series.RData')) 
      tasmin.rcp45 <- gcm.data 
      rcp45.data <- mapply(function(x,y){return(list(series=(x$series+y$series)/2,time=y$time))},tasmax.rcp45,tasmin.rcp45,SIMPLIFY=FALSE)
      rcp45.seas <- get_season(rcp45.data,seas)
      rcp45.anoms <- calculate_anomalies(var.name,rcp45.seas,yst,yen)
      rcp45.matrix <- t(sapply(rcp45.anoms,'[',1:max(sapply(rcp45.anoms,length))))      
      rcp45.time <- lapply(rcp45.seas,function(x){return(x$time)})

      load(paste0(series.dir,region,'.tasmax.',seas.name,'.rcp85.bccaq.series.RData'))
      tasmax.rcp85 <- gcm.data 
      load(paste0(series.dir,region,'.tasmin.',seas.name,'.rcp85.bccaq.series.RData')) 
      tasmin.rcp85 <- gcm.data 
      rcp85.data <- mapply(function(x,y){return(list(series=(x$series+y$series)/2,time=y$time))},tasmax.rcp85,tasmin.rcp85,SIMPLIFY=FALSE)
      rcp85.seas <- get_season(rcp85.data,seas)
      rcp85.anoms <- calculate_anomalies(var.name,rcp85.seas,yst,yen)
      rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
      rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   } else {

      load(paste0(series.dir,region,'.',var.name,'.',seas.name,'.rcp45.bccaq.series.RData')) 
      rcp45.data <- gcm.data 
      rcp45.seas <- get_season(rcp45.data,seas)
      rcp45.anoms <- calculate_anomalies(var.name,rcp45.seas,yst,yen)
      rcp45.matrix <- t(sapply(rcp45.anoms,'[',1:max(sapply(rcp45.anoms,length))))      
      rcp45.time <- lapply(rcp45.seas,function(x){return(x$time)})


      load(paste0(series.dir,region,'.',var.name,'.',seas.name,'.rcp85.bccaq.series.RData'))
      rcp85.data <- gcm.data 
      rcp85.seas <- get_season(rcp85.data,seas)
      rcp85.anoms <- calculate_anomalies(var.name,rcp85.seas,yst,yen)
      rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
      rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   }

   rcp45.series <- apply(rcp45.matrix,2,mean,na.rm=T)
   rcp85.series <- apply(rcp85.matrix,2,mean,na.rm=T)

   obs.read <- switch(var.name,
                      tas=get_tas_pcds_series,
                      pr=get_pr_pcds_series)
 
   obs.series <- obs.read(obs.regions,seas,rd.shp)

   rcp45.10 <- apply(rcp45.matrix,2,quantile,0.1,na.rm=T)
   rcp45.90 <- apply(rcp45.matrix,2,quantile,0.9,na.rm=T)
   rcp85.10 <- apply(rcp85.matrix,2,quantile,0.1,na.rm=T)
   rcp85.90 <- apply(rcp85.matrix,2,quantile,0.9,na.rm=T)


   ylims <- c(floor(min(c(min(obs.series$anoms,min(rcp45.10),min(rcp85.10))))),
              ceiling(max(c(max(obs.series$anoms,max(rcp45.90),max(rcp85.90))))))

   ix <- 50:140
   px <- 1:50
   rx <- 11
   yrs <- 2005:2095
   hys <- 1956:2005

   ##rcp26.mean <- rollmean(rcp26.series,rx)
   rcp45.mean <- rollmean(rcp45.series,rx)
   rcp85.mean <- rollmean(rcp85.series,rx)

   hist.90 <- rollmean(rcp85.90,rx)[px]
   hist.10 <- rollmean(rcp85.10,rx)[px]

   rcp45.90.m <- rollmean(rcp45.90,rx)[ix]
   rcp45.10.m <- rollmean(rcp45.10,rx)[ix]
   rcp85.90.m <- rollmean(rcp85.90,rx)[ix]
   rcp85.10.m <- rollmean(rcp85.10,rx)[ix]

   ylabel <- switch(var.name,
                    tas='Temperature Change (\u00B0C)',
                    pr='Precipitation Change (%)')

   plot.dir <- '/storage/data/projects/rci/data/assessments/vancouver_island/plots/time_series/'
   plot.file <- paste0(plot.dir,region,'.',var.name,'.',seas,'.2018.png')

   png(file=plot.file,width=8,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5,5,5,3))
   plot(1951:2100,rcp45.series,type='l',lwd=4,col='white',ylim=ylims,
             main=plot.title,xlab='Year',ylab=ylabel,yaxs='i',xaxs='i',
             cex.axis=1.75,cex.lab=1.75,cex.main=2.15)

   leg.colours <- switch(var.name,
                         tas=c('black','red','orange'),
                         pr=c('black','blue','green'))

   polygon(c(yrs,rev(yrs)),c(rcp85.10.m,rev(rcp85.90.m)),col=alpha(leg.colours[2],0.3),border=alpha(leg.colours[2],0.2))
   polygon(c(yrs,rev(yrs)),c(rcp45.10.m,rev(rcp45.90.m)),col=alpha(leg.colours[3],0.3),border=alpha(leg.colours[3],0.2))
   polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

   lines(yrs,rcp85.mean[ix],lwd=2,col=leg.colours[2])
   lines(yrs,rcp45.mean[ix],lwd=2,col=leg.colours[3])
   lines(hys,rcp85.mean[px],lwd=2,col='darkgray')

   ##lines(c(1985,2010),rep(mean(rcp26.series[35:60]),2),col='black',lwd=3)

   ##abline(h=seq(-2,8,2),col='gray',lty=3,lwd=1)
   grid(nx=NA,ny=NULL,col='gray',lty=3,lwd=1)

   lines(obs.series$years,obs.series$anoms,lwd=2,col='black')
   box(which='plot')
   leg.colours <- switch(var.name,
                         tas=c('black','red','orange'),
                         pr=c('black','blue','green'))
   legend('topleft',legend=c('PCDS','RCP8.5','RCP4.5'),col=leg.colours,cex=1.75,pch=15)

   dev.off()

}



plot_lines_version_of_tas_or_pr_series <- function(var.name,seas,yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.title) {

   seas.name <- seas
   if (seas %in% c('winter','spring','summer','fall')) {
      seas.name <- 'seasonal'
   }

   if (var.name=='tas') {
      load(paste0(series.dir,region,'.tasmax.',seas.name,'.rcp85.bccaq.series.RData'))
      tasmax.rcp85 <- gcm.data 
      load(paste0(series.dir,region,'.tasmin.',seas.name,'.rcp85.bccaq.series.RData')) 
      tasmin.rcp85 <- gcm.data 
      rcp85.data <- mapply(function(x,y){return(list(series=(x$series+y$series)/2,time=y$time))},tasmax.rcp85,tasmin.rcp85,SIMPLIFY=FALSE)
      rcp85.seas <- get_season(rcp85.data,seas)
      rcp85.anoms <- calculate_anomalies(var.name,rcp85.seas,yst,yen)
      rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
      rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   } else {

      load(paste0(series.dir,region,'.',var.name,'.',seas.name,'.rcp85.bccaq.series.RData'))
      rcp85.data <- gcm.data 
      rcp85.seas <- get_season(rcp85.data,seas)
      rcp85.anoms <- calculate_anomalies(var.name,rcp85.seas,yst,yen)
      rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
      rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   }

   rcp85.series <- apply(rcp85.matrix,2,mean,na.rm=T)

   obs.read <- switch(var.name,
                      tas=get_tas_pcds_series,
                      pr=get_pr_pcds_series)
 
   obs.series <- obs.read(obs.regions,seas,rd.shp)

   rcp85.10 <- apply(rcp85.matrix,2,quantile,0.1,na.rm=T)
   rcp85.90 <- apply(rcp85.matrix,2,quantile,0.9,na.rm=T)


   ylims <- c(floor(min(rcp85.matrix,na.rm=T)),
              ceiling(max(rcp85.matrix,na.rm=T)))

   ix <- 50:140
   px <- 1:50
   rx <- 11
   yrs <- 2005:2095
   hys <- 1956:2005

   ##rcp26.mean <- rollmean(rcp26.series,rx)
   ##rcp45.mean <- rollmean(rcp45.series,rx)
   rcp85.mean <- rollmean(rcp85.series,rx)

   hist.90 <- rollmean(rcp85.90,rx)[px]
   hist.10 <- rollmean(rcp85.10,rx)[px]

   ##rcp45.90.m <- rollmean(rcp45.90,rx)[ix]
   ##rcp45.10.m <- rollmean(rcp45.10,rx)[ix]
   rcp85.90.m <- rollmean(rcp85.90,rx)[ix]
   rcp85.10.m <- rollmean(rcp85.10,rx)[ix]

   ylabel <- switch(var.name,
                    tas='Temperature Change (\u00B0C)',
                    pr='Precipitation Change (%)')

   plot.dir <- '/storage/data/projects/rci/data/assessments/okanagan/plots/time_series/'
   plot.file <- paste0(plot.dir,region,'.',var.name,'.',seas,'.2018.png')

   png(file=plot.file,width=8,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5,5,5,3))
   plot(1951:2100,rcp85.series,type='l',lwd=4,col='white',ylim=ylims,
             main=plot.title,xlab='Year',ylab=ylabel,
             cex.axis=1.75,cex.lab=1.75,cex.main=2.15)

   leg.colours <- switch(var.name,
                         tas=c('black','red','orange'),
                         pr=c('black','blue','green'))


   apply(rcp85.matrix,1,function(x,y){lines(y,x,col=alpha('red',0.5))},1951:2100)
   ##polygon(c(yrs,rev(yrs)),c(rcp85.10.m,rev(rcp85.90.m)),col=alpha(leg.colours[2],0.3),border=alpha(leg.colours[2],0.2))
   ##polygon(c(yrs,rev(yrs)),c(rcp45.10.m,rev(rcp45.90.m)),col=alpha(leg.colours[3],0.3),border=alpha(leg.colours[3],0.2))
   ##polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

##   lines(yrs,rcp85.mean[ix],lwd=2,col=leg.colours[2])
##   lines(yrs,rcp45.mean[ix],lwd=2,col=leg.colours[3])
##   lines(hys,rcp85.mean[px],lwd=2,col='darkgray')

   ##lines(c(1985,2010),rep(mean(rcp26.series[35:60]),2),col='black',lwd=3)

   ##abline(h=seq(-2,8,2),col='gray',lty=3,lwd=1)
   grid(nx=NA,ny=NULL,col='gray',lty=3,lwd=1)

   lines(obs.series$years,obs.series$anoms,lwd=2,col='black')
   box(which='plot')
   leg.colours <- switch(var.name,
                         tas=c('black','red'),
                         pr=c('black','blue'))
   legend('topleft',legend=c('PCDS','RCP8.5'),col=leg.colours,cex=1.75,pch=15)

   dev.off()

}


##--------------------------------------------------------
 
plot_degree_day_series <- function(var.name,yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.title) {

   load(paste0(series.dir,region,'.',var.name,'.degree.days.rcp45.bccaq.series.RData')) 
   rcp45.data <- gcm.data 
   rcp45.seas <- get_season(rcp45.data,'annual')
   rcp45.anoms <- calculate_anomalies(var.name,rcp45.seas,yst,yen)
   rcp45.matrix <- t(sapply(rcp45.anoms,'[',1:max(sapply(rcp45.anoms,length))))      
   rcp45.time <- lapply(rcp45.seas,function(x){return(x$time)})

   load(paste0(series.dir,region,'.',var.name,'.degree.days.rcp85.bccaq.series.RData'))
   rcp85.data <- gcm.data 
   rcp85.seas <- get_season(rcp85.data,'annual')
   rcp85.anoms <- calculate_anomalies(var.name,rcp85.seas,yst,yen)
   rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
   rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   rcp45.series <- apply(rcp45.matrix,2,mean,na.rm=T)
   rcp85.series <- apply(rcp85.matrix,2,mean,na.rm=T)

   ##obs.read <- switch(var.name,
   ##                   tas=get_dd_series,
   ##                   pr=get_dd_series)
   ##obs.series <- obs.read(obs.regions,seas,rd.shp)

   rcp45.10 <- apply(rcp45.matrix,2,quantile,0.1,na.rm=T)
   rcp45.90 <- apply(rcp45.matrix,2,quantile,0.9,na.rm=T)
   rcp85.10 <- apply(rcp85.matrix,2,quantile,0.1,na.rm=T)
   rcp85.90 <- apply(rcp85.matrix,2,quantile,0.9,na.rm=T)

   ##ylims <- c(floor(min(c(min(obs.series$anoms,min(rcp45.10),min(rcp85.10))))),
   ##           ceiling(max(c(max(obs.series$anoms,max(rcp45.90),max(rcp85.90))))))
   ylims <- c(floor(min(c(min(rcp45.10),min(rcp85.10)))),
              ceiling(max(c(max(rcp45.90),max(rcp85.90)))))

   ix <- 50:140
   px <- 1:50
   rx <- 11
   yrs <- 2005:2095
   hys <- 1956:2005

   ##rcp26.mean <- rollmean(rcp26.series,rx)
   rcp45.mean <- rollmean(rcp45.series,rx)
   rcp85.mean <- rollmean(rcp85.series,rx)

   hist.90 <- rollmean(rcp85.90,rx)[px]
   hist.10 <- rollmean(rcp85.10,rx)[px]

   rcp45.90.m <- rollmean(rcp45.90,rx)[ix]
   rcp45.10.m <- rollmean(rcp45.10,rx)[ix]
   rcp85.90.m <- rollmean(rcp85.90,rx)[ix]
   rcp85.10.m <- rollmean(rcp85.10,rx)[ix]

   ylabel <- 'Degree Day Change (Degree Days)'

   plot.dir <- '/storage/data/projects/rci/data/assessments/vancouver_island/plots/time_series/'
   plot.file <- paste0(plot.dir,region,'.',var.name,'.annual.2018.png')

   png(file=plot.file,width=8,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5,5,5,3))
   plot(1951:2100,rcp45.series,type='l',lwd=4,col='white',ylim=ylims,
             main=plot.title,xlab='Year',ylab=ylabel,yaxs='i',xaxs='i',
             cex.axis=1.75,cex.lab=1.75,cex.main=2.15)

   leg.colours <- c('red','orange')
                    
   polygon(c(yrs,rev(yrs)),c(rcp85.10.m,rev(rcp85.90.m)),col=alpha(leg.colours[1],0.3),border=alpha(leg.colours[1],0.2))
   polygon(c(yrs,rev(yrs)),c(rcp45.10.m,rev(rcp45.90.m)),col=alpha(leg.colours[2],0.3),border=alpha(leg.colours[2],0.2))
   polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

   lines(yrs,rcp85.mean[ix],lwd=2,col=leg.colours[1])
   lines(yrs,rcp45.mean[ix],lwd=2,col=leg.colours[2])
   lines(hys,rcp85.mean[px],lwd=2,col='darkgray')

   grid(nx=NA,ny=NULL,col='gray',lty=3,lwd=1)

   ##lines(obs.series$years,obs.series$anoms,lwd=2,col='black')
   box(which='plot')
   leg.colours <- c('red','orange')

   legend('topleft',legend=c('RCP8.5','RCP4.5'),col=leg.colours,cex=1.75,pch=15)

   dev.off()

}

get_climdex_info <- function(climdex.name) {

  climdex.names <- list(fdETCCDI=c('tas','Ann','days','Frost Days'),
                        suETCCDI=c('tas','Ann','days','Summer Days'),
                        su30ETCCDI=c('tas','Ann','days','Summer Hot Days'),
                        idETCCDI=c('tas','Ann','days','Ice Days'),
                        trETCCDI=c('tas','Ann','days','Tropical Nights'),
                        gslETCCDI=c('tas','Ann','days','Growing Season Length'),
                        txxETCCDI=c('tas','Mon','\u00B0C','Hottest Days'),
                        tnxETCCDI=c('tas','Mon','\u00B0C','Hottest Nights'),
                        txnETCCDI=c('tas','Mon','\u00B0C','Coldest Days'),
                        tnnETCCDI=c('tas','Mon','\u00B0C','Coldest Nights'),
                        tn10pETCCDI=c('tas','Mon','days','Cool Nights'),
                        tx10pETCCDI=c('tas','Mon','days','Cool Days'),
                        tn90pETCCDI=c('tas','Mon','days','Warm Nights'),
                        tx90pETCCDI=c('tas','Mon','days','Warm Days'),
                        wsdiETCCDI=c('tas','Ann','days','Warm Spell Duration'),
                        csdiETCCDI=c('tas','Ann','days','Cold Spell Duration'),
                        dtrETCCDI=c('tas','Mon','\u00B0C','Diurnal Temperature Range'),
                        rx1dayETCCDI=c('pr','Mon','mm','One Day Precipitation'),
                        rx2dayETCCDI=c('pr','Mon','mm','Two day precipitation'),
                        rx5dayETCCDI=c('pr','Mon','mm','Five day precipitation'),
                        sdiiETCCDI=c('pr','Ann','mm d-1','Simple Daily Intensity Index'),
                        r10mmETCCDI=c('pr','Ann','days','Heavy Precipitation Days'),
                        r20mmETCCDI=c('pr','Ann','days','Very Heavy Precipitation Days'),
                        cddETCCDI=c('pr','Ann','days','Consecutive Dry Days'),
                        cwdETCCDI=c('pr','Ann','days','Consecutive Wet Days'),
                        r95pETCCDI=c('pr','Ann','mm','Very Wet Days'),
                        r99pETCCDI=c('pr','Ann','mm','Extremely Wet Days'),
                        prcptotETCCDI=c('pr','Ann','mm','Annual Total Precipitation'),
                        r95daysETCCDI=c('pr','Ann','days','Number of Very Wet Days'),
                        r99daysETCCDI=c('pr','Ann','days','Number of Extremely Wet Days'))

  rv <- climdex.names[[climdex.name]]
  return(rv)
}

convert_mon_climdex_to_ann <- function(var.name,gcm.data) {
    fx <- switch(var.name,rx1dayETCCDI=max,rx2dayETCCDI=max,rx5dayETCCDI=max,
               txxETCCDI=max,tnxETCCDI=max,
               txnETCCDI=min,tnnETCCDI=min,
               tn10pETCCDI=mean,tx10pETCCDI=mean,
               tn90pETCCDI=mean,tx90pETCCDI=mean,
               dtrETCCDI=mean)

    clim.ann <- lapply(gcm.data,function(x,fxn){
                       return(list(series=tapply(x$series,as.factor(format(x$time,'%Y')),fxn,na.rm=TRUE),
                                   time=paste0(levels(as.factor(format(x$time,'%Y'))),'-01-01')))},fx)

    return(clim.ann)
}


plot_climdex_series <- function(var.name,yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.suffix) {

   climdex.info <- get_climdex_info(var.name)

   ##Convert monthly to annual
   load(paste0(series.dir,region,'.',var.name,'.climdex.rcp45.bccaq.series.RData')) 
   if (climdex.info[2] == 'Mon') {
      rcp45.data <- convert_mon_climdex_to_ann(var.name,gcm.data)
   } else {
      rcp45.data <- gcm.data
   }
   rcp45.seas <- get_season(rcp45.data,'annual')
   rcp45.anoms <- calculate_anomalies(climdex.info[1],rcp45.seas,yst,yen)
   rcp45.matrix <- t(sapply(rcp45.anoms,'[',1:max(sapply(rcp45.anoms,length))))      
   rcp45.time <- lapply(rcp45.seas,function(x){return(x$time)})

   load(paste0(series.dir,region,'.',var.name,'.climdex.rcp85.bccaq.series.RData'))
   if (climdex.info[2] == 'Mon') {
      rcp85.data <- convert_mon_climdex_to_ann(var.name,gcm.data)
   } else {
      rcp85.data <- gcm.data
   }
   rcp85.seas <- get_season(rcp85.data,'annual')
   rcp85.anoms <- calculate_anomalies(climdex.info[1],rcp85.seas,yst,yen)
   rcp85.matrix <- t(sapply(rcp85.anoms,'[',1:max(sapply(rcp85.anoms,length))))      
   rcp85.time <- lapply(rcp85.seas,function(x){return(x$time)})

   rcp45.series <- apply(rcp45.matrix,2,mean,na.rm=T)
   rcp85.series <- apply(rcp85.matrix,2,mean,na.rm=T)

   ##obs.read <- switch(var.name,
   ##                   tas=get_dd_series,
   ##                   pr=get_dd_series)
   ##obs.series <- obs.read(obs.regions,seas,rd.shp)

   rcp45.10 <- apply(rcp45.matrix,2,quantile,0.1,na.rm=T)
   rcp45.90 <- apply(rcp45.matrix,2,quantile,0.9,na.rm=T)
   rcp85.10 <- apply(rcp85.matrix,2,quantile,0.1,na.rm=T)
   rcp85.90 <- apply(rcp85.matrix,2,quantile,0.9,na.rm=T)

   ##ylims <- c(floor(min(c(min(obs.series$anoms,min(rcp45.10),min(rcp85.10))))),
   ##           ceiling(max(c(max(obs.series$anoms,max(rcp45.90),max(rcp85.90))))))
   ylims <- c(floor(min(c(min(rcp45.10),min(rcp85.10)))),
              ceiling(max(c(max(rcp45.90),max(rcp85.90)))))

   ix <- 50:140
   px <- 1:50
   rx <- 11
   yrs <- 2005:2095
   hys <- 1956:2005

   ##rcp26.mean <- rollmean(rcp26.series,rx)
   rcp45.mean <- rollmean(rcp45.series,rx)
   rcp85.mean <- rollmean(rcp85.series,rx)

   hist.90 <- rollmean(rcp85.90,rx)[px]
   hist.10 <- rollmean(rcp85.10,rx)[px]

   rcp45.90.m <- rollmean(rcp45.90,rx)[ix]
   rcp45.10.m <- rollmean(rcp45.10,rx)[ix]
   rcp85.90.m <- rollmean(rcp85.90,rx)[ix]
   rcp85.10.m <- rollmean(rcp85.10,rx)[ix]

   if (climdex.info[1] == 'pr') {
      ylabel <- paste0(toupper(var.name),' Change (%)')
   }
   if (climdex.info[1] == 'tas') {
      ylabel <- paste0(toupper(var.name),' Change (',climdex.info[3],')')
   }

   plot.dir <- '/storage/data/projects/rci/data/assessments/vancouver_island/plots/time_series/'
   plot.file <- paste0(plot.dir,region,'.',var.name,'.',seas,'.2018.png')

   plot.title <- paste0('Annual Average ',climdex.info[4],' Anomalies\n',plot.suffix)

   png(file=plot.file,width=8,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5,5,5,3))
   plot(1951:2100,rcp45.series,type='l',lwd=4,col='white',ylim=ylims,
             main=plot.title,xlab='Year',ylab=ylabel,yaxs='i',xaxs='i',
             cex.axis=1.75,cex.lab=1.75,cex.main=2.15)

   leg.colours <- switch(climdex.info[1],
                         tas=c('red','orange'),
                         pr=c('blue','green'))
                    
   polygon(c(yrs,rev(yrs)),c(rcp85.10.m,rev(rcp85.90.m)),col=alpha(leg.colours[1],0.3),border=alpha(leg.colours[1],0.2))
   polygon(c(yrs,rev(yrs)),c(rcp45.10.m,rev(rcp45.90.m)),col=alpha(leg.colours[2],0.3),border=alpha(leg.colours[2],0.2))
   polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

   lines(yrs,rcp85.mean[ix],lwd=2,col=leg.colours[1])
   lines(yrs,rcp45.mean[ix],lwd=2,col=leg.colours[2])
   lines(hys,rcp85.mean[px],lwd=2,col='darkgray')

   grid(nx=NA,ny=NULL,col='gray',lty=3,lwd=1)

   ##lines(obs.series$years,obs.series$anoms,lwd=2,col='black')
   box(which='plot')

   legend('topleft',legend=c('RCP8.5','RCP4.5'),col=leg.colours,cex=1.75,pch=15)

   dev.off()

}


##------------------------------------------------------------

##obs.regions <- c('ALBERNI-CLAYOQUOT','CAPITAL','COMOX VALLEY','COWICHAN VALLEY',
##             'MOUNT WADDINGTON','NANAIMO','STRATHCONA')
##region <-  'vancouver_island'
##plot.suffix <- 'Vancouver Island'

obs.regions <- c('CENTRAL OKANAGAN','NORTH OKANAGAN','OKANAGAN-SIMILKAMEEN')
region <- 'okanagan'
plot.suffix <- 'Okanagan Districts'

yst <- 1981
yen <- 2010

series.dir <- '/storage/data/projects/rci/data/assessments/okanagan/okanagan_districts/series_files/'
rd.file <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/RD_2011.shp'
rd.shp <- shapefile(rd.file)

shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan/'
reg.shp <- shapefile(paste0(shape.dir,region))

##----------------------------------------------------
##Temperature and Precipitation 
if (1==1) {
var.list <- 'tas' ##c('pr','tas')
var.titles <- 'Temperature' ##c('Precipitation','Temperature')
seas.titles <- 'Annual' ##c('Winter','Spring','Summer','Fall','Annual')

for (v in seq_along(var.list)) {
   for (seas.title in seas.titles) {
      seas <- tolower(seas.title)
      plot.title <- paste0(seas.title,' Average ',var.titles[v],' Anomalies\n',plot.suffix)
      plot_lines_version_of_tas_or_pr_series(var.list[v],seas,yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.title)
   }
}

browser()
##----------------------------------------------------
##Degree Days

var.list <- c('cdd','fdd','hdd','gdd')
var.titles <- c('Cooling Degree Day','Freezing Degree Day','Heating Degree Day','Growing Degree Day')
for (v in seq_along(var.list)) {
    plot.title <- paste0('Annual Average ',var.titles[v],' Anomalies\n',plot.suffix)
##    plot_degree_day_series(var.list[v],yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.title)
}
}

##----------------------------------------------------
##Climdex
##'su30ETCCDI',
var.list <- c('fdETCCDI','suETCCDI','idETCCDI','trETCCDI','gslETCCDI',
              'txxETCCDI','txnETCCDI','tnnETCCDI','tnxETCCDI','dtrETCCDI',
              'rx1dayETCCDI','rx5dayETCCDI',
              'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI',
              'cwdETCCDI','cddETCCDI',
              'prcptotETCCDI','r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI')
##var.list <- 'fdETCCDI'
for (v in seq_along(var.list)) {
    print(var.list[v])
##    plot_climdex_series(var.list[v],yst,yen,region,reg.shp,obs.regions,rd.shp,series.dir,plot.suffix)
}
