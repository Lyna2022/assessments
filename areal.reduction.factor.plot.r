##Script to gather station, ANUSPLIN, BCCAQ2-PRISM data
##and plot PDFs to illustrate the effect of spatial
##resolution on event magnitudes

##-----------------------------------------------------------------
library(scales)

source('/storage/home/ssobie/code/repos/assessments/single.site.bccaq.to.prism.r')

##-----------------------------------------------------------------
##Find AHCCD station metadata

metadata_for_ahccd <- function(stn.name,var.name,ahccd.dir) {
   avar <- switch(var.name,
                  tasmax='temperature',
                  tasmin='temperature',
                  tas='temperature',
                  pr='precipitation')
                     
   meta.file <- paste0(ahccd.dir,avar,'/ahccd_',avar,'_stations.csv')
   meta.data <- read.csv(meta.file,header=F,as.is=T)
   meta.header <- c('Province','Name','ID','YStart','MStart','YEnd','Mend','Lat','Lon','Elev','Join')
   names(meta.data) <- meta.header
   ##stn.ix <- grep(paste0('^',toupper(stn.name),'$'),meta.data$Name) ##Exact match to station name
   stn.ix <- grep(toupper(stn.name),meta.data$Name)

   if (length(stn.ix)==0) {
      stop('There is not any AHCCD station by that name')
   }
   if (length(stn.ix)>1) {
      print(meta.data$Name[stn.ix])
      stop('There are multiple AHCCD stations by that name')
   }
   stn.meta <- meta.data[stn.ix,]
   return(stn.meta)
}

##-----------------------------------------------------------------
##Retrieve raw AHCCD station data 

read_ahccd_txt_file <- function(var.name,stn.id,ahccd.dir) {

   avar <- switch(var.name,tasmax='dx',tasmin='dn',tas='dm',pr='dt')
   adir <- switch(var.name,
                  tasmax='temperature/Homog_daily_max_temp/',
                  tasmin='temperature/Homog_daily_min_temp/',
                  tas='temperature/Homog_daily_mean_temp/',
                  pr='precipitation/Adj_totalP_daily_v2012/')

   ahccd.file <- paste0(ahccd.dir,adir,avar,stn.id,'.txt')
   ahccd.widths <- switch(var.name,tasmax=c(4,1,2,1,rep(c(7,1),31)),
                                   tasmin=c(4,1,2,1,rep(c(7,1),31)),
                                   tas=c(4,1,2,1,rep(c(7,1),31)),
                                   pr=c(4,1,2,1,rep(c(8,1),31)))
   ##Accounts for letters after some values
   header <- rep(0,62)
   header[seq(1,61,2)] <- 1:31
   header[seq(2,62,2)] <- 'Flag'
   header <- c('Year','','Month','',header)
   ahccd.raw <- read.fwf(ahccd.file,widths=ahccd.widths,skip=2) 
   names(ahccd.raw) <- header
   return(ahccd.raw)
}

##------------------------------------------
##Reformat AHCCD into useful structure
reformat_ahccd_to_series <- function(ahccd.raw) {

   data.ix <- names(ahccd.raw) %in% 1:31
   ahccd.matrix <- as.matrix(ahccd.raw[,data.ix])
   ahccd.vector <- as.vector(t(ahccd.matrix))
   flag <- ahccd.vector==-9999.9 | ahccd.vector==-9999.99
   ahccd.vector[flag] <- NA

   years <- ahccd.raw[,1]
   months <- ahccd.raw[,3]
   ymons <- paste(years,sprintf('%02d',months),sep='-')
   days <- 1:31
   dates <- as.Date(as.vector(sapply(ymons,function(x){paste(x,sprintf('%02d',days),sep='-')},simplify=TRUE)))
   false.dates <- is.na(dates)
   ahccd.series <- ahccd.vector[!false.dates]
   ahccd.dates <- dates[!false.dates]   

   rv <- list(data=ahccd.series,dates=ahccd.dates)
   return(rv)
}


##-----------------------------------------------------------------
##Function to retrieve ANUSPLIN gridded data
retrieve_anusplin_series <- function(var.name,lonc,latc,asplin.dir) {

   asplin.file <- paste0(asplin.dir,'anusplin_',var.name,'_BC.nc')
   nc <- nc_open(asplin.file)
   lon <- ncvar_get(nc,'lon')
   lat <- ncvar_get(nc,'lat')
   lon.ix <- which.min(abs(lonc-lon))
   lat.ix <- which.min(abs(latc-lat))       

   asplin.data <- ncvar_get(nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
   time.atts <- ncatt_get(nc,'time')
   time.calendar <- time.atts$calendar
   time.units <- time.atts$units
   time.values <- ncvar_get(nc,'time')
   origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                            cal=time.calendar)
   asplin.dates <- origin.pcict + time.values*86400

   rv <- list(data=asplin.data,dates=asplin.dates)
   nc_close(nc)
   return(rv)
}


##-----------------------------------------------------------------
##Return the data within the year range

retrieve_year_subset <- function(input,yst,yen,var.name) {

  dates <- input$dates
  st <- head(grep(yst,dates),1)
  en <- tail(grep(yen,dates),1)
  data.subset <- input$data[st:en]
  dates.subset <- dates[st:en]
  if (var.name=='pr') {
     zero <- data.subset < 0.1
     data.subset <- data.subset[!zero]
     dates.subset <- dates.subset[!zero]
  }

  rv <- list(data=data.subset,dates=dates.subset)
  return(rv)

}

##-----------------------------------------------------------------
##Calculate annual quantiles

annual_quantiles <- function(input,qval) {

  ann.fac <- as.factor(format(input$dates,'%Y'))
  ann.qts <- tapply(input$data,ann.fac,quantile,qval,na.rm=T)
  rv <- list(years=levels(ann.fac),data=ann.qts)
  return(rv)
}

##-----------------------------------------------------------------
##Calculate number of days above threshold

days_above <- function(input,thresh) {
  yr.fac <- as.factor(format(input$dates,'%Y'))
  days <- input$data*0
  days[input$data > thresh] <- 1
  total <- round(mean(tapply(days,yr.fac,sum,na.rm=T)),1)
  return(total)
}

##-----------------------------------------------------------------
##Create PDF plot of the three data types

##-----------------------------------------------------------------

##*****************************************************************
ptm <- proc.time()

ahccd.dir <- '/storage/data/climate/AHCCD/AHCCD_2012-04/'
yst <- 1971
yen <- 2000
var.name <- 'tasmax'
var.title <-  'Daily Minimum Temperature' ##'Daily Total Precipitation' ##
if (var.name=='tasmin') {
  qtl <- 0.05
} else {
  qtl <- 0.95
}

stn.names <- c('Victoria','Kelowna','Kamloops','Prince George',
               'Fort St John','Cranbrook') ##'Vancouver',
##stn.names <- c('Fort St John','Cranbrook')
stn.names <- 'Victoria'

for (stn.name in stn.names) {

   base.dir <- '/storage/data/projects/rci/data/assessments/bc/site_histograms/'
   save.dir <- paste0(base.dir,'saved_data/')

   plot.file <- paste0(base.dir,'plots/',var.name,'.site.histogram.at.',gsub(' ','_',stn.name),'.png')
   cdf.file <-  paste0(base.dir,'plots/',var.name,'.site.CDF.at.',gsub(' ','_',stn.name),'.png')
   stn.meta <- metadata_for_ahccd(stn.name,var.name,ahccd.dir)
   lonc <- stn.meta$Lon
   latc <- stn.meta$Lat

   ##---------------------------------------------------------
   ##AHCCD

   stn.id <- stn.meta$ID ###'1018620' ##Victoria Airport?
   ahccd.file <- paste0(save.dir,var.name,'.',gsub(' ','_',stn.name),'.',stn.id,'.AHCCD.series.RData')
   if (!file.exists(ahccd.file)) {
      ahccd.raw <- read_ahccd_txt_file(var.name,stn.id,ahccd.dir)
      ahccd.series <- reformat_ahccd_to_series(ahccd.raw)
      ahccd.subset <- retrieve_year_subset(ahccd.series,yst,yen,var.name)    
      save(ahccd.subset,file=ahccd.file)
   } else {
      load(ahccd.file)
   }

   ##---------------------------------------------------------
   ##ANUSPLIN
   asplin.file <- paste0(save.dir,var.name,'.',gsub(' ','_',stn.name),'.',stn.id,'.ANUSPLIN.series.RData')
   if (!file.exists(asplin.file)) {
      asplin.dir <- '/storage/data/climate/downscale/BCCAQ2/ANUSPLIN/'
      asplin.series <- retrieve_anusplin_series(var.name,lonc,latc,asplin.dir)
      asplin.subset <- retrieve_year_subset(asplin.series,yst,yen,var.name)
      save(asplin.subset,file=asplin.file)
   } else {
      load(asplin.file)
   }

   ##---------------------------------------------------------
   ##BCCAQ2-PRISM
   ##base.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/bccaq2_tps/BCCAQ2/'
   bccaq2.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/'

   gcm.list <- c('ACCESS1-0','CCSM4','CanESM2','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
                 'HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

   bccaq2.file <- paste0(save.dir,var.name,'.',gsub(' ','_',stn.name),'.',stn.id,'.BCCAQ2.series.RData')
   if (!file.exists(bccaq2.file)) {

      bccaq2.series <- retrieve_bccaq2_prism_series(
                             var.name=var.name,scenario='rcp85',
                             gcm.list=gcm.list,
                             lonc=lonc,latc=latc,
                             base.dir=bccaq2.dir)
      bccaq2.subset <- lapply(bccaq2.series,retrieve_year_subset,yst,yen,var.name)
      save(bccaq2.subset,file=bccaq2.file)
   } else {
      load(bccaq2.file)
   }

   bccaq2.merged <- unlist(lapply(bccaq2.subset,function(x){x$data}))

   ##---------------------------------------------------------
   ##Quantiles 

   ahccd.qt <- annual_quantiles(ahccd.subset,qtl)
   asplin.qt <- annual_quantiles(asplin.subset,qtl)

   bccaq2.qt <- lapply(bccaq2.subset,annual_quantiles,qtl)
   bccaq2.mean.qt <- lapply(bccaq2.qt,function(x){mean(x$data)})
 
   ##---------------------------------------------------------
   ##Days above for TASMAX
   if (var.name=='tasmax') {
      ahccd.25 <- days_above(ahccd.subset,25)
      asplin.25 <- days_above(asplin.subset,25)
      bccaq2.25 <- lapply(bccaq2.subset,days_above,25)

      ahccd.30 <- days_above(ahccd.subset,30)
      asplin.30 <- days_above(asplin.subset,30)
      bccaq2.30 <- lapply(bccaq2.subset,days_above,30)           
   }

   ##---------------------------------------------------------
   ##Create histogram plot
   bk.min <- floor(min(c(ahccd.subset$data,asplin.subset$data,bccaq2.merged),na.rm=T)/5)*5
   bk.max <- ceiling(max(c(ahccd.subset$data,asplin.subset$data,bccaq2.merged),na.rm=T)/5)*5
   print(c(bk.min,bk.max))
   breaks <- seq(bk.min,bk.max,1)
   ##if (var.name=='pr')
   ##   breaks <- seq(bk.min,ceiling(bk.max/5)*5,5)
   info.hist <- hist(bccaq2.merged,breaks=breaks,plot=F)
   ahccd.hist <- hist(ahccd.subset$data,breaks=breaks,plot=F)  
   asplin.hist <- hist(asplin.subset$data,breaks=breaks,plot=F)

   ymax <- ceiling(max(c(info.hist$density,ahccd.hist$density,asplin.hist$density),na.rm=T)/0.02)*0.02+0.02

   png(file=plot.file,width=10,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5.1,5.1,4.1,2.1))
   plot(c(),xlim=c(bk.min,bk.max),ylim=c(0,ymax),yaxs='i',xaxs='i',xlab='Temperature (\u00B0C)',ylab='Density',
        cex=2,cex.axis=2,cex.lab=2)
   title(main=stn.name,line=2.5,cex.main=1.95)
   title(main=var.title,line=1.0,cex.main=1.5)

   hist(bccaq2.merged,breaks=breaks,col=alpha('red',0.2),add=T,
     freq=F,yaxs='i',xaxs='i',xlab='Temperature',ylab='Density',cex.axis=1.75,cex.lab=1.75)

   hist(asplin.subset$data,add=T,col=alpha('blue',0.2),breaks=breaks,freq=F)
   hist(ahccd.subset$data,add=T,breaks=breaks,freq=F,col=alpha('black',0.2))

   hist(bccaq2.merged,breaks=breaks,border='red',add=T,freq=F)
   hist(asplin.subset$data,add=T,border='blue',breaks=breaks,freq=F)
   hist(ahccd.subset$data,add=T,breaks=breaks,freq=F,border='black')

   ##abline(v=unlist(bccaq2.mean.qt),col='black',lwd=0.5)
   ##abline(v=mean(unlist(bccaq2.mean.qt)),col='red',lwd=1.5)
   ###abline(v=mean(unlist(bccaq2.mean.qt)),col='red',lwd=2)
   ###abline(v=mean(ahccd.qt$data),lwd=2,col='black')
   ###abline(v=mean(asplin.qt$data),col='blue',lwd=2)
   ##Replace vertical lines with points

   leg.loc <- 'topleft'
   if (var.name=='pr')
      leg.loc <- 'bottomright'
   legend(leg.loc,leg=c('Station','(Point)','Downscaled','(800m x 800m)','Gridded Obs.','(6km x 10km)'),
                  col=c('black','white','red','white','blue','white'),pch=15,cex=2)

   if (var.name=='tasmax') {
     legend('topright',title='No. Days Above',
                    leg=c('25\u00B0C',
                    paste0('Stn: ',ahccd.25),
                    paste0('DS: ',round(mean(unlist(bccaq2.25)))),
                    paste0('Grid: ',asplin.25),
                    '30\u00B0C',
                    paste0('Stn: ',ahccd.30),
                    paste0('DS: ',round(mean(unlist(bccaq2.30)))),
                    paste0('Grid: ',asplin.30)),
                    col=c('white','black','red','blue','white','black','red','blue'),pch=15,cex=2)
   } else {
     legend('topright',title=paste0(round(qtl*100),'th %iles'),
                    leg=c(
                    paste0('Stn: ',round(mean(ahccd.qt$data),1)),
                    paste0('DS: ',round(mean(unlist(bccaq2.mean.qt)),1)),
                    paste0('Grid: ',round(mean(asplin.qt$data),1))),
                    col=c('black','red','blue'),pch=15,cex=2)
   }
   box(which='plot')
   par(xpd=NA)
   points(x=mean(unlist(bccaq2.mean.qt)),y=0,col='red',pch=18,cex=2)
   points(x=mean(asplin.qt$data),y=0,col='blue',pch=18,cex=2)
   points(x=mean(ahccd.qt$data),y=0,col='black',pch=18,cex=2)
   par(xpd=T)

   dev.off()

   ##------------------------------------------------------------------------
   ##Also create a CDF version
   png(file=cdf.file,width=10,height=6,units='in',res=600,pointsize=6,bg='white')
   par(mar=c(5.1,5.1,4.1,2.1))
   plot(c(),xlim=c(bk.min,bk.max),ylim=c(0,1),yaxs='i',xaxs='i',xlab='Temperature (\u00B0C)',ylab='Density',
        cex=2,cex.axis=2,cex.lab=2)
   title(main=stn.name,line=2.5,cex.main=1.95)
   title(main=var.title,line=1.0,cex.main=1.5)

   bccaq2.cdf <- ecdf(bccaq2.merged)
   ahccd.cdf <- ecdf(ahccd.subset$data)  
   asplin.cdf <- ecdf(asplin.subset$data)

   lines(sort(bccaq2.merged),bccaq2.cdf(sort(bccaq2.merged)),col='red',lwd=4)
   lines(sort(ahccd.subset$data),ahccd.cdf(sort(ahccd.subset$data)),col='black',lwd=4)
   lines(sort(asplin.subset$data),asplin.cdf(sort(asplin.subset$data)),col='blue',lwd=4)

   leg.loc <- 'topleft'
   legend(leg.loc,leg=c('Station','(Point)','Downscaled','(800m x 800m)','Gridded Obs.','(6km x 10km)'),
                  col=c('black','white','red','white','blue','white'),pch=15,cex=2)

   if (var.name=='tasmax') {
     legend('bottomright',title='No. Days Above',
                    leg=c('25\u00B0C',
                    paste0('Stn: ',ahccd.25),
                    paste0('DS: ',round(mean(unlist(bccaq2.25)))),
                    paste0('Grid: ',asplin.25),
                    '30\u00B0C',
                    paste0('Stn: ',ahccd.30),
                    paste0('DS: ',round(mean(unlist(bccaq2.30)))),
                    paste0('Grid: ',asplin.30)),
                    col=c('white','black','red','blue','white','black','red','blue'),pch=15,cex=2)
   } else {
     legend('bottomright',title=paste0(round(qtl*100),'th %iles'),
                    leg=c(
                    paste0('Stn: ',round(mean(ahccd.qt$data),1)),
                    paste0('DS: ',round(mean(unlist(bccaq2.mean.qt)),1)),
                    paste0('Grid: ',round(mean(asplin.qt$data),1))),
                    col=c('black','red','blue'),pch=15,cex=2)
   }

   box(which='plot')
   par(xpd=NA)
   points(x=mean(unlist(bccaq2.mean.qt)),y=0,col='red',pch=18,cex=2)
   points(x=mean(asplin.qt$data),y=0,col='blue',pch=18,cex=2)
   points(x=mean(ahccd.qt$data),y=0,col='black',pch=18,cex=2)
   par(xpd=T)

   dev.off()


}