##This script extracts regional averages of climdex values for regions within BC.
##It is based on the original MOTI script that did the same for the three highway regions.
##The script must be called with a wrapper indicating the regions and paths.

library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)
library(PCICt)

##-----------------------------------------------------------------------------------------------

get.variable.title <- function(var.name) {

  var.title <- switch(var.name,
                      fdETCCDI='Frost Days',
                      suETCCDI='Summer Days',
                      su30ETCCDI='Summer Hot Days',
                      idETCCDI='Ice Days',
                      gslETCCDI='Growing Season Length',
                      wsdiETCCDI='Warm Spell Duration',
                      csdiETCCDI='Cold Spell Duration',
                      rx1dayETCCDI='One Day Precipitation',
                      rx5dayETCCDI='Five day precipitation',
                      sdiiETCCDI='Simple Daily Intensity Index',
                      r10mmETCCDI='Heavy Precipitation Days',
                      r20mmETCCDI='Very Heavy Precipitation Days',
                      cddETCCDI='Consecutive Dry Days',
                      cwdETCCDI='Consecutive Wet Days',
                      r95pETCCDI='Very Wet Days',
                      r95daysETCCDI='Number of Very Wet Days',
                      r95distETCCDI='Time between Wet Day Events',
                      r95sepETCCDI='Wet Day Intervals', 
                      r99pETCCDI='Extremely Wet Days',
                      r99daysETCCDI='Number of Extremely Wet Days',
                      prcptotETCCDI='Annual Total Precipitation',
                      trETCCDI='Tropical Nights',
                      txxETCCD='Hottest Days',
                      tnxETCCDI='Hottest Nights',
                      txnETCCDI='Coldest Days',
                      tnnETCCDI='Coldest Nights',
                      tn10pETCCDI='Cool Nights',
                      tx10pETCCDI='Cool Days',
                      tn90pETCCDI='Warm Nights',
                      tx90pETCCDI='Warm Days',
                      dtrETCCDI='Diurnal Temperature Range')
  return(var.title)
}

get.seas.fx <- function(var.name) {

  fx <- switch(var.name,
               rx1dayETCCDI=max,
               rx5dayETCCDI=max,
               txxETCCDI=max,
               tnxETCCDI=max,
               txnETCCDI=min,
               tnnETCCDI=min,
               tn10pETCCDI=mean,
               tx10pETCCDI=mean,
               tn90pETCCDI=mean,
               tx90pETCCDI=mean,
               dtrETCCDI=mean)
  if (is.null(fx))
    fx <- mean
  return(fx)
}


get.rounding.value <- function(var.name) {

  zero.list <- c('pr',
                 'fdETCCDI',
                 'suETCCDI',
                 'idETCCDI',
                 'gslETCCDI',
                 'wsdiETCCDI',
                 'csdiETCCDI',                 
                 'rx1dayETCCDI',
                 'rx5dayETCCDI',
                 'sdiiETCCDI',
                 'r10mmETCCDI',
                 'r20mmETCCDI',
                 'cddETCCDI',
                 'cwdETCCDI',
                 'r95pETCCDI',  
                 'r95daysETCCDI',
                 'r95distETCCDI',
                 'r95sepETCCDI',
                 'r99pETCCDI',
                 'r99daysETCCDI',
                 'prcptotETCCDI')
  
  one.list <- c('tasmax',
                'tasmin',
                'trETCCDI',
                'txxETCCDI',
                'tnxETCCDI',
                'txnETCCDI',
                'tnnETCCDI', 
                'tn10pETCCDI',
                'tx10pETCCDI',
                'tn90pETCCDI',
                'tx90pETCCDI',
                'dtrETCCDI')
  rd <- 1
  if (length(grep(var.name,zero.list))>0)
    rd <- 0
  if (length(grep(var.name,one.list))>0)
    rd <- 1
  
  return(rd)
}


compute.climdex.values <- function(model,ds.type,
                                   var.class,
                                   seas.fx,
                                   region,type,
                                   read.dir,proj.dir,
                                   past.int,proj.int,
                                   clip.shp) {
  gcm <- model 
  var.name <- var.class
  print(var.name)
  file.dir <- paste(read.dir,model,sep='')

  ##For Annual Files
  base.files <- list.files(path=file.dir,pattern=paste('^',var.name,sep=''),full.names=TRUE)
  past.file <- base.files[grep(past.int,base.files)]
  proj.file <- base.files[grep(proj.int,base.files)]

  past.brick <- subset(brick(past.file),1)
  past.subset <- mask(past.brick,clip.shp)
  past.avg <- cellStats(past.subset,'mean')

  proj.brick <- subset(brick(proj.file),1)
  proj.subset <- mask(proj.brick,clip.shp)
  proj.avg <- cellStats(proj.subset,'mean')

  abs.anoms <- proj.avg - past.avg
  prc.anoms <- (proj.avg - past.avg)/past.avg*100
  rv <- c(past.avg,proj.avg,abs.anoms,prc.anoms)
  return(rv)
}

format.tables <- function(mon.vals,models,rd,pctl=FALSE,var.name,region.title) {
  all.vals <- mon.vals

  vals.avg <- apply(all.vals,2,mean,na.rm=T)
  table.vals <- rbind(all.vals,vals.avg)
  if (pctl) {
    vals.10 <- apply(all.vals,2,quantile,0.1,na.rm=T)
    vals.50 <- apply(all.vals,2,quantile,0.5,na.rm=T)
    vals.90 <- apply(all.vals,2,quantile,0.9,na.rm=T)
    table.vals <- rbind(table.vals,vals.10,vals.50,vals.90)
  }
  new.table <- round(table.vals,rd)
  if (pctl) {
    new.table <- cbind(c(models,'Ens. Avg.','10th %ile','Median','90th %ile'),new.table) 
  } else {
    new.table <- cbind(c(models,'Ens. Avg.'),new.table)
  }
  new.table <- rbind(c('Model','Annual'),new.table)
  title <- c(paste('Table: CLIMDEX ',get.variable.title(var.name),' for ',region.title,sep=''),' ')
  new.table <- rbind(title,new.table)
  return(new.table)
}

make.tables <- function(model.list,ds.type,region,region.title,scenario,clip.shp,
                        past.int,proj.int,
                        proj.dir,read.dir,pctl) {
  
  ##Climdex parameters
  for (var.name in climdex.list) {
    
    var.class <- strsplit(var.name,'_')[[1]][1]
    seas.fx <- get.seas.fx(var.class)
    print(var.class)


    my.writedir <- paste(proj.dir,'tables/',region,'/',ds.type,'/',scenario,'/climdex/',var.class,'/',sep='')
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)
    
    monthly.avgs <- lapply(model.list,compute.climdex.values,ds.type,
                           var.class,
                           seas.fx=seas.fx,
                           region,type,
                           read.dir,proj.dir,
                           past.int,proj.int,
                           clip.shp)
    rd <- get.rounding.value(var.class)
    ncol <- 1
    past.yr.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[1])})),nrow=length(monthly.avgs),ncol=1,byrow=TRUE)
    past.table <- format.tables(past.yr.vals,model.list,rd,pctl,var.name,region.title)
    write.table(past.table,file=paste(my.writedir,'past.',var.class,'.',past.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    future.yr.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[2])})),nrow=length(monthly.avgs),ncol=1,byrow=TRUE)                   
    future.table <- format.tables(future.yr.vals,model.list,rd,pctl,var.name,region.title)
    write.table(future.table,file=paste(my.writedir,'future.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    abs.yr.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[3])})),nrow=length(monthly.avgs),ncol=1,byrow=TRUE)  
    abs.table <- format.tables(abs.yr.vals,model.list,1,pctl,var.name,region.title)
    write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    prc.yr.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[4])})),nrow=length(monthly.avgs),ncol=1,byrow=TRUE)                     
    prc.table <- format.tables(prc.yr.vals,model.list,1,pctl,var.name,region.title)
    write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)      
  }##var.list loop
}##make.tables function





