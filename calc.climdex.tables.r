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
                      cddmaxETCCDI='Maximum Consecutive Dry Days',
                      cdd90ETCCDI='90th % Consecutive Dry Days',
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
                 'cdd90ETCCDI',
                 'cddmaxETCCDI',
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
                                   seas.flag,
                                   seas.fx,
                                   region,
                                   read.dir,write.dir,
                                   interval,
                                   clip.shp) {
  gcm <- model 
  print(gcm)
  var.name <- var.class
  print(var.name)
  file.dir <- paste(read.dir,'/climdex/',model,sep='')
  
  ##For Annual Files
  base.files <- list.files(path=file.dir,pattern=paste('^',var.name,sep=''),full.names=TRUE)
  ann.files <- base.files[grep('annual',base.files)]
  ##print(base.files)  
  ann.file <- ann.files[grep(interval,ann.files)]
  print(ann.file)
  ann.brick <- brick(ann.file)
  ann.sub <- subset(ann.brick,1)
  ann.subset <- mask(ann.sub,clip.shp)
  ann.avg <- cellStats(ann.subset,'mean')
  rv <- ann.avg
  if (seas.flag) {
    seas.files <- base.files[grep('seasonal',base.files)]
    seas.file <- seas.files[grep(interval,seas.files)]

    ##Function to extract subset of data for moti region
    seas.brick <- brick(seas.file)
    seas.subset <- mask(seas.brick,clip.shp)
    seas.avg <- cellStats(seas.subset,'mean')

    all.values <- c(seas.avg,ann.avg)
    rv <- all.values
  } 
  return(rv)
}

format.tables <- function(mon.vals,models,rd,pctl=FALSE,var.name,seas.flag,region.title) {
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
  if (seas.flag) {
     new.table <- rbind(c('Model','Winter','Spring','Summer','Fall','Annual'),new.table)
     title <- c(paste('Table: CLIMDEX ',get.variable.title(var.name),' for ',region.title,sep=''),rep(' ',5))
  } else {
     new.table <- rbind(c('Model','Annual'),new.table)
     title <- c(paste('Table: CLIMDEX ',get.variable.title(var.name),' for ',region.title,sep=''),rep(' ',1))
  }
  new.table <- rbind(title,new.table)
  return(new.table)
}


make.climdex.tables <- function(model.list,var.name,ds.type,region,region.title,scenario,clip.shp,
                                past.int,proj.list,
                                read.dir,write.dir,pctl) {

##Not included in current round of BC-PRISM downscaling
##                  'tn10pETCCDI','tx10pETCCDI','tn90pETCCDI','tx90pETCCDI',
##                  'wsdiETCCDI','csdiETCCDI',

  seasonal.list <- c('txxETCCDI','tnxETCCDI','txnETCCDI','tnnETCCDI','dtrETCCDI',
                     'rx1dayETCCDI','rx2dayETCCDI','rx5dayETCCDI')


###  climdex.list <- c('idETCCDI','trETCCDI','fdETCCDI',
###                     'txxETCCDI','tnxETCCDI','txnETCCDI','tnnETCCDI','dtrETCCDI',
###                    'rx1dayETCCDI','rx2dayETCCDI','rx5dayETCCDI',
###                    'suETCCDI','su30ETCCDI','gslETCCDI',
###                    'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI','cddETCCDI','cwdETCCDI',
###                    'r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI',
###                    'prcptotETCCDI')
  climdex.list <- c('rx2dayETCCDI','rx5dayETCCDI',
                    'suETCCDI','su30ETCCDI','gslETCCDI',
                    'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI','cddETCCDI','cwdETCCDI',
                    'r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI',
                    'prcptotETCCDI')
##  climdex.list <- 'suETCCDI'

 
  ##Climdex parameters
##  for (var.name in climdex.list) {
    seas.flag <- var.name %in% seasonal.list    
    var.class <- strsplit(var.name,'_')[[1]][1]
    seas.fx <- get.seas.fx(var.class)
    print(var.class)

    rd <- get.rounding.value(var.class)
    if (seas.flag) {
      ncol <- 5     
    } else {
      ncol <- 1
    }

    my.writedir <- paste(write.dir,'tables/climdex/',var.class,'/',sep='')
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)

    past.values <- lapply(model.list,compute.climdex.values,ds.type,
                          var.class,
                          seas.flag=seas.flag,
                          seas.fx=seas.fx,
                          region,
                          read.dir,write.dir,
                          interval=past.int,
                          clip.shp)

    past.yr.vals <- matrix(unlist(lapply(past.values,
                           function(x) {return(x)})),nrow=length(past.values),ncol=ncol,byrow=TRUE)
    past.table <- format.tables(past.yr.vals,model.list,rd,pctl,var.name,seas.flag,region.title)
    write.table(past.table,file=paste(my.writedir,'past.',var.class,'.',scenario,'.',past.int,'.csv',sep=''),
                sep=',',quote=F,col.name=FALSE,row.name=FALSE)

    for (proj.int in proj.list) {
      proj.values <- lapply(model.list,compute.climdex.values,ds.type,
                            var.class,
                            seas.flag=seas.flag,
                            seas.fx=seas.fx,
                            region,
                            read.dir,write.dir,
                            interval=proj.int,
                            clip.shp)

      future.yr.vals <- matrix(unlist(lapply(proj.values,function(x) {return(x)})),nrow=length(proj.values),ncol=ncol,byrow=TRUE)
      future.table <- format.tables(future.yr.vals,model.list,rd,pctl,var.name,seas.flag,region.title)
      write.table(future.table,file=paste(my.writedir,'future.',var.class,'.',scenario,'.',proj.int,'.csv',sep=''),
                  sep=',',quote=F,col.name=FALSE,row.name=FALSE)

      abs.yr.anoms <- future.yr.vals - past.yr.vals
      abs.table <- format.tables(abs.yr.anoms,model.list,1,pctl,var.name,seas.flag,region.title)
      write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.class,'.',scenario,'.',proj.int,'.csv',sep=''),
                  sep=',',quote=F,col.name=FALSE,row.name=FALSE)

      prc.yr.anoms <- (future.yr.vals - past.yr.vals)/past.yr.vals*100
      prc.table <- format.tables(prc.yr.anoms,model.list,1,pctl,var.name,seas.flag,region.title)
      write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.class,'.',scenario,'.',proj.int,'.csv',sep=''),
                  sep=',',quote=F,col.name=FALSE,row.name=FALSE)      
    }##proj.list loop
##  }##var.list loop
}##make.tables function





