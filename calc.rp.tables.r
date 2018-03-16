##Script to produce tables of projected return period changes
##The output format is the same as the MOTI tables 

library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)
library(PCICt)

##Updated to include running CMIP5 GCMs

##-----------------------------------------------------------------------------------------------
get.variable.title <- function(var.name,rperiod) {
  rv <- switch(var.name,
               pr=paste('Maximum Precipitation (',rperiod,'-Year)',sep='') ,
               tasmax=paste('Maximum Temperature (',rperiod,'-Year)',sep='') ,
               tasmin=paste('Minimum Temperature (',rperiod,'-Year)',sep=''),
               rx2dayETCCDI=paste('2-Day Precipitation (',rperiod,'-Year)',sep='') ,
               rx5dayETCCDI=paste('5-Day Precipitation (',rperiod,'-Year)',sep='') ,
               snowdepth='Snowpack (m)')
  return(rv)
}


##Read in the regional time series
compute.climate.values <- function(model,ds.type,
                                   var.name,
                                   region,type,rperiod,
                                   past.int,proj.int,
                                   read.dir,proj.dir,
                                   clip.shp) {
  
  rp.name <- paste('rp.',rperiod,sep='')
    gcm <- model

    files <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=paste(var.name,'_',region,'_RP',rperiod,sep=''),full.name=TRUE)

    ##-------------------------------------------------
    past.file <- files[grep(past.int,files)]
    print(past.file)

    past.brick <- subset(brick(past.file),1)   
    past.subset <- mask(past.brick,clip.shp)
    past.avg <- cellStats(past.subset,'mean')
    print('Past')
    
    proj.file <- files[grep(proj.int,files)]
    proj.brick <- subset(brick(proj.file),1)
    print(proj.file)
    proj.subset <- mask(proj.brick,clip.shp)
    proj.avg <- cellStats(proj.subset,'mean')
    print('Proj')

    abs.anoms <- proj.avg - past.avg
    prc.anoms <- (proj.avg - past.avg)/past.avg*100
    rv <- c(past.avg,proj.avg,abs.anoms,prc.anoms)
    print(rv)
  return(rv)
}

##Correct the table formatting
format.tables <- function(mon.vals,models,rd,var.name,region.title,rperiod) {
  vect.vals <- unlist(mon.vals)
  vals.avg <- mean(vect.vals,na.rm=T)
  vals.10 <- quantile(vect.vals,0.1,na.rm=T)
  vals.50 <- quantile(vect.vals,0.5,na.rm=T)
  vals.90 <- quantile(vect.vals,0.9,na.rm=T)
  new.table <- round(c(vect.vals,vals.avg,vals.10,vals.50,vals.90),rd)
  new.table <- cbind(c(models,'Ens. Avg.','10th %ile','Median','90th %ile'),new.table)
##  new.table <- rbind(c('Models','Annual'),new.table)
  new.table <- rbind(c('Models','Annual'),new.table)
  title <- c(paste('Table: ',get.variable.title(var.name,rperiod),' for ', region.title,sep=''),rep(' ',1))
  new.table <- rbind(title,new.table)
  
  return(new.table)
}


##*********************************************************************
##*********************************************************************
make.tables <- function(var.list,model.list,
                        ds.type,region,region.title,clip.shp,type,rperiod,scenario,
                        proj.dir,read.dir,write.dir,pctl,
                        past.int,proj.int) {
  ##Climate parameters
  for (var.name in var.list) {
    monthly.avgs <- lapply(model.list,compute.climate.values,ds.type=ds.type,
                           var.name=var.name,
                           region=region,type=type,rperiod=rperiod,
                           past.int=past.int,proj.int=proj.int,
                           read.dir=read.dir,proj.dir=proj.dir,
                           clip.shp=clip.shp)

    my.writedir <- paste(write.dir,'tables/',region,'/',ds.type,'/',scenario,'/return_periods/',var.name,'/',sep='')
    #print(my.writedir)
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)

    past.mon.vals <- lapply(monthly.avgs,function(x) {return(x[1])})
    past.table <- format.tables(past.mon.vals,model.list,2,var.name,region.title,rperiod)
    write.table(past.table,file=paste(my.writedir,'past.',var.name,'.rp.',rperiod,'.',past.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)

    future.mon.vals <- lapply(monthly.avgs,function(x) {return(x[2])})    
    future.table <- format.tables(future.mon.vals,model.list,2,var.name,region.title,rperiod)
    write.table(future.table,file=paste(my.writedir,'future.',var.name,'.rp.',rperiod,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)

    abs.mon.vals <- lapply(monthly.avgs,function(x) {return(x[3])})    
    abs.table <- format.tables(abs.mon.vals,model.list,2,var.name,region.title,rperiod)
    write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.name,'.rp.',rperiod,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)

     prc.mon.vals <- lapply(monthly.avgs,function(x) {return(x[4])})    
     prc.table <- format.tables(prc.mon.vals,model.list,2,var.name,region.title,rperiod)
     write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.name,'.rp.',rperiod,'.',proj.int,'.values.csv',sep=''),
                 sep=',',quote=F,col.name=FALSE,row.name=FALSE)
  }
}





