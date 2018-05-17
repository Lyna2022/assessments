##Script to produce tables of projected temperature and precipitation changes
##The output format is the same as the MOTI tables 

library(raster)
library(ncdf4)
library(PCICt)
library(rgdal)

##-----------------------------------------------------------------------------------------------

get.rounding.value <- function(var.name) {
  rd <- 1
  if (var.name=='pr')
    rd <- 0
  if (var.name=='snowdepth')
    rd <- 2      
  return(rd)
}


get.variable.title <- function(var.name) {
  rv <- switch(var.name,
               pr='Total Precipitation',
               tasmax='Maximum Temperature',
               tasmin='Minimum Temperature',
               tas='Average Temperature',
               snowdepth='Average Snowpack (m)')
  return(rv)
}

##Read in the regional time series
compute.climate.values <- function(model,ds.type,
                                   var.name,
                                   region,type,
                                   read.dir,proj.dir,
                                   clip.shp,
                                   past.int,proj.int,
                                   lonc,latc) {


  ##GCM and Regional averages grouping monthly and seasonal values - needs pre-computed seasonal and monthly files
    gcm <- model
    print(gcm)
    seas.files <- list.files(path=paste(read.dir,'seasonal/',gcm,'/',sep=''),pattern=var.name,full.name=TRUE)
    mon.files <- list.files(path=paste(read.dir,'monthly/',gcm,'/',sep=''),pattern=var.name,full.name=TRUE)
    ann.files <- list.files(path=paste(read.dir,'annual/',gcm,'/',sep=''),pattern=var.name,full.name=TRUE)

    ##-------------------------------------------------
    past.seas.file <- seas.files[grep(past.int,seas.files)]
    past.mon.file <- mon.files[grep(past.int,mon.files)]
    past.ann.file <- ann.files[grep(past.int,ann.files)]

    run.ix <- grep('r*i1p1',strsplit(past.seas.file,'_')[[1]])
    run <- strsplit(past.seas.file,'_')[[1]][run.ix]
    
    proj.seas.file <- seas.files[grep(proj.int,seas.files)]
    proj.mon.file <- mon.files[grep(proj.int,mon.files)]
    proj.ann.file <- ann.files[grep(proj.int,ann.files)]    

    ##Function to extract subset of data for moti region
    past.seas.brick <- brick(past.seas.file)
    past.seas.subset <- mask(past.seas.brick,clip.shp)
    past.seas.avg <- cellStats(past.seas.subset,'mean')
    print('Seasonal')

    past.mon.brick <- brick(past.mon.file)
    past.mon.subset <- mask(past.mon.brick,clip.shp)
    past.mon.avg <- cellStats(past.mon.subset,'mean')
    print('Monthly')

    past.ann.brick <- brick(past.ann.file)
    past.ann.sub <- subset(past.ann.brick,1)
    past.ann.subset <- mask(past.ann.sub,clip.shp)
    past.ann.avg <- cellStats(past.ann.subset,'mean')
    print('Annual')

    ##Function to extract subset of data for moti region
    proj.seas.brick <- brick(proj.seas.file)
    proj.seas.subset <- mask(proj.seas.brick,clip.shp)
    proj.seas.avg <- cellStats(proj.seas.subset,'mean')
    print('Seasonal')

    proj.mon.brick <- brick(proj.mon.file)
    proj.mon.subset <- mask(proj.mon.brick,clip.shp)
    proj.mon.avg <- cellStats(proj.mon.subset,'mean')
    print('Monthly')

    proj.ann.brick <- brick(proj.ann.file)
    proj.ann.sub <- subset(proj.ann.brick,1)
    proj.ann.subset <- mask(proj.ann.sub,clip.shp)
    proj.ann.avg <- cellStats(proj.ann.subset,'mean')
    print('Annual')

    ##Function to extract subset of data for moti region
    past.values <- c(past.mon.avg,past.seas.avg,past.ann.avg)
    proj.values <- c(proj.mon.avg,proj.seas.avg,proj.ann.avg)

    abs.anoms <- proj.values - past.values
    prc.anoms <- (proj.values - past.values)/past.values*100    
  rv <- rbind(past.values,proj.values,abs.anoms,prc.anoms)
  print(rv)

  return(rv)
}

##Correct the table formatting
format.tables <- function(mon.vals,models,rd,pctl=FALSE,var.name,region.title) {
  vals.avg <- apply(mon.vals,2,mean,na.rm=T)
  table.vals <- rbind(mon.vals,vals.avg)
  if (pctl) {
    vals.10 <- apply(mon.vals,2,quantile,0.1,na.rm=T)
    vals.50 <- apply(mon.vals,2,quantile,0.5,na.rm=T)
    vals.90 <- apply(mon.vals,2,quantile,0.9,na.rm=T)
    table.vals <- rbind(table.vals,vals.10,vals.50,vals.90)
  }
  new.table <- round(table.vals,rd)
  if (pctl) {
    new.table <- cbind(c(models,'Ens. Avg.','10th %ile','Median','90th %ile'),new.table)
  } else {
    new.table <- cbind(c(models,'Ens. Avg.'),new.table)
  }
  if (var.name=='snowdepth') {

    new.table <- rbind(c('Model','March1','April1','Winter','Spring','Summer','Fall','Annual'),new.table)
    title <- c(paste('Table: ',get.variable.title(var.name),' for ',region.title,sep=''),rep(' ',7))
  } else {
    new.table <- rbind(c('Model',month.abb,'Winter','Spring','Summer','Fall','Annual'),new.table) 
    title <- c(paste('Table: ',get.variable.title(var.name),' for ',region.title,sep=''),rep(' ',17))
  }

  new.table <- rbind(title,new.table)
  return(new.table)
}

##*********************************************************************
##*********************************************************************

make.tables <- function(var.list,model.list,
                        ds.type,region,region.title,clip.shp,type,scenario,
                        proj.dir,read.dir,write.dir,
                        past.int,proj.int,pctl,lonc,latc) {
  
  ##Climate parameters
  for (var.name in var.list) {
    print('------------------------------')
    print(var.name)    

    monthly.avgs <- lapply(model.list,compute.climate.values,ds.type=ds.type,
                           var.name=var.name,
                           region=region,type=type,
                           read.dir=read.dir,proj.dir=proj.dir,
                           clip.shp=clip.shp,past.int=past.int,proj.int=proj.int,
                           lonc=lonc,latc=latc)

    rd <- get.rounding.value(var.name)    
    col.val <- switch(var.name,
                      pr=17,
                      tasmax=17,
                      tasmin=17,
                      tas=17,
                      snowdepth=7)
    
    my.writedir <- paste(write.dir,'tables/',region,'/',ds.type,'/',scenario,'/',var.name,'/',sep='')
    
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)

    past.mon.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[1,])})),nrow=length(monthly.avgs),ncol=col.val,byrow=TRUE)    
    past.table <- format.tables(past.mon.vals,model.list,rd,pctl,var.name,region.title)
    write.table(past.table,file=paste(my.writedir,'past.',var.name,'.',past.int,'.values.csv',sep=''),sep=',',quote=FALSE,col.name=FALSE,row.name=FALSE)

    if (type != 'obs') {
      future.mon.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[2,])})),nrow=length(monthly.avgs),ncol=col.val,byrow=TRUE)
      future.table <- format.tables(future.mon.vals,model.list,rd,pctl,var.name,region.title)
      write.table(future.table,file=paste(my.writedir,'future.',var.name,'.',proj.int,'.values.csv',sep=''),sep=',',quote=FALSE,col.name=FALSE,row.name=FALSE)
      
      abs.mon.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[3,])})),nrow=length(monthly.avgs),ncol=col.val,byrow=TRUE)
      abs.table <- format.tables(abs.mon.vals,model.list,rd,pctl,var.name,region.title)
      write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.name,'.',proj.int,'.values.csv',sep=''),sep=',',quote=FALSE,col.name=FALSE,row.name=FALSE)
      
##      if (var.name=='pr'|var.name=='snowdepth') {
        prc.mon.vals <- matrix(unlist(lapply(monthly.avgs,function(x) {return(x[4,])})),nrow=length(monthly.avgs),ncol=col.val,byrow=TRUE)
        prc.table <- format.tables(prc.mon.vals,model.list,1,pctl,var.name,region.title)
        write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.name,'.',proj.int,'.values.csv',sep=''),
                    sep=',',quote=FALSE,col.name=FALSE,row.name=FALSE)
##      }
    }

  }
}





