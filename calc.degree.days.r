
library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)
library(PCICt)



##-----------------------------------------------------------------------------------------------

dd.list <- c('cdd','hdd','gdd','fdd')

get.degree.title <- function(var.name) {

  var.title <- switch(var.name,
                      ffd='Frost Free Days',
                      fdd='Freezing Degree Days',
                      cdd='Cooling Degree Days',
                      hdd='Heating Degree Days',
                      gdd='Growing Degree Days',
                      pas='Precipitation as Snow',
                      s30='Summer Days (30C)')
  return(var.title)
}

compute.degree.values <- function(model,ds.type,
                                  var.class,
                                  region,type,
                                  read.dir,proj.dir,
                                  past.int,proj.int,
                                  clip.shp) {
  gcm <- model 
  var.name <- var.class 
  print(var.name)
  file.dir <- paste(read.dir,model,sep='')
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

format.dd.tables <- function(mon.vals,models,pctl=FALSE,var.name,region.title) {

  all.vals <- mon.vals
  vals.avg <- apply(all.vals,2,mean,na.rm=T)
  table.vals <- rbind(all.vals,vals.avg)
  if (pctl) {
    vals.10 <- apply(all.vals,2,quantile,0.1,na.rm=T)
    vals.50 <- apply(all.vals,2,quantile,0.5,na.rm=T)
    vals.90 <- apply(all.vals,2,quantile,0.9,na.rm=T)
    table.vals <- rbind(table.vals,vals.10,vals.50,vals.90)
  }
  new.table <- round(table.vals)
  if (pctl) {
    new.table <- cbind(c(models,'Ens. Avg.','10th %ile','Median','90th %ile'),new.table) 
  } else {
    new.table <- cbind(c(models,'Ens. Avg.'),new.table)
  }
  new.table <- rbind(c('Model','Annual'),new.table)
  title <- c(paste('Table: ',get.degree.title(var.name),' for ',region.title,sep=''),rep(' ',1))  
  new.table <- rbind(title,new.table)

  return(new.table)
}

make.dd.tables <- function(model.list,ds.type,region,region.title,scenario,clip.shp,
                        past.int,proj.int,
                        proj.dir,read.dir,pctl) {
  
  ##Climdex parameters
  for (var.name in dd.list) {    
    var.class <- strsplit(var.name,'_')[[1]][1]
    my.writedir <- paste(proj.dir,'tables/',region,'/',ds.type,'/',scenario,'/degree_days/',var.class,'/',sep='')
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)
    
    annual.avgs <- lapply(model.list,compute.degree.values,ds.type,
                          var.class,
                          region,type,
                          read.dir,proj.dir,
                          past.int,proj.int,
                          clip.shp)
                             
    past.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[1])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)
    past.table <- format.dd.tables(past.yr.vals,model.list,pctl,var.name,region.title)
    write.table(past.table,file=paste(my.writedir,'past.',var.class,'.',past.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    future.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[2])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)                   
    future.table <- format.dd.tables(future.yr.vals,model.list,pctl,var.name,region.title)
    write.table(future.table,file=paste(my.writedir,'future.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    abs.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[3])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)  
    abs.table <- format.dd.tables(abs.yr.vals,model.list,pctl,var.name,region.title)
    write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
    prc.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[4])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)                     
    prc.table <- format.dd.tables(prc.yr.vals,model.list,pctl,var.name,region.title)
    write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.class,'.',proj.int,'.values.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)      
  }##var.list loop
}##make.tables function





