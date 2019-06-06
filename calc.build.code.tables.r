
library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)
library(PCICt)


##-----------------------------------------------------------------------------------------------

get.build.title <- function(var.sub) {

  var.title <- switch(var.sub,
              maximum_annual='Maximum Annual Total Precipitation',
              minimum_annual='Minimum Annual Total Precipitation',
              standard_deviation='Standard Deviation of Total Precipitation',
              annual_quantile_975='Warm Month Design Temperature 97.5%',
              annual_quantile_990='Warm Month Design Temperature 99.0%',
              annual_quantile_996='Warm Month Design Temperature 99.6%',
              annual_quantile_004='Cold Month Design Temperature 0.4%',
              annual_quantile_010='Cold Month Design Temperature 1.0%',
              annual_quantile_025='Cold Month Design Temperature 2.5%')

  return(var.title)
}

compute.build.code.values <- function(model,ds.type,
                                  var.name,
                                  var.sub,
                                  region,type,
                                  read.dir,
                                  past.int,proj.int,
                                  clip.shp) {
  gcm <- model 
  print(var.name)
  suffix <- switch(var.name,
                   pr='annual',tasmax='annual_quantiles',tasmin='annual_quantiles')
                   
  file.dir <- paste0(read.dir,suffix,'/',model)

  all.files <- list.files(path=file.dir,pattern=paste('^',var.name,sep=''),full.names=TRUE)
  base.files <- all.files[grep(var.sub,all.files)]
  print(var.name)
  print(file.dir)
  print(base.files)
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

format.bc.tables <- function(mon.vals,models,pctl=FALSE,var.name,var.sub,region.title) {

  all.vals <- mon.vals
  vals.avg <- apply(all.vals,2,mean,na.rm=T)
  table.vals <- rbind(all.vals,vals.avg)
  if (pctl) {
    vals.10 <- apply(all.vals,2,quantile,0.1,na.rm=T)
    vals.50 <- apply(all.vals,2,quantile,0.5,na.rm=T)
    vals.90 <- apply(all.vals,2,quantile,0.9,na.rm=T)
    table.vals <- rbind(table.vals,vals.10,vals.50,vals.90)
  }
  new.table <- round(table.vals,1)
  if (pctl) {
    new.table <- cbind(c(models,'Ens_Avg.','10th_%ile','Median','90th_%ile'),new.table) 
  } else {
    new.table <- cbind(c(models,'Ens_Avg.'),new.table)
  }
  new.table <- rbind(c('Model','Annual'),new.table)
  title <- c(paste('Table: ',get.build.title(var.sub),' for ',region.title,sep=''),rep(' ',1))  
  new.table <- rbind(title,new.table)

  return(new.table)
}

make.build.tables <- function(model.list,
                           ds.type,region,region.title,scenario,clip.shp,
                           past.int,proj.int,
                           read.dir,write.dir,pctl) {
  
  ##Degree Day parameters
  var.list <- c('pr','tasmax','tasmin')
  pr.list <- c('maximum','minimum','standard_deviation')
  tx.list <- c('annual_quantile_975','annual_quantile_990','annual_quantile_996')
  tn.list <- c('annual_quantile_004','annual_quantile_010','annual_quantile_025')

  for (var.name in var.list) {    
    var.subs <- switch(var.name,
                        pr=pr.list,tasmax=tx.list,tasmin=tn.list)
    my.writedir <- paste0(write.dir,'tables/build_code/',var.name,'/')
    if (!file.exists(my.writedir))
      dir.create(my.writedir,recursive=TRUE)
    
    for (var.sub in var.subs) {    
      annual.avgs <- lapply(model.list,compute.build.code.values,ds.type,
                            var.name,
                            var.sub,
                            region,type,
                            read.dir,
                            past.int,proj.int,
                            clip.shp)

      past.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[1])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)
      past.table <- format.bc.tables(past.yr.vals,model.list,pctl,var.name,var.sub,region.title)
      write.table(past.table,file=paste(my.writedir,'past.',var.name,'.',var.sub,'.',scenario,'.',past.int,'.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
      future.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[2])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)
      future.table <- format.bc.tables(future.yr.vals,model.list,pctl,var.name,var.sub,region.title)
      write.table(future.table,file=paste(my.writedir,'future.',var.name,'.',var.sub,'.',scenario,'.',proj.int,'.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
      abs.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[3])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)  
      abs.table <- format.bc.tables(abs.yr.vals,model.list,pctl,var.name,var.sub,region.title)
      write.table(abs.table,file=paste(my.writedir,'abs.anomalies.',var.name,'.',var.sub,'.',scenario,'.',proj.int,'.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)
    
      prc.yr.vals <- matrix(unlist(lapply(annual.avgs,function(x) {return(x[4])})),nrow=length(annual.avgs),ncol=1,byrow=TRUE)                       
      prc.table <- format.bc.tables(prc.yr.vals,model.list,pctl,var.name,var.sub,region.title)
      write.table(prc.table,file=paste(my.writedir,'percent.anomalies.',var.name,'.',var.sub,'.',scenario,'.',proj.int,'.csv',sep=''),sep=',',quote=F,col.name=FALSE,row.name=FALSE)      
    }##var.sub loop  
  }##var.list loop
}##make.tables function





