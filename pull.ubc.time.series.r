##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/home/ssobie/code/repos/building_code/gcm.bccaq.build.code.data.r',chdir=T)


extract.series <- function(read.dir,gcm,scenario,var.name,lon.ix,lat.ix) {

    file.names <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_gcm_prism'),full.name=TRUE) 
    ##file.names <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_day'),full.name=TRUE) 
    scen.files <- file.names[grep(scenario,file.names)]
    past.file <- scen.files[grep('1951-2000',scen.files)]
    print(past.file)   
    proj.file <- scen.files[grep('2001-2100',scen.files)]
    print(proj.file)

    past.nc <- nc_open(past.file)
    proj.nc <- nc_open(proj.file)
    
    data.past <- ncvar_get(past.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.proj <- ncvar_get(proj.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.sub <- round(c(data.past,data.proj),1)

    past.time <- netcdf.calendar(past.nc)
    proj.time <- netcdf.calendar(proj.nc)
    full.time <- as.character(c(past.time,proj.time))
    nc_close(past.nc)
    nc_close(proj.nc)
    rv <- list(time=full.time,
                 data=data.sub)

    return(rv)    
}


bccaq.correct.for.dates <- function(tx,tn,pr,series.dates,gcm) {

   full.dates <- seq(from=as.Date('1951-01-01'),by='day',to=as.Date('2100-12-31'))                      
   fd.leap.flag <- grep('02-29',full.dates)           
   noleap.dates <- full.dates[-fd.leap.flag]
 
   ##If Hadley, fill in the 5 missing days per year
   if (grepl('HadGEM2',gcm)) {
      print('Hadley 360 day garbage')
      ##150 years by 365 days - standard vector length
      had.len <- 150*365
      sv <- rep(NaN,had.len)               
      mat.template <- cbind(as.character(noleap.dates),sv,sv,sv)
      hadley.ix <- seq(73,54750,73)   
      hadley.flag <- (1:54750) %in% hadley.ix
      hadley.fill <- rep(NaN,360) ##To fill in 2100
      mat.template[!hadley.flag,2] <- c(tx,hadley.fill)
      mat.template[!hadley.flag,3] <- c(tn,hadley.fill)
      mat.template[!hadley.flag,4] <- c(pr,hadley.fill)
      rv <- rbind(c('DATE','TASMAX','TASMIN','PR'),
                   mat.template)
   } else {
          
      data.matrix <- rbind(c('DATE','TASMAX','TASMIN','PR'), 
                        cbind(as.character(series.dates),tx,tn,pr))
      print(dim(data.matrix))                        
      ##Strip out the leap days if they exist
      data.noleap <- data.matrix   
      leap.flag <- grep('02-29',series.dates)           
      if (length(leap.flag)!=0) {
         print(paste0(gcm,' is Gregorian'))
         rv <- data.matrix[-leap.flag,]      
         } else {
      rv <- data.matrix
      }                       
   }   

   print(dim(rv))
   return(rv)
}

gcm.correct.for.dates <- function(input.matrix,gcm) {

   series.dates <- input.matrix[-1,1]                      
   full.dates <- seq(from=as.Date('1951-01-01'),by='day',to=as.Date('2099-12-31'))                      
   fd.leap.flag <- grep('02-29',full.dates)           
   noleap.dates <- full.dates[-fd.leap.flag]
 
   ##If Hadley, fill in the 5 missing days per year
   if (grepl('HadGEM2',gcm)) {
                          
      print('Hadley 360 day garbage')
      ##150 years by 365 days - standard vector length
      had.len <- 149*365
      sv <- rep(NaN,had.len)               
##Change the column length to match the number of variables     
      mat.template <- cbind(as.character(noleap.dates),sv,sv,sv) ##3 GCM variables
      hadley.ix <- seq(73,had.len,73)   
      hadley.flag <- (1:had.len) %in% hadley.ix
      ##Add in the missing December 2005 points
      dec.05 <- grep('2005-12-*',noleap.dates)
      hadley.flag[dec.05] <- TRUE

##Change the column length to match the number of variables     

      mat.template[!hadley.flag,2:4] <- input.matrix[-1,2:4]

##
      rv <- rbind(input.matrix[1,],
                   mat.template)

   } else {
          
      print(dim(input.matrix))                        
      ##Strip out the leap days if they exist
      data.noleap <- input.matrix   
      leap.flag <- grep('02-29',series.dates)           

      if (length(leap.flag)!=0) {
         print(paste0(gcm,' is Gregorian'))
         rv <- input.matrix[-leap.flag,]      
         } else {
      rv <- input.matrix
      }                       
   }   

   print(dim(rv))

   return(rv)
}

read.dir <- '/storage/data/climate/downscale/BCCAQ2/high_res_downscaling/bccaq_gcm_van_whistler_subset/'
write.dir <- '/storage/data/projects/rci/data/assessments/ubc/gcm/'

coords <- c(-123.252562,49.262532)
location <- 'UBC_EOS'
var.names <- c('pr','tasmax','tasmin')
scenario <- 'rcp85'

gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'HadGEM2-ES',
              'inmcm4',
              'MIROC5',
              'MRI-CGCM3')

lon.bnds <- c(-122.81,
              -122.30,
              -123.03,
              -122.80,
              -122.46,
              -122.78,
              -122.80,
              -123.00,
              -123.00,
              -123.17)

lat.bnds <- c(49.38,
              0,
              0,
              0,
              0,
              0,
              0,
              49.50,
              0,
              49.34)
ubc.dist <- rep(0,length(gcm.list))

for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    if (1==1) {
    ##file.names <- list.files(path=paste0(read.dir,gcm),pattern='pr_gcm_prism',full.name=TRUE)
    file.names <- list.files(path=paste0(read.dir,gcm),pattern='pr_day',full.name=TRUE)
    scen.files <- file.names[grep(scenario,file.names)]
    past.file <- file.names[grep('1951-2000',scen.files)]

    past.nc <- nc_open(past.file)
    lon <- ncvar_get(past.nc,'lon')
    lon.sub <- lon[lon > lon.bnds[i]]
    lon.ix <- which.min(abs(lon-coords[1]))
    print('Lon')
    print(lon[lon.ix])
    lon.near <- which.min(abs(lon.sub-coords[1]))
    print(lon.sub[lon.near])
    lat <- ncvar_get(past.nc,'lat')
    lat.sub <- lat[lat > lat.bnds[i]]
    lat.ix <- which.min(abs(lat-coords[2]))
    print('Lat')
    print(lat[lat.ix])
    lat.near <- which.min(abs(lat.sub-coords[2]))
    print(lat.sub[lat.near])
    ubc.dist[i] <- sqrt( (111.3 *( lon[lon.ix] - lon.sub[lon.near]))^2 + ( 78* (lat[lat.ix] - lat.sub[lat.near]))^2)
    print('Distances')

    nc_close(past.nc)

    if (1==0) {
    print('Precip')    
    pr.data <- extract.series(read.dir,gcm,scenario,'pr',lon.ix,lat.ix)
    print('TASMAX')
    tasmax.data <- extract.series(read.dir,gcm,scenario,'tasmax',lon.ix,lat.ix)
    print('TASMIN')
    tasmin.data <- extract.series(read.dir,gcm,scenario,'tasmin',lon.ix,lat.ix)
    print(sum(tasmax.data$data < tasmin.data$data))
    
    ##Fill in missing dates
    series.dates <- as.Date(tasmax.data$time)
    
    tx.data <- tasmax.data$data
    tn.data <- tasmin.data$data
    pr.data <- pr.data$data
    
    output.matrix <- bccaq.correct.for.dates(tx.data,tn.data,pr.data,series.dates,gcm)

    write.file <- paste0(write.dir,gcm,'_BCCAQ-PRISM_800m_TASMAX_TASMIN_PR_',location,'_1951-2100.csv')
    ##write.file <- paste0(write.dir,gcm,'_BCCAQ_10km_TASMAX_TASMIN_PR_',location,'_1951-2100.csv')
    write.table(output.matrix,file=write.file,quote=F,row.names=F,col.names=F,sep=',')
    }
    }
    if (1==0) {
        other.vars <- gather.gcm.data(gcm,coords[1],coords[2],scenario)
        dates <- as.character(other.vars$rhs$time)
        ##gcm.matrix <- rbind(c('DATE','HUSS','PSL','UAS','VAS'),
          gcm.matrix <- rbind(c('DATE','RHS','RSDS','CLT'), 
                            cbind(dates,
                              round(other.vars$rhs$data,5),
                              round(other.vars$rsds$data,2),
                              round(other.vars$clt$data,2)))
        output.matrix <- gcm.correct.for.dates(gcm.matrix,gcm)

        write.file <- paste0(write.dir,gcm,'_GCM_RHS_RSDS_CLT_',location,'_1951-2100.csv')   
        write.table(gcm.matrix,file=write.file,quote=F,row.names=F,col.names=F,sep=',')
        print('Written')
    }
}

