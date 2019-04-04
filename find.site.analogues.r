##Script to find a station analogue to provided tasmax, tasmin, precip
##monthly climatologies

##-----------------------------------------------------------------
##Make tas average
make_tas <- function() {
  tx.data <- read.csv('/storage/data/projects/rci/data/prism/tx_uscdn_8110.csv',header=FALSE,as.is=TRUE)
  tn.data <- read.csv('/storage/data/projects/rci/data/prism/tn_uscdn_8110.csv',header=FALSE,as.is=TRUE)

  tx.stns <- tx.data[,1]
  tn.stns <- tn.data[,1]

  tn.match <- tn.stns %in% tx.stns
  tx.match <- tx.stns %in% tn.stns

  tx.common <- tx.data[tx.match,]
  tx.order <- tx.common[order(tx.common[,1]),]
  tn.common <- tn.data[tn.match,]
  tn.order <- tn.common[order(tn.common[,1]),]

  tas <- tx.order
  tx.sub <- tx.order[,8:20]
  tx.sub[tx.sub==-9999] <- NA  
  tn.sub <- tn.order[,8:20]
  tn.sub[tn.sub==-9999] <- NA
  tas[,8:20] <- (tx.sub + tn.sub)/2

  write.table(tas,file='/storage/data/projects/rci/data/prism/ts_uscdn_8110.csv',sep=',',row.name=F,col.name=F,quote=F)
}


##-----------------------------------------------------------------
##Read in PRISM station climatologies

read_in_prism_stations <- function(var.name,read.dir) {

   prism.var <- switch(var.name,pr='ppt',tasmax='tx',tasmin='tn',tas='ts')
   stns.file <- paste0(read.dir,prism.var,'_uscdn_8110.csv')

   prism.names <- c('station_id', 'flag', 'station_name', 'elevation', 'elev flag', 'lon','lat',
                    month.abb,'Annual')
   
   stns.data <- read.csv(stns.file,header=FALSE,as.is=TRUE)
   stns.data[stns.data==-9999] <- NA
   if (grepl('(tasmax|tasmin|tas)',var.name)) {
      stns.data[,8:20] <- stns.data[,8:20]/10
   }
   names(stns.data) <- prism.names
   rd <- switch(var.name,tasmax=1,tasmin=1,pr=0,tas=1)
   winter.data <- round((stns.data$Dec+stns.data$Jan+stns.data$Feb)/3,rd)
   spring.data <- round((stns.data$Mar+stns.data$Apr+stns.data$May)/3,rd)
   summer.data <- round((stns.data$Jun+stns.data$Jul+stns.data$Aug)/3,rd)
   fall.data <- round((stns.data$Sep+stns.data$Oct+stns.data$Nov)/3,rd)
   stns.all <- cbind(stns.data[,1:19],winter.data,spring.data,summer.data,fall.data,stns.data[,20])

   all.names <- c('station_id', 'flag', 'station_name', 'elevation', 'elev flag', 'lon','lat',
                    month.abb,'Winter','Spring','Summer','Fall','Annual')
   names(stns.all) <- all.names
   return(stns.all)
}

##-----------------------------------------------------------------
##Load site climatologies - this points to a table of temperature
##or precipitation future climatologies as produced by the
##table creation script for climate assessments

get_scenarios <- function(var.name,seas,interval,scenario,type,read.dir) {

  gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
                'HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

  file.name <- paste(type,'.',var.name,'.rcp85.',interval,'.csv',sep='')
  file.scen <- as.matrix(read.csv(paste(read.dir,'/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))

  models <- file.scen[,1]
  mod.sub <- models %in% gcm.list
  data.names <- file.scen[1,]
  seas.sub <- data.names %in% seas
  data.scen <- file.scen[which(mod.sub),which(seas.sub)]
  return(data.scen)

}

##-----------------------------------------------------------------

get_data <- function(var.name,seas,interval,type,scenario,read.dir) {
  data.rcp <- get_scenarios(var.name,seas,interval,scenario,type,read.dir)
  data.comb <- as.numeric(data.rcp)
  data.stats <- c(mean(data.comb,na.rm=T,finite=T),
                  quantile(data.comb,0.1,na.rm=T,finite=T),
                  quantile(data.comb,0.9,na.rm=T,finite=T))
  names(data.stats) <- c('avg','10%','90%')
  return(data.stats)

}

get_monthly_data <- function(var.name,scenario,read.dir) {
  months <- month.abb               
  month.values <- matrix(0,nrow=12,ncol=10)
  rd <- switch(var.name,tasmax=1,tasmin=1,pr=0,tas=1)
  for (m in seq_along(months)) {
    mon <- months[m]
    vals <- c(round(get_data(var.name,mon,'1971-2000','past',scenario,read.dir)[1],rd),
              round(get_data(var.name,mon,'2011-2040','future',scenario,read.dir),rd),
              round(get_data(var.name,mon,'2041-2070','future',scenario,read.dir),rd),
              round(get_data(var.name,mon,'2071-2100','future',scenario,read.dir),rd))
    month.values[m,] <- vals
  }
  val.names <- c('past','f2020s','f2020s_10%','f2020s_90%',
                        'f2050s','f2050s_10%','f2050s_90%',
                        'f2080s','f2080s_10%','f2080s_90%')
  month.values <- as.data.frame(month.values)
  names(month.values) <- val.names
  row.names(month.values) <- months
  return(month.values)
}


get_seasonal_data <- function(var.name,scenario,read.dir) {
  seasons <- c('Winter','Spring','Summer','Fall','Annual')
  seas.values <- matrix(0,nrow=5,ncol=10)
  rd <- switch(var.name,tasmax=1,tasmin=1,pr=0,tas=1)
  for (s in seq_along(seasons)) {
    seas <- seasons[s]
    vals <- c(round(get_data(var.name,seas,'1971-2000','past',scenario,read.dir)[1],rd),
              round(get_data(var.name,seas,'2011-2040','future',scenario,read.dir),rd),
              round(get_data(var.name,seas,'2041-2070','future',scenario,read.dir),rd),
              round(get_data(var.name,seas,'2071-2100','future',scenario,read.dir),rd))
    seas.values[s,] <- vals
  }
  val.names <- c('past','f2020s','f2020s_10%','f2020s_90%',
                        'f2050s','f2050s_10%','f2050s_90%',
                        'f2080s','f2080s_10%','f2080s_90%')
  seas.values <- as.data.frame(seas.values)
  names(seas.values) <- val.names
  row.names(seas.values) <- seasons
  return(seas.values)
}


get_annual_data <- function(var.name,scenario,read.dir) {
  seas <- 'Annual'
  rd <- switch(var.name,tasmax=1,tasmin=1,pr=0,tas=1)
  vals <- c(round(get_data(var.name,seas,'1971-2000','past',scenario,read.dir)[1],rd),
            round(get_data(var.name,seas,'2011-2040','future',scenario,read.dir),rd),
            round(get_data(var.name,seas,'2041-2070','future',scenario,read.dir),rd),
            round(get_data(var.name,seas,'2071-2100','future',scenario,read.dir),rd))
  val.names <- c('past','f2020s','f2020s_10%','f2020s_90%',
                        'f2050s','f2050s_10%','f2050s_90%',
                        'f2080s','f2080s_10%','f2080s_90%')
  vals <- as.data.frame(t(vals))
  names(vals) <- val.names
  return(vals)
}

single_variable_match <- function(var.name,time,interval,
                                  prism.dir,read.dir) {

   prism.stns <- read_in_prism_stations(var.name,prism.dir) 
   site.seas <- get_seasonal_data(var.name,scenario,read.dir)
   site.mon <- get_monthly_data(var.name,scenario,read.dir)
   site.all <- rbind(site.mon,site.seas)
   site.future <- as.data.frame(t(site.all[[interval]]))
   names(site.future) <- row.names(site.all)

   prism.time <- prism.stns[[time]]
   flag <- is.na(prism.time)
   prism.stns.valid <- prism.stns[!flag,]

   site.diff <- abs(prism.stns.valid[[time]] - site.future[[time]])
   diff.order <- order(jitter(site.diff,0.05))
   stns.ordered <- prism.stns.valid[diff.order,]
   return(stns.ordered)
}


##********************************************************************************
##
##********************************************************************************

scenario <- 'rcp85'

prism.dir <- '/storage/data/projects/rci/data/prism/'

readloc <- 'northeast/fort_st_john'
site <- 'fort_st_john'
read.dir <- paste('/storage/data/projects/rci/data/assessments/',readloc,'/tables',sep='')
write.dir <- paste('/storage/data/projects/rci/data/assessments/',readloc,'/analogue_stations/',sep='')

tx.prism.stns <- read_in_prism_stations('tasmax',prism.dir) 
tn.prism.stns <- read_in_prism_stations('tasmin',prism.dir) 
ppt.prism.stns <- read_in_prism_stations('pr',prism.dir) 

site.seas.tx <- get_seasonal_data('tasmax',scenario,read.dir)
site.mon.tx <- get_monthly_data('tasmax',scenario,read.dir)
site.tasmax <- rbind(site.mon.tx,site.seas.tx)
 

tx.prism.ann <- tx.prism.stns$Annual
flag <- is.na(tx.prism.ann)
tx.prism.ann.valid <- tx.prism.stns[!flag,]

site.future <- as.data.frame(t(site.tasmax$f2050s))
names(site.future) <- row.names(site.tasmax)
tx.diff <- abs(tx.prism.ann.valid$Annual - site.future$Annual)
tx.diff.rank <- order(jitter(tx.diff,0.05))

tx.stns.ranked <- tx.prism.ann.valid[tx.diff.rank,]

##seasons <- c('Winter','Spring','Summer','Fall','Annual')
seasons <- month.abb
var.list <- 'tas' ##c('pr','tasmax','tasmin')
intervals <- c('f2020s','f2050s','f2080s')

for (var.name in var.list) {
   print(var.name)
   for (season in seasons) {
      print(season)
      for (interval in intervals) {
         print(interval)
         write.file <- paste0(write.dir,site,'_',substr(interval,2,6),'_',season,'_',var.name,'_ordered_analogues.csv')
         analogues <- single_variable_match(var.name,season,interval,prism.dir,read.dir)         
         write.table(analogues,file=write.file,
          row.name=FALSE,col.name=TRUE,sep=',',quote=FALSE)
      }
   }
}              

tasmin.winter.2050s <- single_variable_match('tasmin','Winter','f2050s',
                                  prism.dir,read.dir)

tasmax.jan.2050s <- single_variable_match('tasmax','Jan','f2050s',
                                             prism.dir,read.dir)


##write.table(tasmin.winter.2050s,file=paste0(write.dir,site,'_2050s_Winter_tasmin_ordered_analogues.csv'),
##          row.name=FALSE,col.name=TRUE,sep=',',quote=FALSE)