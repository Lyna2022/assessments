##Script to sort out the data requested for Okanagan

 
get.var.units <- function(var.name) {

  leg.label <- NA
  if (grepl("(tas|txx|tnn|tmax|tmin|dtr)", var.name))
    leg.label <- 'degC'
  if (grepl("(pr|rx|r9|RP|rp)", var.name))
    leg.label <- 'mm'
  if (grepl("(pas|snowdepth)", var.name))
    leg.label <- 'cm'
  if (grepl("(tx90|tn10)", var.name))
    leg.label <- '%'
  if (grepl("(dd)", var.name))
    leg.label <- 'Degree days'
  if (grepl("(fdE|cddE|cwd|su|gsl|id|trE|s30|wsdi|csdi)", var.name))
    leg.label <- 'days'
  if (grepl("(dtr)", var.name))
    leg.label <- 'degC'
  if (grepl("(r95sep|r95dist)", var.name))
    leg.label <- 'days'
  
  return(leg.label)
} 

get.round.val <- function(var.name) {
  rd <- 0
  if (grepl("(dd)", var.name))
    rd <- 0    
  if (grepl("(tas|txx|tnn|tmax|tmin|trE|cddE|cwdE|idE|dtrE|wsdiE|csdiE|r95sep)", var.name))
    rd <- 1
  if (grepl("(pr|rx|r9|RP|rp|tx90|tn10)", var.name))
    rd <- 0
  if (grepl("(pas|snowdepth)", var.name))
    rd <- 0
  return(rd)
} 

get.scenarios <- function(var.name,seas,interval,scenario,type,rp=NULL) {
  
  file.name <- paste(type,'.',var.name,'.rcp85.',interval,'.csv',sep='')
  
  if (!is.null(rp)) {
    if (type!='ratio') {
    file.name <- paste(type,'.',var.name,'.rcp85.rp.',rp,'.',interval,'.csv',sep='')
    file.scen <- as.matrix(read.csv(paste(read.dir,'/return_periods/',var.name,'/',file.name,sep=''),
                                    header=TRUE,as.is=TRUE))
    } else {
        file.name <- paste('future.',var.name,'.rcp85.new.rp.',rp,'.interval.',interval,'.csv',sep='')
        file.scen <- as.matrix(read.csv(paste(read.dir,'/return_periods/',var.name,'/',file.name,sep=''),
                                    header=TRUE,as.is=TRUE))       
    }

  } else if (grepl('ETCCDI',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/climdex/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else if (grepl('(^cdd$|hdd|gdd|ffd|pas|s30)',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/degree_days/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else if (grepl('(maximum|minimum|quantile|standard_deviation)',var.name)) {
    var.sub <- strsplit(var.name,'\\.')[[1]][1]
    file.scen <- as.matrix(read.csv(paste(read.dir,'/build_code/',var.sub,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  }

  models <- file.scen[,1]
  mod.sub <- models %in% gcm.list
  data.names <- file.scen[1,]
  seas.sub <- data.names %in% seas
  data.scen <- file.scen[which(mod.sub),which(seas.sub)]
  return(data.scen)
}
 
get.data <- function(var.name,seas,interval,type,scenario,rp=NULL) {
  data.rcp <- get.scenarios(var.name,seas,interval,scenario,type,rp)
#  data.rcp26 <- get.scenarios(var.name,seas,interval,'rcp26',type)  
#  data.rcp45 <- get.scenarios(var.name,seas,interval,'rcp45',type)
#  data.rcp85 <- get.scenarios(var.name,seas,interval,'rcp85',type)
  data.comb <- as.numeric(data.rcp) ##as.numeric(c(data.rcp26,data.rcp45,data.rcp85))##
  if (var.name=='snowdepth' & type!='percent.anomalies')
    data.comb <- data.comb*100
  data.stats <- c(mean(data.comb,na.rm=T,finite=T),
                  quantile(data.comb,0.1,na.rm=T,finite=T),
                  quantile(data.comb,0.9,na.rm=T,finite=T))
  if (type=='ratio') {
    data.stats <- c(median(data.comb,na.rm=T),
                    quantile(data.comb,0.1,na.rm=T,finite=T),
                    quantile(data.comb,0.9,na.rm=T,finite=T))
  }

  names(data.stats) <- c('avg','10%','90%')
  return(data.stats)
}
 
get.seasonal.data <- function(var.name,scenario) {
  seasons <- c('Winter','Spring','Summer','Fall','Annual')
  pr.vals <- matrix(0,nrow=5,ncol=13)
  if (var.name=='snowdepth') {
    seasons <- c('March1','April1','Winter','Spring','Summer','Fall','Annual')
    pr.vals <- matrix(0,nrow=7,ncol=13)
  }
  rd <- get.round.val(var.name)
  for (s in seq_along(seasons)) {
    seas <- seasons[s]
    vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario)[1],rd),
              round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario),rd),              
              round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario),1),
              round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario),1),
              round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario),1))
    lower <- vals[names(vals)=='10%']
    upper <- vals[names(vals)=='90%']
    brackets <- paste('(',paste(lower,upper,sep=' to '),')',sep='')
    avgs <- vals[names(vals)=='avg']
    proj <- avgs[-1]
    len  <- length(c(proj,brackets))
    result <- rep(0,length=len)
    result[seq(1,len,2)] <- proj
    result[seq(2,len,2)] <- brackets
    result <- c(avgs[1],result)
    
    pr.vals[s,] <- result
  }
  pr.data <- cbind(seasons,pr.vals)
  
  return(pr.data)
}


get.annual.data <- function(var.name,scenario,rp) {
  seas <- 'Annual'
  rd <- get.round.val(var.name)
  vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario,rp)[1],rd),
            round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario,rp),rd),              
            round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario,rp),1))
  lower <- vals[names(vals)=='10%']
  upper <- vals[names(vals)=='90%']
  brackets <- paste('(',paste(lower,upper,sep=' to '),')',sep='')
  avgs <- vals[names(vals)=='avg']
  proj <- avgs[-1]
  len  <- length(c(proj,brackets))
  result <- rep(0,length=len)
  result[seq(1,len,2)] <- proj
  result[seq(2,len,2)] <- brackets
  result <- c('Annual',avgs[1],result)
  return(result)
}


get.ratio.data <- function(var.name,scenario,rp) {
  seas <- 'Annual'
  rd <- 1
  vals <- c(rp,
            round(get.data(var.name,seas,'2011-2040','ratio',scenario,rp),rd),
            round(get.data(var.name,seas,'2041-2070','ratio',scenario,rp),rd),
            round(get.data(var.name,seas,'2071-2100','ratio',scenario,rp),rd),              
            round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario,rp),1),
            round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario,rp),1))
  lower <- vals[names(vals)=='10%']
  upper <- vals[names(vals)=='90%']
  brackets <- paste('(',paste(lower,upper,sep=' to '),')',sep='')
  avgs <- vals[names(vals)=='avg']
  
  len  <- length(c(avgs,brackets))
  result <- rep(0,length=len)
  result[seq(1,len,2)] <- avgs
  result[seq(2,len,2)] <- brackets
  result <- c('Annual',vals[1],result)
  
  return(result)
}

seasonal.table <- function(var.name,scenario) {

  no.percent <- '(tasmax|tasmin|txxETCCDI|tnnETCCD|trETCCDI|suETCCDI|su30ETCCDI)'
  var.units <- get.var.units(var.name)
  
  data.top <- c(toupper(var.name),paste('Past (',var.units,')',sep=''),
                paste('2020s Change (',var.units,')',sep=''),'',
                paste('2050s Change (',var.units,')',sep=''),'',
                paste('2080s Change (',var.units,')',sep=''),'',
                paste('2020s Percent Change (%)',sep=''),'',
                paste('2050s Percent Change (%)',sep=''),'',
                paste('2080s Percent Change (%)',sep=''),'')
  
  data.bottom <-c('Season','','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')
  data.header <- rbind(data.top,data.bottom)
  
  var.result <- rbind(data.header,get.seasonal.data(var.name,scenario))

  if (grepl(no.percent,var.name))
    var.result[3:7,9:14] <- NA
  return(var.result)
}


annual.table <- function(var.name,scenario,rp=NULL) {
  
  no.percent <- '(tasmax|tasmin|txxETCCDI|tnnETCCD|trETCCDI|suETCCDI|s30|r95sep|r99days|r95days)'
  var.units <- get.var.units(var.name)
  var.label <- var.name
  if (!is.null(rp))
    var.label <- paste('RP',rp,' ',var.name,sep='')
  data.top <- c(toupper(var.label),paste('Past (',var.units,')',sep=''),
                paste('2020s Change (',var.units,')',sep=''),'',
                paste('2050s Change (',var.units,')',sep=''),'',
                paste('2080s Change (',var.units,')',sep=''),'',
                paste('2020s Percent Change (%)',sep=''),'',
                paste('2050s Percent Change (%)',sep=''),'',
                paste('2080s Percent Change (%)',sep=''),'')
  
  data.bottom <-c('Annual','','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')
  data.header <- rbind(data.top,data.bottom)
  result <- get.annual.data(var.name,scenario,rp)
  var.result <- rbind(data.header,result)
  if (grepl(no.percent,var.name))
    var.result[3,9:14] <- NA
  return(var.result)
}

ratio.table <- function(var.name,scenario,rp) {
  
  var.units <- get.var.units(var.name)
  var.label <- var.name
  var.label <- paste('RP',rp,' ',var.name,' Interval',sep='')
  data.top <- c(toupper(var.label),paste('Past Interval (Year)',sep=''),
                paste('2020s Interval (Years)',sep=''),'',
                paste('2050s Interval (Years)',sep=''),'',
                paste('2080s Interval (Years)',sep=''),'',
                paste('2020s Percent Change (%)',sep=''),'',
                paste('2050s Percent Change (%)',sep=''),'',
                paste('2080s Percent Change (%)',sep=''),'')  
  data.bottom <-c('Annual','','Median','10th%-90th%','Median','10th%-90th%','Median','10th%-90th%',
                  'Median','10th%-90th%','Median','10th%-90th%','Median','10th%-90th%')
  data.header <- rbind(data.top,data.bottom)
  result <- get.ratio.data(var.name,scenario,rp)
  var.result <- rbind(data.header,result)
  var.result[3,9:14] <- NA

  return(var.result)
}


##---------------------------------------------------------

reg.list <- list(c("bella_hospital_site","van_coastal_health/bella_hospital_site"),
                 c("richmond","van_coastal_health/richmond"),
                 c("lionsgate_hospital_site","van_coastal_health/lionsgate_hospital_site"),
                 c("royal_columbian_hospital_site","van_coastal_health/royal_columbian_hospital_site"),
                 c("vancouver_general_hospital_site","van_coastal_health/vancouver_general_hospital_site"),
                 c("bella_health","van_coastal_health/bella_health"),
                 c("lionsgate_hospital","van_coastal_health/lionsgate_hospital"),
                 c("van_coastal_health","van_coastal_health/van_coastal_health"))

reg.list <- list(c("richmond_hospital_site","van_coastal_health/richmond_hospital_site"),       
                 c("powell_river_hospital_site","van_coastal_health/powell_river_hospital_site"),       
                 c("sechelt_hospital_site","van_coastal_health/sechelt_hospital_site"),       
                 c("squamish_hospital_site","van_coastal_health/squamish_hospital_site"),       
                 c("ubc_hospital_site","van_coastal_health/ubc_hospital_site"))       


reg.list <- list(c("east_willow_road","willow_road/east_willow_road"),                
                 c("west_willow_road","willow_road/west_willow_road"))
reg.list <- list(c('north_rockies_muni','northeast/north_rockies_muni'))
reg.list <- list(c('downtown_community_health_centre','van_coastal_health/downtown_community_health_centre'),
                 c('george_pearson_centre','van_coastal_health/george_pearson_centre'),
                 c('bella_coola_general_hospital','van_coastal_health/bella_coola_general_hospital'))
build_code <- TRUE

scen.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')

for (i in seq_along(reg.list)) {
  region.info <- reg.list[[i]]
  region <- region.info[1]
  readloc <- region.info[2]
  writeloc <- readloc
  for (scenario in scen.list) { 
    read.dir <- paste('/storage/data/projects/rci/data/assessments/',readloc,'/tables',sep='')  
    print(read.dir)        
    gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
                  'HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

      pr.table <- seasonal.table('pr',scenario)
      rx1.table <- seasonal.table('rx1dayETCCDI',scenario)
      rx5.table <- seasonal.table('rx5dayETCCDI',scenario)
      tx.table <- seasonal.table('tasmax',scenario)
      tn.table <- seasonal.table('tasmin',scenario)
      txx.table <- seasonal.table('txxETCCDI',scenario)
      tnn.table <- seasonal.table('tnnETCCDI',scenario)
      dtr.table <- seasonal.table('dtrETCCDI',scenario)
      ##if (scenario=='rcp85') {
      ##  snow.table <- seasonal.table('snowdepth','rcp85')
      ##  seas.data <- rbind(pr.table,rx1.table,rx5.table,tx.table,tn.table,txx.table,tnn.table,dtr.table,snow.table) 
      ##} else {
          seas.data <- rbind(pr.table,rx1.table,rx5.table,tx.table,tn.table,txx.table,tnn.table,dtr.table)
      ##}
      ##-----------------------------------------------
      ##Annual Data

      cddE.table <- annual.table('cddETCCDI',scenario)
      cwdE.table <- annual.table('cwdETCCDI',scenario)
      r95.table <- annual.table('r95pETCCDI',scenario)
      r95days.table <- annual.table('r95daysETCCDI',scenario)
##      r95dist.table <- annual.table('r95distETCCDI',scenario)
##      r95sep.table <- annual.table('r95sepETCCDI',scenario)
      r99.table <- annual.table('r99pETCCDI',scenario)
      r99days.table <- annual.table('r99daysETCCDI',scenario)
      su.table <- annual.table('suETCCDI',scenario)
      su30.table <- annual.table('su30ETCCDI',scenario)
      tr.table <- annual.table('trETCCDI',scenario)
      id.table <- annual.table('idETCCDI',scenario)
      fd.table <- annual.table('fdETCCDI',scenario)
      gsl.table <- annual.table('gslETCCDI',scenario)
##      csdi.table <- annual.table('csdiETCCDI',scenario)
##      wsdi.table <- annual.table('wsdiETCCDI',scenario)

      cdd.table <- annual.table('cdd',scenario)
      gdd.table <- annual.table('gdd',scenario)
      hdd.table <- annual.table('hdd',scenario)

      pr.rp20.table <- annual.table('pr',scenario,rp=20)
      tx.rp20.table <- annual.table('tasmax',scenario,rp=20)
      tn.rp20.table <- annual.table('tasmin',scenario,rp=20)


##      rx2.rp.table <- annual.table('rx2dayETCCDI',scenario,rp=10)
##      rx5.rp.table <- annual.table('rx5dayETCCDI',scenario,rp=10)

##      pr.rp10.ratio <- ratio.table('pr',scenario,rp=10)
##      tx.rp10.ratio <- ratio.table('tasmax',scenario,rp=10)
##      tn.rp10.ratio <- ratio.table('tasmin',scenario,rp=10)

##      pr.rp10.ratio <- ratio.table('pr',scenario,rp=10)
##      tx.rp10.ratio <- ratio.table('tasmax',scenario,rp=10)
##      tn.rp10.ratio <- ratio.table('tasmin',scenario,rp=10)

if (build_code) {

      pr.rp5.table <- annual.table('pr',scenario,rp=5)
      tx.rp5.table <- annual.table('tasmax',scenario,rp=5)
      tn.rp5.table <- annual.table('tasmin',scenario,rp=5)

      pr.max.table <- annual.table('pr.maximum',scenario)
      pr.min.table <- annual.table('pr.minimum',scenario)
      pr.std.table <- annual.table('pr.standard_deviation',scenario)

      tx.975.table <- annual.table('tasmax.annual_quantile_975',scenario)
      tx.990.table <- annual.table('tasmax.annual_quantile_990',scenario)
      tx.996.table <- annual.table('tasmax.annual_quantile_996',scenario)

      tn.004.table <- annual.table('tasmin.annual_quantile_004',scenario)
      tn.010.table <- annual.table('tasmin.annual_quantile_010',scenario)
      tn.025.table <- annual.table('tasmin.annual_quantile_025',scenario)

      pr.rp50.table <- annual.table('pr',scenario,rp=50)

      ann.data <- rbind(cddE.table,cwdE.table,r95.table,r95days.table,r99.table,r99days.table,
                        su.table,su30.table,tr.table,
                        id.table,fd.table,gsl.table,cdd.table,gdd.table,hdd.table,                        
                        pr.max.table,pr.min.table,pr.std.table,
                        tx.975.table,tx.990.table,tx.996.table,                        
                        tn.004.table,tn.010.table,tn.025.table,
                        pr.rp5.table,pr.rp20.table,pr.rp50.table,
                        tx.rp5.table,tx.rp20.table,
                        tn.rp5.table,tn.rp20.table)                      
} else {

      ann.data <- rbind(cddE.table,cwdE.table,r95.table,r95days.table,r99.table,r99days.table,
                        su.table,su30.table,tr.table,
                        id.table,fd.table,gsl.table,cdd.table,gdd.table,hdd.table,                        
                        pr.rp20.table,
                        tx.rp20.table,
                        tn.rp20.table)                      
}


      group.data <- rbind(seas.data,ann.data)

      write.table(group.data,
              file=paste('/storage/data/projects/rci/data/assessments/',writeloc,'/tables/',region,'_variable_table_',scenario,'.csv',sep=''),sep=',',quote=F,row.name=F,col.name=T)

    }
   
}


