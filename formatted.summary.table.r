##Script to sort out the data requested for Okanagan

source('/storage/home/ssobie/code/repos/assessments/summary.table.comments.r',chdir=T)

library(openxlsx)

##------------------------------------------------------------- 
##------------------------------------------------------------- 

get.units.pane <- function(var.name) {
  leg.label <- NA
  if (grepl("(tas|txx|tnn|txn|tnx|tmax|tmin|dtr)", var.name))
    leg.label <- c(rep('degC',7),rep('%',6))
  if (grepl("(pr|rx|r10|r20|r9|RP|sdii|prcptot)", var.name))
    leg.label <- c(rep('mm',7),rep('%',6))
  if (grepl("(dd)", var.name))
    leg.label <- c(rep('degree days',7),rep('%',6))
  if (grepl("(fdE|cddE|cdd90|cddmax|cwd|su|gsl|id|trE|su30|r95daysE|r99daysE)", var.name))
    leg.label <- c(rep('days',7),rep('%',6))
  if (grepl("(dtr)", var.name))
    leg.label <- c(rep('degC',7),rep('%',6))
  if (grepl("(r95sep|r95dist|r95days|r99days)", var.name))
    leg.label <- c(rep('days',7),rep('%',6))
  return(leg.label)
} 

##------------------------------------------------------------- 
get.round.val <- function(var.name) {
  rd <- 0
  if (grepl("(dd)", var.name))
    rd <- 0    
  if (grepl("(tas|txx|tnn|tnx|txn|tmax|tmin)", var.name))
    rd <- 1
  if (grepl("(pr|rx|r9|RP|rp|tx90|tn10|trE|cddE|cdd90|cddmax|cwdE|idE|dtrE|wsdiE|csdiE|r95sep)", var.name))
    rd <- 0
  if (grepl("(pas|snowdepth)", var.name))
    rd <- 0
  return(rd)
} 

##------------------------------------------------------------- 
##Separate variables into precip and temperature
## 'E' denotes a climdex index
##Set the order that the variables should be arranged here.

filter.input.variables <- function(table.vars) {
  all.vars <- unlist(lapply(table.vars,function(x){return(x[1])}))
  pr.vars <- c('pr',
               'prcptotETCCDI','sdiiETCCDI','r10mmETCCDI','r20mmETCCDI',
               'rx1dayETCCDI','rx2dayETCCDI','rx5dayETCCDI',
               'r95pETCCDI','r95daysETCCDI','r99pETCCDI','r99daysETCCDI',
               'pr.maximum','pr.minimum','pr.standard_deviation',
               'pr_rp5','pr_rp20','pr_rp50',
               'cddETCCDI','cdd90ETCCDI','cddmaxETCCDI','cwdETCCDI') 
  pr.ix <- match(pr.vars,all.vars)
  pr.selected <- table.vars[pr.ix[!is.na(pr.ix)]]

  tas.vars <- c('tasmax','tas','tasmin',
                'txxETCCDI','tnnETCCDI','txnETCCDI','tnxETCCDI',
                'dtrETCCDI','suETCCDI','su30ETCCDI','trETCCDI',
                'idETCCDI','fdETCCDI','gslETCCDI',
                'cdd','gdd','hdd','fdd',
                'tasmax.annual_quantile_975',
                'tasmax.annual_quantile_990',       
                'tasmax.annual_quantile_996',                        
                'tasmin.annual_quantile_004',
                'tasmin.annual_quantile_010',
                'tasmin.annual_quantile_025',
                'tasmax_rp5','tasmax_rp20',
                'tasmin_rp5','tasmin_rp20')                          
  tas.ix <- match(tas.vars,all.vars)
  tas.selected <- table.vars[tas.ix[!is.na(tas.ix)]]
  rv <- list(pr=pr.selected,tas=tas.selected)
  return(rv)
}

##------------------------------------------------------------- 

find.row.locations <- function(sorted.vars) {
                   
   pr.vars <- sorted.vars$pr
   pr.rows <- vector(length=length(pr.vars)+1,mode='list')

   pr.rows[[1]] <- 3:5 ##Header Rows
   for (i in 1:length(pr.vars)) {
      row.len <- switch(pr.vars[[i]][[2]],annual=2,seasonal=6)
      pr.rows[[i+1]] <- seq(tail(pr.rows[[i]],1)+1,length.out=row.len)
   }

   tas.start <- tail(pr.rows[[length(pr.vars)+1]],1)
   tas.vars <- sorted.vars$tas
   tas.rows <- vector(length=length(tas.vars)+1,mode='list')
   tas.rows[[1]] <- (1:3)+tas.start
   for (i in 1:length(tas.vars)) {
      row.len <- switch(tas.vars[[i]][[2]],annual=2,seasonal=6)
      tas.rows[[i+1]] <- seq(tail(tas.rows[[i]],1)+1,length.out=row.len)
   }

   rv <- list(pr=pr.rows,tas=tas.rows)
   return(rv)

}
##---------------------------------------------

get.scenarios <- function(var.name,seas,interval,scenario,type,rp=NULL) {
  
  gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
                'HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

  file.name <- paste(type,'.',var.name,'.rcp85.',interval,'.csv',sep='')
  
  if (!is.null(rp)) {
    file.name <- paste(type,'.',var.name,'.rcp85.rp.',rp,'.',interval,'.csv',sep='')
    file.scen <- as.matrix(read.csv(paste(read.dir,'/return_periods/',var.name,'/',file.name,sep=''),
                                    header=TRUE,as.is=TRUE))
  } else if (grepl('ETCCDI',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/climdex/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
  } else if (grepl('(^cdd$|hdd|gdd|fdd|pas)',var.name)) {
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
  data.comb <- as.numeric(data.rcp)
  data.stats <- c(mean(data.comb,na.rm=T,finite=T),
                  quantile(data.comb,0.1,na.rm=T,finite=T),
                  quantile(data.comb,0.9,na.rm=T,finite=T))
  names(data.stats) <- c('avg','10%','90%')
  return(data.stats)
}
 
get.seasonal.data <- function(var.name,scenario) {
  seasons <- c('Winter','Spring','Summer','Fall','Annual')
  seas.values <- matrix(0,nrow=5,ncol=13)
  rd <- get.round.val(var.name)
  for (s in seq_along(seasons)) {
    seas <- seasons[s]
    vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario)[1],rd),
              round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario),rd),              
              round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario),rd),
              round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario),rd))
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
    
    seas.values[s,] <- result
  }
  seas.data <- cbind(seasons,seas.values)  
  return(seas.data)
}


get.annual.data <- function(var.name,scenario,rp) {
  seas <- 'Annual'
  rd <- get.round.val(var.name)
  vals <- c(round(get.data(var.name,seas,'1971-2000','past',scenario,rp)[1],rd),
            round(get.data(var.name,seas,'2011-2040','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2041-2070','abs.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2071-2100','abs.anomalies',scenario,rp),rd),              
            round(get.data(var.name,seas,'2011-2040','percent.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2041-2070','percent.anomalies',scenario,rp),rd),
            round(get.data(var.name,seas,'2071-2100','percent.anomalies',scenario,rp),rd))
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

seasonal.table <- function(var.name,scenario='rcp85',rp=NULL) {

  no.percent <- '(tas|tasmax|tasmin|txxETCCDI|tnnETCCD|tnxETCCDI|txnETCCDI|trETCCDI|suETCCDI|su30ETCCDI)'
  result <- get.seasonal.data(var.name,scenario)
  if (grepl(no.percent,var.name))
    result[,9:14] <- 'NA'
  return(as.data.frame(result))
}


annual.table <- function(var.name,scenario='rcp85',rp=NULL) {
  
  no.percent <- '(tas|tasmax|tasmin|txxETCCDI|tnnETCCD|trETCCDI|r95sep|r99days|r95days)'
  result <- get.annual.data(var.name,scenario,rp) 

  if (grepl(no.percent,var.name))
    result[9:14] <- 'NA'
  return(as.data.frame(t(result)))
}


##----------------------------------------------------------------------------------------
write.variables <- function(wb,sorted.vars,row.locs,type) {
  len <- length(sorted.vars)
  for (i in 1:len) {               
    current.var <- sorted.vars[[i]]
    var.name <- current.var[1]
    print(var.name)
    season <- current.var[2]
    var.title <- current.var[3]
    print(var.title)
    current.row <- row.locs[[i+1]]

    s1 <- createStyle(fontSize = 12, fontColour = "black", textDecoration = c("BOLD"))
    s2 <- createStyle(fontSize = 12, fontColour = "black")
    c1 <- createComment(comment = variable.comment(var.name),style=c(s1,s2),visible=FALSE)
    writeComment(wb, 1, col = 1, row = current.row[1], comment = c1)
    seas.fx <- switch(season,   
                      annual=annual.table,   
                      seasonal=seasonal.table)
    if (grepl('rp',var.name)) {
      rp <- as.numeric(gsub('rp','',strsplit(var.name,'_')[[1]][2]))
      rpvar <- gsub('rp','',strsplit(var.name,'_')[[1]][1])
      var.entry <- seas.fx(rpvar,rp=rp)
    } else {                      
      var.entry <- seas.fx(var.name)                      
    }

    pane.colour <- switch(type,pr='lightblue',tas='tan1')             

    hdstyle <- createStyle(fgFill = pane.colour, halign = "CENTER", textDecoration = "Bold",
                             border = "TopBottomLeftRight", fontColour = "black")  
    units.pane <- c(var.title,get.units.pane(var.name))                             
    writeData(wb, sheet=1, as.data.frame(t(units.pane)), startRow = current.row[1], startCol = 1, headerStyle = hdstyle,
              borders = "rows", borderStyle = "medium",colNames=FALSE)
    addStyle(wb,sheet=1,hdstyle,rows=current.row[1],cols=1:14,gridExpand=FALSE,stack=FALSE)

    datastyle <- createStyle(fgFill = 'white', halign = "RIGHT",
                             border = "TopBottomLeftRight", fontColour = "black")                              
    writeData(wb, sheet=1, var.entry, startRow = current.row[2], startCol = 1, headerStyle = hdstyle,
              borders = "rows", borderStyle = "medium",colNames=FALSE)
    addStyle(wb,sheet=1,datastyle,rows=current.row[-1],cols=1:14,gridExpand=TRUE,stack=FALSE)

    highlight <- createStyle(fgFill = 'lightyellow', halign = "CENTER", 
                             border = "TopBottomLeftRight", fontColour = "black")  
    addStyle(wb,sheet=1,highlight,rows=current.row[-1],cols=2,gridExpand=FALSE,stack=FALSE)
    addStyle(wb,sheet=1,highlight,rows=current.row[-1],cols=5,gridExpand=FALSE,stack=FALSE)
    addStyle(wb,sheet=1,highlight,rows=current.row[-1],cols=6,gridExpand=FALSE,stack=FALSE)
    addStyle(wb,sheet=1,highlight,rows=current.row[-1],cols=11,gridExpand=FALSE,stack=FALSE)
    addStyle(wb,sheet=1,highlight,rows=current.row[-1],cols=12,gridExpand=FALSE,stack=FALSE)

    if (grepl('(suETCCDI|su30ETCCDI)',var.name) | var.name =='cdd') {
        prctstyle <- createStyle(fgFill = 'lightgray', halign = "RIGHT",
                                 border = "TopBottomLeftRight", fontColour = "black")                              
        writeData(wb, sheet=1, var.entry[9:14], startRow = current.row[2], startCol = 9, headerStyle = prctstyle,
                  borders = "rows", borderStyle = "medium",colNames=FALSE)
        addStyle(wb,sheet=1,prctstyle,rows=current.row[-1],cols=9:14,gridExpand=TRUE,stack=FALSE)
        s1 <- createStyle(fontSize = 12, fontColour = "black", textDecoration = c("BOLD"))
        s2 <- createStyle(fontSize = 12, fontColour = "black")
        f1 <- createComment(comment = c('CAUTION\n','Percent changes from a low baseline value can result in deceptively large percent values'),
                            style=c(s1,s2),visible=FALSE)
        for (j in 9:14) {                     
          writeComment(wb, 1, col = j, row = current.row[1]+1, comment = f1)
        }

    }


  }
     
}
##----------------------------------------------------------------------------------------
      ##Top Frozen Pane

create.frozen.top.pane <- function(wb) {

      pane.titles <- list(' ','Past','2020s Change',' ',
                              '2050s Change',' ',
                              '2080s Change',' ',
                              '2020s Percent Change',' ',
                              '2050s Percent Change',' ',
                              '2080s Percent Change',' ')

      fz1 <- createStyle(fgFill = "gray94", halign = "CENTER", textDecoration = "Bold",
                         border = "TopBottomLeftRight", fontColour = "black")
      writeData(wb, sheet=1, pane.titles, startRow = 1, startCol = 1, headerStyle = fz1,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,fz1,rows=1,cols=1:14,gridExpand=FALSE,stack=FALSE)
      mergeCells(wb,sheet=1,cols=c(3,4),rows=1)
      mergeCells(wb,sheet=1,cols=c(5,6),rows=1)
      mergeCells(wb,sheet=1,cols=c(7,8),rows=1)
      mergeCells(wb,sheet=1,cols=c(9,10),rows=1)
      mergeCells(wb,sheet=1,cols=c(11,12),rows=1)
      mergeCells(wb,sheet=1,cols=c(13,14),rows=1)
      ##freezePane(wb,sheet=1,firstRow=TRUE)

      prct.header <- list(' ',' ','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')          
      writeData(wb, sheet=1, prct.header, startRow = 2, startCol = 1, headerStyle = fz1,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,fz1,rows=2,cols=1:14,gridExpand=FALSE,stack=FALSE)
      ##freezePane(wb,sheet=1,firstActiveRow=3)
}

##-----------------------------------------------------------------------------------------
      ##Precipitation Header Rows
create.title.panes <- function(wb,var.name,start.row) {

      pane.titles <- list(' ','Past','2020s Change',' ',
                               '2050s Change',' ',
                              '2080s Change',' ',
                              '2020s Percent Change',' ',
                              '2050s Percent Change',' ',
                              '2080s Percent Change',' ')

      pane.colour <- switch(var.name,pr='lightblue',tas='tan1')             
      hdstyle <- createStyle(fgFill = pane.colour, halign = "CENTER", textDecoration = "Bold",
                             border = "TopBottomLeftRight", fontColour = "black") 
      writeData(wb, sheet=1, pane.titles, startRow = start.row, startCol = 1, headerStyle = hdstyle,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,hdstyle,rows=start.row,cols=1:14,gridExpand=FALSE,stack=FALSE)
      mergeCells(wb,sheet=1,cols=c(3,4),rows=start.row)
      mergeCells(wb,sheet=1,cols=c(5,6),rows=start.row)
      mergeCells(wb,sheet=1,cols=c(7,8),rows=start.row)
      mergeCells(wb,sheet=1,cols=c(9,10),rows=start.row)
      mergeCells(wb,sheet=1,cols=c(11,12),rows=start.row)
      mergeCells(wb,sheet=1,cols=c(13,14),rows=start.row)
      prct.header <- list(' ',' ','Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%',
                                  'Average','10th%-90th%','Average','10th%-90th%','Average','10th%-90th%')          
      writeData(wb, sheet=1, prct.header, startRow = start.row+1, startCol = 1, headerStyle = hdstyle,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,hdstyle,rows=start.row+1,cols=1:14,gridExpand=FALSE,stack=FALSE)

      mergeCells(wb,sheet=1,cols=1:14,rows=start.row+2)
      pane.header <- list(switch(var.name,pr='Precipitation',tas='Temperature'))
      writeData(wb, sheet=1, pane.header, startRow = start.row+2, startCol = 1, headerStyle = hdstyle,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,hdstyle,rows=start.row+2,cols=1:14,gridExpand=FALSE,stack=FALSE)
}


##*******************************************************************
##
##*******************************************************************

##(region,readloc)
if (1==0) {
reg.list <- list(c('bella_coola_general_hospital','van_coastal_health/bella_coola_general_hospital'),
                 c('downtown_community_health_centre','van_coastal_health/downtown_community_health_centre'),
                 c('george_pearson_centre','van_coastal_health/george_pearson_centre'),
                 c('lionsgate_hospital_site','van_coastal_health/lionsgate_hospital_site'),
                 c('lionsgate_hospital','van_coastal_health/lionsgate_hospital'),
                 c('powell_river_hospital_site','van_coastal_health/powell_river_hospital_site'),
                 c('richmond_hospital_site','van_coastal_health/richmond_hospital_site'),
                 c('royal_columbian_hospital_site','van_coastal_health/royal_columbian_hospital_site'),
                 c('sechelt_hospital_site','van_coastal_health/sechelt_hospital_site'),
                 c('squamish_hospital_site','van_coastal_health/squamish_hospital_site'),
                 c('ubc_hospital_site','van_coastal_health/ubc_hospital_site'),
                 c('van_coastal_health','van_coastal_health/van_coastal_health'),
                 c('vancouver_general_hospital_site','van_coastal_health/vancouver_general_hospital_site'))
}

##reg.list <- list(c('chetwynd','northeast/chetwynd'),                 
##                 c('dawson_creek','northeast/dawson_creek'),                                                              
##                 c('fort_nelson','northeast/fort_nelson'),
##                 c('fort_st_john','northeast/fort_st_john'),
##                 c('northeast','northeast'),
##                 c('north_rockies_muni','northeast/north_rockies_muni'),
##                 c('peace_highlands','northeast/peace_highlands'),
##                 c('peace_lowlands','northeast/peace_lowlands'),
##                 c('pouce_coupe','northeast/pouce_coupe'),
##                 c('tumbler_ridge','northeast/tumbler_ridge'))

###c('golden','interior_health/golden'))
###

##Agriculture
reg.list <- list(c('central','agriculture/central'),
                 c('kootenays','agriculture/kootenays'),
                 c('robson_valley','agriculture/robson_valley'),
                 c('slocan_valley','agriculture/slocan_valley'),
                 c('bulkley_nechako','agriculture/bulkley_nechako'),
                 c('central_kootenay','agriculture/central_kootenay'),
                 c('east_kootenay','agriculture/east_kootenay'),
                 c('fraser_fort_george','agriculture/fraser_fort_george'),
                 c('kootenay_boundary','agriculture/kootenay_boundary'),
                 c('kaslo','agriculture/kaslo'),        
                 c('new_denver','agriculture/new_denver'),        
                 c('cranbrook','agriculture/cranbrook'),        
                 c('creston','agriculture/creston'),        
                 c('grand_forks','agriculture/grand_forks'),
                 c('valemount','agriculture/valemount'),        
                 c('mcbride','agriculture/mcbride'),        
                 c('prince_george','agriculture/prince_george'),        
                 c('vanderhoof','agriculture/vanderhoof'),        
                 c('smithers','agriculture/smithers'))

reg.list <- list(c('fraser_health','van_coastal_health/vch_fraser/fraser_health'))

##VCH Fraser
reg.list <- list(c('abbotsford_regional_hospital_site','van_coastal_health/vch_fraser/abbotsford_regional_hospital_site'), 
                 c('burnaby_hospital_site','van_coastal_health/vch_fraser/burnaby_hospital_site'), 
                 c('chilliwack_general_hospital_site','van_coastal_health/vch_fraser/chilliwack_general_hospital_site'), 
                 c('delta_hospital_site','van_coastal_health/vch_fraser/delta_hospital_site'), 
                 c('eagle_ridge_hospital_site','van_coastal_health/vch_fraser/eagle_ridge_hospital_site'),
                 c('fraser_canyon_hospital_site','van_coastal_health/vch_fraser/fraser_canyon_hospital_site'),
                 c('langley_memorial_hospital_site','van_coastal_health/vch_fraser/langley_memorial_hospital_site'), 
                 c('mission_memorial_hospital_site','van_coastal_health/vch_fraser/mission_memorial_hospital_site'),
                 c('peace_arch_hospital_site','van_coastal_health/vch_fraser/peace_arch_hospital_site'), 
                 c('ridge_meadows_hospital_site','van_coastal_health/vch_fraser/ridge_meadows_hospital_site'), 
                 c('riverview_hospital_site','van_coastal_health/vch_fraser/riverview_hospital_site'), 
                 c('royal_columbian_hospital_site','van_coastal_health/vch_fraser/royal_columbian_hospital_site'), 
                 c('surrey_memorial_hospital_site','van_coastal_health/vch_fraser/surrey_memorial_hospital_site'))

##Interior Health                
reg.list <- list(c('golden','interior_health/golden'),
                 c('golden_hospital_site','interior_health/golden_hospital_site'),
                 c('interior_health','interior_health/interior_health'))

##reg.list <- list(c('invermere','agriculture/invermere'))
##reg.list <- list(c('terrace','terrace'))

reg.list <- list(c('abbotsford','fraser_municipal/abbotsford'),
                 c('chilliwack','fraser_municipal/chilliwack'),
                 c('FVRDelectoralG','fraser_municipal/FVRDelectoralG'),
                 c('FVRDelectoralH','fraser_municipal/FVRDelectoralH'),
                 c('kent','fraser_municipal/kent'),
                 c('mission','fraser_municipal/mission'))   


##Okanagan
##reg.list <- list(c('vernon','okanagan/vernon'),
##                 c('central_okanagan','okanagan/central_okanagan'),                 
##                 c('north_okanagan','okanagan/north_okanagan'),
##                 c('okanagan_similkameen','okanagan/okanagan_similkameen'))                                  
##reg.list <- list(c('okanagan','okanagan/okanagan'))

##Toquaht
##reg.list <- list(c('toquaht','toquaht/toquaht_coastal'),
##                 c('toquaht','toquaht/toquaht_mountains'))

##Van City
##reg.list <- list(c('van_city','van_city'))

##reg.list <- list(c('skeena_hydro','skeena_hydro/skeena_cap_bank1'),
##                 c('skeena_hydro','skeena_hydro/skeena_cap_bank2'),
##                 c('skeena_hydro','skeena_hydro/skeena_cap_bank3'),
##                 c('skeena_hydro','skeena_hydro/skeena_cap_substation'))

##reg.list <- list(c('cariboo','resource_regions/cariboo'),
##                 c('northeast','resource_regions/northeast'),
##                 c('omineca','resource_regions/omineca'),
##                 c('skeena','resource_regions/skeena'),
##                 c('south','resource_regions/south'),
##                 c('thompson','resource_regions/thompson'),
##                 c('west','resource_regions/west'))

##Interior Health Sites
reg.list <- list(c('cariboo_memorial_hospital','interior_health/cariboo_memorial_hospital'),
                 c('helmcken_memorial_hospital','interior_health/helmcken_memorial_hospital'),
                 c('hundred_mile_general_hospital','interior_health/hundred_mile_general_hospital'),
                 c('lillooet_hospital','interior_health/lillooet_hospital'),
                 c('royal_inland_hospital','interior_health/royal_inland_hospital'),
                 c('shuswap_lake_hospital','interior_health/shuswap_lake_hospital'),
                 c('nicola_valley_health','interior_health/nicola_valley_health'),
                 c('queen_victoria_hospital','interior_health/queen_victoria_hospital'),
                 c('vernon_jubilee_hospital','interior_health/vernon_jubilee_hospital'),
                 c('kelowna_general_hospital','interior_health/kelowna_general_hospital'),
                 c('penticton_regional_hospital','interior_health/penticton_regional_hospital'),
                 c('princeton_general_hospital','interior_health/princeton_general_hospital'),
                 c('south_okanagan_hospital','interior_health/south_okanagan_hospital'),
                 c('arrow_lakes_hospital','interior_health/arrow_lakes_hospital'),
                 c('kootenay_lake_hospital','interior_health/kootenay_lake_hospital'),
                 c('kootenay_boundary_hospital','interior_health/kootenay_boundary_hospital'),
                 c('boundary_hospital','interior_health/boundary_hospital'),
                 c('golden_districts_hospital','interior_health/golden_districts_hospital'),
                 c('invermere_district_hospital','interior_health/invermere_district_hospital'),
                 c('east_kootenay_hospital','interior_health/east_kootenay_hospital'),
                 c('elk_valley_hospital','interior_health/elk_valley_hospital'),
                 c('creston_valley_hospital','interior_health/creston_valley_hospital'))

reg.list <- list(c('bc','bc'))

reg.list <- list(c('campbell_river','vancouver_island/campbell_river'),
                 c('courtenay','vancouver_island/courtenay'),
                 c('port_alberni','vancouver_island/port_alberni'),
                 c('nanaimo','vancouver_island/nanaimo'),
                 c('duncan','vancouver_island/duncan'),
                 c('victoria','vancouver_island/victoria'),
                 c('sooke','vancouver_island/sooke'),
                 c('alberni_clayoquot','vancouver_island/alberni_clayoquot'),     
                 c('comox_valley','vancouver_island/comox_valley'),     
                 c('cowichan_valley','vancouver_island/cowichan_valley'),     
                 c('capital_region','vancouver_island/capital_region'),     
                 c('nanaimo_regional_district','vancouver_island/nanaimo_regional_district'))     
                    
reg.list <- list(c('sayward','vancouver_island/sayward'))

table.vars <- list(c('pr','seasonal','PR'),
                   c('rx1dayETCCDI','seasonal','RX1DAY'),
                   c('rx5dayETCCDI','seasonal','RX5DAY'),
                   c('r95pETCCDI','annual','R95P'),
                   c('r95daysETCCDI','annual','R95DAYS'),
                   c('r99pETCCDI','annual','R99P'),
                   c('r99daysETCCDI','annual','R99DAYS'),
                   c('pr_rp20','annual','RP20 PR'),
                   c('pr_rp5','annual','RP5 PR'),
                   c('pr_rp50','annual','RP50 PR'),
                   c('cddETCCDI','annual','CDD'),
                   c('cwdETCCDI','annual','CWD'),
                   c('tasmax','seasonal','TASMAX'),
                   c('tas','seasonal','TAS'),
                   c('tasmin','seasonal','TASMIN'),
                   c('txxETCCDI','seasonal','TXX'),
                   c('txnETCCDI','seasonal','TXN'),
                   c('tnnETCCDI','seasonal','TNN'),
                   c('tnxETCCDI','seasonal','TNX'),
                   c('dtrETCCDI','seasonal','DTR'),
                   c('suETCCDI','annual','SU'),
                   c('su30ETCCDI','annual','SU30'),
                   c('trETCCDI','annual','TR'),
                   c('idETCCDI','annual','ID'),
                   c('fdETCCDI','annual','FD'),
                   c('gslETCCDI','annual','GSL'),
                   c('cdd','annual','CDD'),
                   c('gdd','annual','GDD'),
                   c('hdd','annual','HDD'),
                   c('fdd','annual','FDD'),
                   c('tasmax_rp20','annual','RP20 TX'),
                   c('tasmin_rp20','annual','RP20 TN'),
                   c('tasmax_rp5','annual','RP5 TX'),
                   c('tasmin_rp5','annual','RP5 TN'),
                   c('tasmax.annual_quantile_975','annual','TX 97.5%'),
                   c('tasmax.annual_quantile_990','annual','TX 99.0%'),
                   c('tasmax.annual_quantile_996','annual','TX 99.6%'),
                   c('tasmin.annual_quantile_004','annual','TN 0.4%'),
                   c('tasmin.annual_quantile_010','annual','TN 1.0%'),
                   c('tasmin.annual_quantile_025','annual','TN 2.5%'),                          
                   c('pr_rp5','annual','RP5 PR'),
                   c('pr.maximum','annual','ANN PR MAX'),
                   c('pr.minimum','annual','ANN PR MIN'),
                   c('pr.standard_deviation','annual','ANN PR SD'))


##bc.vars <-    list(

##                   c('cdd90ETCCDI','annual','CDD90'),
##                   c('cddmaxETCCDI','annual','CDDMAX'),


##table.vars <- list(c('tasmax_rp20','annual','RP20 TX'),
##                   c('txxETCCDI','seasonal','TXX'),                 
##                   c('r99pETCCDI','annual','R99P'),
##                    c('pr','seasonal','PR'))


sorted.vars <- filter.input.variables(table.vars) 

row.locs <- find.row.locations(sorted.vars)

##------------------------------------------------------------------------



for (i in seq_along(reg.list)) {
  region.info <- reg.list[[i]]
  region <- region.info[1]
  readloc <- region.info[2]
  writeloc <- readloc
  read.dir <- paste('/storage/data/projects/rci/data/assessments/',readloc,'/tables',sep='')  

  ##Formatted Table
  wb <- createWorkbook()
  addWorksheet(wb, "Regional Averages")
  setColWidths(wb, sheet = 1, cols = 1:14, widths = 14) ##Set fixed width
  create.frozen.top.pane(wb)
  create.title.panes(wb,var.name='pr',start.row=row.locs$pr[[1]][1])
  write.variables(wb,sorted.vars$pr,row.locs$pr,'pr')
  create.title.panes(wb,var.name='tas',start.row=row.locs$tas[[1]][1])
  write.variables(wb,sorted.vars$tas,row.locs$tas,'tas')
  freezePane(wb,sheet=1,firstActiveCol=2,firstActiveRow=3)

  ##saveWorkbook(wb, paste0('/storage/home/ssobie/general/assessment_tables/',region,'_variable_table_rcp85.xlsx'), overwrite = TRUE)
  saveWorkbook(wb, paste0('/storage/data/projects/rci/data/assessments/',readloc,'/tables/',region,'_variable_table_rcp85.xlsx'), overwrite = TRUE)

}

