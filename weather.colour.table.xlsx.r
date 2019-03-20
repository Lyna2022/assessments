##Script to sort out the data requested for Okanagan

source('/storage/home/ssobie/code/repos/assessments/summary.table.comments.r',chdir=T)

library(openxlsx)

##------------------------------------------------------------- 
##Luminosity Contrast Algorithm
get.text.contrasting <- function(hex) {

   ##hexColor RGB
   rgb.col <- col2rgb(hex)

   ##Black RGB
   rgb.black <- col2rgb('black')

   ##Calc contrast ratio
   Lum.col <- 0.2126 * (rgb.col[1] / 255)^2.2 +
              0.7152 * (rgb.col[2] / 255)^2.2 +
              0.0722 * (rgb.col[3] / 255)^2.2

   Lum.black <- 0.2126 * (0 / 255)^2.2 +
                0.7152 * (0 / 255)^2.2 +
                0.0722 * (0 / 255)^2.2
                      
   contrastRatio <-  0;
   if (Lum.col > Lum.black) {
      contrastRatio <- (Lum.col + 0.05) / (Lum.black + 0.05)
   } else {
      contrastRatio <- (Lum.black + 0.05) / (Lum.col + 0.05)
   }

   ##If contrast is more than 5, return black color
   if (contrastRatio > 4.5) {
      return('black')
   } else { #if not, return white color.
      return('white')
   }
}



##------------------------------------------------------------- 
choose.break.point <- function(map.class.breaks,bp=""){

  if (bp == "") { bp <- "median"}
  if (bp == "median"){
    bp <- median(map.class.breaks)
  } else {
    bp <- as.numeric(bp)
  }
  class.pairs <- rbind(map.class.breaks[1:length(map.class.breaks)-1], map.class.breaks[2:length(map.class.breaks)])
  ##Changed code here to only look at the 1st class break of the pair.
  right <- apply(class.pairs, 2, function(x) { x[1] >= bp})
  left <- !right
  left.length <- length(which(left == T))#ncol(class.pairs[,left])
  right.length <- length(which(right == T))#ncol(class.pairs[,right])
  if (left.length==0|right.length==0)
     browser()
  return(list(left.length, right.length))
}
##------------------------------------------------------------- 
colour.map <- function(ramp.lengths,type) {

  left.len <- ramp.lengths[[1]]
  right.len <- ramp.lengths[[2]]
  print(ramp.lengths)
  colour.brewer.light.red.to.dark.red <- c("#FFDDDD", "#FF9999","#FF6666","#FF3333","#FF0000","#CC0000","#990000")
  light.red.to.dark.red <- colorRampPalette(colors=colour.brewer.light.red.to.dark.red, bias=1, space = "Lab", interpolate = "linear")

  colour.brewer.dark.blue.to.light.blue <- c("#000099","#0000CC","#0000FF","#3333FF","#6666FF","#9999FF","#E4E4FF")
  dark.blue.to.light.blue <- colorRampPalette(colors=colour.brewer.dark.blue.to.light.blue, bias=1, space = "Lab", interpolate = "linear")

  colour.brewer.tan.to.light.green <- c("#FFF77C","#B2FF66","#33FF33","#00CC00")
  tan.to.light.green <-  colorRampPalette(colors=colour.brewer.tan.to.light.green, bias=1, space = "Lab", interpolate = "linear")
  
  colour.brewer.light.green.to.blue <- c("#00CC66","#00CCCC","#0080FF","#0066CC")
  light.green.to.blue <- colorRampPalette(colors=colour.brewer.light.green.to.blue, bias=1, space = "Lab", interpolate = "linear")
  
  print(type)
  if (type=='tas') {
    rv <- c(dark.blue.to.light.blue(left.len),light.red.to.dark.red(right.len))
  }
  if (type=='pr') {
    rv <- c(tan.to.light.green(left.len),light.green.to.blue(right.len))
  }

  return(rv)
}

##------------------------------------------------------------- 

get.class.breaks <- function(map.range, manual.breaks=""){
  print('Class Breaks')
  print(map.range)

  map.min <- floor(map.range[1])-1
  map.max <- ceiling(map.range[2])+1
  class.breaks <-seq(from=map.min, to=map.max, length.out=12) ##by=class.rv)
  print(class.breaks)
  return(class.breaks)
}
##------------------------------------------------------------- 
get.cell.colours <- function(map.range,type,my.bp='median',manual.breaks="") {

  class.breaks <- get.class.breaks(map.range, manual.breaks)
  my.bp <- switch(type,tas=0,pr='median')
  divisions <- choose.break.point(class.breaks,my.bp)
  other.map <- colour.map(ramp.lengths=divisions,type)
  return(other.map)
}
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
  tas.vars <- c('txxETCCDI','tasmax','tas','tasmin','tnnETCCDI')
  tas.ix <- match(tas.vars,all.vars)
  tas.selected <- table.vars[tas.ix[!is.na(tas.ix)]]

  pr.vars <- c('rx1dayETCCDI','rx5dayETCCDI','pr',
               'prcptotETCCDI','sdiiETCCDI','r10mmETCCDI','r20mmETCCDI')
               
  pr.ix <- match(pr.vars,all.vars)
  pr.selected <- table.vars[pr.ix[!is.na(pr.ix)]]

  rv <- list(pr=pr.selected,tas=tas.selected)
  return(rv)
}

##------------------------------------------------------------- 

find.row.locations <- function(sorted.vars) {
   offset <- 2
   tas.vars <- sorted.vars$tas
   tas.rows <- vector(length=length(tas.vars),mode='list')
   for (i in 1:length(tas.vars)) {
      row.len <- 1 
      tas.rows[[i]] <- i+offset
   }

   pr.vars <- sorted.vars$pr
   pr.start <- tas.rows[[length(tas.vars)]]
   pr.rows <- vector(length=length(pr.vars),mode='list')

   for (i in 1:length(pr.vars)) {
      row.len <- 1 ##switch(pr.vars[[i]][[2]],annual=2,seasonal=6)
      pr.rows[[i]] <- seq(tail(pr.start,1)+i,length.out=row.len)
   }
   rv <- list(pr=pr.rows,tas=tas.rows)
   return(rv)

}
##---------------------------------------------

get.scenarios <- function(var.name,seas,interval,scenario,type) {
  
  gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
                'HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

  file.name <- paste(type,'.',var.name,'.rcp85.',interval,'.csv',sep='')
  
  if (grepl('ETCCDI',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/climdex/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))

  } else if (grepl('(^cdd$|hdd|gdd|fdd|pas)',var.name)) {
    file.scen <- as.matrix(read.csv(paste(read.dir,'/degree_days/',var.name,'/',file.name,sep=''),header=TRUE,as.is=TRUE))
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
 
get.data <- function(var.name,seas,interval,type,scenario) {
  data.rcp <- get.scenarios(var.name,seas,interval,scenario,type)
  data.comb <- as.numeric(data.rcp)
  data.stats <- mean(data.comb,na.rm=T,finite=T)
  return(data.stats)
}

get.monthly.data <- function(var.name,scenario,interval) {
  months <- month.abb
  mon.values <- rep(0,12)
  rd <- get.round.val(var.name)
  for (s in seq_along(months)) {
    mon <- months[s]
    vals <- round(get.data(var.name,mon,interval,type='past',scenario),rd)
    mon.values[s] <- vals
  }
  return(mon.values)
}



##----------------------------------------------------------------------------------------
write.variables <- function(wb,sorted.vars,row.locs,type) {
  len <- length(sorted.vars)
  var.matrix <- matrix(NA,nrow=len,ncol=12)

  for (i in 1:len) {               
    current.var <- sorted.vars[[i]]
    var.name <- current.var[1]
    print(var.name)
    season <- current.var[2]
    var.title <- current.var[3]
    print(var.title)
    var.entry <- get.monthly.data(var.name,scenario,interval='1971-2000')
    print(var.entry)
    var.matrix[i,] <- var.entry
  }

  var.range <- range(var.matrix,na.rm=T)
  cell.colours <- get.cell.colours(var.range,type,manual.breaks="")
  class.breaks <- get.class.breaks(var.range)
  class.diff <- diff(class.breaks)[1]
  rounded.vals <- floor(var.matrix/class.diff)*class.diff
  col.ix <- apply(var.matrix,c(1,2),function(x,y){tail(which((x-y)>0),1)},class.breaks)

  for (i in 1:len) {
    current.row <- row.locs[[i]]
    current.var <- sorted.vars[[i]]
    var.name <- current.var[1]
    var.title <- current.var[3]

    ##Variable Title and Comment
    s1 <- createStyle(fontSize = 12, fontColour = "black", textDecoration = c("BOLD"))
    s2 <- createStyle(fontSize = 12, fontColour = "black")
    c1 <- createComment(comment = variable.comment(var.name),style=c(s1,s2),visible=FALSE)
    writeComment(wb, 1, col = 1, row = current.row[1], comment = c1)
    title.style <- createStyle(fgFill = 'gray94', halign = "CENTER", textDecoration = "Bold",
                             border = "TopBottomLeftRight", fontColour = "black")  
    writeData(wb, sheet=1, var.title, startRow = current.row[1], startCol = 1, headerStyle = title.style,
              borders = "rows", borderStyle = "medium",colNames=FALSE)
    addStyle(wb,sheet=1,title.style,rows=current.row[1],cols=1,gridExpand=FALSE,stack=FALSE)

    ##Monthly values
    for (j in 1:12) {
      text.col <- get.text.contrasting(cell.colours[col.ix[i,j]])
      ##if (col.ix[i,j] %in% c(1,2,3,4,10,11,12))         
      ##      text.col <- 'white'
      highlight <- createStyle(fgFill = cell.colours[col.ix[i,j]], halign = "CENTER", 
                               border = "TopBottomLeftRight", fontColour = text.col)  
      writeData(wb, sheet=1, var.matrix[i,j], startRow = current.row, startCol = j+1, headerStyle = highlight,
                borders = "rows", borderStyle = "medium",colNames=FALSE)
      addStyle(wb,sheet=1,highlight,rows=current.row,cols=j+1,gridExpand=FALSE,stack=FALSE)
    }
  }
}

##----------------------------------------------------------------------------------------
      ##Top Frozen Pane

create.frozen.top.pane <- function(wb,site) {

      hdstyle <- createStyle(fgFill = 'gray94', halign = "CENTER", textDecoration = "Bold",
                             border = "TopBottomLeftRight", fontColour = "black") 
      ##Title with site name                       
      mergeCells(wb,sheet=1,cols=1:13,rows=1)
      pane.title <- paste0('Historical Climate Data for ',region)
      writeData(wb, sheet=1, pane.title, startRow = 1, startCol = 1, headerStyle = hdstyle,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,hdstyle,rows=1,cols=1:13,gridExpand=FALSE,stack=FALSE)

      month.header <- list('Month','January','February','March','April','May','June','July',
                                   'August','September','October','November','December')
      writeData(wb, sheet=1, month.header, startRow = 2, startCol = 1, headerStyle = hdstyle,
                borders = "rows", borderStyle = "medium")
      addStyle(wb,sheet=1,hdstyle,rows=2,cols=1:13,gridExpand=FALSE,stack=FALSE)
}

##-----------------------------------------------------------------------------------------


##*******************************************************************
##
##*******************************************************************

reg.list <- list(c('terrace','terrace','Terrace'))

table.vars <- list(c('pr','monthly','PR'),
                   c('rx1dayETCCDI','monthly','RX1DAY'),
                   c('rx5dayETCCDI','monthly','RX5DAY'),
                   c('txxETCCDI','monthly','TXX'),
                   c('tasmax','monthly','TASMAX'),
                   c('tas','monthly','TAS'),
                   c('tasmin','monthly','TASMIN'),
                   c('tnnETCCDI','monthly','TNN'))

sorted.vars <- filter.input.variables(table.vars) 

row.locs <- find.row.locations(sorted.vars)

##------------------------------------------------------------------------

for (i in seq_along(reg.list)) {
  region.info <- reg.list[[i]]
  region <- region.info[1]
  readloc <- region.info[2]
  region.title <- region.info[3]
  writeloc <- readloc
  read.dir <- paste('/storage/data/projects/rci/data/assessments/',readloc,'/tables',sep='')  

  ##Formatted Table
  wb <- createWorkbook()
  addWorksheet(wb, "Regional Averages")
  setColWidths(wb, sheet = 1, cols = 1:13, widths = 10) ##Set fixed width
  create.frozen.top.pane(wb,region.title)
  write.variables(wb,sorted.vars$tas,row.locs$tas,'tas')
  write.variables(wb,sorted.vars$pr,row.locs$pr,'pr')
  freezePane(wb,sheet=1,firstActiveCol=2,firstActiveRow=3)

  saveWorkbook(wb, paste0('/storage/data/projects/rci/data/assessments/',readloc,'/tables/',region,'_colour_box_rcp85.xlsx'), overwrite = TRUE)

}

