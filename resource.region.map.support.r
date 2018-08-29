##Script to map the downscaled output

##Copied from precip.scale.mapping.r and is meant to be a script that
##creates better maps specifically for the BC resource regions.

##----------------------------------------------------------------------

library(ncdf4)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer) # to get the sweet color ramps
##library(spatstat) ##to set up the color ramp values 
library(TeachingDemos) #For shadow text

rasterOptions(tmpdir='/home/data/scratch/ssobie/tmp/R_raster_tmp',
                            progress='text',
                            maxmemory=1E+09,
                            chunksize=5E+08)
              
rep.row<-function(x,n){
     matrix(rep(x,each=n),nrow=n)
   }
rep.col<-function(x,n){
     matrix(rep(x,each=n),ncol=n)
   }

get.class.breaks <- function(var.name,type,map.range, manual.breaks=""){
  print('Class Breaks')
  print(map.range)
  map.diff <- diff(map.range)
  class.width <- (map.diff/6)

  if (type=='percent') {
    if (class.width <= 5)
      class.rv <- round(class.width/1)*1    
    if (class.width < 10 & class.width > 5)
      class.rv <- round(class.width/5)*5
    if (class.width >= 10)
      class.rv <- round(class.width/10)*10      
    class.rv <- min(c(class.rv,50))
    class.rv <- max(c(class.rv,0.5))
  } else { 
    if (grepl("(pr|snm|snd|ptot|rx|r9|RP|rp|su)", var.name)){
      if (class.width <= 5)
        class.rv <- round(class.width/1)*1
      if (class.width < 10 & class.width > 5)
        class.rv <- round(class.width/5)*5
      if (class.width >= 10)
        class.rv <- round(class.width/10)*10
      if (class.width >= 100)
        class.rv <- round(class.width/100)*100
      class.rv <- min(c(class.rv,500))
      class.rv <- max(c(class.rv,1))
    } else {
      if (class.width < 5 & class.width > 1) {
        class.rv <- ceiling(class.width/1)*1
        if (type=='anomaly') {
          class.rv <- max(c(1,class.rv))
        }
      }               
      if (class.width < 1) {
        class.rv <- ceiling(class.width/0.1)*0.1
        class.rv <- max(c(0.1,class.rv))        
      }
      if (class.width < 0.251) {
        class.rv <- ceiling(class.width/0.05)*0.05
        class.rv <- max(c(0.05,class.rv))
      }
      
      if (class.width >= 5) {
        class.rv <- round(class.width/5)*5
        class.rv <- max(c(5,class.rv))
      }      
      if (class.width >= 10) {
        class.rv <- round(class.width/10)*10
        class.rv <- max(c(10,class.rv))
      }
    }
  }

  if (manual.breaks[1] != ""){
    class.breaks <- manual.breaks
  } else {
      map.min <- floor(map.range[1]/class.rv) * class.rv
      map.max <- ceiling(map.range[2]/class.rv) * class.rv
      class.breaks <-seq(from=map.min, to=map.max, by=class.rv)
 }

  return(class.breaks)
}
 
get.class.break.labels <- function(class.breaks,greater.sign=FALSE,lesser.sign=FALSE,fixed=FALSE) {
 
  map.class.breaks.labels <- sapply(1:(length(class.breaks)-1), function(x){
    paste(class.breaks[x], "to", class.breaks[x+1])})
  ##Reversed the placement of greater and lesser signs. Previously the greater sign went at the end, lesser was first. --Reversed
  if (lesser.sign){
    map.class.breaks.labels[1] <- paste("<", strsplit(map.class.breaks.labels[1], " ")[[1]][3]) ##Changed last number from 1 to 3
    ##map.class.breaks.labels[1] <- paste(strsplit(map.class.breaks.labels[1], ">")[[1]][1])
  }
  if (greater.sign){
    i <- length(map.class.breaks.labels)
    map.class.breaks.labels[i] <- paste(">", strsplit(map.class.breaks.labels[i], " ")[[1]][1]) ##Changed last number from 3 to 1
    ##map.class.breaks.labels[i] <- paste("<", strsplit(map.class.breaks.labels[i], " ")[[1]][3])
  }
  print(class.breaks)
  print(map.class.breaks.labels)

  return(map.class.breaks.labels)
}
 
choose.break.point <- function(map.class.breaks,bp=""){

  if (bp == "") { bp <- "median"}
  if (bp == "median"){
    bp <- median(map.class.breaks)
  } else {
    bp <- as.numeric(bp)
  }
  class.pairs <- rbind(map.class.breaks[1:length(map.class.breaks)-1], map.class.breaks[2:length(map.class.breaks)])
  ## DB: Changed code here to only look at the 1st class break of the pair.
  right <- apply(class.pairs, 2, function(x) { x[1] >= bp})
  left <- !right
  left.length <- length(which(left == T))#ncol(class.pairs[,left])
  right.length <- length(which(right == T))#ncol(class.pairs[,right])
  return(list(left.length, right.length))
}

get.color.ramp <- function(map.class.breaks, var.name, grey.zones, choose.ramp="", ramp.lengths){
  dark.brown.to.light.brown <- c("#AF6932", "#C89150", "#DCB478", "#F6E8C3")
  light.brown.to.dark.brown <- rev(dark.brown.to.light.brown)

  dark.green.to.light.green <- c('#336600','#4C9900','#66CC00','#CCCC00') ##'#99FF33') ##,'#CCFF99') 
  light.green.to.dark.green <- rev(dark.green.to.light.green)
    
  color.brewer.yellow.orange.red <- c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20")
  color.brewer.red.orange.yellow <- rev(color.brewer.yellow.orange.red) 
  ##"#C6DBEF", "#6BAED6",
  color.brewer.light.blue.to.med.blue <- c("#9ECAE1" , "#4292C6","#0080FF","#003366") ##"#F7FBFF", ###"#DEEBF7", ##First entry
  color.brewer.med.blue.to.light.blue <- rev(color.brewer.light.blue.to.med.blue)

  color.brewer.dark.blue.to.yellow <- c("#003366","#4292C6","#9ECAE1" ,"#FFFF66")

  ##color.brewer.yellow.green.blue <- c("#FFFF66", "#99FF33", "#66CC00", "#00CC66", "#009999","#003366")
  ##MetroVan Alternative
   color.brewer.yellow.green.blue <- c("#FFFF66", "#99FF33" , "#00CC66", "#009999","#0080FF","#003366")
  
  yellow.green.blue <- colorRampPalette(colors=color.brewer.yellow.green.blue, bias=1, space = "Lab", interpolate = "linear")

  yellow.to.dark.red <- colorRampPalette(colors=color.brewer.yellow.orange.red, bias=1, space = "Lab", interpolate = "linear")
  dark.red.to.yellow <- colorRampPalette(colors=color.brewer.red.orange.yellow, bias=1, space = "Lab", interpolate = "linear")
   
  dark.brown.to.light.brown <- colorRampPalette(colors=dark.brown.to.light.brown, bias=1, space = "Lab", interpolate = "linear")
  light.brown.to.dark.brown <- colorRampPalette(colors=light.brown.to.dark.brown, bias=1, space = "Lab", interpolate = "linear")

  dark.green.to.light.green <- colorRampPalette(colors=dark.green.to.light.green, bias=1, space = "Lab", interpolate = "linear")
  light.green.to.dark.green <- colorRampPalette(colors=light.green.to.dark.green, bias=1, space = "Lab", interpolate = "linear")
  
  dark.blue.to.light.blue <- colorRampPalette(colors=color.brewer.med.blue.to.light.blue, bias=1, space = "Lab", interpolate = "linear")
  light.blue.to.dark.blue <- colorRampPalette(colors=color.brewer.light.blue.to.med.blue, bias=1, space = "Lab", interpolate = "linear")

  dark.blue.to.yellow <- colorRampPalette(colors=color.brewer.dark.blue.to.yellow, bias=1, space = "Lab", interpolate = "linear")
  
  ## determine if there is a break point & thuerefore number is both part of the ramp index, if yes, continue
  left.len <- ramp.lengths[[1]]
  right.len <- ramp.lengths[[2]]
  if (all(ramp.lengths[[2]] >= 1 & ramp.lengths[[1]] >= 1)){
    ramp.left.len <- left.len
    ramp.right.len <- right.len
    ##ramp.less.than.breakpoint.length <- left.len
    ##ramp.greater.than.breakpoint.length <- right.len - 1
    if (nchar(choose.ramp) == 0){
      if (grey.zones){
        grey <- "#CCCCCC"
        ramp.left.len <- left.len - 1
        ramp.right.len <- right.len
        if (grepl("(pr|snm|snd|ptot|rx|r9|RP|rp|pas|r10|r20)", var.name)){
          ##rv <- c(dark.brown.to.light.brown(ramp.left.len), grey, light.blue.to.dark.blue(ramp.right.len))
          rv <- c(dark.brown.to.light.brown(ramp.left.len), grey, yellow.green.blue(ramp.right.len))
        } else if (grepl('(snowdepth|swe)',var.name)) {
          rv <- c(dark.brown.to.light.brown(ramp.left.len), grey, light.blue.to.dark.blue(ramp.right.len))
        } else {
          rv <- c(dark.blue.to.light.blue(ramp.left.len), grey, yellow.to.dark.red(ramp.right.len))
        }
      } else {
        if (grepl("(pr|snm|snd|ptot|rx|r9|RP|rp|pas|snowdepth|r10|r20)", var.name)){
          ##rv <- c(dark.brown.to.light.brown(ramp.left.len), light.blue.to.dark.blue(ramp.right.len))
          rv <- c(dark.brown.to.light.brown(ramp.left.len), yellow.green.blue(ramp.right.len))
        } else if (grepl('(snowdepth|swe)',var.name)) {
          rv <- c(dark.brown.to.light.brown(ramp.left.len), grey, light.blue.to.dark.blue(ramp.right.len))          
        } else {
          rv <- c(dark.blue.to.light.blue(ramp.left.len), yellow.to.dark.red(ramp.right.len))
          #if (grepl("(hdd)",var.name))
          #  rv <- rev(rv)
          #if (grepl("gdd",var.name))
          #  rv <- c(dark.brown.to.light.brown(ramp.left.len),light.green.to.dark.green(ramp.right.len))
          if (grepl("(fdd|hdd|idE|fdE)",var.name)) 
            rv <- c(dark.red.to.yellow(ramp.left.len),light.blue.to.dark.blue(ramp.right.len))
          if (grepl("cwdE",var.name))
            rv <- c(dark.brown.to.light.brown(ramp.left.len),light.blue.to.dark.blue(ramp.right.len))
          if (grepl("cddE",var.name))
            rv <- c(dark.blue.to.light.blue(ramp.left.len),light.brown.to.dark.brown(ramp.right.len))
          
        }
      }
      return(rv)
    } else {
      if (grey.zones){
        grey <- "#CCCCCC"
        
        ramp.left.len <- left.len - 1
        ramp.right.len <- right.len
        rv <- c(color.ramp.lookup[[choose.ramp]]$left(ramp.left.len),
                grey, color.ramp.lookup[[choose.ramp]]$right(ramp.right.len))
      } else {
        rv <- c(color.ramp.lookup[[choose.ramp]]$left(ramp.left.len), color.ramp.lookup[[choose.ramp]]$right(ramp.right.len))
      }
    }
  } else {
    ## ramps with no break points (only numbers in the ramp.lengths[[1]]
    ramp.length <- left.len + right.len
    
    if (nchar(choose.ramp) != 0){
      ## if there is a ramp chosen & it is one sided
      if (left.len != 0){
        rv <- color.ramp.lookup[[choose.ramp]]$left(ramp.length)
      } else {
        rv <- color.ramp.lookup[[choose.ramp]]$right(ramp.length)
      }
    } else {
      if (grepl("(pr|snm|snd|ptot|rx|r9|RP|rp|cwdE|r10|r20)", var.name)){
        if (left.len==0)
          rv <- yellow.green.blue(ramp.length) ####light.blue.to.dark.blue(ramp.length)
        if (right.len==0)
          rv <- dark.brown.to.light.brown(ramp.length)
      } else if (grepl('(pas|snowdepth|swe)',var.name)) {
        if (left.len==0)
          rv <- light.blue.to.dark.blue(ramp.length)
        if (right.len==0)
          rv <- dark.brown.to.light.brown(ramp.length)
      } else {
        if (left.len==0)
          rv <- yellow.to.dark.red(ramp.length)
        if (right.len==0)
          ##rv <- dark.blue.to.light.blue(ramp.length)
          rv <- dark.blue.to.yellow(ramp.length)
          
       # if (grepl("(hdd)",var.name)) {
       #   if (left.len==0)
       #     rv <- yellow.to.dark.red(ramp.length)
       #   if (right.len==0)
       #     rv <- dark.blue.to.light.blue(ramp.length)
       # }
        if (grepl("(fdd|hdd|tn10p|idE|fdE)",var.name)) {
          if (left.len==0)
            rv <- light.blue.to.dark.blue(ramp.length)
          if (right.len==0)
            rv <- dark.red.to.yellow(ramp.length)
        }
        if (grepl("(cddE)",var.name)) {
          if (left.len==0)
            rv <- light.brown.to.dark.brown(ramp.length)            
          if (right.len==0)
            rv <- dark.blue.to.light.blue(ramp.length)
        }
      }
    }
   }
  return(rv)  
}
 
get.legend.colourbar <- function(var.name,map.range,my.bp,class.breaks,type='climatology') {
 
  if (sum(map.range<0) > 0)
    my.bp <- 0
  divisions <- choose.break.point(class.breaks,my.bp)
  st <- divisions[[1]]
  en <- divisions[[2]]

  other.map <- get.color.ramp(class.breaks, var.name, grey.zones=FALSE, choose.ramp='', ramp.lengths=divisions)
  return(other.map)
}

leg.label.units <- function(var.name,type) {
  leg.label <- NA
  if (grepl("(tas|txx|tnn|txn|tnx|tmax|tmin|t2m)", var.name))
    leg.label <- '\u00B0C'
  if (grepl("(pr|rx|r9|RP|rp|sdii)", var.name))
    leg.label <- 'mm'
  if (grepl("(pas|snowdepth)", var.name))
    leg.label <- 'm'
  if (grepl("(tx90|tn10)", var.name))
    leg.label <- '%'
  if (type == 'percent' | grepl("percent",tolower(type)))
    leg.label <- '% change'
  if (type == 'increases')
    leg.label <- '# of Models'
  if (grepl("dd", var.name))
    leg.label <- 'Degree days'
  if (grepl("(fdE|cddE|cwd|su|gsl|id|trE|s30|r10|r20)", var.name))
    leg.label <- 'days'
  if (grepl("(wspd)", var.name))
    leg.label <- 'km/h'
  if (grepl("(insol)", var.name))
    leg.label <- 'W m-2'
  return(leg.label)
}   

##------------------------------------------------------------------------------------------------
##Shapefiles

##Basedata
get.basedata.shape <- function(shape.name,search.dir) {
  base.dir <- '/home/data/gis/'
  shape.dir <- paste(base.dir,search.dir,sep='')
  base.shp <- readOGR(shape.dir, shape.name, stringsAsFactors=F, verbose=F)
  return(base.shp)
}

get.intersected.shape <- function(target.shp,clipping.shp) {
  ##Make sure shapes the same projection
  target.shp <- spTransform(target.shp,clipping.shp@proj4string)
  clipped.target <- gIntersection(target.shp,clipping.shp)
  return(clipped.target)
}

##------------------------------------------------------------------------------------------------
##Convert BC Albers to Lat/Lon
##------------------------------------------------------------------------------------------------

convert.albers.to.lat.lon <- function(x,y) {
  ##BC Albers Standard Parallels
  phi.0 <- (45.0/180)*pi
  phi.1 <- (50.0/180)*pi
  phi.2 <- (58.5/180)*pi 

  fn <- 0       ##False Northing
  fe <- 1000000 ##False Easting

  sx <- x - fe
  sy <- fn - y
  
  n.val <- 0.5 * (sin(phi.1) + sin(phi.2))
  c.val <- (cos(phi.1))^2 + 2*n.val*sin(phi.1)
  rho.0 <- sqrt( (c.val - (2*n.val*sin(phi.0)))) / n.val
  rho <- sqrt(sx^2 + (rho.0 - sy)^2)
  theta <- atan((sx)/(rho.0-sy)) * 180/pi

  phi <- asin( (c.val - rho^2 * n.val^2) / (2*n.val)) *180/pi
  lambda <- -126 + (theta / n.val)
  
  ##Taking the northings and eastings to be 0

  rv <- c(lambda,phi) ##Lat then Lon
  return(rv)  
}

##-------------------------------------------------------


   

