################################################################
################################################################
##
## Funzioni per leggere i meteo
##
################################################################
################################################################

################################################################
## Read.folder legge tutti i file in una cartella
read.folder <- function (dpath,dateformat='NULL',checkdata=FALSE) {
  
  if ( dateformat == 'NULL' ) {
    stop("\nDateformat is not set: read.folder(..., dateformat=)")
  }
  
  #Elenco dei files
  files <- list.files(dpath)
  nfiles <- length(files)
  
  #Read first file to initialize data frame
  dataframe <- read.station(files[1],dpath,dateformat=dateformat,checkdata=checkdata)
  
  if (checkdata) {
    print('Dim dataframe in read.folder')
    print(dim(dataframe))
  }
  
  
  for (ifile in 2:nfiles) {
    r <- read.station(files[ifile],dpath,dateformat=dateformat,checkdata=checkdata)
    dataframe <- merge(dataframe,r,by="Date",all=TRUE,sort=TRUE)
    
    if (checkdata) {
      print('Dim dataframe in read.folder')
      print(dim(dataframe))
    }
    
    
  }
  
  return(dataframe)
  
}

################################################################
## Read.folder.img legge tutti i file in una cartella
## I file sono organizzati per anni
## Restituisce una lista
read.folder.img <- function (dpath,checkdata=FALSE) {
  
  #Elenco delle cartelle
  folders <- list.files(dpath)
  nfolders <- length(folders)
  
  count<-1
  for (ifolder in 1:nfolders) {
    
    #Elenco delle cartelle
    ddpath <- paste0(dpath,folders[ifolder],"/")
    files <- list.files(ddpath)
    nfiles <- length(files)
    print(paste0(nfiles, " files in folder: ",folders[ifolder]))
    
    for (ifile in 1:nfiles) {      
      y<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][2]
      m<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][3]
      d<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][4]
#      print(paste0("Year:",y," Month:",m," Decad:",d))
      r <- read.img.leap(files[ifile],ddpath,coord=T)
      if (count==1) {
        r.list <- list(lon=r$x,lat=r$y,y=list(),m=list(),d=list(),p=list())
        r.list$y[[count]] <- y
        r.list$m[[count]] <- m
        r.list$d[[count]] <- d
        r.list$p[[count]] <- r$p
      } else {
        r.list$y[[count]] <- y
        r.list$m[[count]] <- m
        r.list$d[[count]] <- d
        r.list$p[[count]] <- r$p
      }      
      count<-count+1
    }
  }
  return(r.list)
  
}

################################################################
## Read.folder.img legge tutti i file in una cartella
## I file sono organizzati per anni
## Restituisce una lista di GridSpatialData
read.folder.img.spatial <- function (dpath,checkdata=FALSE) {
  
  #Elenco delle cartelle
  folders <- list.files(dpath)
  nfolders <- length(folders)
  
  count<-1
  r.list <- list(y=list(),m=list(),d=list(),p=list())
  for (ifolder in 1:nfolders) {
    
    #Elenco delle cartelle
    ddpath <- paste0(dpath,folders[ifolder],"/")
    files <- list.files(ddpath)
    nfiles <- length(files)
    print(paste0(nfiles, " files in folder: ",folders[ifolder]))
    
    for (ifile in 1:nfiles) {      
      y<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][2]
      m<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][3]
      d<-strsplit(as.character(strsplit(files[ifile],split=".img")),split="_")[[1]][4]
      #      print(paste0("Year:",y," Month:",m," Decad:",d))
      r <- read.img.leap(files[ifile],ddpath,coord=T)
      filename <- paste0(ddpath,files[ifile])
      r <- readGDAL(filename,silent=TRUE)
        r.list$y[[count]] <- y
        r.list$m[[count]] <- m
        r.list$d[[count]] <- d
        r.list$p[[count]] <- r
      count <- count+1
    }
  }
  return(r.list)
  
}



################################################################
## Read.station legge i singoli file di stazione
read.station <- function (filename,dpath,dateformat='NULL',checkdata=FALSE) {
  
  print(paste0('Reading: ',filename))
  
  if ( dateformat == 'NULL' ) {
    stop("\nDateformat is not set: read.folder(..., dateformat=)\n
         Available dateformats:\n  
         %Y%m%d\n
         %d.%B.%Y")
  }
  
  filepath    <- paste0(dpath,filename)  
  fileformat  <- unlist(strsplit(filename,'.',fixed=TRUE))[2]
  namestation <- unlist(strsplit(filename,'.',fixed=TRUE))[1]
  
  #Legge la stazione
  #On Windows, the perl based routines (read.xls, xls2sep, xls2csv, xls2tab,
  #xls2tsv, sheetNames, sheetCount) will work with ActiveState perl but not with
  #Rtools perl.
  #See http://cran.r-project.org/web/packages/gdata/INSTALL
  #    http://cran.at.r-project.org/doc/manuals/R-admin.html#The-Windows-toolset
  
  if ( fileformat == 'xls' ) { dta <- read.xls(filepath) }
  if ( fileformat == 'csv' ) { dta <- read.table(filepath,header=TRUE,sep=';')  }
  
  #Check class
  if (checkdata) {
    cclass<-'Check Class: '
    for (i in 1:dim(dta)[2]) {
      cclass<-paste(cclass,as.character(class(dta[[i]])))
    }
    print(cclass)
    print(dim(dta))
  }
  if (dateformat == "%Y%m%d") {
    dta <- melt(dta,c(1,2))
    names(dta) <- c('month','day','year','Rain')
    #     
    #Converte le date
    dta$year <- as.integer(sub('X','',dta$year))
    Date <- as.POSIXct(as.character(10000*dta$year+100*dta$month+dta$day),"",dateformat)
    dta <- cbind.data.frame(Date,dta$Rain)
  }
  
  if (dateformat == "%d.%B.%Y") {
    dta <- melt(dta,1)
    names(dta) <- c('monthday','year','Rain')
    #     
    #Converte le date
    dta$year <- as.integer(sub('X','',dta$year))
    #    Date <- as.POSIXct(paste0(as.character(dta$monthday),'.',as.character(dta$year)),"",dateformat)
    Date <- as.Date(paste0(as.character(dta$monthday),'.',as.character(dta$year)),dateformat)
    dta <- cbind.data.frame(Date,dta$Rain)
  }
  
  if (checkdata) {
    print('Dim dta dopo converte le date')
    print(dim(dta))
  }
  
  names(dta) <- c("Date",namestation)
  
  #Identifica NA
  dta[namestation][dta[namestation]<0] <- NA 
  #   }
  
  dta <- dta[!is.na(dta$Date),]
  
  if (checkdata) {
    print('Dim dta Dopo rimuovi na')
    print(dim(dta))
  }
  
  return(dta)
  }

################################################################
## Read.img.leap legge le singole mappe LEAP
read.img.leap<-function(name,dpath,coord=FALSE){
  
  filename <- paste0(dpath,name)
  x <- readGDAL(filename,silent=TRUE)
  
  info <- GDALinfo(filename,silent=TRUE) 
  if (coord) {  
    offs <- info[c("ll.x", "ll.y")] 
    scl <- info[c("res.x", "res.y")] 
    dimn <- info[c("columns", "rows")] 
    xs <- seq(offs[1], by = scl[1], length = dimn[1]) + scl[1]/2 
    ys <- seq(offs[2], by = scl[2], length = dimn[2]) + scl[2]/2 
  }
  
  gg<-x$band1
  g2<-matrix(gg,info[2],info[1])
  g2=g2[,rev(seq_len(ncol(g2)))]
  #Return data with coordinates
  if (coord) {  
    ret.data<-list(xs,ys,g2)
    names(ret.data)<-c("x","y","p")
    return (ret.data)
  } else {  
    #Return data without coordinates
    ret.data<-list(g2)
    names(ret.data)<-c("p")
    return (ret.data)
  }
  
}


################################################################
## Read.station legge i singoli file di stazione
read.station.campbell <- function (filename,dpath,dateformat='NULL',checkdata=FALSE) {
#Read data in the format of the Campbell datalogger
#Adds directly information on the day and month
#Flag NaN

  filepath    <- paste0(dpath,filename)
  dta <- read.table(filepath,header=TRUE,sep=',')
  origin <- as.Date(paste0(dta$year, "-01-01"),tz = "UTC") - days(1)
  #Compute date   
  dta_date <- as.Date(dta$doy, origin = origin, tz = "UTC")
  ret.data <- cbind.data.frame(month=month(dta_date),day=day(dta_date), dta) 
  #Flag NaNs
  ret.data[which(ret.data==-9999.00,arr.ind=T)] <- NaN
  
  return(ret.data)
      
}