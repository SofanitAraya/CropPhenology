#======================================================================================================================================
#                                     PhenoMetrics Function
#======================================================================================================================================
#' Phenologic metrics from time series vegetation index data
#'
#' @return  A RasterStack of 15 phenoloical metric images in the order of OnsetV, OnsetT, MaxV, MaxT, OffsetV, OffsetT, LengthGS, BeforeMaxT, AfterMaxT, GreenUpSlope, BrownDownSlope, TINDVI, TINDVIBeforeMax, TINDVIAfterMax, Asymmetry
#' @keywords Phenology
#' @keywords remote sensing
#' @keywords satellite image
#' @keywords Time-series
#' @seealso MultiPointsPlot (VIStack)
#' @description This function extracts 15 Phenological metrics that indicate the growth conditon of crops, from multi-temporal vegetaion index images.
#' @param VIStack - RasterStack - a raster stack from whichthe time series extracted
#' @param ROI - a polygon designated for region of interest. It can be spatialPolygon object or extent.
#' @param Percentage - Optional Numeric Vlaue - percentage of minimum NDVI value at which the Onset and Offset is defined. The 'Percentage' paramenter is optional; if not provided, a Default value of 20 will be taken.
#' @param Smoothing - Optional logical value - if the user chooses to use smoothed curve or row/unsmoothed curve. If "Smoothing' is set to TRUE, the moving avegare filter will be applied to the vegetation index curve. The default value, if not provided, is FALSE, then the unsmoothed row data be used for the analysis.
#'
#' @export
#'
#' #EXAMPLE
#' ExampleROI<- readOGR (system.file("extdata","ROI.shp", package="CropPhenology"))
#' ExampleStack<- stack (system.file("extdata", "ExampleStack.grd", package="CropPhenology"))
#' PhenoStack<- PhenoMetrics (ExampleStack,ExampleROI )
#'

PhenoMetrics<- function (VIStack, ROI=NULL, Percentage=NULL, Smoothing=NULL){
	require(xlsx)
	require(sp)
	require(sf)
	require(roxygen2)
	require(raster)


  if(require('shapefiles')){
    print("Shapefiles is loaded correctly")
  }
  else {
    print("trying to install 'shapefiles'")
    install.packages("shapefiles")
    if(require('shapefiles')){
        print("'shapefiles' installed and loaded")
    } else {
        stop("could not install 'shapefiles'")
    }
  }
	if(require('sp')){
    print("sp is loaded correctly")
  }
  else {
    print("trying to install 'sp'")
    install.packages("sp")
    if(require('sp')){
        print("'sp' installed and loaded")
    } else {
        stop("could not install 'sp'")
    }
  }
	if(require('sf')){
    print("sf is loaded correctly")
  }
  else {
    print("trying to install 'sf'")
    install.packages("sf")
    if(require('sf')){
        print("'sf' installed and loaded")
    } else {
        stop("could not install 'sf'")
    }
  }

	if(require('maptools')){
    print("maptools is loaded correctly")
  }
  else {
    print("trying to install 'maptools'")
    install.packages("maptools")
    if(require('maptools')){
        print("'maptools' installed and loaded")
    } else {
        stop("could not install 'maptools'")
    }
  }
	if(require('rgdal')){
    print("rgdal is loaded correctly")
  }
  else {
    print("trying to install 'rgdal'")
    install.packages("rgdal")
    if(require('rgdal')){
        print("'rgdal' installed and loaded")
    } else {
        stop("could not install 'rgdal'")
    }
  }
	if(require('xlsx')){
    print("xlsx is loaded correctly")
  }
  else {
    print("trying to install 'xlsx'")
    install.packages("xlsx")
    if(require('xlsx')){
        print("'xlsx' installed and loaded")
    } else {
        stop("could not install 'xlsx'")
    }
  }
	if(require('rgeos')){
    print("rgeos is loaded correctly")
  }
  else {
    print("trying to install 'rgeos'")
    install.packages("rgeos")
    if(require('rgeos')){
        print("'rgeos' installed and loaded")
    } else {
        stop("could not install 'rgeos'")
    }
  }
	if(require('raster')){
    print("raster is loaded correctly")
  }
  else {
    print("trying to install 'raster'")
    install.packages("raster")
    if(require('raster')){
        print("'raster' installed and loaded")
    } else {
        stop("could not install 'raster'")
    }
  }

  #===========================================================================
  if (missing(Percentage)) {
    print ("The default value, 20%, will be applied")
    Percentage=20
  }
  if (!is.numeric(Percentage)){
    stop("Percentage value for Onset and Offset should be numeric")
  }
  if (Percentage<0){
    stop("Negative Onset-Offset percentage specified")
  }
  if (Percentage==0){
    stop("Onset-Offset percentage should be greated than 0")
  }
  #===========================================================================

  if (missing(Smoothing)){
    print("Unsmoothed curve will be used")
    Smoothing=FALSE
  }

  #===========================================================================


  # setwd(Path)
  # raDir=dir(path=Path, pattern = c(".img$|.tif$"))
  # FileLen=length(raDir)
  # allras <- list.files(pattern = c(".img$|.tif$"))
  # hugeStack = stack(allras)

  q=1
  qon=1
  qoff=1
  qmax=1
  par(mfrow=c(1,1))
  par(mar=c(3.5, 2.5, 2.5, 5.5))
  s=1
  Enter=FALSE

  if (is.null(ROI) == FALSE){
  	if (class(ROI)== "Extent"){
  		tmpstack = crop(VIStack,ROI)
  		imageStack = tmpstack
      #imgst=stack(imageStack)
      g=array(, dim=dim(imageStack))
      #g[,,]=imageStack[,,]
      r=1
      for (r in 1:(dim(imageStack)[1])){
          g[r,,]  = imageStack[r,,]
      }
      PhenoArray = array(dim = c((dim(g))[1],(dim(g))[2],15))
      PtArray=array(dim = c((dim(g))[1],(dim(g))[2]))
      for ( r in 1:(dim(g))[1]) {
        for ( c in 1:(dim(g))[2]) {
          t1 <- (as.vector(g[r,c,]))
          p1=t1
          PhenoArray[r,c,] = SinglePhenology(p1, Percentage, Smoothing)
        }
      }
      PhenoStack <- raster(PhenoArray[,,1], template = imageStack)
      for (i in 2:15) {
      	PhenoStack <- stack(PhenoStack,raster(PhenoArray[,,i], template = imageStack))
      }
      names(PhenoStack) <- c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','TINDVIBeforeMax','TINDVIAfterMax','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')
  	}
  	if (class(ROI)== "SpatialPolygonsDataFrame"){
  		tmpstack = crop(VIStack,ROI)
      imageStack = mask(tmpstack,ROI)
      #imgst=stack(imageStack)
      g=array(, dim=dim(imageStack))
      #g[,,]=imageStack[,,]
      r=1
      for (r in 1:(dim(imageStack)[1])){
          g[r,,]  = imageStack[r,,]
      }
      PhenoArray = array(dim = c((dim(g))[1],(dim(g))[2],15))
      PtArray=array(dim = c((dim(g))[1],(dim(g))[2]))
      for ( r in 1:(dim(g))[1]) {
        for ( c in 1:(dim(g))[2]) {
          t1 <- (as.vector(g[r,c,]))
          p1=t1
          PhenoArray[r,c,] = SinglePhenology(p1, Percentage, Smoothing)
        }
      }

      PhenoStack <- raster(PhenoArray[,,1], template = imageStack)
      for (i in 2:15) {
      	PhenoStack <- stack(PhenoStack,raster(PhenoArray[,,i], template = imageStack))
      }
    	names(PhenoStack) <- c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','TINDVIBeforeMax','TINDVIAfterMax','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')

  	}
    if (class(ROI)== "SpatialPointsDataFrame"){
    	crs(ROI)= crs(VIStack)
			pcor = coordinates(ROI)
      ModisCurves = extract(VIStack,pcor)
      ModisCurves[is.na(ModisCurves)] <- 0
      PhenoArray = array(dim = c(nrow(ModisCurves), 15))
      for (i in 1:nrow(ModisCurves)){
        PhenoArray[i,] <- SinglePhenology(ModisCurves[i,],Percentage, FALSE)
      }
      'Onset_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,1]))
      crs(Onset_Value)=crs(VIStack)
      'Onset_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,2]))
      crs(Onset_Time)=crs(VIStack)
      'Offset_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,3]))
      crs(Offset_Value)=crs(VIStack)
      'Offset_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,4]))
      crs(Offset_Time)=crs(VIStack)
      'Max_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,5]))
      crs(Max_Value)=crs(VIStack)
      'Max_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,6]))
      crs(Max_Time)=crs(VIStack)
      'TINDVI'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,7]))
      crs(TINDVI)=crs(VIStack)
      'Area_Before'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,8]))
      crs(Area_Before)=crs(VIStack)
      'Area_After'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,9]))
      crs(Area_After)=crs(VIStack)
      'Asymmetry'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,10]))
      crs(Asymmetry)=crs(VIStack)
      'GreenUpSlope'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,11]))
      crs(GreenUpSlope)=crs(VIStack)
      'BrownDownSlope'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,12]))
      crs(BrownDownSlope)=crs(VIStack)
      'LengthGS'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,13]))
      crs(LengthGS)=crs(VIStack)
      'BeforeMaxT'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,14]))
      crs(BeforeMaxT)=crs(VIStack)
      'AfterMaxT'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,15]))
      crs(AfterMaxT)=crs(ROI)
      #PhenoDataFrame=c(Onset_Value,Onset_Time,Offset_Value,Offset_Time,Max_Value,Max_Time,TINDVI,Area_Before,Area_After,Asymmetry,GreenUpSlope,BrownDownSlope,LengthGS,BeforeMaxT,AfterMaxT)
      #names(PhenoStack) = c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')



      cnames = c('x','y', 'Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','TINDVIBeforeMax','TINDVIAfterMax','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')
      PhenoDataframe = data.frame(cbind(pcor[,1], pcor[,2],PhenoArray))
      PointDataframe= data.frame(cbind(pcor[,1], pcor[,2], ModisCurves))
      colnames(PhenoDataframe) = cnames
      return(PhenoDataframe)

      #dir.create("Metrics")
      #setwd(paste(getwd(), "Metrics", sep="/"))
      #getwd()
      #write.csv(PhenoDataframe,"Pheno_table.csv")
      #write.csv(PointDataframe, "AllPixels.csv")

      #print ("output metrics for the point data is saved at 'Metrics' folder as 'Pheno_table.csv'")
      #stop()
    }
    if (class(ROI)== "SpatialPoints"){

    	crs(ROI)= crs(VIStack)
			pcor = coordinates(ROI)
      ModisCurves = extract(VIStack,pcor[,1:2])
      ModisCurves[is.na(ModisCurves)] <- 0
      PhenoArray = array(dim = c(nrow(ModisCurves), 15))
      for (i in 1:nrow(ModisCurves)){
        PhenoArray[i,] <- SinglePhenology(ModisCurves[i,],Percentage, FALSE)
      }
      'Onset_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,1]))
      crs(Onset_Value)=crs(VIStack)
      'Onset_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,2]))
      crs(Onset_Time)=crs(VIStack)
      'Offset_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,3]))
      crs(Offset_Value)=crs(VIStack)
      'Offset_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,4]))
      crs(Offset_Time)=crs(VIStack)
      'Max_Value'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,5]))
      crs(Max_Value)=crs(VIStack)
      'Max_Time'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,6]))
      crs(Max_Time)=crs(VIStack)
      'TINDVI'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,7]))
      crs(TINDVI)=crs(VIStack)
      'Area_Before'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,8]))
      crs(Area_Before)=crs(VIStack)
      'Area_After'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,9]))
      crs(Area_After)=crs(VIStack)
      'Asymmetry'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,10]))
      crs(Asymmetry)=crs(VIStack)
      'GreenUpSlope'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,11]))
      crs(GreenUpSlope)=crs(VIStack)
      'BrownDownSlope'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,12]))
      crs(BrownDownSlope)=crs(VIStack)
      'LengthGS'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,13]))
      crs(LengthGS)=crs(VIStack)
      'BeforeMaxT'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,14]))
      crs(BeforeMaxT)=crs(VIStack)
      'AfterMaxT'<- SpatialPoints(data.frame(cbind(pcor[,1], pcor[,2]),PhenoArray[,15]))
      crs(AfterMaxT)=crs(ROI)
      #PhenoDataFrame=c(Onset_Value,Onset_Time,Offset_Value,Offset_Time,Max_Value,Max_Time,TINDVI,Area_Before,Area_After,Asymmetry,GreenUpSlope,BrownDownSlope,LengthGS,BeforeMaxT,AfterMaxT)
      #names(PhenoStack) = c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')



      cnames = c('x','y', 'Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','TINDVIBeforeMax','TINDVIAfterMax','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')


      PhenoDataframe = data.frame(cbind(pcor[,1], pcor[,2],PhenoArray))
      PointDataframe= data.frame(cbind(pcor[,1], pcor[,2], ModisCurves))
      colnames(PhenoDataframe) = cnames
      return(PhenoDataframe)
    }
  }

  if (is.null(ROI) == TRUE){
  	imageStack= VIStack

    imageArray <- as.array(VIStack)
    PhenoArray = array(dim = c(dim(imageArray)[1],dim(imageArray)[2],15))

    for ( r in 1:dim(imageArray)[1]) {
      for ( c in 1:dim(imageArray)[2]) {
        p1 <- as.vector(imageArray[r,c,])
        PhenoArray[r,c,] = SinglePhenology(p1,Percentage, Smoothing)
      }
    }
    PhenoStack <- raster(PhenoArray[,,1], template = imageStack)
    for (i in 2:15) {
      PhenoStack <- stack(PhenoStack,raster(PhenoArray[,,i], template = imageStack))
    }

    names(PhenoStack) <- c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','TINDVIBeforeMax','TINDVIAfterMax','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')

  }
	return(PhenoStack)
  }
#===============================================================================================================


#===================================================================================================
#===============================================================================================================
#                                     PhenoPlot Function
#===============================================================================================================
# PhenoPlot - plots the raster phenological metrics returned from PhenoMetrics function
#' @export
#' @return plots  15 phenological metrics
#' @title plot phenological metrics in a raster format Phenology plot per pixel
#' @name PhenoPlot
#' @param PhenoStack - RasterStack object of phenological metrics
#' @description PhenoPlot takes the output file of the PhenoMetrics function and plot the raster matrics.
#' @seealso MultiPointsPlot, PhenoMetrics, SinglePhenology
#'
SinglePhenology <- function (PhenoStack) {
	par(mfrow=c(2,2))
  OT=PhenoStack$Onset_Time
  crs(OT)<-crs(ROI)
  brk=seq(2,16, by=0.01)
  nbrk=length(brk)
  plot(OT, main="OnsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(2,16,by=2), labels=seq(2,16,by=2)), zlim=c(2,16))

  OV=PhenoStack$Onset_Value
  brk=seq(0.1,0.6, by=0.001)
  nbrk=length(brk)
  crs(OV)<-crs(ROI)
  plot(OV, main="OnsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.1,0.6,by=0.2), labels=seq(0.1,0.6,by=0.2)), zlim=c(0.1,0.6))

  MT=PhenoStack$Max_Time
  crs(MT)<-crs(ROI)
  brk=seq(8,19, by=1)
  nbrk=length(brk)
  lblbrk=brk
  plot(MT, main="MaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(8,19,by=2), labels=seq(8,19,by=2)),zlim=c(8,19))

  MV=PhenoStack$Max_Value
  crs(MV)<-crs(ROI)
  brk=seq(0.2,1, by=0.1)
  nbrk=length(brk)
  plot(MV, main="MaxV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.2,1,by=0.2), labels=seq(0.2,1,by=0.2)), zlim=c(0.2,1))

  OFT=PhenoStack$Offset_Time
  crs(OFT)<-crs(ROI)
  brk=seq(16,23, by=0.01)
  nbrk=length(brk)
  plot(OFT, main="OffsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(16,23,by=2), labels=seq(16,23,by=2)), zlim=c(16,23))

  OFV=PhenoStack$Offset_Value
  crs(OFV)<-crs(ROI)
  brk=seq(0,0.4, by=0.001)
  nbrk=length(brk)
  plot(OFV, main="OffsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.4,by=0.1), labels=seq(0,0.4,by=0.1)), zlim=c(0,0.4))

  GUS=PhenoStack$GreenUpSlope
  crs(GUS)<-crs(ROI)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(GUS, main="GreenUpSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))

  BDS=PhenoStack$BrownDownSlope
  crs(BDS)<-crs(ROI)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(BDS, main="BrownDownSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))

  BefMaxT=PhenoStack$BeforeMaxT
  crs(BefMaxT)<-crs(ROI)
  brk=seq(0,19, by=0.01)
  nbrk=length(brk)
  plot(BefMaxT, main="BeforeMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,19,by=2), labels=seq(0,19,by=2)), zlim=c(0,19))

  AftMaxT=PhenoStack$AfterMaxT
  crs(AftMaxT)<-crs(ROI)
  brk=seq(0,12, by=0.01)
  nbrk=length(brk)
  plot(AftMaxT, main="AfterMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))

  Len=PhenoStack$LengthGS
  crs(Len)<-crs(ROI)
  brk=seq(6,23, by=0.1)
  nbrk=length(brk)
  plot(Len, main="LengthGS", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(6,23,by=2), labels=seq(6,23,by=2)), zlim=c(6,23))

  AA=PhenoStack$TINDVIAfterMax
  crs(AA)<-crs(ROI)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AA, main="TINDVIAfterMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))

  AB=PhenoStack$TINDVIBeforeMax
  crs(AB)<-crs(ROI)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AB, main="TINDVIBeforeMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))

  AT=PhenoStack$TINDVI
  crs(AT)<-crs(ROI)
  brk=seq(0,8, by=0.001)
  nbrk=length(brk)
  plot(AT, main="TINDVI", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,8,by=2), labels=seq(0,8,by=2)), zlim=c(0,8))

  As=PhenoStack$Asymmetry
  crs(As)<-crs(ROI)
  brk=seq(-6,6, by=0.0001)
  nbrk=length(brk)
  plot(As, main="Asymmetry", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(-6.0,6.0,by=3), labels=seq(-6.0,6.0,by=3)), zlim=c(-6,6))
}


#===============================================================================================================
#                                     SinglePhenology Function
#===============================================================================================================
# SinglePhenology - calculates phenologic metrics for each pixel and return to the PhenoMetrics function
#' @export
#' @return return phenologic metrics for a single pixel as an array in the sequence of: OnsetV, OnsetT, OffsetV, OffsetT, MAxV, MaxT, TINDVI, TINDVIBeforeMax, TINDVIAfeterMax, Assymetry, GreenUpSlope, BrownDownSlope, LengthGS, BeforeMaxT, AfterMaxT".
#' @title Phenology plot per pixel
#' @name SinglePhenology
#' @param AnnualTS - annual time series
#' @param Percentage - the percentage threshold for Onset and Offset
#' @param Smoothing - moving average smoothing applied if TRUE
#' @description SinglePhenology is a premitive function which takes a time series vegetation index data for a single pixel for a single season.
#' @seealso MultiPointsPlot, PhenoMetrics
#'
#' #Example
#' Point <- c(0.2052, 0.1824, 0.1780, 0.1732, 0.1861, 0.1863, 0.1884, 0.2202, 0.2669, 0.2708, 0.3700, 0.5900, 0.6909, 0.6057, 0.6750, 0.5572, 0.4990, 0.3463, 0.2579, 0.2167, 0.1672, 0.1771, 0.1856)
#' SinglePhenology(Point)
SinglePhenology <- function (AnnualTS, Percentage=NULL, Smoothing = FALSE) {
	 #===========================================================================
  if (is.null(Percentage)==TRUE) {
    print ("The default Threshold Percentage value, 20%, will be applied")
    Percentage=20
  }
  if (!is.numeric(Percentage)){
    stop("Percentage value for Onset and Offset should be numeric")
  }
  if (Percentage<0){
    stop("Negative Onset-Offset percentage specified")
  }
  if (Percentage==0){
    stop("percentage Thrshold should be greated than 0")
  }
  #===========================================================================
  if(sum(is.na(AnnualTS)) > 0) {
    PVector = rep(NA,15)
    return(PVector)
  }
  #========================================
  sq=2
  ll=length(AnnualTS)
  Enter = FALSE
  FileLen = ll
  SmthTS = vector(length=length(AnnualTS))
  SmthTS[1]=AnnualTS[1]
  while (sq<ll){
    SmthTS[sq]=(AnnualTS[sq]+AnnualTS[sq+1])/2
    sq=sq+1
  }
  SmthTS[ll]=AnnualTS[ll]
  #    print (AnnualTS)
  #    print (SmthTS)
  aas=ts(AnnualTS)
  ssm=ts(SmthTS)
  #    ts.plot(ssm, aas, col=1:2)
  #=========================================


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                 Choose smooth or unsmooth
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (Smoothing==TRUE){
    Curve=SmthTS
  }
  if (Smoothing==FALSE){
    Curve=AnnualTS
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                  Maximum
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  qmax=1
  max=Curve[qmax]
  Max_T=qmax

  while (qmax>0 & qmax<FileLen){
    if ((Curve[qmax]) > max){
      max=Curve[qmax]
      Max_T=qmax
    }
    qmax=qmax+1
  }
  Max_TF=Max_T
  Max_Value=max
  Max_Time=Max_TF


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                  Onset
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  onsetT=0
  onsetV=0
  # successive slops b/n  points
  j=Max_T
  slopon=(Curve[j]-Curve[j-1])
  slopon=as.matrix(slopon)
  f=2


  while(j >2){
    slopon[f]=(Curve[j-1]-Curve[j-2])
    j=j-1
    f=f+1
  }
  ratio=Percentage/100
  min1=min (Curve[1:Max_T])
  min2=min(Curve[Max_T:FileLen])
  range1=min1+(ratio*min1) #to get 10% of the min before Max
  range2=min2+(ratio*min2)#to get 10% of the min after Max
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                            Last -ve slope- onset
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  len=length(slopon)
  last=slopon[1]
  i=1
  #i=len
  ls=0
  while((i<(len+1)) & (i>0)){
    if (last<0.001){
      #print(last)
      if (last< 0.001){
        #print(last)
        ls=i
        break
      }
      quick=i
    }
    i=i+1
    last=slopon[i]
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (ls==0){ #if only the growing season is presented and only increasing
    k=1
    Checked= FALSE
    while (k<Max_T){
      if (Curve[k]< range1){
        onsetT=k
        onsetV=Curve[k]
        Checked=TRUE
      }
      k=k+1
    }
    if (Checked==FALSE){
      onsetT=1
      onsetV=Curve[1]
    }
  }


  if (ls>0){
    ko=Max_T-ls
    if (Curve[ko]<range1){
      onsetT=ko
      onsetV=Curve[ko]
    }
    if (Curve[ko]>range1){
      p=ls
      Enter=FALSE
      while (p<(length(slopon)+1)){
        if (Curve[Max_T-p]<range1){
          onsetT=Max_T-p
          onsetV=Curve[Max_T-p]
          Enter=TRUE
          break
        }
        p=p+1
      }
    }
  }

  if (Enter==FALSE){
    p=Max_T-ls
    while (p<Max_T){
      if (Curve[p]<range1){
        onsetT=p
        onsetV=Curve[p]
      }
      p=p+1
    }
  }

  onsetTF=onsetT
  Onset_Value=onsetV
  Onset_Time=onsetTF
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                  Offset
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  offsetT=0
  offsetV=0
  crp=TRUE
  z=Max_T+1
  slopof=(Curve[Max_T+1]-Curve[Max_T])
  slopof=as.matrix(slopof)
  y=2

  while (z<(length(Curve))){
    slopof[y]=(Curve[z+1]-Curve[z])
    z=z+1
    y=y+1
  }

  #print (slopof)
  #print(range2)

  lenof=length(slopof)
  lastof=slopof[lenof]

  i=1
  #i=len
  lsof=0


  while(i<lenof+1){
    if (lastof>(-0.01)){
      #print(last)
      if (lastof> (-0.01)){
        #print(last)
        lsof=i
        break
      }
      quick=i
    }
    i=i+1
    lastof=slopof[i]
  }



  if (lsof==0){ #if only the growing season is presented and only increasing
    k=Max_T+1
    Checked= FALSE
    while (k<(length(Curve)+1)){
      if (Curve[k]< range2){
        offsetT=k
        offsetV=Curve[k]
        Checked=TRUE
      }
      k=k+1
    }
    if (Checked==FALSE){
      offsetT=length(Curve)
      offsetV=Curve[offsetT]
    }
  }

  kof=(Max_T+lsof-1)
  if (lsof>0){
    if (Curve[kof]<range2){
      offsetT=kof
      offsetV=Curve[kof]
    }
    if (Curve[kof]>range2){
      p=lsof
      Enter=FALSE
      while (p<length(slopof)){
        if ((slopof[p]>(-0.01)) & (Curve[Max_T+p-1]<range2)){
          offsetT=Max_T+p-1
          offsetV=Curve[Max_T+p-1]
          Enter=TRUE
          break
        }
        p=p+1
      }
    }
    if (Enter==FALSE){
      p=Max_T+lsof-1
      while (p<(length(Curve)+1)){
        if (Curve[p]<range2){
          offsetT=p
          offsetV=Curve[p]
        }
        p=p+1
      }
    }
  }

  if ((max-offsetV)==0) {
    crp=FALSE
    offsetT=length(Curve)
    offsetV=Curve[offsetT]
  }

  #print (lsof)

  #print (offsetV)
  #print(offsetT)
  offsetTF=offsetT

  Offset_Value=offsetV
  Offset_Time= offsetTF


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                Area
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  St=abs(round (onsetT))
  Ed=abs(round(offsetT))
  Area=0
  start=St
  end=Ed+1
  mx=Max_T

  if (St<=0) {
    start=9
    start1=9
    start2=9
  }

  if (crp==FALSE){
    Area=0
    Area1=0
    Area2=0
  }
  while (start<end){
    Area=Area+Curve[start]
    start=start+1
  }
  Area_Total=Area


  start1=St
  Area1=Curve[start1]/2
  start1=start1+1
  while (start1<mx){
    Area1=Area1+Curve[start1]
    start1=start1+1
  }
  Area1=Area1+(Curve[mx]/2)

  if (onsetT==0){ Area1=0}
  if (Area==0){ Area1=0}
  Area_Before=Area1

  Area2=Curve[mx]/2
  start2=mx+1
  while (start2<(end)){
    Area2=Area2+Curve[start2]
    start2=start2+1
  }
  Area2=Area2+Curve[Ed]/2
  if (Area==0){ Area2=0}
  Area_After=Area2

  Asy=Area1-Area2
  Asymmetry=Asy
  PVector = vector(length=15)

  PVector[1] = Onset_Value
  PVector[2] = Onset_Time
  PVector[3] = Offset_Value
  PVector[4] = Offset_Time
  PVector[5] = Max_Value
  PVector[6] = Max_Time
  PVector[7] = Area_Total
  PVector[8] = Area_Before
  PVector[9] = Area_After
  PVector[10] = (Area_Before - Area_After)
  PVector[11] = (Max_Value - Onset_Value) / (Max_Time - Onset_Time)             # GreenUpSlope
  PVector[12] = (Max_Value - Offset_Value) / (Offset_Time - Max_Time)           # BrownDownSlope
  PVector[13] = Offset_Time - Onset_Time                                        # LengthGS
  PVector[14] = Max_Time - Onset_Time                                           # BeforeMaxT
  PVector[15] = Offset_Time - Max_Time                                          # AfterMaxT


  return(PVector)
  print("The phenologic metrics in the sequence of: OnsetV, OnsetT, OffsetV, OffsetT, MAxV, MaxT, TINDVI, TINDVIBeforeMax, TINDVIAfeterMax, Assymetry, GreenUpSlope, BrownDownSlope, LengthGS, BeforeMaxT, AfterMaxT")
}
#===============================================================================================================



#===============================================================================================================
#                                     MultiPointsPlot Function
#===============================================================================================================

#' @export
#' @return Multiple time series curves together at the plot panel
#' @title Time series curves for Multiple points in the Region of Interest
#' @description MultiPointsPlot function allows the user to plot of vegetation dynamics curves from multiple points.
#' @keywords Dynamics Curves
#' @keywords  multiple points
#' @keywords time-series curves
#' @details Plotting dynamics curves from multiple points together in a single plot helps understanding the growth variability across the field.This inforaiton allow observation of the spatial and temporal crop growth variability across the growth seasons, which provide important information about the environmental factors influencing crop growth and thus potential opportunities for influencing crop management (eg . Araya et al., 2016)
#' @param VIStack - RasterStack of time series vegetation index images
#'
#' #EXAMPLE
#' ExampleStack<- stack(system.file("extdata", "ExampleStack.grd", package="CropPhenology"))
#' MultiPointsPlot(ExampleStack)
#'
#' @seealso PhenoMetrics, SinglePhenology
#'
MultiPointsPlot<- function (VIStack){

  # #AP=read.table("Allpixels.txt")
  # setwd(path)
  # AP=read 4z.table("AllPixels.csv", header=TRUE, sep=",", strip.white = TRUE)
  # APP=as.matrix(AP[Id1,])
  # #print (APP)
  par(mfrow=c(1,1))
  dis=VIStack[[1]]
  plot(dis)
  PList=click(dis, n=5, cell=TRUE, xy=TRUE, show=FALSE)
  N=length(PList)
  if (is.null(PList)==FALSE ) {
  	N=length(PList)
  	if (N > 5) {
  		warning ('The maximum No of pixel to plot is 5')
  	}
  	i=1
  	cur=ts(1:nlayers(VIStack))
  	for (i in 1:N){
  		#print (i)
  		id1=PList$cell[i]
  		cur[1]=extract(VIStack[[1]], id1)
  		j=2
  		for (j in 2:nlayers(VIStack)){
  			cur[j]=extract(VIStack[[j]], id1)
  			#print (cur[j])
  		}
  		#print (cur)
  		#print (N)
  		assign(paste0("Curve",i), cur)
  	}
  }

	if (N==1){
		ts.plot(ts(Curve1), col=1)
	}
  if (N==2){
  	ts.plot(Curve1, Curve2, col=c(1:2))
  }
  if (N==3){
  	ts.plot(Curve1, Curve2, Curve3, col=c(1:3))
  }
  if (N==4){
  	ts.plot(Curve1, Curve2, Curve3, Curve4, col=c(1:4))
  }
  if (N==5){
  	ts.plot(Curve1, Curve2, Curve3, Curve4, Curve5, col=c(1:5))
  }
}
