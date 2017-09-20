#======================================================================================================================================
#                                     PhenoMetrics Function
#======================================================================================================================================
#' Phenologic metrics from time series vegetation index data
#'
#' @return  PhenoStack.img - a raster stack of 15 images in the order of OnsetV, OnsetT, MaxV, MaxT, OffsetV, OffsetT, LengthGS, BeforeMaxT, AfterMaxT, GreenUpSlope, BrownDownSlope, TINDVI, TINDVIBeforeMax, TINDVIAfterMax, Asymmetry
#' @keywords Phenology
#' @keywords remote sensing
#' @keywords satellite image
#' @keywords Time-series
#' @seealso MultiPointsPlot (Path, N,Id1, Id2...IdN)
#' @description This function extracts 15 phenologic metrics from time series vegetaion index data, as raster and Ascii files. The function takes path of the vegetation index data and the boolean Value for BolAOI (True- if there is AOI polygon, FALSE- if the parameters are calculated for the whole region).
#' @param Path - Text value - the path where the time series images saved
#' @param BolAOI - Logical value - if there is any area of intererst or not
#' @param Percentage - Optional Numeric Vlaue - percentage of minimum NDVI value at which the Onset and Offset is defined. The 'Percentage' paramenter is optional; if not provided, a Default value of 10 will be taken.
#' @param Smoothing - Optional logical value - if the user chooses to use smoothed curve or row/unsmoothed curve. If "Smoothing' is set to TRUE, the moving avegare filter will be applied to the vegetation index curve. The default value, if not provided, is FALSE, then the unsmoothed row data be used for the analysis.
#'
#' @export
#' @examples
#' #EXAMPLE - 1
#'
#' PhenoMetrics(system.file("extdata/data1", package="CropPhenology"), FALSE, 15, TRUE)
#'
#' #EXAMPLE - 2
#'
#' PhenoMetrics(system.file("extdata/data2", package="CropPhenology"), TRUE)
#'
#'

PhenoMetrics<- function (Path, BolAOI, Percentage, Smoothing){
	require(xlsx)

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

  shp=0
  NMAOI=0
  AOI=0

  #===========================================================================
  if (missing(Percentage)) {
    print ("The default value, 10%, will be applied")
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


  setwd(Path)
  raDir=dir(path=Path, pattern = c(".img$|.tif$"))
  FileLen=length(raDir)
  allras <- list.files(pattern = c(".img$|.tif$"))
  hugeStack = stack(allras)
  q=1
  qon=1
  qoff=1
  qmax=1
  par(mfrow=c(1,1))
  par(mar=c(3.5, 2.5, 2.5, 5.5))
  s=1
  Enter=FALSE

  if (BolAOI == TRUE){
    AOI=dir(pattern="*.shp$")
#    extent(AOI)=extent(hugeStack)
    NMAOI=sub(".shp","",AOI, fixed=TRUE)
    shp=readOGR(Path, NMAOI)
    #shp=readShapePoly(AOI)
    if (class(shp)=="SpatialPointsDataFrame"){
      #points = readOGR(Path,"EP_Apsoil")
      pTrans = spTransform(shp, crs(hugeStack))
      pcor = coordinates(pTrans)
      ModisCurves = extract(hugeStack,pcor[,1:2]) / 10000
      PhenoArray = array(dim = c(nrow(ModisCurves), 15))

      for (i in 1:nrow(ModisCurves)){
        PhenoArray[i,] <- SinglePhenology(ModisCurves[i,],15, FALSE)
      }
      cnames = c('x','y', 'Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','Area_Total','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')

      PhenoDataframe = data.frame(cbind(pcor[,1], pcor[,2],PhenoArray))
      PointDataframe= data.frame(cbind(pcor[,1], pcor[,2], ModisCurves))
      colnames(PhenoDataframe) = cnames

      dir.create("Metrics")
      setwd(paste(getwd(), "Metrics", sep="/"))
      getwd()

      write.csv(PhenoDataframe,"Pheno_table.csv")
      write.csv(PointDataframe, "AllPixels.csv")

      print ("output metrics for the point data is saved at 'Metrics' folder as 'Pheno_table.csv'")
      stop()
    }

    if (class(shp)=="SpatialPolygonsDataFrame"){
      #temp=raDir[1]
      #shp = readOGR(Path,"Bon")
      tmpstack = crop(hugeStack,shp)
      imageStack = mask(tmpstack,shp)
      #imgst=stack(imageStack)
      g=array(, dim=dim(imageStack))
      #g[,,]=imageStack[,,]
      r=1
      for (r in 1:(dim(imageStack)[1])) {
          g[r,,]  = imageStack[r,,]
      }


      #imageArray = as.array(imageStack[,,])
      PhenoArray = array(dim = c((dim(g))[1],(dim(g))[2],15))
      PtArray=array(dim = c((dim(g))[1],(dim(g))[2]))
      for ( r in 1:(dim(g))[1]) {
        for ( c in 1:(dim(g))[2]) {
          t1 <- (as.vector(g[r,c,]))
          p1=t1/10000
          PhenoArray[r,c,] = SinglePhenology(p1, Percentage, Smoothing)
        }
      }
      pts=rasterToPoints(imageStack)

      PhenoStack <- raster(PhenoArray[,,1], template = imageStack)
      for (i in 2:15) {
        PhenoStack <- stack(PhenoStack,raster(PhenoArray[,,i], template = imageStack))
      }
      names(PhenoStack) = c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')
    }

  }

  dir.create("Metrics")
  setwd(paste(getwd(), "Metrics", sep="/"))

  print(getwd())
  if (class(shp)=="SpatialPolygonsDataFrame"){
  	crs(PhenoStack)=crs(hugeStack)
  	writeRaster(PhenoStack,"PhenoStack.img")
  	ppt=pts
  	d=(dim(g))[3]+2
  	pts[,3:d]=pts[,3:d]/10000
  	write.csv(pts, "AllPixels.csv")
  }


  if (BolAOI == FALSE){
    ra=raster(raDir[1])
    Points=rasterToPoints(ra)
    shp=rasterToPolygons((ra*0), dissolve=TRUE)
    temp=raDir[1]
    #    shp = readOGR(Path,"Bon")
    tmpstack = crop(hugeStack,shp)
    imageStack = mask(tmpstack,shp)
    imageArray <- as.array(imageStack)
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

    names(PhenoStack) <- c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')

    dir.create("Metrics")
    setwd(paste(getwd(), "Metrics", sep="/"))

    print(getwd())
    writeRaster(PhenoStack,"PhenoStack.img")
  }


  coords = rasterToPoints(imageStack)
  # Number of pixels:
  nrow(coords)
  # MODIS time series of pixel 1
  p1 <- coords[1,3:ncol(coords)]
  for (i in 1:nrow(coords)){
    plot(coords[i,3:ncol(coords)])
  }
  #===========================================


  #===================================================================================================

  par(mfrow=c(2,2))
  OT=PhenoStack$Onset_Time
  crs(OT)<-crs(shp)
  brk=seq(2,16, by=0.01)
  nbrk=length(brk)
  plot(OT, main="OnsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(2,16,by=2), labels=seq(2,16,by=2)), zlim=c(2,16))
  writeRaster(OT, "OnsetT.img", overwrite=TRUE)

  OV=PhenoStack$Onset_Value
  brk=seq(0.1,0.6, by=0.001)
  nbrk=length(brk)
  crs(OV)<-crs(shp)
  plot(OV, main="OnsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.1,0.6,by=0.2), labels=seq(0.1,0.6,by=0.2)), zlim=c(0.1,0.6))
  writeRaster(OV, "OnsetV.img", overwrite=TRUE)

  MT=PhenoStack$Max_Time
  crs(MT)<-crs(shp)
  brk=seq(8,19, by=1)
  nbrk=length(brk)
  lblbrk=brk
  plot(MT, main="MaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(8,19,by=2), labels=seq(8,19,by=2)),zlim=c(8,19))
  writeRaster(MT, "MaxT.img", overwrite=TRUE)


  MV=PhenoStack$Max_Value
  crs(MV)<-crs(shp)
  brk=seq(0.2,1, by=0.1)
  nbrk=length(brk)
  plot(MV, main="MaxV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.2,1,by=0.2), labels=seq(0.2,1,by=0.2)), zlim=c(0.2,1))
  writeRaster(MV, "MaxV.img", overwrite=TRUE)

  OFT=PhenoStack$Offset_Time
  crs(OFT)<-crs(shp)
  brk=seq(16,23, by=0.01)
  nbrk=length(brk)
  plot(OFT, main="OffsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(16,23,by=2), labels=seq(16,23,by=2)), zlim=c(16,23))
  writeRaster(OFT, "OffsetT.img", overwrite=TRUE)

  OFV=PhenoStack$Offset_Value
  crs(OFV)<-crs(shp)
  brk=seq(0,0.4, by=0.001)
  nbrk=length(brk)
  plot(OFV, main="OffsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.4,by=0.1), labels=seq(0,0.4,by=0.1)), zlim=c(0,0.4))
  writeRaster(OFV, "OffsetV.img", overwrite=TRUE)

  c('Onset_Value','Onset_Time','Offset_Value','Offset_Time','Max_Value','Max_Time','TINDVI','Area_Before','Area_After','Asymmetry','GreenUpSlope','BrownDownSlope','LengthGS','BeforeMaxT','AfterMaxT')

  GUS=PhenoStack$GreenUpSlope
  crs(GUS)<-crs(shp)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(GUS, main="GreenUpSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
  writeRaster(GUS, "GreenUpSlope.img", overwrite=TRUE)

  BDS=PhenoStack$BrownDownSlope
  crs(BDS)<-crs(shp)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(BDS, main="BrownDownSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
  writeRaster(BDS, "BrownDownSlope.img", overwrite=TRUE)

  BefMaxT=PhenoStack$BeforeMaxT
  crs(BefMaxT)<-crs(shp)
  brk=seq(0,12, by=0.01)
  nbrk=length(brk)
  plot(BefMaxT, main="BeforeMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
  writeRaster(BefMaxT, "BeforeMaxT.img", overwrite=TRUE)

  AftMaxT=PhenoStack$AfterMaxT
  crs(AftMaxT)<-crs(shp)
  brk=seq(0,12, by=0.01)
  nbrk=length(brk)
  plot(AftMaxT, main="AfterMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
  writeRaster(AftMaxT, "AfterMaxT.img", overwrite=TRUE)

  Len=PhenoStack$LengthGS
  crs(Len)<-crs(shp)
  brk=seq(6,17, by=0.1)
  nbrk=length(brk)
  writeRaster(Len, "LengthGS.img", overwrite=TRUE)
  plot(Len, main="LengthGS", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(6,17,by=2), labels=seq(6,17,by=2)), zlim=c(6,17))

  AA=PhenoStack$Area_After
  crs(AA)<-crs(shp)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AA, main="TINDVIAfterMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
  writeRaster(AA, "TINDVIAfterMax.img", overwrite=TRUE)

  AB=PhenoStack$Area_Before
  crs(AB)<-crs(shp)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AB, main="TINDVIBeforeMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
  writeRaster(AB, "TINDVIBeforeMax.img", overwrite=TRUE)


  AT=PhenoStack$TINDVI
  crs(AT)<-crs(shp)
  brk=seq(0,8, by=0.001)
  nbrk=length(brk)
  plot(AT, main="TINDVI", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,8,by=2), labels=seq(0,8,by=2)), zlim=c(0,8))
  writeRaster(AT, "TINDVI.img", overwrite=TRUE)


  As=PhenoStack$Asymmetry
  crs(As)<-crs(shp)
  brk=seq(-6,6, by=0.0001)
  nbrk=length(brk)
  plot(As, main="Asymmetry", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(-6.0,6.0,by=3), labels=seq(-6.0,6.0,by=3)), zlim=c(-6,6))
  writeRaster(As, "Asymmetry.img", overwrite=TRUE)


  ##########################====================================##########################

  return("*****Output file saved under <Metrics> folder under directory*****")

  ##########################====================================##########################
}
#===============================================================================================================



#===============================================================================================================
#                                     SinglePhenology Function
#===============================================================================================================

SinglePhenology <- function(AnnualTS, Percentage = 20, Smoothing = FALSE) {
  if(sum(is.na(AnnualTS)) > 0) {
    PVector = rep(NA,15)
    return(PVector)
  }
  #========================================
  sq=2
  ll=length(AnnualTS)
  # SOFI - need to check FileLen and Enter initial values
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
  min2=min(Curve[Max_T:(FileLen-1)])
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
  slopof=(Curve[[Max_T+1]]-Curve[[Max_T]])
  slopof=as.matrix(slopof)
  y=2

  #slopof - is length of the slope b/n points from Max to Offset

  while (z<(length(Curve))){
    slopof[y]=(Curve[[z+1]]-Curve[[z]])
    z=z+1
    y=y+1
  }

  lenof=length(slopof)
  lastof=slopof[lenof]
#lastof - the last posetive slope

  i=lenof
  #i=len
  lsof=0

    while(i>0){
    if (lastof>(-0.01)){
      #print(last)
      #print(last)
      lsof=i
      break
      quick=i
    }
    i=i-1
    lastof=slopof[i]
  }

  DD=lenof-(lsof-1)
  if (lsof!=0){
  	offsetV=Curve[[length(Curve)-DD]]
  	offsetTF=length(Curve)-DD
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
#' @param path - the path whee AllPixel.txt saved
#' @param N - number of intersted points
#' @param Id1 -  ID number for point 1
#' @param Id2 -  Id number for point 2
#' @param Id3 -  ID number for point 3
#' @param Id4 -  ID number for point 4
#' @param Id5 -  ID number for point 5
#' @title Time series curves for Multiple points in the Region of Interest
#' @description MultiPointsPlot function takes the ID for the pixels within the region of interst and returns, the timeseries curves from these points, ploted together. The Id numbers can be obtained from the txt file (AllPixels.txt) outputs.
#' @keywords Curve from multiple points
#' @keywords time-series curves
#' @details This function allows plotting time series curves from multiple points together in a single plot which helps understanding the growth variability across the field.This inforaiton allow observation of the spatial and temporal crop growth variability across the growth seasons, which provide important information about the environmental factors influencing crop growth and thus potential opportunities for influencing crop management (eg . Araya et al., 2016)
#' @details The maximum number of pixeles allowed plotting togther are 5 points.
#'
#'
#'
#' @seealso PhenoMetrics()
#'
MultiPointsPlot<- function (path, N,Id1,Id2,Id3,Id4,Id5){
  #AP=read.table("Allpixels.txt")
  setwd(path)
  AP=read.table("AllPixels.csv", header=TRUE, sep=",", strip.white = TRUE)
  APP=as.matrix(AP[Id1,])
  #print (APP)
  par(mfrow=c(1,1))

  LTS=ncol(AP)
  LDT=nrow(AP)
  if (N>5){
    warning ('The maximum No of pixel to plot is 5')


    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }


    if (missing (Id1) | missing(Id2) | missing (Id3) | missing (Id4) | missing (Id5)){
      stop('Id missed')
    }
    ts.plot((ts(as.matrix(AP[Id1,])[4:LTS])), (ts(as.matrix(AP[Id2,])[4:LTS])), (ts(as.matrix(AP[Id3,])[4:LTS])), (ts(as.matrix(AP[Id4,])[4:LTS])), (ts(as.matrix(AP[Id5,])[4:LTS])),  ylim=c(0,1), col=1:5)
    axis(2, at=seq(0,1,by=0.1))

  }


  if (N==1){
    warning('only one pixel ploted')


    if ((is.numeric(Id1)==FALSE) ){
      stop ('ID should be numeric')
    }



    if ((Id1>LDT)){
      stop ('Id out of range')
    }

    ts.plot ((ts(as.matrix(AP[Id1,])[4:LTS])), ylim=c(0,1))
    axis(2, at=seq(0,1,by=0.1))
  }

  if (N==2){
    if (missing (Id1) || missing(Id2)){
      stop('Id missed')
    }


    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) ){
      stop ('ID should be numeric')
    }


    if ((Id1>LDT) || (Id2>LDT)){
      stop ('Id out of range')
    }

    ts.plot ((ts(as.matrix(AP[Id1,])[4:LTS])), (ts(as.matrix(AP[Id2,])[4:LTS])), ylim=c(0,1), col=1:2)
    axis(2,  at=seq(0,1,by=0.1))
  }
  if (N==3){
    if ((missing (Id1)) || (missing(Id2)) || (missing (Id3))){
      stop ("Id missed")
    }


    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) ){
      stop ('ID should be numeric')
    }


    if ((Id1>LDT) || (Id2>LDT) || (Id3>LDT)){
      stop ('Id out of range')
    }

    ts.plot ((ts(as.matrix(AP[Id1,])[4:LTS])), (ts(as.matrix(AP[Id2,])[4:LTS])), (ts(as.matrix(AP[Id3,])[4:LTS])), ylim=c(0,1), col=1:3)
    axis(2,  at=seq(0,1,by=0.1))
  }

  if (N==4){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4)){
      stop('Id missed')
    }


    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) ){
      stop ('ID should be numeric')
    }


    if ((Id1>LDT) || (Id2>LDT) || (Id3>LDT) || (Id4>LDT)){
      stop ('Id out of range')
    }

    ts.plot ((ts(as.matrix(AP[Id1,])[4:LTS])), (ts(as.matrix(AP[Id2,])[4:LTS])), (ts(as.matrix(AP[Id3,])[4:LTS])), (ts(as.matrix(AP[Id4,])[4:LTS])),  ylim=c(0,1), col=1:4)
    axis(2, at=seq(0,1,by=0.1))
  }
  if (N==5){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4) || missing (Id5)){
      stop('Id missed')
    }


    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }


    if ((Id1>LDT) || (Id2>LDT) || (Id3>LDT) || (Id4>LDT) || (Id5>LDT) ){
      stop ('Id out of range')
    }

    ts.plot ((ts(as.matrix(AP[Id1,])[4:LTS])), (ts(as.matrix(AP[Id2,])[4:LTS])), (ts(as.matrix(AP[Id3,])[4:LTS])), (ts(as.matrix(AP[Id4,])[4:LTS])), (ts(as.matrix(AP[Id5,])[4:LTS])), ylim=c(0,1),  col=1:5)
    axis(2, at=seq(0,1,by=0.1))
  }
  return ("..........Curves ploted............................")
}

#======================================================================================================

