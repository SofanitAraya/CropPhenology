\name{CropPhenology}
\alias{CropPhenology}

\title{
\packageTitle{CropPhenology}
}

\description{
Extract phenological metrics from time series vegetation index data
}

\details{
Introduction


Multi temporal vegetation index data can be used to get information on seasonal vegetation growth dynamics. This information indicates vegetation phenological growth stages and conditions of environmental factors influencing the vegetation growth. In cropping regions the crop growth dynamics observed from multi temporal vegetation index data has been used in applications such as crop type detection (Zhong et.al. 2011, Roerink et.al. 2011), regional crop yield estimation (Hill et.al. 2003) and many more related studies. Moreover, the long term vegetation dynamics can provide information about influential environmental factors such as soil property mapping (Araya et,al. 2016).
Plotting a time series of vegetation index values across time creates a curve that summarises the vegetation dynamics (Figure 1). Extraction of seasonal parameters is an essential step for analysing such vegetation dynamics curve. CropPhenology package has been developed to extract phenological parameters from time series vegetation index data in cropping regions.

\figure{Figure1}

Overview of data processing


CropPhenology has three functions: PhenoMetrics and MultiPointsPlot.

SinglePhenology:- provides phenological metrics for a single pixel. It allows to implement other preprocessing steps on the vegetation index curve, such as smoothing. The following example illustrate the use of Savitzky-Golay smoothing on the single pixed prior to the phenological extraction.

> install.packages(c("signal", "devtools"))
> lapply(c("signal", "devtools"), library, character = TRUE)
> # install CropPhenology
> install_github("SofanitAraya/CropPhenology")
> # NDVI test data for location F1
> MODIS_2015<-c(0.15, 0.1488, 0.152, 0.1526, 0.1699, 0.1634, 0.1647, 0.1693, 0.1899 ,0.2369 ,0.2594, 0.2274, 0.2382, 0.2168, 0.2033, 0.198, 0.183, 0.183, 0.1797, 0.1635, 0.1468, 0.1621, 0.1487)
> # Savitzky-Golay smoothing filter
> sg_MODIS2015 <- sgolayfilt(MODIS_2015)
> ts.plot(ts(MODIS_2015), ts(sg_MODIS2015), col=1:2, lwd=2)
> # Apply CropPheno to smoothed NDVI time series
> SinglePhenology(sg_MODIS2015))
> ts.plot(ts(MODIS_2015), ts(sg_MODIS2015), col=1:2, lwd=2)

\figure{Figure5}

PhenoMetrics:- takes the path for the time series vegetation index data and the vector file that defines the Area of Interest (AOI). It extracts fifteen phenological metrics (Figure 2) which represent the seasonal growth condition of the crop at each pixel for the season. The output is presented as a raster stack of phenological metrics or a table of phenological metrics for point AOI.  Table 1 summaises the defined metrics and their descriptions.

\figure{Figure2}

\figure{Figure3}

MultiPointsPlot:-  provides the user with the ability to visualise the NDVI curve by plotting the temporal sequences of NDVI values of user selected raster pixels (maximum of five). This allows the user to observe the spatial and temporal differences in relative dynamics of the vegetation index for the selected points. Figure 3 shows example for the output of MultiPointsPlot.

\figure{Figure4}

}

\references{

{Araya, S., Lyle, G., Lewis, M., and Ostendorf, B. 2016. Phenologic metrics derived from MODIS NDVI as indicators for Plant Available Water-holding Capacity.\emph{ Ecological Indicators} \bold{60:1263-1272}. http://dx.doi.org/10.1016/j.ecolind.2015.09.012}
{}
{Hill, M. J. and Donald, G. E. 2003. Estimating spatio-temporal patterns of agricultural productivity in fragmented landscapes using AVHRR NDVI time series. \emph{Remote Sensing of Environment} \bold{84:367-384.} DOI:10.1016/s0034-4257(02)00128-1 }
{}
{Roerink, G. J., Danes, M. H. G. I., Prieto, O. G., De Wit, A. J. W., and Van Vliet, A. J. H. 2011. Deriving plant phenology from remote sensing.\emph{ in 6th International Workshop on the Analysis of Multi-temporal Remote Sensing Images}, Trento, Italy, \bold{Pages 261-264}}
{}
{Zhong, L., Hawkins, T., Biging, G., Gong, P., 2011. A phenology-based approach to map crop types in the San Joaquin Valley, California.\emph{International Journal of Remote Sensing,} \bold{32, 7777-7804.}}
}

\keyword{ package }
