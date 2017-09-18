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


CropPhenology has two functions: PhenoMetrics and MultiPointsPlots.
PhenoMetrics:- takes the path for the time series vegetation index data and the vector file that defines the Area of Interest (AOI). It extracts fifteen phenological metrics (Figure 2) which represent the seasonal growth condition of the crop at each pixel for the season. The output is presented as a raster stack of phenological metrics or a table of phenological metrics for point AOI.  Table 1 summaises the defined metrics and their descriptions.

\figure{Figure2}

\figure{Figure3}

MultiPointsPlots:-  provides the user with the ability to visualise the NDVI curve by plotting the temporal sequences of NDVI values of user selected raster pixels (maximum of five). This allows the user to observe the spatial and temporal differences in relative dynamics of the vegetation index for the selected points. Figure 3 shows example for the output of MultiPointsPlots.



  \figure{Figure4}

}

\author{
\packageAuthor{CropPhenology}

}
\references{
{Henebry, G., de Beurs, K., 2013. Remote Sensing of Land Surface Phenology: A Prospectus, in: Schwartz, M.D. (Ed.), Phenology: An Integrative Environmental Science. \emph{Springer Netherlands, pp. 385-411.}}

{Zhong, L., Hawkins, T., Biging, G., Gong, P., 2011. A phenology-based approach to map crop types in the San Joaquin Valley, California.\emph{International Journal of Remote Sensing,} \bold{32, 7777-7804.}}

{Sakamoto, T., Gitelson, A.A., Arkebauer, T.J., 2013. MODIS-based corn grain yield estimation model incorporating crop phenology information.\emph{Remote Sensing of Environment,}\bold{131, 215-231.}}

{Bolton, D.K., Friedl, M.A., 2013. Forecasting crop yield using remotely sensed vegetation indices and crop phenology metrics. \emph{Agricultural and Forest Meteorology,}\bold{173, 74-84.}}
}
\keyword{ package }
