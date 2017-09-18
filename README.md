#CropPhenology
#CropPhenology
CropPhenology

CropPhenology package extracts phenological metrics from time series of seasonal vegetation index data. It has two functions: PhenoMetrics and MultiPointsPLots. PhenoMetrics create a raster stack or a table of phenological metrics for polygon or point area of interstes, respectively.

Prerequesites
packages 'raster', 'rgdal', 'maptools', 'shapefiles' and 'xlsx' are required.

Installing
The CropPhenology package can be installed from GitHub repository using 'Devtools' package. Hence, Devtools should be installed and loaded prior to the CropPhenology installation. The following code lines provide the installation steps:
install.packages ("devtools")
library(devtools)
install_github("SofanitAraya/CropPhenology")

Running the tests
Two test data are availabel with the installation. It can be run from the R program.

Built history 
This package is built using R software, as part of the PhD resaerch of the first Author, at Spatial Information Group (SIG) of The University of Adelaide.

Authors
Sofanit Araya, Bertram Ostendorf, Megan Lewis and Greg Lyle
