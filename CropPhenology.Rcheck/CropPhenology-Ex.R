pkgname <- "CropPhenology"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('CropPhenology')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("PhenoMetrics")
### * PhenoMetrics

flush(stderr()); flush(stdout())

### Name: PhenoMetrics
### Title: Phenologic metrics from time series vegetation index data
### Aliases: PhenoMetrics
### Keywords: Phenology, Time-series image, remote satellite sensing,

### ** Examples

EXAMPLE - 1

PhenoMetrics(system.file("extdata/data1", package="CropPhenology"), FALSE, 15, TRUE)

EXAMPLE - 2

PhenoMetrics(system.file("extdata/data2", package="CropPhenology"), TRUE)





### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
