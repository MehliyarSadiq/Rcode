# whole set of analysis for FMOZ+CLM4.5 simulations
# source the function I wrote
source("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/code/function.R")

setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/difnc")

# analyze sign relationship between two given variable changes
# choose one pair of variables below to analyse
# var.given = c("QVEGT","RS", "negative")
# var.given = c("DV_O3","RS", "negative")
# var.given = c("ELAI","PSN", "positive")
# var.given = c("ELAI","DV_O3", "positive")
# var.given = c("O3_SRF","ISOP_SRF", "positive")
# var.given = c("O3_SRF","QVEGT", "negative")
var.given = c("O3_SRF","DV_O3", "negative")
# var.given = c("RH","QVEGT", "positive")
# var.given = c("PRECT","QVEGT", "positive")

# available names for var.given
# var.list = c("BTRAN", "ELAI", "H2OSOI", "LHFLX","PBLH","PRECT","RH","QBOT","QVEGT","DV_O3", "ISOP_SRF", "O3_SRF","PSN","RS")

# read in changes in two variables
filename.tmp = paste0(var.given[1], "_dan-ctr.nc")
file.tmp = open.ncdf(filename.tmp, write = FALSE)
var.value1 = get.var.ncdf(file.tmp, var.given[1])
lat = get.var.ncdf(file.tmp, "lat")
lon = get.var.ncdf(file.tmp, "lon")
close.ncdf(file.tmp)

filename.tmp = paste0(var.given[2], "_dan-ctr.nc")
file.tmp = open.ncdf(filename.tmp, write = FALSE)
var.value2 = get.var.ncdf(file.tmp, var.given[2])
close.ncdf(file.tmp)
# use the function
sign_harmony = two_changes_sign(var.value1, var.value2, type=var.given[3], lat, lon)




