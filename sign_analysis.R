# following function reads in two maps (presumably changes in two variables, LAI and DryDepVel for example)
# it also needs 'lat' and 'lon' attributes of the map 
# type = "pisitive" / "negative", this specifies the theoratical relationship between these two changes, e.g. higher LAI leads to higher DV
# if the two changes have positive relationship, find the places that have the same sign and output 1, vise versa
source("~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R")

two_changes_sign = function(var1, var2, type, lat, lon)
{
  result = array(NaN, dim(var1))
  multi = var1 * var2
  if(type == "positive"){
    indx = which(multi > 0, arr.ind = TRUE)
    result[indx] = 1
  }
  if(type == "negative"){
    indx = which(multi < 0, arr.ind = TRUE)
    result[indx] = -1
  }
  plot.field(result, lon, lat, type = "sign", zlim = c(-2,2), Pacific.centric = TRUE)
}

# start the analysis
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
