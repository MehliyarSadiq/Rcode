# read in two maps with changes in two variables, e.g. O3_SRF and DV_O3
# if the changes have the same sign, output 1, else -1
source("~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R")
two_changes_sign = function(var1, var2, lat, lon)
{
  # set positive changes to be 1, negative to be -1
  indx = which(var1 > 0, arr.ind = TRUE)
  var1[indx] = 1
  indx = which(var1 < 0, arr.ind = TRUE)
  var1[indx] = -1
  
  indx = which(var2 > 0, arr.ind = TRUE)
  var2[indx] = 1
  indx = which(var2 < 0, arr.ind = TRUE)
  var2[indx] = -1
  # multiply them together and plot it
  plot.field(var1*var2, lon, lat, type = "sign", zlim = c(-2,2), Pacific.centric = TRUE)
}