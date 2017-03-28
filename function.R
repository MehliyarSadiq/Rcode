# read in two maps with changes in two variables, e.g. O3_SRF and DV_O3
# if the changes have the same sign, output 1, else -1
source("~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R")
two_changes_sign = function(var1, var2, type, lat, lon)
{
  result = array(NaN, dim(var1))
  
  multi = var1 * var2
  
  if(type == "positive") 
  {
    indx = which(multi > 0, arr.ind = TRUE)
    result[indx] = 1
  }
  
  if(type == "negative")
  {
    indx = which(multi < 0, arr.ind = TRUE)
    result[indx] = -1
  }
  # multiply them together and plot it
  plot.field(result, lon, lat, type = "sign", zlim = c(-2,2), Pacific.centric = TRUE)
}