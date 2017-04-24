# read in two maps of changes
# type = "pisitive" / "negative"
# pass lat and lon for plotting
# if the two changes have positive relationship, find the places that are positively related and output 1, vise versa

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