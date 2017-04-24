# following function reads in a variable from a simulation, and plot a time series fluctuation in different regions (US, EU and China)
steady_check = function(var.name, case.name)
{
  filepath = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/", case.name)
  # read in variable from the case
  setwd(filepath)
  filename = paste0("fmoz_clm45_", var.name, "_", case.name, ".nc")
  var.file.tmp = open.ncdf(filename, write=FALSE)
  var = get.var.ncdf(var.file.tmp, var.name)
  close.ncdf(var.file.tmp)
  # pick a data point (grid) for each region, US, EU and China
  # for China, I picked HengYang, lat = 26, lon = 112, indices are [46,62], lon first
  # for EU, I picked Frankfurt, lat = 50, lon = 8.7, indices are [5,75]
  # for US, I picked middle of West Virginia, lat = 38, lon = -80, indices are [113,70]
  # do the time series plots for all of them
  xmonth = c(1:length(var[1,1,])) # x axis
  # China
  mean_cn = apply(var[45:47,61:63,], 3, mean, na.rm = TRUE) # average over 9 grids
  plot(xmonth, mean_cn, type = 'l')
  #plot(xmonth, var[46,62,], type = 'l') # exact spot
  # EU
  mean_eu = apply(var[4:6,74:76,], 3, mean, na.rm = TRUE)
  plot(xmonth, mean_eu, type = 'l')
  #plot(xmonth, var[5,75,], type = 'l')
  # US
  mean_us = apply(var[112:114,69:71,], 3, mean, na.rm = TRUE)
  plot(xmonth, mean_us, type = 'l')
  #plot(xmonth, var[113,70,], type = 'l')
}
steady_check(var.name = "ELAI", case.name = "dan")