# steady_check reads in a variable from a simulation, and plot time series for hand-picked spots in different regions (US, EU and China)
library(ncdf)
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
  save.path = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/plots/ts/", case.name)
  setwd(save.path)                                       # change path to save plots
  month = c(1:length(var[1,1,]))                         # x axis
  name = paste0(case.name, "_", var.name, "_TS.jpeg")    # name of the plot to be saved
  jpeg(name, width=800, height=485)                      # parameters of the figure
  par(mfrow=c(3,1))                                      # structure of panel, 3 rows 1 column
  # China, average over 9 grids containing the picked spot as the center
  mean_China = apply(var[45:47,61:63,], 3, mean, na.rm = TRUE)
  title = paste(var.name, "vs. Time")                    # title of the whole panel
  plot(month, mean_China, type = 'l', main = title)
  # plot(month, var[46,62,], type = 'l')                 # exact location picked
  # EU
  mean_Europe = apply(var[4:6,74:76,], 3, mean, na.rm = TRUE)
  plot(month, mean_Europe, type = 'l')
  # plot(month, var[5,75,], type = 'l')
  # US
  mean_US = apply(var[112:114,69:71,], 3, mean, na.rm = TRUE)
  plot(month, mean_US, type = 'l')
  # plot(month, var[113,70,], type = 'l')
  dev.off()                                              # end the plotting and saving
}

# use the function above for different cases and variables
case.names = c("ctr", "dan")
var.names = c("TV","TS", "BTRAN", "ELAI", "H2OSOI", "LHFLX","PBLH","PRECT","RH","QBOT","QVEGT","DV_O3", "ISOP_SRF", "O3_SRF","PSN","RS", "RSSUN", "RSSHA", "PSNSUN", "PSNSHA", "MEG_ISOP")
for(i in 1:length(case.names))
  for (j in 1: length(var.names)) steady_check(var.name = var.names[j], case.name = case.names[i])

