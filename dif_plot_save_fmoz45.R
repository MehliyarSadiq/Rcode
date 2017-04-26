# for analysing FMOZ + CLM4.5 outputs
# Compare two cases (CTR and one of the experiment cases: pht, cond or dan(pht + cond))
# Argument: 'case.name' = "dan", "cond" or "pht"
# Plot the changes in a spefic variable (var.name, e.g. ELAI), in capital letters
# var.name = "ELAI", "GPP", "BTRAN", "H2OSOI" ...
# type = "abs" or "per", absolute or relative changes (%)
# before using the function, you need to source get_geo.R and get_stat.R functions written by Amos P.K. Tai
library(ncdf); library(maps); library(fields)
source('~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R')

dif.plot<-function(case.name, var.name, type = "abs", zlim="fit", begin_yr = 6, end_yr = 10) {
  filepath = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/", case.name)
  # read in variable from experiment case
  setwd(filepath)
  filename.exp = paste0("fmoz_clm45_",var.name,"_",case.name,".nc")
  var.file.tmp = open.ncdf(filename.exp,write=FALSE)
  var.exp = get.var.ncdf(var.file.tmp,var.name)
  close.ncdf(var.file.tmp)
  # read in from control case
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/ctr")
  filename.ctr = paste0("fmoz_clm45_",var.name,"_ctr.nc")
  var.file.tmp = open.ncdf(filename.ctr,write=FALSE)
  var.ctr = get.var.ncdf(var.file.tmp,var.name)
  var.long.name = att.get.ncdf(var.file.tmp, var.name,"long_name")$value
  # get unit, lan and lon attributes
  unit = att.get.ncdf(var.file.tmp, var.name,"units")$value
  lat = get.var.ncdf(var.file.tmp,"lat")
  lon = get.var.ncdf(var.file.tmp,"lon")
  close.ncdf(var.file.tmp)
  
  dim_lon=dim.def.ncdf("lon","degree_east",lon)
  dim_lat=dim.def.ncdf("lat","degree_north",lat)
  
  # change begin_yr and end_yr to the period you want to average over
  # begin_yr = 3 and end_yr = 5 means average over 3rd (2003) to 5th (2005) year summertime
  jja_mnths = (end_yr - begin_yr + 1) * 3  # number of summer months over the period
  year = c(begin_yr:end_yr)
  # get index of the summertime months
  mnth_index = rep(1, jja_mnths)
  for (i in year)
  {
    a = seq(6,8) + (i-1) * 12
    j = (i-begin_yr)*3
    mnth_index[c(j+1,j+2,j+3)] = a
  }
  
  # get summertime subset of the data
  var.ctr.sum = var.ctr[,,mnth_index]
  var.exp.sum = var.exp[,,mnth_index]
  
  # average over the period
  var.ctr.mean = apply(var.ctr.sum[,,1:jja_mnths], c(1,2), mean, na.rm = TRUE)
  var.exp.mean = apply(var.exp.sum[,,1:jja_mnths], c(1,2), mean, na.rm = TRUE)
  
  # get a landmask from a land output
  source('~/Dropbox/Projects/ozone_vegetation/R/plotting/code/fmoz_clm4/landmask.R')
  landmask = read_landmask()
  
  # get rid of areas with no significant vegetation, annual mean < 0.01
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/ctr")
  tmp<-open.ncdf('fmoz_clm45_ELAI_ctr.nc',write=FALSE)
  elai<-get.var.ncdf(tmp,"ELAI")
  close.ncdf(tmp)
  
  elai_ann <- apply(elai[,,], c(1,2), mean, na.rm = TRUE) # annual mean
  ind = which(elai_ann[,] < 0.01, arr.ind = T) # index used later for filtering
  # where to store the plots
  save.path = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/plots/", case.name)
  setwd(save.path)
  
  if(type == "abs")
  {
    # calculate and plot absolute changes
    dif.var = var.exp.mean - var.ctr.mean
    dif.var = dif.var * landmask         # get rid of ocean
    dif.var[ind[]] = 0                   # get rid of land with no significant vegetation, annual mean ELAI < 0.01
    dif.var.new = dif.var[,18:92]        # get rid of antarctica for plotting
    latnew = lat[18:92]
    name = paste0(case.name,"_",var.name,"_dif.jpg") # name of the plot
    # zlim could be preset and passed for inter-simulation comparisons
    if(zlim != "fit"){
      print(name)
      jpeg(name, width=800, height=485)
      plot.field(dif.var.new,lon,latnew,type='sign', Pacific.centric = TRUE, zlim = zlim) # read in zlim
      dev.off()
    }
    else{
      plot.field(dif.var.new,lon,latnew,type='sign', Pacific.centric = TRUE)
    }
  }
  
  if(type == "per")
  {
    # calculate and plot relative changes
    dif.var = var.exp.mean - var.ctr.mean
    var.ctr.mean[var.ctr.mean == 0] <- NA  # set zeros to NA before dividing
    dif.per = dif.var / var.ctr.mean * 100
    dif.per = dif.per * landmask           # get rid of ocean area
    dif.var[ind[]] = 0                     # get rid of land with no significant vegetation
    #dif.per = trim.quant(dif.per, 0.9)    # further filtering of extreme values
    dif.per.new = dif.per[,18:92]
    latnew = lat[18:92]
    name = paste0(case.name,"_",var.name,"_dif_per.jpg")
    if(zlim != "fit")
    {
      jpeg(name, width=800, height=485)
      plot.field(dif.per.new,lon,latnew,type='sign', Pacific.centric = TRUE, zlim = zlim)
      dev.off()
    }
  }
  # define attributes to store the difference data in NC files
  long.string = paste("Changes in",var.name)  # long name in NetCDF file
  #long.string.per = paste("Percentage changes in", var.name)
  var=var.def.ncdf(var.name, unit, list(dim_lon,dim_lat),-1, long.string, prec="float")
  #var.per=var.def.ncdf(var.name, "%", list(dim_lon,dim_lat),-1, long.string.per, prec="float")
  
  # save the differences in NetCDF format
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/difnc")
  file.name.tmp = paste0(var.name,"_",case.name,"-ctr.nc")
  ncnew=create.ncdf(file.name.tmp,var)
  put.var.ncdf(ncnew,var,dif.var)
  #put.var.ncdf(ncnew,var.per,dif.per)
  close.ncdf(ncnew)
}

# use the function with the following code 
# change the case.names below, or add case names after what it was
# legends are chosen to be the same across differnct sets of analysis
case.names = c("dan") # only dan (cond+pht) is available now
var.names = c("TV","TS", "BTRAN", "ELAI", "H2OSOI", "LHFLX","PBLH","PRECT","RH","QBOT","QVEGT","DV_O3", "ISOP_SRF", "O3_SRF","PSN","RS", "RSSUN", "RSSHA", "PSNSUN", "PSNSHA", "MEG_ISOP")
zlim = matrix(c(-3,3,
                -3,3,
                -0.1,0.1,
                -2,2,
                -0.3,0.3,
                -15,15,
                -100,100,
                -5e-9,5e-9,
                -10,10,
                -1e-3,1e-3,
                -6e-6,6e-6,
                -0.1,0.1,
                -2e-10,2e-10,
                -6e-9,6e-9,
                -1,1,
                -500,500,
                -500,500,
                -500,500,
                -1,1,
                -1,1,
                -1e-11,1e-11), nrow = 2, ncol = length(var.names))
dimnames(zlim) = list(c("low", "high"), var.names)

for(i in 1:length(case.names))
    for (j in 1: length(var.names)) dif.plot(case.name = case.names[i], var.name = var.names[j], type = "abs", zlim = zlim[,var.names[j]])
