# compare PFT-level outputs and plot the differences

library(ncdf); library(maps); library(fields)
source('~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R')
source('~/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_stat.R')

pft_plot = function(case.name="dan", var.name, zlim="fit")
{
  filepath = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/", case.name, "/pft/")
  # extract variable from experiment case
  setwd(filepath)
  filename.exp = paste0("fmoz_clm45_",var.name,"_",case.name,".nc")
  var.file.tmp = open.ncdf(filename.exp,write=FALSE)
  var.name.new = paste0(var.name,"_pft")
  var.exp = get.var.ncdf(var.file.tmp,var.name.new)
  close.ncdf(var.file.tmp)
  
  # from control case
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/ctr/pft")
  filename.ctr = paste("fmoz_clm45_",var.name,".nc", sep="")
  var.file.tmp = open.ncdf(filename.ctr,write=FALSE)
  var.ctr.new = paste0(var.name,"_pft")
  var.ctr = get.var.ncdf(var.file.tmp,var.ctr.new)
  var.long.name = att.get.ncdf(var.file.tmp, var.name,"long_name")$value
  # get unit, lan and lon
  unit = att.get.ncdf(var.file.tmp, var.ctr.new,"units")$value
  lat = get.var.ncdf(var.file.tmp,"lat")
  lon = get.var.ncdf(var.file.tmp,"lon")
  close.ncdf(var.file.tmp)
  
  dim_lon=dim.def.ncdf("lon","degree_east",lon)
  dim_lat=dim.def.ncdf("lat","degree_north",lat)
  
  # average over summertime subset of the data
  var.ctr.sum = apply(var.ctr[,,6:8,], c(1,2,4), mean, na.rm = TRUE)
  var.exp.sum = apply(var.exp[,,6:8,], c(1,2,4), mean, na.rm = TRUE)
  
  # get rid of areas with no significant vegetation, annual mean < 0.01
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/ctr")
  tmp<-open.ncdf('fmoz_clm45_ELAI_ctr.nc',write=FALSE)
  elai<-get.var.ncdf(tmp,"ELAI")
  close.ncdf(tmp)
  
  elai_ann <- apply(elai[,,], c(1,2), mean, na.rm = TRUE) # annual mean
  ind = which(elai_ann[,] < 0.01, arr.ind = T) # index used later for filtering
  
  save.path = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/plots/pft/",case.name,"/",var.name)
  setwd(save.path)
  
  #if(type == "abs")
  #{
  for(iveg in 1:16)
  {
    # calculate and plot absolute changes
    dif.var = var.exp.sum[,,iveg] - var.ctr.sum[,,iveg]
    dif.var[ind[]] = 0                   # get rid of land with no significant vegetation
    dif.var.new = dif.var[,18:92]        # get rid of antarctica to plot
    latnew = lat[18:92]
    name = paste(case.name,"_",var.name,"_pft", iveg, "_dif.jpg", sep="") # name of the plot
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
  #}
  if(var.name == "RSSUN"||var.name == "RSSHA")
  {
    setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/ctr/pft")
    filename.ctr = paste("fmoz_clm45_",var.name,".nc", sep="")
    file.tmp = open.ncdf(filename.ctr,write=FALSE)
    pftwt = get.var.ncdf(file.tmp,"pft_wtgcell")
    
    tmp = 1/var.ctr.sum
    tmp = tmp*pftwt[,,7,]
    var.ctr.com = apply(tmp[,,], c(1,2), sum, na.rm = TRUE)
    var.ctr.com = 1/var.ctr.com
    
    tmp = 1/var.exp.sum
    tmp = tmp*pftwt[,,7,]
    var.exp.com = apply(tmp[,,], c(1,2), sum, na.rm = TRUE)
    var.exp.com = 1/var.exp.com
    
    var.dif.com = var.exp.com - var.ctr.com
    var.dif.new = var.dif.com[,18:92]
    var.dif.new = trim.quant(var.dif.new, 0.95)
    
    save.path = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/plots/pft/", case.name, "/combined/")
    setwd(save.path)
    
    name = paste(case.name,"_",var.name,"_pft_com_dif.jpg", sep="") # name of the plot
    if(zlim != "fit"){
      print(name)
      jpeg(name, width=800, height=485)
      plot.field(var.dif.new,lon,latnew,type='sign', Pacific.centric = TRUE, zlim = zlim) # read in zlim
      dev.off()
    }
    else{
      plot.field(var.dif.new,lon,latnew,type='sign', Pacific.centric = TRUE)
    }
  }
}

var.names = c("ELAI", "RSSUN", "RSSHA")
zlim = matrix(c(-2,2,
                -300,300,
                -300,300), nrow = 2, ncol = length(var.names))
dimnames(zlim) = list(c("low", "high"), var.names)

for(i in 1:length(case.names))
  for (j in 1: length(var.names)) pft_plot(var.name = var.names[j], zlim = zlim[,var.names[j]])