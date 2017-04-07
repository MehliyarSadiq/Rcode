# combine sunlit and shaded variables together
# from default 2-d output
library(ncdf); library(maps); library(fields)
source('/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/functions_Amos/get_geo.R')

combine<-function(case.name) {
  filepath = paste0("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/", case.name)
  # getting variable from experiment case
  setwd(filepath)
  
  # combine psn
  name.tmp = paste0("fmoz_clm45_FSUN_",case.name,".nc")
  file.tmp = open.ncdf(name.tmp,write=FALSE)
  fsun = get.var.ncdf(file.tmp,"FSUN")
  lat = get.var.ncdf(file.tmp,"lat")
  lon = get.var.ncdf(file.tmp,"lon")
  month =get.var.ncdf(file.tmp,"month")
  close.ncdf(file.tmp)
  
  name.tmp = paste0("fmoz_clm45_PSNSUN_",case.name,".nc")
  file.tmp = open.ncdf(name.tmp,write=FALSE)
  psnsun = get.var.ncdf(file.tmp,"PSNSUN")
  unit = att.get.ncdf(file.tmp,"PSNSUN","units")$value
  close.ncdf(file.tmp)
  
  name.tmp = paste0("fmoz_clm45_PSNSHA_",case.name,".nc")
  file.tmp = open.ncdf(name.tmp,write=FALSE)
  psnsha = get.var.ncdf(file.tmp,"PSNSHA")
  close.ncdf(file.tmp)
  
  psn = psnsun * fsun + psnsha * (1-fsun)
  
  dim_lon=dim.def.ncdf("lon","degree_east",lon)
  dim_lat=dim.def.ncdf("lat","degree_north",lat)
  dim_mnth=dim.def.ncdf("month","month",month)
  
  var=var.def.ncdf("PSN", unit, list(dim_lon,dim_lat,dim_mnth),-1, "photosynthesis rate", prec="float")
  
  file.name.tmp = paste0("fmoz_clm45_PSN_",case.name,".nc")
  ncnew=create.ncdf(file.name.tmp,var)
  put.var.ncdf(ncnew,var,psn)
  close.ncdf(ncnew)
  
  # combine rs
  name.tmp = paste0("fmoz_clm45_RSSUN_",case.name,".nc")
  file.tmp = open.ncdf(name.tmp,write=FALSE)
  rssun = get.var.ncdf(file.tmp,"RSSUN")
  unit = att.get.ncdf(file.tmp,"RSSUN","units")$value
  close.ncdf(file.tmp)
  
  name.tmp = paste0("fmoz_clm45_RSSHA_",case.name,".nc")
  file.tmp = open.ncdf(name.tmp,write=FALSE)
  rssha = get.var.ncdf(file.tmp,"RSSHA")
  close.ncdf(file.tmp)
  
  rs = 1/(fsun/rssun + (1-fsun)/rssha)
  
  var=var.def.ncdf("RS", unit, list(dim_lon,dim_lat,dim_mnth),-1, "stomatal resistance", prec="float")
  
  file.name.tmp = paste0("fmoz_clm45_RS_",case.name,".nc")
  ncnew=create.ncdf(file.name.tmp,var)
  put.var.ncdf(ncnew,var,rs)
  close.ncdf(ncnew)
}
for(case in c("ctr", "dan")) combine(case)