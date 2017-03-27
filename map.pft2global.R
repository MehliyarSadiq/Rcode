# this function takes in case name and variable name
# extract this variable from the case file (pre-processed: summertime average (not done))
# extract from 1-d (pft-level) output and map it to 2-d (global map) and plot it

library(ncdf)
library(maps)
library(fields)
source('/Users/mehliyarsadiq/Documents/R/functions_Amos/get_geo.R')

map.pft2global<-function(case.name, var.name)
{  
  # get 2d lat and lon from a 2d output file, random
  setwd("/Users/mehliyarsadiq/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm4/Output")
  tmp = open.ncdf('fmoz_clm4cam4.clm2.h0.2020-08.nc',write=FALSE)
  lat = get.var.ncdf(tmp,"lat")
  lon = get.var.ncdf(tmp,"lon")
  close.ncdf(tmp)
  
  # case.name = 'ctr' or 'dan'
  filepath = paste("~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/", case.name, sep="")
  setwd(filepath)
  filename = paste("fmoz_clm45_pft_", var.name, "_", case.name, ".nc", sep="")
  var.file.tmp = open.ncdf(filename,write=FALSE)
  var.exp = get.var.ncdf(var.file.tmp,var.name)
  pfttype = get.var.ncdf(var.file.tmp,"pfts1d_itype_veg")
  pft_elai = get.var.ncdf(var.file.tmp,"ELAI")
  pft_wtgc = get.var.ncdf(var.file.tmp,"pfts1d_wtgcell")
  pft_lat = get.var.ncdf(var.file.tmp,"pfts1d_lat")
  pft_lon = get.var.ncdf(var.file.tmp,"pfts1d_lon")
  close.ncdf(var.file.tmp)
  
  # put them in a data frame
  wtlai_ctr = pft_wtgc * pft_elai
  d = data.frame(pfttype, pft_lat, pft_lon, wtlai_ctr)
  rws = !d$pfttype == 0
  dn = d[rws,]
  rws = !d$pfttype == 16
  dn = dn[rws,]
  rs = !is.na(dn$wtlai)
  dnn = dn[rs,]
  
  elai_2d = array(0, dim = c(144,96,15))
  for (vt in 1:15)
  {
    vtdata = dnn[which(dnn$pfttype == vt),]
    lg = length(vtdata[,1])
    for (i in 1:lg)
    {
      ind = find.lon.lat(vtdata$pft_lon[i],vtdata$pft_lat[i], lon, lat)
      elai_2d[ind[1],ind[2],vt] = vtdata$wtlai[i]
    }
  }
  
  # different pft groups, not necessary for now
#   elai_needleleaf_ctr = apply(elai_2d[,,1:3], c(1,2), sum)
#   plot.field(elai_needleleaf_ctr,lon,lat,type='sign', Pacific.centric = TRUE, zlim=c(-2,2))
#   elai_broadleaf_ctr = apply(elai_2d[,,4:11], c(1,2), sum)
#   plot.field(elai_broadleaf_ctr,lon,lat,type='sign', Pacific.centric = TRUE, zlim=c(-6,6))
#   elai_grasscrop_ctr = apply(elai_2d[,,12:15], c(1,2), sum)
#   plot.field(elai_grasscrop_ctr,lon,lat,type='sign', Pacific.centric = TRUE, zlim=c(-5,5))
#  
  for(i in 1:15) plot.field(elai_2d[,,i],lon,lat,type='sign', Pacific.centric = TRUE, zlim=c(-5,5))
  
  tt_elai = elai_needleleaf_ctr + elai_broadleaf_ctr + elai_grasscrop_ctr
  plot.field(tt_elai,lon,lat,type='sign', Pacific.centric = TRUE, zlim=c(-10,10))
}

map.pft2global("ctr", "ELAI")