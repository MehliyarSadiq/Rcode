# given a case name, plot a few variables with set limit
source('~/Dropbox/Projects/ozone_vegetation/R/data_extract/1.2.2_Aves/fmoz_clm45/code/dif_plot_save_fmoz45.R')

case.names = c("dan")
var.names = c("BTRAN", "ELAI", "H2OSOI", "LHFLX","PBLH","PRECT","RH","QBOT","QVEGT","DV_O3", "ISOP_SRF", "O3_SRF","PSN","RS")
zlim = matrix(c(-0.1,0.1,
                -1,1,
                -0.3,0.3,
                -15,15,
                -100,100,
                -5e-9,5e-9,
                -10,10,
                -1e-3,1e-3,
                -6e-6,6e-6,
                -0.2,0.2,
                -2e-10,2e-10,
                -6e-9,6e-9,
                -1,1,
                -500,500), nrow = 2, ncol = length(var.names))
dimnames(zlim) = list(c("low", "high"), var.names)

for(i in 1:length(case.names)){
  for (j in 1: length(var.names)){
    dif.plot(case.name = case.names[i], var.name = var.names[j], type = "abs", zlim = zlim[,var.names[j]])
  }
}
