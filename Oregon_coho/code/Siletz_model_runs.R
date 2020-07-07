rm(list=ls())
#devtools::install_local( "C:/Users/James.Thorson/Desktop/Git/FishStatsUtils", force=TRUE, dep=FALSE )


# library(VAST)
# devtools::load_all("C:\\merrill\\FishStatsUtils")
devtools::load_all("C:\\merrill\\DHARMa\\DHARMa")
devtools::load_all("C://merrill/TMB_contrib_R/TMBhelper")
library(VAST)
devtools::load_all("C:\\merrill\\FishStatsUtils")
devtools::load_all("C:\\merrill\\VASTPlotUtils")


# sil_dir <- "~/Projects/Spatiotemporal/VAST_SN/Oregon_coho/Siletz"
sil_dir <- "C:/merrill/VAST_SN/Oregon_coho/Siletz"
# jim_dir <- "C:/Users/James.Thorson/Desktop/Work files/Collaborations/2018 -- Rudd stream network/2020-06-01"
# load(file.path(jim_dir, "general_inputs.Rdata"))
load(file.path(sil_dir, "general_inputs.Rdata"))
# path <- file.path(jim_dir, "V2")

#############
## IID
#############
path <- file.path(sil_dir, "multivar_landcover_IID_dist5")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



# ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
# ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
# ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"="IID")

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

Par <- fit1$ParHat
Map <- fit1$tmb_list$Map
  Map$logSigmaM = factor( c(1,NA,2,3,NA,NA) )

# Reduced model run
fit1 = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 


# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=0,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

plotQQunif( Plots$dharmaRes  )


#############
## IID
#############
path <- file.path(sil_dir, "multivar_landcover_IID_dist5_RWEps")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"="IID")

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
Map$logSigmaM <- factor(c(1,2,3,NA,NA,NA))
  Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = NA
  Map$gamma1_ctp <- factor(Map$gamma1_ctp)

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 


# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE,
                  getJointPrescision = TRUE) #,
                  # CompileDir = jim_dir)

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
Zlim = c(min(dens),max(dens))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

plotQQunif( Plots$dharmaRes  )

#################
## Factor
##################
path <- file.path(sil_dir, "multivar_landcover_dist5")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, Variance parameters for juvenile positive catch rates, habitat covariate effects on zero-inflated probability (fixed to zero)
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=0,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

png(file.path(fig, "Diagnostic_figure.png"), height = 600, width = 1000)
par(mfrow = c(1,2))
plotQQunif(dharmaRes)
testDispersion(dharmaRes)
dev.off()



###########
path <- file.path(sil_dir, "multivar_landcover_dist11")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  # Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=0,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

##################
path <- file.path(sil_dir, "multivar_landcover_dist5_RW")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=2, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

##################
path <- file.path(sil_dir, "multivar_landcover_dist5_RWEps")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

Report <- fit$Report
Sdreport <- fit$parameter_estimates$SD
TmbData <- fit$data_list
Data <- fit$data_list
ParHat <- fit$ParHat

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(14), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))


## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")

Res <- Data %>% mutate(Residuals = dharmaRes$scaledResiduals)
p <- ggplot(Res) +
geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", alpha = 0.5, cex = 0.5) +
geom_point(aes(x = Lon, y = Lat, fill = Residuals, shape = Category), cex = 3, alpha = 0.8) +
scale_fill_distiller(palette = "Spectral") +
scale_shape_manual(values = c(24,21)) +
xlab("Longitude") + ylab("Latitude") +
facet_wrap(~Year) +
theme_bw(base_size = 14)
ggsave(file.path(fig, "Scaled_residuals_on_map.png"), p, height = 10, width = 12)

png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

hist(dharmaRes)

png(file.path(fig, "Hist.png"), height = 600, width = 600)
testDispersion(dharmaRes)
dev.off()

png(file.path(fig, "Diagnostic_figure.png"), height = 600, width = 1000)
par(mfrow = c(1,2))
plotQQunif(dharmaRes)
testDispersion(dharmaRes)
dev.off()


Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

##################
path <- file.path(sil_dir, "multivar_dist5_RWEps")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  # X_gtp = X_gtp_all,
                  # X_itp = X_itp_all,
                  # Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor(rep(NA,length(Map$logSigmaM)) #factor( c(1,NA,2,NA,NA,NA) )
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  # X_gtp = X_gtp_all,
                  # X_itp = X_itp_all,
                  # Xconfig_zcp = Xconfig_all2, 
                  # # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  # X_gtp = X_gtp_all,
                  # X_itp = X_itp_all,
                  # Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

Report <- fit$Report
Sdreport <- fit$parameter_estimates$SD
TmbData <- fit$data_list
Data <- fit$data_list
ParHat <- fit$ParHat

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(14), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))


## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

hist(dharmaRes)

png(file.path(fig, "Hist.png"), height = 600, width = 600)
testDispersion(dharmaRes)
dev.off()

png(file.path(fig, "Diagnostic_figure.png"), height = 600, width = 1000)
par(mfrow = c(1,2))
plotQQunif(dharmaRes)
testDispersion(dharmaRes)
dev.off()


Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

##################
path <- file.path(sil_dir, "juveniles_landcover_dist5_RWEps")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count_juv

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=1)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_juv,
                  X_itp = X_itp_juv,
                  Xconfig_zcp = Xconfig_juv2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  # Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
   Map$logSigmaM <- factor(c(1,NA,NA))
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_juv,
                  X_itp = X_itp_juv,
                  Xconfig_zcp = Xconfig_juv2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=rep(0,nrow(Data)),
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_juv,
                  X_itp = X_itp_juv,
                  Xconfig_zcp = Xconfig_juv2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")

Res <- Data %>% mutate(Residuals = dharmaRes$scaledResiduals)
p <- ggplot(Res) +
geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", alpha = 0.5, cex = 0.5) +
geom_point(aes(x = Lon, y = Lat, fill = Residuals), cex = 3, pch = 24, alpha = 0.8) +
scale_fill_distiller(palette = "Spectral") +
# scale_shape_manual(values = c(24,21)) +
xlab("Longitude") + ylab("Latitude") +
facet_wrap(~Year) +
theme_bw(base_size = 14)
ggsave(file.path(fig, "Scaled_residuals_on_map.png"), p, height = 10, width = 12)


png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

hist(dharmaRes)

png(file.path(fig, "Hist.png"), height = 600, width = 600)
testDispersion(dharmaRes)
dev.off()

png(file.path(fig, "Diagnostic_figure.png"), height = 600, width = 1000)
par(mfrow = c(1,2))
plotQQunif(dharmaRes)
testDispersion(dharmaRes)
dev.off()


Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )

##################
path <- file.path(sil_dir, "multivar_landcover_dist5_v2")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  Map$gamma1_ctp[which(Map$gamma1_ctp == 1)[1:10]] = NA
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par)) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )


##################
path <- file.path(sil_dir, "multivar_landcover_dist7")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=7, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  # Map$logSigmaM = factor( c(1,NA,2,NA,NA,NA) )
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)[1]] = NA
  # # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par)) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )


##################
path <- file.path(sil_dir, "multivar_landcover_dist2")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_dens

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=2, "Epsilon1"=2, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=2, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(1,NA,NA,NA,NA,NA) )
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)[1]] = NA
  # # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par)) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )


### habsurvey
path <- file.path(sil_dir, "multivar_habsurvey_dist5")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(NA,NA,NA,NA,NA,NA) )
  Map$gamma1_ctp <- factor(rep(NA,length(Map$gamma1_ctp)))


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=0,
                  test_fit = FALSE) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )


df <- data.frame("Model" = c("multivar_landcover_dist11",
                              "multivar_landcover_dist5",
                              "multivar_landcover_IID_dist5",
                              "multivar_landcover_dist7",
                              "multivar_landcover_dist5_RW",
                              "multivar_landcover_dist5_RWEps",
                              "multivar_landcover_dist5_v2",
                              "multivar_landcover_dist2",
                              "multivar_habsurvey_dist5"))
df$AIC <- NULL
for(i in 1:nrow(df)){
   res <- readRDS(file.path(sil_dir, df[i,"Model"], "Fit.rds"))
   aic <- as.numeric(res$parameter_estimates$AIC)
   df[i,"AIC"] <- aic
}
df$dAIC <- sapply(1:nrow(df), function(x) df[x,"AIC"] - min(df[,"AIC"]))
df[order(df$dAIC),]


## remove last year of juveniles
#################
## Factor
##################
path <- file.path(sil_dir, "multivar_landcover_dist5_RWEps_rm")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)



#
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.dll"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

index <- which(Data_count$Category == "Juveniles" & Data_count$Year == 2017)
Data <- Data_count[-index,]
X_itp_inp <- X_itp_all[-index,,]

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=5, "Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
             "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings( Version = "VAST_v8_2_0",
                           n_x = nrow(Network_sz),
                           Region = "Stream_network", 
                           FieldConfig=FieldConfig, 
                           RhoConfig=RhoConfig, 
                           OverdispersionConfig=OverdispersionConfig, 
                           Options=Options, 
                           ObsModel=ObsModel, 
                           purpose = "index2", 
                           fine_scale=FALSE, 
                           bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_inp,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
  # Map$beta1_ft <- factor(rep(NA, length(Map$beta1_ft)))
   # Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  Map$logSigmaM = factor( c(NA,NA,NA,NA,NA,NA) )
  Map$gamma1_ctp <- factor(rep(NA, length(Map$gamma1_ctp)))
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 1)] = NA
  # Map$gamma1_ctp[which(Map$gamma1_ctp == 2)] = 1
  # Map$gamma1_ctp <- factor(Map$gamma1_ctp)


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_inp,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  getsd=FALSE,
                  newtonsteps=0,
                  test_fit = FALSE)
                  # CompileDir = jim_dir)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

# Reduced model run
fit = fit_model( "settings"=settings,
                  "Lat_i"=Data[,"Lat"],
                  "Lon_i"=Data[,"Lon"],
                  "t_i"=Data[,'Year'],
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1,
                  "b_i"=Data[,'Catch_KG'],
                  "a_i"=Data[,'AreaSwept_km2'],
                  "v_i"=Data[,'Vessel'],
                  working_dir = path,
                  input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"]),
                  Network_sz_LL=Network_sz_LL,
                  Network_sz = Network_sz,
                  Map = Map,
                  Parameters = Par,
                  X_gtp = X_gtp_all,
                  X_itp = X_itp_inp,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  getsd=TRUE,
                  newtonsteps=3,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par)) #,
                  # CompileDir = jim_dir)
## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


dharmaRes = summary( fit, what="residuals")
png(file.path(fig, "DHARMa_res.png"), height = 600, width = 900)
plot(dharmaRes, quantreg = TRUE)
dev.off()
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 600)
plotQQunif(dharmaRes)
dev.off()

Plots = plot(fit,
  working_dir=paste0(path,"/figures/"),
  land_color=rgb(0,0,0,0),
  quantreg=TRUE )




######################################
### Manuscript figures
######################################

## Network
net <- ggplot(Network_sz_LL_info, aes(x = Lon, y = Lat)) +
   geom_point(aes(fill = Network, cex = Network), color = gray(0.9), pch = 21, alpha = 0.6) +
   xlab("Longitude") + ylab("Latitude") +
   scale_fill_manual(values = c("gray", "goldenrod")) +
   # guides(fill = guide_legend(title = "")) +
   theme_bw(base_size = 14)

## locations for arrows
l2 <- lapply(1:nrow(Network_sz_LL), function(x){
  parent <- Network_sz_LL$parent_s[x]
  find <- Network_sz_LL %>% filter(child_s == parent)
  if(nrow(find)>0) out <- cbind.data.frame(Network_sz_LL[x,], 'Lon2'=find$Lon, 'Lat2'=find$Lat)
  if(nrow(find)==0) out <- cbind.data.frame(Network_sz_LL[x,], 'Lon2'=NA, 'Lat2'=NA)
  return(out)
})
l2 <- do.call(rbind, l2)
net <- net + geom_segment(data=l2, aes(x = Lon,y = Lat, xend = Lon2, yend = Lat2), arrow=arrow(length=unit(0.2,"cm")), col="gray")
ggsave(file.path(fig_dir, "Network.png"), net, width = 8, height = 6)


## network with observations
net_wObs <- net + 
   geom_point(data = Data_dens, aes(color = Category), cex = 2) + 
   scale_color_brewer(palette = "Set1") +
   guides(color = guide_legend(title = "Survey location"))
ggsave(file.path(fig_dir, "Survey_locs.png"), net_wObs, height = 6, width = 8)

colors <- RColorBrewer::brewer.pal(3, "Set1")
pspawn <- ggplot() +
   geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", cex = 1, alpha = 0.5) +
   geom_point(data = Data_dens %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, size = Catch_KG, color = Category), alpha = 0.6) +
   facet_wrap(.~Year) +
   scale_color_manual(values = colors[2]) +
   theme_bw(base_size = 14) +
   ggtitle("Observed spawner density") +
   guides(size = guide_legend(title = "Coho per km"), color = FALSE)
ggsave(file.path(fig_dir, "Spawner_Density_byYear.png"), pspawn, height = 12, width = 14)
pjuv <- ggplot() +
   geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", cex = 1, alpha = 0.5) +
   geom_point(data = Data_dens %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, size = Catch_KG, color = Category), alpha = 0.6) +
   facet_wrap(.~Year) +
   scale_color_manual(values = colors[1]) +
   theme_bw(base_size = 14) +
   ggtitle("Observed juvenile density") +
   guides(size = guide_legend(title = "Coho per km"), color = FALSE)
ggsave(file.path(fig_dir, "Juvenile_Density_byYear.png"), pjuv, height = 12, width = 14)

pobs <- ggplot() +
   geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", cex = 1, alpha = 0.5) +
   geom_point(data = Data_dens, aes(x = Lon, y = Lat, size = Catch_KG, color = Category), alpha = 0.6) +
   facet_wrap(.~Year) +
   scale_color_brewer(palette = "Set1") +
   theme_bw(base_size = 14) +
   ggtitle("Observed density") +
   guides(size = guide_legend(title = "Coho per km")) +
   scale_x_continuous(breaks = as.numeric(quantile(round(Data_dens$Lon,1),prob=c(0.05,0.5,0.99)))) +
   xlab("Longitude") + ylab("Latitude")
ggsave(file.path(fig_dir, "Observed_density_byYear.png"), pobs, height = 9, width = 10)

### Results
library(tidyverse)
base <- readRDS(file.path(sil_dir, "multivar_landcover_dist5_RWEps", "Fit.rds"))
iid <- readRDS(file.path(sil_dir, "multivar_landcover_IID_dist5", "Fit.rds"))
juv <- readRDS(file.path(sil_dir, "juveniles_landcover_dist5_RWEps", "Fit.rds"))

## compare maps
dens_byModel <- lapply(1:3, function(x){
  if(x == 1){
    Report <- base$Report
    year_labels = base$year_labels
    years_to_plot = base$years_to_plot
    spatial_list <- base$spatial_list
    name <- "Multivariate factor analysis"
  }
  if(x == 2){
    Report <- iid$Report
    year_labels = iid$year_labels
    years_to_plot = iid$years_to_plot
    spatial_list <- iid$spatial_list
    name <- "Independent"
  }
  if(x == 3){
    Report <- juv$Report
    year_labels = juv$year_labels
    years_to_plot = juv$years_to_plot
    spatial_list <- juv$spatial_list
    name <- "Juvenile survey only"
  }
  Array_xct = log(Report$D_gcy)
  if(x %in% c(1:2)) dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Spawners","Juveniles"), Year = year_labels)
  if(x == 3) dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Juveniles"), Year = year_labels)
  xct <- reshape2::melt(Array_xct) %>% mutate(Model = name)
  xctll <- full_join(xct, cbind.data.frame("Node" = 1:spatial_list$n_g,spatial_list$latlon_g))
  return(xctll)
})
dens <- do.call(rbind, dens_byModel)

plot_dens <- dens #%>% filter(Year %in% c(1997,2007,2017))

plot_dens$value <- as.numeric(plot_dens$value)
p <- ggplot(plot_dens %>% filter(Model == "Multivariate factor analysis") %>% filter(Category == "Spawners")) +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 1.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  # scale_color_viridis_c() + 
  facet_wrap(Year ~ .) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="log(Coho per km)")) +
  ggtitle("Spawner log-density") + 
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Spawner_density_base.png"), p, height = 12, width = 15)

p <- ggplot(plot_dens %>% filter(Model == "Multivariate factor analysis") %>% filter(Category == "Juveniles")) +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 1.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  facet_wrap(Year ~ .) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="log(Coho per km)")) +
  ggtitle("Juvenile log-density") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Juvenile_density_base.png"), p, height = 12, width = 15)

plot_both <- plot_dens %>% filter(Model == "Multivariate factor analysis") %>% filter(Year %in% seq(1997,2017,by=5))
p <- ggplot(plot_both) +
  geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 2.5, alpha = 0.75) +
  geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 3, alpha = 0.75, pch = 21, color = "white") +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(Year ~ Category) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Juveniles"), fill=guide_colourbar(title="Spawners")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Density_sub_base.png"), p, height = 15, width = 10)

plot_both <- plot_dens %>% filter(Model == "Multivariate factor analysis")
p <- ggplot(plot_both) +
   geom_point(aes(x = Lon, y = Lat, color = value), alpha = 0.75) +
   # geom_point(data = hab_df %>% filter(variable == "land_cover") %>% filter(grepl("Developed", value)), aes(x = Lon, y = Lat), pch = 1, stroke = 1.2) +
   # geom_point(data = Data_dens, aes(x = Lon, y = Lat), alpha = 0.75, pch = 1, stroke = 1.2) +
  # geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 1, alpha = 0.75) +
  # geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 1.5, alpha = 0.75, pch = 21) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(Year ~ Category, ncol = 6) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="log(Coho per km)")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Density_compare_base.png"), p,  height = 18, width = 15)


plot_both <- plot_dens %>% filter(Model == "Independent")
p <- ggplot(plot_both) +
   geom_point(aes(x = Lon, y = Lat, color = value), alpha = 0.75) +
   # geom_point(data = Data_dens, aes(x = Lon, y = Lat), alpha = 0.75, pch = 1, stroke = 1.2) +
  # geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 1, alpha = 0.75) +
  # geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 1.5, alpha = 0.75, pch = 21) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(Year ~ Category, ncol = 6) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="log(Coho per km)")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Density_compare_IID.png"), p,  height = 18, width = 15)

plot_both <- plot_dens %>% filter(Model != "Independent") %>% filter(Year %in% c(1997,2005,2017))
p <- ggplot(plot_both) +
   geom_point(aes(x = Lon, y = Lat, color = value), cex = 3, alpha = 0.75) +
   # geom_point(data = Data_dens, aes(x = Lon, y = Lat), alpha = 0.75, pch = 1, stroke = 1.2) +
  # geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 1, alpha = 0.75) +
  # geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 1.5, alpha = 0.75, pch = 21) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(Year ~ Model) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="log(Coho per km)")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Density_compare_Juv.png"), p,  height = 18, width = 15)

## covariate impact
covar <- lapply(1, function(x){
   if(x == 1){
    Report <- base$Report
    year_labels = base$year_labels
    years_to_plot = base$years_to_plot
    spatial_list <- base$spatial_list
    name <- "Multivariate factor analysis"
   }
 Array_xct = Report$eta2_gct
  dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Spawners","Juveniles"), Year = year_labels)
  xct <- reshape2::melt(Array_xct) %>% mutate(Model = name)
  xctll <- full_join(xct, cbind.data.frame("Node" = 1:spatial_list$n_g,spatial_list$latlon_g))
  return(xctll)
})
covar <- do.call(rbind, covar)

hab_sub <- hab_df %>% filter(variable == 'land_cover')
covar_sub <-  covar %>% 
      filter(Model == "Multivariate factor analysis") %>% 
      filter(Category == "Juveniles") %>% filter(Year == max(Year)) %>%
      rename(child_s = Node) %>%
      select(-c(Category, Year, Lat, Lon)) %>%
      rename(Impact = value)
hab_plot <- full_join(hab_sub, covar_sub)

library(ggthemes)
p <- ggplot(hab_plot) +
   geom_point(aes(x = Lon, y = Lat, fill = value, size = -Impact), pch = 21, alpha = 0.75) +
   scale_fill_brewer(palette = "Set1") +
   xlab("Longitude") + ylab("Latitude") +
   guides(fill=guide_legend(title="Land cover"), size=guide_legend(title = "Juvenile covariate impact")) +
   theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Juv_covar_base.png"), p,  height = 5, width = 8)


## epsilon
eps_byModel <- lapply(1:3, function(x){
  if(x == 1){
    Report <- base$Report
    year_labels = base$year_labels
    years_to_plot = base$years_to_plot
    spatial_list <- base$spatial_list
    name <- "Multivariate factor analysis"
  }
  if(x == 2){
    Report <- iid$Report
    year_labels = iid$year_labels
    years_to_plot = iid$years_to_plot
    spatial_list <- iid$spatial_list
    name <- "Independent"
  }
  if(x == 3){
    Report <- iid$Report
    year_labels = iid$year_labels
    years_to_plot = iid$years_to_plot
    spatial_list <- iid$spatial_list
    name <- "Juvenile survey only"
  }
  Array_xct = Report$Epsilon2_gct
  dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Spawners","Juveniles"), Year = year_labels)
  xct <- reshape2::melt(Array_xct) %>% mutate(Model = name)
  xctll <- full_join(xct, cbind.data.frame("Node" = 1:spatial_list$n_g,spatial_list$latlon_g))
  return(xctll)
})
eps <- do.call(rbind, eps_byModel)

plot_eps <- eps #%>% filter(Year %in% c(1997,2007,2017))

plot_eps$value <- as.numeric(plot_eps$value)
p <- ggplot(plot_eps %>% filter(Model == "Multivariate factor analysis") %>% filter(Category == "Spawners")) +
  geom_point(aes(x = Lon, y = Lat, color = abs(value)), cex = 1.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  # scale_color_viridis_c() + 
  facet_wrap(Year ~ .) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Variation")) +
  ggtitle("Spawner spatiotemporal variation in abundance-density") + 
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Spawner_epsilon_base.png"), p, height = 12, width = 15)

p <- ggplot(plot_eps %>% filter(Model == "Multivariate factor analysis") %>% filter(Category == "Juveniles")) +
  geom_point(aes(x = Lon, y = Lat, color = abs(value)), cex = 1.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  facet_wrap(Year ~ .) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Variation")) +
  ggtitle("Juvenile spatiotemporal variation in abundance-density") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Juvenile_epsilon_base.png"), p, height = 12, width = 15)

plot_both <- plot_eps %>% filter(Model == "Multivariate factor analysis") %>% filter(Year %in% seq(1997,2017,by=5))
p <- ggplot(plot_both) +
  geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = abs(value)), cex = 2.5, alpha = 0.75) +
  geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = abs(value)), cex = 3, alpha = 0.75, pch = 21, color = "white") +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(Year ~ Category) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Juveniles"), fill=guide_colourbar(title="Spawners")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Epsilon_sub_base.png"), p, height = 15, width = 10)

plot_both <- plot_eps %>% filter(Model == "Multivariate factor analysis")
p <- ggplot(plot_both) +
   geom_point(aes(x = Lon, y = Lat, color = abs(value)), alpha = 0.75) +
   geom_point(data = Data_dens, aes(x = Lon, y = Lat), alpha = 0.75, pch = 1, stroke = 1.2) +
  # geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 1, alpha = 0.75) +
  # geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 1.5, alpha = 0.75, pch = 21) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(Year ~ Category, ncol = 6) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Variation")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Epsilon_compare_base.png"), p,  height = 18, width = 15)

plot_both <- plot_eps %>% filter(Model != "Independent") %>% filter(Year %in% c(1997,2005,2017))
p <- ggplot(plot_both) +
   geom_point(aes(x = Lon, y = Lat, color = abs(value)), cex = 3, alpha = 0.75) +
   # geom_point(data = Data_dens, aes(x = Lon, y = Lat), alpha = 0.75, pch = 1, stroke = 1.2) +
  # geom_point(data= plot_both %>% filter(Category == "Juveniles"), aes(x = Lon, y = Lat, color = value), cex = 1, alpha = 0.75) +
  # geom_point(data = plot_both %>% filter(Category == "Spawners"), aes(x = Lon, y = Lat, fill = value), cex = 1.5, alpha = 0.75, pch = 21) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(Year ~ Model) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Variation")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Epsilon_compare_Juv.png"), p,  height = 18, width = 15)

## effective area occupied
eao_byModel <- lapply(1:3, function(x){
  if(x == 1){
    SD <- TMB::summary.sdreport(base$parameter_estimates$SD)
    TmbData <- base$data_list
    year_labels = base$year_labels
    years_to_plot = base$years_to_plot
    spatial_list <- base$spatial_list
    name <- "Multivariate factor analysis"
  }
  if(x == 2){
    SD <- TMB::summary.sdreport(iid$parameter_estimates$SD)
    TmbData <- iid$data_list
    year_labels = iid$year_labels
    years_to_plot = iid$years_to_plot
    spatial_list <- iid$spatial_list
    name <- "Independent"
  }
  if(x == 3){
    SD <- TMB::summary.sdreport(juv$parameter_estimates$SD)
    TmbData <- juv$data_list
    year_labels = juv$year_labels
    years_to_plot = juv$years_to_plot
    spatial_list <- juv$spatial_list
    name <- "Juvenile survey only"
  }
  
  EffectiveName = "effective_area_cyl"
  SD_effective_area_ctl = SD_log_effective_area_ctl = array( NA, dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
  use_biascorr = TRUE
    # Extract estimates
    SD_effective_area_ctl = SD_log_effective_area_ctl = array( NA, dim=c(unlist(TmbData[c('n_c','n_t','n_l')]),2), dimnames=list(NULL,NULL,NULL,c('Estimate','Std. Error')) )
    # Effective area
    if( use_biascorr==TRUE && "unbiased"%in%names(SD) ){
      SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Est. (bias.correct)','Std. Error')]
    }
    if( !any(is.na(SD_effective_area_ctl)) ){
      message("Using bias-corrected estimates for effective area occupied (natural scale)...")
    }else{
      message("Not using bias-corrected estimates for effective area occupied (natural scale)...")
      SD_effective_area_ctl[] = SD[which(rownames(SD)==EffectiveName),c('Estimate','Std. Error')]
    }
    # Log-Effective area
    if( use_biascorr==TRUE && "unbiased"%in%names(SD) ){
      SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Est. (bias.correct)','Std. Error')]
    }
    if( !any(is.na(SD_log_effective_area_ctl)) ){
      message("Using bias-corrected estimates for effective area occupied (log scale)...")
    }else{
      message("Not using bias-corrected estimates for effective area occupied (log scale)...")
      SD_log_effective_area_ctl[] = SD[which(rownames(SD)==paste0("log_",EffectiveName)),c('Estimate','Std. Error')]
    }

  Index_ctl=array(SD_log_effective_area_ctl[,,,'Estimate'],dim(SD_log_effective_area_ctl)[1:3])
  if(x %in% c(1:2)) dimnames(Index_ctl) <- list(Category = c("Spawners","Juveniles"), Year = year_labels, Stratum = NA)
   if(x==3)dimnames(Index_ctl) <- list(Category = c("Juveniles"), Year = year_labels, Stratum = NA)

  sd_Index_ctl=array(SD_log_effective_area_ctl[,,,'Std. Error'],dim(SD_log_effective_area_ctl)[1:3])
  if(x %in% c(1:2)) dimnames(sd_Index_ctl) <- list(Category = c("Spawners","Juveniles"), Year = year_labels, Stratum = NA)
  if(x == 3) dimnames(sd_Index_ctl) <- list(Category = c("Juveniles"), Year = year_labels, Stratum = NA)

  df1 <- reshape2::melt(Index_ctl) %>% rename("Estimate" = value)
  df2 <- reshape2::melt(sd_Index_ctl) %>% rename("SD" = value)
  df <- full_join(df1, df2) %>% mutate(Model = name)
  return(df)
})
eao <- do.call(rbind, eao_byModel)

p <- ggplot(eao) +
  geom_ribbon(aes(x = Year, ymin = Estimate - 1.96*SD, ymax = Estimate + 1.96*SD, fill = Model), alpha = 0.25) +
  # geom_point(aes(x = Year, y = Estimate, color = Model), cex = 3) +
  geom_line(aes(x = Year, y = Estimate, color = Model), lwd = 2) +
  coord_cartesian(ylim = c(0,max(eao$Estimate + 1.96 * eao$SD)*1.01)) +
  facet_grid(~Category) +
  ylab("Effective area occupied (km^2)") +
  theme_bw(base_size = 14) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")
ggsave(file.path(fig_dir, "Compare_effective_area_occupied.png"), p, height = 6, width = 14)
