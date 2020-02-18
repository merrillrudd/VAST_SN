rm(list=ls())

####################
## load packages
####################

# devtools::install_github("james-thorson/VAST", ref = "3.1.0")
devtools::install_github("james-thorson/VAST", ref = "development")
devtools::install_github("merrillrudd/VASTPlotUtils")

library(VAST)
library(tidyverse)
library(VASTPlotUtils)
library(ggthemes)

###################
## Directories
###################

# main_dir <- "C:\\merrill\\OR_coho"
main_dir <- "~/Projects/Spatiotemporal/VAST_SN/Oregon_coho"
data_dir <- file.path(main_dir, "data")

sil_dir <- file.path(main_dir, "Siletz")
dir.create(sil_dir, showWarnings=FALSE)

fig_dir <- file.path(sil_dir, "figures")
dir.create(fig_dir, showWarnings=FALSE)

####################
## Read in data
####################

obs <- readRDS(file.path(data_dir, "siletz_observations.rds"))

## adjust juvenile year to spawning year
obs$Year[which(obs$Survey == "Juveniles")] <- obs$Year[which(obs$Survey == "Juveniles")] - 1

hab <- readRDS(file.path(data_dir, "siletz_habitat_interp.rds"))
network <- readRDS(file.path(data_dir, "siletz_network.rds"))

spawn_info <- read.csv(file.path(data_dir, "WildSpawnerAbundance.csv")) %>%
		select(Year, Siletz) %>%
		rename(value = Siletz) %>%
		mutate(Category = "Spawners")

####################
## Network
####################
net_nodes <- network$child_s
Network_sz_LL <- network
Network_sz <- Network_sz_LL %>% select("parent_s", "child_s", "dist_s")
net <- plot_network(Network_sz_LL = Network_sz_LL, arrows=TRUE, root=TRUE)
# ggsave(file.path(fig_dir, "Network.png"), net)

####################
## Observations
####################
## upstream child nodes along network
net_nodes <- network$child_s

## upstream child node for each observation
obs_nodes <- obs$child_i

## make sure all are in the upstream network nodes
all(obs_nodes %in% net_nodes)

## label category names
cat_names <- unique(obs$Survey)

## use density data
Data_dens <- data.frame( "Catch_KG" = obs$Density, 
              "Year" = as.numeric(obs$Year),
               "Vessel" = "missing", 
               "AreaSwept_km2" = 1,
               "Lat" = obs$Lat, 
               "Lon" = obs$Lon, 
               "Pass" = 0,
               "Knot" = unlist(obs$child_i),
               "Category" = obs$Survey,
               "CategoryNum" = sapply(1:length(obs$Survey), function(x) ifelse(obs$Survey[x]=="Spawners", 1, ifelse(obs$Survey[x]=="Juveniles", 2, NA))))

## spawners only density data
Data_dens_spawn <- Data_dens %>% filter(Category == "Spawners")
Data_dens_juv <- Data_dens %>% filter(Category == "Juveniles")

## count data as alternative, but juvenile data makes more sense to use density data
Data_count <- data.frame( "Catch_KG" = obs$Count, 
              "Year" = as.numeric(obs$Year),
               "Vessel" = "missing", 
               "AreaSwept_km2" = obs$dist_i,
               "Lat" = obs$Lat, 
               "Lon" = obs$Lon, 
               "Pass" = 0,
               "Knot" = unlist(obs$child_i),
               "Category" = obs$Survey,
               "CategoryNum" = sapply(1:length(obs$Survey), function(x) ifelse(obs$Survey[x]=="Spawners", 1, ifelse(obs$Survey[x]=="Juveniles", 2, NA))))
Data_count$Catch_KG[which(Data_count$Category == "Juveniles")] <- obs$Density[which(obs$Survey == "Juveniles")]
Data_count$AreaSwept_km2[which(Data_count$Category == "Juveniles")] <- 1

Data_count_spawn <- Data_count %>% filter(Category == "Spawners")
Data_count_juv <- Data_count %>% filter(Category == "Juveniles")

## plot all observations on the network
p_obs <- plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens, arrows=TRUE)

## plot all observations by year
p_obs_y <- plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens, arrows=TRUE, byYear=TRUE)

## plot only spawners by density
p_obs_y_spawn <- plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens_spawn, value_label = "Observed spawner density", obs_color = RColorBrewer::brewer.pal(3, "Set1")[2], arrows=TRUE, byYear=TRUE, byValue=TRUE)

## plot only juveniles by density
p_obs_y_juv <- plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens_juv, value_label = "Observed juvenile density", obs_color = RColorBrewer::brewer.pal(3, "Set1")[1], arrows=TRUE, byYear=TRUE, byValue=TRUE)

####################
## Habitat covariates
####################

## rename land cover covariates from names to values
hab_sub <- hab %>% filter(variable == "land_cover")
land_cover_names <- unique(hab_sub$value)
land_cover_num <- seq_along(land_cover_names)
hab_sub$value2 <- sapply(1:nrow(hab_sub), function(x) land_cover_num[which(land_cover_names==hab_sub$value[x])])

hab_df <- hab %>% mutate(value2 = value)
hab_df[which(hab_df$variable == "land_cover"),"value2"] <- hab_sub$value2
hab_df <- unique(hab_df)
hab_df <- hab_df %>% filter(variable != "Coho_distr")

habvar <- unique(hab_df$variable)
n_p <- length(habvar)
var_names <- c("Land cover", "Gradient", "Secondary channel area", "Large wood volume", "Large wood volume\nper 100m", "Primary channel area", "Pools per 100m", "Percent slackwater pools", "Weighted gravel in riffles")
hab_df$variable2 <- sapply(1:nrow(hab_df), function(x) var_names[which(habvar == hab_df$variable[x])])
# hab_df$value2[which(hab_df$variable=="Coho_distr" & hab_df$value2 == 1)] <- max(hab_df$value2)
# hab_df$value2[which(hab_df$variable=="Coho_distr" & hab_df$value2 == 0)] <- min(hab_df$value2)

phab <- ggplot(hab_df) +
	geom_point(aes(x=Lon,y=Lat,color=value2)) +
	guides(color = FALSE) +
	ggtitle("Land cover") +
	xlab("Longitude") + ylab("Longitude") +
	scale_color_viridis_d() +
   	facet_wrap(.~variable2) +
	theme_bw()	
# ggsave(file.path(fig_dir, "Habitat_scaled.png"), phab, height = 8, width = 10)

habvar_df <- data.frame("variable"=habvar, 
						"HabitatImpact"=sapply(1:length(habvar), function(x) unique(unlist(hab_df %>% filter(variable==habvar[x]) %>% select(HabitatImpact)))))
habvar_df$toUse <- 1
habvar_df[which(habvar_df$variable %in% c("PCTSWPOOL", "LWDVOL1")),"toUse"] <- 0
habvar_df$HabitatImpact <- as.character(habvar_df$HabitatImpact)
habvar_df$HabitatImpact[which(habvar_df$HabitatImpact=="Coho")] <- "SpawnersJuveniles"

## Setup habitat covariates
## number of network nodes
n_x <- nrow(Network_sz_LL)

## number of years
years <- min(Data_count$Year):max(Data_count$Year)
n_t <- length(min(Data_count$Year):max(Data_count$Year))
n_t_spawn <- length(min(Data_count_spawn$Year):max(Data_count_spawn$Year))
n_t_juv <- length(min(Data_count_juv$Year):max(Data_count_juv$Year))

nobs_juv <- lapply(1:n_t, function(x){
	sub <- Data_dens_juv %>% filter(Year == years[x])
	out <- data.frame("year"=years[x], "nobs" = nrow(sub))
	return(out)
})
nobs_juv <- do.call(rbind, nobs_juv)

nobs_spawn <- lapply(1:n_t, function(x){
	sub <- Data_dens_spawn %>% filter(Year == years[x])
	out <- data.frame("year"=years[x], "nobs" = nrow(sub))
	return(out)
})
nobs_spawn <- do.call(rbind, nobs_spawn)

## number of habitat covariates
n_p <- length(habvar)

## number of observations
n_i <- nrow(Data_count)
n_i_spawn <- nrow(Data_count_spawn)
n_i_juv <- nrow(Data_count_juv)

### all data
X_gtp_all <- array(0, dim=c(n_x,n_t,n_p))
for(p in 1:n_p){
	psub <- hab_df %>% filter(variable == habvar[p])
	mat <- matrix(0, nrow=n_x, ncol=1)
	mat[psub$child_s,1] <- as.numeric(psub$value2)
	mat_sd <- (mat - mean(mat))/sd(mat)
	X_gtp_all[,,p] <- mat_sd
}

X_itp_all <- array(0, dim=c(n_i,n_t,n_p))
for(i in 1:n_i){
	for(p in 1:n_p){
		knot <- Data_count$Knot[i]
		index <- which(net_nodes == knot)
		X_itp_all[i,,p] <- X_gtp_all[index,,p]
	}
}

Xconfig_all <- array(1, dim=c(2,2,n_p))
## remove related
Xconfig_all[,,which(habvar_df$toUse==0)] <- 0
## remove things that don't apply to spawners
Xconfig_all[,1,which(grepl("Spawners",habvar_df$HabitatImpact)==FALSE)] <- 0
## remove things that don't apply to juveniles
Xconfig_all[,2,which(grepl("Juveniles",habvar_df$HabitatImpact)==FALSE)] <- 0

## more basic habitat variables
Xconfig_all2 <- Xconfig_all
Xconfig_all2[,,which(habvar %in% c("land_cover", "Coho_distr")==FALSE)] <- 0

## remove landcover
Xconfig_all3 <- Xconfig_all
Xconfig_all3[,,which(habvar == c("land_cover"))] <- 0


## spawners
X_gtp_spawn <- array(0, dim=c(n_x,n_t_spawn,n_p))
for(p in 1:n_p){
	psub <- hab_df %>% filter(variable == habvar[p])
	mat <- matrix(0, nrow=n_x, ncol=1)
	mat[psub$child_s,1] <- as.numeric(psub$value2)
	mat_sd <- (mat - mean(mat))/sd(mat)
	X_gtp_spawn[,,p] <- mat_sd
}

X_itp_spawn <- array(0, dim=c(n_i_spawn,n_t_spawn,n_p))
for(i in 1:n_i_spawn){
	for(p in 1:n_p){
		knot <- Data_count_spawn$Knot[i]
		index <- which(net_nodes == knot)
		X_itp_spawn[i,,p] <- X_gtp_spawn[index,,p]
	}
}

Xconfig_spawn <- array(Xconfig_all[,1,], dim=c(dim(Xconfig_all)[1], 1, dim(Xconfig_all)[3]))
Xconfig_spawn2 <- Xconfig_spawn
Xconfig_spawn2[,,which(habvar %in% c("land_cover", "Coho_distr")==FALSE)] <- 0

## juveniles
X_gtp_juv <- array(0, dim=c(n_x,n_t_juv,n_p))
for(p in 1:n_p){
	psub <- hab_df %>% filter(variable == habvar[p])
	mat <- matrix(0, nrow=n_x, ncol=1)
	mat[psub$child_s,1] <- as.numeric(psub$value2)
	mat_sd <- (mat - mean(mat))/sd(mat)
	X_gtp_juv[,,p] <- mat_sd
}

X_itp_juv <- array(0, dim=c(n_i_juv,n_t_juv,n_p))
for(i in 1:n_i_juv){
	for(p in 1:n_p){
		knot <- Data_count_juv$Knot[i]
		index <- which(net_nodes == knot)
		X_itp_juv[i,,p] <- X_gtp_juv[index,,p]
	}
}

Xconfig_juv <- array(Xconfig_all[,2,], dim=c(dim(Xconfig_all)[1], 1, dim(Xconfig_all)[3]))
Xconfig_juv2 <- Xconfig_juv
Xconfig_juv2[,,which(habvar %in% c("land_cover", "Coho_distr")==FALSE)] <- 0


##################
## Catchability
##################
Q_ik <- ThorsonUtilities::vector_to_design_matrix(Data_dens[,"Category"])[,-2,drop=FALSE]


save.image(file.path(sil_dir, "general_inputs.Rdata"))


#############################
## spawners_landcover
## spawners, land cover, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, 'spawners_landcover')
dir.create(path, showWarnings=FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)


## spawners only
Data <- Data_count_spawn
# Data$Catch_KG[which(Data$Catch_KG > 0)] <- log(Data$Catch_KG[which(Data$Catch_KG > 0)])

## turn on spatial and spatiotemporal effects
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=1)

## IID structure on temporal intercepts
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

## gamma distribution, conventional delta link model
ObsModel = c("PosDist"=11,"Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
            "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings(Version = "VAST_v8_2_0", n_x = nrow(Network_sz), Region = "Stream_network", FieldConfig=FieldConfig, RhoConfig=RhoConfig, OverdispersionConfig=OverdispersionConfig, Options=Options, ObsModel=ObsModel, purpose = "index", fine_scale=FALSE, bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# check estimated parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn,
                  Xconfig_zcp = Xconfig_spawn2,
                  test_fit = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[["beta1_ft"]] <- factor(rep(NA, length(Par[["beta1_ft"]])))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Par[["gamma1_ctp"]])))

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn, 
                  Xconfig_zcp = Xconfig_spawn2,
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn,
                  Xconfig_zcp = Xconfig_spawn2,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par))

fit$parameter_estimates$diagnostics

saveRDS(fit, file.path(path, "Fit.rds"))    

fit <- readRDS(file.path(path, "Fit.rds")) 
spawn_fit <- fit

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Spawners", cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Spawners", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Spawners", cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Spawners")

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = "Spawners", add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)

#############################
## juveniles_landcover
## juveniles, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, 'juveniles_landcover')
dir.create(path, showWarnings=FALSE)
setwd(path)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)


## spawners only
Data <- Data_count_juv
# Data$Catch_KG[which(Data$Catch_KG > 0)] <- log(Data$Catch_KG[which(Data$Catch_KG > 0)])

## turn on spatial and spatiotemporal effects
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=1)

## IID structure on temporal intercepts
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

## gamma distribution, conventional delta link model
ObsModel = c("PosDist"=11,"Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
            "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings(Version = "VAST_v8_2_0", n_x = nrow(Network_sz), Region = "Stream_network", FieldConfig=FieldConfig, RhoConfig=RhoConfig, OverdispersionConfig=OverdispersionConfig, Options=Options, ObsModel=ObsModel, purpose = "index", fine_scale=FALSE, bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# check estimated parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_juv, X_itp = X_itp_juv,
                  Xconfig_zcp = Xconfig_juv2,
                  test_fit = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[["beta1_ft"]] <- factor(rep(NA, length(Par[["beta1_ft"]])))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Par[["gamma1_ctp"]])))

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_juv, X_itp = X_itp_juv, 
                  Xconfig_zcp = Xconfig_juv2,
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_juv, X_itp = X_itp_juv,
                  Xconfig_zcp = Xconfig_juv2,
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par))

fit$parameter_estimates$diagnostics

saveRDS(fit, file.path(path, "Fit.rds"))    

fit <- readRDS(file.path(path, "Fit.rds")) 
juv_fit <- fit

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Juveniles")

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = "Juveniles", Plot_suffix = "Count", interval_width = 1.96)


#############################
## multivar_landcover_IID
## multivariate, land cover & coho distr, discrete -- IID
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_landcover_IID")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"="IID")

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  # Q_ik = Q_ik,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[['logSigmaM']][2] <- NA
# Map[["logSigmaM"]] <- factor(Map[["logSigmaM"]])
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))


# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  # Q_ik = Q_ik,
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)


#############################
## multivar_landcover
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")



#############################
## multivar_landcover_ST
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_ST")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=0)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")



#############################
## multivar_landcover_tempRW
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_tempRW")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=2, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")


#############################
## multivar_landcover_AR1
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_AR1")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=2, "Epsilon1"=0, "Epsilon2"=2)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

fit1 <- readRDS(file.path(path, "fit1.rds"))
## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")



#############################
## multivar_habsurvey
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_habsurvey")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

fit1 <- readRDS(file.path(path, "fit1.rds"))

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3, 
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par))

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

## density
quant <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(quant), max(quant)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(quant), max(quant)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")


#############################
## multivar_habsurvey_tempRW
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_habsurvey_tempRW")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=2, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[["logSigmaM"]] <- factor(rep(NA, length(Map[["logSigmaM"]])))
# Par[["logkappa2"]] <- 0.01

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

fit1 <- readRDS(file.path(path, "fit1.rds"))

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all3, 
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par))

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

## density
quant <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(quant), max(quant)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(quant), max(quant)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")





#############################
## juveniles_landcover_spawn
## juveniles, habitat, and spawner covariate
#############################
path <- file.path(sil_dir, "juveniles_landcover_spawn")
unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

Data <- Data_count_juv

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=1)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=11,"Link"=0)

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
                        purpose = "index", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

X_gtp_juv_spawn <- array(0, dim=c(dim(X_gtp_juv)[1:2], dim(X_gtp_juv)[3]+1))
X_gtp_juv_spawn[,,1:dim(X_gtp_juv)[3]] <- X_gtp_juv

X_itp_juv_spawn <- array(0, dim=c(dim(X_itp_juv)[1:2], dim(X_itp_juv)[3]+1))
X_itp_juv_spawn[,,1:dim(X_itp_juv)[3]] <- X_itp_juv


sdens <- spawn_fit$Report$D_gcy[,1,]
spawn_years <- unique(Data_count_spawn$Year)[order(unique(Data_count_spawn$Year))]
juv_years <- unique(Data_count_juv$Year)[order(unique(Data_count_juv$Year))]
which(spawn_years %in% juv_years)
X_gtp_juv_spawn[,2:21,dim(X_gtp_juv_spawn)[3]] <- sdens

X_itp_juv_spawn[,2:21,dim(X_gtp_juv_spawn)[3]] <- sdens[which(net_nodes %in% Data_count_juv$Knot)]

Xconfig_juv_spawn <- array(1,dim=c(2,1,dim(X_gtp_juv_spawn)[3]))
Xconfig_juv_spawn[,,1:dim(X_gtp_juv)[3]] <- Xconfig_juv2

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  working_dir=path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_juv_spawn, X_itp = X_itp_juv_spawn,
                  Xconfig_zcp = Xconfig_juv_spawn,
                  test_fit = FALSE)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Map[['gamma1_ctp']] <- factor(rep(NA, length(Map[['gamma1_ctp']])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))
# Map[['logSigmaM']][1] <- NA
# Map[["logSigmaM"]] <- factor(Map[["logSigmaM"]])

# first model run
fit1 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_juv_spawn, X_itp = X_itp_juv_spawn,
                  Xconfig_zcp = Xconfig_juv_spawn, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_iz"=Data[,'Year'], 
                  "c_i"=rep(0,nrow(Data)), 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map),
                  X_gtp = X_gtp_juv_spawn, X_itp = X_itp_juv_spawn,
                  Xconfig_zcp = Xconfig_juv_spawn, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Juveniles", cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Juveniles"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Juveniles"), Plot_suffix = "Count", interval_width = 1.96)





df <- data.frame("Model" = c("multivar_habsurvey",
                              "multivar_landcover",
                              "multivar_landcover_IID",
                              "multivar_landcover_tempRW"))
df$AIC <- NULL
for(i in 1:nrow(df)){
   res <- readRDS(file.path(sil_dir, df[i,"Model"], "Fit.rds"))
   aic <- as.numeric(res$parameter_estimates$AIC)
   df[i,"AIC"] <- aic
}
df$dAIC <- sapply(1:nrow(df), function(x) df[x,"AIC"] - min(df[,"AIC"]))
df[order(df$dAIC),]