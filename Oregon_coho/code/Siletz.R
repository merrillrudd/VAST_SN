rm(list=ls())

####################
## load packages
####################

# devtools::install_github("james-thorson/VAST", ref = "3.1.0")
devtools::install_github("james-thorson/VAST", ref = "3.4.0")
library(VAST)

devtools::install_github("james-thorson/FishStatsUtils", ref = "development")
library(FishStatsUtils)

devtools::install_github("florianhartig/DHARMa", subdir = "DHARMa", dependencies = TRUE)
devtools::install_github("merrillrudd/VASTPlotUtils")
# devtools::install_github("james-thorson/FishStatsUtils", ref = "development", force = TRUE)


library(DHARMa)
library(VASTPlotUtils)

# devtools::load_all("~/Projects/Spatiotemporal/FishStatsUtils")
# devtools::load_all("C://merrill/DHARMa/DHARMa")
# devtools::load_all("C://merrill/TMB_contrib_R/TMBhelper")
# devtools::load_all("C://merrill//FishStatsUtils")
# devtools::load_all("C://merrill//VASTPlotUtils")

library(tidyverse)
library(ggthemes)

###################
## Directories
###################

# main_dir <- "C:\\merrill\\VAST_SN\\Oregon_coho"
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

spawn_info <- read.csv(file.path(data_dir, "WildSpawnerAbundance.csv"))
colnames(spawn_info)[1] <- "Year"
spawn_info <- spawn_info %>%
		select(Year, Siletz) %>%
		rename(value = Siletz) %>%
		mutate(Category = "Spawners")

####################
## Network
####################
net_nodes <- network$child_s
Network_sz_LL <- network
Network_sz <- Network_sz_LL %>% select("parent_s", "child_s", "dist_s")
# net <- plot_network(Network_sz_LL = Network_sz_LL, arrows=TRUE, root=TRUE)
# ggsave(file.path(fig_dir, "Network.png"), net)

## network plot for manuscript
Network_sz_LL_info <- Network_sz_LL
Network_sz_LL_info$Network <- "Node"
Network_sz_LL_info$Network[which(Network_sz_LL_info$parent_s == 0)] <- "Root"



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

# phab <- ggplot(hab_df) +
# 	geom_point(aes(x=Lon,y=Lat,color=value2)) +
# 	guides(color = FALSE) +
# 	xlab("Longitude") + ylab("Longitude") +
# 	scale_color_viridis_d() +
#    	facet_wrap(.~variable2) +
#    ggtitle("Habitat covariates (standardised)") +
# 	theme_bw()	
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

path <- file.path(sil_dir, 'spawners_landcover_dist11')
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
Version = FishStatsUtils::get_latest_version()
settings <- make_settings(Version = "VAST_v8_2_0", n_x = nrow(Network_sz), Region = "Stream_network", FieldConfig=FieldConfig, RhoConfig=RhoConfig, OverdispersionConfig=OverdispersionConfig, Options=Options, ObsModel=ObsModel, purpose = "index2", fine_scale=FALSE, bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1
# Version = "VAST_v8_2_0", 

# check estimated parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
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

fit1 <- readRDS(file.path(path, "fit1.rds"))

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
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

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = "Spawners", add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)
VASTPlotUtils::plot_residuals(fit = fit, Data = Data, Network_sz_LL = Network_sz_LL, category_names = "Spawners", FilePath = fig, ObsModel = 11)

dir.create(file.path(fig, "extra"))
Plots = plot(fit1,
             working_dir=paste0(fig,"/extra/"),
             land_color=rgb(0,0,0,0),
             quantreg=TRUE )

dharmaRes = summary( fit, what="residuals")
plot(dharmaRes, quantreg = TRUE)
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 800)
plotQQunif(dharmaRes)
dev.off()

testResiduals(dharmaRes)


#############################
## juveniles_landcover
## juveniles, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, 'juveniles_landcover_dist11')
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
settings <- make_settings(Version = "VAST_v8_2_0", n_x = nrow(Network_sz), Region = "Stream_network", FieldConfig=FieldConfig, RhoConfig=RhoConfig, OverdispersionConfig=OverdispersionConfig, Options=Options, ObsModel=ObsModel, purpose = "index2", fine_scale=FALSE, bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# check estimated parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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

dharmaRes = summary( fit, what="residuals")
png(file.path(fig, ""))
plot(dharmaRes, quantreg = TRUE)
# Various potential plots
png(file.path(fig, "QQplot.png"), height = 600, width = 800)
plotQQunif(dharmaRes)
dev.off()

testResiduals(dharmaRes)


#############################
## multivar_landcover_IID
## multivariate, land cover & coho distr, discrete -- IID
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_landcover_IID_dist11")
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
                        purpose = "index2", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE,
						            knot_method = "samples")
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

fit1 <- readRDS(file.path(path, "fit1.rds"))

### 2nd logSigmaR estimated close to zero
# Par <- fit1$ParHat
# Map <- fit1$tmb_list$Map
# Map$logSigmaM = factor( c(1,NA,2,3,NA,NA) )

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  # Map = Map,
                  # Parameters = Par,
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

testResiduals(dharmaRes)

#############################
## multivar_landcover_IID
## multivariate, land cover & coho distr, discrete -- IID
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_landcover_IID_dist5")
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

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

### 2nd logSigmaR estimated close to zero
Par <- fit1$ParHat
Map <- fit1$tmb_list$Map
Map$logSigmaM = factor( c(1,NA,2,3,NA,NA) )

saveRDS(fit1, file.path(path, "fit1.rds"))
## check initial model estimates
fit1$parameter_estimates$diagnostics

## run the model
fit = fit_model( "settings"=settings, 
                 "Lat_i"=Data[,"Lat"], 
                 "Lon_i"=Data[,"Lon"], 
                 "t_i"=Data[,'Year'], 
                 "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                 "b_i"=Data[,'Catch_KG'], 
                 "a_i"=Data[,'AreaSwept_km2'], 
                 "v_i"=Data[,'Vessel'], 
                 working_dir = path,
                 extrapolation_args=list(
                   input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                 spatial_args=list(Network_sz_LL=Network_sz_LL),
                 Network_sz = Network_sz,
                 Map = Map,
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

testResiduals(dharmaRes)

########################
## multivar_landcover_IID_gamma
## multivariate, land cover & coho distr, discrete -- IID
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))
path <- file.path(sil_dir, "multivar_landcover_IID_gamma")
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
FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"=0, "Epsilon2"=0)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=2,"Link"=1)

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
                           bias.correct=FALSE,
                           knot_method = "samples")
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
                  "t_i"=Data[,'Year'], 
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

fit1 <- readRDS(file.path(path, "fit1.rds"))

## run the model
fit = fit_model( "settings"=settings, 
                 "Lat_i"=Data[,"Lat"], 
                 "Lon_i"=Data[,"Lon"], 
                 "t_i"=Data[,'Year'], 
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
VASTPlotUtils::plot_maps(plot_set = c(6,7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)

dharmaRes = summary( fit, what="residuals")
# Various potential plots
plot(dharmaRes)
plot(dharmaRes, quantreg = TRUE)
plot(dharmaRes, quantreg = FALSE)
plotQQunif(dharmaRes)
hist(dharmaRes )

plotResiduals(dharmaRes, rank = TRUE, quantreg = FALSE)
testResiduals(dharmaRes)

#############################
## multivar_landcover_gamma
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_gamma")
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
FieldConfig = c("Omega1"=2, "Epsilon1"=2, "Omega2"=0, "Epsilon2"=0)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=2,"Link"=1)

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
                        bias.correct=FALSE,
                        knot_method = "samples")
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=as.numeric(Data[,'Year']), 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  model_args = list(Map = Map, startval = fit1$parameter_estimates$par),
                  X_gtp = X_gtp_all, X_itp = X_itp_all,
                  Xconfig_zcp = Xconfig_all2, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens_base <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens_base), max(dens_base)))
VASTPlotUtils::plot_maps(plot_set = c(6,7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens_base), max(dens_base)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))



bi <- VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)

ji <- bi %>% filter(Category == "Juveniles") %>% 
   mutate(Proplow = Ybound_low/Estimate) %>%
   mutate(Prophigh = Ybound_high/Estimate)
mean(ji$Proplow)
mean(ji$Prophigh)

si <- bi %>% filter(Category == "Spawners") %>%
   left_join(spawn_info)
si$cover <- sapply(1:nrow(si), function(x) ifelse(si$value[x] <= si$Ybound_high[x] & si$value[x] >= si$Ybound_low[x], 1, 0))

dharmaRes = summary( fit, what="residuals")
saveRDS(dharmaRes,file.path(path, "DHARMa_res.rds"))
# Various potential plots
plot(dharmaRes)
png(file.path(fig, "DHARMa_residual_analysis.png"), height = 600, width = 800)
plot(dharmaRes, quantreg = TRUE)
dev.off()

plot(dharmaRes, quantreg = FALSE)
plotQQunif(dharmaRes)
hist(dharmaRes )

plotResiduals(dharmaRes, rank = TRUE, quantreg = FALSE)
testResiduals(dharmaRes)

#############################
## multivar_landcover_lognormal
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_lognormal")
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
FieldConfig = c("Omega1"=2, "Epsilon1"=2, "Omega2"=0, "Epsilon2"=0)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

ObsModel = c("PosDist"=1,"Link"=1)

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
                           bias.correct=FALSE,
                           knot_method = "samples")
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

# compile model and check parameters
fit0 = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=as.numeric(Data[,'Year']), 
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
                  "t_i"=Data[,'Year'], 
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
                 "t_i"=Data[,'Year'], 
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

dens_base <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens_base), max(dens_base)))
VASTPlotUtils::plot_maps(plot_set = c(6,7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens_base), max(dens_base)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))



bi <- VASTPlotUtils::plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)

ji <- bi %>% filter(Category == "Juveniles") %>% 
  mutate(Proplow = Ybound_low/Estimate) %>%
  mutate(Prophigh = Ybound_high/Estimate)
mean(ji$Proplow)
mean(ji$Prophigh)

si <- bi %>% filter(Category == "Spawners") %>%
  left_join(spawn_info)
si$cover <- sapply(1:nrow(si), function(x) ifelse(si$value[x] <= si$Ybound_high[x] & si$value[x] >= si$Ybound_low[x], 1, 0))

dharmaRes = summary( fit, what="residuals")
saveRDS(dharmaRes,file.path(path, "DHARMa_res.rds"))
# Various potential plots
plot(dharmaRes)
png(file.path(fig, "DHARMa_residual_analysis.png"), height = 600, width = 800)
plot(dharmaRes, quantreg = TRUE)
dev.off()

plot(dharmaRes, quantreg = FALSE)
plotQQunif(dharmaRes)
hist(dharmaRes )

plotResiduals(dharmaRes, rank = TRUE, quantreg = FALSE)
testResiduals(dharmaRes)

#############################
## multivar_landcover_rm2017
## multivariate, land cover & coho distr, discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar_landcover_rm2017")
# unlink(path, TRUE)
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

## remove final year
all_years <- unique(Data_count$Year)[order(unique(Data_count$Year))]
juvyr_index <- which(Data_count$Year == max(all_years) & Data_count$Category == "Juveniles")
Data <- Data_count[-juvyr_index,]
X_gtp_inp <- X_gtp_all
X_itp_inp <- X_itp_all[-juvyr_index,,]



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
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
                  X_gtp = X_gtp_inp, X_itp = X_itp_inp,
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
                  "t_i"=Data[,'Year'], 
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
                  X_gtp = X_gtp_inp, X_itp = X_itp_inp,
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
                  "t_i"=Data[,'Year'], 
                  "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                  "b_i"=Data[,'Catch_KG'], 
                  "a_i"=Data[,'AreaSwept_km2'], 
                  "v_i"=Data[,'Vessel'], 
                  working_dir = path,
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  X_gtp = X_gtp_inp, X_itp = X_itp_inp,
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
VASTPlotUtils::plot_maps(plot_set = c(7,11,13,14), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

bi <- plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count", interval_width = 1.96)

Data_count[juvyr_index,]
find_yr <- fit$Report$D_gcy[,,dim(fit$Report$D_gcy)[3]]
find_knot <- find_yr[Data_count[juvyr_index,"Knot"],]
find_juv <- find_knot[,2]


#############################
## multivar
## multivariate,  discrete
#############################
load(file.path(sil_dir, "general_inputs.Rdata"))

path <- file.path(sil_dir, "multivar")
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
                  extrapolation_args=list(
                    input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])),
                  spatial_args=list(Network_sz_LL=Network_sz_LL),
                  Network_sz = Network_sz,
                  run_model = FALSE,
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
                  "t_i"=Data[,'Year'], 
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
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

## check initial model estimates
fit1$parameter_estimates$diagnostics


saveRDS(fit1, file.path(path, "fit1.rds"))

## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
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
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

dens_base <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(5), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)

VASTPlotUtils::plot_maps(plot_set = c(7,13,14), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.75, Panel = "Year", Zlim = c(min(dens), max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))

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
FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=3, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=2)

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
                  "t_i"=Data[,'Year'], 
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

## check initial model estimates
fit1$parameter_estimates$diagnostics

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1 <- readRDS(file.path(path, "fit1.rds"))
## run the model
fit = fit_model( "settings"=settings, 
                  "Lat_i"=Data[,"Lat"], 
                  "Lon_i"=Data[,"Lon"], 
                  "t_i"=Data[,'Year'], 
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
                  test_fit = FALSE,
                  optimize_args = list(startpar = fit1$parameter_estimates$par))

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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                        purpose = "index2", 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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
                  "t_i"=Data[,'Year'], 
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

##########################
## crossval
##########################

load(file.path(sil_dir, "general_inputs.Rdata"))

cpath <- file.path(sil_dir, "crossval-loo")
dir.create(cpath)
fig <- file.path(cpath, "figures")
dir.create(fig)

## model settings
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
                        purpose = "index2", 
                        fine_scale=FALSE, 
                        bias.correct=FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1

## Data
set.seed(123)
Data_count <- Data_count %>% mutate(Obs = 1:nrow(Data_count))
choose <- sample(Data_count$Obs[which(Data_count$Category == "Juveniles")], length(which(Data_count$Category == "Juveniles")))

# ## count how many have been tested
# tested <- 0
# found <- 0
# check_save <- NULL
# for(ind in 1:length(choose)){
   ind <- 1
   tested <- tested + 1 

   path <- file.path(cpath, ind)
   dir.create(path, showWarnings = FALSE)
   fig <- file.path(path, "figures")
   dir.create(fig, showWarnings=FALSE)

   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

   find <- which(Data_count$Obs == choose[ind])
   Data <- Data_count %>% filter(Obs != choose[ind])
   X_itp_inp <- X_itp_all[-find,,]

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
                        extrapolation_args=list(
                          input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                        spatial_args=list(Network_sz_LL=Network_sz_LL),
                        Network_sz = Network_sz,
                        X_gtp = X_gtp_all, X_itp = X_itp_inp,
                        Xconfig_zcp = Xconfig_all2, 
                        optimize_args = list(getsd=FALSE, newtonsteps=0),
                        test_fit = FALSE)    

      ## check that parameters are identifiable
      check <- tryCatch(TMBhelper::Check_Identifiable(fit1$tmb_list$Obj), error = function(e) "highgradient")

            ## if all parameters are identifiable:
            saveRDS(fit1, file.path(path, "fit1.rds"))   

            ## run the model
            fit = tryCatch(fit_model( "settings"=settings, 
                              "Lat_i"=Data[,"Lat"], 
                              "Lon_i"=Data[,"Lon"], 
                              "t_i"=Data[,'Year'], 
                              "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                              "b_i"=Data[,'Catch_KG'], 
                              "a_i"=Data[,'AreaSwept_km2'], 
                              "v_i"=Data[,'Vessel'], 
                              working_dir = path,
                              extrapolation_args=list(
                                input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                              spatial_args=list(Network_sz_LL=Network_sz_LL),
                              Network_sz = Network_sz,
                              X_gtp = X_gtp_all, X_itp = X_itp_inp,
                              Xconfig_zcp = Xconfig_all2, 
                              test_fit = FALSE), error = function(e) NA)

               ## save model file
               saveRDS(fit, file.path(path, "Fit.rds")) 

      ## load model fit
      fit <- readRDS(file.path(path, "Fit.rds"))            

      dens <- quantile(log(fit$Report$D_gcy))
      VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
      VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
        

      ## plot effective area occupied and center of gravity
      VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))            

      plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")  


   ind <- 2

   path <- file.path(cpath, ind)
   dir.create(path, showWarnings = FALSE)
   fig <- file.path(path, "figures")
   dir.create(fig, showWarnings=FALSE)

   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

   find <- which(Data_count$Obs == choose[ind])
   Data <- Data_count %>% filter(Obs != choose[ind])
   X_itp_inp <- X_itp_all[-find,,]

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
                        extrapolation_args=list(
                          input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                        spatial_args=list(Network_sz_LL=Network_sz_LL),
                        Network_sz = Network_sz,
                        X_gtp = X_gtp_all, X_itp = X_itp_inp,
                        Xconfig_zcp = Xconfig_all2, 
                        optimize_args = list(getsd=FALSE, newtonsteps=0),
                        test_fit = FALSE)    

      ## check that parameters are identifiable
      check <- tryCatch(TMBhelper::Check_Identifiable(fit1$tmb_list$Obj), error = function(e) "highgradient")

            ## if all parameters are identifiable:
            saveRDS(fit1, file.path(path, "fit1.rds"))   

   ind <- 3

   path <- file.path(cpath, ind)
   dir.create(path, showWarnings = FALSE)
   fig <- file.path(path, "figures")
   dir.create(fig, showWarnings=FALSE)

   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

   find <- which(Data_count$Obs == choose[ind])
   Data <- Data_count %>% filter(Obs != choose[ind])
   X_itp_inp <- X_itp_all[-find,,]

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
                        extrapolation_args=list(
                          input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                        spatial_args=list(Network_sz_LL=Network_sz_LL),
                        Network_sz = Network_sz,
                        X_gtp = X_gtp_all, X_itp = X_itp_inp,
                        Xconfig_zcp = Xconfig_all2, 
                        optimize_args = list(getsd=FALSE, newtonsteps=0),
                        test_fit = FALSE)    

      ## check that parameters are identifiable
      check <- tryCatch(TMBhelper::Check_Identifiable(fit1$tmb_list$Obj), error = function(e) "highgradient")

            ## if all parameters are identifiable:
            saveRDS(fit1, file.path(path, "fit1.rds"))   

   ind <- 4

   path <- file.path(cpath, ind)
   dir.create(path, showWarnings = FALSE)
   fig <- file.path(path, "figures")
   dir.create(fig, showWarnings=FALSE)

   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

   find <- which(Data_count$Obs == choose[ind])
   Data <- Data_count %>% filter(Obs != choose[ind])
   X_itp_inp <- X_itp_all[-find,,]

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
                        extrapolation_args=list(
                          input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                        spatial_args=list(Network_sz_LL=Network_sz_LL),
                        Network_sz = Network_sz,
                        X_gtp = X_gtp_all, X_itp = X_itp_inp,
                        Xconfig_zcp = Xconfig_all2, 
                        optimize_args = list(getsd=FALSE, newtonsteps=0),
                        test_fit = FALSE)    

      ## check that parameters are identifiable
      check <- tryCatch(TMBhelper::Check_Identifiable(fit1$tmb_list$Obj), error = function(e) "highgradient")

            ## if all parameters are identifiable:
            saveRDS(fit1, file.path(path, "fit1.rds"))   

   ind <- 5

   path <- file.path(cpath, ind)
   dir.create(path, showWarnings = FALSE)
   fig <- file.path(path, "figures")
   dir.create(fig, showWarnings=FALSE)

   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.cpp"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.so"), to = path)
   ignore <- file.copy(from = file.path(sil_dir, "VAST_v8_2_0.o"), to = path)

   find <- which(Data_count$Obs == choose[ind])
   Data <- Data_count %>% filter(Obs != choose[ind])
   X_itp_inp <- X_itp_all[-find,,]

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
                        extrapolation_args=list(
                          input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                        spatial_args=list(Network_sz_LL=Network_sz_LL),
                        Network_sz = Network_sz,
                        X_gtp = X_gtp_all, X_itp = X_itp_inp,
                        Xconfig_zcp = Xconfig_all2, 
                        optimize_args = list(getsd=FALSE, newtonsteps=0),
                        test_fit = FALSE)    

      ## check that parameters are identifiable
      check <- tryCatch(TMBhelper::Check_Identifiable(fit1$tmb_list$Obj), error = function(e) "highgradient")

            ## if all parameters are identifiable:
            saveRDS(fit1, file.path(path, "fit1.rds"))   
            ## run the model
            fit = tryCatch(fit_model( "settings"=settings, 
                              "Lat_i"=Data[,"Lat"], 
                              "Lon_i"=Data[,"Lon"], 
                              "t_i"=Data[,'Year'], 
                              "c_i"=as.numeric(Data[,"CategoryNum"]) - 1, 
                              "b_i"=Data[,'Catch_KG'], 
                              "a_i"=Data[,'AreaSwept_km2'], 
                              "v_i"=Data[,'Vessel'], 
                              working_dir = path,
                              extrapolation_args=list(
                                input_grid=cbind("Lat"=Data[,"Lat"], "Lon"=Data[,"Lon"],"child_i"=Data[,"Knot"],"Area_km2"=Data[,"AreaSwept_km2"])), 
                              spatial_args=list(Network_sz_LL=Network_sz_LL),
                              Network_sz = Network_sz,
                              X_gtp = X_gtp_all, X_itp = X_itp_inp,
                              Xconfig_zcp = Xconfig_all2, 
                              test_fit = FALSE), error = function(e) NA)

               ## save model file
               saveRDS(fit, file.path(path, "Fit.rds")) 

      ## load model fit
      fit <- readRDS(file.path(path, "Fit.rds"))            

      dens <- quantile(log(fit$Report$D_gcy))
      VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5, Zlim = c(min(dens), max(dens)))
      VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Spawners", "Juveniles"), cex = 0.5)
        

      ## plot effective area occupied and center of gravity
      VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Spawners", "Juveniles"))            

      plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Spawners", "Juveniles"), add = spawn_info, Plot_suffix = "Count")  


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
base <- readRDS(file.path(sil_dir, "multivar_landcover", "Fit.rds"))
iid <- readRDS(file.path(sil_dir, "multivar_landcover_IID", "Fit.rds"))
juv <- readRDS(file.path(sil_dir, "juveniles_landcover", "Fit.rds"))

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
