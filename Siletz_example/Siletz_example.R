####################################################
## Example of how to use VAST for stream networks
## Oregon coastal coho data
####################################################

## install VAST from GitHub
devtools::install_github("james-thorson/VAST")

## install some additional utilities for plotting
devtools::install_github("merrillrudd/VASTPlotUtils")

## libraries
library(VAST)
library(VASTPlotUtils)
library(tidyverse)

####################
## Directory structure
####################

## change to your directory
main_dir <- "~/VAST_SN/Siletz_example"
data_dir <- file.path(main_dir, "data")

####################
## Read in data
####################

## data for the Siletz region
obs <- readRDS(file.path(data_dir, "siletz_observations.rds"))
hab <- readRDS(file.path(data_dir, "siletz_habitat_interp.rds"))
network <- readRDS(file.path(data_dir, "siletz_network.rds"))


####################
## Network
####################
## stream segments defined by unique upstream (child) node,  labeled from 1 to n nodes
## required format for network with lat and lon info for the upstream (child) node
Network_sz_LL <- network
nrow(Network_sz_LL) == length(unique(Network_sz_LL$child_s))

## parents may have multiple child nodes, but each child has only one parent
## must specify an additional segment for the furthest downstream node, with:
##    parent_s = 0, 
##    child_s = the parent of the furthest downsream segment,
##    dist_s = Inf
root <- Network_sz_LL %>% filter(parent_s == 0)
final_segment <- Network_sz_LL %>% filter(parent_s == root$child_s)

## additional required format for network with only parent, child, and distance info
## (used for some functions within fit_model)
Network_sz <- network %>% select("parent_s", "child_s", "dist_s")

## plots network with arrows pointing downstream, and root node highlighted (furthest downstream)
VASTPlotUtils::plot_network(Network_sz_LL = Network_sz_LL, arrows=TRUE, root=TRUE)

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

phab <- ggplot(hab_df) +
	geom_point(aes(x=Lon,y=Lat,color=value2)) +
	guides(color = FALSE, fill = guide_legend(title = "Category")) +
	ggtitle("Land cover") +
	xlab("Longitude") + ylab("Longitude") +
	scale_color_viridis_d() +
	scale_fill_viridis_d() +
   facet_wrap(.~variable) +
	mytheme()	

## choose land cover for this example
hab_toUse <- hab_df %>% filter(variable == "land_cover")
				
## Setup habitat covariates
## number of network nodes
n_x <- nrow(Network_sz_LL)

## number of years
n_t <- length(min(Data_dens$Year):max(Data_dens$Year))
n_t_spawn <- length(min(Data_dens_spawn$Year):max(Data_dens_spawn$Year))

## number of habitat covariates (here, land cover)
n_p <- 1

## number of observations
n_i <- nrow(Data_dens)
n_i_spawn <- nrow(Data_dens_spawn)

### standardised input for habitat covariates by network node, time, covariate
## for both spawners and juveniles
X_gtp_all <- array(0, dim=c(n_x,n_t,n_p))
mat <- matrix(0, nrow=n_x, ncol=1)
mat[hab_toUse$child_s,1] <- as.numeric(hab_toUse$value2)
mat_sd <- (mat - mean(mat))/sd(mat)
X_gtp_all[,,1] <- mat_sd

### standardised input for habitat covariates by observation node, time, covariate
## for both spawners and juveniles
X_itp_all <- array(0, dim=c(n_i,n_t,n_p))
for(i in 1:n_i){
	knot <- Data_dens$Knot[i]
	index <- which(net_nodes == knot)
	X_itp_all[i,,1] <- X_gtp_all[index,,1]
}

## specifies which habitat covariates to include for spawners and juveniles
## can use this to include or exclude covariates for a model component and/or category
Xconfig_all <- array(1, dim=c(2,2,n_p))

## repeat above process for only spawners
## number of years in spawner dataset may vary
X_gtp_spawn <- array(0, dim=c(n_x,n_t_spawn,n_p))
mat <- matrix(0, nrow=n_x, ncol=1)
mat[hab_toUse$child_s,1] <- as.numeric(hab_toUse$value2)
mat_sd <- (mat - mean(mat))/sd(mat)
X_gtp_spawn[,,1] <- mat_sd

## number of observations in spawner dataset will vary 
X_itp_spawn <- array(0, dim=c(n_i_spawn,n_t_spawn,n_p))
for(i in 1:n_i_spawn){
   knot <- Data_dens_spawn$Knot[i]
   index <- which(net_nodes == knot)
   X_itp_spawn[i,,1] <- X_gtp_spawn[index,,1]
}

## to use in single-category model, requires different dimensions than in two-category model
Xconfig_spawn <- array(Xconfig_all[,1,], dim=c(dim(Xconfig_all)[1], 1, dim(Xconfig_all)[3]))

#############################
## run univariate model
#############################
path <- file.path(main_dir, "univariate")
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

Data <- Data_dens_spawn

## turn on spatial and spatiotemporal effects for both components
FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1)

## IID structure on temporal intercepts
RhoConfig = c("Beta1"=1, "Beta2"=1, "Epsilon1"=0, "Epsilon2"=0)

## density data = continuous model
## choose gamma distribution
ObsModel = c("PosDist"=2,"Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
            "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings(n_x = nrow(Network_sz), 
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

# compile model and check parameters, mapped parameters, and model structure
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
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn,
                  Xconfig_zcp = Xconfig_spawn)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map

# first model run without calculating standard errors
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
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn,
                  Xconfig_zcp = Xconfig_spawn, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that all parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

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
                  X_gtp = X_gtp_spawn, X_itp = X_itp_spawn,
                  Xconfig_zcp = Xconfig_spawn, 
                  test_fit = FALSE)

## check model estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## read in model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

## plot maps
VASTPlotUtils::plot_maps(plot_set = c(1,2,3,7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Spawners", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Spawners", cex = 0.75, Panel = "Year", Zlim = c(-3,6))

## encounter probability diagnostic -- similar to QQ plot but for encounter probability
Enc_prob <- plot_encounter_diagnostic(Report = fit$Report, Data_Geostat = Data, DirName = fig)

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Spawners")

#############################
## run multivariate model
#############################
path <- file.path(main_dir, "multivariate")
dir.create(path, showWarnings = FALSE)

fig <- file.path(path, "figures")
dir.create(fig, showWarnings=FALSE)

Data <- Data_dens

## turn on spatial and spatiotemporal effects
## two factors -- one for each category (spawners and juveniles)
FieldConfig = c("Omega1"=2, "Epsilon1"=2, "Omega2"=2, "Epsilon2"=2)

## random walk structure on temporal intercepts and spatiotemporal random effect
## not much information for juveniles, model needs a little more structure to converge
RhoConfig = c("Beta1"=2, "Beta2"=2, "Epsilon1"=2, "Epsilon2"=2)

## density data = continuous model
## choose gamma distribution
ObsModel = c("PosDist"=2,"Link"=0)

## other options
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
Options =  c("Calculate_Range"=1, 
            "Calculate_effective_area"=1)

## wrapper function to set up common settings
settings <- make_settings(n_x = nrow(Network_sz),
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
                  Xconfig_zcp = Xconfig_all)


Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map

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
                  Xconfig_zcp = Xconfig_all, 
                  optimize_args = list(getsd=FALSE, newtonsteps=0),
                  test_fit = FALSE)

## check that parameters are identifiable
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj) 

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
                  Xconfig_zcp = Xconfig_all, 
                  test_fit = FALSE)

## check parameter estimates and final gradients
fit$parameter_estimates$diagnostics

## save model fit
saveRDS(fit, file.path(path, "Fit.rds"))    

## load model fit
fit <- readRDS(file.path(path, "Fit.rds")) 

## plot maps by category
VASTPlotUtils::plot_maps(plot_set = c(1,2,3,7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = cat_names, cex = 0.5)

## plot maps by year
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = cat_names, cex = 0.75, Panel = "Year", Zlim = c(-3,8))

## encounter probability diagnostic
Enc_prob <- plot_encounter_diagnostic(Report = fit$Report, Data_Geostat = Data, DirName = fig)

## effective area occupied and center of gravity by category
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = cat_names)
