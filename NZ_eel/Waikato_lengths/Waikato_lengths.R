  rm(list=ls())

  # devtools::install_github("james-thorson/VAST", ref = "development")
  devtools::install_github("merrillrudd/VASTPlotUtils")

  library(VAST)
  library(VASTPlotUtils)
  library(TMB)
  library(tidyverse)
  library(RColorBrewer)
  library(proj4)
  library(foreach)
  library(doParallel)



################
## Directories
################

main_dir <- "/home/merrill/Projects/Spatiotemporal/VAST_SN/NZ_eel"
# R_dir <- file.path(main_dir, "R")
# R_files <- list.files(R_dir)
# readr <- sapply(1:length(R_files), function(x) source(file.path(R_dir, R_files[x])))

data_dir <- file.path(main_dir, "data")

res_dir <- file.path(main_dir, "Waikato_lengths")
dir.create(res_dir, showWarnings=FALSE)

fig_dir <- file.path(res_dir, "figures")
dir.create(fig_dir, showWarnings=FALSE)

#########################
## read in data
##########################
####################################
## Network
###################################

network <- readRDS(file.path(data_dir, "Greater_waikato_network.rds"))

## add a small value to the latitude and longitude where we don't have updated location for the nodes emptying into the ocean
# network$lat[which(network$parent_s == 0)] <- network$lat[which(network$parent_s == 0)] + 0.00001
# network$long[which(network$parent_s == 0)] <- network$long[which(network$parent_s == 0)] + 0.00001

## format network data
Network_sz = network %>% select(c('parent_s','child_s','dist_s'))
Network_sz_LL = network %>% select(c('parent_s', 'child_s', 'dist_s', 'lat', 'long')) %>%
  rename("Lon"=long, "Lat"=lat)

pnet1 <- plot_network(Network_sz_LL, arrows = TRUE, root = TRUE)

##############################
### observations
##############################
obs <- readRDS(file.path(data_dir, "Greater_waikato_observations.rds")) %>% select(-c(parent_s,child_s)) %>% rename(Year = year, dist_i = dist_s)
####################################
## habitat information
###################################
hab <- readRDS(file.path(data_dir, "Greater_waikato_habitat.rds"))
covar_toUse <- c('MeanFlowCumecs','Dist2Coast_FromMid','loc_elev','loc_slope',"local_twarm",'loc_rnvar','DamAffected') #
hab <- hab %>% filter(covariate %in% covar_toUse)

# ggplot(hab %>% filter(covariate == "DamAffected")) + geom_point(aes(x = easting, y = northing, color = value))

nodes <- network$child_s[order(network$child_s)]
years <- unique(obs$Year)[order(unique(obs$Year))]
covar <- unique(hab$covariate)
n_x <- length(nodes)
n_t <- length(years)
n_p <- length(covar)

hab_std <- lapply(1:length(covar), function(x){
  sub <- hab %>% 
    filter(covariate == covar[x]) %>%
    mutate(value_std = (value - mean(value))/sd(value))
  return(sub)
})
hab_std <- do.call(rbind, hab_std)

p_habstd <- ggplot(hab_std) +
  geom_point(aes(x = easting, y = northing, color = value_std)) +
  facet_wrap(.~covariate, nrow = 2) +
  scale_color_distiller(palette = "Spectral") +
  scale_x_continuous(breaks = quantile(hab$easting, prob = c(0.1, 0.5, 0.95)), labels = round(quantile(hab$easting, prob = c(0.1,0.5,0.95)),0)) +
  guides(colorbar = guide_legend(title = "Standardised\nvalue")) +
  theme_bw(base_size = 14) 
ggsave(file.path(fig_dir, "Habitat_covariates_standardised.png"), p_habstd, width = 15, height = 8)

for(i in 1:length(covar)){
  p <- ggplot(hab_std %>% filter(covariate == covar[i])) +
  geom_point(aes(x = easting, y = northing, color = value)) +
  guides(color=guide_legend(title=covar[i])) +
  scale_color_distiller(palette = "Spectral") +
  theme_minimal()
  ggsave(file.path(fig_dir, paste0("Habitat_covariate_", covar[i],".png")),p, width=10, height=8)
}

# plot_network(Network_sz_LL = Network_sz_LL, Data_Geostat = Data_Geostat_all, byYear = TRUE, root = TRUE, FilePath = fig_dir, FileName = "Waikato_network")
X_gtp_input1 <- array(0, dim=c(n_x, n_t, n_p))
for(p in 1:n_p){
  psub <- hab %>% filter(covariate == covar[p])
  mat <- matrix(0, nrow=n_x, ncol = 1)
  mat[psub$child_s,1] <- psub$value
  if(covar[p]=="DamAffected"){
    X_gtp_input1[,,p] <- mat
  } else {
      mat_sd <- (mat - mean(mat, na.rm=TRUE))/sd(mat, na.rm=TRUE)
      X_gtp_input1[,,p] <- mat_sd
  }
}
## years since dam impact
X_choose <- X_gtp_input1[,,which(covar == "DamAffected")]
X_gtp1 <- sapply(1:length(years), function(x){
  sub <- X_choose[,x]
  sub[which(sub == 1)] <- years[x] - 1935
  return(sub)
})
X_gtp1_sd <- (X_gtp1 - mean(X_gtp1))/sd(X_gtp1)

# ## years since impact squared
# X_gtp2 <- sapply(1:length(years), function(x){
#   sub <- X_choose[,x]
#   sub[which(sub == 1)] <- (years[x] - 1935)^2
#   return(sub)
# })
# X_gtp2_sd <- (X_gtp2 - mean(X_gtp2))/sd(X_gtp2)

# covar2 <- c(covar, "YearsSinceDam","YearsSinceDam2")[-which(covar=="DamAffected")]
covar2 <- c(covar, "YearsSinceDam")[-which(covar=="DamAffected")]
n_p <- length(covar2)
X_gtp_input <- array(0, dim=c(n_x,n_t,n_p))
for(p in 1:(n_p)){
  ## skip dam affected
  if(p < length(covar)) X_gtp_input[,,p] <- X_gtp_input1[,,p]
   # X_gtp_input[,,p] <- X_gtp_input1[,,p]

  ## in place of dam affected, years since dam
  if(p == length(covar)) X_gtp_input[,,p] <- X_gtp1_sd

  ## additional covariate, years since dam squared
  # if(p ==length(covar)+1) X_gtp_input[,,p] <- X_gtp2_sd
}



###################
## encounters
###################
## convert to encounters
obs$encounter <- sapply(1:nrow(obs), function(x) ifelse(obs$count[x] >= 1, 1, obs$count[x]))

Data_encounter0 <- data.frame( "Catch_KG" = obs$encounter, 
              "Year" = as.numeric(obs$Year),
               "AreaSwept_km2" = obs$dist_i, 
               "Lat" = obs$lat, 
               "Lon" = obs$long, 
               "Pass" = 0,
               "Knot" = obs$child_i,
               "Category" = "Longfin_eels")
Data_encounter <- unique(Data_encounter0)

pnet <- ggplot(Network_sz_LL) + geom_point(aes(x = Lon, y = Lat), col = "gray", alpha = 0.6) + xlab("Longitude") + ylab("Latitude") + theme_bw(base_size = 14)
pobs <- pnet + geom_point(data = Data_encounter, aes(x = Lon, y = Lat))
pobs2 <- pnet + 
	geom_point(data = Data_encounter, aes(x = Lon, y = Lat)) +
	facet_wrap(.~Year)
pobs3 <- pnet +
	geom_point(data = Data_encounter, aes(x = Lon, y = Lat, color = factor(Catch_KG))) +
	scale_color_brewer(palette = "Set1") +
	guides(color = guide_legend(title = "Encounter")) +
	facet_wrap(.~Year)
ggsave(file.path(fig_dir, "Obs_encounter.png"), pobs3, height = 7, width = 8)

##### add small value to encounter observations
set.seed(1234)
present <- Data_encounter$Catch_KG
devs <- rnorm(length(present), 0, 0.01)
present_new <- sapply(1:length(present), function(x) ifelse(present[x]==1, present[x]+devs[x], present[x]))
Data_encounter$Catch_KG <- present_new

n_i_enc <- nrow(Data_encounter)
## match habitat covariates to observations
## double check the indices will match up properly
X_itp_enc <- array(0, dim=c(n_i_enc,n_t,n_p))
for(i in 1:n_i_enc){
  for(p in 1:n_p){
    child_i <- obs$child_i[i]
    index <- which(nodes == child_i)
    X_itp_enc[i,,p] <- X_gtp_input[index,,p]
  }
}

###################
## counts
###################
## convert to counts
# obs$encounter <- sapply(1:nrow(obs), function(x) ifelse(obs$count[x] >= 1, 1, obs$count[x]))
obs_count <- obs %>% select(Year, count, dist_i, long, lat, child_i) %>%
	group_by(Year, dist_i, long, lat, child_i) %>%
	summarise(count = sum(count))


Data_count <- data.frame( "Catch_KG" = obs_count$count, 
              "Year" = as.numeric(obs_count$Year),
               "AreaSwept_km2" = obs_count$dist_i, 
               "Lat" = obs_count$lat, 
               "Lon" = obs_count$long, 
               "Pass" = 0,
               "Knot" = obs_count$child_i,
               "Category" = "Longfin_eels") %>%
	mutate(Density = Catch_KG /AreaSwept_km2)
Data_dens <- Data_count
Data_dens$Catch_KG <- Data_dens$Density
Data_dens$AreaSwept_km2 <- 1

pnet <- ggplot(Network_sz_LL) + geom_point(aes(x = Lon, y = Lat), col = "gray", alpha = 0.6) + xlab("Longitude") + ylab("Latitude") + theme_bw(base_size = 14)
pobs3 <- pnet +
	geom_point(data = Data_count, aes(x = Lon, y = Lat, size = Density)) +
	# scale_color_brewer(palette = "Set1") +
	# guides(color = guide_legend(title = "Encounter")) +
	facet_wrap(.~Year)
ggsave(file.path(fig_dir, "Obs_count.png"), pobs3, height = 7, width = 8)

n_i_count <- nrow(Data_count)
## match habitat covariates to observations
## double check the indices will match up properly
X_itp_count <- array(0, dim=c(n_i_count,n_t,n_p))
for(i in 1:n_i_count){
  for(p in 1:n_p){
    child_i <- obs$child_i[i]
    index <- which(nodes == child_i)
    X_itp_count[i,,p] <- X_gtp_input[index,,p]
  }
}

###################
## Length category
###################
absent <- obs %>% filter(Category == "Absent")
present <- obs %>% filter(Category != "Absent")
present$Category <- as.numeric(present$Category)
quantile(present$Category)

present$Cat2 <- sapply(1:nrow(present), function(x) ifelse(present$Category[x] < 250, "Under250", "Over250"))
absent1 <- absent %>% mutate(Cat2 = "Under250")
absent2 <- absent %>% mutate(Cat2 = "Over250")
obs_len2 <- rbind.data.frame(present, absent1, absent2)
obs_len2 <- obs_len2 %>% select(Year, count, dist_i, long, lat, child_i, Cat2) %>%
	group_by(Year, dist_i, long, lat, child_i, Cat2) %>%
	summarise(count = sum(count))
obs_len2$CatNum <- sapply(1:nrow(obs_len2), function(x) ifelse(obs_len2$Cat2[x] == "Under250", 1, 2))

Data_len <- data.frame( "Catch_KG" = obs_len2$count, 
              "Year" = as.numeric(obs_len2$Year),
               "AreaSwept_km2" = obs_len2$dist_i, 
               "Lat" = obs_len2$lat, 
               "Lon" = obs_len2$long, 
               "Pass" = 0,
               "Knot" = obs_len2$child_i,
               "Category" = obs_len2$Cat2,
               "CategoryNum" = obs_len2$CatNum) %>%
	mutate(Density = Catch_KG /AreaSwept_km2)

pnet <- ggplot(Network_sz_LL) + geom_point(aes(x = Lon, y = Lat), col = "gray", alpha = 0.6) + xlab("Longitude") + ylab("Latitude") + theme_bw(base_size = 14)
pobs3 <- pnet +
	geom_point(data = Data_len, aes(x = Lon, y = Lat, size = Density, color = Category), alpha = 0.6) +
	scale_color_brewer(palette = "Set1") +
	# guides(color = guide_legend(title = "Encounter")) +
	facet_wrap(.~Year)
ggsave(file.path(fig_dir, "Obs_length_cut200.png"), pobs3, height = 7, width = 8)

n_i_len <- nrow(Data_len)
## match habitat covariates to observations
## double check the indices will match up properly
X_itp_len <- array(0, dim=c(n_i_len,n_t,n_p))
for(i in 1:n_i_len){
  for(p in 1:n_p){
    child_i <- obs$child_i[i]
    index <- which(nodes == child_i)
    X_itp_len[i,,p] <- X_gtp_input[index,,p]
  }
}

###################
## 3 length categories
###################
absent <- obs %>% filter(Category == "Absent")
present <- obs %>% filter(Category != "Absent")
present$Category <- as.numeric(present$Category)
quantile(present$Category)

present$Cat3 <- sapply(1:nrow(present), function(x) ifelse(present$Category[x] < 180, "Under180", ifelse(present$Category[x] < 330, "Under330", "Above330")))
absent1 <- absent %>% mutate(Cat3 = "Under180")
absent2 <- absent %>% mutate(Cat3 = "Under330")
absent3 <- absent %>% mutate(Cat3 = "Above330")
obs_len2 <- rbind.data.frame(present, absent1, absent2, absent3)
obs_len2 <- obs_len2 %>% select(Year, count, dist_i, long, lat, child_i, Cat3) %>%
  group_by(Year, dist_i, long, lat, child_i, Cat3) %>%
  summarise(count = sum(count))
obs_len2$CatNum <- sapply(1:nrow(obs_len2), function(x) ifelse(obs_len2$Cat3[x] == "Under180", 1, ifelse(obs_len2$Cat3[x] == "Under330", 2, 3)))
obs_len2$Cat3 <- factor(obs_len2$Cat3, levels = c("Under180", "Under330", "Above330"))

Data_len3 <- data.frame( "Catch_KG" = obs_len2$count, 
              "Year" = as.numeric(obs_len2$Year),
               "AreaSwept_km2" = obs_len2$dist_i, 
               "Lat" = obs_len2$lat, 
               "Lon" = obs_len2$long, 
               "Pass" = 0,
               "Knot" = obs_len2$child_i,
               "Category" = obs_len2$Cat3,
               "CategoryNum" = obs_len2$CatNum) %>%
  mutate(Density = Catch_KG /AreaSwept_km2)

pnet <- ggplot(Network_sz_LL) + geom_point(aes(x = Lon, y = Lat), col = "gray", alpha = 0.6) + xlab("Longitude") + ylab("Latitude") + theme_bw(base_size = 14)
pobs3 <- pnet +
  geom_point(data = Data_len3, aes(x = Lon, y = Lat, size = Density, color = Category), alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  # guides(color = guide_legend(title = "Encounter")) +
  facet_wrap(.~Year)
ggsave(file.path(fig_dir, "Obs_length_3cat.png"), pobs3, height = 7, width = 8)

n_i_len <- nrow(Data_len3)
## match habitat covariates to observations
## double check the indices will match up properly
X_itp_len <- array(0, dim=c(n_i_len,n_t,n_p))
for(i in 1:n_i_len){
  for(p in 1:n_p){
    child_i <- obs$child_i[i]
    index <- which(nodes == child_i)
    X_itp_len[i,,p] <- X_gtp_input[index,,p]
  }
}


save.image(file.path(res_dir, "general_inputs.Rdata"))
##########################
## encounter1
## spatiotemporal, spatial, temporal, 
## beta1 IID,
## habitat covariate
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "encounter1")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_encounter
X_gtp_inp <- X_gtp_input
X_itp_inp <- X_itp_enc
Xconfig_zcp_inp <- array(1, dim = c(2,1,n_p))
Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0)
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- cbind("PosDist" = 1, "Link" = 0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1$parameter_estimates$diagnostics

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

plot_maps(plot_set = c(1,6,13), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin_eel", cex = 0.5)
plot_maps(plot_set = c(1), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin_eel", cex = 0.1, Panel = "Year")

map_list <- make_map_info(Region = settings$Region, spatial_list = fit$spatial_list, Extrapolation_List = fit$extrapolation_list)
Enc_prob <- plot_encounter_diagnostic(Report = fit$Report, Data_Geostat = Data_inp, DirName = fig)
plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Longfin_eels")
plot_residuals(ObsModel = 1, fit = fit, Data = Data_inp, Network_sz_LL = Network_sz_LL, category_names = "Longfin_eels", FilePath = fig)


##########################
## encounter1_nohab
## spatiotemporal, spatial, temporal, 
## beta1 IID,
## habitat covariate
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "encounter1_nohab")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_encounter
X_gtp_inp <- NULL
X_itp_inp <- NULL
Xconfig_zcp_inp <- NULL

FieldConfig <- c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 0, "Epsilon2" = 0)
RhoConfig <- c("Beta1" = 2, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- cbind("PosDist" = 1, "Link" = 0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
Par[["logkappa1"]] <- 2

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1 <- readRDS(file.path(path, "fit1.rds"))

fit1$parameter_estimates$diagnostics

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

plot_maps(plot_set = c(1,6), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin_eel", cex = 0.5)
plot_maps(plot_set = c(1), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin_eel", cex = 0.1, Panel = "Year")

map_list <- make_map_info(Region = settings$Region, spatial_list = fit$spatial_list, Extrapolation_List = fit$extrapolation_list)
Enc_prob <- plot_encounter_diagnostic(Report = fit$Report, Data_Geostat = Data_inp, DirName = fig)
plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Longfin_eels")
plot_residuals(ObsModel = 1, fit = fit, Data = Data_inp, Network_sz_LL = Network_sz_LL, category_names = "Longfin_eels", FilePath = fig)



##########################
## count1
## spatiotemporal, spatial, temporal, 
## beta1 IID,
## habitat covariate
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "count1")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_count
X_gtp_inp <- X_gtp_input
X_itp_inp <- X_itp_count
Xconfig_zcp_inp <- array(1, dim = c(2,1,n_p))
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 1)
RhoConfig <- c("Beta1" = 3, "Beta2" = 2, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- cbind("PosDist" = 11, "Link" = 0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1$parameter_estimates$diagnostics

fit1 <- readRDS(file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Longfin eel")
plot_residuals(ObsModel = 11, fit = fit, Data = Data_inp, Network_sz_LL = Network_sz_LL, category_names = "Longfin_eels", FilePath = fig)


##########################
## count1_nohab
## spatiotemporal, spatial, temporal, 
## beta1 IID,
## habitat covariate
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "count1_nohab")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_count
X_gtp_inp <- NULL
X_itp_inp <- NULL
Xconfig_zcp_inp <- NULL
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 1)
RhoConfig <- c("Beta1" = 3, "Beta2" = 2, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- cbind("PosDist" = 11, "Link" = 0)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1$parameter_estimates$diagnostics

fit1 <- readRDS(file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = rep(0, nrow(Data_inp)),
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = "Longfin eel", cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = "Longfin eel")
plot_residuals(ObsModel = 11, fit = fit, Data = Data_inp, Network_sz_LL = Network_sz_LL, category_names = "Longfin_eels", FilePath = fig)

##########################
## length1_nohab
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length1_nohab")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len
X_gtp_inp <- NULL
X_itp_inp <- NULL
Xconfig_zcp_inp <- NULL
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),2),nrow=2, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under250","Over250"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under250","Over250"), Plot_suffix = "Count", interval_width = 1.96)

plot_residuals(ObsModel = 11, fit = fit, Data = Data_inp, Network_sz_LL = Network_sz_LL, category_names = c("Under 250 mm", "Over 250 mm"), FilePath = fig)


##########################
## length2_nohab
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length2_nohab")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len3
X_gtp_inp <- NULL
X_itp_inp <- NULL
Xconfig_zcp_inp <- NULL
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 1, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),3),nrow=3, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
# Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under180","Under330","Over330"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under180","Under330","Over330"), Plot_suffix = "Count", interval_width = 1.96)

##########################
## length1_dam
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length1_dam")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len
X_gtp_inp <- array(X_gtp_input[,,which(grepl("Dam", covar2))], dim = c(dim(X_gtp_input)[1:2],1))
X_itp_inp <- array(X_itp_len[,,which(grepl("Dam", covar2))], dim = c(dim(X_itp_len)[1:2],1))
Xconfig_zcp_inp <- array(1, dim = c(2,2,1))
Xconfig_zcp_inp[1,,] <- 0
Xconfig_zcp_inp[,2,] <- 0
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 1, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),2),nrow=2, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

# Map[["beta2_ft"]][which(Map[["beta2_ft"]]==2)] <- NA
# Map[["beta2_ft"]] <- factor(Map[["beta2_ft"]])

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under250","Over250"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under250","Over250"), Plot_suffix = "Count", interval_width = 1.96)

##########################
## length2_dam
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length2_dam")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len3
X_gtp_inp <- array(X_gtp_input[,,which(grepl("Dam", covar2))], dim = c(dim(X_gtp_input)[1:2],1))
X_itp_inp <- array(X_itp_len[,,which(grepl("Dam", covar2))], dim = c(dim(X_itp_len)[1:2],1))
Xconfig_zcp_inp <- array(1, dim = c(2,3,1))
Xconfig_zcp_inp[1,,] <- 0
# Xconfig_zcp_inp[,2,] <- 0
# Xconfig_zcp_inp[2,,] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 1, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),3),nrow=3, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

# Map[["beta2_ft"]][which(Map[["beta2_ft"]]==2)] <- NA
# Map[["beta2_ft"]] <- factor(Map[["beta2_ft"]])

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit1 <- readRDS(file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under180","Under330","Over330"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under180","Under330","Over330"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under180","Under330","Over330"), Plot_suffix = "Count", interval_width = 1.96)


##########################
## length1_allhab
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length1_allhab")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len
X_gtp_inp <- X_gtp_input
X_itp_inp <- X_itp_len
Xconfig_zcp_inp <- array(1, dim = c(2,2,n_p))
Xconfig_zcp_inp[1,,] <- 0
Xconfig_zcp_inp[2,2,n_p] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 1, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),2),nrow=2, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

# Map[["beta2_ft"]][which(Map[["beta2_ft"]]==2)] <- NA
# Map[["beta2_ft"]] <- factor(Map[["beta2_ft"]])

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under250","Over250"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under250","Over250"), Plot_suffix = "Count", interval_width = 1.96)

##########################
## length1_rmFlow
## spatiotemporal, spatial, temporal, 
## beta1 IID,
###########################
load(file.path(res_dir, "general_inputs.Rdata"))
path <- file.path(res_dir, "length1_rmFlow")
dir.create(path, showWarnings = FALSE)
fig <- file.path(path, "figures")
dir.create(fig, showWarnings = FALSE)

ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.cpp"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.o"), to = path)
ignore <- file.copy(from = file.path(res_dir, "VAST_v8_2_0.so"), to = path)

Data_inp <- Data_len
X_gtp_inp <- X_gtp_input
X_itp_inp <- X_itp_len
Xconfig_zcp_inp <- array(1, dim = c(2,2,n_p))
Xconfig_zcp_inp[,,which(grepl("Flow", covar2))] <- 0
Xconfig_zcp_inp[1,,] <- 0
Xconfig_zcp_inp[,2,which(grepl("Dam",covar2))] <- 0

FieldConfig <- c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = "IID", "Epsilon2" = "IID")
RhoConfig <- c("Beta1" = 3, "Beta2" = 1, "Epsilon1" = 0, "Epsilon2" = 0)
ObsModel <- matrix(rep(cbind("PosDist" = 11, "Link" = 0),2),nrow=2, byrow = TRUE)
OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0)
Options <- c("Calculate_range" = 1,
            "Calculate_effective_area" = 1)

settings <- make_settings(Version = "VAST_v8_2_0", 
                          n_x = nrow(Network_sz), 
                          Region = "Stream_network",
                          FieldConfig = FieldConfig,
                          RhoConfig = RhoConfig,
                          ObsModel = ObsModel,
                          OverdispersionConfig = OverdispersionConfig,
                          Options = Options,
                          purpose = "index",
                          fine_scale = FALSE, 
                          bias.correct = FALSE)
settings$Method <- "Stream_network"
settings$grid_size_km <- 1


fit0 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  run_model = FALSE)

Par <- fit0$tmb_list$Parameters
Map <- fit0$tmb_list$Map
# Par[["logkappa1"]] <- log(1/median(Network_sz$dist_s))
# Map[["gamma1_ctp"]] <- factor(rep(NA, length(Map[["gamma1_ctp"]])))
Map[["beta1_ft"]] <- factor(rep(NA, length(Map[["beta1_ft"]])))

# Map[["beta2_ft"]][which(Map[["beta2_ft"]]==2)] <- NA
# Map[["beta2_ft"]] <- factor(Map[["beta2_ft"]])

fit1 <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map, Par = Par),
                  optimize_args = list(getsd = FALSE, newtonsteps = 0),
                  test_fit = FALSE)
check <- TMBhelper::Check_Identifiable(fit1$tmb_list$Obj)

saveRDS(fit1, file.path(path, "fit1.rds"))

fit <- fit_model(settings = settings,
                  Lat_i = Data_inp[,"Lat"],
                  Lon_i = Data_inp[,"Lon"],
                  t_iz = Data_inp[,"Year"],
                  c_i = as.numeric(Data_inp[,"CategoryNum"]) - 1,
                  b_i = Data_inp[,"Catch_KG"],
                  a_i = Data_inp[,"AreaSwept_km2"],
                  working_dir = path,
                  extrapolation_args_input = list(input_grid = cbind("Lat" = Data_inp[,"Lat"], "Lon" = Data_inp[,"Lon"], "child_i" = Data_inp[,"Knot"], "Area_km2" = Data_inp[,"AreaSwept_km2"])),
                  spatial_args = list(Network_sz_LL = Network_sz_LL),
                  Network_sz = Network_sz,
                  Xconfig_zcp = Xconfig_zcp_inp,
                  X_gtp = X_gtp_inp, 
                  X_itp = X_itp_inp,
                  model_args = list(Map = Map),
                  optimize_args = list(startpar = fit1$parameter_estimates$par),
                  test_fit = FALSE)
saveRDS(fit, file.path(path, "Fit.rds"))

fit <- readRDS(file.path(path, "Fit.rds"))

fit$parameter_estimates$diagnostics

## plot maps
dens <- quantile(log(fit$Report$D_gcy))
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5, Zlim = c(min(dens),max(dens)))
VASTPlotUtils::plot_maps(plot_set = c(7), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.5)
VASTPlotUtils::plot_maps(plot_set = c(3), fit = fit, Sdreport = fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName = fig, category_names = c("Under250","Over250"), cex = 0.75, Panel = "Year", Zlim = c(min(dens),max(dens)))

## plot effective area occupied and center of gravity
VASTPlotUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = fig, Year_Set = fit$year_labels, use_biascorr = TRUE, category_names = c("Under250","Over250"))

plot_biomass_index(fit = fit, Sdreport = fit$parameter_estimates$SD, DirName = fig, category_names = c("Under250","Over250"), Plot_suffix = "Count", interval_width = 1.96)


load(file.path(res_dir, "general_inputs.Rdata"))
df <- data.frame(Model = c('length1_allhab',
                            'length1_nohab',
                            'length1_dam',
                            'length1_rmFlow'))
df$AIC <- NULL
for(i in 1:nrow(df)){
  res <- readRDS(file.path(res_dir, df[i,"Model"], "Fit.rds"))
  df$AIC[i] <- as.numeric(res$parameter_estimates$AIC)
}
df$dAIC <- df$AIC - min(df$AIC)
df[order(df$dAIC),]

df2 <- data.frame(Model = c('length2_nohab',
                            'length2_dam'))
df2$AIC <- NULL
for(i in 1:nrow(df2)){
  res <- readRDS(file.path(res_dir, df2[i,"Model"], "Fit.rds"))
  df2$AIC[i] <- as.numeric(res$parameter_estimates$AIC)
}
df2$dAIC <- df2$AIC - min(df2$AIC)
df2[order(df2$dAIC),]

df3 <- data.frame(Model = c('count1_nohab',
                            'count1'))
df3$AIC <- NULL
for(i in 1:nrow(df3)){
  res <- readRDS(file.path(res_dir, df3[i,"Model"], "Fit.rds"))
  df3$AIC[i] <- as.numeric(res$parameter_estimates$AIC)
}
df3$dAIC <- df3$AIC - min(df3$AIC)
df3[order(df3$dAIC),]



modelA <- readRDS(file.path(res_dir, "count1_nohab", "Fit.rds"))
modelB <- readRDS(file.path(res_dir, "length1_nohab", "Fit.rds"))
modelC <- readRDS(file.path(res_dir, "length2_nohab", "Fit.rds"))



## compare maps
ep_byModel <- lapply(1, function(x){
  # if(x == 1){
  #   Report <- modelA$Report
  #   year_labels = modelA$year_labels
  #   years_to_plot = modelA$years_to_plot
  #   spatial_list <- modelA$spatial_list
  #   name <- "Count data only"
  # }
  if(x == 1){
    Report <- modelB$Report
    year_labels = modelB$year_labels
    years_to_plot = modelB$years_to_plot
    spatial_list <- modelB$spatial_list
    name <- "Two length categories"
  }
  # if(x == 3){
  #   Report <- modelC$Report
  #   year_labels = modelC$year_labels
  #   years_to_plot = modelC$years_to_plot
  #   spatial_list <- modelC$spatial_list
  #   name <- "Three length categories"
  # }
  Array_xct = log(Report$D_gcy)
  if(x == 1) dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Under 250 mm", "Over 250 mm"), Year = year_labels)
  xct <- reshape2::melt(Array_xct) %>% mutate(Model = name)
  xctll <- full_join(xct, cbind.data.frame("Node" = 1:spatial_list$n_g,spatial_list$latlon_g))
  return(xctll)
})
ep <- do.call(rbind, ep_byModel)

plot_ep <- ep %>% filter(Year %in% c(2009,2013,2017))

p <- ggplot(plot_ep) +
  geom_point(aes(x = Lon, y = Lat, color = value), cex = 0.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  facet_grid(Year ~ Category) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Compare_density_maps.png"), p, height = 10, width = 12)

eao_byModel <- lapply(1, function(x){
  # if(x == 1){
  #   SD <- TMB::summary.sdreport(modelA$parameter_estimates$SD)
  #   TmbData <- modelA$data_list
  #   year_labels = modelA$year_labels
  #   years_to_plot = modelA$years_to_plot
  #   spatial_list <- modelA$spatial_list
  #   name <- "Spatiotemporal variation,\n Habitat & Method*Agency covariates"
  # }
  if(x == 1){
    SD <- TMB::summary.sdreport(modelB$parameter_estimates$SD)
    TmbData <- modelB$data_list
    year_labels = modelB$year_labels
    years_to_plot = modelB$years_to_plot
    spatial_list <- modelB$spatial_list
     name <- "Two length categories"
  }
  # if(x == 3){
  #   SD <- TMB::summary.sdreport(modelC$parameter_estimates$SD)
  #   TmbData <- modelC$data_list
  #   year_labels = modelC$year_labels
  #   years_to_plot = modelC$years_to_plot
  #   spatial_list <- modelC$spatial_list
  #   name <- "No spatiotemporal variation,\n Habitat & Method covariates"
  # }
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
  dimnames(Index_ctl) <- list(Category = c("Under 250 mm", "Over 250 mm"), Year = year_labels, Stratum = NA)

  sd_Index_ctl=array(SD_log_effective_area_ctl[,,,'Std. Error'],dim(SD_log_effective_area_ctl)[1:3])
  dimnames(sd_Index_ctl) <- list(Category = c("Under 250 mm", "Over 250 mm"), Year = year_labels, Stratum = NA)

  df1 <- reshape2::melt(Index_ctl) %>% rename("Estimate" = value)
  df2 <- reshape2::melt(sd_Index_ctl) %>% rename("SD" = value)
  df <- full_join(df1, df2) %>% mutate(Model = name)
  return(df)
})
eao <- do.call(rbind, eao_byModel)

p <- ggplot(eao) +
  geom_segment(aes(x = Year, xend = Year, y = Estimate - 1.96 * SD, yend = Estimate + 1.96 * SD), color = "red", lwd = 1.2) +
  geom_point(aes(x = Year, y = Estimate), color = "red", cex = 3) +
  geom_line(aes(x = Year, y = Estimate), color = "red") +
  coord_cartesian(ylim = c(0,max(eao$Estimate + 1.96 * eao$SD)*1.01)) +
  facet_grid(~Category) +
  ylab("Effective area occupied (km^2)") +
  scale_x_continuous(breaks = seq(2009,2017,length.out = 5)) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Compare_effective_area_occupied.png"), p, height = 6, width = 10)

## epsilon
eps_byModel <- lapply(1, function(x){
  if(x == 1){
    Report <- modelB$Report
    year_labels = modelB$year_labels
    years_to_plot = modelB$years_to_plot
    spatial_list <- modelB$spatial_list
    name <- "Two length categories"
  }
  
  Array_xct = Report$Epsilon2_gct
  dimnames(Array_xct) <- list(Node = 1:dim(Array_xct)[1], Category = c("Under 250 mm", "Over 250 mm"), Year = year_labels)
  xct <- reshape2::melt(Array_xct) %>% mutate(Model = name)
  xctll <- full_join(xct, cbind.data.frame("Node" = 1:spatial_list$n_g,spatial_list$latlon_g))
  return(xctll)
})
eps <- do.call(rbind, eps_byModel)

plot_eps <- eps %>% filter(Year %in% c(2009,2013,2017))

p <- ggplot(plot_eps) +
  geom_point(aes(x = Lon, y = Lat, color = abs(value)), cex = 2.5, alpha = 0.75) +
  scale_color_distiller(palette = "Spectral") +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(Year ~ Category) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color=guide_colourbar(title="Variation")) +
  theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Epsilon_compare.png"), p, height = 15, width = 10)



pr_byModel <- lapply(1, function(x){

    if(x == 1){
    Report <- modelB$Report
    year_labels = modelB$year_labels
    years_to_plot = modelB$years_to_plot
    spatial_list <- modelB$spatial_list
    df <- modelB$data_frame
    name <- "Two length categories"
  }


## Pearson resids for detection and catch rate
  D_i <- Report$R1_i*Report$R2_i
  PR1_i <- PR2_i <- rep(NA, length(D_i))
  for(i in 1:length(D_i)){
    
    ## bernoulli for presence
      mui <- Report$R1_i[i]
      obs <- as.numeric(df$b_i[i]>0)
      PR1_i[i] <- (obs-mui)/sqrt(mui*(1-mui)/1)
    
    ## NA for 0 observations
    obs <- df$b_i[i]
    if(obs>0){
      ## make sure to use the right variance as this depends on gear type
      # gr <- as.numeric(Data_Geostat$Gear[i])
      # if(ObsModel == 1) PR2_i[i] <- (log(obs)-log(Report$R2_i[i])+Report$SigmaM[1]^2/2)/Report$SigmaM[1]
      # if(ObsModel %in% c(7,11)) PR2_i[i] <- (log(obs)-log(Report$R2_i[i]))/log(Report$R2_i[i])
      # PR2_i[i] <- (log(obs)-log(Report$R2_i[i])+Report$SigmaM[1]^2/2)/Report$SigmaM[1]
      PR2_i[i] <- (log(obs)-log(Report$R2_i[i]))/log(Report$R2_i[i])

    }
  }
  outdf <- cbind(df, PR1=PR1_i, PR2=PR2_i, positive=ifelse(df$b_i>0,1,0)) %>% mutate(Model = name)
  xlim <- range(outdf$Lon); ylim <- range(outdf$Lat)

  return(outdf)
})
pr_byModel <- do.call(rbind, pr_byModel)

plot_pr <- pr_byModel %>% filter(t_i %in% c(2009,2013,2017)) %>% rename(Year = t_i) %>% rename(CatNum = c_iz)
plot_pr$Category <- sapply(1:nrow(plot_pr), function(x) ifelse(plot_pr$CatNum[x] == 0, "Under 250 mm", "Over 250 mm")) 
plot_pr <- plot_pr %>% filter(is.na(PR2)==FALSE)

p <- ggplot(plot_pr) +
    geom_point(data = Network_sz_LL, aes(x = Lon, y = Lat), color = "gray", alpha = 0.6) +
    geom_point(aes(x = Lon_i, y = Lat_i, col = PR2>0, size = abs(PR2)), alpha = 0.7) +
    facet_grid(Year~Category) +
    scale_color_brewer(palette = "Set1") +
    scale_size("Pearson Residual", range = c(0,max(plot_pr$PR2, na.rm = TRUE))) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("Second component") + 
    theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Residual_compare.png"), p, height = 13, width = 12)



