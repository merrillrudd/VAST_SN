rm(list=ls())

####################
## load packages
####################

devtools::install_github("merrillrudd/VASTPlotUtils")
library(tidyverse)
library(VASTPlotUtils)

###################
## Directories
###################

main_dir <- "C:\\merrill\\VAST_SN\\Oregon_coho"
# main_dir <- "~/Projects/Spatiotemporal/VAST_SN/Oregon_coho"
data_dir <- file.path(main_dir, "data")

sil_dir <- file.path(main_dir, "Siletz")
dir.create(sil_dir, showWarnings=FALSE)

fig_dir <- file.path(sil_dir, "figures")
dir.create(fig_dir, showWarnings=FALSE)

####################
## Read in data
####################

network_all <- readRDS(file.path(data_dir, "full_network.rds"))
obs_all <- readRDS(file.path(data_dir, "observations.rds"))
hab_all <- readRDS(file.path(data_dir, "habitat.rds"))

## subset Siletz
network <- readRDS(file.path(data_dir, "siletz_network.rds")) 
obs <- obs_all %>% dplyr::filter(Population == "Siletz")# %>% filter(dist_i > 0)
hab <- hab_all %>% dplyr::filter(Population == "Siletz")

saveRDS(obs, file.path(data_dir, "siletz_observations.rds"))

####################
## Network
####################
net_nodes <- network$child_s
Network_sz_LL <- network
Network_sz <- network %>% dplyr::select("parent_s", "child_s", "dist_s")
# net <- plot_network(Network_sz_LL = Network_sz_LL, arrows=TRUE, root=TRUE)
# ggsave(file.path(fig_dir, "Network.png"), net)

####################
## Observations
####################
obs_nodes <- obs$child_i
all(obs_nodes %in% net_nodes)
cat_names <- unique(obs$Survey)

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

Data_count_spawn <- Data_count %>% dplyr::filter(Category == "Spawners")
Data_count_juv <- Data_count %>% dplyr::filter(Category == "Juveniles")

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

Data_dens_spawn <- Data_dens %>% dplyr::filter(Category == "Spawners")
Data_dens_juv <- Data_dens %>% dplyr::filter(Category == "Juveniles")

# plot_network(Network_sz_LL = Network_sz_LL, Data = Data_count, arrows=TRUE, FilePath=fig_dir, FileName = "Survey_Locs")
# # plot_network(Network_sz_LL = Network_sz_LL, Data = Data_count, arrows=TRUE, byYear=TRUE, FilePath=fig_dir, FileName = "Survey_Locs_byYear")
# # plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens, arrows=TRUE, byYear=TRUE, byValue=TRUE, FilePath=fig_dir, FileName = "Density_byYear")
# plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens %>% filter(Category=="Spawners"), value_label = "Spawner density", obs_color=RColorBrewer::brewer.pal(3,"Set1")[2], arrows=TRUE, byYear=TRUE, byValue=TRUE, FilePath=fig_dir, FileName = "Spawner_Density_byYear")
# plot_network(Network_sz_LL = Network_sz_LL, Data = Data_dens %>% filter(Category=="Juveniles"), value_label = "Juvenile density", obs_color=RColorBrewer::brewer.pal(3,"Set1")[1], arrows=TRUE, byYear=TRUE, byValue=TRUE, FilePath=fig_dir, FileName = "Juvenile_Density_byYear")

####################
## Habitat
####################
habvar <- unique(hab$variable)
n_p <- length(habvar)
var_names <- c("Land cover", "Coho distribution", "Gradient", "Secondary channel area", "Large wood volume", "Large wood volume\nper 100m", "Primary channel area", "Pools per 100m", "Percent slackwater pools", "Weighted gravel in riffles")

## interpolation
hablist <- lapply(1:n_p, function(p){
	sub <- unique(hab %>% dplyr::filter(variable == habvar[p])) #%>% dplyr::select(-Year)) #%>% dplyr::filter(Population=="Siletz")

	## if habitat variable is not land cover or coho distribution
	if(habvar[p] %in% c("land_cover", "Coho_distr") == FALSE){
		# sub2 <- unique(sub)
		# sub3 <- sub2 %>% dplyr::filter(Year == 0)
		# p <- ggplot(sub3) + geom_point(aes(x = Lon, y = Lat, col = value))

		# sub4 <- sub2 %>% dplyr::filter(Year > 0)
		# sub4$value <- as.numeric(sub4$value)
		# p <- ggplot(sub4) + geom_point(aes(x = Lon, y = Lat, col = value)) + facet_wrap(~Year)
		# p <- ggplot(sub4, aes(x = Year, y = value)) + geom_point() + geom_smooth(method = "lm")

		## samples from specific years but not many samples per year
		sub <- unique(sub %>% dplyr::filter(Year > 0)) %>% dplyr::select(-Year)
		sub$value <- as.numeric(sub$value)

		if(nrow(sub)<nrow(Network_sz)){
			## smooth across space

			interp_lat <- sub$Lat
			interp_lon <- sub$Lon
			interp_z <- sub$value				

			find_lat <- Network_sz_LL$Lat[which(Network_sz_LL$child_s %in% sub$child_i == FALSE)]
			find_lon <- Network_sz_LL$Lon[which(Network_sz_LL$child_s %in% sub$child_i == FALSE)]
			find_child <- Network_sz_LL$child_s[which(Network_sz_LL$child_s %in% sub$child_i == FALSE)]			

			# compute <- akima::interp(x = interp_lon, y = interp_lat, z = interp_z, xo = find_lon, yo=find_lat, linear=FALSE, extrap=TRUE, duplicate = "mean")
			compute <- akima::interpp(x = interp_lat, y = interp_lon, z = interp_z, xo=find_lat, yo=find_lon, duplicate = "mean", extrap=TRUE)			

			interp_df <- data.frame('Population'=unique(sub$Population), 'child_s'=find_child, 'Lon'=find_lon, 'Lat'=find_lat, 'HabitatImpact'=unique(sub$HabitatImpact), 'variable'=unique(sub$variable), 'value'=as.numeric(compute$z))				

			sub$Type = "Observed"
			interp_df$Type <- "Interpolated"
			hab_info <- rbind.data.frame(interp_df, sub)
			obs_new <- sapply(1:nrow(hab_info), function(x) ifelse(is.na(hab_info$value[x]),mean(hab_info$value,na.rm=TRUE),hab_info$value[x]))
			hab_info$value <- obs_new			

			p3 <- ggplot(hab_info)+
			geom_point(aes(x=Lon,y=Lat,color=value)) +
			geom_point(data=sub, aes(x=Lon, y=Lat, fill=value), pch=22, cex=3) +
			guides(fill = FALSE, color = guide_legend(title = var_names[p])) +
			ggtitle(paste0(var_names[p], " (", habvar[p], ")", " interpolated")) +
			xlab("Longitude") + ylab("Longitude") +
			scale_color_viridis_c() +
			scale_fill_viridis_c() +
			mytheme()	
			ggsave(file.path(fig_dir, paste0(habvar[p], "_interpolated.png")),p3)		

			hab_new <- lapply(1:nrow(Network_sz_LL), function(x){
			# for(x in 1:nrow(Network_sz_LL)){
				child <- Network_sz_LL$child_s[x]
				find_hab <- hab_info %>% dplyr::filter(child_s==child)
				if(nrow(find_hab)==1) return(find_hab)
				if(nrow(find_hab)>1){
					if(any(find_hab$Type == "Observed")){
						find_hab_sub <- find_hab %>% dplyr::filter(Type == "Observed")
						if(nrow(find_hab_sub) == 1) return(find_hab_sub)
						if(nrow(find_hab_sub) > 1){
							find_hab_sub$value <- mean(find_hab_sub$value)
							return(find_hab_sub[1,])
						}
					} else{
						find_hab$value <- mean(find_hab$value)
						return(find_hab[1,])
					}
				}
			})
			hab_new <- do.call(rbind, hab_new)

			sub <- hab_new			
			sub$value <- as.numeric(sub$value)

		}
	## if habitat variable is land cover or coho distribution
	} else {
		# sub2 <- unique(sub)
		# sub3 <- sub2 %>% dplyr::filter(Year == 0)
		# p <- ggplot(sub3) + geom_point(aes(x = Lon, y = Lat, col = value))

		# sub4 <- sub2 %>% dplyr::filter(Year > 0)
		# p <- ggplot(sub4) + geom_point(aes(x = Lon, y = Lat, col = value)) + facet_wrap(~Year)

		## remove year --- year only available for a few data points, others have no year assigned
		sub <- unique(sub %>% dplyr::select(-Year)) %>% mutate(Type = "Observed")

		missing_child <- net_nodes[which(net_nodes %in% sub$child_s == FALSE)]
		if(length(missing_child)>0){
				fill <- lapply(1:length(missing_child), function(x){
					find_next <- Network_sz_LL %>% dplyr::filter(child_s == missing_child[x])
					find_prev <- Network_sz_LL %>% dplyr::filter(parent_s == missing_child[x])		

					if(nrow(find_next)>0){
						hab_next <- sub %>% dplyr::filter(child_s == find_next$parent_s)
					} else { hab_next <- NULL }
					if(nrow(find_prev)>0){
						hab_prev <- sub %>% dplyr::filter(child_s == find_prev$child_s)
					} else{ hab_prev <- NULL}		

					hab_fill <- NULL
					if(all(is.null(hab_next))==FALSE){
						if(nrow(hab_next)>0){
								hab_fill <- hab_next
								hab_fill$child_s = missing_child[x]
								hab_fill$Lat = Network_sz_LL[which(Network_sz_LL$child_s == missing_child[x]),"Lat"]
								hab_fill$Lon = Network_sz_LL[which(Network_sz_LL$child_s == missing_child[x]),"Lon"]
						}
					}
					if(all(is.null(hab_prev))==FALSE){
							if(nrow(hab_prev)>0){
								hab_fill <- hab_prev
								hab_fill$child_s = missing_child[x]
								hab_fill$Lat = Network_sz_LL[which(Network_sz_LL$child_s == missing_child[x]),"Lat"]
								hab_fill$Lon = Network_sz_LL[which(Network_sz_LL$child_s == missing_child[x]),"Lon"]
							}
					} 
					hab_fill <- hab_fill %>% mutate(Type = "Interpolated")
					return(hab_fill)
				})	
				fill <- do.call(rbind, fill)

			p3 <- ggplot(fill)+
			geom_point(aes(x=Lon,y=Lat,color=value), pch = 20) +
			geom_point(data=sub, aes(x=Lon, y=Lat, fill=value), pch=21, cex=3) +
			guides(color = FALSE, fill = guide_legend(title = var_names[p])) +
			# ggtitle(paste0(var_names[p], " (", habvar[p], ")", " interpolated")) +
			xlab("Longitude") + ylab("Longitude") +
			scale_color_brewer(palette = "Spectral") +
			scale_fill_brewer(palette = "Spectral") +
			# scale_color_viridis_d() +
			# scale_fill_viridis_d() +
			theme_bw(base_size = 14)	
			ggsave(file.path(fig_dir, paste0(habvar[p], "_interpolated.png")),p3, height = 7, width = 10)		
				
				sub <- rbind(sub, fill)
		}
	}
	return(sub)
})
hab_df <- do.call(rbind, hablist)



# hab_spread <- hab_df %>% dplyr::select(-c(Lon, Lat, HabitatImpact)) %>% tidyr::spread(key = variable, value = value)
# hab_spread$GRADIENT <- as.numeric(hab_spread$GRADIENT)
# hab_spread$LWDVOL1 <- as.numeric(hab_spread$LWDVOL1)
# hab_spread$PCTSWPOOL <- as.numeric(hab_spread$PCTSWPOOL)
# hab_spread$POOLS100 <- as.numeric(hab_spread$POOLS100)
# hab_spread$PRICHNAREA <- as.numeric(hab_spread$PRICHNAREA)
# hab_spread$SECCHNAREA <- as.numeric(hab_spread$SECCHNAREA)
# hab_spread$VOLUMELWD <- as.numeric(hab_spread$VOLUMELWD)
# hab_spread$WGTED_SLOPE_GRAVEL <- as.numeric(hab_spread$WGTED_SLOPE_GRAVEL)

# require(GGally)
# ppairs1 <- ggpairs(data=hab_spread, columns=3:ncol(hab_spread)) +
# 		mytheme()

# hab_spread2 <- hab_spread %>% dplyr::select(-c(PCTSWPOOL, LWDVOL1))
# ppairs <- ggpairs(data=hab_spread2, columns=3:ncol(hab_spread2)) +
# 		mytheme()


saveRDS(hab_df, file.path(data_dir, "siletz_habitat_interp.rds"))
