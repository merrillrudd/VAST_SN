rm(list=ls())

################
## Directories
################

nz_dir <- "~/Projects/Spatiotemporal/VAST_SN/NZ_eel"
sub_dir <- file.path(nz_dir, "Waikato_lengths")
dir.create(sub_dir, showWarnings = FALSE)

data_dir <- file.path(nz_dir, "data")
data_dir2 <- file.path(sub_dir, "data")
dir.create(data_dir2, showWarnings = FALSE)

fig_dir <- file.path(sub_dir, "figures")
dir.create(fig_dir, showWarnings=FALSE)

#################
## Packages
#################

library(tidyverse)
library(proj4)
library(akima)

# ################
## Load data
################
# obsfull <- readRDS(file.path(data_dir, "NZ_observations.rds"))
netfull <- readRDS(file.path(data_dir, "NZ_network.rds"))
habfull <- readRDS(file.path(data_dir, "NZ_habitat.rds"))
obslen <- readRDS(file.path(data_dir, "Waikato_observations_length.rds"))

## select important info from network
netfull2 <- netfull %>% select(CatName, nzsegment, parent_s, child_s, dist_s, long, lat)

## bring important info from network to observations, removing root nodes
obslen2 <- left_join(obslen, netfull2) %>% filter(parent_s != 0)

#################################################
## subset catchments included in length dataset
#################################################

## find network with same catchment names as observations
netsub <- netfull %>% filter(CatName %in% obslen2$CatName) #%>% filter(parent_s != 0)

## full new zealand map
nzmap <- ggplot() +
	geom_point(data = netfull, aes(x = long, y = lat), pch = ".") +
	xlab("Longitude") + ylab("Latitude") +
	theme_bw(base_size = 14)

catmap <- nzmap + 
	geom_point(data = netsub, aes(x = long, y = lat, color = CatName)) +
	geom_point(data = obslen2, aes(x = long, y = lat), pch = 21, fill = "white", alpha = 0.8) +
	guides(color = FALSE)
ggsave(file.path(fig_dir, "Length_network_v1.png"), catmap, height = 6, width = 7)


### find nodes downstream of observations
#################################
## network with all observations
obs_child <- unique(obslen2$child_s)
net_obs <- netsub %>% filter(child_s %in% obs_child)
nextdown <- netsub %>% filter(child_s %in% net_obs$parent_s)
save <- rbind.data.frame(net_obs,nextdown)
for(i in 1:200){
  nextdown <- netsub %>% filter(child_s %in% nextdown$parent_s)
  save <- unique(rbind.data.frame(save, nextdown))
  print(nrow(save))
}
netsub2 <- save

roots <- netsub2 %>% filter(parent_s == 0)
nextup <- netsub %>% filter(parent_s %in% roots$child_s)
save <- rbind.data.frame(net_obs,nextup)
for(i in 1:200){
	nextup <- netsub %>% filter(parent_s %in% nextup$child_s)
	save <- unique(rbind.data.frame(save, nextup))
	print(nrow(save))
}
netsub3 <- save

netsub_toUse <- unique(rbind.data.frame(netsub2, roots))
netsub_long <- unique(rbind.data.frame(netsub2, netsub3, roots))



all(obslen2$nzsegment %in% netsub_toUse$nzsegment)
all(obslen2$nzsegment %in% netsub_long$nzsegment)

length(which(netsub_toUse$parent_s == 0))
length(which(netsub_long$parent_s == 0))

catmap2 <- nzmap + 
	geom_point(data = netsub_toUse, aes(x = long, y = lat, color = CatName)) +
	geom_point(data = obslen2, aes(x = long, y = lat), pch = 21, fill = "white", alpha = 0.8) +
	guides(color = FALSE) 
	# facet_wrap(.~CatName) +
	# geom_segment(data=l2, aes(x = long2,y = lat2, xend = long, yend = lat), col="gray") +
	# theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Length_network_v2.png"), catmap2, height = 6, width = 7)


catmap2_v2 <- nzmap + 
	geom_point(data = netsub_long, aes(x = long, y = lat, color = CatName)) +
	geom_point(data = obslen2, aes(x = long, y = lat), pch = 21, fill = "white", alpha = 0.8) +
	guides(color = FALSE) 
	# facet_wrap(.~CatName) +
	# geom_segment(data=l2, aes(x = long2,y = lat2, xend = long, yend = lat), col="gray") +
	# theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Length_network_v2_long.png"), catmap2_v2, height = 6, width = 7)


catmap3 <- ggplot() + 
	geom_point(data = netsub_toUse, aes(x = long, y = lat, color = CatName)) +
	geom_point(data = obslen2, aes(x = long, y = lat), pch = 21, fill = "white", alpha = 0.8) +
	geom_point(data = netsub_toUse %>% filter(parent_s == 0))
	guides(color = FALSE) +
	xlab("Longitude") + ylab("Latitude") + 
	theme_bw(base_size = 14)
	# facet_wrap(.~CatName) +
	# geom_segment(data=l2, aes(x = long2,y = lat2, xend = long, yend = lat), col="gray") +
	# theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Length_network_v3.png"), catmap3, height = 6, width = 7)

catmap3_v2 <- ggplot() + 
	geom_point(data = netsub_long, aes(x = long, y = lat, color = CatName)) +
	geom_point(data = obslen2, aes(x = long, y = lat), pch = 21, fill = "white", alpha = 0.8) +
	guides(color = FALSE) +
	xlab("Longitude") + ylab("Latitude") + 
	theme_bw(base_size = 14)
	# facet_wrap(.~CatName) +
	# geom_segment(data=l2, aes(x = long2,y = lat2, xend = long, yend = lat), col="gray") +
	# theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Length_network_v3_long.png"), catmap3_v2, height = 6, width = 7)



## rename nodes
nodes <- unique(c(netsub_toUse$child_s, netsub_toUse$parent_s))
inodes <- seq_along(nodes)

net_parents <- sapply(1:nrow(netsub_toUse), function(x){
  if(netsub_toUse$parent_s[x] != 0) new_node <- inodes[which(nodes == netsub_toUse$parent_s[x])]
  if(netsub_toUse$parent_s[x] == 0) new_node <- 0
  return(new_node)
})
net_children <- sapply(1:nrow(netsub_toUse), function(x) inodes[which(nodes == netsub_toUse$child_s[x])])

netsub_toUse$parent_s <- net_parents
netsub_toUse$child_s <- net_children

obs_parents <- sapply(1:nrow(obslen2), function(x){
  if(obslen2$parent_s[x] != 0) new_node <- inodes[which(nodes == obslen2$parent_s[x])]
  if(obslen2$parent_s[x] == 0) new_node <- 0
  return(new_node)  
})
obs_children <- sapply(1:nrow(obslen2), function(x) inodes[which(nodes == obslen2$child_s[x])])

obslen3 <- obslen2
obslen3$parent_i <- obs_parents
obslen3$child_i <- obs_children


saveRDS(netsub_toUse, file.path(data_dir, "Greater_waikato_network.rds"))
saveRDS(obslen3, file.path(data_dir, "Greater_waikato_observations.rds"))


network_sub <- readRDS(file.path(data_dir, "Greater_waikato_network.rds"))
obs_sub <- readRDS(file.path(data_dir, "Greater_waikato_observations.rds"))

catchmap2 <- ggplot() +
		geom_point(data=netfull, aes(x = long, y = lat), col = "black", cex=0.2) +
		geom_point(data = network_sub, aes(x = long, y = lat), col = "gray") +
		# geom_point(data=obsfull %>% filter(data_type=="encounter"), aes(x = long, y = lat, fill=data_type), pch=22, alpha=0.6) +
		xlab("Longitude") + ylab("Latitude") +
		# scale_fill_brewer(palette = "Set1") +
		theme_bw(base_size = 14)
ggsave(file.path(fig_dir, "Waikato_on_NZ.png"), catchmap2, height = 8, width = 9)


netsub <- readRDS(file.path(data_dir, "Greater_waikato_network.rds"))
habsub <- habfull %>% filter(nzsegment %in% netsub$nzsegment) %>% filter(covariate != "width")
length(which(is.na(habsub$value)))

covar <- unique(habsub$covariate)
covar_toUse <- c('MeanFlowCumecs','Dist2Coast_FromMid','loc_elev','loc_slope','loc_rnvar',"local_twarm",'DamAffected')
all(covar_toUse %in% covar)


habsub2 <- lapply(1:length(covar_toUse), function(x){
# for(x in 1:length(covar_toUse)){
	sub <- habsub %>% filter(covariate == covar_toUse[x])

	# ggplot(sub) + geom_point(aes(x = easting, y = northing, color = value))
	# any(is.na(sub$value))
	if(any(is.na(sub$value))){

		if(covar_toUse[x]!="DamAffected"){
			if(length(which(is.na(sub$value)) == 1)){
				na <- sub %>% filter(is.na(value))
				up <- sub %>% filter(parent_s == na$child_s)
				down <- sub %>% filter(child_s == na$parent_s)
				val <- mean(c(up$value, down$value), na.rm=TRUE)
				sub$value[which(is.na(sub$value))] <- val
			} else {
				interp_east <- sub$easting[which(is.na(sub$value)==FALSE)]
				interp_north <- sub$northing[which(is.na(sub$value)==FALSE)]
				interp_z <- sub$value[which(is.na(sub$value)==FALSE)]		

				find_df <- data.frame('east' = sub$easting[which(is.na(sub$value))], 'north' = sub$northing[which(is.na(sub$value))])		

				east <- sub$easting[order(sub$easting)]
				north <- sub$northing[order(sub$northing)]
				# mat2 <- zoo::na.approx(object = mat)
				compute <- akima::interp(x = interp_east, y = interp_north, z = interp_z, xo=east, yo=north, extrap=TRUE)
				mat2 <- compute$z		

				vals <- sapply(1:nrow(find_df), function(y){
					mat2[which(compute$x == find_df$east[y]), which(compute$y == find_df$north[y])]
				})		

				inp_vals <- sub$value
				inp_vals[which(is.na(inp_vals))] <- vals		

				sub$value <- inp_vals		

				if(length(which(is.na(sub$value)))==1){
					xx <- sub[(which(is.na(sub$value))-5):(which(is.na(sub$value))+5),]
					xx2 <- xx[order(xx$easting),]
					val_inp <- median(xx$value, na.rm=TRUE)
					sub$value[which(is.na(sub$value))] <- val_inp
				}
				if(length(which(is.na(sub$value)))>1){
					val_inp <- median(sub$value, na.rm=TRUE)
					sub$value[which(is.na(sub$value))] <- val_inp
				}
			}
		}
		if(covar_toUse[x]=="DamAffected"){
			inp <- sub$value
			inp[which(is.na(inp))] <- 2
			ggplot(sub) + geom_point(aes(x = easting, y = northing, color = factor(inp)))

			ina <- which(is.na(sub$value))
			new <- rep(NA, length(ina))
			for(i in 1:length(ina)){
				sub2 <- sub[ina[i],]
				up <- sub %>% filter(parent_s == sub2$child_s)
				down <- sub %>% filter(child_s == sub2$parent_s)
				check <- rbind.data.frame(up, down)
				if(any(is.na(check$value))) check <- check %>% filter(is.na(value)==FALSE)
				if(all(check$value == 0)) new[i] <- 0
				if(all(check$value == 1)) new[i] <- 1
			}

			sub$value[ina] <- new
		}
	}
	return(sub)
})
check <- sapply(1:length(habsub2), function(x) any(is.na(habsub2[[x]]$value)))
all(check == FALSE)
habsub2 <- do.call(rbind, habsub2)

find0 <- sapply(1:length(covar_toUse), function(x){
	sub <- habsub2 %>% filter(covariate == covar_toUse[x])
	any(sub$value==0)
})
names(find0) <- covar_toUse


hab_parents <- sapply(1:nrow(habsub2), function(x){
  if(habsub2$parent_s[x] != 0) new_node <- inodes[which(nodes == habsub2$parent_s[x])]
  if(habsub2$parent_s[x] == 0) new_node <- 0
  return(new_node)
})
hab_children <- sapply(1:nrow(habsub2), function(x) inodes[which(nodes == habsub2$child_s[x])])

habsub2$parent_s <- hab_parents
habsub2$child_s <- hab_children
saveRDS(habsub2, file.path(data_dir, "Greater_waikato_habitat.rds"))
