
# devtools::install_local( "C:/Users/James.Thorson/Desktop/Git/FishStatsUtils", force=TRUE, dep=FALSE )
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", dependencies = T)
# devtools::install_local("C:/Users/James.Thorson/Desktop/Git/DHARMa/DHARMa", dependencies = TRUE, build_vignettes = TRUE)

# Set local working directory (change for your machine)
# setwd("C:/Users/James.Thorson/Desktop/Work files/AFSC/2020-04 -- Explore DHARMa/Test")
setwd("C://merrill/VAST_SN/Oregon_coho")


# Load packages
library(VAST)
devtools::load_all("C://merrill/DHARMa/DHARMa")
devtools::load_all("C://merrill/TMB_contrib_R/TMBhelper")
devtools::load_all("C://merrill//FishStatsUtils")

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x=100, Region=example$Region, purpose="index2",
  strata.limits=example$strata.limits, bias.correct=FALSE )

# Run model
fit = fit_model( "settings"=settings, "Lat_i"=example$sampling_data[,'Lat'],
  "Lon_i"=example$sampling_data[,'Lon'], "t_i"=example$sampling_data[,'Year'],
  "c_i"=rep(0,nrow(example$sampling_data)), "b_i"=example$sampling_data[,'Catch_KG']/1e9,
  "a_i"=example$sampling_data[,'AreaSwept_km2'], "v_i"=example$sampling_data[,'Vessel'],
  "getsd"=FALSE, "newtonsteps"=0 )

# Plot results
## requires adjusting FishStatsUtils/simulate_data line 49:
  # dlls <- getLoadedDLLs()
  # isTMBdll <- function(dll) !is(try(getNativeSymbolInfo("MakeADFunObject",dll), TRUE), "try-error")
  # TMBdll <- sapply(dlls, isTMBdll)
  # if( sum(TMBdll)<1 ){ change from != 1
  #   stop("VAST is not linked as a DLL, so `simulate_data` will not work.
  #   Please re-run model (potentially from informative starting values to save time) to use `simulate_data`")
  # }
plot( fit, plot_set=c(), type=4 )

##############
# Demonstrate new feature
##############

dharmaRes = summary( fit, what="residuals")

# Various potential plots
plot(dharmaRes, quantreg = TRUE)
plot(dharmaRes, quantreg = FALSE)
plotQQunif(dharmaRes)
hist(dharmaRes )

# rank transformation, using a simulationOutput
plotResiduals(dharmaRes, rank = TRUE, quantreg = FALSE)
plotResiduals(dharmaRes, quantreg = TRUE)

# residual vs predictors, using explicit values for pred, residual
plotResiduals(pred = log(fit$Report$D_gcy),
              residuals = dharmaRes$scaledResiduals, quantreg = TRUE)

# if pred is a factor, or asFactor = T, will produce a boxplot
plotResiduals(pred = fit$data_list$c_i, residuals = dharmaRes$scaledResiduals,
              quantreg = FALSE, asFactor = TRUE)

