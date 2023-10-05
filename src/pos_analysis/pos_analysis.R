library(kuenm)
# Params ------------------------------------------------------------------
specie_name <- "Anopheles_darlingi"
fmod_dir <-
  paste0("data/workflow_maxent/", species_name, "/","Final_models")

format <- "asc"
project <- TRUE
stats <- c("med", "range")
rep <- TRUE
scenarios <- c("Current", "future_layer_can_126_60", 
               "future_layer_can_585_60", 
               "future_layer_mc_126_60", 
               "future_layer_mc_585_60")
ext_type <- c("E", "EC", "NE") 
out_dir <- 
  paste0("data/workflow_maxent/", species_name, "/Final_Model_Stats")

kuenm_modstats(sp.name = specie_name, fmod.dir = fmod_dir, 
               format = format, project = project, 
               statistics = stats, replicated = rep, 
               proj.scenarios = scenarios, 
               ext.type = ext_type, out.dir = out_dir)


# project changes ---------------------------------------------------------

occ_joint <-
  paste0("data/workflow_maxent/", species_name, "/", species_name, "_joint.csv")
thres <- 5
curr <- "Current"
emi_scenarios <- c("126_60", "585_60")
c_mods <- c("future_layer_can", "future_layer_mc")
ext_type <- c("E", "EC", "NE")
out_dir1 <-  paste0("data/workflow_maxent/", species_name, "/Projection_Changes")

kuenm_projchanges(occ = occ_joint, fmod.stats = out_dir, threshold = thres, current = curr, 
                  emi.scenarios = emi_scenarios, clim.models = c_mods, ext.type = ext_type, 
                  out.dir = out_dir1)

# modvar ------------------------------------------------------------------

split <- 100
out_dir2 <-  paste0("data/workflow_maxent/", species_name, "/Variation_from_sources")

kuenm_modvar(sp.name = specie_name, fmod.dir = fmod_dir, replicated = rep, format = format,  
             project = project, current = curr, emi.scenarios = emi_scenarios, 
             clim.models = c_mods, ext.type = ext_type, split.length = split, out.dir = out_dir2, 
             is.swd = F)


# hiereachical_partitioned ------------------------------------------------

sp_name <- "sp1"
fmod_dir <- "Final_Models"
rep <- TRUE
format <- "asc"
project <- TRUE
curr <- "current"
emi_scenarios <- c("RCP4.5", "RCP8.5")
c_mods <- c("GCM1", "GCM2")
ext_type <- c("E", "EC", "NE")
iter <- 100
s_size <- 1000
out_dir3 <- "Hierarchical_partitioning"
# argument "factors_col" is not defined here, but if default colors (grey scale) need to be changed, 
# you can use this argument.
