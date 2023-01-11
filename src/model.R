#library(ellipsenm)
library(kuenm)
#library(ntbox)

#-----------------------Model candidate models----------------------------------

occ_joint <- "data/workflow_maxent/an_albimanus/an_albimanus_joint.csv"
occ_tra <- "data/workflow_maxent/an_albimanus/an_albimanus_train.csv"
M_var_dir <- "data/workflow_maxent/an_albimanus/Model_calibration/M_variables"
batch_cal <- "data/workflow_maxent/an_albimanus/Candidate_models"
out_dir <- "data/workflow_maxent/an_albimanus/Candidate_Models"
reg_mult <-  c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)
f_clas <- 'all'
args <- NULL
maxent_path <- getwd()
wait <- FALSE
run <- TRUE

kuenm_cal(
  occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir,
  batch = batch_cal, out.dir = out_dir, reg.mult = reg_mult,
  f.clas = f_clas, args = args, maxent.path = maxent_path,
  wait = wait, run = run
)

#-----------------------Model evaluating models---------------------------------

occ_test <- "data/workflow_maxent/an_albimanus/an_albimanus_test.csv"
out_eval <- "data/workflow_maxent/an_albimanus/Calibration_results"
threshold <- 5
rand_percent <- 50
iterations <- 100
kept <- TRUE
selection <- "OR_AICc"

kuenm_ceval(
  path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra,
  occ.test = occ_test, batch = batch_cal, out.eval = out_eval,
  threshold = threshold, rand.percent = rand_percent,
  iterations = iterations, kept = kept, selection = selection
)


#----------------------------------Final Models ---------------------

dir.create("data/workflow_maxent/an_albimanus/Final_models")

batch_fin <- "data/workflow_maxent/an_albimanus/Final_models"
mod_dir <- "data/workflow_maxent/an_albimanus/Final_models"
rep_n <- 5
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"
project <- TRUE
G_var_dir <- "data/workflow_maxent/an_albimanus/G_Variables"
ext_type <- "all"
write_mess <- FALSE
write_clamp <- FALSE
wait1 <- FALSE
run1 <- TRUE
args <- NULL

kuenm_mod(
  occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval,
  batch = batch_fin, rep.n = rep_n, rep.type = rep_type,
  jackknife = jackknife, out.dir = mod_dir, out.format = out_format,
  project = project, G.var.dir = G_var_dir, ext.type = ext_type,
  write.mess = write_mess, write.clamp = write_clamp,
  maxent.path = maxent_path, args = args, wait = wait1, run = run1
)


occ_ind <-  "data/workflow_maxent/an_albimanus/an_albimanus_test.csv"
replicates <- TRUE
out_feval <- "data/workflow_maxent/an_albimanus/Final_Models_evaluation"
# Most of the variables used here as arguments were already created for previous functions


fin_eval <- kuenm_feval(path = mod_dir, occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
                        out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
                        iterations = iterations, parallel.proc = TRUE)

sets_var <- "Set1" # a vector of various sets can be used
out_mop <- "data/workflow_maxent/an_albimanus/MOP_results"
percent <- 10
paral <- FALSE 

kuenm_mmop(G.var.dir = G_var_dir, M.var.dir = M_var_dir, 
           is.swd = FALSE, sets.var = sets_var, 
           out.mop = out_mop, percent = percent, parallel = FALSE)

percent = 10
comp.each = 2000
parallel = FALSE 
n.cores = NULL

# mmop --------------------------------------------------------------------

kuenm_mmop <- function(G.var.dir, M.var.dir, is.swd, sets.var, out.mop, percent = 10,
                       comp.each = 2000, parallel = FALSE, n.cores = NULL) {
  if (missing(G.var.dir)) {
    stop("Argument G.var.dir is not defined.")
  }
  if (!dir.exists(G.var.dir)) {
    stop(paste(G.var.dir, "does not exist in the working directory, check folder name",
               "\nor its existence."))
  }
  if (length(list.dirs(G.var.dir, recursive = FALSE)) == 0) {
    stop(paste(G.var.dir, "does not contain any subdirectory with sets of projection variables;",
               "\neach subdirectory inside", G.var.dir, "must containg at least one subdirectory",
               "\nwith the projection variables"))
  }
  if (missing(M.var.dir)) {
    stop("Argument M.var.dir is not defined.")
  }
  if (!dir.exists(M.var.dir)) {
    stop(paste(M.var.dir, "does not exist in the working directory, check folder name",
               "\nor its existence."))
  }
  if (length(dir(M.var.dir)) == 0) {
    stop(paste(M.var.dir, "is empty. Check function's help for details."))
  }
  if (missing(is.swd)) {
    stop("Argument is.swd is not defined.")
  }
  
  #MOP directory
  dir.create(out_mop)
  
  #Calculating MOP for each comparison set by set
  for (h in 1:length(sets.var)) {
    if (is.swd == TRUE) {
      dirsm <- dir(M.var.dir, pattern = paste0("^", sets.var[h], ".csv$"),
                   full.names = TRUE)
      m_vars <- read.csv(dirsm)[, -(1:3)] #stacking the variables
    } else {
      dirsm <- dir(M_var_dir, pattern = paste0("^", 'Set_1', "$"),
                   full.names = TRUE)
      m_var <- list.files(dirsm, pattern = "asc", full.names = TRUE) #listing vars in M
      m_vars <- raster::stack(m_var) #stacking the variables
    }
    
    dirsg <- dir(G_var_dir, pattern = paste0("^", 'Set_1', "$"),
                 full.names = TRUE)
    dirsg_in <- dir(dirsg, full.names = TRUE)
    namesg <- dir(dirsg)
    
    dir.create(paste(out_mop, 'Set_1', sep = "/"))
    
    dirs_mop <- paste(paste(out_mop, 'Set_1', "MOP", sep = "/"),
                      paste(percent, "%", sep = ""), namesg, sep = "_")
    
    if(.Platform$OS.type != "unix") {
      pb <- winProgressBar(title = "Progress bar", min = 0, max = length(dirsg_in),
                           width = 300) #progress bar
    }
    
    for(i in 1:length(dirsg_in)) {
      Sys.sleep(0.1)
      if(.Platform$OS.type != "unix") {
        setWinProgressBar(pb, i, title = paste(round(i / length(dirsg_in) * 100, 2),
                                               paste("% of the process for", 'Set_1', "has finished")))
      }
      
      g_var <- list.files(dirsg_in[i], pattern = "asc",
                          full.names = TRUE) #listing var of different Gs
      g_vars <- raster::stack(g_var)
      
      #MOP calculation
      mop_res <- kuenm_mop(M.variables = m_vars, G.stack = g_vars, percent = percent,
                           comp.each = comp.each, parallel = parallel, n.cores = n.cores)
      
      #Writing results
      raster::writeRaster(mop_res, filename = paste(dirs_mop[i],".tif", sep = ""),
                          format = "GTiff")
      
      if(.Platform$OS.type == "unix") {
        cat("\n\t", paste(i, "of", length(dirsg_in), "MOPs", sep = " "), "\n")
      }
    }
    
