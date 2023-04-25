library(kuenm)
species_name <- "m_molossus"
# Params ------------------------------------------------------------------
occ_joint <-
  paste0("data/workflow_maxent/", species_name, "/", species_name, "_joint.csv")
occ_tra <-
  paste0("data/workflow_maxent/", species_name, "/", species_name, "_train.csv")
M_var_dir <-
  paste0("data/workflow_maxent/", species_name, "/Model_calibration/M_variables")
batch_cal <-
  paste0("data/workflow_maxent/", species_name, "/Candidate_models")
out_dir <-
  paste0("data/workflow_maxent/", species_name, "/Candidate_Models")
reg_mult <-
  c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)
f_clas <- "all"
args <- NULL
maxent_path <- "/home/grati/Documents/model-niche-anopheles"
wait <- FALSE
run <- TRUE
occ_test <-
  paste0("data/workflow_maxent/", species_name, "/", species_name, "_test.csv")
out_eval <-
  paste0("data/workflow_maxent/", species_name, "/Calibration_results")
threshold <- 5
rand_percent <- 50
iterations <- 100
kept <- TRUE
selection <- "OR_AICc"

dir.create(paste0("data/workflow_maxent/", species_name, "/Final_models"))

batch_fin <-
  paste0("data/workflow_maxent/", species_name, "/Final_models")
mod_dir <-
  paste0("data/workflow_maxent/", species_name, "/Final_models")
rep_n <- 5
rep_type <- "Bootstrap"
jackknife <- TRUE
out_format <- "logistic"
project <- TRUE
G_var_dir <-
  paste0("data/workflow_maxent/", species_name, "/G_Variables")
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

occ_ind <- paste0("data/workflow_maxent/", species_name, "/", species_name, "_test.csv")
replicates <- TRUE
out_feval <- paste0("data/workflow_maxent/", species_name, "/Final_Models_evaluation")

# Most of the variables used here as arguments were already created for previous functions


fin_eval <- kuenm_feval(
  path = mod_dir, occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
  out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
  iterations = iterations, parallel.proc = TRUE
)

sets_var <- "Set1" # a vector of various sets can be used
out_mop <- paste0("data/workflow_maxent/", species_name, "/MOP_results")
percent <- 10
paral <- FALSE
# comp.each = 2000
# n.cores = NULL

kuenm_mmop(
  G.var.dir = G_var_dir, M.var.dir = M_var_dir,
  is.swd = FALSE, sets.var = sets_var,
  out.mop = out_mop, percent = percent, parallel = FALSE
)
