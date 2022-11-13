#library(ellipsenm)
library(kuenm)
#library(ntbox)

#-----------------------Model candidate models----------------------------------

occ_joint <- "data/workflow_maxent/an_albimanus/an_albimanus_joint.csv"
occ_tra <- "data/workflow_maxent/an_albimanus/an_albimanus_train.csv"
M_var_dir <- "data/workflow_maxent/an_albimanus/Model_calibration/PCs_M"
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