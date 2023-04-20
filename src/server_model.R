library(kuenm)

species_name <- "Anopheles_"

#-----------------------Model candidate models----------------------------------

occ_joint <- paste0("data/workflow_maxent/",species_name,"/",species_name,"_joint.csv")
occ_tra <- paste0("data/workflow_maxent/",species_name,"/",species_name,"_train.csv")
M_var_dir <- paste0("data/workflow_maxent/",species_name,"/Model_calibration/M_variables")
batch_cal <-paste0("data/workflow_maxent/",species_name,"/Candidate_models")
out_dir <- paste0("data/workflow_maxent/",species_name,"/Candidate_Models")
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

occ_test <- paste0("data/workflow_maxent/",species_name,"/",species_name,"_test.csv")
out_eval <- paste0("data/workflow_maxent/",species_name,"/Calibration_results")
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

