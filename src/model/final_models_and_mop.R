library(kuenm)
species_list <-
  c(
    # "Anopheles_albimanus",
    "Anopheles_albitarsis",
    "Anopheles_apicimacula",
    "Anopheles_aquasalis", "Anopheles_argyritarsis", "Anopheles_braziliensis",
    # "Anopheles_cruzii",
    "Anopheles_eiseni", "Anopheles_evansae", "Anopheles_intermedius",
    "Anopheles_mediopunctatus",
    # "Anopheles_nuneztovari",
    "Anopheles_oswaldoi",
    "Anopheles_peryassui", "Anopheles_pseudopunctipennis",
    "Anopheles_punctimacula", "Anopheles_rangeli", "Anopheles_strodei"
    # "Anopheles_triannulatos")
  )

maxent_path <- getwd()

for (i in seq_along(species_list)) {
  sp_name <- species_list[[i]]

  path <- paste0("data/workflow_maxent/", sp_name)

  # Params ------------------------------------------------------------------
  occ_joint <-
    paste0(path, "/", sp_name, "_joint.csv")

  occ_tra <-
    paste0(path, "/", sp_name, "_train.csv")

  M_var_dir <-
    paste0(path, "/Model_calibration/M_variables")

  batch_cal <-
    paste0(path, "/Candidate_models")

  out_dir <-
    paste0(path, "/Candidate_Models")

  reg_mult <-
    c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)

  f_clas <- "all"

  args <- NULL

  wait <- FALSE

  run <- TRUE

  occ_test <-
    paste0(path, "/", sp_name, "_test.csv")

  out_eval <-
    paste0(path, "/Calibration_results")

  threshold <- 5

  rand_percent <- 50

  iterations <- 100

  kept <- TRUE

  selection <- "OR_AICc"

  # dir.create(paste0("data/workflow_maxent/", sp_name, "/Final_models"))

  batch_fin <-
    paste0(path, "/Final_models")

  mod_dir <-
    paste0(path, "/Final_models")

  rep_n <- 5

  rep_type <- "Bootstrap"

  jackknife <- TRUE

  out_format <- "logistic"

  project <- TRUE

  G_var_dir <-
    paste0(path, "/G_Variables")

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

  occ_ind <- paste0(path, "/", sp_name, "_test.csv")

  replicates <- TRUE

  out_feval <- paste0(path, "/Final_Models_evaluation")

  # Most of the variables used here as arguments were already created for previous functions

  fin_eval <- kuenm_feval(
    path = mod_dir, occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
    out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
    iterations = iterations
  )
}

# MOP ---------------------------------------------------------------------
for (i in seq_along(species_list)) {
  sp_name <- species_list[[i]]

  path <- paste0("data/workflow_maxent/", sp_name)

  # Params ------------------------------------------------------------------
  occ_joint <-
    paste0(path, "/", sp_name, "_joint.csv")

  occ_tra <-
    paste0(path, "/", sp_name, "_train.csv")

  M_var_dir <-
    paste0(path, "/Model_calibration/M_variables")

  batch_cal <-
    paste0(path, "/Candidate_models")

  out_dir <-
    paste0(path, "/Candidate_Models")

  reg_mult <-
    c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)

  f_clas <- "all"

  args <- NULL

  wait <- FALSE

  run <- TRUE

  occ_test <-
    paste0(path, "/", sp_name, "_test.csv")

  out_eval <-
    paste0(path, "/Calibration_results")

  threshold <- 5

  rand_percent <- 50

  iterations <- 100

  kept <- TRUE

  selection <- "OR_AICc"

  # dir.create(paste0("data/workflow_maxent/", sp_name, "/Final_models"))

  batch_fin <-
    paste0(path, "/Final_models")

  mod_dir <-
    paste0(path, "/Final_models")

  rep_n <- 5

  rep_type <- "Bootstrap"

  jackknife <- TRUE

  out_format <- "logistic"

  project <- TRUE

  G_var_dir <-
    paste0(path, "/G_Variables")

  ext_type <- "all"

  write_mess <- FALSE

  write_clamp <- FALSE

  wait1 <- FALSE

  run1 <- TRUE

  args <- NULL
  sets_var <- "Set_1" # a vector of various sets can be used
  out_mop <- paste0(path, "/MOP_results")
  percent <- 10
  paral <- FALSE

  kuenm_mmop(
    G.var.dir = G_var_dir, M.var.dir = M_var_dir,
    is.swd = FALSE, sets.var = sets_var,
    out.mop = out_mop, percent = percent
  )
}

# MOP Agreement -----------------------------------------------------------
for (i in seq_along(species_list)) {
  sp_name <- species_list[[i]]
  path <- paste0("data/workflow_maxent/", sp_name)
  mop_dir <- paste0(path, "/MOP_results")
  format <- "GTiff"
  curr <- "current"
  time_periods <- 60
  emi_scenarios <- c("126", "585")
  out_dir <- paste0(path, "/MOP_agremment")

  kuenm_mopagree(
    mop.dir = mop_dir, in.format = format, out.format = format,
    current = curr, time.periods = time_periods,
    emi.scenarios = emi_scenarios, out.dir = out_dir
  )
}
