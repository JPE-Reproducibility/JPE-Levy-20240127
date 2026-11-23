# Route system-R invocations to the bundled runtime before loading any packages,
# unless POPULIST_SYSTEM_R=1 asks to run in the current R (see README, other platforms).
if (!identical(Sys.getenv("POPULIST_SYSTEM_R"), "1") &&
    !identical(normalizePath(R.home()), normalizePath("offline/runtime", mustWork=FALSE))) {
  if (!dir.exists("offline/runtime")) stop("Run make setup first, or set POPULIST_SYSTEM_R=1 to use your own R 4.6.1 (see README).")
  status <- system2("bash", c("offline/Rscript", "--vanilla", "code/masterFile.R"))
  if (status != 0L) stop("Bundled replication failed; see output/logs.")
} else {
rm(list=ls())
source("renv/activate.R")
if (identical(Sys.getenv("POPULIST_RESET_OUTPUT"), "1")) {
  if (isTRUE(nzchar(Sys.readlink("output"), keepNA=TRUE)))
    stop("Refusing to reset output through a symbolic link.")
  if (unlink("output", recursive=TRUE) != 0L || file.exists("output"))
    stop("Could not remove the existing output tree.")
}
Sys.setenv(OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1", VECLIB_MAXIMUM_THREADS="1", RCPP_PARALLEL_NUM_THREADS="1")
options(error = function() {
  writeLines(c(paste("Failed:", format(Sys.time(), tz="UTC", usetz=TRUE)), capture.output(traceback())),
             file.path(getOption("populist.log.dir"), "FAILURE.txt"))
  quit(save="no", status=1, runLast=FALSE)
})
options(warn = 1) # Record warnings at the step that produces them.
run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "-", Sys.getpid())
options(populist.log.dir = file.path("output", "logs", run_id))
Sys.setenv(POPULIST_LOG_DIR=file.path(getwd(), getOption("populist.log.dir")))
dir.create(getOption("populist.log.dir"), recursive = TRUE, showWarnings = FALSE)
options(populist.console.connection=file(file.path(getOption("populist.log.dir"), "console.log"), open="wt"))
sink(getOption("populist.console.connection"), split=TRUE)
sink(getOption("populist.console.connection"), type="message")
file.copy("code/util/config.R", file.path(getOption("populist.log.dir"), "config.R"), overwrite=TRUE)
options(device=function(...) grDevices::pdf(file=file.path(Sys.getenv("POPULIST_LOG_DIR"), paste0("diagnostics-", Sys.getpid(), ".pdf")), ...))
writeLines(c(paste("Started:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
             paste("R:", R.version.string), paste("Platform:", R.version$platform)),
           file.path(getOption("populist.log.dir"), "run-info.txt"))
source("code/util/preflight.R")
replication_preflight()
source("code/util/commonVars.R")
writeLines(c(readLines(file.path(getOption("populist.log.dir"), "run-info.txt")),
             capture.output(dput(list(processData=processData, runModel=runModel, runBS=runBS,
                 runAnalysis=runAnalysis, runCompile=runCompile, PRECISE_ESTIMATION=PRECISE_ESTIMATION,
                 estimationWorkers=estimationWorkers, bootstrapWorkers=bootstrapWorkers,
                 bootstrapDraws=bootstrapDraws, bootstrapFolder="output/modeldata/bootstrap/BS")))), file.path(getOption("populist.log.dir"), "run-info.txt"))
for (marker in list.files("code", pattern="Config$", full.names=TRUE)) unlink(marker)
if (runAnalysis == 1) for (folder in c("figures", "tables", "values", "other"))
  unlink(list.files(file.path("output", folder), full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)
if (runCompile == 1)
  unlink(list.files("output/documents", full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)

# Refresh generated inputs; supplied raw inputs and bootstrap runs are retained.
if (processData == 1) {
  unlink(list.files("data/intdata", full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)
  unlink(list.files("data/cleandata", full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)
}
if (runModel == 1) {
  unlink(list.files("output/modeldata/results", full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)
  unlink(list.files("output/modeldata/decomposition", full.names=TRUE, all.files=TRUE, no..=TRUE), recursive=TRUE)
}
source("code/util/create_folders.R")
if (processData == 1) local({
  # Keep the helper local because cleaning scripts clear the global workspace.
  run_cleaning_step <- function(path, run = source, ...) {
    cat("Running ", path, "\n", sep="")
    connection <- getOption("populist.console.connection")
    sink()
    sink(connection, split=FALSE)
    on.exit({
      sink()
      sink(connection, split=TRUE)
    }, add=TRUE)
    run(path, ...)
  }

  run_cleaning_step("code/process/analysis_inputs/prepare_metadata.R")

  # Clean data
  run_cleaning_step("code/process/stata.R")
  run_cleaning_step("code/process/prepare_data_master_part_1.do", runStata)
  run_cleaning_step("code/process/ivs/imputeRF.R")
  run_cleaning_step("code/process/ivs/imputeRF_with_manifesto.R")
  
  run_cleaning_step("code/util/commonVars.R")
  run_cleaning_step("code/process/stata.R")
  run_cleaning_step("code/process/prepare_data_master_part_2.do", runStata)
  run_cleaning_step("code/process/analysis_inputs/prepare_us_voters.R")
  
  # Populist data
  run_cleaning_step("code/process/merge/merge_manifesto_populist.R")
  run_cleaning_step("code/analyze/party_positions/remove_stopwords.R")
  run_cleaning_step("code/process/manifesto/measure_manifesto_style.R")
  
  # Demographics Zs
  run_cleaning_step("code/process/demographics/femShareMatch_closestYear.R")
  run_cleaning_step("code/process/ipums/prepare_wage_occ.R")
  run_cleaning_step("code/process/demographics/clean_party_leaders.R")
  # Consume supplied raw scores; optional scoring is separate from replication.
  run_cleaning_step("code/process/demographics/process_supply_data.R")
  
  # Final Zs
  run_cleaning_step("code/process/manifesto/clean_manifesto_parties.R")
  file.create("code/styleGoogleConfig")
  run_cleaning_step("code/process/demographics/process_supply_data.R")
  run_cleaning_step("code/process/manifesto/clean_manifesto_parties.R")
  file.remove("code/styleGoogleConfig")
  
  # Pledges
  run_cleaning_step("code/process/manifesto/process_pledge.R")
  run_cleaning_step("code/process/analysis_inputs/prepare_pledge.R")
  # Final datasets consumed by paper and response analyses; no presentation outputs.
  for (step in c("supporting_data", "wvs_summary", "manifesto_summary", "prrp_scores",
                 "wvs_manifesto_with_indexes", "waves1to6_wvs_manifesto_with_indexes")) {
    local(run_cleaning_step(file.path("code", "process", "analysis_inputs", paste0("prepare_", step, ".R")),
                            local=environment()))
  }
})

if (processData == 1) {
  # Non-proprietary copies of the prepared Stata files (JPE data policy).
  source("code/util/write_csv_copies.R")
  write_csv_copies("data/cleandata")
  cat("STAGE COMPLETE: preprocessing\n")
}

if (runModel==1) {
  # Run model; configuration markers were cleared before stage cleanup.
  # Estimate all variants together (estimate_all.R), then decompose each variant as before.
  source("code/model/estimate_all.R")
  source("code/model/decompose_delta.R")

  if (runEastSouthWestNorth==1){
    print("Running East South West North")
    file.create("code/eastSouthConfig")  
    source("code/model/decompose_delta.R")
    file.remove("code/eastSouthConfig")
    file.create("code/westNorthConfig")
    source("code/model/decompose_delta.R")
    file.remove("code/westNorthConfig")
  }
  
  # Generate weights
  source("code/model/get_weights.R")
  
  
  if (runNoDemographics==1) {
    file.create("code/noDemographicsConfig")  
    source("code/model/decompose_delta.R")
    file.remove("code/noDemographicsConfig")
  }

  if (runWithCZSK==1) {
    file.create("code/withCZSKConfig") 
    source("code/model/decompose_delta.R")
    file.remove("code/withCZSKConfig")
  }
  
  if (runNoGRIT==1) {
    file.create("code/noGRITConfig") 
    source("code/model/decompose_delta.R")
    file.remove("code/noGRITConfig")
  }


  if (runPledge==1){
    file.create("code/pledgeConfig") 
    source("code/model/decompose_delta.R")
    file.remove("code/pledgeConfig")
  }
  
  if (runStyle_Google==1){
    file.create("code/styleGoogleConfig")
    source("code/model/decompose_delta.R")
    file.remove("code/styleGoogleConfig")
  }
  
  if (runCohort==1) {
    file.create("code/cohortConfig")  
    source("code/model/decompose_delta.R")
    file.remove("code/cohortConfig")
  }
  
  
  if (runLeaveOneOut==1) {
    source("code/model/LCO_master.R")
  }
  else{
    if (!file.exists(file.path(decomposition_path, "loo", "decomposition.Rds"))) {
      dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))

      saveRDS(dec_empty, file.path(decomposition_path, "loo", "decomposition.Rds"))
    }  
  }
  
}

# Run analysis
if (runModel == 1) cat("STAGE COMPLETE: estimation\n")
if (runBS == 1) {
  source("code/model/bootstrap.R")
  cat("STAGE COMPLETE: bootstrap\n")
}
if (runAnalysis == 1) { source("code/analyze/analyzeMaster.R"); cat("STAGE COMPLETE: analysis\n") }

# Prepare paper
if (runCompile == 1) { source("code/documents/compilePaper.R"); cat("STAGE COMPLETE: compilation\n") }

# Record successful completion.
if (dev.cur() > 1L) graphics.off()
writeLines(paste("Requested replication stages completed:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  file.path(getOption("populist.log.dir"), "SUCCESS.txt"))

}
