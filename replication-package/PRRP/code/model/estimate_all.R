# Estimate the main model and the robustness variants (waves 4-6 each).
# estimate_wave.R prepares each variant under its configuration marker, as before, and queues it.
# Then the cross-validation fits of all variants run as one queue on the estimationWorkers pool,
# followed by all full fits, so workers are not left idle while one variant's full fits run.
# The fits are deterministic, so the results are the same as estimating one variant at a time.

source("code/util/config.R")
local({
  markers <- c("",
               if (runEastSouthWestNorth == 1) c("eastSouthConfig", "westNorthConfig"),
               if (runNoDemographics == 1) "noDemographicsConfig",
               if (runWithCZSK == 1) "withCZSKConfig",
               if (runNoGRIT == 1) "noGRITConfig",
               if (runPledge == 1) "pledgeConfig",
               if (runStyle_Google == 1) "styleGoogleConfig",
               if (runCohort == 1) "cohortConfig")
  options(populist.estimation.queue = new.env())
  for (marker in markers) {
    if (nzchar(marker)) file.create(file.path("code", marker))
    source("code/model/estimate_wave.R")  # runs in the global environment, as before
    if (nzchar(marker)) file.remove(file.path("code", marker))
  }
  jobs <- getOption("populist.estimation.queue")$jobs
  options(populist.estimation.queue = NULL)
  logging(text = paste0("Fitting ", length(jobs), " model variants together"))
  print(system.time(run.estimation.jobs(jobs)))
})
