# Offline activation: no downloads and no user libraries.
# POPULIST_SYSTEM_R=1 instead uses your own R 4.6.1 with the project library renv/system-library,
# which `POPULIST_SYSTEM_R=1 make setup` builds from renv.lock (see README, Other platforms).
local({
  project <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(project, "DESCRIPTION"))) stop("Start R in the replication folder.")
  if (identical(Sys.getenv("POPULIST_SYSTEM_R"), "1")) {
    if (!identical(getOption("populist.replication.project"), project)) {
      lib <- file.path(project, "renv", "system-library")
      if (!file.exists(file.path(lib, "renv", "DESCRIPTION")))
        stop("Project packages are not installed. Run: POPULIST_SYSTEM_R=1 make setup")
      .libPaths(c(lib, .Library))
      options(populist.replication.project = project)
    }
  } else {
  if (!dir.exists("offline/runtime") ||
      !identical(normalizePath(R.home()), normalizePath("offline/runtime")))
    stop("Use the bundled R runtime: bash offline/R, or run code/masterFile.R for batch preprocessing.")
  if (!identical(getOption("populist.replication.project"), project)) {
    libroot <- file.path(project, "renv", "library")
    lib <- file.path(libroot, "macos", "R-4.6", "aarch64-apple-darwin23")
    if (!file.exists(file.path(lib, "renv", "DESCRIPTION")))
      stop("Offline dependencies are not installed. Run: bash offline/setup.sh")
    Sys.setenv(RENV_PATHS_ROOT = file.path(project, "renv", "state"), RENV_PATHS_LIBRARY = libroot, RENV_PATHS_SANDBOX = file.path(project, "renv", "sandbox"),
               RENV_CONFIG_SANDBOX_ENABLED = "FALSE", RENV_CONFIG_CACHE_ENABLED = "FALSE", RENV_CONFIG_AUTO_SNAPSHOT = "FALSE")
    .libPaths(c(lib, .Library))
    options(renv.config.cache.enabled = FALSE, renv.config.auto.snapshot = FALSE)
    renv::load(project = project, quiet = TRUE)
    paths <- normalizePath(.libPaths())
    if (any(!startsWith(paths, project) & !startsWith(paths, normalizePath(R.home()))))
      stop("Environment isolation failed. Use bash offline/Rscript --vanilla code/masterFile.R")
    options(populist.replication.project = project)
  }
  }
})
