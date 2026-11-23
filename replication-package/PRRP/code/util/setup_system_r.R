# Build the project library renv/system-library from renv.lock in your own R 4.6.1.
# Run from the package root with: POPULIST_SYSTEM_R=1 make setup (see README, Other platforms).
# Needs internet access to CRAN and Bioconductor; packages may be compiled from source.
if (as.character(getRversion()) != "4.6.1") stop("Use R 4.6.1.")
lib <- file.path(normalizePath(getwd()), "renv", "system-library")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .Library))
if (!requireNamespace("renv", lib.loc = lib, quietly = TRUE))
  utils::install.packages("renv", lib = lib, repos = c(CRAN = "https://cloud.r-project.org"))
options(renv.config.cache.enabled = FALSE, renv.config.auto.snapshot = FALSE, renv.config.sandbox.enabled = FALSE)
# Restore in a temporary project holding only a copy of renv.lock: renv::restore() would otherwise
# rewrite this package's own renv/activate.R and renv/settings.json.
tmp <- tempfile("prrp-restore-")
dir.create(tmp)
file.copy("renv.lock", file.path(tmp, "renv.lock"))
renv::restore(project = tmp, library = lib, lockfile = file.path(tmp, "renv.lock"), prompt = FALSE)
lock <- renv::lockfile_read("renv.lock")
for (name in names(lock$Packages)) {
  if (utils::packageVersion(name, lib.loc = c(lib, .Library)) != package_version(lock$Packages[[name]]$Version))
    stop("Package version mismatch after restore: ", name)
}
cat("Restored", length(lock$Packages), "packages into renv/system-library\n")
