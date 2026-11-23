# Restore the vendored binary artifacts without contacting package servers.
stopifnot(as.character(getRversion()) == "4.6.1")
if (Sys.info()[["machine"]] != "arm64") stop("Apple Silicon macOS required.")
archives <- list.files("offline/packages", pattern = "\\.tgz$", full.names = TRUE)
if (!length(archives)) stop("Offline package archives are missing.")
lib <- normalizePath("renv", mustWork = TRUE)
lib <- file.path(lib, "library", "macos", "R-4.6", "aarch64-apple-darwin23")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
# Remove previous installed packages only; the vendored archives are immutable.
for (archive in archives) {
  name <- sub("_.*", "", basename(archive))
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", name)) stop("Invalid package name.")
  target <- file.path(lib, name)
  unlink(target, recursive = TRUE)
  utils::untar(archive, exdir = lib)
  d <- read.dcf(file.path(target, "DESCRIPTION"))
  stopifnot(d[1, "Package"] == name)
}
source("renv/activate.R")
lock <- renv::lockfile_read("renv.lock")
for (name in names(lock$Packages)) {
  stopifnot(utils::packageVersion(name) == package_version(lock$Packages[[name]]$Version))
  loadNamespace(name)
}
status <- renv::status(sources = FALSE)
if (!isTRUE(status$synchronized)) stop("The project library does not match renv.lock.")
cat("Offline restore complete:", length(archives), "pinned packages; R", as.character(getRversion()), "\n")
