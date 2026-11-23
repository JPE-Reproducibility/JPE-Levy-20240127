if (identical(Sys.getenv("POPULIST_SYSTEM_R"), "1") ||
    (dir.exists("offline/runtime") &&
     identical(normalizePath(R.home()), normalizePath("offline/runtime")))) {
  source("renv/activate.R")
} else {
  message("This replication package uses bundled R. Start with bash offline/R, or set POPULIST_SYSTEM_R=1 to use your own R 4.6.1 (see README).")
}
