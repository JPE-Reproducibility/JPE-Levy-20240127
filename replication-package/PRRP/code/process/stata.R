#doFile = "code/process/prepare_data_master_part_1.do"
runStata <- function(doFile) {
  log_dir <- getOption("populist.log.dir")
  if (is.null(log_dir)) stop("Run code/masterFile.R to initialize logs.")
  stata_logs <- file.path(log_dir, "stata")
  dir.create(stata_logs, recursive = TRUE, showWarnings = FALSE)
  wrapper <- "data/intdata/run_stata.do"
  stem <- tools::file_path_sans_ext(basename(doFile))
  # Use a unique wrapper basename to prevent stale batch logs from satisfying checks.
  wrapper <- file.path("data/intdata", paste0("run_stata_", stem, ".do"))
  logfile <- paste0(tools::file_path_sans_ext(basename(wrapper)), ".log")
  unlink(logfile)
  writeLines(c(paste0('global POPULISM_ROOT "', DB.folder, '"'),
               paste0('global POPULISM_DROPBOX "', DB.folder, '"'),
               paste0('global REPLICATION_LOGS "', normalizePath(stata_logs), '"'),
               'global S_ADO "BASE;PLUS"',
               'sysdir set PLUS "$POPULISM_ROOT/offline/stata"',
               'sysdir set PERSONAL "$POPULISM_ROOT/offline/empty-library"',
               'sysdir set OLDPLACE "$POPULISM_ROOT/offline/empty-library"',
               'mata: mata mlib index',
               paste0('cd "', DB.folder, '"'),
               paste0('do "', doFile, '"'),
               'display "REPLICATION_STATA_SUCCESS"'), wrapper)
  on.exit(unlink(c(wrapper, logfile)), add = TRUE)
  status <- system2(stata_path, c("-b", "do", shQuote(wrapper)))
  lines <- if (file.exists(logfile)) readLines(logfile, warn = FALSE) else character()
  destination <- file.path(stata_logs, paste0(stem, ".log"))
  if (file.exists(logfile)) file.copy(logfile, destination, overwrite = TRUE)
  step_log <- switch(basename(doFile),
    "prepare_data_master_part_1.do" = file.path(stata_logs, "prepare_data_master_1.txt"),
    "prepare_data_master_part_2.do" = file.path(stata_logs, "prepare_data_master_2.txt"), "")
  if (nzchar(step_log) && file.exists(step_log)) lines <- c(lines, readLines(step_log, warn = FALSE))
  if (status != 0L || !any(trimws(lines) == "REPLICATION_STATA_SUCCESS") ||
      any(grepl("^r\\([0-9]+\\);", trimws(lines)))) {
    stop("Stata failed: ", doFile, "; see ", destination)
  }
}
