# One R handler per run; sourcing commonVars repeatedly does not duplicate it.
library(logging)
logging <- function(text = "Beginning file ", dir = getOption("populist.log.dir", Sys.getenv("POPULIST_LOG_DIR", unset=NA_character_))) {
  if (is.null(dir) || is.na(dir) || !nzchar(dir)) return(invisible(NULL))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(dir, "R.log")
  if (!identical(getOption("populist.log.handler"), filename)) {
    logging::basicConfig(level = "INFO")
    logging::addHandler(logging::writeToFile, file = filename, level = "INFO")
    options(populist.log.handler = filename)
  }
  logging::loginfo(text)
}
