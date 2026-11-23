# Start R in the replication folder; this folder is the project root.
# Set STATA_PATH in your environment if Stata is not on PATH.
stata_path <- Sys.getenv("STATA_PATH", unset = "stata-mp")
runAnalysis = as.integer(Sys.getenv("POPULIST_RUN_ANALYSIS", "1"))
runCompile = as.integer(Sys.getenv("POPULIST_RUN_COMPILE", "1"))
PRECISE_ESTIMATION = 0.5
# R worker processes for estimation and bootstrap; set POPULIST_WORKERS for smaller machines (see README).
estimationWorkers = as.integer(Sys.getenv("POPULIST_WORKERS", "12"))
bootstrapWorkers = estimationWorkers
bootstrapDraws = 200

# Preprocessing always refreshes data/intdata and data/cleandata; bootstrap files are retained.
processData = as.integer(Sys.getenv("POPULIST_PROCESS_DATA", "0"))

# Which models to run 
runModel = as.integer(Sys.getenv("POPULIST_RUN_MODEL", "1"))
runAdvancedDecomposition = 1
runBS = as.integer(Sys.getenv("POPULIST_RUN_BS", "1"))

# Change in Xs
runEastSouthWestNorth = 1
runWithCZSK = 1
runNoGRIT = 1

# Change in Zs
runPledge = 1
runStyle_Google = 1

# Change in Xs and/or Zs
runNoDemographics = 1

# Change in Model
runLeaveOneOut = 1



# Cohort specification for the paper's robustness figure.
runCohort = 1

# TeX Live 2023 has native Apple Silicon biber; override for an equivalent installed toolchain.
tex_bin <- Sys.getenv("TEX_BIN", "/usr/local/texlive/2023/bin/universal-darwin")
if (runCompile == 1) {
  Sys.setenv(TEXMFHOME=file.path(normalizePath(getwd()), "offline", "empty-library"),
             TEXINPUTS="", BIBINPUTS="", BSTINPUTS="", MKTEXPK="0", MKTEXTFM="0", MKTEXMF="0")
  if (!dir.exists(tex_bin)) stop("Set TEX_BIN to the installed TeX toolchain directory.")
  paths <- strsplit(Sys.getenv("PATH"), ":", fixed=TRUE)[[1]]
  Sys.setenv(PATH=paste(unique(c(tex_bin, paths)), collapse=":"))
}
