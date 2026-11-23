source("code/util/commonVars.R")
latex_root <- file.path(getOption("populist.log.dir"), "latex")
dir.create(latex_root, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(TEXMFOUTPUT=normalizePath(latex_root), MKTEXPK="0", MKTEXTFM="0", MKTEXMF="0")
for (name in "main") {
  build <- file.path(latex_root, name)
  dir.create(build, recursive=TRUE, showWarnings=FALSE)
  cat("Compiling:", name, "\n")
  status <- system2("latexmk", c("-norc", "-pdf", "-interaction=nonstopmode", "-halt-on-error",
    shQuote(paste0("-outdir=", build)), shQuote(paste0("-jobname=", name)),
    shQuote(paste0("-usepretex=", "\\def\\ReplicationLatexRoot{", latex_root, "}")),
    shQuote(file.path("code/documents", paste0(name, ".tex")))),
    stdout=file.path(build, "latexmk.log"), stderr=file.path(build, "latexmk.log"))
  if (status != 0) stop("Compilation failed: ", name)
  stopifnot(file.copy(file.path(build, paste0(name, ".pdf")), "output/documents", overwrite=TRUE))
}
