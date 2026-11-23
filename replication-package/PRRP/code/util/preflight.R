# Called once before generated inputs are cleared.
replication_preflight <- function() {
  if (as.character(getRversion()) != "4.6.1") stop("Use R 4.6.1: the bundled runtime, or your own R 4.6.1 with POPULIST_SYSTEM_R=1.")
  lock <- renv::lockfile_read("renv.lock")
  for (name in names(lock$Packages)) {
    if (utils::packageVersion(name) != package_version(lock$Packages[[name]]$Version))
      stop("Package version mismatch: ", name, "; rerun make setup")
    loadNamespace(name)
  }
  required <- c(
    "data/rawdata/authorcreated/Manifesto/codebook_categories_MPDS2021a_indices.csv",
    "data/rawdata/authorcreated/MissingManifestos/manifesto_texts.csv",
    "data/rawdata/authorcreated/Pledges/ClassifiedManifestos.csv",
    "data/rawdata/authorcreated/Supply/insultscores/insult_scores_google.rds",
    "data/rawdata/authorcreated/Supply/manifestotexts/manifesto_texts.rds",
    "data/rawdata/authorcreated/Supply/partyleaders/party_leaders.rds",
    "data/rawdata/authorcreated/WVS/variable_list.csv",
    "data/rawdata/authorcreated/merge/cleanedPartiesToMatchManually_ivs.csv",
    "data/rawdata/authorcreated/model/dec_4_6_empty.rds",
    "data/rawdata/authorcreated/partyleader/party_leaders.csv",
    "data/rawdata/downloaded/not_share/EVS/EVS_Time_Series/ZA7503_v2-0-0.dta",
    "data/rawdata/downloaded/not_share/GESIS/Politbarometer_ZA2391_v16-1-0.dta",
    "data/rawdata/downloaded/public/IPUMS/cps_00047.dta",
    "data/rawdata/downloaded/not_share/Manifesto/MPDataset_MPDS2021a_stata14.dta",
    "data/rawdata/downloaded/not_share/Manifesto/codebook_categories_MPDS2021a.csv",
    "data/rawdata/downloaded/public/PopuList/The PopuList 3.0.csv",
    "data/rawdata/downloaded/not_share/WVS/WVS_Time_Series_2020/WVS_Trend_v2_0.dta",
    "data/rawdata/downloaded/public/femaleShare/Replication_data.RData",
    "data/rawdata/downloaded/public/femaleShare/global_legislator_database.dta",
    "data/rawdata/downloaded/public/femaleShare/multilevel_1-5-22.Rdata",
    "data/rawdata/downloaded/public/partyfacts/partyfacts-external-parties.csv",
    "data/rawdata/downloaded/public/word_freq/czech_stopwords.txt",
    "data/rawdata/downloaded/public/word_freq/estonia_stopwords.txt",
    "data/rawdata/downloaded/public/word_freq/extra_stopwords.csv",
    "data/rawdata/downloaded/public/word_freq/iceland_stopwords.txt",
    "data/rawdata/downloaded/public/word_freq/lithuania_stopwords.txt",
    "data/rawdata/downloaded/public/word_freq/poland_stopwords.txt",
    "data/rawdata/downloaded/public/word_freq/slovakia_stopwords.txt"
  )
  source("code/util/config.R")
  if (processData == 0) required <- character()
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Missing supplied inputs: ", paste(missing, collapse = ", "))
  source("code/util/config.R")
  stopifnot(processData %in% 0:1, runModel %in% 0:1, runAnalysis %in% 0:1, runCompile %in% 0:1, runBS %in% 0:1)
  if (!PRECISE_ESTIMATION %in% c(-1, 0, 0.5)) stop("PRECISE_ESTIMATION must be -1, 0 or 0.5.")
  cores <- parallelly::availableCores()
  if (is.na(estimationWorkers) || estimationWorkers < 1L || estimationWorkers > cores)
    stop("POPULIST_WORKERS must be between 1 and the ", cores, " available cores.")
  stopifnot(bootstrapDraws==200L, bootstrapWorkers==estimationWorkers)
  source("code/model/bootstrap_checks.R")
  if (processData == 0) {
    required_clean <- c(
      "Germany/politbarometer.rds",
      "IPUMS/wage_occ_2023.dta",
      "WVS/wvs_clean_1to6_waves.dta",
      "WVS/wvs_clean_4to6_waves.dta",
      "WVS/wvs_clean_4to6_waves_NoGRIT.dta",
      "WVS/wvs_clean_4to6_waves_eastsouth.dta",
      "WVS/wvs_clean_4to6_waves_no_dem.dta",
      "WVS/wvs_clean_4to6_waves_westnorth.dta",
      "WVS/wvs_clean_4to6_waves_with_CZSK.dta",
      "ivs/full_clean_ivs.dta",
      "ivs/ivs_femaleShare.RDS",
      "ivs/ivs_selected_countries_imputed_with_manifesto.dta",
      "ivs/ivs_selected_countries_with_manifesto.dta",
      "ivs/ivs_to_manifesto.dta",
      "ivs/prrp_profiles.rds",
      "ivs/prrp_score.rds",
      "ivs/prrp_score_covariate_ranking.rds",
      "ivs/wvs_top5_vars.rds",
      "leaders/imputation_summary.rds",
      "leaders/leader_changes.rds",
      "leaders/party_leaders.rds",
      "manifesto/descriptive_summary.rds",
      "manifesto/insultscores/insult_scores_google.rds",
      "manifesto/manifesto.dta",
      "manifesto/manifesto_parties.rds",
      "manifesto/manifesto_parties_1to6.rds",
      "manifesto/manifesto_parties_style_google.rds",
      "manifesto/manifesto_parties_withCZSK.rds",
      "manifesto/manifesto_populist.rds",
      "manifesto/manifesto_style.rds",
      "manifesto/manifesto_supply.Rds",
      "manifesto/manifesto_supply_google.Rds",
      "manifesto/pledges.rds",
      "manifesto/rhetoric_texts.rds",
      "manifesto/sources.rds",
      "manifesto/top_distinct_vars.rds",
      "manifesto/waves1to6_wvs_manifesto_with_indexes.rds",
      "manifesto/wvs_manifesto_with_indexes.rds",
      "metadata/empty_decomposition.rds",
      "metadata/manifesto_categories.rds",
      "metadata/manifesto_indices.rds",
      "metadata/wvs_variables.rds",
      "WVS/us_voters.rds",
      "manifesto/manifesto_parties_pledge.rds"
    )
    missing <- required_clean[!file.exists(file.path("data/cleandata", required_clean))]
    if (length(missing)) stop("Missing prepared inputs: ", paste(missing, collapse=", "))
    if (runAnalysis == 1) {
      seed <- attr(readRDS("data/cleandata/ivs/prrp_profiles.rds"), "post_lasso_seed")
      if (!is.integer(seed) || length(seed) < 2L) stop("Rebuild PRRP inputs with the current preprocessing code before reusing data/cleandata.")
    }
  }
  if (runAnalysis == 1 && runAdvancedDecomposition == 1 && runBS == 0) {
    bootstrap_check_collection("output/modeldata/bootstrap/BS",seq_len(bootstrapDraws))
  }
  if (runCompile == 1) {
    tools <- c("latexmk", "pdflatex", "biber", "kpsewhich")
    if (any(!nzchar(Sys.which(tools)))) stop("Install the documented TeX toolchain: ", paste(tools, collapse=", "))
    tex_sources <- list.files("code/documents", pattern="\\.tex$", recursive=TRUE, full.names=TRUE)
    text <- paste(unlist(lapply(tex_sources, readLines, warn=FALSE)), collapse="\n")
    matches <- regmatches(text, gregexpr("\\\\usepackage(?:\\[[^]]*\\])?\\{[^}]+\\}", text, perl=TRUE))[[1]]
    packages <- unique(unlist(strsplit(sub(".*\\{([^}]+)\\}", "\\1", matches), ",")))
    for (pkg in trimws(packages)) if (!length(system2("kpsewhich", shQuote(paste0(pkg, ".sty")), stdout=TRUE)))
      stop("Missing TeX package: ", pkg)
    fonts <- c("t1cmr.fd", "cmr10.tfm", "ecrm1000.tfm")
    for (font in fonts) if (!length(system2("kpsewhich", font, stdout=TRUE))) stop("Missing TeX font: ", font)
    versions <- unlist(lapply(tools[1:3], function(x) c(paste("Tool:", Sys.which(x)), local({ v <- system2(x, "--version", stdout=TRUE, stderr=TRUE); if (!is.null(attr(v, "status")) && attr(v, "status") != 0) stop("TeX tool cannot execute: ", x); v }))))
    writeLines(versions, file.path(getOption("populist.log.dir"), "tex-toolchain.txt"))
  }
  if (processData == 1) {
  dir.create("data/intdata", showWarnings = FALSE)
  dir.create("data/intdata/temp", showWarnings = FALSE)
  dir.create(file.path(getOption("populist.log.dir"), "stata"), recursive = TRUE, showWarnings = FALSE)
  source("code/util/commonVars.R")
  source("code/process/stata.R")
  check <- file.path("data/intdata", "preflight.do")
  writeLines(c("clear", "set obs 3", "gen x = _n", "which distinct", "which gunique", "which unique",
               "distinct x", "assert r(ndistinct) == 3", "gunique x", "assert r(unique) == 3", "unique x", "assert r(unique) == 3",
               "gen group = 1", "unique x, by(group) gen(nunique)", "assert nunique == 3 if !missing(nunique)"), check)
  on.exit(unlink(check), add = TRUE)
  runStata(check)
  }
  cat("Preflight passed: pinned dependencies and requested-stage inputs.\n")
}
