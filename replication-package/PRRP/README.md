# Decomposing the Rise of the Populist Radical Right

Oren Danieli, Noam Gidron, Shinnosuke Kikuchi, and Ro’ee Levy.

## Quick start

This package reproduces every table, figure and reported number in the paper and its appendices. It includes the prepared analysis data in `data/cleandata/`. Some raw survey and manifesto files must be obtained from their providers; the [data availability statement](#data-availability-statement) lists them.

**Which run to use**

- **If you have the raw data**, saved at the paths in the [data availability statement](#data-availability-statement), run `make all`. It rebuilds the prepared data from the raw files and then runs the full analysis. It needs a licensed Stata.
- **If you do not have the raw data**, run `make fresh`. It starts from the prepared data in `data/cleandata/` and runs the same analysis. It needs neither Stata nor the raw data.

**What you need**

- By default, 12 CPU cores and 64 GB of RAM; smaller machines can use fewer workers (see [Smaller machines](#smaller-machines)).
- About 5 GB of free disk space.
- About 27 hours for the analysis with 12 workers, which runs unattended.
- TeX Live 2023 (or an equivalent TeX installation) to compile the paper.
- For `make all` only: the raw files and a licensed Stata (StataNow 19.5).

**On an Apple Silicon Mac** (the setup the authors used; R 4.6.1 and all R packages are bundled), run from the package root:

```bash
make setup   # once: installs the bundled R and packages, no internet needed
make all     # with the raw data, about 27 hours; without it, run make fresh instead (about 26.5 hours)
make paper   # about 30 seconds: compiles output/documents/main.pdf
```

**On any other machine** (Linux, Windows through WSL, or an Intel Mac), install R 4.6.1 with the tools to build R packages from source, then run:

```bash
POPULIST_SYSTEM_R=1 make setup   # once: installs the package versions in renv.lock, needs internet
POPULIST_SYSTEM_R=1 make all     # or POPULIST_SYSTEM_R=1 make fresh without the raw data
POPULIST_SYSTEM_R=1 make paper
```

For `make all`, if `stata-mp` is not on your shell's `PATH`, set `STATA_PATH` to your licensed Stata batch executable. If TeX Live 2023 is not in `/usr/local/texlive/2023/bin/universal-darwin`, set `TEX_BIN` to your TeX executable directory before `make paper`. Results are written to `output/`; the [exhibit mapping](#exhibit-mapping) lists which file is which exhibit. A run has finished successfully when `output/logs/<run-id>/SUCCESS.txt` exists.

The [instructions](#instructions-to-replicators) below explain each step.

## Package contents

PRRP is the replication package for the article and appendices in `code/documents/main.tex`. The workflow prepares data, estimates the voting model, computes decompositions, and produces paper exhibits.

| Path | Contents |
|---|---|
| `code/` | All programs; see [Description of programs](#description-of-programs). |
| `data/cleandata/` | Prepared analysis data; `make fresh` starts from it, and `make all` rebuilds it. |
| `data/rawdata/` | Raw inputs; the [data availability statement](#data-availability-statement) lists which are included and which must be obtained from providers. |
| `output/` | Empty; every run writes its results here. |
| `offline/` | Bundled R 4.6.1 runtime and R packages for Apple Silicon macOS, and the Stata add-ons. |
| `renv.lock`, `renv/` | Pinned R package versions and the project's R activation script. |
| `documentation/input-inventory.csv` | Source documentation for each input file. |
| `Makefile` | The run targets `setup`, `fresh`, `paper` and `all`. |
| `LICENSE` | Licenses for the code and data. |

## Instructions to replicators

Run every command from the package root, the folder that contains this README. Which run to use in Step 2 depends on whether you have the raw data.

### Step 1. Set up R (once per machine)

- **Apple Silicon Mac.** Run `make setup`. It unpacks the bundled R 4.6.1 runtime (`offline/R-4.6.1-arm64-runtime.tar.gz`) and the pinned R packages (`offline/packages/`) and checks them against `renv.lock`. It needs no internet access.
- **Any other machine** (Linux, Windows through the Windows Subsystem for Linux, or an Intel Mac). Install R 4.6.1, with the compilers and system libraries needed to build R packages from source; `make` and `bash` must be available. Then run `POPULIST_SYSTEM_R=1 make setup`. It builds the project library `renv/system-library/` with the exact package versions in `renv.lock`, downloaded from CRAN and Bioconductor, so it needs internet access. Add `POPULIST_SYSTEM_R=1` to every `make` command below. `make` calls `Rscript` from your `PATH`; set `RSCRIPT` to the full path of another `Rscript` to choose a different R installation. Preflight still requires R 4.6.1 and the package versions in `renv.lock`. The authors ran the package only with the bundled runtime on Apple Silicon macOS; this route has not been tested on other platforms.

### Step 2. Run the analysis: `make all` or `make fresh`

- **If you have the raw data: `make all`.** It rebuilds `data/intdata/` and `data/cleandata/` from the raw inputs, re-estimates the model, fits 200 bootstrap draws, and writes every table, figure and reported value to `output/`. It takes about 27 hours with 12 R workers. It needs the raw files marked **Obtain from provider** in the [data availability statement](#data-availability-statement), saved at the listed paths, and a licensed Stata (StataNow 19.5). If `stata-mp` is not on your shell's `PATH`, set `STATA_PATH` to your licensed batch executable, for example `export STATA_PATH="/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp"`.
- **If you do not have the raw data: `make fresh`.** It starts from the prepared data in `data/cleandata/` and runs the same estimation, bootstrap and exhibit stages. It takes about 26.5 hours with 12 R workers and needs neither Stata nor the raw data.

Both runs clear the entire `output/` tree when they start, so keep any earlier results elsewhere. Each run writes its settings and diagnostics to `output/logs/<run-id>/`. Look for `SUCCESS.txt` when the run finishes. If the run stops, check `console.log` and `FAILURE.txt` if present. `make all` also writes Stata logs under the run's `stata/` subfolder and records memory use in `output/logs/<run-id>/resources.txt`.

### Step 3. Compile the paper: `make paper`

`make paper` compiles the exhibits into `output/documents/main.pdf` in about 30 seconds. It needs the TeX tools listed under [Computational requirements](#computational-requirements). The default TeX directory is `/usr/local/texlive/2023/bin/universal-darwin`. If yours differs, set `TEX_BIN` to your TeX executable directory before running `make paper`, even if the tools are already on your shell's `PATH`.

### Smaller machines

By default, estimation and bootstrap use 12 R worker processes. On a machine with fewer cores or less memory, set `POPULIST_WORKERS` to a smaller number, for example `POPULIST_WORKERS=6 make all` or `POPULIST_WORKERS=6 make fresh` (with `POPULIST_SYSTEM_R=1` as well on other machines). Fewer workers need less memory and take longer. The figures below are rough estimates scaled from the authors' 12-worker run, in which the main R session used up to 15 GiB and each worker roughly 4.5 GiB more.

| Workers | Approximate RAM | Approximate time for `make fresh` |
|---:|---:|---:|
| 12 (default) | 64 GB | 26.5 hours |
| 8 | 48 GB | 40 hours |
| 6 | 40 GB | 53 hours |
| 4 | 32 GB | 80 hours |

`make all` takes about 20 minutes longer than `make fresh` for raw preprocessing. `POPULIST_WORKERS` must not exceed the number of available cores. Every random draw is seeded per task, so results are expected to be identical for any number of workers; the authors produced the reported results with 12 workers and did not rerun them with fewer.

### Run targets and output folders

| Command | Starting point and stages |
|---|---|
| `make fresh` | Start from `data/cleandata/`, re-estimate the model at precision 0.5, fit 200 bootstrap draws, and write the exhibits. |
| `make all` | Rebuild `data/intdata/` and `data/cleandata/` from the raw inputs, then run the `make fresh` stages. |
| `make paper` | Compile the exhibits into `output/documents/main.pdf`. |

| Run | Folders written |
|---|---|
| `make fresh` | `output/figures/`, `output/tables/`, `output/values/`, `output/other/`, `output/modeldata/results/`, `output/modeldata/decomposition/`, and `output/modeldata/bootstrap/BS/`. |
| `make all` | The `make fresh` folders, plus `data/intdata/` and `data/cleandata/`. |
| `make paper` | `output/documents/`. TeX build files and `latexmk.log` are written under the run log directory in `latex/main/`. |

## Computational requirements

- **R.** The supplied offline bundle supports Apple Silicon macOS and uses R 4.6.1. Keep `offline/R-4.6.1-arm64-runtime.tar.gz` and `offline/packages/` in place. Run `make setup` to install and check the dependencies recorded in `renv.lock`. On other systems, use your own R 4.6.1; see [Step 1](#step-1-set-up-r-once-per-machine).
- **TeX.** Use TeX Live 2023 with native Apple Silicon biber 2.19. The run needs `latexmk`, `pdflatex`, `biber`, and `kpsewhich`. Preflight checks the packages and fonts required by `code/documents/main.tex`.
- **Stata.** Raw preprocessing needs a licensed Stata executable. Use StataNow 19.5 for raw preprocessing. The project supplies its Stata add-ons under `offline/stata/`. `make fresh` starts from prepared data and does not need Stata.
- **Workers and memory.** Estimation and bootstrap use 12 R worker processes by default, with one numerical-library thread per process; set `POPULIST_WORKERS` to use fewer (see [Smaller machines](#smaller-machines)). Allow local communication between R processes. With 12 workers the reported run peaked at 63.4 GiB of sampled memory, so use at least 64 GB of RAM for the default setting.
- **Hardware used by the authors.** Apple Mac Studio (Mac16,9) with an Apple M4 Max (16 cores: 12 performance and 4 efficiency) and 128 GB of RAM, running macOS 27.2.1.

### Randomness, runtime and storage

Model estimation uses fixed seeds in `code/model/estimate_wave.R`. Bootstrap sampling uses seed-indexed draws and `future.seed=TRUE` in `code/model/bootstrap.R`. Keep the scripts’ seeds and execution order when replicating.

The November 16–17, 2026 full replication ran on an Apple Silicon Mac Studio with 12 R workers for estimation and bootstrap, one numerical-library thread per worker, and 200 bootstrap draws.

| Command and run | Wall-clock runtime | Peak sampled memory |
|---|---:|---:|
| `make all` (November 16–17, 12 workers) | 26 hours, 46 minutes, 59 seconds | 63.38 GiB |
| `make fresh` (estimated from the same run) | about 26 hours, 25 minutes | 63.38 GiB |
| `make paper` (earlier benchmark) | 29 seconds | — |

The November 16–17 runtime covers preprocessing, model estimation, bootstrap, and exhibit generation. Preprocessing from raw data took about 22 minutes of that run; `make fresh` skips it, so its runtime is estimated as the `make all` runtime minus preprocessing. The memory peak occurred at the start of the leave-country-out estimation, about 8.6 hours into the run.

`make all` records the combined resident memory and CPU of the R session and all its worker processes every five seconds in `output/logs/<run>/resources.txt`. The file's last line gives the peaks.

The completed local replication workspace occupies approximately 15.4 GiB on disk, excluding Git metadata. This includes 1.08 GiB for raw inputs, 10.37 GiB for intermediate data, 1.29 GiB for cleaned data, 1.06 GiB for outputs, and 1.61 GiB for R software and libraries. These are on-disk measurements taken after the run. Peak disk usage during the run was not measured.

## Data availability statement

Paths in the table are relative to the PRRP replication package root. **Included** identifies files supplied with the replication package. Files marked **Obtain from provider** are needed only for `make all`: download the exact version and save it at the listed path. If a historical version is missing from the download interface, request that version from the provider.

Use the [input inventory](documentation/input-inventory.csv) for additional source documentation and file descriptions. This README states which raw inputs are included.

| Source | Exact version or description | Path in PRRP | How to obtain |
|---|---|---|---|
| World Values Survey | Time-Series v2.0, 1981–2020 | `data/rawdata/downloaded/not_share/WVS/WVS_Time_Series_2020/WVS_Trend_v2_0.dta` | **Obtain from provider** |
| EVS via GESIS | Trend File 1981–2017, ZA7503 v2.0.0 | `data/rawdata/downloaded/not_share/EVS/EVS_Time_Series/ZA7503_v2-0-0.dta` | **Obtain from provider** |
| Forschungsgruppe Wahlen via GESIS | Politbarometer ZA2391 v16.1.0 | `data/rawdata/downloaded/not_share/GESIS/Politbarometer_ZA2391_v16-1-0.dta` | **Obtain from provider** |
| Manifesto Project | Dataset 2021a, Stata 14 format | `data/rawdata/downloaded/not_share/Manifesto/MPDataset_MPDS2021a_stata14.dta` | **Obtain from provider** |
| Manifesto Project | Original category codebook, 2021a | `data/rawdata/downloaded/not_share/Manifesto/codebook_categories_MPDS2021a.csv` | **Obtain from provider** |
| IPUMS CPS | v12.0, extract 47, created 2025-08-05 | `data/rawdata/downloaded/public/IPUMS/cps_00047.dta` | **Included** |
| Party Facts | External-party crosswalk downloaded 2021-03-28 | `data/rawdata/downloaded/public/partyfacts/partyfacts-external-parties.csv` | **Included** |
| The PopuList | The PopuList 3.0 | `data/rawdata/downloaded/public/PopuList/The PopuList 3.0.csv` | **Included** |
| Weeks et al. (2022) | Replication data, Dataverse V1.0 | `data/rawdata/downloaded/public/femaleShare/Replication_data.RData` | **Included** |
| Carnes et al. (2024) | Global Legislator Database, Dataverse V1.0 | `data/rawdata/downloaded/public/femaleShare/global_legislator_database.dta` | **Included** |
| Adams et al. (2022) | Multilevel data, Dataverse V1.0 | `data/rawdata/downloaded/public/femaleShare/multilevel_1-5-22.Rdata` | **Included** |
| Authors and Manifesto Project | Official 2021a category descriptions with author-assigned indices | `data/rawdata/authorcreated/Manifesto/codebook_categories_MPDS2021a_indices.csv` | **Included** |
| Authors | Source list for individual manifesto texts | `data/rawdata/authorcreated/MissingManifestos/manifesto_texts.csv` | **Included** |
| Authors and manifesto sources | Final pledge classifications and underlying manifesto text | `data/rawdata/authorcreated/Pledges/ClassifiedManifestos.csv` | **Included** |
| Authors and Google Perspective | Fixed clause-level INSULT scores and source material | `data/rawdata/authorcreated/Supply/insultscores/insult_scores_google.rds` | **Included** |
| Authors and manifesto sources | Compiled manifesto texts and translations | `data/rawdata/authorcreated/Supply/manifestotexts/manifesto_texts.rds` | **Included** |
| Authors | Prepared party-leader table | `data/rawdata/authorcreated/Supply/partyleaders/party_leaders.rds` | **Included** |
| Authors | IVS variable list with question wording and coding notes | `data/rawdata/authorcreated/WVS/variable_list.csv` | **Included** |
| Authors | Completed survey-to-Party-Facts crosswalk | `data/rawdata/authorcreated/merge/cleanedPartiesToMatchManually_ivs.csv` | **Included** |
| Authors | Empty decomposition template | `data/rawdata/authorcreated/model/dec_4_6_empty.rds` | **Included** |
| Authors | Party-leader characteristics coded from public sources | `data/rawdata/authorcreated/partyleader/party_leaders.csv` | **Included** |
| Stopwords ISO | Czech list, bundled copy | `data/rawdata/downloaded/public/word_freq/czech_stopwords.txt` | **Included** |
| Stopwords ISO | Estonian list, bundled copy | `data/rawdata/downloaded/public/word_freq/estonia_stopwords.txt` | **Included** |
| Stopwords ISO | Icelandic list, bundled copy | `data/rawdata/downloaded/public/word_freq/iceland_stopwords.txt` | **Included** |
| Stopwords ISO | Lithuanian list, bundled copy | `data/rawdata/downloaded/public/word_freq/lithuania_stopwords.txt` | **Included** |
| Stopwords ISO | Polish list, bundled copy | `data/rawdata/downloaded/public/word_freq/poland_stopwords.txt` | **Included** |
| Stopwords ISO | Slovak list, bundled copy | `data/rawdata/downloaded/public/word_freq/slovakia_stopwords.txt` | **Included** |
| NLTK and project additions | Bundled supplementary stopword list | `data/rawdata/downloaded/public/word_freq/extra_stopwords.csv` | **Included** |

### Obtaining the provider files

- **World Values Survey.** The study uses the Time-Series v2.0 file downloaded on 2022-02-23. Complete the registration form, accept the conditions of use, and request v2.0 from the WVS archive if it is unavailable on the download page. [WVS time-series downloads](https://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp) · [Conditions of use](https://www.worldvaluessurvey.org/AJDownloadLicense.jsp)
- **EVS Trend File.** Register for a free GESIS account and select ZA7503 v2.0.0, downloaded for this study on 2022-02-23. Request v2.0.0 from GESIS if necessary. [GESIS study page](https://search.gesis.org/research_data/ZA7503) · [Version DOI](https://doi.org/10.4232/1.13736) · [GESIS terms](https://www.gesis.org/en/institute/data-usage-terms)
- **Politbarometer.** Use a free GESIS account to access ZA2391 and select v16.1.0. Request v16.1.0 from GESIS if necessary. [GESIS study page](https://search.gesis.org/research_data/ZA2391) · [Version DOI](https://doi.org/10.4232/1.14527) · [GESIS terms](https://www.gesis.org/en/institute/data-usage-terms)
- **Manifesto Project 2021a.** Create a free account or sign in, accept the terms, and select the Stata 14 download for release 2021a. The original 2021a category codebook is available as a direct CSV download. Contact the Manifesto Project if either file is unavailable. [2021a dataset](https://manifesto-project.wzb.eu/datasets/MPDS2021a) · [Original category codebook](https://manifesto-project.wzb.eu/down/data/2021a/codebooks/codebook_categories_MPDS2021a.csv) · [Terms of use](https://manifesto-project.wzb.eu/information/documents/terms_of_use)

### Sources and terms for included files

- **IPUMS CPS.** The package includes the existing extract 47 from IPUMS CPS v12.0. The analysis uses 2023 ASEC records and the variables YEAR, ASECWT, OCC, WKSWORK1, and INCWAGE. Cite Flood et al. (2024) and acknowledge the U.S. Census Bureau and Bureau of Labor Statistics. Independent extraction requires a free IPUMS account. Ask IPUMS about access to archived v12.0 to reproduce the original extract. [Version DOI](https://doi.org/10.18128/D030.V12.0) · [Variable documentation](https://cps.ipums.org/cps/) · [Citation instructions](https://cps.ipums.org/cps/citation.shtml) · [Terms of use](https://cps.ipums.org/cps/terms.shtml)
- **Party Facts.** The supplied external-party CSV is the website snapshot downloaded on 2021-03-28. Cite Döring and Regel (2019) and record the snapshot date. The authors’ completed crosswalk is listed separately in the table. [Party Facts downloads](https://partyfacts.herokuapp.com/download/) · [Döring and Regel (2019)](https://doi.org/10.1177/1354068818820671)
- **PopuList and female-share data.** The four source deposits list CC0. Cite the specific datasets used. [The PopuList 3.0](https://doi.org/10.7910/DVN/WDZZ1V) · [Weeks et al. replication data](https://doi.org/10.7910/DVN/SG55BJ) · [Global Legislator Database](https://doi.org/10.7910/DVN/U1ZNVT) · [Adams et al. multilevel data](https://doi.org/10.7910/DVN/AHQRVR)
- **Manifesto-derived project files.** The annotated codebook combines the official category descriptions and author-assigned indices. The corpus combines Manifesto Project, Comparative Agendas, ManifestoVault, Polidoc, and translated texts. The Manifesto Project terms apply to its material. The source terms recorded for ManifestoVault and Comparative Agendas codes are CC BY-NC-ND 4.0 and CC BY-NC-SA 4.0, respectively. Replication uses fixed scores and final pledge classifications. [Manifesto Project terms](https://manifesto-project.wzb.eu/information/documents/terms_of_use)
- **Stopwords.** Credit Stopwords ISO for the six language lists and the NLTK stopwords corpus for source material used in the supplementary list. The supplementary CSV also records project additions. [Stopwords ISO](https://github.com/stopwords-iso) · [NLTK data](https://www.nltk.org/nltk_data/)

Third-party materials are subject to their respective source terms.

### Statement about rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permissions are documented in the [LICENSE](LICENSE) file.

### License

See [LICENSE](LICENSE). The code is licensed under the MIT License. Data created by the authors are licensed under CC BY 4.0. Third-party data, the bundled R runtime and packages, and the Stata add-ons remain under their original licenses and terms of use, listed above.

## Dataset list and generated files

`data/rawdata/` contains the inputs listed above. Preprocessing builds `data/intdata/` (scratch files) and `data/cleandata/` (prepared analysis inputs). Model fits and decompositions are written under `output/modeldata/`.

The package includes the raw inputs marked **Included** above, and the complete prepared analysis data in `data/cleandata/`. The raw files marked **Obtain from provider** are needed only to rebuild the prepared data with `make all`. The scratch folder `data/intdata/` is not included; preprocessing rebuilds it. The `output/` folder is empty; each run writes its results there.

Every Stata (`.dta`) file in `data/cleandata/` and every included raw Stata file has a CSV copy with the same name in the same folder. The CSV files contain the stored values; variable and value labels remain in the `.dta` files. `code/util/write_csv_copies.R` writes the copies, and `make all` regenerates those in `data/cleandata/`. R data files (`.rds`, `.RData`) are supplied as is.

Use the source codebooks, Stata variable and value labels, and supplied variable list to interpret inputs. Rebuilding the prepared inputs requires every file in the data availability table. Starting from complete prepared data uses the files in `data/cleandata/`.

### Manual party matching

We matched WVS and EVS party labels to Party Facts IDs using the Party Facts core-party file. The source is Party Facts, described by [Döring and Regel (2019)](https://doi.org/10.1177/1354068818820671). The supplied crosswalk (`data/rawdata/authorcreated/merge/cleanedPartiesToMatchManually_ivs.csv`) records the final assignments. Preprocessing imports this crosswalk and excludes entries without an assigned ID.

The manual matching used the following decisions for ambiguous survey labels:

- **Germany.** Use the longer-running Green Party for the 1979–1993 Green label. The survey label does not distinguish the separate East German party. Leave the combined REP, NPD, or DVU response without a single party ID.
- **United Kingdom.** Use the Green Party of England and Wales ID for the general Green Party label and the Scottish National Party ID for the combined Scottish/Welsh National Party label. The Social Democrat label appeared in 13 responses from a period when the party was listed as inactive in the sources consulted.
- **Sweden.** Assign the mistranslated Fascist People’s Party responses to the Liberals, the party identified by the Swedish label.
- **Italy.** Treat votes assigned to Italian Radicals before that party’s founding as votes for its predecessor, the Radical Party.

## Description of programs

| Programs | Purpose |
|---|---|
| `code/masterFile.R` | Orchestrates preprocessing, estimation and exhibit production. |
| `code/process/` | Imports, cleans, matches and prepares inputs. |
| `code/model/` | Estimates models, runs bootstrap and computes decompositions. |
| `code/analyze/` | Generates tables, figures and reported values. |
| `code/util/` | Configuration, preflight checks, logging, CSV copies of Stata files, and memory recording. |
| `code/documents/main.tex` | Assembles the article and appendices. |

## Exhibit mapping

`make all` or `make fresh` writes every exhibit below; `make paper` compiles them into `output/documents/main.pdf`. Figure A.1 is a diagram drawn in TeX and is not produced by a program.

| Exhibit | Title | Output file | Program |
|---|---|---|---|
| Figure 1 | Support for Populist Radical Right Parties Over Time | `output/figures/manifesto_populist_rise/fr_vote_share_global.pdf` | `code/analyze/party_positions/analyze_prrp_rise.R` |
| Figure 2 | Support for Populist Radical Right Parties by Country, 2017–2020 IVS Wave | `output/figures/WVS/populist_support_maps/prrp_support_map_wave6.pdf` | `code/analyze/voter_attributes/map_populist_support.R` |
| Figure 3 | Voting Weights Distribution by Supported Party Family, 2017–2020 | `output/figures/model_weight/weight_static_index_and_highbtwgrvar_weighted_sd.pdf` | `code/analyze/priorities_and_model/plot_weight_static_parfam_bar.R` |
| Figure 4 | Decomposition of the Rise in Populist Radical Right Support | `output/figures/decomposition/waterfall_total_46.pdf` | `code/analyze/decomposition/plotDecomposition.R` |
| Figure 5 | Decomposition Robustness | `output/figures/decomposition/Decomposition_Robustness_Waterfalls.pdf` | `code/analyze/decomposition/DecompositionRobustnessGraph.R` |
| Figure 6 | Changes in the Distribution of Voting Weights, Holding Attributes Fixed | `output/figures/model_weight/weight_distr_index_allpar_standardized_econ_soc_46.pdf` | `code/analyze/priorities_and_model/plot_weight_static_parfam_bar.R` |
| Figure 7 | Change in Predicted Ideological Self-Identification | `output/figures/heat_map_voters/voters_heatmap_4to6_rescale.pdf` | `code/analyze/priorities_and_model/waves1to6_heatmap.R` |
| Figure 8 | Relative Increase in Support for PRRP due to Change in Priorities | `output/figures/decomposition/weighted_avg_diff_from_full_pop_bar.pdf` | `code/analyze/priorities_and_model/hetero_priorities_prrp.R` |
| Figure 9 | Evolution of Voter Opinions | `output/figures/WVS/populist_voters/PRRP_pop_opinion_over_time.pdf` | `code/analyze/voter_attributes/analyze_voter_opinion_trend.R` |
| Figure 10 | Counterfactual Support for the National Front by Voter Attributes | `output/figures/counterfactual_maps/france_counterfactual_with_US.pdf` | `code/analyze/priorities_and_model/reserviors.R` |
| Figure 11 | Changes in Party Positions Over Time | `output/figures/manifesto_party_pos_change/index_aggregate.pdf` | `code/analyze/party_positions/analyze_party_family_positions.R` |
| Table 1 | Descriptive Statistics | `output/tables/top5_manifesto_wvs.tex` | `code/analyze/party_positions/analyze_desc_stat_manifesto.R` |
| Figure A.1 | Voting Model Summarized | `code/documents/input_common/model_flow.tex` | Author-maintained TeX source |
| Figure A.2 | Covariates Most Strongly Correlated with the PRRP Score | `output/figures/WVS/populist_voters/PRRP_cor_weighted.pdf` | `code/analyze/voter_attributes/analyze_populist_voters.R` |
| Figure A.3 | Similarities between Party Voters | `output/figures/WVS/voter_correlations/correlations_average_diff.pdf` | `code/analyze/voter_attributes/analyze_party_correlation.R` |
| Figure A.4 | Voter Attributes With the Largest Coefficients in the Weight Indices | `output/figures/phis/phi_newZs_pos.pdf`; `output/figures/phis/phi_newZs_demo.pdf` | `code/analyze/priorities_and_model/plot_phi_with_new_Zs.R` |
| Figure A.5 | Additional Decomposition Robustness | `output/figures/decomposition/Decomposition_Robustness_Waterfalls_Appendix.pdf` | `code/analyze/decomposition/DecompositionRobustnessGraph.R` |
| Figure A.6 | Changes in the Distribution of Voting Weights, Holding Attributes Fixed: Alternative Economic and Cultural Indices | `output/figures/model_weight/alternative_cmp_measures/bak_hob_weight_distr.pdf`; `output/figures/model_weight/alternative_cmp_measures/prosser_weight_distr.pdf` | `code/analyze/priorities_and_model/prosser_weights.R` |
| Figure A.7 | Changes in the Distribution of Voting Weights, Holding Attributes Fixed: Indices on Additional Topics | `output/figures/model_weight/weight_distr_index_allpar_standardized_ex_index_46.pdf` | `code/analyze/priorities_and_model/plot_weight_static_parfam_bar.R` |
| Figure A.8 | Case Study: Most Important Issues in Germany Over Time | `output/figures/case_studies/Germany/stacked_bar_plot_categorized.pdf` | `code/analyze/case_studies/GG2.R` |
| Figure A.9 | Correlations of Voter Opinions and the Positions of their Preferred Party | `output/figures/voter_party_correlation/voterPartyCorPCA.pdf`; `output/figures/voter_party_correlation/voterPartyCorPCA_1346_combined.pdf` | `code/analyze/priorities_and_model/analyze_voter_parties_correlation.R`; `code/analyze/priorities_and_model/waves1to6_voter_parties_correlation.R` |
| Figure A.10 | Change in Cultural and Economic Weight Ratio, Holding Attributes Fixed | `output/figures/model_weight/dist_by_group/hetero_prior_change.pdf` | `code/analyze/priorities_and_model/plot_weight_dem_op_slide.R` |
| Figure A.11 | Average PRRP Score by Subpopulation | `output/figures/decomposition/demo_prrp_score_bar.pdf` | `code/analyze/priorities_and_model/hetero_priorities_prrp.R` |
| Figure A.12 | Change in Predicted Ideological Self-Identification, Earlier Time Period | `output/figures/heat_map_voters/voters_heatmap_1to4.pdf`; `output/figures/heat_map_voters/voters_heatmap_46_V2.pdf` | `code/analyze/priorities_and_model/waves1to6_heatmap.R` |
| Figure A.13 | Changes in the Salience of Topics in Manifestos Over Time | `output/figures/manifesto_party_pos_change/salience_grouped_broad.pdf` | `code/analyze/party_positions/analyze_party_family_positions.R` |
| Figure A.14 | Changes in PRRP Score and PRRP Support, 2005–2017 | `output/figures/WVS/populist_voters/PRRP_score_corr_support_weighted.pdf` | `code/analyze/voter_attributes/analyze_populist_voters.R` |
| Figure A.15 | Voters' PRRP Score by Country | `output/figures/WVS/populist_voters/PRRP_score_dist_density_country_ceil_weighted.pdf` | `code/analyze/voter_attributes/analyze_populist_voters.R` |
| Figure A.16 | Opinions that Changed the Most, 2005–2020 | `output/figures/WVS/opinions_trend/opinions_average_trend_1st_half.pdf` | `code/analyze/voter_attributes/analyze_voter_opinion_trend.R` |
| Figure A.17 | Counterfactual Support for the AfD by Voter Attributes | `output/figures/counterfactual_maps/germ_counterfactual_with_US.pdf` | `code/analyze/priorities_and_model/reserviors.R` |
| Figure A.18 | Changes in Most Distinctive Party Positions Over Time | `output/figures/manifesto_party_pos_change/top_distinct_aggregate.pdf` | `code/analyze/party_positions/analyze_party_family_positions.R` |
| Figure A.19 | Within-Party Position Changes | `output/figures/manifesto_party_pos_change/index_parfam_within.pdf` | `code/analyze/party_positions/analyze_party_family_positions.R` |
| Figure A.20 | Changes in Leadership Demographics Over Time | `output/figures/femaleShare/demo_weighted_all.pdf` | `code/analyze/party_positions/femaleShareGraph.R` |
| Figure A.21 | Decomposition with New Entrants | `output/figures/decomposition/waterfall_entry_46.pdf` | `code/analyze/decomposition/plotDecomposition.R` |
| Figure A.22 | Average Number of PRRP per Country | `output/figures/manifesto_populist_rise/fr_num_global.pdf` | `code/analyze/party_positions/analyze_prrp_rise.R` |
| Figure A.23 | Change in the Residual for Parties With and Without New Leaders | `output/figures/newLeaderResidual/waves46_density.pdf` | `code/analyze/priorities_and_model/newLeaderResidual.R` |
| Figure A.24 | Distribution of Counterfactual Vote Shares for New Parties | `output/figures/partyResidual/errbar_prrpShrs_vs_all.pdf` | `code/analyze/decomposition/smallPartyResidual.R` |
| Table A.1 | List of Populist Radical Right Parties | `output/tables/descriptive_stat/prrp_parties.tex` | `code/analyze/party_positions/rrp_parties.R` |
| Table A.2 | IVS Data | `output/tables/descriptive_stat/dis_stat_table.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| Table A.3 | Decomposition of the Rise in PRRP Support by Country | `output/tables/InferenceTable.tex` | `code/analyze/priorities_and_model/create_inference_table.R` |
| Table A.4 | Manifesto Rhetoric Descriptive Statistics | `output/tables/manifesto_rhetoric_sum_stats.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| Table A.5 | Populist Radical Right Parties Absent in the 2005–2009 Wave | `output/tables/descriptive_stat/party_entry_list.tex` | `code/analyze/party_positions/create_party_entry_table.R` |
| Table A.6 | Internal Consistency of Manifesto Rhetoric | `output/tables/manifesto/manifesto_rhetoric_consistency.tex` | `code/analyze/party_positions/check_manifesto_rhetoric_consistency_demean.R` |
| Table A.7 | IVS Data Matched with CMP | `output/tables/descriptive_stat/data_filtering.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| Table A.8 | Observed and Predicted Support by Party Family Over Time | `output/tables/shy_voters.tex` | `code/analyze/voter_attributes/analyze_shy_voters.R` |
| Table B.1 | CMP Indices | `output/tables/manifesto/manifestoVariablesExtended.tex` | `code/analyze/party_positions/create_manifesto_var_table.R` |
| Table B.2 | Leadership Demographics and Manifesto Rhetoric | `output/tables/party_leaders_code.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| Table B.3 | IVS Variables | `output/tables/ivsVariables.tex` | `code/analyze/voter_attributes/create_wvs_var_table.R` |

Numbers reported in the text are TeX macros defined in `output/values/`:

| Macro | Description | File | Program |
|---|---|---|---|
| `\IVSObvsNum` | Number of IVS observations | `output/values/values.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| `\capparty` | Manifestos sourced from CAP | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\cmpparty` | Election-party manifestos sourced from CMP | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\cmptext` | CMP observations with externally sourced text | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\cohortAttributeNum` | Cohort-specification voter-attribute contribution | `output/values/modelValues.tex` | `code/analyze/decomposition/plotDecomposition.R` |
| `\countriesNum` | Number of countries | `output/values/values.tex` | `code/analyze/voter_attributes/analyze_desc_stats_wvs.R` |
| `\demOpNum` | Number of voter demographic and opinion variables | `output/values/values.tex` | `code/analyze/voter_attributes/analyze_desc_stats_wvs.R` |
| `\finalparty` | Final party-election observations | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\haveParfamVoteShare` | Vote share retained after party-family classification | `output/values/manifestoValues.tex` | `code/analyze/party_positions/analyze_party_family_positions.R` |
| `\lastWavePrrpScore` | Last-wave change in PRRP score | `output/values/values.tex` | `code/analyze/voter_attributes/analyze_voter_opinion_trend.R` |
| `\leaderDemoImputedAllNum` | Leaders with all used demographics imputed | `output/values/values.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| `\leaderDemoImputedAnyNum` | Leaders with any used demographic imputed | `output/values/values.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| `\leaderDemoImputedOverOneNum` | Leaders with more than one used demographic imputed | `output/values/values.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| `\leaderDemoImputedOverOnePct` | Share of leaders with more than one used demographic imputed | `output/values/values.tex` | `code/analyze/party_positions/create_party_leader_var_table.R` |
| `\matchedClosestPct` | Share matched to the closest election manifesto | `output/values/values.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| `\matchedDropped` | Matched respondents dropped by final sample restrictions | `output/values/values.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| `\matchedPct` | Share of party supporters matched to CMP | `output/values/values.tex` | `code/analyze/voter_attributes/IVS_data_desc_tables.R` |
| `\missingparty` | Manifestos obtained outside CMP | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\numXVariables` | Number of voter attributes used in model | `output/values/modelValues.tex` | `code/model/clean_data_model.R` |
| `\perPartiesAllTime` | Party-characteristics contribution to PRRP support change | `output/values/modelValues.tex` | `code/analyze/decomposition/plotDecomposition.R` |
| `\perPrioritiesAllTime` | Voter-priorities contribution to PRRP support change | `output/values/modelValues.tex` | `code/analyze/decomposition/plotDecomposition.R` |
| `\perResidualAllTime` | Residual contribution to PRRP support change | `output/values/modelValues.tex` | `code/analyze/decomposition/plotDecomposition.R` |
| `\perVoterAllTime` | Voter-attributes contribution to PRRP support change | `output/values/modelValues.tex` | `code/analyze/decomposition/plotDecomposition.R` |
| `\percindex` | Share of manifesto text represented by included positions | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\polidocparty` | Manifestos sourced from Polidoc | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |
| `\prrpSupChangeCor` | Correlation between changes in manifesto and WVS PRRP support | `output/values/values.tex` | `code/analyze/party_positions/analyze_prrp_rise.R` |
| `\prrpSupportCor` | Correlation between manifesto and WVS PRRP support levels | `output/values/values.tex` | `code/analyze/party_positions/analyze_prrp_rise.R` |
| `\trendScoreShareOfDiffUnweighted` | Share of PRRP-score gap explained by opinion trends | `output/values/values.tex` | `code/analyze/voter_attributes/analyze_populist_voters.R` |
| `\vaultparty` | Manifestos sourced from ManifestoVault | `output/values/manifestoValues.tex` | `code/analyze/party_positions/save_party_numbers.R` |

## References

- Haerpfer, C., Inglehart, R., Moreno, A., Welzel, C., Kizilova, K., Diez-Medrano, J., Lagos, M., Norris, P., Ponarin, E., & Puranen, B. (2021). World Values Survey Time-Series (1981-2020) Cross-National Data-Set (Version 2.0) [Dataset]. World Values Survey Association. [Source](https://doi.org/10.14281/18241.15)
- EVS. (2021). EVS Trend File 1981-2017 (Version 2.0.0) [Dataset]. GESIS Data Archive. [Source](https://doi.org/10.4232/1.13736)
- Forschungsgruppe Wahlen, Mannheim. (2025). Politbarometer 1977-2023 (Partielle Kumulation) (Version 16.1.0) [Dataset]. GESIS. [Source](https://doi.org/10.4232/1.14527)
- Döring, H., & Regel, S. (2019). Party Facts: A database of political parties worldwide. Party Politics, 25(2), 97-109. [Source](https://doi.org/10.1177/1354068818820671)
- Volkens, A., Burst, T., Krause, W., Lehmann, P., Matthieß, T., Regel, S., Weßels, B., Zehnter, L., & Wissenschaftszentrum Berlin Für Sozialforschung (WZB). (2021). Manifesto Project Dataset (Version 2021a) [Dataset]. Manifesto Project. [Source](https://doi.org/10.25522/manifesto.mpds.2021a)
- Manifesto Project. Category codebook for dataset version 2021a (codebook_categories_MPDS2021a.csv).
- Sarah Flood, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry (2024). Integrated Public Use Microdata Series, Current Population Survey: Version 12.0 [dataset]. Minneapolis, MN: IPUMS. [Source](https://doi.org/10.18128/D030.V12.0)
- Rooduijn, M. (2023). Replication Data for: "The PopuList: A database of populist, far-left, and far-right parties, using Expert-informed Qualitative Comparative Classification (EiQCC)" (Version 1.0) [Dataset]. Harvard Dataverse. [Source](https://doi.org/10.7910/DVN/WDZZ1V)
- Weeks, A., Meguid, B. M., Kittilson, M. C., & Coffé, H. (2022). Replication Data for: When do Männerparteien Elect Women? Radical Right Populist Parties and Strategic Descriptive Representation (Version 1.0) [Dataset]. Harvard Dataverse. [Source](https://doi.org/10.7910/DVN/SG55BJ)
- Adams, J., Bracken, D., Horne, W., Gidron, N., O'Brien, D. Z., & Senk, K. (2022). Can't We All Just Get Along? How Women MPs Can Ameliorate Affective Polarization in Western Publics (Version 1.0) [Dataset]. Harvard Dataverse. [Source](https://doi.org/10.7910/DVN/AHQRVR)
- Carnes, N., Golden, M., Lupu, N., & Nazrullaeva, E. (2024). Global Legislator Database (Version 1.0) [Dataset]. Harvard Dataverse. [Source](https://doi.org/10.7910/DVN/U1ZNVT)
- Merz, N., Regel, S., & Lewandowski, J. (2016). The Manifesto Corpus: A new resource for research on political parties and quantitative text analysis. Research & Politics, 3(2). [Source](https://doi.org/10.1177/2053168016643346)
- Dolinsky, A. O., Huber, L. M., & Horne, W. (2024). ManifestoVault V1.0: Annotated full-text general election manifestos at the natural-sentence level of three European countries 1970-2021 (Version 1.0) [Dataset]. DataverseNL. [Source](https://doi.org/10.34894/VKQSPO)
- Benoit, K., Bräuninger, T., & Debus, M. (2009). Challenges for estimating policy preferences: Announcing an open access archive of political documents. German Politics, 18(3), 440-453. [Source](https://doi.org/10.1080/09644000903055856)
- Baumgartner, F. R., Breunig, C., & Grossman, E. (Eds.). (2019). Comparative Policy Agendas: Theory, Tools, Data. Oxford University Press.
- Lees, A., Tran, V. Q., Tay, Y., Sorensen, J., Gupta, J., Metzler, D., & Vasserman, L. (2022). A new generation of Perspective API: Efficient multilingual character-level transformers. Proceedings of the 28th ACM SIGKDD Conference on Knowledge Discovery and Data Mining, 3197-3207.
- Stopwords ISO. Stopword lists for multiple languages. [Source](https://github.com/stopwords-iso)
- Bird, S., Klein, E., & Loper, E. (2009). Natural Language Processing with Python. O'Reilly Media. NLTK stopwords corpus: [Source](https://www.nltk.org/nltk_data/)
