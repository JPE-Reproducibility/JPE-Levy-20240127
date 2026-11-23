# README Review (LLM)

_Generated 2026-11-23T22:00:18.687, reviewed `/Users/floswald/Dropbox/Apps/JPE-packages/JPE/Levy-20240127/1/repo/replication-package/PRRP/README.md`_

# README Review: "Decomposing the Rise of the Populist Radical Right" (PRRP Replication Package)

**Verdict up front: Needs major revision.** The README is unusually thorough and well-organized — far above the median JPE submission — but one licensing/redistribution issue is a likely blocker, and I was unable to verify that the package's `data/` and `documentation/` contents actually exist on disk as claimed (see note on verification limits below).

**Note on method:** I reviewed the README text against the actual package files where my tools allowed (I can read individual files but cannot list directories or run `find`/`ls`). I confirmed `LICENSE`, `Makefile`, `renv.lock`, `code/masterFile.R`, `code/util/config.R`, `code/util/commonVars.R`, `code/util/write_csv_copies.R`, `code/model/clean_data_model.R`, several exhibit-mapping programs, and `offline/R-4.6.1-arm64-runtime.tar.gz` all exist and are consistent with the README's claims. However, **every file I tried to read under `data/` (7 of 7 — spanning `not_share/`, `public/`, `authorcreated/`, and `cleandata/`) and `documentation/input-inventory.csv` came back "does not exist."** This is not interpretable from my side as evidence either way: the same absence pattern applies both to a file that should be excluded (`not_share/WVS/...`) and to files that should be included (`public/IPUMS/cps_00047.dta`, `cleandata/WVS/wvs_clean_4to6_waves.dta`), which means it's most likely a local access/sync artifact in this review environment rather than a real gap in the submitted package. **The data editor must independently run a full file listing (`find data documentation -type f`) against the actual submitted archive before certifying §3's "Included" claims, the `make fresh` starting point, and the input-inventory deferral.** I flag this as an open verification item, not a confirmed defect.

---

## 1. Title and Overview — **Yes**

> "Decomposing the Rise of the Populist Radical Right" — Oren Danieli, Noam Gidron, Shinnosuke Kikuchi, and Ro'ee Levy.

The "Quick start" section (lines 5–40) does the job of an overview: it states the package reproduces every table/figure/reported number, distinguishes the two run paths (`make all` with raw data vs. `make fresh` from prepared data), states hardware/time requirements, and gives copy-pasteable commands. No issues here — this is close to best practice.

## 2. Data Availability and Provenance — **Partially**

- External data statement: implied throughout but the canonical sentence ("This paper analyzes external data") is never stated verbatim. Minor — add it for data-editor checklist matching.
- Rights statement: present (lines 187–188), using the exact certifying language, pointing to `LICENSE`.
- Data license: stated (line 192) — MIT for code, CC BY 4.0 for author-created data, third-party terms otherwise.
- Public availability categorization: implied ("Some raw survey and manifesto files must be obtained from their providers") but the explicit categorical sentence ("Some data cannot be made publicly available due to licensing restrictions") is missing.

**Issue:** Rights checkbox #2 states "the author(s) have documented permission to redistribute/publish the data contained within this replication package. Appropriate permissions are documented in the LICENSE file." But `LICENSE` item 2 hedges this ("licensed under CC BY 4.0 **to the extent of the authors' rights in them**") and item 3 explicitly subjects third-party content embedded in those same files back to original source terms. This is not documentation of permission — it's an acknowledgment that permission is uncertain. See §3 below for why this matters concretely.

**Suggestion:** Add the canonical external-data and availability-category sentences. More importantly, resolve the rights-checkbox/LICENSE-hedge contradiction (§3).

## 3. Summary and Details of Data Sources — **Partially — contains the package's most serious issue**

The data availability table (lines 137–166) is a genuine strength: 27 rows, clear Source/Version/Path/How-to-obtain columns, provider-specific registration instructions, and per-source terms/license prose (lines 168–183). This is better documented than most JPE submissions.

**Blocking issue — derived individual-level microdata redistributed under a license the authors don't fully own:**

`data/cleandata/` is declared **Included** and licensed as "data created by the authors" under CC BY 4.0 (LICENSE item 2, README line 192). But tracing the code shows this directory contains **respondent-level survey microdata**, not an aggregate:

- `code/util/commonVars.R:37`: `pathWVS_Data = data/cleandata/WVS/wvs_clean_4to6_waves.dta`
- `code/model/clean_data_model.R:35-56` loads this as "X data (voter demographics and opinions)," with per-respondent columns (`X_dem_age`, `X_dem_cohort`, `X_op_*`)
- The exhibit-mapping table even defines a macro `\IVSObvsNum` = "Number of IVS observations" (line 284) — a respondent-level N

This is a derived extract of WVS Time-Series and EVS Trend File microdata — exactly the sources the README itself says require individual registration and "Conditions of use" / GESIS terms acceptance to access (lines 170–172). The LICENSE's hedge ("to the extent of the authors' rights in them") is the tell: the authors likely do **not** have a documented right to redistribute derived respondent-level WVS/EVS extracts, only to use them for their own analysis. The rights checkbox's claim of "documented permission to redistribute" is not actually backed by any WVS/GESIS permission letter or clause cited anywhere in the package.

**Actionable fix (pick one):**
(a) Obtain and cite written permission from WVS/GESIS (and Manifesto Project, which also restricts the raw `.dta`) explicitly covering redistribution of the derived `cleandata` extract, and document it in the README/LICENSE; or
(b) If the data editor's own data-use agreement with WVS/GESIS permits derived-data sharing, state that basis explicitly; or
(c) If this is not resolvable, remove `data/cleandata/` from the public package, ship `make all` only (requiring the replicator's own licensed raw data), and add a smaller illustrative/de-identified or aggregated sample so `make fresh` still runs on *something* non-restricted.

This is the one finding that should block sign-off until the authors or the data editor resolve it — everything else below is fixable in an afternoon.

**Other issues in this section:**
- The table has Source / Version / Path / How-to-obtain but no explicit **Format**, **Access method**, or **License** column per row — license/access terms are only in prose for a subset of sources (lines 168–183), and several "Included" rows (stopwords, author-created crosswatch files) have no explicit per-row license.
- **Google Perspective API** — `insult_scores_google.rds` contains Perspective-derived scores (line 153); Lees et al. (2022) is cited in References, but Perspective API's own terms of use for redistributing derived scores are never addressed.
- No **data preservation commitment** (e.g., "the authors will preserve the raw survey extracts for N years").
- No explicit statement that pseudo-data/sample data "successfully run the code" (not applicable here since real — if restricted — data is shipped, but worth confirming `make fresh` was tested from a clean checkout, see §6).
- `documentation/input-inventory.csv` is referenced as authoritative for "additional source documentation" (line 135) but could not be verified to exist — see verification note above.

## 4. Computational Requirements — **Partially**

Strong on specificity: R 4.6.1 pinned via `renv.lock` (verified — matches), Stata version (StataNow 19.5), TeX Live 2023, bundled offline runtime for Apple Silicon (tarball verified present), `make setup`/`POPULIST_SYSTEM_R=1` fallback for other platforms, measured runtime/memory from an actual dated run (Nov 16–17, 2026), seeds documented (`estimate_wave.R`, `bootstrap.R`).

**Issues:**
- **Disk space is inconsistent and not split by run type.** Quick start (line 17) states "about 5 GB of free disk space," but §4.3 (line 129) measures the completed workspace at **15.4 GiB**, of which 10.37 GiB is `data/intdata/` (built only by `make all`). The checklist asks for storage stated separately for the restricted-data run (`make all`) vs. the open run (`make fresh`); neither the 5 GB headline figure nor the 15.4 GiB breakdown is labeled by which run it corresponds to. Fix: state two numbers explicitly, e.g. "`make fresh`: ~X GB; `make all`: ~15.4 GiB including 10.37 GiB of intermediate data."
- **RAM/time table for fewer workers (lines 82–89) is extrapolated, not measured** — the authors say so themselves ("did not rerun them with fewer [workers]"). This is honest but should be flagged to the replicator more prominently (e.g., "estimated, not measured" in the table header) since a replicator with a smaller machine is the one most likely to rely on it.
- **Resource recording is asymmetric across targets.** `Makefile:27-34` shows `make all` is wrapped in `record_resources.sh` (producing `output/logs/<run>/resources.txt`), but `make fresh` is not. This means a replicator running only `make fresh` (the no-raw-data, no-Stata path — the one most replicators without licenses will actually use) has no way to verify their own run's memory against the README's reported peak. Worth a line noting this, or extending resource recording to `make fresh` too.
- **Untested platform risk.** The README candidly states the system-R path "has not been tested on other platforms" (line 65), yet the TeX default path is `universal-darwin`-only and the bundled runtime is Apple-Silicon-only. Combined with a 27-hour / 64 GB run, this is the most likely practical cause of a failed verification by a data editor who isn't on an Apple Silicon Mac. Recommend asking the authors to verify the `POPULIST_SYSTEM_R=1` path at least once on Linux before submission, or flag this clearly as a known gap for the editor's own replication environment planning.
- Paths generally look relative-to-root and forward-slash (consistent with the OS-agnostic best practice), but one shipped filename contains a space and mixed case: `data/rawdata/downloaded/public/PopuList/The PopuList 3.0.csv` — functional but worth normalizing (e.g., `populist_3_0.csv`) to avoid shell-quoting pitfalls on replicators' systems.

## 5. Description of Programs — **Partially**

The programs table (lines 217–224) gives directory-level purpose statements, which is adequate but minimal. I verified `code/masterFile.R` exists and is the common entry point invoked by every Makefile target (`setup` excepted) with different `POPULIST_*` environment flags controlling which stages run (`Makefile:27-38`) — this mechanism is accurate but **never explained in the README text**; a replicator reading only the programs table can't tell that `make fresh/all/paper` are just `masterFile.R` with different flags, or that `code/util/config.R` is where `PRECISE_ESTIMATION`, worker counts, and `TEX_BIN`/`STATA_PATH` defaults actually live (I confirmed `PRECISE_ESTIMATION = 0.5` there, matching the "precision 0.5" claim at line 95 — good internal consistency).

Code license is explicitly stated (MIT, per LICENSE item 1) and matches the README.

**Suggestion:** Add one or two sentences explaining that all four Make targets dispatch to `masterFile.R` via environment-variable flags, and that `code/util/config.R` is the single place controlling precision, worker count, and tool paths — this would materially help a replicator debugging a failed run.

## 6. Instructions to Replicators — **Yes, with one gap**

Exceptionally concrete: exact commands for both platforms, explicit environment variables (`STATA_PATH`, `TEX_BIN`, `POPULIST_WORKERS`, `POPULIST_SYSTEM_R`), a clear statement of what each run clears/writes, and a definitive success signal (`output/logs/<run-id>/SUCCESS.txt`).

**Gap:** no statement that the authors (or the data editor) verified these instructions end-to-end in a genuinely clean environment immediately before submission. The dated Nov 16–17, 2026 run (line 117) is good evidence of *a* successful run, but it's not explicitly tied to "we ran this from a fresh clone of the exact submitted package." Add one sentence confirming this.

## 7. List of Tables and Programs — **Yes**

This is the strongest section. The exhibit mapping (lines 226–312) covers every figure and table in the main paper and both appendices, plus every text-reported numeric macro, each mapped to an exact output file and generating program. I spot-checked several programs referenced here (`reserviors.R`, `GG2.R`, `code/documents/input_common/model_flow.tex`) and all exist with content consistent with their stated role. This is exactly the level of granularity the checklist asks for and exceeds typical submissions.

## 8. References — **Partially**

Comprehensive — every dataset (including CC0, proprietary, and author-constructed ones) is cited with DOIs. Two small gaps:
- Line 321 (Manifesto category codebook) lacks a year and DOI/URL, inconsistent with every other entry's format.
- R, Stata, and key R packages (`haven`, `data.table`, `randomForest`, etc., per `renv.lock`) are not cited as software, nor is LaTeX/biber. Minor by JPE standards but worth adding for completeness.

## 9. Acknowledgements — **No**

No Acknowledgements section exists. The README's structure (Data Availability Statement → Statement about Rights → License → Description of Programs → Exhibit Mapping → References) closely mirrors the Social Science Data Editors' README template. If it was adapted from that template (likely, given the structural match), it should be acknowledged per that template's own terms.

---

## Final Assessment

**Verdict: Needs major revision**, pending resolution of one licensing finding, plus one mandatory verification step the editor must perform.

### Prioritized fixes

1. **(Blocking) Resolve the `data/cleandata/` redistribution rights issue.** The CC BY 4.0 license on author-created data covers a directory that contains derived individual-level WVS/EVS survey microdata. The LICENSE's own hedge ("to the extent of the authors' rights in them") contradicts the rights checkbox's claim of "documented permission to redistribute." Authors must supply actual permission documentation from WVS/GESIS/Manifesto Project covering the derived extract, or restructure the package so restricted-derived data isn't shipped under a license implying free redistribution.
2. **(Pre-condition for everything else) Verify package completeness on disk.** I could not confirm that `data/` (all four subtrees) and `documentation/input-inventory.csv` are actually present and populated in the submitted archive — every path I tried under those directories returned "not found" in this review environment, which may be a local access artifact rather than a real gap, but it must be independently confirmed with a full directory listing before certifying §3.
3. Fix the disk-space contradiction (5 GB headline vs. 15.4 GiB measured) by splitting figures explicitly by `make fresh` vs. `make all`.
4. Add a data-preservation commitment statement.
5. Add an Acknowledgements section (template provenance).
6. Add the canonical "analyzes external data" / public-availability-category sentences for checklist matching.
7. Address Google Perspective API redistribution terms for the derived insult scores.
8. Note the untested non-Apple-Silicon path more prominently, and consider asking authors to verify it once before submission.
9. Minor: fix the line-321 reference format, note resource-recording is `make all`-only, flag the extrapolated worker/RAM table as "estimated, not measured," and explain the Makefile→`masterFile.R` dispatch mechanism in §5.

### General observation

Apart from the licensing question, this is one of the more rigorous and specific READMEs I've reviewed against this checklist — the exhibit mapping, data availability table, and instructions sections are genuinely exemplary and should be used as a positive example for other submissions. The fixes above are almost all additive text; only item 1 requires a substantive decision from the authors.
