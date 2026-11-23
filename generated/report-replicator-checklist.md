# Replicator Checklist

_Generated 2026-11-23T22:02:35.521 -- a deterministic summary of what the automated checks flagged. This doesn't replace reading the full reports in `generated/`; it's a starting point for where to look first._

## Crosscheck: claims vs. package (Stage D)

Coverage: 85.7% (56 claims checked).

Claims worth a closer look:

- **[Missing]** data_source `Ipsos 2009 aggregate polling (UKIP/BNP)`: The file listings show no Ipsos data and no script that references it. It is probably a text-only external citation.
- **[Plausible]** table `Table A.1 (tab:prrp_parties)`: rrp_parties.R is plausibly the script that lists PRRPs. manifesto_populist.rds is available. Stage A shows no direct trace for rrp_parties.R.
- **[Plausible]** table `Table A.3 (tab:decomposition_table)`: The model scripts cover estimation, decomposition and bootstrap: estimate_all.R, estimate_wave.R, decompose_delta.R, bootstrap.R and create_inference_table.R. No Stage A trace shows the table being produced.
- **[Plausible]** table `Table B.3 (tab:ivs_vars)`: create_wvs_var_table.R plausibly builds it. The inputs wvs_variables.rds and variable_list.csv are listed, but Stage A shows no direct trace in the portion I read.
- **[Plausible]** figure `Figure 4 (fig:decomposition)`: plotDecomposition.R, functions_for_plotDecomposition.R and decompose_delta.R exist. No Stage A trace was seen.
- **[Plausible]** figure `Figure 5 (fig:decomposition_robustness)`: DecompositionRobustnessGraph.R and 'permutate order.R' (Shorrocks-Shapley orders) exist. The leave-country-out scripts are leave_country_out.R, LCO_master.R, decomposition_lco.R and run_loo.R. The pledge and rhetoric variants have their own scripts.
- **[Plausible]** figure `Figure 7 (fig:heat_map)`: waves1to6_heatmap.R references manifesto data and is likely the heatmap script. Whether the 2005–2020 version is in the same script is not visible.
- **[Plausible]** figure `Figure A.4 (fig:phi_all)`: plot_phi_with_new_Zs.R is a plausibly named script, and Phi comes from the estimation scripts. Stage A shows no direct trace.
- **[Plausible]** figure `Figure A.5 (fig:decomposition_robustness_app)`: DecompositionRobustnessGraph.R is plausible. The wvs_clean_4to6 variants (with_CZSK, no_dem) and manifesto_parties_withCZSK.rds support the CZSK and no-demographics variants.
- **[Plausible]** figure `Figure A.7 (fig:alternative_indices)`: alternative_indices.R and the weight plotting scripts plausibly cover the Hawk-Dove, European integration and government efficiency indices.
- **[Plausible]** figure `Figure A.13 (fig:change_salience)`: analyze_party_family_positions.R or analyze_prrp_rise.R plausibly produce the salience time series from manifesto.csv. Stage A shows no figure-specific trace.
- **[Plausible]** figure `Figure A.14 (fig:corr_score_support)`: It needs the PRRP score (analyze_voter_opinion_trend.R, prrp_score.rds) and the vote shares in manifesto data. analyze_voter_opinion_trend.R or analyze_populist_voters.R is the likely source.
- **[Plausible]** figure `Figure A.16 (fig:opinions_change)`: analyze_voter_opinion_trend.R is plausibly the script for opinion trends. Stage A shows no figure-specific trace.
- **[Plausible]** figure `Figure A.21 (fig:dec_new_entry)`: smallPartyResidual.R and plotDecomposition.R are plausible sources for the new-entrant residual split. Stage A shows no trace.
- **[Plausible]** figure `Figure A.24 (fig:new_party_residual)`: smallPartyResidual.R and the decomposition scripts are plausibly named for the new-party residual counterfactual. Stage A shows no trace.

Code files with no claim connected to them (worth a "what does this actually do" check):

- `offline/stata/_/_gtools_internal.ado`
- `offline/stata/d/distinct.ado`
- `offline/stata/f/fasterxtile.ado`
- `offline/stata/g/gcollapse.ado`
- `offline/stata/g/gcontract.ado`
- `offline/stata/g/gdistinct.ado`
- `offline/stata/g/gduplicates.ado`
- `offline/stata/g/gegen.ado`
- `offline/stata/g/gglm.ado`
- `offline/stata/g/gisid.ado`
- `offline/stata/g/givregress.ado`
- `offline/stata/g/glevelsof.ado`
- `offline/stata/g/gpoisson.ado`
- `offline/stata/g/gquantiles.ado`
- `offline/stata/g/greg.ado`
- `offline/stata/g/gregress.ado`
- `offline/stata/g/greshape.ado`
- `offline/stata/g/gstats.ado`
- `offline/stata/g/gtools.ado`
- `offline/stata/g/gtop.ado`
- `offline/stata/g/gtoplevelsof.ado`
- `offline/stata/g/gunique.ado`
- `offline/stata/h/hashsort.ado`
- `offline/stata/u/unique.ado`
- `offline/setup.R`
- `offline/setup.sh`
- `renv/activate.R`
- `code/util/record_resources.sh`
- `code/util/setup_system_r.R`
- `code/util/config.R`
- `code/util/log.R`
- `code/util/write_csv_copies.R`
- `code/util/create_folders.R`
- `code/process/paths.R`
- `code/process/stata.R`
- `code/process/utils.do`
- `code/documents/compilePaper.R`
- `code/documents/input_common/model_flow.tex`
- `code/model/bootstrap_checks.R`
- `Makefile`


## README structure (Stage B)

The structural checklist flagged these required sections as missing:

- Reproduction instructions: not found


## README review (Stage R)

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

## Referee review (refine.ink)

_(not available -- referee review via refine.ink: processing)_
