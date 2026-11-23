### Data Dependency Audit

_Generated on 2026-11-23T21:25:47.673_

**Warning**: this audit is a static string search (data file name as a word-bounded
token in the code text), not a trace of actual data loading. It cannot catch filenames
built dynamically at runtime, or data fetched from a URL rather than a local path. Treat
it as a lead for manual review, not as ground truth.


| Data files | Referenced by code | Orphans (no code reference found) |
|------------|---------------------|-------------------------------------|
| 294 | 83 | 211 |

#### Detail

**/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/data/cleandata/Germany/politbarometer.rds**

- referenced by `/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/code/analyze/case_studies/GG2.R`
- referenced by `/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/code/process/analysis_inputs/prepare_supporting_data.R`

> **Note**: This list contains 1483 entries (0.1 MB) — too large to embed in the report.
> The full list is available here: <https://www.dropbox.com/scl/fo/1l6em1pdoidn3cxm38jnc/AC_wFsr6p2kIfuq_BNAC5M8?rlkey=tx7lxyv0lx1ssc2905bafoees&dl=0&preview=report-data-dependencies.md>
