## Potentially Hardcoded Numeric Constants


We found the following set of hard coded numbers. This may be completely legitimate (parameter input, thresholds for computations, etc), and is hence only for information.

**/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/code/analyze/decomposition/smallPartyResidual.R**

- Line 250, : binwidth = 0.015, color = "black", alpha = 1, position = "identity") +  # Red bars at the back
- Line 252, : binwidth = 0.015, color = "black", alpha = 1, position = "identity") +  # Blue bars in front
- Line 266, : geom_histogram(binwidth = 0.015, fill = "lightcoral", color = "black") +

**/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/offline/stata/g/gcollapse.ado**

- Line 1664, : return local st_time = max(`total_time', 0.001) * scalar(__gtools_gc_k_extra) * `factor'

**/Users/florianoswald/actions-runner/_work/JPE-Levy-20240127/JPE-Levy-20240127/replication-package/PRRP/code/analyze/decomposition/functions_for_plotDecomposition.R**

- Line 66, : prepareDecompositionPlot <- function (DATA, adjustHeight = 0.025) {
- Line 130, : scale_y_continuous(limits = c(-0.075, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0,0), labels = scales::percent) +


> **Note**: This list contains 39 entries (0.0 MB) — too large to embed in the report.
> The full list is available here: <https://www.dropbox.com/scl/fo/1l6em1pdoidn3cxm38jnc/AC_wFsr6p2kIfuq_BNAC5M8?rlkey=tx7lxyv0lx1ssc2905bafoees&dl=0&preview=report-hardcoded-numbers.md>
