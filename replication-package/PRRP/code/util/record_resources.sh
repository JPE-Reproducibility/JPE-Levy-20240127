#!/bin/bash
# Run a command and record the memory and CPU of all its processes every 5 seconds.
# R's parallel workers are re-parented to launchd when they start, so tools that follow the
# process tree (such as psrecord --include-children) miss them. They stay in the command's
# process group, so this samples every process in that group (resident memory summed).
# The run resets output/ when it starts, so the samples go to a temporary file first and are
# copied to output/logs/<run>/resources.txt at the end; the last line gives the peaks.
# Usage: bash code/util/record_resources.sh command [args...]

interval=5
marker=$(mktemp "${TMPDIR:-/tmp}/prrp-run-start.XXXXXX")
samples=$(mktemp "${TMPDIR:-/tmp}/prrp-resources.XXXXXX")
pgid=$(ps -o pgid= -p $$ | tr -d ' ')
start=$(date +%s)

sample() {
  printf '# elapsed_s rss_MiB cpu_pct processes (process group %s, every %s s)\n' "$pgid" "$interval"
  while kill -0 $$ 2>/dev/null; do
    ps -A -o pgid=,rss=,%cpu= | awk -v g="$pgid" -v t=$(( $(date +%s) - start )) \
      '$1 == g {rss += $2; cpu += $3; n++} END {printf "%d %.0f %.1f %d\n", t, rss / 1024, cpu, n}'
    sleep "$interval"
  done
}

finish() {
  { kill "$sampler" && wait "$sampler"; } 2>/dev/null
  awk '!/^#/ {t = $1; if ($2 > m) m = $2; if ($3 > c) c = $3}
       END {printf "# Elapsed %.2f h; peak sampled memory %.2f GiB; peak sampled CPU %.0f%%\n", t / 3600, m / 1024, c}' \
    "$samples" | tee -a "$samples"
  run_dir=$(find output/logs -mindepth 1 -maxdepth 1 -type d -newer "$marker" 2>/dev/null | sort | tail -1)
  if [ -n "$run_dir" ] && cp "$samples" "$run_dir/resources.txt"; then
    echo "Resource samples: $run_dir/resources.txt"
  else
    echo "Resource samples: $samples"
  fi
  rm -f "$marker"
}

sample > "$samples" &
sampler=$!
trap finish EXIT
"$@"
status=$?
exit $status
