.PHONY: help setup fresh all paper
.NOTPARALLEL:

# POPULIST_SYSTEM_R=1 runs every target in your own R 4.6.1 instead of the bundled
# Apple Silicon runtime; set RSCRIPT to choose a specific Rscript (see README).
ifeq ($(POPULIST_SYSTEM_R),1)
RSCRIPT ?= Rscript
else
RSCRIPT ?= bash offline/Rscript
endif

help:
	@printf '%s\n' \
	  'make setup      Install the bundled R runtime and packages (POPULIST_SYSTEM_R=1: restore renv.lock instead)' \
	  'make fresh      Re-estimate from data/cleandata, fit new draws, and regenerate exhibits' \
	  'make all        Rebuild from data/rawdata through paper exhibits' \
	  'make paper      Compile the paper from existing exhibits'

setup:
ifeq ($(POPULIST_SYSTEM_R),1)
	mkdir -p offline/empty-library
	$(RSCRIPT) --vanilla code/util/setup_system_r.R
else
	bash offline/setup.sh
endif

fresh:
	POPULIST_RESET_OUTPUT=1 POPULIST_PROCESS_DATA=0 POPULIST_RUN_MODEL=1 POPULIST_RUN_BS=1 \
	  POPULIST_RUN_ANALYSIS=1 POPULIST_RUN_COMPILE=0 $(RSCRIPT) --vanilla code/masterFile.R

all:
	POPULIST_RESET_OUTPUT=1 POPULIST_PROCESS_DATA=1 POPULIST_RUN_MODEL=1 POPULIST_RUN_BS=1 \
	  POPULIST_RUN_ANALYSIS=1 POPULIST_RUN_COMPILE=0 bash code/util/record_resources.sh \
	  $(RSCRIPT) --vanilla code/masterFile.R

paper:
	POPULIST_RESET_OUTPUT=0 POPULIST_PROCESS_DATA=0 POPULIST_RUN_MODEL=0 POPULIST_RUN_BS=0 \
	  POPULIST_RUN_ANALYSIS=0 POPULIST_RUN_COMPILE=1 $(RSCRIPT) --vanilla code/masterFile.R
