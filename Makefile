################################################################################
# Makefile for PNA_HS_closure
################################################################################
# 
# Rebuilds the project pipeline:
#   data/raw          -->  data/processed   (scripts/01_processing)
#   data/processed    -->  content/tab, content/img  (scripts/02_analysis)
#   data/processed    -->  content/img      (scripts/03_content)
# 
# Run from the project root. All scripts locate files via here::here(), so no
# `cd` into the script directory is needed.
# 
# Usage:
#   make                # build everything (processing, analysis, content)
#   make processing     # (re)build processed data / panels only
#   make analysis       # (re)build regression tables & figures only
#   make content        # (re)build manuscript maps / figures only
#   make h1 / h2 / h3    # (re)build outputs for a single hypothesis
#   make clean           # remove generated tables and figures
# 
################################################################################

RSCRIPT := Rscript

.PHONY: all processing analysis content clean \
        h1 h2 h2_robustness h3 h3_robustness \
        content_maps content_ps_catch

all: processing analysis content

# 1) PROCESSING #################################################################
# data/raw --> data/processed
processing: data/processed/PNA_high_seas_pockets.gpkg \
            data/processed/WCPFC_convention_area.gpkg \
            data/processed/wcpfc_ps_annual.rds \
            data/processed/wcpfc_ll_annual.rds \
            data/processed/h1_panel.rds \
            data/processed/h2_panel.rds \
            data/processed/h3_ps_panel.rds \
            data/processed/h3_ll_panel.rds

data/processed/PNA_high_seas_pockets.gpkg data/processed/PNA_eezs.gpkg: scripts/01_processing/01_make_PNA_hs_pocket.R
	$(RSCRIPT) $<

data/processed/WCPFC_convention_area.gpkg: scripts/01_processing/02_build_WCPFC_convention_area.R
	$(RSCRIPT) $<

data/processed/wcpfc_ps_annual.rds: scripts/01_processing/03_clean_wcpfc_ps_data.R
	$(RSCRIPT) $<

data/processed/wcpfc_ll_annual.rds: scripts/01_processing/04_clean_wcpfc_ll_data.R
	$(RSCRIPT) $<

data/processed/h1_panel.rds: scripts/01_processing/05_build_h1_panel.R \
                              data/processed/wcpfc_ps_annual.rds
	$(RSCRIPT) $<

data/processed/h2_panel.rds: scripts/01_processing/06_build_h2_panel.R \
                              data/processed/wcpfc_ll_annual.rds
	$(RSCRIPT) $<

data/processed/h3_ps_panel.rds data/processed/h3_ll_panel.rds: scripts/01_processing/07_build_h3_panels.R \
                              data/processed/PNA_high_seas_pockets.gpkg \
                              data/processed/PNA_eezs.gpkg \
                              data/processed/wcpfc_ps_annual.rds \
                              data/processed/wcpfc_ll_annual.rds
	$(RSCRIPT) $<

# 2) ANALYSIS ####################################################################
# data/processed --> content/tab, content/img (regression tables & figures)
#
# Each script below writes several tables and/or figures at once, so targets
# are declared .PHONY and simply re-run their script whenever invoked, rather
# than tracking every individual output file.
analysis: h1 h2 h2_robustness h3 h3_robustness

h1: data/processed/h1_panel.rds
	$(RSCRIPT) scripts/02_analysis/01_estimate_h1.R

h2: data/processed/h2_panel.rds
	$(RSCRIPT) scripts/02_analysis/02_estimate_h2.R

h2_robustness: data/processed/h2_panel.rds
	$(RSCRIPT) scripts/02_analysis/02_estimate_h2_robustness_by_pocket.R

h3: data/processed/h3_ps_panel.rds data/processed/h3_ll_panel.rds
	$(RSCRIPT) scripts/02_analysis/03_estimate_h3.R

h3_robustness: data/processed/h3_ps_panel.rds
	$(RSCRIPT) scripts/02_analysis/03_estimate_h3_robustness_by_pocket.R

# 3) CONTENT #####################################################################
# data/processed --> content/img (manuscript maps & figures)
content: content_maps content_ps_catch

content_maps: data/processed/PNA_eezs.gpkg data/processed/WCPFC_convention_area.gpkg \
              data/processed/PNA_high_seas_pockets.gpkg \
              data/processed/h1_panel.rds data/processed/h2_panel.rds \
              data/processed/h3_ps_panel.rds data/processed/h3_ll_panel.rds
	$(RSCRIPT) scripts/03_content/01_make_HS_pocket_map.R

content_ps_catch: data/processed/h1_panel.rds
	$(RSCRIPT) scripts/03_content/03_fig_ps_catch_inside.R

# CLEAN ##########################################################################
clean:
	rm -f content/tab/*.tex content/img/*.png content/summaries/*.tex

dag:
	make -Bnd | make2graph | dot -Tpng -o dag.png