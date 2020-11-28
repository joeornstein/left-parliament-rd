# Define variables
FIGURES := $(wildcard paper/figures/*)

# Target
all : $(FIGURES)

# Build figures
$(FIGURES) : R/IntRateCostSocDem_analysis.R data/IntRateCostSocDem_dataset_v2_1.csv
	Rscript R/IntRateCostSocDem_analysis.R

# Merge and clean up dataset
data/IntRateCostSocDem_dataset_v2_1.csv : R/IntRateCostSocDem_preprocessing.R
	Rscript R/IntRateCostSocDem_preprocessing.R
