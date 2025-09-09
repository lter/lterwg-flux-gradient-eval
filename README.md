# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems


## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University

## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Repository Structure

```
lterwg-flux-gradient-eval/
├── functions/               # Core analysis functions
│   └── calc.*.R             # Calculation functions
│   
├── workflows/               # Complete analysis workflows
│   └── flow.*.R             # Analysis workflows
│
├── exploratory/             # Preliminary analyses and development
│   └── flow.evaluation.*.R
│
└── aop/             # Preparation of AOP, canopy diversity, and caopy information data
    ├── flow.NEONAOP.EVI.Download.R
    ├── flow_AOP_FormatLayers.R
    ├── flow.AOP.viz.R
    └── flow.StructuralDiversity.R 
```

## Data Frame Organization
- Column name should use snake case include units, last "_" proceeds units (i.e. var_molperm3)
- no "- or /" in column names

## Getting Started

1. Clone this repository
   ```bash
   git clone https://github.com/lter/lterwg-flux-gradient.git
   cd lterwg-flux-gradient
   ```

2. Install required R packages
   ```r
   # Core packages
   install.packages(c("tidyverse", "neonUtilities", "rhdf5", "googledrive", 
                     "foreach", "doParallel", "lubridate", "ggplot2"))
   
   # Additional packages
   install.packages(c("gslnls", "terra", "sf", "ggh4x"))
   
   # If using BiocManager
   if (!requireNamespace("BiocManager", quietly = TRUE))
       install.packages("BiocManager")
   BiocManager::install("rhdf5")
   ```

## Evaluation Workflow
1. `flow.evaluation.batch` → Creates the data needed to evaluate gradient fluxes using the products of `lterwg-flux-gradient/workflows/flow.evaluation.dataframe.R`.
First data is filtered (`flow.evaluation.filter.R`) to produce: FilteredData_ALLSites.Rdata and FilterReport_ALLSites.Rdata. Next, the One2One analysis (`flow.evaluation.One2One.R`) is done on filtered data to produce: One2One_ALLSites.Rdata and FilteredData_ALLSites_BH.Rdata. BH stands for the best height, which is determined by the height levels with the highest R2. Next the diurnal analysis (`flow.evaluation.diurnal.R`) produces: DiurnalSummary_ALLSites_BH.Rdata. Finally, we fit carbon exchange parameters (`flow.evaluation.cparms.R`) to produce: CarbonParms.Rdata. 

2. `flow.evaluation.figures` → produces the figures and tables that evaluate the gradient flux. The products of `flow.evaluation.batch` are used. 

   ```
   SITE_aligned_conc_flux_9min.RData → flow.calc.flux.batch.R 
                                     ├─ flow.calc.flag.mbr.batch.R
                                     ├─ flow.calc.flag.aero.batch.R 
                                     └─ flow.calc.flag.windprof.batch.R
                                     → SITE_METHOD.RData
   
   SITE_METHOD.RData → lterwg-flux-gradient/workflows/flow.evaluation.dataframe.R → SITE_Evaluation.RData
   
   SITE_Evaluation.RData → flow.evaluation.batch
                         ├─ flow.evaluation.filter.R → FilteredData_ALLSites.Rdata, FilterReport_ALLSites.Rdata
                         ├─ flow.evaluation.One2One.R → One2One_ALLSites.Rdata, FilteredData_ALLSites_BH.Rdata
                         ├─ flow.evaluation.diurnal.R → DiurnalSummary_ALLSites_BH.Rdata
                         └─ flow.evaluation.cparms.R → CarbonParms.Rdata
   
   FilteredData_ALLSites.Rdata, etc. → flow.evaluation.figures → Figures and tables
   ```

## AOP Workflow

1. `flow.NEONAOP.Download.R` → Downloads and mosaics NEON AOP Data. Data is stored locally on the Malone Lab server.

2. `flow.neon.site.squarebuffers.R` → Creates square buffers for LTER-NEON co-located sites using this file: Ameriflux_NEON field-sites.csv to produce: NEONLTERsiteBuffers.Rdata.

3. `flow.neon.site.simplefeatures.R` → This script uses the NEONLTERsiteBuffers.Rdata and breaks each buffer into wedges to produce: FG_Site_Wdges.RDATA.

4. `flow_AOP_FormatLayers` → Extracts AOP information for each wedge to produce a summary file.

   ```
   NEON AOP Data → flow.NEONAOP.Download.R → Downloaded AOP data 
   
   Ameriflux_NEON field-sites.csv → flow.neon.site.squarebuffers.R → NEONLTERsiteBuffers.Rdata
   
   NEONLTERsiteBuffers.Rdata → flow.neon.site.simplefeatures.R → FG_Site_Wdges.RDATA
   
   FG_Site_Wdges.RDATA + Downloaded AOP data → flow_AOP_FormatLayers → Summary files
   ```
   
## Function Folder
- Use hierarchical naming with the active verb first, i.e. "flag.iqr.R"
- This is where all functions called by flow. scripts in Workflow Folder are stored

## Exploratory Folder
- Wild West, this is where preliminary functions and workflows are stored

## Related Repositories

- [lterwg-flux-gradient](https://github.com/lter/lterwg-flux-gradient): Main repo, contains code for filtering
- [lterwg-flux-gradient-methane](https://github.com/lter/lterwg-flux-gradient-methane): Code for the methane paper

## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)
