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

## Workflow
TBA

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

NCEAS Scientific Computing Support Team page [link](https://lter.github.io/scicomp/)
