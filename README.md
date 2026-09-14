# PIMMA

Reproducibility materials for the manuscript:

**Post-selection Inference in Multiverse Meta-Analysis: The PIMMA Framework**

## Repository structure

The main reproducibility materials are organized as follows:

```text
application/cbt-depression/
├── 01_preprocessing.R
├── 02-multiverse.R
├── data/
│   └── data-depression-psyctr-2022.xlsx
└── results/
    ├── cbt-dep-clean.rds
    └── cbt-dep-multi.rds

simulation/
├── sim.R
├── utils.R
└── res-clean.rds

paper/
├── pimma.qmd
├── supplementary.qmd
├── pimma.bib
└── references.bib

renv.lock
```

## Software environment

Package versions used for the analyses are recorded in `renv.lock`.

From the repository root, restore the R environment with:

```r
install.packages("renv")
renv::restore()
```

## Empirical application

The raw dataset is stored at:

```text
application/cbt-depression/data/data-depression-psyctr-2022.xlsx
```

From the repository root, reproduce the empirical application by running:

```r
source("application/cbt-depression/01_preprocessing.R")
source("application/cbt-depression/02-multiverse.R")
```

These scripts generate the processed dataset and multiverse-analysis results in:

```text
application/cbt-depression/results/
```

## Simulation study

Precomputed simulation results used in the manuscript are included in:

```text
simulation/res-clean.rds
```

The complete simulation can be reproduced from the repository root with:

```bash
Rscript simulation/sim.R
```

The full simulation uses 5,000 replications per condition and is computationally intensive.

When `simulation/sim.R` is sourced interactively in R or RStudio, it uses a reduced number of replications for testing purposes. Use `Rscript simulation/sim.R` to reproduce the full simulation.

## Manuscript and supplementary materials

All bibliography files required for rendering are stored locally in `paper/`.

From the repository root, render the manuscript with:

```bash
quarto render paper/pimma.qmd
```

and the supplementary materials with:

```bash
quarto render paper/supplementary.qmd
```

## Complete reproduction workflow

To reproduce the analyses from source:

1. Restore the R environment with `renv::restore()`.
2. Run `application/cbt-depression/01_preprocessing.R`.
3. Run `application/cbt-depression/02-multiverse.R`.
4. Reproduce the simulation with `Rscript simulation/sim.R`, or use the included `simulation/res-clean.rds` file to reproduce the manuscript results without rerunning the full simulation.
5. Render `paper/pimma.qmd`.
6. Render `paper/supplementary.qmd`.

The precomputed simulation results are provided for convenience because rerunning the complete simulation is substantially more computationally demanding than reproducing the empirical application and rendering the manuscript.
