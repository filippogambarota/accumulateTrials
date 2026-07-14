This repository contains the R code to reproduce the analyses of the paper. The `paper/` folder contains the Quarto `.qmd` files used to render the manuscript and the supplementary materials.

The repository is organized as an R package where:

- `R/`: contains the custom functions used in the project. You can load them with `devtools::load_all()`, which is equivalent to loading an installed package with `library()`.
- `data/`: contains the processed datasets and the raw data used to create them. The cleaned datasets can be loaded with `data("simon")`, `data("snarc")`, and `data("tswitch")`.

Functions in the `R/` folder and datasets are documented. You can access the documentation using, for example, `?simon` for a dataset or `?accumulate_trials` for a custom function.

The `data/raw/` folder contains the raw data used in the analyses. The original PsychoPy files were cleaned only to remove private participant identifiers, which can be considered sensitive information. All other preprocessing steps are contained in `scripts/01-pre-processing.R`.

## Scripts

The `scripts/` folder contains the main scripts used to clean and analyze the data. In particular:

- `01-pre-processing.R`: contains the pre-processing steps and creates the cleaned datasets.
- `02-cumulative-model.R`: imports the cleaned data, fits the cumulative models, and extracts the relevant model parameters.
- `03a-resampling.R`: performs the participant-subsampling analysis of the main models. The script is meant to be run in parallel on a cluster with 70 cores. In interactive mode, it runs serially and the computation time can be prohibitive. With 70 cores, the resampling took roughly 13 hours.
- `03b-res-resampling.R`: removes large columns from the resampling output to make the results faster to import.

## Using the project

The best way to use the project and reproduce the analyses is to clone or download the repository, then open the `.Rproj` file in RStudio or open the main folder with any other IDE.

Relevant R packages are managed with `renv`. When opening the project, the `.Rprofile` file activates the `renv` environment. If packages are missing, run `renv::restore()` to install the versions recorded in `renv.lock`.

Then you can run `devtools::load_all()` to load the functions and datasets before running the scripts. The scripts can also be run with `Rscript` from the terminal, which evaluates each step in a separate R session.

## Paper

The `paper/` folder contains the `paper.qmd` and `supplementary.qmd` Quarto files used to render the manuscript and the supplementary materials. The full project can be rendered with `quarto render` from the root folder. The paper is created using the [`apaquarto`](https://github.com/wjschne/apaquarto) extension.
