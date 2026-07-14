The repository contains the R code to reproduce the analyses of the paper. In addition the `paper/` folder contains the Quarto `qmd` files to reproduce the entire paper and supplementary materials.

The repository is organized as an R package where:

- `R/`: contains the custom functions that are used in the project. You can load the functions using `devtools::load_all()` that is equivalent to calling `library()`.
- `data/`: contains the raw and processed datasets. You can load the cleaned data directly using `data("simon")`, `data("snarc")` and `data("tswitch")` or equivalently loading directly the files within `data/`.

Functions within the `R` folder and datasets are documented. You can access the documentation using `?simon` for the dataset or `?accumulate_trials` for a custom function.

Notice that the `data/raw/` folder contains the raw data but the original dataset from Psychopy has been cleaned removing the participant private ID that can be considered as a sensitive information. Any other processing step is contained in the `scripts/01-pre-processing.R` script.

## Scripts

The `scripts` folder contains the main scripts used to clean and analyze the data. In particular:

- `01-pre-processing.R`: contains the pre-processing steps and creates the cleaned datasets.
- `02-cumulative-model.R`: import the cleaned data and fit the comulative models extracting all the relevant parameters
- `03a-resampling.R`: perform the subsampling analysis of the main models. Notice that the script is meant to be run in parallel using a cluster with 70 cores. In interactive mode the script runs serially but computations time could be really proebitive. In parallel with 70 cores the resampling took roughly 13 hours.
- `03b-res-resampling.R`: is a simple pre-processing script that remove large columns from the main output for faster importing

## Using the project

The best way to use the project and reproduce the analyses is cloning or downloading the repository then opening the `Rproj` file with R Studio or opening the main folder with any other IDE.

Relevant R packages are managed using `renv` and the `.Rprofile` file will try to activate the `renv` envinronment and you will be prompted to install the correct version of the packages.

Then you can simply run `devtools::load_all()` to load functions and data and run the scripts. The suggestion is to run the script using `Rscript` from the terminal to evaluate each step into an isolated environment.

## Paper

The `paper/` folder contains the `paper.qmd` and `supplementary.qmd` Quarto files to render the paper and the supplementary materials. The full project can be rendered using `quarto render` from the root folder. The paper is created using the [`apaquarto`](https://github.com/wjschne/apaquarto) extension. 