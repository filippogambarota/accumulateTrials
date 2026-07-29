# Flatten resampling results for lighter downstream use.
#
# Inputs:  objects/task_resampling.rds
# Outputs: objects/task_resampling_lt.rds
# Run after: scripts/03a-resampling.R

# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
devtools::load_all()

# Import nested resampling results ----------------------------------------

res_resampling_raw <- readRDS("objects/task_resampling.rds")

# Flatten bootstrap results and drop the nested task data ------------------

res_resampling <- unnest(res_resampling_raw, res)
res_resampling_new <- select(res_resampling, -data)

# Save lightweight result table -------------------------------------------

saveRDS(res_resampling_new, file = "objects/task_resampling_lt.rds")
