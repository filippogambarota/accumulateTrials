library(tidyr)
library(dplyr)
devtools::load_all()

res_resampling_raw <- readRDS("objects/task_resampling.rds")
res_resampling <- unnest(res_resampling_raw, res)
res_resampling_new <- select(res_resampling, -data)
saveRDS(res_resampling_new, file = "objects/task_resampling_lt.rds")
