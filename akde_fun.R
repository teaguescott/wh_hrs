library(ctmm)
library(readr)

wh.telem <- read_rds("wh.telem.rda")

best_fit <- read_rds("best_fit2019-09-06.rda")

AKDES <- akde(wh.telem[1:7], best_fit[1:7]) # a subset of the 129 bird-months works...

saveRDS(AKDES, file = paste0("./output/akdes", Sys.Date(), ".rds"))

summary(AKDES[[2]])

OLS <- overlap(AKDES)

saveRDS(OLS, file = paste0("./output/overlaps", Sys.Date(), ".rds"))
