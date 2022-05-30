


stats <- readRDS("data-raw/stats.rds")
walk(names(stats), ~ saveRDS(stats[[.x]], paste0("~/projects/yphws_dashboard/data-raw/stats_", .x, ".eds")))
saveRDS(stats, paste0("~/projects/yphws_dashboard/data-raw/stats_reserve.Rds"))
stats[1] -> stats
saveRDS(stats, paste0("~/projects/yphws_dashboard/data-raw/stats.Rds")) ## just sex
