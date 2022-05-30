


stats <- readRDS("data-raw/stats.rds")
walk(names(stats), ~ write_rds(stats[[.x]], paste0("~/projects/yphws_dashboard/data-raw/stats_", .x, ".Rds")))
write_rds(stats, paste0("~/projects/yphws_dashboard/data-raw/stats_reserve.Rds"))
stats[1] -> stats
write_rds(stats, paste0("~/projects/yphws_dashboard/data-raw/stats.Rds")) ## just sex
