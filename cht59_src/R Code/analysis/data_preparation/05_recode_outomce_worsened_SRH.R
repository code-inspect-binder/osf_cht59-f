library(easystats)

share8 <- data_filter(share, both_waves == 2 & wave == 8)
share9 <- data_filter(share, both_waves == 2 & wave == 9)

share8 <- dplyr::arrange(share8, mergeid)
share9 <- dplyr::arrange(share9, mergeid)

stopifnot(identical(share8$mergeid, share9$mergeid))

change_in_health <- recode_into(
  # same, same
  share8$cah002_ == 3 & share9$cah002_ == 3 ~ 1,
  # same, improved
  share8$cah002_ == 3 & share9$cah002_ == 1 ~ 2,
  # same, worsened
  share8$cah002_ == 3 & share9$cah002_ == 2 ~ 3,
  # improved, same
  share8$cah002_ == 1 & share9$cah002_ == 3 ~ 4,
  # improved, improved
  share8$cah002_ == 1 & share9$cah002_ == 1 ~ 5,
  # improved, worsened
  share8$cah002_ == 1 & share9$cah002_ == 2 ~ 6,
  # worsened, same
  share8$cah002_ == 2 & share9$cah002_ == 3 ~ 7,
  # worsened, improved
  share8$cah002_ == 2 & share9$cah002_ == 1 ~ 8,
  # worsened, worsened
  share8$cah002_ == 2 & share9$cah002_ == 2 ~ 9,
  # missing...
  default = as.numeric(NA)
)

share9$change_in_health <- change_in_health
share9$change_in_health3 <- recode_values(
  share9$change_in_health,
  recode = list(`0` = c(1, 2, 4, 5), `1` = c(3, 7, 9), `2` = c(6, 8))
)
share9$change_in_health2 <- recode_values(
  share9$change_in_health3,
  recode = list(`0` = 0, `1` = 1),
  default = NA
)

share9$change_in_health <- sjlabelled::set_labels(
  share9$change_in_health,
  labels = c("same, same", "same, improved", "same, worsened", "improved, same",
             "improved, improved", "improved, worsened", "worsened, same",
             "worsened, improved", "worsened, worsened")
)

share9$change_in_health3 <- sjlabelled::set_labels(
  share9$change_in_health3,
  labels = c("same or improved", "worsened", "inconsistent")
)

share9$change_in_health2 <- sjlabelled::set_labels(
  share9$change_in_health2,
  labels = c("same or improved", "worsened")
)

share9 <- to_factor(share9, select = c("change_in_health", "change_in_health3"))

share8$health_past3months <- recode_values(
  share8$cah002_,
  recode = list(`0` = c(1, 3), `1` = 2)
)

share9$health_past3months <- recode_values(
  share9$cah002_,
  recode = list(`0` = c(1, 3), `1` = 2)
)

share8$health_past3months <- sjlabelled::set_labels(
  share8$health_past3months,
  labels = c("same or improved", "worsened")
)

share9$health_past3months <- sjlabelled::set_labels(
  share9$health_past3months,
  labels = c("same or improved", "worsened")
)

save(share8, file = "./Daten/share8.RData")
save(share9, file = "./Daten/share9.RData")


# data_tabulate(share8$health_past3months)
# data_tabulate(share9$health_past3months)
#
# data_tabulate(share9, c("change_in_health", "change_in_health3", "change_in_health2"))
