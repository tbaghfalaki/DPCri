rm(list = ls())
library(Landmarking)
library(pec)
data(data)
set.seed(2)
INDTRAIN <- sample(unique(data$id), 0.5 * length(unique(data$id)))
INDVALID <- unique(data$id)[-INDTRAIN]
########################
data_model_landmark_LOCF <- return_ids_with_LOCF(
  data_long = data,
  individual_id = "id",
  covariates = c("Y1", "Y2"),
  covariates_time = c(rep("obstime", 2)),
  x_L = 0.2
)

data_t <- data_model_landmark_LOCF[(data_model_landmark_LOCF$id %in% c(INDTRAIN)), ]

data_model_landmark_LOCF1 <- fit_LOCF_landmark(
  data_long = data_t,
  x_L = 0.2,
  x_hor = 0.5,
  covariates = c("Y1", "Y2"),
  covariates_time = c(rep("obstime", 2)),
  individual_id = "id",
  event_time = "Time",
  event_status = "Censoring",
  survival_submodel = "standard_cox"
)
data_v <- data_model_landmark_LOCF[!(data_model_landmark_LOCF$id %in% c(INDVALID)), ]

AA <- predict(
  object = data_model_landmark_LOCF1, x_L = 0.2,
  x_hor = 0.5, newdata = data_v
)

DP <- AA[, 4]

AAA <- data.frame(AA$id, data_v[!duplicated(data_v[, 2]), ], DP)


Criteria(s = 0.1, t = 0.5, Survt = AAA$Time, CR = AAA$Censoring, P = DP, cause = 1)
