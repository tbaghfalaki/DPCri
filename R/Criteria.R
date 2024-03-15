#' AUC and BS
#'
#' @description
#' Compute the iid-representation of areas under time-dependent ROC curves (AUC) and Brier score (BS)
#'
#' @details
#' The Brier score and AUC calculations utilize the available functions within the timeROC package, with added flexibility in the function arguments. Alongside the obtained results, a summary including estimates and standard deviations is provided for enhanced interpretation.
#'
#' @param s the landmark time for prediction
#' @param t the window of prediction for prediction
#' @param Survt the survival time
#' @param CR the indicator for competing risks
#' @param P the risk prediction
#' @param cause the main cause for prediction
#'
#'
#'
#' @return
#' - AUC The values of AUC from timeROC package
#' - BS The values of BS from available documents
#' - Cri A summary of AUC and BS
#'
#' @author Taban Baghfalaki \email{t.baghfalaki@gmail.com}
#'
#' @example inst/example.R
#'
#' @md
#' @export


Criteria <- function(s, t, Survt, CR, P, cause) {
  index <- 1:length(Survt)
  index1 <- index[Survt > s][is.na(index[Survt > s]) == FALSE]
  Time1 <- Survt[index1]
  death1 <- CR[index1]
  p_dem1 <- P[index1]



  bs <- BS(
    timepoints = t,
    times = Time1 - s,
    status = death1,
    pred = matrix(p_dem1),
    cause = 1
  )

  auc <- timeROC::timeROC(
    T = Time1 - s,
    delta = death1,
    marker = p_dem1,
    cause = 1, weighting = "marginal",
    times = t,
    iid = TRUE
  )
  bst <- c(bs$BS, bs$sd)
  auct <- c(auc$AUC[2], auc$inference$vect_sd_1[2])

  Res <- rbind(auct, bst)
  rownames(Res) <- c("AUC", "BS")
  colnames(Res) <- c("est", "sd")


  list(BS = bs, AUC = auc, Cri = Res)
}
