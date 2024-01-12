# FUNCTIONS AND VARIABLES


AH2CEa <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  return(CE)
}

AH2CEb <- function(AH) {
  CE <- round(AH - AH/33 + 622)
  AH <- ifelse(AH == 0, 1, AH)
  final <- paste0(AH, " AH\n", CE, " CE")
  return(final)
}

periodsAH <- seq(0, 1400, 100)
periodsCEa <- AH2CEa(periodsAH)
periodsCEb <- AH2CEb(periodsAH)