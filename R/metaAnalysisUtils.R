computeCohenD <- function(tstat, NCases, NControls) {
  d <- (tstat*(NCases+NControls)/(sqrt(NCases*NControls)*sqrt(NCases+NControls - 2)))
  return(d)
}

#Compute Std. error of d
computeCohenSE <- function(CohenD, NCases, NControls) {
  se_d <- sqrt(((NCases+NControls-1)/(NCases+NControls-3))*((4/(NCases+NControls))*(1+((CohenD**2)/8))))
  return(se_d)
}

runMetaanalysis <- function(d_vector, SE_d_vector) {
  a <- meta::metagen(TE=d_vector,
                     seTE=SE_d_vector,
                     studlab=rownames(d_vector),
                     method.tau = "PM",
                     sm="SMD")
  
  final_vector <-c(a$TE.random,
                   a$seTE.random,
                   a$lower.random,
                   a$upper.random,
                   a$zval.random,
                   a$pval.random,
                   a$tau2,
                   a$I2, 
                   a$pval.Q)
  #final_vector <- as.numeric(final_vector)
  
  names(final_vector) <- c("RE_Eff.Size","RE_SE","RE_CI_lower", "RE_CI_upper", "RE_Zscore","RE_P.value","RE_het_tau2", "RE_het_I2", "RE_het_P.value")
  return(final_vector)
}