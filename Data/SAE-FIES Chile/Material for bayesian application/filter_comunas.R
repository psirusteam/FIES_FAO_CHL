dir_estim_comuna_CASEN_2020_hh <- readRDS("dir_estim_comuna_CASEN_2020_hh.rds")

# Filter comunas based on quality of direct estimates --------------------------
filter_comunas <- function(direct_estimates){
  
  # mean households by upm
  direct_estimates$promhupm <- direct_estimates$n/direct_estimates$n_psu_comuna
  
  # rho
  direct_estimates$rho <- (direct_estimates$deff-1)/(direct_estimates$promhupm-1)
  
  # limit of acceptable rho
  direct_estimates$coterho <- 1-(direct_estimates$n_psu_comuna/(direct_estimates$n_psu_comuna-1))
  
  # application of the criterias
  direct_estimates$calidad2[direct_estimates$n<50|
                              direct_estimates$deg_free<=2|
                              direct_estimates$rho<direct_estimates$coterho] <- 0
  direct_estimates$calidad2[!direct_estimates$n<50 &
                              !direct_estimates$deg_free<=2 &
                              !direct_estimates$rho<direct_estimates$coterho] <- 1
  direct_estimates$calidad2[direct_estimates$deg_free>=14] <- 1
  
  # factor of the criteria
  direct_estimates$calidadf <- factor(direct_estimates$calidad2,levels=c(1,0),
                                      labels = c("Incluir","Excluir"))
  table(direct_estimates$calidadf)
  
  # Variables by criteria (to check reason of inclusion/exclusion)
  direct_estimates$incdf14 <- direct_estimates$deg_free>=14
  direct_estimates$excn50 <- !direct_estimates$n<50
  # direct_estimates$excdeff1=!direct_estimates$DEff<1
  direct_estimates$excrho <- !direct_estimates$rho<direct_estimates$coterho
  direct_estimates$excdf2 <- !direct_estimates$deg_free<=2
  
  direct_estimates$incdf14 <- factor(direct_estimates$incdf14,levels = c(T,F),
                                     labels = c("Incluir", "Excluir"))
  direct_estimates$excn50 <- factor(direct_estimates$excn50,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  # direct_estimates$excdeff1=factor(direct_estimates$excdeff1,levels = c(T,F),
  #                                  labels = c( "Incluir","Excluir"))
  direct_estimates$excrho <- factor(direct_estimates$excrho,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  direct_estimates$excdf2 <- factor(direct_estimates$excdf2,levels = c(T,F),
                                    labels = c( "Incluir","Excluir"))
  
  return(direct_estimates)
}


dir_estim_comuna_CASEN_2020_hh2 <- filter_comunas(dir_estim_comuna_CASEN_2020_hh)
table(dir_estim_comuna_CASEN_2020_hh2$calidad2)
