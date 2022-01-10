#' getQuanjerPred 
#'
#' @description Quanjer Reference Equations for calculating predictive values 
#' for PFT variables
#' @param sex biological sex, 'F' vs 'M'
#' @param ht height in meter (m)
#' @param age age in year
#' @param value PFT variables name (default in FEV1) (L)
#'
#' @return the predictive PFT values
#' @export
#'
getQuanjerPred <- function(sex, ht, age, value = 'FEV1'){
  ## NOTE: unit for height(ht):[m], unit for age:[year]
  value <- toupper(value)
  stopifnot(value %in% c('IVC', 'FVC', 'FEV1', 'FEF2575', 'FEV1FVC',
                         'TLC', 'RV', 'FRC', 'RVTLC', 'FRCTLC') )
  #
  ret <-  ifelse(
    sex %in% c("M", 'Male', 'MALE'),  
    # Male version
    switch(
      value,
      # air flow
      'IVC' = 6.10 * ht - 0.028 * age - 4.65,
      'FVC' = 5.76 * ht - 0.026 * age - 4.34,
      'FEV1' =  4.30 * ht - 0.029 * age - 2.49,
      'FEF2575' = 1.94 * ht - 0.043 * age + 2.7,
      'FEV1FVC' = -0.18 * ht  + 87.21,
      # lung volume
      'TLC' = 7.99 * ht - 7.08,
      'RV' = 1.31 * ht + 0.022 * age - 1.23,
      'FRC' = 2.34 * ht + 0.009 * age - 1.09,
      'RVTLC' = 0.39 * age + 13.96,
      'FRCTLC' = 0.21 * age + 43.8
    ),
  
    # Female version
    switch(
      value,
      # air flow
      'IVC' = 4.66 * ht - 0.026 * age - 3.28,
      'FVC' = 4.43 * ht - 0.026 * age - 2.89,
      'FEV1' =  3.95 * ht - 0.025 * age - 2.6,
      'FEF2575' = 1.25 * ht - 0.034 * age + 2.92,
      'FEV1FVC' = -0.19 * ht  + 89.1,
      # lung volume
      'TLC' = 6.60 * ht - 5.79,
      'RV' = 1.81 * ht + 0.016 * age - 2,
      'FRC' = 2.24 * ht + 0.001 * age - 1,
      'RVTLC' = 0.34 * age + 18.96,
      'FRCTLC' = 0.16 * age + 45.1
    )
  )
  return(ret)
}

#' getQuanjerLLN
#'
#' @description Quanjer Reference Equations for calculating lower limit of norm(LLN)
#' values for PFT variables
#' @param sex biological sex, 'F' vs 'M'
#' @param ht height in meter (m)
#' @param age age in year
#' @param value PFT variables name (default in FEV1) (L)
#'
#' @return the computed LLN value for the imputed PFT variable
#' @export
#'
getQuanjerLLN <- function(sex, ht, age, value = 'FEV1'){
  ## NOTE: unit for height(ht):[m], unit for age:[year]
  value <- toupper(value)
  stopifnot(value %in% c('IVC', 'FVC', 'FEV1', 'FEF2575', 'FEV1FVC',
                         'TLC', 'RV', 'FRC', 'RVTLC', 'FRCTLC') )
  
  ret <- ifelse(
    sex %in% c("M", 'Male', 'MALE'),  
    # Male version
    switch(
      value,
      # air flow
      'IVC' = 6.10 * ht - 0.028 * age - 4.65 - 0.92,
      'FVC' = 5.76 * ht - 0.026 * age - 4.34 - 1,
      'FEV1' =  4.30 * ht - 0.029 * age - 2.49 - 0.84,
      'FEF2575' = 1.94 * ht - 0.043 * age + 2.7 - 1.71,
      'FEV1FVC' = -0.18 * ht  + 87.21 - 11.8,
      # lung volume
      'TLC' = 7.99 * ht - 7.08 - 1.15,
      'RV' = 1.31 * ht + 0.022 * age - 1.23 - 0.67,
      'FRC' = 2.34 * ht + 0.009 * age - 1.09 - 0.99,
      'RVTLC' = 0.39 * age + 13.96 - 9,
      'FRCTLC' = 0.21 * age + 43.8 - 11.1
    ),
    
    # Female version
    switch(
      value,
      # air flow
      'IVC' = 4.66 * ht - 0.026 * age - 3.28 - 0.69,
      'FVC' = 4.43 * ht - 0.026 * age - 2.89 - 0.71,
      'FEV1' =  3.95 * ht - 0.025 * age - 2.6 - 0.62,
      'FEF2575' = 1.25 * ht - 0.034 * age + 2.92 - 1.4,
      'FEV1FVC' = -0.19 * ht  + 89.1 - 10.7,
      # lung volume
      'TLC' = 6.60 * ht - 5.79 - 0.99,
      'RV' = 1.81 * ht + 0.016 * age - 2 - 0.58,
      'FRC' = 2.24 * ht + 0.001 * age - 1 - 0.82,
      'RVTLC' = 0.34 * age + 18.96 - 9.6,
      'FRCTLC' = 0.16 * age + 45.1 - 9.8
    )
  )
  return(ret)
}

#' getQuanjerULN
#'
#' @description Quanjer Reference Equations for calculating upper limit of norm(ULN)
#' values for PFT variables
#' @param sex biological sex, 'F' vs 'M'
#' @param ht height in meter (m)
#' @param age age in year
#' @param value PFT variables name (default in FEV1) (L)
#'
#' @return the computed ULN value for the imputed PFT variable
#' @export
#'
getQuanjerULN <- function(sex, ht, age, value = 'FEV1'){
  ## NOTE: unit for height(ht):[m], unit for age:[year]
  value <- toupper(value)
  stopifnot(value %in% c('IVC', 'FVC', 'FEV1', 'FEF2575', 'FEV1FVC',
                         'TLC', 'RV', 'FRC', 'RVTLC', 'FRCTLC') )
  
  ret <-  ifelse(
    sex %in% c("M", 'Male', 'MALE'),  
    # Male version
    switch(
      value,
      # air flow
      'FVC' = 5.76 * ht - 0.026 * age - 4.34 + 1,
      'FEV1' =  4.30 * ht - 0.029 * age - 2.49 + 0.84,
      'FEF2575' = 1.94 * ht - 0.043 * age + 2.7 + 1.71,
      'FEV1FVC' = -0.18 * ht  + 87.21 + 11.8,
      # lung volume
      'TLC' = 7.99 * ht - 7.08 + 1.15,
      'RV' = 1.31 * ht + 0.022 * age - 1.23 + 0.67,
      'FRC' = 2.34 * ht + 0.009 * age - 1.09 + 0.99,
      'RVTLC' = 0.39 * age + 13.96 + 9,
      'FRCTLC' = 0.21 * age + 43.8 + 11.1
    ),
    
    # Female version
    switch(
      value,
      # air flow
      'FVC' = 4.43 * ht - 0.026 * age - 2.89 + 0.71,
      'FEV1' =  3.95 * ht - 0.025 * age - 2.6 + 0.62,
      'FEF2575' = 1.25 * ht - 0.034 * age + 2.92 + 1.4,
      'FEV1FVC' = -0.19 * ht  + 89.1 + 10.7,
      # lung volume
      'TLC' = 6.60 * ht - 5.79 + 0.99,
      'RV' = 1.81 * ht + 0.016 * age - 2 + 0.58,
      'FRC' = 2.24 * ht + 0.001 * age - 1 + 0.82,
      'RVTLC' = 0.34 * age + 18.96 + 9.6,
      'FRCTLC' = 0.16 * age + 45.1 + 9.8
    )
  )
  return(ret)
}
