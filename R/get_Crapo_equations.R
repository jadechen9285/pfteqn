#' Crapo Reference Equations for calculating predictive values for PFT variables
#'
#' @param sex string for biological sex (Female vs Male)
#' @param ht height in cm
#' @param age numeric age (could be in decimal)
#' @param value wanted PFT variables
#'
#' @return Predictive value for PFT variable input
#' @export
#'
#' @examples
getCrapoPred <-  function(sex, ht, age, value = "FEV1"){
  ## NOTE: unit for height(ht): [cm], age in [years]
  stopifnot(param %in% c('FEV1', 'FVC', 'FEF2575', 'FEV1FVC', 
                         'RV', 'FRC', 'VC', 'IC', 'TLC', 'RVTLC',
                         'DLCO', 'DAB', 'DVA') )
  
  ret <- ifelse(
    sex %in% c("M", 'Male', 'MALE'),  
    # Male version
    switch(
      param,
      # air flow
      'FVC' = 0.06 * ht - 0.0214 * age - 4.65,
      'FEV1' = 0.0414 * ht - 0.0244 * age - 2.190,
      'FEF2575' = 0.0204 * ht - 0.038 * age + 2.133,
      'FEV1FVC' = -0.13 * ht - 0.152 * age + 110.49,
      # lung volume
      'RV' = 0.0216 * ht + 0.0207 * age - 2.84,
      'FRC' = 0.0471 * ht + 0.009 * age - 5.290,
      'VC' = 0.06 * ht - 0.0214 * age - 4.65,
      'IC' = 6.10 * (ht/100) - 0.028 * age - 4.65,
      'TLC' = 0.0795 * ht + 0.0032 * age - 7.333,
      'RVTLC' = 0.309 * age + 14.06,
      # diffusion
      'DLCO' =  0.410*ht - 0.210*age - 26.31,
      'DAB' = 0.410*ht - 0.210*age - 26.31, # same as DLCO
      'DVA' = 6.93 - 0.033*age
    ),
    
    # Female version
    switch(
      param,
      # air flow
      'FVC' = 0.0491 * ht - 0.0216 * age - 3.590,
      'FEV1' = 0.0342 * ht - 0.0255 * age - 1.578,
      'FEF2575' = 0.0154 * ht - 0.046 * age + 2.683,
      'FEV1FVC' = -0.202 * ht - 0.252 * age + 126.58,
      # lung volume
      'RV' = 0.0197 * ht + 0.0201 * age - 2.421,
      'FRC' = 0.036 * ht + 0.0031 * age - 3.182,
      'VC' = 0.0491 * ht - 0.0216 * age - 3.590,
      'IC' = 4.66 * (ht/100) - 0.026 * age - 3.28,
      'TLC' = 0.059 * ht - 4.537,
      'RVTLC' = 0.416 * age + 14.35,
      # diffusion
      'DLCO' = 0.282 * ht - 0.157 *age - 10.89,
      'DAB' = 0.282 * ht - 0.157 *age - 10.89, # same as DLCO
      'DVA' = 6.94 - 0.028*age
    )
  )
  return(ret)
}

#' Crapo Reference Equations for calculating lower limit of norm(LLN) for PFT variables
#'
#' @param sex string for biological sex (Female vs Male)
#' @param ht height in cm
#' @param age numeric age (could be in decimal)
#' @param value wanted PFT variables
#'
#' @return LLN values for PFT variable input
#' @export
#'
#' @examples
getCrapoLLN <- function(sex, ht, age, value = "FEV1"){
  ## NOTE: unit for height(ht): [cm], age in [years]
  stopifnot(param %in% c('FEV1', 'FVC', 'FEF2575', 'FEV1FVC', 
                         'RV', 'FRC', 'VC', 'IC', 'TLC', 'RVTLC',
                         'DLCO', 'DAB', 'DVA') )
  
  ret <- ifelse(
    sex %in% c("M", 'Male', 'MALE'),  
    # Male version
    switch(
      param,
      # air flow
      'FVC' = 0.06 * ht - 0.0214 * age - 4.65 - 1.12,
      'FEV1' = 0.0414 * ht - 0.0244 * age - 2.190 - 0.84,
      'FEF2575' = 0.0204 * ht - 0.038 * age + 2.133 - 1.67,
      'FEV1FVC' = -0.13 * ht - 0.152 * age + 110.49 - 8.3,
      # lung volume
      'RV' = 0.0216 * ht + 0.0207 * age - 2.84 - 0.76,
      'FRC' = 0.0471 * ht + 0.009 * age - 5.290 - 1.46,
      'VC' = 0.06 * ht - 0.0214 * age - 4.65 - 1.12,
      'TLC' = 0.0795 * ht + 0.0032 * age - 7.333 - 1.61 ,
      'RVTLC' = 0.309 * age + 14.06 - 8.8,
      # diffusion
      'DLCO' =  0.410*ht - 0.210*age - 26.31 - 8.2,
      'DAB' = 0.410*ht - 0.210*age - 26.31 - 8.2, # same as DLCO
      'DVA' = 6.93 - 0.033*age - 1.39
    ),
    
    # Female version
    switch(
      param,
      # air flow
      'FVC' = 0.0491 * ht - 0.0216 * age - 3.590 - 0.68,
      'FEV1' = 0.0342 * ht - 0.0255 * age - 1.578 - 0.56,
      'FEF2575' = 0.0154 * ht - 0.046 * age + 2.683 - 1.36,
      'FEV1FVC' = -0.202 * ht - 0.252 * age + 126.58 - 9.1,
      # lung volume
      'RV' = 0.0197 * ht + 0.0201 * age - 2.421 - 0.78,
      'FRC' = 0.036 * ht + 0.0031 * age - 3.182 - 1.06,
      'VC' = 0.0491 * ht - 0.0216 * age - 3.590 - 0.68,
      'TLC' = 0.059 * ht - 4.537 - 1.08,
      'RVTLC' = 0.416 * age + 14.35 - 11.0,
      # diffusion
      'DLCO' = 0.282 * ht - 0.157 *age - 10.89 - 6.1,
      'DAB' = 0.282 * ht - 0.157 *age - 10.89 - 8.2, # same as DLCO
      'DVA' = 6.94 - 0.028*age - 1.34
    )
  )
  return(ret)
}
