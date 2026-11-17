#' Calculate relative risk given noise exposure level
#'
#' Calculate the relative risk (RR) for each person in the baseline population for each
#' disease related to air pollution and each scenario based on the individual noise exposure levels
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item various checks to ensure the correct noise exposure levels (doses) and disease are read in
#'
#' \item get the lookup table for the required dose response functions which contains for each dose
#'   the median RR values
#'
#' \item for the noise exposure doses in the baseline population find the needed RR by extrapolating the
#'   dose responses given in the lookup table
#'
#' }
#'
#' @param cause name of disease
#' @param dose vector of noise exposure levels from individuals for a given age range and scenario
#'
#' @return data frame of relative risks for each person in the baseline population and each noise related disease in each scenario
#'
#' @export


noise_dose_response <- function(cause, dose) {
  # Check there are NAs in dose or the classes is not numeric
  if (sum(is.na(dose)) > 0 || class(dose) != "numeric") {
    stop("Please provide dose in numeric")
  }
  # check that the correct disease name is used
  if (!cause %in% c(
    "all_cause_noise",
    "ihd_noise",
    "diabetes_noise",
    "stroke_noise"
  )) {
    
    stop("Unsupported cause/disease. Please select from \n
         all_cause_noise \n diabetes_noise \n stroke_noise")
  } # End if unsupported causes
  
  # read in dose response functions for disease cause
  # contains relative risk with upper and lower limits for a variety of doses
  lookup_table <- get(cause)
  names(lookup_table) <- base::tolower(names(lookup_table))
  lookup_df <- setDT(lookup_table)
  
  # interpolate the values in the lookup table to get RR values for all noise exposure doses
  # in the baseline population
  suppressWarnings(
    rr <- approx(
      x = lookup_df$dose, y = lookup_df$rr,
      xout = dose, yleft = 1, yright = min(lookup_df$rr)
    )$y
  )
  return(data.frame(rr = rr))
}
