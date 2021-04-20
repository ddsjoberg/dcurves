#' A simulated data frame used to motivate examples in the \code{dca} package
#'
#' @format A data frame with 7 variables and 750 rows:
#' \describe{
#'   \item{patientid}{Identification Number}
#'   \item{cancer}{Cancer Diagnosis: 0=No, 1=Yes}
#'   \item{dead}{Dead (1=yes; 0=no)}
#'   \item{ttcancer}{Years to Cancer Dx/Censor}
#'   \item{risk_group}{Patient Risk Group (Low, Intermediate, High)}
#'   \item{casecontrol}{Case-control Status: 1=Case, 0=Control}
#'   \item{age}{Patient Age, years}
#'   \item{famhistory}{Family History of Cancer: 0=No, 1=Yes}
#'   \item{marker}{Marker}
#'   \item{cancerpredmarker}{Prob. of Cancer based on Age, Family History, and Marker}
#' }
"df_dca"

