#' Simulated data with a binary outcome
#'
#' @format A data frame with 750 rows:
#' \describe{
#'   \item{patientid}{Identification Number}
#'   \item{cancer}{Cancer Diagnosis: 0=No, 1=Yes}
#'   \item{dead}{Dead (1=yes; 0=no)}
#'   \item{risk_group}{Patient Risk Group (Low, Intermediate, High)}
#'   \item{age}{Patient Age, years}
#'   \item{famhistory}{Family History of Cancer: 0=No, 1=Yes}
#'   \item{marker}{Marker}
#'   \item{cancerpredmarker}{Prob. of Cancer based on Age, Family History, and Marker}
#' }
"df_binary"

#' Simulated data with a survival outcome
#'
#' @format A data frame with 750 rows:
#' \describe{
#'   \item{patientid}{Identification Number}
#'   \item{cancer}{Cancer Diagnosis: 0=No, 1=Yes}
#'   \item{ttcancer}{Years to Cancer Dx/Censor}
#'   \item{risk_group}{Patient Risk Group (Low, Intermediate, High)}
#'   \item{age}{Patient Age, years}
#'   \item{famhistory}{Family History of Cancer: 0=No, 1=Yes}
#'   \item{marker}{Marker}
#'   \item{cancerpredmarker}{Prob. of Cancer based on Age, Family History, and Marker}
#' }
"df_surv"

#' Simulated data with a case-control outcome
#'
#' @format A data frame with 750 rows:
#' \describe{
#'   \item{patientid}{Identification Number}
#'   \item{casecontrol}{Case-control Status: 1=Case, 0=Control}
#'   \item{risk_group}{Patient Risk Group (Low, Intermediate, High)}
#'   \item{age}{Patient Age, years}
#'   \item{famhistory}{Family History of Cancer: 0=No, 1=Yes}
#'   \item{marker}{Marker}
#'   \item{cancerpredmarker}{Prob. of Cancer based on Age, Family History, and Marker}
#' }
"df_case_control"
