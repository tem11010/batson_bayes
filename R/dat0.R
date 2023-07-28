#' Jury Selection in Connecticut federal court
#'
#' A dataset containing the jury selection information for cases completed in the federal district court
#'  of Connecticut for 2013-2018
#'
#' @format A data frame with 979 rows and 30 variables:
#' \describe{
#'   \item{Docket.x}{case docket number}
#'   \item{Case_Name}{case name on caption}
#'   \item{courthouse}{courthouse location}
#'   \item{Defendant.x}{first defendant name on case caption}
#'   \item{Judge}{ID (initials) for district court judge}
#'   \item{date}{date jury selection began}
#'   \item{R_No}{venireperson sequence order}
#'   \item{Pool_Seq}{jury pool ID number}
#'   \item{race}{numerical code for race from juror questionaire}
#'   \item{hisp}{1 = "Hispanic", 0 = not}
#'   \item{sex}{M = "male", F = "female"}
#'   \item{Disp}{voir dire disposition. "PP" - prosecutor strike; "PD" - defense strike; "J" - seated juror}
#'   \item{strike_num}{order of strikes - each side}
#'   \item{P_atty}{prosecutors on docket sheet at time of jury selection}
#'   \item{D_atty}{defense attorneys on docket sheet at time of jury selection}
#'   ...
#' }
#' @source Jury Administrator, US District Court - Connecticut
"dat0"