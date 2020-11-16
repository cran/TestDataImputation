#' Treat missing responses as incorrect (IN)
#' @description This function replaces all missing responses by zero (see Lord, 1974 <doi: 10.1111/j.1745-3984.1974.tb00996.x>; 
#'  Mislevy & Wu, 1996 <doi: 10.1002/j.2333-8504.1996.tb01708.x>; Pohl et al., 2014 <doi: 10.1177/0013164413504926>);).
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @return A data frame with all missing responses imputed.
#' @import stats
#' @examples  
#'         TreatIncorrect(test.data, Mvalue="8")
#' @export
#' @references {
#'  Lord, F. M. (1974).
#' " Quick estimates of the relative efficiency of two tests as a function of ability level."
#'  Journal of Educational Measurement, 11(4), 247–254. doi: 10.1111/j.1745-3984.1974.tb00996.x. 
#' }
#' @references {
#'  Mislevy, R. J., & Wu, P. K. (1996).
#' " Missing responses and IRT ability estimation: Omits, choice, time limits, and adaptive testing. "
#'  ETS Research Report Series, 1996(2), i-36. doi: 10.1002/j.2333-8504.1996.tb01708.x. 
#' }
#' @references {
#' Pohl, S., Gräfe, L., & Rose, N. (2014).
#' "Dealing with omitted and not-reached items in competence tests evaluating approaches accounting for missing responses in item response theory models. "
#' Educational and Psychological Measurement, 74(3), 423–452. doi: 10.1177/0013164413504926. 
#' }


TreatIncorrect<-function (test.data, Mvalue="NA") {
  if (Mvalue == "NA") {
    test.data[is.na(test.data)] <- 0 
  } else {test.data[test.data==Mvalue]<-NA
    test.data[is.na(test.data)] <- 0}
  test.data<-as.data.frame(test.data)
    return(test.data)
}


