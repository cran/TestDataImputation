#' Treat missing responses as incorrect (IN)
#' @description This function replaces all missing responses by zero.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @return A data frame with all missing responses imputed.
#' @import stats
#' @examples  
#'         TreatIncorrect(test.data, Mvalue="8")
#' @export

TreatIncorrect<-function (test.data, Mvalue="NA") {
  if (Mvalue == "NA") {
    test.data[is.na(test.data)] <- 0 
  } else {test.data[test.data==Mvalue]<-NA
    test.data[is.na(test.data)] <- 0}
  test.data<-as.data.frame(test.data)
    return(test.data)
}


