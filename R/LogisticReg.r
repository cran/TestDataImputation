#' Logistic Regression (LR) Imputation
#' @description This function imputes for all missing responses using logistic regression.
#' Integrated scores are obtained by rounding imputed values to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats mice
#' @examples  
#'         LogsticReg(test.data, Mvalue="8",max.score=1)
#' @export

LogsticReg<-function (test.data, Mvalue="NA",max.score=1) {
  if (max.score==1){
  if (Mvalue == "NA") {
    test.data[] <- lapply(test.data, factor)
    LR.out <- mice(test.data, m=1, method="logreg")
          } else {test.data[test.data==Mvalue]<-NA
          test.data[] <- lapply(test.data, factor)
          LR.out <- mice(test.data, m=1, method="logreg")
          }
  } else if (max.score>1) {
    if (Mvalue == "NA") {
      test.data[] <- lapply(test.data, factor)
      LR.out <- mice(test.data, m=1, method="polyreg")
    } else {test.data[test.data==Mvalue]<-NA
    test.data[] <- lapply(test.data, factor)
    LR.out <- mice(test.data, m=1, method="polyreg")
    } }
  dataout<-as.data.frame(complete(LR.out))
  test.data<-as.data.frame(dataout)
  return(test.data)
}