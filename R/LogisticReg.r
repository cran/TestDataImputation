#' Logistic Regression (LR) Imputation
#' @description This function imputes for all missing responses using logistic regression (for binary responses) or
#' polytomous regression (for polytomous responses). The mice () function with default settings 
#' from the mice package (Van Buuren & Groothuis-Oudshoorn, 2011 <doi: 10.18637/jss.v045.i03>) is used 
#' to impute for the missing responses. 
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats 
#' @importFrom mice mice complete
#' @examples  
#'         LogisticReg(test.data, Mvalue="NA",max.score=1)
#' @export
#' @references {
#' Van Buuren, S., & Groothuis-Oudshoorn, K. (2011).
#' "mice: Multivariate imputation by chained equations in R."
#'  Journal of statistical software, 45(1), 1-67. DOI: 10.18637/jss.v045.i03.
#' }

LogisticReg<-function (test.data, Mvalue="NA",max.score=1) {
  if (max.score==1){
  if (Mvalue == "NA") {
    test.data[] <- lapply(test.data, factor)
    LR.out <- mice::mice(test.data, m=1, method="logreg")
          } else {test.data[test.data==Mvalue]<-NA
          test.data[] <- lapply(test.data, factor)
          LR.out <- mice::mice(test.data, m=1, method="logreg")
          }
  } else if (max.score>1) {
    if (Mvalue == "NA") {
      test.data[] <- lapply(test.data, factor)
      LR.out <- mice::mice(test.data, m=1, method="polyreg")
    } else {test.data[test.data==Mvalue]<-NA
    test.data[] <- lapply(test.data, factor)
    LR.out <- mice::mice(test.data, m=1, method="polyreg")
    } }
  dataout<-as.data.frame(mice::complete(LR.out))
  test.data<-as.data.frame(dataout)
  return(test.data)
}