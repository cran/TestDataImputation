#' Predictive mean matching (PMM) 
#' @description This function imputes for all missing responses using predictive mean matching. 
#' The mice () function with default settings from the mice package (Van Buuren & Groothuis-Oudshoorn, 2011 
#' <doi: 10.18637/jss.v045.i03>) is used to impute for the missing responses. 
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats 
#' @importFrom mice mice complete
#' @examples  
#'         micePMM(test.data, Mvalue="NA")
#' @export
#' @references {
#' Van Buuren, S., & Groothuis-Oudshoorn, K. (2011).
#' "mice: Multivariate imputation by chained equations in R."
#'  Journal of statistical software, 45(1), 1-67. DOI: 10.18637/jss.v045.i03.
#' }

micePMM<-function (test.data, Mvalue="NA") {

  if (Mvalue == "NA") {
    test.data[] <- lapply(test.data, factor)
    pmm.out <- mice::mice(test.data, m=1, method="pmm")
          } else {test.data[test.data==Mvalue]<-NA
          test.data[] <- lapply(test.data, factor)
          pmm.out <- mice::mice(test.data, m=1, method="pmm")
          }
  dataout<-as.data.frame(mice::complete(pmm.out))
  test.data<-as.data.frame(dataout)
  return(test.data)
}

