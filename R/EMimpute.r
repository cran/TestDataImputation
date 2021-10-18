#' EM Imputation
#' @description This function imputes for all missing responses using EM imputation (see Finch, 2008) 
#' <doi: 10.1111/j.1745-3984.2008.00062.x>. The Amelia package (Honaker et al., 2011 <doi: 10.18637/jss.v045.i07>)
#' is used for the imputation. Integrated scores are then obtained by rounding imputed values 
#' to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in the test data. By default max.score=1 (i.e.,binary test data).
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats
#' @importFrom Amelia amelia
#' @examples  
#'         EMimpute(test.data, Mvalue="NA",max.score=1,round.decimal=0)
#' @export
#' @references {
#'  Finch, H. (2008).
#' "Estimation of Item Response Theory Parameters in the Presence of Missing Data."
#'  Journal of Educational Measurement, 45(3), 225-245. doi: 10.1111/j.1745-3984.2008.00062.x. 
#' }
#' @references {
#'  Honaker, J., King, G., & Blackwell, M. (2011).
#' "Amelia II: A program for missing data."
#'  Journal of statistical software, 45(1), 1-47. doi: 10.18637/jss.v045.i07. 
#' }


  

EMimpute<-function (test.data, Mvalue="NA",max.score=1,round.decimal=0) {
  if (Mvalue == "NA") {
    EM.imp<-Amelia::amelia(test.data, m = 1, boot.type = "none",
                   bound=cbind(rep(1:ncol(test.data)),
                               rep(-1000,ncol(test.data)),rep(1000,ncol(test.data))))
    data.imp<-data.frame(EM.imp$imputations$imp1)
    data.imp[data.imp<0]<-0
    data.imp[data.imp>max.score]<-max.score
    test.data<-round(data.imp,digits=round.decimal)
          } else {test.data[test.data==Mvalue]<-NA
          EM.imp<-Amelia::amelia(test.data, m = 1, boot.type = "none",
                         bound=cbind(rep(1:ncol(test.data)),
                                     rep(-1000,ncol(test.data)),rep(1000,ncol(test.data))))
          data.imp<-data.frame(EM.imp$imputations$imp1)
          data.imp[data.imp<0]<-0
          data.imp[data.imp>max.score]<-max.score
          test.data<-round(data.imp,digits=round.decimal)
      }
  test.data<-as.data.frame(test.data)
  return(test.data)
}