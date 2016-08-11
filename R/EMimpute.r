#' EM Imputation
#' @description This function imputes for all missing responses using EM imputation.
#' Integrated scores are obtained by rounding imputed values to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats mice Amelia
#' @examples  
#'         EMimpute(test.data, Mvalue="8",max.score=1)
#' @export

EMimpute<-function (test.data, Mvalue="NA",max.score=1) {
  if (Mvalue == "NA") {
    EM.imp<-amelia(test.data, m = 1, boot.type = "none",
                   bound=cbind(rep(1:ncol(test.data)),
                               rep(-1000,ncol(test.data)),rep(1000,ncol(test.data))))
    data.imp<-data.frame(EM.imp$imputations$imp1)
    data.imp[data.imp<0]<-0
    data.imp[data.imp>max.score]<-max.score
    test.data<-round(data.imp,digits=0)
          } else {test.data[test.data==Mvalue]<-NA
          EM.imp<-amelia(test.data, m = 1, boot.type = "none",
                         bound=cbind(rep(1:ncol(test.data)),
                                     rep(-1000,ncol(test.data)),rep(1000,ncol(test.data))))
          data.imp<-data.frame(EM.imp$imputations$imp1)
          data.imp[data.imp<0]<-0
          data.imp[data.imp>max.score]<-max.score
          test.data<-round(data.imp,digits=0)
      }
  test.data<-as.data.frame(test.data)
  return(test.data)
}