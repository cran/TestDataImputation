#' Item Mean (IM) Imputation
#' @description This function imputes for all missing responses of an item by its mean (i.e., IM) on the available responses.
#' Integrated scores for items are obtained by rounding their means to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses replaced by Integrated item means.
#' @import stats
#' @examples  
#'         ItemMean(test.data, Mvalue="NA",max.score=1,round.decimal=0)
#' @export

ItemMean<-function (test.data, Mvalue="NA",max.score=1,round.decimal=0) {
  if (Mvalue == "NA") {
    IM<- colMeans(test.data, na.rm=TRUE)
    for (j in 1:length(IM)) {
      if (IM[j] < 0) {IM[j]<-0}
      else if (IM[j] > max.score) {IM[j]<-max.score}
    }
    IM<-round(IM,digits = round.decimal)
    index <- which(is.na(test.data), arr.ind=TRUE)
    test.data[index] <-IM[index[,2]]
      } else {test.data[test.data==Mvalue]<-NA
      IM <- colMeans(test.data, na.rm=TRUE)
      for (j in 1:length(IM)) {
        if (IM[j] < 0) {IM[j]<-0}
        else if (IM[j] > max.score) {IM[j]<-max.score}
      }
      IM<-round(IM,digits = round.decimal)
      index <- which(is.na(test.data), arr.ind=TRUE)
      test.data[index] <-IM[index[,2]]
      }
  test.data<-round(test.data,digits=round.decimal)
  test.data<-as.data.frame(test.data)
  return(test.data)
}