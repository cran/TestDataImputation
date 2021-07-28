#' Person Mean Imputation (PM)
#' @description This function imputes for all missing responses of an examinee by his/her mean (i.e., PM) 
#' on the available items. Integrated scores for examinees are obtained 
#' by rounding their means to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses replaced by person means.
#' @import stats
#' @examples  
#'         PersonMean(test.data, Mvalue="NA",max.score=1,round.decimal=0)
#' @export
#' @references {
#' Sijtsma, K., & Van der Ark, L. A. (2003). 
#' "Investigation and treatment of missing item scores in 
#' test and questionnaire data."
#'  Multivariate Behavioral Research, 38(4), 505-528.DOI: 10.1207/s15327906mbr3804_4.
#' }

PersonMean<-function (test.data, Mvalue="NA",max.score=1,round.decimal=0) {
  if (Mvalue == "NA") {
    PM<-rowMeans(test.data,  na.rm = TRUE)
    for (i in 1:length(PM)) {
      if (PM[i] < 0) {PM[i]<-0}
      else if (PM[i] > max.score) {PM[i]<-max.score}
    }
    PM<-round(PM,digits = round.decimal)
    index <- which(is.na(test.data), arr.ind=TRUE)
    test.data[index] <-PM[index[,1]]
      } else {test.data[test.data==Mvalue]<-NA
      PM<-rowMeans(test.data,  na.rm = TRUE)
      for (i in 1:length(PM)) {
        if (PM[i] < 0) {PM[i]<-0}
        else if (PM[i] > max.score) {PM[i]<-max.score}
      }
      PM<-round(PM,digits = round.decimal)
      index <- which(is.na(test.data), arr.ind=TRUE)
      test.data[index] <-PM[index[,1]]
      }
  test.data<-round(test.data,digits=round.decimal)
  test.data<-as.data.frame(test.data)
  return(test.data)
}