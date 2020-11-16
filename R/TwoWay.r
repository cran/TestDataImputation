#' Two-Way Imputation (TW)
#' @description This function imputes for all missing responses using two-way imputation.
#' Integrated responses are obtained by rounding imputed values to the closest possible response value.
#' If a case showed missingness on all the variables (i.e., empty record), the missing values are 
#' replaced by item means first. see Sijtsma and van der Ark (2003) <doi: 10.1207/s15327906mbr3804_4>;
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).#' 
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses replaced by integrated two-way imputed values.
#' @import stats
#' @examples  
#'         Twoway(test.data, Mvalue="8",max.score=1,round.decimal=0)
#' @export
#' @references {
#' Bernaards, C. A., & Sijtsma, K. (2000).
#' " Influence of imputation and EM methods on factor analysis when 
#' item nonresponse in questionnaire data is nonignorable."
#'  Multivariate Behavioral Research, 35(3), 321-364.DOI: 10.1207/S15327906MBR3503_03.
#' }

Twoway<-function (test.data, Mvalue="NA",max.score=1,round.decimal=0) {
  if (Mvalue == "NA") {
    IM<- colMeans(test.data, na.rm=T)
    for (r in 1:nrow(test.data)) {
      if (sum(is.na(test.data[r,]))==ncol(test.data)){
        test.data[r,]<-IM}}
    PM<-rowMeans(test.data,  na.rm = T)
    OM<-mean(as.matrix(test.data),na.rm=T)
    for (i in 1:nrow(test.data)) { 
      for (j in 1:ncol(test.data)) { 
        TW<-PM[i]+IM[j]-OM
        if (TW < 0) {TW<-0}
        else if (TW > max.score) {TW <- max.score}
        TW<-round(TW,digits=round.decimal)
        if (is.na(test.data[i,j])){
        test.data[i,j]<-TW}  
      }}
    
  } else {test.data[test.data==Mvalue]<-NA
  IM<- colMeans(test.data, na.rm=T)
  for (r in 1:nrow(test.data)) {
    if (sum(is.na(test.data[r,]))==ncol(test.data)){
      test.data[r,]<-IM
    }}
  PM<-rowMeans(test.data,  na.rm = T)
  OM<-mean(as.matrix(test.data),na.rm=T)
  for (i in 1:nrow(test.data)) { 
    for (j in 1:ncol(test.data)) { 
      TW<-PM[i]+IM[j]-OM
      if (TW < 0) {TW<-0}
      else if (TW > max.score) {TW <- max.score}
      TW<-round(TW,digits=round.decimal)
      if (is.na(test.data[i,j])){
        test.data[i,j]<-TW}  
    }}
  }
  test.data<-round(test.data,digits=round.decimal)
  test.data<-as.data.frame(test.data)
    return(test.data)
}