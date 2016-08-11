
#' This main function imputes for missing responses using selected method
#' @description This function imputes for all missing responses using selected miputation methods.
#' Integrated scores are obtained by rounding imputed values to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @param method  Missing response imputation methods. "LW" (by default) represents listwise that deletes all examinees
#'  who reported missing responses; "IN" means treating all missing responses as incorrect; "PM" imputes for 
#'  all missing responses of an examinee by his/her mean on the available items; "IM" imputes for all missing responses 
#'  of an item by its mean on the available responses; "TW" imputes for all missing responses using two-way 
#'  imputation.; "LR" imputes for all missing responses using logistic regression; "EM" imputes 
#'  for all missing responses using EM imputation.
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats mice Amelia
#' @examples  
#'         ImputeTestData(test.data, Mvalue="8",max.score=1, method ="TW")
#' @export

ImputeTestData<-function (test.data, Mvalue="NA",max.score=1, method ="LW")
{
  if (method == "LW") {
    if (Mvalue == "NA") {
      test.data<-na.omit(test.data) 
    } else {test.data[test.data==Mvalue]<-NA
    test.data<-na.omit(test.data)}
    test.data<-as.data.frame(test.data)
    } else if (method=="IN"){
      if (Mvalue == "NA") {
      test.data[is.na(test.data)] <- 0 
    } else {test.data[test.data==Mvalue]<-NA
    test.data[is.na(test.data)] <- 0}
    test.data<-as.data.frame(test.data)
    } else if (method=="PM"){
    if (Mvalue == "NA") {
      PM<-rowMeans(test.data,  na.rm = TRUE)
      for (i in 1:length(PM)) {
        if (PM[i] < 0) {PM[i]<-0}
        else if (PM[i] > max.score) {PM[i]<-max.score}
      }
      PM<-round(PM,digits = 0)
      index <- which(is.na(test.data), arr.ind=TRUE)
      test.data[index] <-PM[index[,1]]
    } else {test.data[test.data==Mvalue]<-NA
    PM<-rowMeans(test.data,  na.rm = TRUE)
    for (i in 1:length(PM)) {
      if (PM[i] < 0) {PM[i]<-0}
      else if (PM[i] > max.score) {PM[i]<-max.score}
    }
    PM<-round(PM,digits = 0)
    index <- which(is.na(test.data), arr.ind=TRUE)
    test.data[index] <-PM[index[,1]]}
    test.data<-round(test.data,digits=0)
    test.data<-as.data.frame(test.data)
  } else if (method=="IM"){
    if (Mvalue == "NA") {
      IM<- colMeans(test.data, na.rm=TRUE)
      for (j in 1:length(IM)) {
        if (IM[j] < 0) {IM[j]<-0}
        else if (IM[j] > max.score) {IM[j]<-max.score}
      }
      IM<-round(IM,digits = 0)
      index <- which(is.na(test.data), arr.ind=TRUE)
      test.data[index] <-IM[index[,2]]
    } else {test.data[test.data==Mvalue]<-NA
    IM <- colMeans(test.data, na.rm=TRUE)
    for (j in 1:length(IM)) {
      if (IM[j] < 0) {IM[j]<-0}
      else if (IM[j] > max.score) {IM[j]<-max.score}
    }
    IM<-round(IM,digits = 0)
    index <- which(is.na(test.data), arr.ind=TRUE)
    test.data[index] <-IM[index[,2]]}
    test.data<-round(test.data,digits=0)
    test.data<-as.data.frame(test.data)
    } else if (method=="TW"){
      if (Mvalue == "NA") {
        IM<- colMeans(test.data, na.rm=T)
        PM<-rowMeans(test.data,  na.rm = T)
        OM<-mean(as.matrix(test.data),na.rm=T)
        for (i in 1:nrow(test.data)) { 
          for (j in 1:ncol(test.data)) { 
            TW<-PM[i]+IM[j]-OM
            if (TW < 0) {TW<-0}
            else if (TW > max.score) {TW <- max.score}
            TW<-round(TW,digits=0)
            if (is.na(test.data[i,j])){
              test.data[i,j]<-TW}  
          }}
      } else {test.data[test.data==Mvalue]<-NA
      IM<- colMeans(test.data, na.rm=T)
      PM<-rowMeans(test.data,  na.rm = T)
      OM<-mean(as.matrix(test.data),na.rm=T)
      for (i in 1:nrow(test.data)) { 
        for (j in 1:ncol(test.data)) { 
          TW<-PM[i]+IM[j]-OM
          if (TW < 0) {TW<-0}
          else if (TW > max.score) {TW <- max.score}
          TW<-round(TW,digits=0)
          if (is.na(test.data[i,j])){
            test.data[i,j]<-TW}  
        }}}
      test.data<-round(test.data,digits=0)
      test.data<-as.data.frame(test.data)
    } else if (method=="LR") {
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
    } else if (method=="EM"){
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
    }
return(test.data)
}
