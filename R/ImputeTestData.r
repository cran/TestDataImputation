
#' This main function imputes for missing responses using selected method
#' @description This function imputes for all missing responses using selected miputation methods.
#' Integrated scores are obtained by rounding imputed values to the closest possible response value.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data. By default max.score=1 (i.e.,binary test data).
#' @param method  Missing response imputation methods. 
#' "LW" (by default) represents listwise that deletes all examinees
#' who reported missing responses (see De Ayala et al. 2001) <doi:10.1111/j.1745-3984.2001.tb01124.x>;
#' "IN" means treating all missing responses as incorrect (see Lord, 1974 <doi: 10.1111/j.1745-3984.1974.tb00996.x>; 
#' Mislevy & Wu, 1996 <doi: 10.1002/j.2333-8504.1996.tb01708.x>; Pohl et al., 2014 <doi: 10.1177/0013164413504926>); 
#' "PM" imputes for all missing responses of an examinee by his/her mean on the available items; 
#' "IM" imputes for all missing responses of an item by its mean on the available responses; 
#' "TW" imputes for all missing responses using two-way imputation (if an examinee has no response to all items, 
#' the missing responses are replaced by item means first; see Sijtsma & van der Ark, 2003 <doi: 10.1207/s15327906mbr3804_4>); 
#' "LR" imputes for all missing responses using logistic regression; 
#' "EM" imputes for all missing responses using EM imputation (see Finch, 2008) <doi: 10.1111/j.1745-3984.2008.00062.x>.
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses replaced by integrated imputed values.
#' @import stats mice Amelia
#' @examples  
#'         ImputeTestData(test.data, Mvalue="8",max.score=1, method ="TW",round.decimal=0)
#' @export
#' @references {
#'  De Ayala, R. J., Plake, B. S., & Impara, J. C. (2001). 
#' "The impact of omitted responses on the accuracy of ability estimation in item response theory."
#'  Journal of Educational Measurement, 38(3), 213–234. doi:10.1111/j.1745-3984.2001.tb01124.x. 
#' }
#' @references {
#'  Finch, H. (2008).
#' "Estimation of Item Response Theory Parameters in the Presence of Missing Data."
#'  Journal of Educational Measurement, 45(3), 225-245. doi: 10.1111/j.1745-3984.2008.00062.x. 
#' }
#' @references {
#'  Lord, F. M. (1974).
#' " Quick estimates of the relative efficiency of two tests as a function of ability level."
#'  Journal of Educational Measurement, 11(4), 247–254. doi: 10.1111/j.1745-3984.1974.tb00996.x. 
#' }
#' @references {
#'  Mislevy, R. J., & Wu, P. K. (1996).
#' " Missing responses and IRT ability estimation: Omits, choice, time limits, and adaptive testing. "
#'  ETS Research Report Series, 1996(2), i-36. doi: 10.1002/j.2333-8504.1996.tb01708.x. 
#' }
#' @references {
#' Pohl, S., Gräfe, L., & Rose, N. (2014).
#' "Dealing with omitted and not-reached items in competence tests evaluating approaches accounting for missing responses in item response theory models. "
#' Educational and Psychological Measurement, 74(3), 423–452. doi: 10.1177/0013164413504926. 
#' }
#' @references {
#' Sijtsma, K., & Van der Ark, L. A. (2003). 
#' "Investigation and treatment of missing item scores in test and questionnaire data."
#'  Multivariate Behavioral Research, 38(4), 505-528. doi: 10.1207/s15327906mbr3804_4.
#' }



ImputeTestData<-function (test.data, Mvalue="NA",max.score=1, method ="LW",round.decimal=0)
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
    test.data[index] <-PM[index[,1]]}
    test.data<-round(test.data,digits=round.decimal)
    test.data<-as.data.frame(test.data)
  } else if (method=="IM"){
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
    test.data[index] <-IM[index[,2]]}
    test.data<-round(test.data,digits=round.decimal)
    test.data<-as.data.frame(test.data)
    } else if (method=="TW"){
      if (Mvalue == "NA") {
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
        }}}
      test.data<-round(test.data,digits=round.decimal)
      test.data<-as.data.frame(test.data)
    } else if (method=="LR") {
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
    } else if (method=="EM"){
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
    }
return(test.data)
}
