#' Response Function Imputation (RF)
#' @description This function imputes for all missing responses using the response function imputation 
#' (Sijtsma and van der Ark, 2003 <doi: 10.1207/s15327906mbr3804_4>).
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).#' 
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param max.score  The max possible response value in test data (the lowest response value should be 0). 
#' By default max.score=1 (i.e.,binary test data). max.score = 2 if the response categories are (0, 1, 2), etc.
#' @param round.decimal The number of digits or decimal places for the imputed value. The default value is 0.
#' @return A data frame with all missing responses imputed with response function.
#' @import stats
#' @examples  
#'         ResponseFun(test.data, Mvalue="NA",max.score=1,round.decimal=0)
#' @export
#' @references {
#' Sijtsma, K., & Van der Ark, L. A. (2003).
#' "Investigation and treatment of missing item scores in test and questionnaire data."
#'  Multivariate Behavioral Research, 38(4), 505-528. DOI: 10.1207/s15327906mbr3804_4.
#' }

ResponseFun<-function (test.data, Mvalue="NA",max.score=1,round.decimal=0) {
  
  if (Mvalue == "NA") {
    x.plus<-rowSums(test.data,na.rm = T)
    R.est<-rowMeans(test.data,na.rm=T)*(ncol(test.data)-1)
    R.left<-floor(R.est)
    R.right<-ceiling(R.est)
    p.mat.left<-matrix(NA,max.score,ncol(test.data))
    p.mat.right<-matrix(NA,max.score,ncol(test.data))
    p.i.j<-rep(NA,max.score)
    for (i in 1:nrow(test.data)) { 
      for (j in 1:ncol(test.data)) { 
        if (!is.na(test.data[i,j])) next
        x.r<-rowSums(test.data[,-j],na.rm = T)
        x.r.na<-x.plus-test.data[,j]
        for (k in 1:max.score) {
          p.mat.left[k,j]<-length(which(R.left==x.r & test.data[,j]==k))/length(which(test.data[,j]==k))
          p.mat.right[k,j]<-length(which(R.right==x.r & test.data[,j]==k))/length(which(test.data[,j]==k))
          p.i.j[k]<-p.mat.left[k,j]+((p.mat.right[k,j]-p.mat.left[k,j])*(x.r[i]-R.left[i]))
        }
        p.i.j.tp1<-p.i.j.tp2<-rep(0,max.score+1)
        p.i.j.tp1[2:(max.score+1)]<-p.i.j.tp2[1:max.score]<-p.i.j
        p.i.j.tp<-p.i.j.tp1-p.i.j.tp2
        p.i.j.fn<-p.i.j.tp[-1]
        if (is.na(test.data[i,j])) {test.data[i,j]<-rbinom(1,match(max(p.i.j.fn),p.i.j.fn),max(p.i.j.fn))}
      }}
  } else {test.data[test.data==Mvalue]<-NA
  x.plus<-rowSums(test.data,na.rm = T)
  R.est<-rowMeans(test.data,na.rm=T)*(ncol(test.data)-1)
  R.left<-floor(R.est)
  R.right<-ceiling(R.est)
  p.mat.left<-matrix(NA,max.score,ncol(test.data))
  p.mat.right<-matrix(NA,max.score,ncol(test.data))
  p.i.j<-rep(NA,max.score)
  for (i in 1:nrow(test.data)) { 
    for (j in 1:ncol(test.data)) { 
      if (is.na(test.data[i,j])) next
      x.r<-rowSums(test.data[,-j],na.rm = T)
      x.r.na<-x.plus-test.data[,j]
      for (k in 1:max.score) {
        p.mat.left[k,j]<-length(which(R.left==x.r & test.data[,j]==k))/length(which(test.data[,j]==k))
        p.mat.right[k,j]<-length(which(R.right==x.r & test.data[,j]==k))/length(which(test.data[,j]==k))
        p.i.j[k]<-p.mat.left[k,j]+((p.mat.right[k,j]-p.mat.left[k,j])*(x.r[i]-R.left[i]))
      }
      p.i.j.tp1<-p.i.j.tp2<-rep(0,max.score+1)
      p.i.j.tp1[2:(max.score+1)]<-p.i.j.tp2[1:max.score]<-p.i.j
      p.i.j.tp<-p.i.j.tp1-p.i.j.tp2
      p.i.j.fn<-p.i.j.tp[-1]
      if (is.na(test.data[i,j])){test.data[i,j]<-rbinom(1,match(max(p.i.j.fn),p.i.j.fn),max(p.i.j.fn))}
      }}}
  test.data<-round(test.data,digits=round.decimal)
  test.data<-as.data.frame(test.data)
    return(test.data)
  }
    