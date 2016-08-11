#' Listwise Deletion (LW)
#' @description This function deletes examinees who report missing responses.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @return A data frame with no missing responses.
#' @import stats
#' @examples  
#'         Listwise(test.data, Mvalue="8")
#' @export

Listwise<-function (test.data, Mvalue="NA") {
  if (Mvalue == "NA") {
    test.data<-na.omit(test.data) 
  } else {test.data[test.data==Mvalue]<-NA
  test.data<-na.omit(test.data)}
  test.data<-as.data.frame(test.data)
  return(test.data)
}