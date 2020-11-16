#' Listwise Deletion (LW)
#' @description This function deletes examinees who report missing responses (see De Ayala et al. 2001)
#' <doi:10.1111/j.1745-3984.2001.tb01124.x>.
#' @param test.data  Test data set (a data frame or a matrix) containing missing responses. 
#' Missing values are coded as NA or other values (e.g., 8, 9).
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @param Mvalue  Missing response indicators in the data (e.g. "NA", "8", "9", etc.). Mvalue="NA" by default.
#' @return A data frame with no missing responses.
#' @import stats
#' @examples  
#'         Listwise(test.data, Mvalue="8")
#' @export
#' @references {
#'  De Ayala, R. J., Plake, B. S., & Impara, J. C. (2001). 
#' "The impact of omitted responses on the accuracy of ability estimation in item response theory."
#'  Journal of Educational Measurement, 38(3), 213–234. doi:10.1111/j.1745-3984.2001.tb01124.x. 
#' }


Listwise<-function (test.data, Mvalue="NA") {
  if (Mvalue == "NA") {
    test.data<-na.omit(test.data) 
  } else {test.data[test.data==Mvalue]<-NA
  test.data<-na.omit(test.data)}
  test.data<-as.data.frame(test.data)
  return(test.data)
}