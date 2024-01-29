#' @title Get res from mr() in the same beta direction
#'
#' @param dat result from mr().see more \code{\link[TwoSampleMR]{mr}}
#' @param met participation in MR methods for comparing beta directions.Default is c("MR Egger", "Weighted median", "Inverse variance weighted")
#'
#' @return a data frame which is MR results with beta removed in different directions.
#' @export
#'
#' @description
#' Removes the results of a specific MR method with a different beta direction, returning the res data frame after removal.
#'
#' @examples
#' res<-get_sbeta_res(res)
#'
#' @importFrom dplyr filter
get_sbeta_res<-function(dat,met=c("MR Egger","Weighted median","Inverse variance weighted")){
  ids<-unique(dat$id.exposure)
  for (id in ids) {
    temp<-dat%>%dplyr::filter(id.exposure==id)%>%dplyr::filter(method%in%met)
    if(nrow(temp)!=length(met))next
    if(length(unique(sign(temp$b)))!=1)ids<-ids[-which(id==ids)]
  }
  return(dat%>%dplyr::filter(id.exposure%in%ids))
}
