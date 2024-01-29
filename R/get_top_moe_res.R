#' @title Get result for larger moe values
#' @description
#' Get the row with the higher moe value among multiple moe results
#'
#' @param res_moe result from mr_moe().see more \code{\link[TwoSampleMR]{mr_moe}}
#' @param top number of rows for larger results
#' @param list the result is a list, which defaults to FALSE.
#'
#' @return a data frame or a list
#' @export
#'
#' @examples
#' res<-get_top_moe_res(res_moe)
get_top_moe_res<-function(res_moe,top=1,list=FALSE){
  res_list<-list()
  if(top==0){  for (i in 1:length(res_moe)) {
    res_list[[i]]<-res_moe[[i]]$estimates
  }}else{
    for (i in 1:length(res_moe)) {
      n<-ifelse(nrow(res_moe[[i]]$estimates)<top,nrow(res_moe[[i]]$estimates),top)
      res_list[[i]]<-res_moe[[i]]$estimates[1:n,]
    }}
  if(list){d<-res_list}else{d<-do.call(rbind,res_list)}
  return(d)
}
