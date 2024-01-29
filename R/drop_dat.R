#' @title Remove exposures or outcomes in dat for a specific number of snps.
#'
#' @param dat dat from harmonise_data().see more \code{\link[TwoSampleMR]{harmonise_data}}
#' @param num_snp minimum of the number of snp
#' @param drop_mrkeep whether to remove snp with mr_keep parameter FALSE, the default is TRUE
#'
#' @description
#' This function is designed to remove exposures or outcomes in the dat that are less than num_snp,
#' and the result returns the new dat after removal
#'
#' @return a data frame of the result
#' @export
#' @importFrom dplyr filter
#' @examples
#' dat<-drop_dat(dat,num_snp=3,drop_mrkeep=TRUE)
#'
drop_dat<-function(dat,num_snp=5,drop_mrkeep=TRUE){
  if(drop_mrkeep)dat<-subset(dat,mr_keep)
  if(nrow(dat)<num_snp)stop("The total number of SNPs is less than the set number.")
  cn<-colnames(dat)
  dat[["idm"]]<-paste(dat[["id.exposure"]],dat[["id.outcome"]],sep = "|")
  ids<-unique(dat[["idm"]])
  if(length(ids)!=1){
    for (id in ids) {
      if(nrow(dat[dat$idm==id,])<num_snp)ids<-ids[-which(ids==id)]
    }}
  dat<-dat%>%dplyr::filter(idm%in%ids)
  return(dat[,cn])
}
