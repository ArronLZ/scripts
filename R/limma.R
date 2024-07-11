#' General compare the differnt ID(Gene) between the group B vs A
#' @description General compare the differnt ID(Gene) between the group B vs A
#'
#' @param eset data.frame, the exprs data, must be log2 transformed
#' @param group data.frame, the pheno information, the first coloumn is the type information and must be factor.
#' @param pval number, the pvalue cutoff
#' @param fdr number, the fdr cutoff
#' @param logfc number, the log2(fc) value cutoff
#'
#' @return # all deg dataframe(resdf)
#' @export
#' @importFrom limma lmFit eBayes topTable
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' pn_eset <- data.table::fread("./protein_count_pn.csv", data.table = F)
#' pn_eset <- pn_eset %>% tibble::column_to_rownames(var = "Protein")
#' 
#' group <- read.csv("./group.csv", row.names = 1)
#' group$Type <- as.factor(group$Type)
#' 
#' df <- log2(pn_eset + 1)
#' diff <- limma.general(eset = df, group = group)
#' }
limma.general <- function(eset, group, pval = 0.05, fdr = 0.1, logfc = log2(2)) {
  names(group)[1] <- "Type"
  stopifnot(max(eset) < 25, all(colnames(eset) == rownames(group)),
            is.factor(group$Type))
  group_list <- group$Type
  # ~法,截距法
  design <- model.matrix(~group_list) 
  colnames(design) <- levels(group_list)
  rownames(design) <- colnames(eset)
  # ~三部曲
  fit <- lmFit(eset, design)
  fit <- eBayes(fit, trend = TRUE)
  tab <- topTable(fit, coef = 2, number = Inf)
  tab <- merge(tab, eset, by = 0)
  tab <- tab %>%
    dplyr::rename(Gene=Row.names, log2FC=logFC, PValue=P.Value, FDR=adj.P.Val) %>%
    # dplyr::select(Gene, log2FC, PValue, FDR, names(eset), everything()) %>%
    dplyr::arrange(desc(log2FC), PValue)
  
  deg <- tab[which(tab$PValue < pval & tab$FDR < fdr & abs(tab$log2FC) > logfc),]
  return(list(groupdata = group, resdf=tab, deg=deg))
}
