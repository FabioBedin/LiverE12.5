
box::use(
  ggplot2[...],
  Seurat[...],
)


theme_seurat_box <- function() {
  theme_bw() + 
    theme(panel.grid = element_blank())
}

#' @export
UMAP_cellType <-  function(object) {
  
  
  UMAP = DimPlot(object,
                 pt.size = 0.8) +
    labs(y = "UMAP 2", x = "UMAP 1") +
    theme_seurat_box() +
    NoLegend()
  
  LabelClusters(UMAP,
                id = 'ident',
                box = T,
                position = 'nearest')
}

#' @export
Barplot_cellType = function(object, type) {
  
  if(type == "dataset1"){
    tab_cluster = as.data.frame(table(object$def_cluster))
  }else{
    tab_cluster = as.data.frame(table(object$celltype))
  }
  
  ggplot(tab_cluster, aes(x = Var1, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", show.legend = F) + theme_classic() +
    labs(x = NULL, y = NULL) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    theme_seurat_box()
}





