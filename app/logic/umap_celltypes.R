
box::use(
  ggplot2[...],
  Seurat[...],
)

box::use(
  app/logic/constants[color_all, color_subset]
)


theme_seurat_box <- function() {
  theme_bw() + 
    theme(panel.grid = element_blank())
}

#' @export
UMAP_cellType <-  function(object, type) {
  
  color=NULL
  
  if(type == "dataset1"){
    color <- color_all
  }else{
    color <- color_subset
  }
  
  
  UMAP = DimPlot(object,
                 pt.size = 2.3, raster = FALSE) +
    labs(y = "UMAP 2", x = "UMAP 1") +
    theme_seurat_box() + theme_bw() +
    scale_color_manual(values = alpha(color, 0.5))+
    theme(panel.grid = element_blank(),
          plot.title = element_blank(),
          axis.text = element_text(size=15),
          axis.title = element_text(size=15))+
    NoLegend()
  
  LabelClusters(UMAP,
                id = 'ident',
                box = T,
                position = 'nearest', size = 10, fill = "white")
}

#' @export
Barplot_cellType = function(object, type) {
  
  color=NULL
  
  if(type == "dataset1"){
    color <- color_all
  }else{
    color <- color_subset
  }
  
  tab_cluster = as.data.frame(table(object$def_cluster))

  ggplot(tab_cluster, aes(x = Var1, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", show.legend = F) + theme_classic() +
    labs(x = NULL, y = NULL) + theme_bw() + 
    scale_fill_manual(values = color)+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1, size=15),
          axis.text.y = element_text(size=15),
          axis.ticks.y = element_blank())
}



#' @export
violin_plot <- function(object, gene, type) {
  
  if(type == "dataset1"){
    color <- color_all
  }else{
    color <- color_subset
  }
  
  VlnPlot(
    object,
    features = gene,
    cols = color,
    pt.size = 0) +  theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size=15),
          plot.title = element_blank(),
          axis.title= element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1, size=15)) + NoLegend()
}

