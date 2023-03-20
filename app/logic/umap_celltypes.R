
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
  
  
  # if(type == "dataset1"){
  #   color = list(
  #     "EryP" = "#5e4fa2",
  #     "Ery6" = "#9e0142",
  #     "Ery5" = "#d53e4f",
  #     "Ery4" = "#f46d43",
  #     "Ery3" = "#fdae61",
  #     "Ery2" = "#fee08b",
  #     "Ery1" = "#FFF05A",
  #     "Mk"   = "#abd9e9",
  #     "Imm"  = "#2166ac",
  #     "Endo" = "#de77ae",
  #     "Hepa" = "#33a02c"
  #   )
  # }else{
  #   color = list(
  #     "pre-HSPCs/HSCs" = "#08519c",
  #     "Lymphoid Progenitors" = "#810f7c",
  #     "EMPs/MMPs" = "#8c96c6",
  #     "GMPs" = "#02818a",
  #     "Granulocyte Progenitors" = "#238443",
  #     "Neutrophils" = "#78c679",
  #     "Monocytes" = "#7fcdbb",
  #     "Kupffer cells" = "#1d91c0",
  #     "MEPs" = "#cb181d",
  #     "Megakaryoblasts" = "#ae017e",
  #     "Megakaryocytes" = "#fa9fb5",
  #     "BFU-E" = "#fd8d3c",
  #     "hepatic stellate cells" = "#F5DF51"
  #   )
  # }
  # 
  
  
  UMAP = DimPlot(object,
                 # cols = color,
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





