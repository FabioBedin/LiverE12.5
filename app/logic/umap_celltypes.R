
box::use(
  ggplot2[theme_bw, theme, element_blank, labs],
  Seurat[...],
)

#### libraries ####
# library(Seurat)
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(magrittr)
# library(ggpubr)

# #set the working directory
# setwd("/hpcnfs/data/SM/scRNAseq_Fantin")
# 
# #### read the object ####
# combined=readRDS("combined_last_last.rds")
# 
# ### additional functions ####
# theme_seurat_box <- function(){
#   theme_bw() %+replace% #replace elements we want to change
#     theme(panel.grid = element_blank())}
# 
# #### ordering the cell type ####
# combined$def_cluster <- factor(x = combined$def_cluster,
#                                levels = rev(c("EryP","Ery6","Ery5","Ery4","Ery3",
#                                               "Ery2","Ery1", "Mk" ,"Imm","Endo" ,
#                                               "Hepa")))
# 
# combined$def_cluster_reverse <- factor(x = combined$def_cluster,
#                                        levels = c("EryP","Ery6","Ery5","Ery4","Ery3",
#                                                   "Ery2","Ery1", "Mk" ,"Imm","Endo" ,
#                                                   "Hepa"))
# 
# Idents(combined)=rev(combined$def_cluster) 
# 
# #### colors ####
# combined_color=list("EryP"="#5e4fa2","Ery6"="#9e0142","Ery5"="#d53e4f",
#                     "Ery4"="#f46d43","Ery3"="#fdae61",
#                     "Ery2"="#fee08b","Ery1"="#FFF05A",
#                     "Mk"="#abd9e9","Imm"="#2166ac",
#                     "Endo"="#de77ae","Hepa"="#33a02c")


#' @export
UMAP_cellType <-  function(object) {
  theme_seurat_box <- function() {
    theme_bw() 
  }
  
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

#try the function: IT WORKS
#UMAP.cellType(combined)



# #### barplot ####
# Barplot.cellType = function(object){
#   
# }




#' 
#' #' @export
#' FeaturePlot(
#'   object,
#'   features = feature,
#'   pt.size = 0.8,
#'   order = T,
#'   cols = c("lightgrey", "brown")
#' )


# #try the function: IT WORKS
# FeatPlot(combined, 'Pf4')







