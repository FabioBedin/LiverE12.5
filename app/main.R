box::use(
  shiny[moduleServer, NS, tags, plotOutput, renderPlot, eventReactive, observeEvent, uiOutput, renderUI],
  shiny.fluent[fluentPage, Stack, Dropdown.shinyInput, Text, ComboBox.shinyInput],
  here[here],
  ggplot2[theme_bw, theme, element_blank],
  Seurat[...]
)

box::use(
  app/view/cardUI[makeCard],
)

box::use(
  app/logic/umap_celltypes[UMAP_cellType, Barplot_cellType],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    Stack(
      style= "padding: 2rem; justify-content: space-around;",
      horizontal = TRUE,
      tags$div(
        style= "display: flex; align-items: center; justify-content: center;",
        tags$span(
          "LiverE12.5",
          class = "ms-fontSize-32 ms-fontWeight-semibold",
          style = "color: #323130"
        ) 
        # tags$span(
        #   "se serve aggiungere un sottotitolo",
        #   class = "ms-fontSize-14 ms-fontWeight-regular",
        #   style = "color: #605E5C; margin: 14px;"
        # )
      ),
      tags$div(
        style= "display: flex; align-items: center; justify-content: center;",
        Text(variant = "large", "Dataset: ", style = "margin-right: 1rem"),
        Dropdown.shinyInput(
          style="width: 200px;",
          inputId = ns("dropdown_input"),
          value = "dataset1",
          options = list(
            list(key = "dataset1", text = "Dataset 1"),
            list(key = "dataset2", text = "Dataset 2")
          )
        ),
        Text(variant = "large", "Gene: ", style = "margin: 0 1rem"),
        ComboBox.shinyInput(
          style="width: 200px;",
          inputId = ns("gene_input"),
          autoComplete = 'on',
          autofill = TRUE,
          value = list(text = "Pf4"),
          # options = otp,
          options = list(
            list(key = "Pf4", text = "Pf4"),
            list(key = "Plek", text = "Plek"),
            list(key = "Cd34", text = "Cd34"),
            list(key = "Cbx7", text = "Cbx7")
          )
        )
      )
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(
        title = "Number of genes:",
        content = "15158",
        size = 4,
        style = "height: 75px; display: flex; align-items: center; justify-content: center;"
      ),
      makeCard(
        title = "Number of cells:",
        content = uiOutput(ns("text_cells")),
        size = 4,
        style = "height: 75px; display: flex; align-items: center; justify-content: center;"
      ),
      makeCard(
        title = "Number of cell types:",
        content =  uiOutput(ns("text_types")),
        size = 4,
        style = "height: 75px; display: flex; align-items: center; justify-content: center;"
      )
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(
        title = "Umap cell types",
        content = plotOutput(ns("Umap1"), height = 700),
        size = 6,
        style = "height: 750px;"
      ),
      makeCard(
        title = "Gene expression",
        content = plotOutput(ns("Umap2"), height = 700),
        size = 6,
        style = "height: 750px;"
      )
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(
        title = "Number of cells in each cell type",
        content = plotOutput(ns("barplot")),
        size = 6,
        style = "height: 450px;"
      ),
      makeCard(
        title = "Gene expression in each cell type",
        content = plotOutput(ns("violin_plot")),
        size = 6,
        style = "height: 450px;"
      )
    ),
    Stack(
      style = "background-color: #f3f2f1; padding: 12px 20px;",
      horizontal = TRUE,
      horizontalAlign = 'space-between',
      tokens = list(childrenGap = 20),
      Text(variant = "medium", "Built by Fabio Bedin with love for Elena.", block=TRUE),
      Text(variant = "medium", nowrap = FALSE, "link to papers."),
      Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data1 <- readRDS(file = here("app/static/combined.rds"))
    data2 <- readRDS(file = here("app/static/subset.rds"))
    
    
    dataset <- eventReactive(input$dropdown_input, {
    
      
      if(input$dropdown_input == "dataset1"){
        data <- data1
      } else{
        data <- data2
      }
      
      return(data)
      
    })
    
    output$text_cells <- renderUI({
      
      data <- dataset()
      
      nrow(data@meta.data)
      
    })
    
    output$text_types <- renderUI({
      
      data <- dataset()
      
      if(input$dropdown_input == "dataset1"){
        length(unique(data@meta.data$def_cluster))
      } else{
        length(unique(data@meta.data$celltype))
      }
      
    })
    
   
    output$Umap1 <- renderPlot({
      
      UMAP_cellType(dataset())
    })
    
    output$Umap2 <- renderPlot({
      
      gene <- input$gene_input$text
      
      FeaturePlot(
        dataset(),
        features = gene,
        pt.size = 0.8,
        order = T,
        cols = c("lightgrey", "brown")
      ) +  theme_bw() + theme(panel.grid = element_blank())
    })
    
    output$barplot <- renderPlot({
      
    Barplot_cellType(dataset(), input$dropdown_input)
      
    })
    
    output$violin_plot <- renderPlot({
      gene <- input$gene_input$text
      
      # if(input$dropdown_input == "dataset1"){
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
      
      VlnPlot(
        dataset(),
        features = gene,
        # cols = color,
        pt.size = 0) + NoLegend()
    })
  
    
  })
}
