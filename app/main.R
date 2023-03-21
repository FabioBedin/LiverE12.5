box::use(
  shiny[moduleServer, NS, tags, plotOutput, renderPlot, eventReactive, observeEvent, uiOutput, renderUI],
  shiny.fluent[fluentPage, Stack, Dropdown.shinyInput, Text, ComboBox.shinyInput, SearchBox.shinyInput],
  here[here],
  ggplot2[...],
  Seurat[...]
)

box::use(
  app/view/cardUI[makeCard, info_box],
)

box::use(
  app/logic/umap_celltypes[UMAP_cellType, Barplot_cellType, violin_plot],
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
          "Mouse liverE12.5",
          class = "ms-fontSize-42 ms-fontWeight-semibold",
          style = "color: #323130"
        ),
        tags$span(
          "scRNA-seq dataset from a whole mouse E12.5 liver",
          class = "ms-fontSize-14 ms-fontWeight-regular",
          style = "color: #605E5C; margin: 14px;"
        )
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
        # ComboBox.shinyInput(
        #   style="width: 200px;",
        #   inputId = ns("gene_input"),
        #   autoComplete = 'on',
        #   autofill = TRUE,
        #   value = list(text = "Rhd"),
        #   options = geni
        # )
        SearchBox.shinyInput(inputId = ns("gene_input"), value = "Rhd")
      )
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      info_box(
        title = "Number of cells:",
        content = uiOutput(ns("text_cells")),
        size = 4,
        style = "height: 75px; display: flex; align-items: center; justify-content: center;"
      ),
      info_box(
        title = "Number of genes:",
        content = "15158",
        size = 4,
        style = "height: 75px; display: flex; align-items: center; justify-content: center;"
      ),
      info_box(
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
        title = "UMAP cell types",
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
      length(unique(data@meta.data$def_cluster))
      
    })
    
   
    output$Umap1 <- renderPlot({
      
      UMAP_cellType(dataset(), input$dropdown_input)
    })
    
    output$Umap2 <- renderPlot({
      
      gene <- input$gene_input
      
      FeaturePlot(
        dataset(),
        features = gene,
        pt.size = 2.3,
        order = T,
        cols = c("lightgrey", "brown")) + 
        labs(y = "UMAP 2", x = "UMAP 1") + 
        theme_bw() + 
        theme(panel.grid = element_blank(),
              plot.title = element_blank(),
              axis.text = element_text(size=15),
              axis.title = element_text(size=15), 
              legend.direction = "horizontal",
              legend.position = c(0.1,0.95))
    })
    
    output$barplot <- renderPlot({
      
    Barplot_cellType(dataset(), input$dropdown_input)
      
    })
    
    output$violin_plot <- renderPlot({
      gene <- input$gene_input
      
      violin_plot(dataset(), gene, input$dropdown_input)
    })
  
    
  })
}
