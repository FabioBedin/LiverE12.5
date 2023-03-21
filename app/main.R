box::use(
  shiny[moduleServer, NS, tags, plotOutput, renderPlot, eventReactive, observeEvent, uiOutput, renderUI, tagList, h3, req, isolate],
  shiny.fluent[fluentPage, Stack, Dropdown.shinyInput, Text, SearchBox.shinyInput, DefaultButton.shinyInput],
  here[here],
  ggplot2[...],
  Seurat[...],
  waiter[useWaiter, Waiter, waiterShowOnLoad, spin_solar, waiter_hide, waiterOnBusy],
  graphics[text], 
  gargoyle[init, trigger, watch]
)

box::use(
  app/view/cardUI[makeCard, info_box, makeCard_gene],
)

box::use(
  app/logic/umap_celltypes[UMAP_cellType, Barplot_cellType, violin_plot],
)


loading_screen <- tagList(
  spin_solar(), 
  h3("Loading App", style = "color: white;")
)

loading_data <- tagList(
  spin_solar(), 
  h3("Loading dataset", style = "color: white;")
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    useWaiter(),
    waiterShowOnLoad(html = loading_screen, color ="#78c679"),
    waiterOnBusy(html = loading_data, color ="#78c679"),
    Stack(
      style= "padding: 2rem; justify-content: space-around;",
      horizontal = TRUE,
      tags$div(
        style= "display: flex; align-items: end; justify-content: center;",
        tags$span(
          "Mouse liver E12.5",
          class = "ms-fontSize-42 ms-fontWeight-semibold",
          style = "color: #323130"
        ),
        tags$span(
          "scRNA-seq dataset from a whole mouse E12.5 liver",
          class = "ms-fontSize-16 ms-fontWeight-regular",
          style = "color: #605E5C; margin-bottom: 10px; margin-left: 0.5rem;"
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
            list(key = "dataset1", text = "Liver E12.5"),
            list(key = "dataset2", text = "Subset")
          )
        ),
        Text(variant = "large", "Gene: ", style = "margin: 0 1rem"),
        SearchBox.shinyInput(inputId = ns("gene_input"), value = "Rhd"),
        DefaultButton.shinyInput(inputId = ns("search"), text = "Search", style = "margin-left: 1rem")
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
      makeCard_gene(
        title = "Gene expression of",
        gene = uiOutput(ns("gene_selected")),
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
        title = "Number of cells for cell type",
        content = plotOutput(ns("barplot")),
        size = 6,
        style = "height: 450px;"
      ),
      makeCard_gene(
        title = "Gene expression of",
        gene = uiOutput(ns("gene_selected2")),
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
      Stack(
        horizontal = TRUE,
        Text(variant = "medium", "Built by ", block=TRUE, style = "margin-right: 0.4rem"),
        tags$a(href="https://github.com/FabioBedin", "Fabio Bedin.")
      ),
      Stack(
        horizontal = TRUE,
        Text(variant = "medium", "link to GEO dataset:", block=TRUE, style = "margin-right: 0.4rem"),
        tags$a(href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE180050", "GSE180050")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    init("genes")
    
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
    
    
    
    output$gene_selected <- renderUI({
      watch("genes")
      
      isolate(input$gene_input)
    })
    
    output$gene_selected2 <- renderUI({
      watch("genes")
     
      isolate(input$gene_input)
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
      
      watch("genes")
      
      gene <- isolate(input$gene_input)
      data <- dataset()
      
      
      if(gene %in% row.names(data@assays$RNA)){
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
      }else{
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, "The selected gene is not present in the dataset", cex = 3, col = "black")
      }
      
     
      
    })
    
    output$barplot <- renderPlot({
      
    Barplot_cellType(dataset(), input$dropdown_input)
      
    })
    
    output$violin_plot <- renderPlot({
      
      watch("genes")
      gene <- isolate(input$gene_input)
      data <- dataset()
      
      
      if(gene %in% row.names(data@assays$RNA)){
        violin_plot(dataset(), gene, input$dropdown_input)
      }else{
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, "The selected gene is not present in the dataset", cex = 3, col = "black")
      }
      
      
    })
    
    observeEvent(input$search, {
      req(input$gene_input)
      
      trigger("genes")
    })
    
    waiter_hide()
  })
}
