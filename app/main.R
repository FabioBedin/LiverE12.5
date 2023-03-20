box::use(
  shiny[moduleServer, NS, tags, plotOutput, renderPlot, eventReactive],
  shiny.fluent[fluentPage, Stack, Dropdown.shinyInput, Text, ComboBox.shinyInput]
)

box::use(
  app/view/cardUI[makeCard],
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
        ) ,
        tags$span(
          "se serve aggiungere un sottotitolo",
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
        ComboBox.shinyInput(
          style="width: 200px;",
          inputId = ns("gene_input"),
          autoComplete = 'on',
          autofill = TRUE,
          value = list(text = "gene1"),
          options = list(
            list(key = "gene1", text = "gene 1"),
            list(key = "test", text = "test"),
            list(key = "ciaocaio", text = "ciaociao")
          )
        )
      )
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(title="info1", content="numero geni", size = 4, style="height: 150px;"),
      makeCard(title="info2", content="numero cellule", size = 4, style="height: 150px;"),
      makeCard(title="info3", content="clusters", size = 4, style="height: 150px;")
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(title="card1", content="mettere grafico statico 1", size = 6, style="height: 750px;"),
      makeCard(title="card2", content="mettere grafico dinamico 1", size = 6, style="height: 750px;")
    ),
    Stack(
      style= "margin: 1rem 2rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(title="card3", content="mettere grafico statico 2", size = 6, style="height: 450px;"),
      makeCard(title="card4", content="mettere grafico dinamico 1", size = 6, style="height: 450px;")
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
    
    dataset <- eventReactive(input$dropdown_input, {
      
      if(input$dropdown_input == "dataset1"){
        data <- readRDS()
      }
      
    })
    
  })
}
