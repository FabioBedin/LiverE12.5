box::use(
  shiny[bootstrapPage, moduleServer, NS, renderText, tags, textOutput],
  shiny.fluent[fluentPage, Stack, Dropdown.shinyInput]
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
      # tokens = list(childrenGap = 10),
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
        Dropdown.shinyInput(
          style="width: 200px;",
          inputId = ns("dropdown_input"),
          value = "dataset1",
          options = list(
            list(key = "dataset1", text = "Dataset 1"),
            list(key = "dataset2", text = "Dataset 2")
          )
        )
      )
    ),
    Stack(
      style= "padding: 1rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(title="card1", content="ciao ciao ciao", style="height: 550px; flex-grow: 1;"),
      makeCard(title="card2", content="ciao", style="height: 550px; flex-grow: 1;")
    ),
    Stack(
      style= "padding: 1rem;",
      horizontal = TRUE,
      tokens = list(childrenGap = 15),
      makeCard(title="card3", content="ciao ciao ciao", style="height: 400px; flex-grow: 1;"),
      makeCard(title="card4", content="ciao", style="height: 400px; flex-grow: 1;")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderText("Hello!")
  })
}
