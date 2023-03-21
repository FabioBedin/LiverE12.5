box::use(
  shiny[NS, renderText, tags],
  glue[glue],
  shiny.fluent[Stack, Text]
)


#' @export
makeCard <- function(title, content, size = 12, style = "") {
  tags$div(
    class = glue("card ms-depth-8 ms-sm12 ms-xxxl{size}"),
    style = glue("padding: 1rem; background-color: #fff; {style}"),
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "xLarge", title, block = TRUE),
      content
    )
  )
}

#' @export
info_box <- function(title, content, size = 12, style = "") {
  tags$div(
    class = glue("card ms-depth-16 ms-sm12 ms-xxxl{size}"),
    style = glue("padding: 1rem; background-color: #78c67980; {style}"),
    Stack(
      tokens = list(childrenGap = 5),
      Stack(
        horizontal = TRUE,
        style = "display: flex; align-items: center; justify-content: center;",
        Text(variant = "xxLarge", title, block = TRUE),
        Text(variant = "xxLarge", content, block = TRUE, style="margin-left: 1rem;"),
      )
    )
  )
}