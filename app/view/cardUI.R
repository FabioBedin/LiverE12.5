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