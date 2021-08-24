#' Select colors in UI
#' @import shiny
color_module_ui <- function(id, label = "Color all by same color or based on category?" ) {
  ns <- NS(id)
  tagList(
    selectInput(ns("color_style"), label = label,
                choices = color_choices # Single color
                
    ),
    conditionalPanel("input.color_style == 'Single color'", ns = ns, 
                     { 
                       colourpicker::colourInput(ns("single_color"), 
                                                 "Choose color:",
                                                 default_color)
                     }
    )
  )
}

#' Select colors in server
#' @import shiny
color_module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::reactive({
        list(
          color_style  = input$color_style,
          single_color = input$single_color
        )
      })
    }
  ) 
}


#' Display the plotting code in UI
#' @import shiny
display_plot_code_module_ui <- function(id, width = "600px", height = "400px"){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(1,shinyWidgets::dropdownButton(
        h3("Code:"),
        verbatimTextOutput(ns("plot_code")),
        circle = FALSE, status = "warning",
        icon = icon("gear"), width = "600px"
      )),
      column(11, plotOutput(ns("plot"), width = width, height = height))
    ), # fluidRow
    br()
  )
}

#' Display the plotting code in server
#' @import shiny
display_plot_code_module_server <- function(id, plot_string)
{
  ## MUST REFER TO plot_string as plot_string() (not in function definition, but in body)
  # Otherwise module won't be reactive.
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$plot <- shiny::renderPlot({
        eval(parse(text = plot_string()))
      })
      output$plot_code <- shiny::renderText({plot_string()})
      
    }
  )
}
