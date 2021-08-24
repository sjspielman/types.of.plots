#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



#' Thanks, stackoverflow!
#' https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)


numeric_choices <- c("bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g")
discrete_choices <- c("species",
                      "island", 
                      "sex", 
                      "year")

color_choices <- c("Single color", "Color each category separately")
color_choices_scatter <- c(color_choices[1], "Color points based on a varibble")
position_choices <- c("Side-by-side ('dodged')", "Stacked")
default_color <- "dodgerblue"
jitter_choices <- c("Jitter", "No jitter")
regression_choices <- c("Yes", "No")


display_top <- function(dataviz, text) {
  shiny::tagList(
    dataviz,
    shiny::fluidRow(
      shinydashboard::box(text, width = 12)
    ),
    shiny::h3("Explore:"),
  )
}



theme_custom_string <- function() {
  glue::glue(
    'ggplot2::theme_set(
      ggplot2::theme_light() +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
          axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
          strip.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
          strip.background = ggplot2::element_rect(fill = "grey30"),
          plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
          legend.key.size = ggplot2::unit(1, "cm"),
          legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
          legend.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
          legend.position = "bottom"
      )
    )'
  )
}
