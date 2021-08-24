#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  ## Server: Overview Panel------------------------------------ 
  output$penguin_table <- DT::renderDT({
    DT::datatable(palmerpenguins::penguins, 
              options = list(rowCallback = htmlwidgets::JS(rowCallback)))
    
  })
  output$penguin_image <- renderUI({
    #if (input$hide_penguins)
    #{
    #  tagList()
    #} else {
    tagList(
      div(style="display:block; text-align: center;",
          tags$img(src = "www/img/lter_penguins.png", width = "45%"),
          div(style = "font-size:0.8em;", 
              p("Artwork by", a("@allison_horst", href="https://github.com/allisonhorst/palmerpenguins"))
          )
      )
    )
    #}
  })
  
  output$penguins_year_factor <- renderText({
    glue::glue(
      "penguins <- palmerpenguins::penguins %>% 
              dplyr::mutate(year = as.factor(year))
        "
    )
  })
  output$theme_custom_string <- renderText({
    theme_custom_string()
  })
  
  
  
  ## Server: Histograms Panel ---------------------------------
  histogram_color <- color_module_server("histogram_color")
  display_plot_code_module_server("single_histogram", plot_string = reactive(histogram_plot()$single))
  display_plot_code_module_server("faceted_histogram", plot_string = reactive(histogram_plot()$faceted))
  
  
  histogram_plot <- reactive({
    eval(parse(text = theme_custom_string())) # just plop it _somewhere_
    build_histogram_string(
      list(color_style   = histogram_color()$color_style,
           x             = input$histogram_variable,
           binwidth      = input$binwidth,
           fill          = paste0('"', histogram_color()$single_color, '"'),
           facet         = input$histogram_facet_variable,
           title_single  = glue::glue('"Histogram of all `{input$histogram_variable}` values"'),
           sub_single    = glue::glue('"All values in `{input$histogram_variable}` are shown in this histogram."'),
           title_faceted = glue::glue('"Faceted histogram of `{input$histogram_variable}` values across `{input$histogram_facet_variable}` values"'),
           sub_faceted   = glue::glue('"A subset of values `{input$histogram_variable}` is shown in each panel, also known as a facet."')
      )
    )
  })
  
  
  
  ## Server: Boxplots Panel ---------------------------------
  boxplot_color <- color_module_server("boxplot_color")
  display_plot_code_module_server("boxplot", plot_string = reactive(boxplot_string()))
  
  boxplot_string <- reactive({
    build_boxplot_violin_string(
      list(x = input$boxplot_x_variable,
           y = input$boxplot_y_variable,
           fill = paste0('"',boxplot_color()$single_color,'"'),
           color_style = boxplot_color()$color_style,
           title = glue::glue('"Boxplot of `{input$boxplot_y_variable}` values across `{input$boxplot_x_variable}` values"')
      ),
      geom = "geom_boxplot"
    )
  })
  
  ## Server: Density Panel ---------------------------------
  display_plot_code_module_server("single_density", plot_string = reactive(density_string()$single))
  display_plot_code_module_server("overlapping_density", plot_string = reactive(density_string()$overlapping))
  display_plot_code_module_server("faceted_density", plot_string = reactive(density_string()$faceted))
  
  density_string <- reactive({
    build_density_string(
      list(
        x = input$density_variable,
        fill = paste0('"',input$density_single_fill,'"'),
        fillby = input$density_fill_variable,
        title_single = glue::glue('"Density plot of all `{input$density_variable}` values"'),
        sub_single   = glue::glue('"All values of `{input$density_variable}` are shown in this plot."'),
        title_overlapping = glue::glue('"Overlapping density plot of `{input$density_variable}` values across `{input$density_fill_variable}` values"'),
        sub_overlapping = '"This plot has a single x-axis for all categories, and categories are distinguished by color. Without colors, we could not interpret this plot."',
        title_faceted = glue::glue('"Faceted density plot of `{input$density_variable}` values across `{input$density_fill_variable}` values"'),
        sub_faceted = '"This plot has a separate x-axis for each category. Colors also distinguish categories, but they are not necessary to interpret the plot."'       
      )
    )
  })
  
  
  ## Server: Violin Panel ---------------------------------
  violin_color <- color_module_server("violin_color")
  display_plot_code_module_server("violin", plot_string = reactive(violin_string()))
  
  violin_string <- reactive({
    build_boxplot_violin_string(
      list(x = input$violin_x_variable,
           y = input$violin_y_variable,
           fill = paste0('"',violin_color()$single_color,'"'),
           color_style = violin_color()$color_style,
           title = glue::glue('"Violin plot of `{input$violin_y_variable}` values across `{input$violin_x_variable}` values"')
      ),
      geom = "geom_violin"
    )
  })
  
  
  ## Server: Strip (jitter) Panel ---------------------------------
  jitter_color <- color_module_server("jitter_color")
  display_plot_code_module_server("jitter", plot_string = reactive(jitter_string()))
  
  jitter_string <- reactive({
    build_jitter_string(
      list(x = input$jitter_x_variable,
           y = input$jitter_y_variable,
           color = paste0('"',jitter_color()$single_color,'"'),
           color_style = jitter_color()$color_style,
           jitter_setting = input$jitter_setting,
           title = glue::glue('"Strip/jitter plot of `{input$jitter_y_variable}` values across `{input$jitter_x_variable}` values"')
      )
    )
  })
  
  
  ## Server: Sina Panel ---------------------------------
  # sina_color <- color_module_server("sina_color")
  # display_plot_code_module_server("sina", plot_string = reactive(sina_string()))
  # 
  # sina_string <- reactive({
  #   build_sina_string(
  #     list(x = input$sina_x_variable,
  #          y = input$sina_y_variable,
  #          color = paste0('"',sina_color()$single_color,'"'),
  #          color_style = sina_color()$color_style,
  #          title = glue::glue('"Sina plot of `{input$sina_y_variable}` values across `{input$sina_x_variable}` values"')
  #     )
  #   )   
  # })
  # 
  ## Server: Barplot ----------------------------------
  display_plot_code_module_server("barplot_single", plot_string = reactive(barplot_string()$single))
  display_plot_code_module_server("barplot_double", plot_string = reactive(barplot_string()$double))
  #display_plot_code_module_server("barplot_errorbar", plot_string = reactive(barplot_string()$errorbar))
  
  barplot_string <- reactive({
    build_barplot_string(
      list(x = input$barplot_variable,
           fill = paste0('"',input$barplot_single_fill,'"'),
           fillby = input$barplot_second_variable,
           y = input$sina_y_variable,
           position = input$barplot_position,
           title_single = '"The number of penguin observations in each category."',
           title_double = '"Grouped barplot of the number of penguins in each combination of categories."'
      )
    )
  })
  
  
  
  ## Server: Scatterplot ----------------------------------
  display_plot_code_module_server("scatter", plot_string = reactive(scatter_string()))
  
  scatter_string <- reactive({
    build_scatter_string(
      list(x = input$scatter_x_variable,
           y = input$scatter_y_variable,
           color = paste0('"',input$scatter_single_color,'"'),
           colorby = input$scatter_color_variable,
           color_style = input$scatter_color_style,
           regression = input$regression,
           title = glue::glue('"Scatter plot of `{input$scatter_y_variable}` across `{input$scatter_x_variable}` values"'),
           subtitle = glue::glue('"Notice how the regression line sometimes changes direction when you color by a discrete variable - read on!"')
      )
    )   
  })     
  
  ## Server: Scatterplot ----------------------------------
  display_plot_code_module_server("line", plot_string = reactive(line_string()))
  
  line_string <- reactive({
    build_line_string(
      list(point_size = input$lineplot_point_size,
           line_size = input$lineplot_line_size)
    )   
  })    

}
