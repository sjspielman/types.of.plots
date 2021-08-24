#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      shinydashboard::dashboardPage(skin = "black",
                 shinydashboard::dashboardHeader(title = "Common types of plots"),
                 shinydashboard::dashboardSidebar(
                   shinydashboard::sidebarMenu(
                     shinydashboard::menuItem("About this app", tabName = "about"),
                     shinydashboard::menuItem("Histograms", tabName = "histogram", badgeLabel = "Continuous data", badgeColor = "blue"),
                     shinydashboard::menuItem("Density plots", tabName = "density", badgeLabel = "Continuous data", badgeColor = "blue"),
                     shinydashboard::menuItem("Boxplots", tabName = "boxplot", badgeLabel = "Continuous data", badgeColor = "blue"),
                     shinydashboard::menuItem("Violin plots", tabName = "violin", badgeLabel = "Continuous data", badgeColor = "blue"),
                     shinydashboard::menuItem("Strip/Jitter plots", tabName = "jitter", badgeLabel = "Continuous data", badgeColor = "blue"),
                     shinydashboard::menuItem("Barplots", tabName = "barplot", badgeLabel = "Discrete data", badgeColor = "orange"),
                     shinydashboard::menuItem("Scatterplots", tabName = "scatterplot", badgeLabel = "Relationships", badgeColor = "teal"),
                     shinydashboard::menuItem("Line plots", tabName = "line", badgeLabel = "Data over time", badgeColor = "fuchsia")
                        
                      )#sidebarMen
                    ),
                 shinydashboard::dashboardBody(
                   shinydashboard::tabItems(
                        # UI: About ------------------------------------
                        shinydashboard::tabItem(tabName = "about",
                                #br(),br(),
                                #  prettyCheckbox(
                                #    inputId = "hide_penguins",
                                #    label = "Click to hide the penguins", 
                                #    value = FALSE,
                                #    status = "warning",
                                #    shape = "curve"
                                #  ),
                                uiOutput("penguin_image"),
                                penguins_text(),
                                DT::DTOutput("penguin_table"),
                                br(),
                                overview_technical(),
                                p("App written by ", a("Stephanie J. Spielman.", href = "https://spielmanlab.github.io"), "The", a("source code", href = "https://github.com/sjspielman/ds4b.materials/tree/master/inst/apps/types_of_plots"), "is released under an MIT license."),
                                
                        ),
                        # UI: Histograms ---------------------------------------
                        shinydashboard::tabItem(tabName = "histogram",
                                display_top(histogram_dataviz(), histogram_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("histogram_variable", 
                                                "Select a numeric continuous variable to visualize. This variable will be placed along the x-axis.", 
                                                choices = numeric_choices),
                                    sliderInput("binwidth", "How wide (along the x-axis) should the histogram bins be?",
                                                value = 1, min = 0.1, max = 100, step = 0.5),
                                    selectInput("histogram_facet_variable", "Select a discrete variable to visualize numeric distributions across:",
                                                choices = discrete_choices),
                                    color_module_ui("histogram_color", 
                                                    label = "Should the faceted histograms be filled with the same color, or filled with a separate color for each category?")
                                  ), # sidebarPanel
                                  mainPanel(
                                    display_plot_code_module_ui("single_histogram", width = "700px"),
                                    display_plot_code_module_ui("faceted_histogram", width = "700px")
                                  ) # mainPanel
                                )
                        ),
                        # UI Boxplots -------------------------------------------------
                        shinydashboard::tabItem(tabName = "boxplot",
                                display_top(boxplot_dataviz(), boxplot_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("boxplot_y_variable", "Select a numeric continuous variable to visualize. This variable will be placed along the y-axis.",
                                                choices = numeric_choices,
                                                selected = "bill_length_mm"),
                                    selectInput("boxplot_x_variable", "Select a discrete variable to visualize numeric distributions across. This variable will be placed along the x-axis. There will be a separate boxplot for each category.",
                                                choices = discrete_choices,
                                                selected = "species"),   
                                    color_module_ui("boxplot_color", 
                                                    label = "Should the boxplots be filled with the same color, or filled with a separate color for each category?")
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("boxplot")
                                  )
                                )      
                        ),
                        # UI Density -------------------------------------------------
                        shinydashboard::tabItem(tabName = "density",
                                display_top(density_dataviz(), density_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("density_variable", "Select a numeric continuous variable to visualize. This variable will be placed along the x-axis.",
                                                choices = numeric_choices),
                                    
                                    selectInput("density_fill_variable", "Select a discrete variable to visualize numeric distributions across. There will be a separate density plot for each category.",
                                                choices = discrete_choices),
                                    colourpicker::colourInput("density_single_fill", "Color of the single density plot?", value = default_color)
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("single_density", width = "500px", height = "350px"),
                                    display_plot_code_module_ui("overlapping_density", width = "750px", height = "400px"),
                                    display_plot_code_module_ui("faceted_density", width = "750px", height = "400px"),
                                  )
                                )
                        ),
                        # UI Violin -------------------------------------------------
                        shinydashboard::tabItem(tabName = "violin",
                                display_top(violin_dataviz(), violin_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("violin_y_variable", "Select a numeric continuous variable to visualize. This variable will be placed along the y-axis.",
                                                choices = numeric_choices),
                                    selectInput("violin_x_variable", "Select a discrete variable to visualize numeric distributions across. This variable will be placed along the x-axis. There will be a separate violin plot for each category.",
                                                choices = discrete_choices),
                                    color_module_ui("violin_color", 
                                                    label = "Should the violin plots be filled with the same color, or filled with a separate color for each category?")
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("violin")
                                  )
                                )      
                        ),
                        # UI Jitter -------------------------------------------------
                        shinydashboard::tabItem(tabName = "jitter",
                                display_top(jitter_dataviz(), jitter_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("jitter_y_variable", "Select a numeric continuous variable to visualize. This variable will be placed along the y-axis.",
                                                choices = numeric_choices
                                    ),
                                    selectInput("jitter_x_variable", "Select a discrete variable to visualize numeric distributions across. This variable will be plced along the x-axis. There will be a separate strip/jitter plot for each category.", 
                                                choices = discrete_choices),    
                                    color_module_ui("jitter_color",
                                                    label = "Should the points all have the same color, or be colored separately for each category?"),
                                    radioButtons("jitter_setting", "Turn off the 'jittering' to see regular points and discover the importance of 'jittering.'", 
                                                 choices = jitter_choices)
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("jitter")
                                  )
                                )
                        ),
                        # UI barplot -------------------------------------------------
                        shinydashboard::tabItem(tabName = "barplot",
                                display_top(barplot_dataviz(), barplot_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("barplot_variable", "Select a discrete variable to visualize. This variable will be placed along the x-axis.",
                                                choices = discrete_choices
                                    ), 
                                    colourpicker::colourInput("barplot_single_fill", 
                                                              "What color should the single barplot's bars be filled with?", value = default_color),
                                    
                                    selectInput("barplot_second_variable", "Select a second discrete variable to visualize in comparison with the first discrete variable in a 'grouped barplot.'",
                                                choices = discrete_choices,
                                                selected = discrete_choices[2]
                                    ),
                                    selectInput("barplot_position", "How should the grouped barplot bars be styled?", 
                                                choices = position_choices
                                    )
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("barplot_single", width = "600px", height = "400px"),
                                    display_plot_code_module_ui("barplot_double", width = "700px", height = "400px")
                                  )
                                )
                        ),
                        # UI scatterplot -------------------------------------------------
                        shinydashboard::tabItem(tabName = "scatterplot",
                                display_top(scatter_dataviz(), scatter_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("scatter_x_variable", "Select a numeric variable to place along the x-axis. This is sometimes called the 'independent' or 'predictor' variable.",
                                                choices =numeric_choices),
                                    
                                    selectInput("scatter_y_variable", "Select a numeric variable to place along the y-axis.  This is sometimes called the 'response' variable.",
                                                choices =numeric_choices,
                                                selected = numeric_choices[2]),
                                    radioButtons("regression", "Display linear regression line ('line-of-best-fit')",
                                                 choices = regression_choices),
                                    
                                    ## The module is not well-suited here. it's ok.
                                    selectInput("scatter_color_style", "Should all points be the same color, or colored based on their value of another given variable?",
                                                choices = color_choices_scatter),
                                    conditionalPanel("input.scatter_color_style == 'Single color'",
                                                     { 
                                                       colourpicker::colourInput("scatter_single_color", "What single color should all points be?",
                                                                                 value = default_color)
                                                     }),
                                    conditionalPanel("input.scatter_color_style != 'Single color'",
                                                     { 
                                                       selectInput("scatter_color_variable", "What variable should determine point colors?",
                                                                   choices = list("Numeric variables" = numeric_choices, "Discrete variables" = discrete_choices)
                                                       )
                                                     }
                                    ) # conditionalpanel
                                  ), #sidebarpanel
                                  mainPanel(
                                    display_plot_code_module_ui("scatter")
                                  )
                                )
                        ),
                        # UI line plot ----------------------------------------
                        shinydashboard::tabItem(tabName = "line",
                                display_top(lineplot_dataviz(), lineplot_text()),
                                sidebarLayout(
                                  sidebarPanel(
                                    sliderInput("lineplot_point_size", "How large should the points in the line plot be? Set to 0 to remove points entirely.",
                                                value = 2, min = 0, max = 6, step = 0.5),
                                    sliderInput("lineplot_line_size", "How thick should the lines be? Set to 0 to remove lines entirely.",
                                                value = 1, min = 0, max = 3, step = 0.5)
                                  ),
                                  mainPanel(
                                    display_plot_code_module_ui("line")
                                  )
                                )
                        )
                      ) # tabItems
                    ) #dashboardBody
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'types.of.plots'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

