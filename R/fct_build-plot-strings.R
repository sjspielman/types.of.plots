# Functions to build plot strings

build_scatter_string <- function(args)
{
  
  if (args$color_style == color_choices[1])
  {
    glue::glue("ggplot2::ggplot(penguins) + 
                ggplot2::aes(x = {args$x},
                             y = {args$y}) + 
    ") -> plot_string
  } else {
    glue::glue("
      ggplot2::ggplot(penguins) + 
        ggplot2::aes(x = {args$x},
                     y = {args$y},
                     color = {args$colorby}) + 
    ") -> plot_string
    
  }  
  
  
  
  if (args$color_style == color_choices[1])
  {
    glue::glue(
      plot_string,
      "
    ggplot2::geom_point(color = {args$color},
               size  = 2) + 
      ") -> plot_string    
  } else {
    glue::glue(
      plot_string,
      "
      ggplot2::geom_point(size  = 2) + 
    ") -> plot_string       
  }
  if (args$regression == regression_choices[1])
  {
    glue::glue(
      plot_string,
      "
      ggplot2::geom_smooth(method = 'lm') + 
      ") -> plot_string
  } 
  glue::glue(
    plot_string,
    "
    ggplot2::labs(title    = {args$title},
                  subtitle = {args$subtitle}) + 
    ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)))") -> plot_string
  plot_string
}


build_barplot_string <- function(args)
{
  glue::glue(
    "penguins %>%
          # Remove potential NAs for demonstrating visualization
          tidyr::drop_na({args$x}) %>%
          ggplot2::ggplot() +
              ggplot2::aes(x = {args$x}) + 
              ggplot2::geom_bar(fill = {args$fill}, color = 'black') +
              ggplot2::labs(title = {args$title_single})") -> single
  
  
  glue::glue(
    "penguins %>%
          # Remove potential NAs for demonstrating visualization
          tidyr::drop_na({args$x}, {args$fillby}) %>%
          ggplot2::ggplot() + 
            ggplot2::aes(x = {args$x},
                        fill = {args$fillby}) +
    ") -> double
  
  if (args$position == position_choices[1])
  {
    glue::glue(
      double, 
      "
        ggplot2::geom_bar(color = 'black',
                          position = ggplot2::position_dodge(preserve = 'single')) +
        ggplot2::labs(title = {args$title_double})"
    ) -> double
  } else {
    glue::glue(
      double, 
      "
        ggplot2::geom_bar(color = 'black') +
        ggplot2::labs(title = {args$title_double})"
    ) -> double   
  }
  glue::glue(
    "penguins %>%
     tidyr::drop_na(flipper_length_mm, species) %>%
     dplyr::group_by(species) %>%
     dplyr::summarize(mean_flipper = mean(flipper_length_mm), 
                      sd_flipper   = sd(flipper_length_mm)) %>%
     ggplot2::ggplot(
      ggplot2::aes(x  = species, 
                   y  = mean_flipper, 
                  fill = species)) + 
     ggplot2::geom_col(color = 'black') + 
     ggplot2::geom_errorbar(ggplot2::aes(ymax = mean_flipper + sd_flipper/2, 
                       ymin =  mean_flipper - sd_flipper/2), 
                   width = 0.05, size=1) + 
     ggplot2::ylab('Mean +/- SD of flipper length (mm)') + 
     ggplot2::theme(axis.title.y = ggplot2::element_text(size=12))") -> errorbar
  
  list("single" = single, 
       "double" = double,
       "errorbar" = errorbar)
}


build_sina_string <- function(args)
{
  plot_string <- glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$x}) %>%
            ggplot2::ggplot() +
              ggplot2::aes(x = {args$x},
                           y = {args$y}) +
    ") 
  
  if (args$color_style == color_choices[1])
  {
    plot_string <- glue::glue(
      plot_string, 
      "
          ggforce::geom_sina(color = {args$color},
                    size  = 2) + 
      ")
  } else {
    plot_string <- glue::glue(
      plot_string, 
      "
          ggforce::geom_sina(color = {args$x},
                    size  = 2) + 
      ")
  }
  plot_string <- glue::glue(
    plot_string, 
    "
    ggplot2::labs(title = {args$title})"
  )
  plot_string
}


build_jitter_string <- function(args)
{
  
  if (args$jitter_setting == jitter_choices[1])
  {
    geom <- "ggplot2::geom_jitter("
  } else {
    geom <- "ggplot2::geom_point("
    
  }
  plot_string <- glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$x}) %>%
            ggplot2::ggplot() +
              ggplot2::aes(x = {args$x},
                           y = {args$y}) +
    ") 
  
  if (args$color_style == color_choices[1])
  {
    plot_string <- glue::glue(
      plot_string, 
      "
          {geom}color = {args$color}, 
                      size  = 2, 
                      width = 0.2) + 
      ")
  } else {
    plot_string <- glue::glue(
      plot_string, 
      "
          {geom}ggplot2::aes(color = {args$x}),
                      size = 2,
                      width = 0.2) + 
      ")
  }
  plot_string <- glue::glue(
    plot_string, 
    "
    ggplot2::labs(title = {args$title})"
  )
  plot_string
}


build_boxplot_violin_string <- function(args, geom)
{
  plot_string <- glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$x}) %>%
            ggplot2::ggplot() +
              ggplot2::aes(x = {args$x},
                           y = {args$y}) +
    ") 
  
  if (args$color_style == color_choices[1])
  {
    plot_string <- glue::glue(
      plot_string, 
      "
          ggplot2::{geom}(fill = {args$fill}) + 
      ")
  } else {
    plot_string <- glue::glue(
      plot_string, 
      "
          ggplot2::{geom}(ggplot2::aes(fill = {args$x})) + 
      ")
  }
  plot_string <- glue::glue(
    plot_string, 
    "
    ggplot2::labs(title = {args$title})"
  )
  plot_string
}

build_histogram_string <- function(args)
{
  
  # single
  glue::glue("ggplot2::ggplot(penguins) + 
                 ggplot2::aes(x = {args$x}) + 
                 ggplot2::geom_histogram(binwidth = {args$binwidth}, fill = {args$fill}, color = 'black') +
                 ggplot2::labs(title    = {args$title_single},
                               subtitle = {args$sub_single})") -> single
  
  # faceted
  faceted <- glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$facet}) %>%
            ggplot2::ggplot() +
              ggplot2::aes(x = {args$x}) +
          "
  )
  
  if (args$color_style == color_choices[1])
  {
    
    faceted <- glue::glue(
      faceted, 
      "
        ggplot2::geom_histogram(fill     = {args$fill},
                                color    = 'black',
                                binwidth = {args$binwidth}) + 
             ")
  } else {
    faceted <- glue::glue(
      faceted, 
      "
        ggplot2::geom_histogram(ggplot2::aes(fill = {args$facet}),
                                color    = 'black', 
                                binwidth = {args$binwidth}) +
            ")
  }
  
  faceted <- glue::glue(
    faceted, 
    "  
      ggplot2::facet_wrap(ggplot2::vars({args$facet})) +
      ggplot2::labs(title = {args$title_faceted},
           subtitle = {args$sub_faceted}) + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.8)))")
  
  list("single" = single,
       "faceted" = faceted)
  
}

build_density_string <- function(args)
{
  
  
  # single
  glue::glue("ggplot2::ggplot(penguins) + 
                ggplot2::aes(x = {args$x}) + 
                ggplot2::geom_density(fill = {args$fill}) +
                 ggplot2::labs(title    = {args$title_single},
                               subtitle = {args$sub_single})") -> single
  
  # overlapping
  glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$fillby}) %>%
            ggplot2::ggplot() + 
            ggplot2::aes(x    = {args$x},
                       fill = {args$fillby}) +
              ggplot2::geom_density(alpha = 0.7) +
              ggplot2::labs(title = {args$title_overlapping},
                   subtitle = {args$sub_overlapping})"
  ) -> overlapping  
  
  # faceted
  glue::glue(
    "penguins %>%
            # Remove potential NAs for demonstrating visualization
            tidyr::drop_na({args$fillby}) %>%
            ggplot2::ggplot() + 
              ggplot2::aes(x = {args$x},
                          fill = {args$fillby}) +
              ggplot2::geom_density() +
              ggplot2::facet_wrap(ggplot2::vars({args$fillby})) +
              ggplot2::labs(title = {args$title_faceted},
                 subtitle = {args$sub_faceted})"
  ) -> faceted  
  
  
  list("single" = single,
       "overlapping" = overlapping,
       "faceted" = faceted)
  
}




build_line_string <- function(args)
{
  glue::glue(
    "# Data is first modified to be the mean bill length per species, per year, and then plotted:
penguins %>%
   dplyr::group_by(species, year) %>%
   dplyr::summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE)) %>%
   dplyr::ungroup() %>%
   # Now, plot the mean bill lengths over time:
   ggplot2::ggplot() + 
    ggplot2::aes(x = year,
              y = mean_bill_length,
              color = species,
              group = species) +
     ggplot2::geom_point(size = {args$point_size}) +
     ggplot2::geom_line(size = {args$line_size}) +
     ggplot2::labs(title = 'Line plot of mean bill length over time, shown separately for each species.')"
  )
}