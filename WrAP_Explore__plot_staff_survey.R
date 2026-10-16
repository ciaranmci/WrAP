#############################
## Plots for staff survey. ##
#############################
# The purpose of this script is to create the plots of the distributions of
# staff-survey scores and stability index. Plots are created for 
# unstratified data and data stratified by profession (known as 'Care setting').
#
# This script is called within `WrAP_Explore.R`.


# Write the functions.
# ----
fnc__ss_SI_plots <-
  function(
    data = NULL # One of the `ls_churn_within_NHS` or `ls_churn_from_NHS` datasets.
    ,var_of_interest = NULL # A character string of the variate of interest.
    ,dataset_id = NULL # A character string of the dataset ID.
  )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( var_of_interest ) ){ stop( "The 'var_of_interest' arguement was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    # Extract the set of professions in the data.
    roles <- unique( data[[1]]$`Care setting` )[-1]
    
    # Unstratified.
    # # Prepare data
    ss_data <-
      data[[1]] %>%
      dplyr::filter(
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      dplyr::filter( year_end > 2022 ) %>%
      # Collate the staff-survey variables.
      dplyr::select(
        `Org code`, `Care setting`, year_end, `Stability index`
        ,contains( "ss_" ) ) %>%
      tidyr::pivot_longer(
        cols = contains('ss_')
        ,names_to = 'Question'
        ,values_to = 'Score'
        ) %>%
      # Shorten the names of the questions because, given the context, we know
      # the questions are from the staff survey.
      dplyr::mutate(
        Question = stringr::str_split_i( string = Question, pattern = "_", i = -1)
      )
    
    # Make table of summary statistics.
    save_directory <- 
      paste0(
        "Tables/"
        ,var_of_interest
        ,"/"
        ,dataset_id
      )
    dir.create( file.path( getwd(), save_directory ), recursive = TRUE ) %>% suppressWarnings()
    ss_data %>%
      dplyr::reframe(
        min = min( `Stability index`, na.rm = T )
        ,median = median( `Stability index`, na.rm = T )
        ,mean = mean( `Stability index`, na.rm = T )            
        ,max = max( `Stability index`, na.rm = T )
        ,.by = c( `Care setting`, Question, year_end )
      ) %>%
      dplyr::arrange( `Care setting`, Question, year_end ) %>%
      write.csv( file =
                   paste0(
                     save_directory
                     ,"/"
                     ,janitor::make_clean_names( var_of_interest, case = "title" )
                     ,"_by_staff_survey_question_summary_statistics.csv"
                   )
      ) %>%
      suppressWarnings()
    
    sumstat_plot_data <-
      ss_data %>%
      dplyr::filter( `Care setting` == "All care settings" ) %>%
      dplyr::reframe(
        class_median = median( `Stability index`, na.rm = TRUE )
        ,class_min = min( `Stability index`, na.rm = TRUE )
        ,class_max = max( `Stability index`, na.rm = TRUE )
        ,.by = c( Question, year_end )
      )
    
    min_val <-
      min(
        ss_data %>%
          dplyr::select( `Stability index` )
        ,na.rm = TRUE
      )
    max_val <-
      max(
        ss_data %>%
          dplyr::select( `Stability index` )
        ,na.rm = TRUE
      )
    
    # # Plot satisfaction scores for all care settings, unstratfied.
    p <-
      ss_data %>%
      ggplot(
        aes(
          x = `Stability index`
          ,y = Score
        )
      ) +
      geom_point( size = 0.5 ) +
      labs(
        title =
          paste0(
            'Distributions of Staff Survey Scores\nand Stability index.'
          )
        ,subtitle =
          paste0(
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 Showing stability-index values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), ".\n"
            ,"\u2022 Red line shows the median."
          )
      ) +
      facet_grid(  Question ~ year_end ) +
      geom_vline(
        data = sumstat_plot_data
        ,aes( xintercept = class_median )
        ,colour = "red"
      ) +
      scale_y_continuous( breaks = c( 5, 10 ) ) +
      theme(
        axis.text = element_text( size = 10 )
        ,plot.title = element_text( size = 20 )
        ,plot.subtitle = element_text( size = 15 )
      )
    ggsave(
      plot = p
      ,filename =
        paste0(
          "Plots/"
          ,var_of_interest
          ,"/"
          ,dataset_id
          ,"/Staff survey/"
          ,"plot__ss_vs_SI_all_professions_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 20
      ,height = 20
      ,units = "cm"
    )
    
    
    # Make the plot for each AHP role.
    # ## Work through each profession.
    for( i_role in 1:length( roles ) )
    {
      i_plot_data <-
        ss_data %>%
        dplyr::filter( `Care setting` == roles[ i_role ] ) %>%
        dplyr::filter( !is.na( `Stability index` ) )
      
      
      if ( nrow( i_plot_data ) > 0 )
      {
        sumstat_plot_data <-
          i_plot_data %>%
          dplyr::reframe(
            class_median = median( `Stability index`, na.rm = TRUE )
            ,class_min = min( `Stability index`, na.rm = TRUE )
            ,class_max = max( `Stability index`, na.rm = TRUE )
            ,.by = c( Question, year_end )
          )
        
        min_val <-
          min(
            i_plot_data$`Stability index`
            ,na.rm = TRUE
          )
        max_val <-
          max(
            i_plot_data$`Stability index`
            ,na.rm = TRUE
          )
        
        p <-
          i_plot_data %>%
          ggplot(
            aes(
              x = `Stability index`
              ,y = Score
            )
          ) +
          geom_point( size = 0.5 ) +
          labs(
            title =
              paste0(
                'Distributions of Staff Survey Scores\n'
                ,'and Stability index, for '
                ,roles[ i_role ], "."
              )
            ,subtitle =
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Showing stability-index values \u2265", round( min_val, 2 )
                ," and \u2264", round( max_val, 2 ), ".\n"
                ,"\u2022 Red line shows the median."
              )
          ) +
          facet_grid(  Question ~ year_end ) +
          geom_vline(
            data = sumstat_plot_data
            ,aes( xintercept = class_median )
            ,colour = "red"
          ) +
          scale_y_continuous( breaks = c( 5, 10 ) ) +
          theme(
            axis.text = element_text( size = 10 )
            ,plot.title = element_text( size = 20 )
            ,plot.subtitle = element_text( size = 15 )
          )
        ggsave(
          plot = p
          ,filename =
            paste0(
              "Plots/"
              ,var_of_interest
              ,"/"
              ,dataset_id
              ,"/Staff survey/plot__ss_vs_SI_"
              ,gsub(roles[ i_role ], pattern = "/", replacement = " & ")
              ,"_"
              ,dataset_id
              ,"_dataset.png"
            )
          ,dpi = 300
          ,width = 20
          ,height = 20
          ,units = "cm"
        )
        
      }
    }
  }
# ----

# Apply the functions.
# ----
fnc__ss_SI_plots( data = ls_churn_within_NHS, var_of_interest = "Stability index", dataset_id = 'WITHIN_NHS' )
# fnc__ss_SI_plots( data = ls_churn_from_NHS, var_of_interest = "Stability index", dataset_id = 'FROM_NHS' )
# ----
