#####################################
## Plots for patient satisfaction. ##
#####################################
# The purpose of this script is to create the plots of the distributions of
# Patient Satisfaction Scores and Stability index. Plots are created for 
# unstratified data and data stratified by profession (known as 'Care setting').
#
# This script is called within `WrAP_Explore.R`.


# Write the functions.
# ----
fnc__ps_SI_plots <-
  function(
    data = NULL # One of the `ls_churn_within_NHS` or `ls_churn_from_NHS` datasets.
    ,dataset_id = NULL # A character string of the dataset ID.
  )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    # Extract the set of professions in the data.
    roles <- unique( data[[1]]$`Care setting` )[-1]
    
    # Unstratified.
    # # Prepare data
    ps_data <-
      data[[1]] %>%
      dplyr::filter(
        stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      # Format the text of the profession values.
      dplyr::mutate(
        `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
      ) %>%
      # Collate the satisfaction variables.
      dplyr::select(
        `Org code`, `Care setting`, 
        ,contains( "satisfaction" ) ) %>%
      tidyr::pivot_longer(
        cols = contains('satisfaction')
        ,names_to = 'Question'
        ,values_to = 'Score'
      ) %>%
      tidyr::separate_wider_delim(
        cols = Question
        ,delim = "_"
        ,names = c(NA, "Q", NA, "Year")
      ) %>%
      dplyr::mutate( Year = as.double( Year ) ) %>%
      dplyr::filter( Year > 2022 ) %>%
      dplyr::distinct()
    
    ps_data <-
      data[[1]] %>%
      dplyr::filter(
        stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      dplyr::mutate(
        Year = dplyr::case_when(
          stringr::str_detect( year, "25" ) ~ 2024
          ,stringr::str_detect( year, "22" ) ~ 2022
          ,.default = 2023
        )
      ) %>%
      dplyr::filter( Year > 2022 ) %>%
      dplyr::distinct(
        `Org code`, `Care setting`, Year, `Stability index`
      ) %>%
      dplyr::left_join(
        ps_data
        ,by = join_by( Year, `Org code`, `Care setting` )
        ,relationship = "one-to-many"
      ) %>%
      # Remove Trusts with no satisfaction scores.
      dplyr::filter( !is.na( Score ) )
    
    
    # Make table of summary statistics.
    save_directory <- 
      paste0(
        "Tables/"
        ,var_of_interest
        ,"/"
        ,dataset_id
      )
    dir.create( file.path( getwd(), save_directory ), recursive = TRUE ) %>% suppressWarnings()
    ps_data %>%
      dplyr::reframe(
        min = min( `Stability index`, na.rm = T )
        ,median = median( `Stability index`, na.rm = T )
        ,mean = mean( `Stability index`, na.rm = T )            
        ,max = max( `Stability index`, na.rm = T )
        ,.by = c( `Care setting`, Q, Year )
      ) %>%
      dplyr::arrange( `Care setting`, Q, Year ) %>%
      write.csv( file =
                   paste0(
                     save_directory
                     ,"/"
                     ,janitor::make_clean_names( var_of_interest, case = "title" )
                     ,"_by_patient_satisfaction_question_summary_statistics.csv"
                   )
      ) %>%
      suppressWarnings()
    
    sumstat_plot_data <-
      ps_data %>%
      dplyr::filter( `Care setting` == "All care settings" ) %>%
      dplyr::reframe(
        class_median = median( `Stability index`, na.rm = TRUE )
        ,class_min = min( `Stability index`, na.rm = TRUE )
        ,class_max = max( `Stability index`, na.rm = TRUE )
        ,.by = c( Q, Year )
      )
    
    min_val <-
      min(
        ps_data %>%
          dplyr::filter( `Care setting` == "All care settings" ) %>%
          dplyr::select( `Stability index` )
        ,na.rm = TRUE
      )
    max_val <-
      max(
        ps_data %>%
          dplyr::filter( `Care setting` == "All care settings" ) %>%
          dplyr::select( `Stability index` )
        ,na.rm = TRUE
      )
    
    # # Plot satisfaction scores for all care settings, unstratfied.
    p <-
      ps_data %>%
      dplyr::filter( `Care setting` == "All care settings" ) %>%
      ggplot(
        aes(
          x = `Stability index`
          ,y = Score
        )
      ) +
      geom_point() +
      labs(
        title =
          paste0(
            'Distributions of Patient Satisfaction Scores\n'
            ,'and Stability index.'
          )
        ,subtitle =
          paste0(
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 Showing values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), ".\n"
            ,"\u2022 Red line shows the median."
          )
      ) +
      facet_grid(  Q ~ Year ) +
      geom_vline(
        data = sumstat_plot_data
        ,aes( xintercept = class_median )
        ,colour = "red"
      ) +
      theme(
        axis.text = element_text( size = 10 )
        ,plot.title = element_text( size = 20 )
        ,plot.subtitle = element_text( size = 15 )
      )
    ggsave(
      plot = p
      ,filename =
        paste0(
          "Plots/Stability index/"
          ,dataset_id
          ,"/Patient satisfaction/"
          ,"plot__ps_vs_SI_all_professions_"
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
        ps_data %>%
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
            ,.by = c( Q, Year )
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
          geom_point() +
          labs(
            title =
              paste0(
                'Distributions of Patient Satisfaction Scores\n'
                ,'and Stability index, for '
                ,roles[ i_role ], "."
              )
            ,subtitle =
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Showing values \u2265", round( min_val, 2 )
                ," and \u2264", round( max_val, 2 ), ".\n"
                ,"\u2022 Red line shows the median."
              )
          ) +
          facet_grid(  Q ~ Year ) +
          geom_vline(
            data = sumstat_plot_data
            ,aes( xintercept = class_median )
            ,colour = "red"
          ) +
          theme(
            axis.text = element_text( size = 10 )
            ,plot.title = element_text( size = 20 )
            ,plot.subtitle = element_text( size = 15 )
          )
        ggsave(
          plot = p
          ,filename =
            paste0(
              "Plots/Stability index/"
              ,dataset_id
              ,"/Patient satisfaction/plot__ps_vs_SI_"
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
fnc__ps_SI_plots( data = ls_churn_within_NHS, dataset_id = 'WITHIN_NHS' )
# fnc__ps_SI_plots( data = ls_churn_from_NHS, dataset_id = 'FROM_NHS' )
# ----
