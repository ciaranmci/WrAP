#########################
## Plots for Rurality. ##
#########################
# The purpose of this script is to create the plots of the distributions of
# rurality score and Stability index. Plots are created for unstratified data
# and data stratified by profession (known as 'Care setting').
#
# This script is called within `WrAP_Explore.R`.


# Write the functions.
# ----
fnc__rurality_SI_plots <-
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
    
    # Make table of summary statistics.
    save_directory <- 
      paste0(
        "Tables/"
        ,var_of_interest
        ,"/"
        ,dataset_id
      )
    dir.create( file.path( getwd(), save_directory ), recursive = TRUE ) %>% suppressWarnings()
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
      # Remove Trusts with no `RUC21 settlement class`.
      dplyr::filter( !is.na( `RUC21 settlement class` ) ) %>%
      # Calculate statistics.
      dplyr::reframe(
        min = min( `Stability index`, na.rm = T )
        ,median = median( `Stability index`, na.rm = T )
        ,mean = mean( `Stability index`, na.rm = T )            
        ,max = max( `Stability index`, na.rm = T )
        ,.by = c( `Care setting`, `RUC21 settlement class`, year )
      ) %>%
      dplyr::arrange( `Care setting`, `RUC21 settlement class`, year ) %>%
      write.csv( file =
                   paste0(
                     save_directory
                     ,"/"
                     ,janitor::make_clean_names( var_of_interest, case = "title" )
                     ,"_by_rurality_summary_statistics.csv"
                   )
      ) %>%
      suppressWarnings()
    
    # Unstratified.
    plot_data <-
      data[[1]] %>% 
      dplyr::filter(
        `Care setting` == "All care settings" 
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      # Format the text of the profession values.
      dplyr::mutate(
        `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
      ) %>% 
      # Remove Trusts with no `RUC21 settlement class`.
      dplyr::filter( !is.na( `RUC21 settlement class` ) )
    
    
    sumstat_plot_data <-
      plot_data %>%
      dplyr::reframe(
        class_median = median( `Stability index`, na.rm = TRUE )
        ,class_min = min( `Stability index`, na.rm = TRUE )
        ,class_max = max( `Stability index`, na.rm = TRUE )
        ,.by = c( `RUC21 settlement class`, year )
      )
    
    min_val <-
      min(
        plot_data$`Stability index`
        ,na.rm = TRUE
      )
    max_val <-
      max(
        plot_data$`Stability index`
        ,na.rm = TRUE
      )
    
    p <-
      plot_data %>%
      ggplot(
        aes(
          x = `Stability index`
          ,y = `RUC21 settlement class`
        ) ) +
      geom_point( position = position_jitter( height = 0.1 ) ) +
      geom_point(
        data = sumstat_plot_data
        ,aes( x = class_median, y = `RUC21 settlement class`)
        ,colour = "red", shape = "|", size = 15
      ) +
      facet_wrap( vars( year ), nrow = 1 ) +
      labs(
        title =
          paste0(
            'Relationship between rurality\n'
            ,'and Stability Index.'
          )
        ,subtitle =
          paste0(
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 Showing values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), ".\n"
            ,"\u2022 Red line shows the median."
          )
        ,y = 'Rurality'
      ) +
      theme(
        axis.title.x = element_blank()
        ,axis.text = element_text( size = 10 )
        ,plot.title = element_text( size = 20 )
        ,plot.subtitle = element_text( size = 15 )
      )
    ggsave(
      plot = p
      ,filename =
        paste0(
          "Plots/Stability index/"
          ,dataset_id
          ,"/Rurality/plot__rurality_vs_SI"
          ,"_all_professions_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 25
      ,height = 10
      ,units = "cm"
    )
    
    
    # Make the plot for each AHP role.
    # ## Do the data processing that applies to all professions.
    professions_plot_data <-
      data[[1]] %>%
      dplyr::filter(
        `Care setting` != "All care settings" 
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      # Remove Trusts with no RUC21 settlement class.
      dplyr::filter( !is.na( `RUC21 settlement class` ) )
    
    
    # ## Work through each profession.
    for( i_role in 1:length( roles ) )
    {
      i_plot_data <-
        professions_plot_data %>%
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
            ,.by = c( `RUC21 settlement class`, year )
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
              ,y = `RUC21 settlement class`
            ) ) +
          geom_point( position = position_jitter( height = 0.1 ) ) +
          geom_point(
            data = sumstat_plot_data
            ,aes( x = class_median, y = `RUC21 settlement class`)
            ,colour = "red", shape = "|", size = 15
          ) +
          facet_wrap( vars( year ), nrow = 1 ) +
          labs(
            title =
              paste0(
                'Relationship between rurality\n'
                ,'and Stability Index, for '
                ,roles[ i_role ]
                ,"."
              )
            ,subtitle =
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Showing values \u2265", round( min_val, 2 )
                ," and \u2264", round( max_val, 2 ), ".\n"
                ,"\u2022 Red line shows the median."
              )
            ,y = 'Rurality'
          ) +
          theme(
            axis.title.x = element_blank()
            ,axis.text = element_text( size = 10 )
            ,plot.title = element_text( size = 20 )
            ,plot.subtitle = element_text( size = 15 )
          )
        ggsave(
          plot = p
          ,filename =
            paste0(
              "Plots/Stability index/"
              ,dataset_id
              ,"/Rurality/plot__rurality_vs_SI_"
              ,gsub(roles[ i_role ], pattern = "/", replacement = " & ")
              ,"_"
              ,dataset_id
              ,"_dataset.png"
            )
          ,dpi = 300
          ,width = 25
          ,height = 10
          ,units = "cm"
        )
        
      }
    }
  }
# ----

# Apply the functions.
# ----
fnc__rurality_SI_plots( data = ls_churn_within_NHS, dataset_id = 'WITHIN_NHS' )
# fnc__rutality_SI_plots( data = ls_churn_from_NHS, dataset_id = 'FROM_NHS' )
# ----