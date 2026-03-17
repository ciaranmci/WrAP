#############################################################
## Make plots for qualitative assessment of distributions. ##
#############################################################
# The purpose of this script is to create the plots of the distributions of
# the outcomes of interest. Plots are created for unstratified data, stratified
# by profession, and stratified by the candidate factors provided in the data set.
#
# This script is called within `WrAP_Explore.R`.


# Make the function.
# ----
fnc__analyseDistributions <-
  function(
    data = NULL # An `ls_churn` object.
    ,var_of_interest = NULL # A character string of the column name.
    ,dataset_id = NULL # A character string of the dataset ID.
  ) 
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( var_of_interest ) ){ stop( "The 'var_of_interest' argument was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    
    
    ###################################
    ## Create the overall plot data. ##
    ###################################
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
      dplyr::reframe(
        min = min( `Stability index`, na.rm = T )
        ,median = median( `Stability index`, na.rm = T )
        ,mean = mean( `Stability index`, na.rm = T )            
        ,max = max( `Stability index`, na.rm = T )
        ,.by = c( `Care setting`, year )
      ) %>%
      dplyr::arrange( `Care setting`, year ) %>%
      write.csv( file =
                   paste0(
                     save_directory
                     ,"/"
                     ,janitor::make_clean_names( var_of_interest, case = "title" )
                     ,"_overall_and_by-profession_summary_statistics.csv"
                   )
      ) %>%
      suppressWarnings()
    
    plot_data <-
      data[[1]] %>%
      dplyr::filter(
        `Care setting` == "All care settings"
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      )
    
    sumstat_plot_data <-
      plot_data %>%
      dplyr::reframe(
        class_median = median( !!( sym( var_of_interest ) ), na.rm = TRUE )
        ,class_min = min( !!( sym( var_of_interest ) ), na.rm = TRUE )
        ,class_max = max( !!( sym( var_of_interest ) ), na.rm = TRUE )
        ,.by = c( year )
      )
    
    min_val <-
      min(
        plot_data[ ,var_of_interest ]
        ,na.rm = TRUE
      )
    max_val <-
      max(
        plot_data[ ,var_of_interest ]
        ,na.rm = TRUE
      )
    
    # Make the overall plot.
    p <-
      plot_data %>%
      ggplot(
        aes(
          x = !!( sym( var_of_interest ) )
          ,y = year
        ) ) +
      geom_point(
        position = position_jitter( height = 0.1 )
        ,alpha = 0.2
      ) +
      geom_point(
        data = sumstat_plot_data
        ,aes( x = class_median, y = year )
        ,colour = "red", shape = "|", size = 15
      ) +
      labs(
        title =
          paste0(
            "Distribution of "
            ,janitor::make_clean_names( var_of_interest, case = "title" )
            ,"\nacross Trusts, stratified by year."
          )
        ,subtitle =
          paste0(
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 Showing values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), ".\n"
            ,"\u2022 Red line shows the median."
          )
      ) +
      theme_minimal() +
      theme(
        axis.title.y = element_blank()
        ,axis.text = element_text( size = 10 )
        ,plot.title = element_text( size = 20 )
        ,plot.subtitle = element_text( size = 15 )
      )
    # Save the overall plot.
    ggsave(
      plot = p
      ,filename =
        paste0(
          "Plots/"
          ,var_of_interest
          ,"/"
          ,dataset_id
          ,"/plot__distribution_of_all_professions_"
          ,var_of_interest
          ,"_from_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 15
      ,height = 15
      ,units = "cm"
    )
    
    
    #############################
    ## Loop through AHP roles. ##
    #############################
    # Create the plot data, stratified by staff role.
    plot_data <-
      # Arbitrarily use the payband data sheet to look at the totals.
      data[[1]] %>%
      dplyr::filter(
        `Care setting` != "All care settings" 
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>% 
      # Edit the `Care setting` strings so that they can run over multiple lines.
      dplyr::mutate(
        `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
      )
    
    # Make the plot for each AHP role.
    for( i_role in 1:length( roles ) )
    {
      i_plot_data <-
        plot_data %>% 
        dplyr::filter( `Care setting` == gsub( pattern = "/", replacement = " / ", x = roles[ i_role ] ) ) 
      
      if ( nrow( i_plot_data ) > 0 )
      {
        
        sumstat_plot_data <-
          i_plot_data %>%
          dplyr::reframe(
            class_median = median( !!( sym( var_of_interest ) ), na.rm = TRUE )
            ,class_min = min( !!( sym( var_of_interest ) ), na.rm = TRUE )
            ,class_max = max( !!( sym( var_of_interest ) ), na.rm = TRUE )
            ,.by = c( year )
          )
        
        min_val <-
          min(
            i_plot_data[ , var_of_interest ]
            ,na.rm = TRUE
          )
        max_val <-
          max(
            i_plot_data[ , var_of_interest ]
            ,na.rm = TRUE
          )
        
        p <-
          i_plot_data %>% 
          ggplot(
            aes(
              x = !!( sym( var_of_interest ) )
              ,y = year
            ) ) +
          geom_point(
            position = position_jitter( height = 0.1 )
            ,alpha = 0.2
          ) +
          geom_point(
            data = sumstat_plot_data
            ,aes( x = class_median, y = year )
            ,colour = "red", shape = "|", size = 15
          ) +
          labs(
            title =
              paste0(
                'Distribution of '
                ,var_of_interest
                ,' for\n'
                ,roles[ i_role ]
                ,',\nstratified by year.'
              )
            ,subtitle =
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Showing values \u2265", round( min_val, 2 )
                ," and \u2264", round( max_val, 2 ), ".\n"
                ,"\u2022 Red line shows the median."
              )
          ) +
          theme_minimal() +
          theme(
            axis.title.y = element_blank()
            ,axis.text = element_text( size = 10 )
            ,plot.title = element_text( size = 20 )
            ,plot.subtitle = element_text( size = 15 )
          )
        # Save plot, stratified by AHP role.
        ggsave(
          plot = p
          ,filename =
            paste0(
              "Plots/"
              ,var_of_interest
              ,"/"
              ,dataset_id
              ,"/plot__distribution_of_"
              ,gsub(roles[ i_role ], pattern = "/", replacement = " & ")
              ,"_"
              ,var_of_interest
              ,"_from_"
              ,dataset_id
              ,"_dataset.png"
            )
          ,dpi = 300
          ,width = 25
          ,height = 20
          ,units = "cm"
        )
      } else
      {
        message(
          paste0(
            '\tInsufficient data to plot stratification by '
            ,var_of_interest
            ,' for '
            ,roles[ i_role ]
            ,'.'
          )
        )
      }
    }
    
    
    
    
    
    #############################################
    ## Loop through candidate factor and role. ##
    #############################################
    lapply(
      data
      ,function(x)
      {
        # Get name of plot from the stratification.
        stratification_name <- colnames( x )[11]
        # Remove paybands that are not of interest.
        if( stratification_name == "AfC band" )
        {
          x <-
            x %>%
            dplyr::filter(
              !`AfC band` %in% c( 'All AfC band', 'Non AfC band', 'Band 4' )
            )
        }
        stratification_name <- sub( pattern = " ", replacement = "_", x = stratification_name )
        colnames( x )[11] <- stratification_name
        
        # Make plot data, stratified by candidate factor.
        plot_data <-
          x %>%
          dplyr::filter(
            `Care setting` != "All care settings"
            ,!stringr::str_detect( string = !!( sym( stratification_name ) ), pattern = "All " )
            # Remove SI = 0%.
            ,!`Stability index` %in% c(0)
          ) %>%
          # Edit the `Care setting` strings so that they can run over multiple lines.
          dplyr::mutate(
            `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
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
        plot_data %>%
          dplyr::reframe(
            min = min( `Stability index`, na.rm = T )
            ,median = median( `Stability index`, na.rm = T )
            ,mean = mean( `Stability index`, na.rm = T )            
            ,max = max( `Stability index`, na.rm = T )
            ,.by = c( `Care setting`, year, !!( sym( stratification_name ) ) )
          ) %>%
          dplyr::arrange( `Care setting`, year, !!( sym( stratification_name ) ) ) %>%
          write.csv( file =
                       paste0(
                         save_directory
                         ,"/"
                         ,janitor::make_clean_names( var_of_interest, case = "title" )
                         ,"_by_"
                         ,stratification_name
                         ,"_summary_statistics.csv"
                       )
          ) %>%
          suppressWarnings()
        
        # Make plot for each candidate factor for each role.
        for( i_role in 1:length( roles ) )
        {
          i_plot_data <-
            plot_data %>%
            dplyr::filter( `Care setting` == gsub( pattern = "/", replacement = " / ", x = roles[ i_role ] ) )
          
          if ( nrow( i_plot_data ) > 0 )
          {
            
            sumstat_plot_data <-
              i_plot_data %>%
              dplyr::reframe(
                class_median = median( !!( sym( var_of_interest ) ), na.rm = TRUE )
                ,class_min = min( !!( sym( var_of_interest ) ), na.rm = TRUE )
                ,class_max = max( !!( sym( var_of_interest ) ), na.rm = TRUE )
                ,.by = c( !!( sym( stratification_name ) ), year )
              ) %>%
              dplyr::arrange( year, !!( sym(stratification_name ) ) )
            
            min_val <-
              min(
                i_plot_data[ ,var_of_interest ]
                ,na.rm = TRUE
              )
            max_val <-
              max(
                i_plot_data[ ,var_of_interest ]
                ,na.rm = TRUE
              )
            
            p <- 
              i_plot_data %>%
              ggplot(
                aes(
                  x = !!( sym( var_of_interest ) )
                  ,y = !!( sym( stratification_name ) )
                ) ) +
              geom_point(
                position = position_jitter( height = 0.1 )
                ,alpha = 0.2
              ) +
              geom_point(
                data = sumstat_plot_data
                ,aes( x = class_median, y = !!( sym( stratification_name ) ) )
                ,colour = "red", shape = "|", size = 15
              ) +
              facet_wrap( vars( year ), nrow = 1 ) +
              labs(
                title =
                  paste0(
                    'Distribution of '
                    ,var_of_interest
                    ,' for '
                    ,roles[ i_role ]
                    ,'\nstratified by year and '
                    ,stratification_name
                    ,'.'
                  )
                ,subtitle =
                  paste0(
                    "\u2022 Using ", dataset_id," dataset.\n"
                    ,"\u2022 Showing values \u2265", round( min_val, 2 )
                    ," and \u2264", round( max_val, 2 ), ".\n"
                    ,"\u2022 Red line shows the median."
                  )
                , y = stratification_name
              ) +
              theme_minimal() +
              theme(
                axis.title.x = element_blank()
                ,axis.text = element_text( size = 10 )
                ,plot.title = element_text( size = 20 )
                ,plot.subtitle = element_text( size = 15 )
              )
            # Save plot, stratified by candidate factor.
            ggsave(
              plot = p
              ,filename =
                paste0(
                  "Plots/"
                  ,var_of_interest
                  ,"/"
                  ,dataset_id
                  ,"/"
                  ,stratification_name
                  ,"/plot__distribution_of_"
                  ,gsub(roles[ i_role ], pattern = "/", replacement = " & ")
                  ,"_"
                  ,var_of_interest
                  ,"_stratified_by_"
                  ,stratification_name
                  ,"_from_"
                  ,dataset_id
                  ,"_dataset.png"
                )
              ,dpi = 300
              ,width = 30
              ,height = 20
              ,units = "cm"
            )
            
          } else { 
            message(
              paste0(
                '\tInsufficient data to plot stratification by '
                ,var_of_interest
                ,' for '
                ,roles[ i_role ]
                ,'.'
              )
            )
          }
        }
      }
    )
  }
# ----


# Run the function for the variates of interest.
# ----
# # Using the `ls_churn_from_NHS` dataset.
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'joiner_rate', dataset_id = "FROM_NHS" )
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'leaver_rate', dataset_id = "FROM_NHS" )
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'remainer_rate', dataset_id = "FROM_NHS" )
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'Stability index', dataset_id = "FROM_NHS" )
# # Using the `ls_churn_within_NHS` dataset.
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'joiner_rate', dataset_id = "WITHIN_NHS" )
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'leaver_rate', dataset_id = "WITHIN_NHS" )
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'remainer_rate', dataset_id = "WITHIN_NHS" )
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'Stability index', dataset_id = "WITHIN_NHS" )
# ----