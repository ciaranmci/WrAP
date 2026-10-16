###################
## Plots for IMD ##
###################
# The purpose of this script is to create the plots of the distributions of
# the index of multiple deprivation score and Stability index. Plots are created
# for unstratified data and data stratified by profession (known as 'Care setting').
#
# This script is called within `WrAP_Explore.R`.

# Write the functions.
# ----
fnc__IMD_SI_plots <-
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
    plot_data <-
      data[[1]] %>% 
      dplyr::filter(
        `Care setting` == "All care settings" 
        ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
      ) %>%
      # Remove Trusts with no IMD score.
      dplyr::filter( !is.na( `IMD Score` ) )
    
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
      ggplot(aes(
        x = `Stability index`
        ,y = `IMD Score`
      ) ) +
      geom_point() +
      facet_wrap( vars( year ), nrow = 1 ) +
      labs(
        title =
          paste0(
            'Relationship between Index of Multiple Deprivation Score\n'
            ,'and Stability Index.'
          )
        ,subtitle =
          paste0(
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 Showing values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), ".\n"
          )
        ,y = 'Index of Multiple Deprivation Score'
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
          ,"/IMD/plot__IMD_vs_SI"
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
      # Remove Trusts with no IMD score.
      dplyr::filter( !is.na( `IMD Score` ) )
    # ## Work through each profession.
    for( i_role in 1:length( roles ) )
    {
      i_plot_data <-
        professions_plot_data %>%
        dplyr::filter( `Care setting` == roles[ i_role ] )
      
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
      
      if ( nrow( i_plot_data ) > 0 )
      {
        
        p <-
          i_plot_data %>%
          ggplot(aes(
            x = `Stability index`
            ,y = `IMD Score`
          ) ) +
          geom_point() +
          facet_wrap( vars( year ), nrow = 1 ) +
          labs(
            title =
              paste0(
                'Relationship between Index of Multiple Deprivation Score\n'
                ,'and Stability Index, for '
                ,roles[ i_role ]
                ,"."
              )
            ,subtitle =
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Showing values \u2265", round( min_val, 2 )
                ," and \u2264", round( max_val, 2 ), ".\n"
              )
            ,y = 'Index of Multiple Deprivation Score'
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
              ,"/IMD/plot__IMD_vs_SI_"
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
fnc__IMD_SI_plots( data = ls_churn_within_NHS, dataset_id = 'WITHIN_NHS' )
# fnc__IMD_SI_plots( data = ls_churn_from_NHS, dataset_id = 'FROM_NHS' )
# ----