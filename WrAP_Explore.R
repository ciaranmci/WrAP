# WrAP_Explore.R
#
# The purpose of this script is to describe the distribution of stability-index
# scores, and to explore an associations that there might be between the index
# and other variables that we collected.
#
# The analysis will need to be applied to each of the data sets in the `ls_churn`
# list. Therefore, I will create a function that processes one data set before I
# apply it to all elements of the `ls_churn` list.
#


#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  tidyverse
  ,janitor
)
# ----

##############################
## Create folder for plots. ##
##############################
# ----
dir.create( file.path( getwd, 'Plots' ) )
# ----

#############################################################
## Make plots for qualitative assessment of distributions. ##
#############################################################
# The following function creates plots that show the distribution of a variable
# of interest unstratified, stratified by AHP role,  stratified by the
# candidate factors provided in the data set, and stratified by AHP role and by
# candidate factors.
#
# Findings from these plots.
# 1. The leaver, joiner, and remainer rates are in a narrow range and don't change
#    throughout the years.
# 
# ~ AHP roles ~
# 2. Except for individual outliers, remainer and joiner rates are in a narrow
#    range and don't change throughout the years, irrespective of AHP role.
# 3. Leaver rates are in a narrow range and don't change throughout the years,
#    irrespective of AHP role.
# 4. There is a trend for higher AfC bands to have higher remainer rates within
#    a narrower range, but outliers exist.
# 5. There is a trend for higher AfC bands to have lower leaver rates within a
#    narrower range, but outliers exist.
# 
# ~ Candidate factors ~
# 6. Band 5 staff seem to be have markedly higher joiner rates and larger
#    range compared to the other bands.
# 7. Neither the leaver, joiner, nor remainer rates differ across sexes.
# 8. Neither the leaver, joiner, nor remainer rates differ across ethnicity,
#    though 'White' show higher average remainer rates and lower leaver and joiner rates.
# 9. Under-31s have markedly higher leaver rates, higher joiner rates, and lower
#    remainer rates, but in a wider range. In other words, under-31s are moving
#    move.
# 
# ~ AHP roles and candidate factors ~
# 10. Males are barely represented in the data set so no conclusions can be made
#     about sexes.
# 11. Age is only ever a distinguishing factor in the last year of all rates
#     for therapeutic radiographers.
# 12. Joiner rates for under-25s markedly higher for roles in emergency care,
#     physiotherapy, and diagnostic radiography.
# 13. Neither the leaver, joiner, nor remainer rates differ across ethnicity.
# 14. There is a trend for lower AfC bands to have higher leaver and joiner rates.
# ----

# Make the function.
fnc__analyseDistributions <-
  function(
    data = NULL # An `ls_churn` object.
    ,var_of_interest = NULL # A character string of the colunm name.
    )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( var_of_interest ) ){ stop( "The 'var_of_interest' arguement was not supplied." ) }
    
    # Create the overall plot data.
    plot_data <-
      data[[1]] %>%
      # Filter for the overall value for each organisation.
      dplyr::group_by( `Org code`, Period ) %>%
      dplyr::filter(
        `Care setting` == "All care settings" &
          `Denominator at start of period` == max( `Denominator at start of period` )
      ) %>%
      dplyr::ungroup() %>%
      # Filter out the values that are calculated from counts too small to disclose.
      # This has the benefit of removing sites whose overall value is the same as the
      # stratified values. This stratified values will not have been removed with the
      # previous function, unfortunately.
      dplyr::filter( !too_small_to_disclose )
    message("Overall data made")
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
          )
        ) +
        geom_boxplot( ) +
        geom_point(
          position = position_jitter( heigh = 0.2 )
          ,alpha = 0.5
          ,colour = "grey") +
        labs(
          title =
            paste0(
              "Distribution of "
              ,janitor::make_clean_names( var_of_interest, case = "title" )
              ," across Trusts, stratified by year."
            )
          ,subtitle =
            paste0(
              "\u2022 Excludes values calculated from counts too small to disclose.\n"
              ,"\u2022 Showing values >", round( min_val, 2 )," and <", round( max_val, 2 ), "."
            )
          ,x = janitor::make_clean_names( var_of_interest, case = "title" )
        ) +
        xlim( min_val * 0.9, max_val * 1.1 ) +
        facet_grid( rows = vars( year ) ) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank()
          ,axis.title.y = element_blank()
          ,strip.text.y = element_text( angle = 0 )
        )
      message("Overall plot made")
      
      # Save the overall plot.
      ggsave(
        plot = p
        ,filename = paste0( "Plots/plot__distribution_of_", var_of_interest, ".png" )
        ,dpi = 300
        ,width = 15
        ,height = 10
        ,units = "cm"
      )
      message("Overall plot saved")
      
      
      ###########################
      ## Stratify by AHP role. ##
      ###########################
      # Create the plot data, stratified by staff role.
      plot_data <-
        ls_churn[[1]] %>%
        # Filter for the overall value for each organisation.
        dplyr::group_by( `Org code`, Period ) %>%
        dplyr::filter(
          `Care setting` != "All care settings" 
          ,stringr::str_detect( string = `AfC band`, pattern = "All " )
        ) %>% 
        dplyr::ungroup() %>%
        # Filter out the values that are calculated from counts too small to disclose.
        # This has the benefit of removing sites whose overall value is the same as the
        # stratified values. This stratified values will not have been removed with the
        # previous function, unfortunately.
        dplyr::filter( !too_small_to_disclose ) %>%
        # Edit the `Care setting` strings so that they can run over multiple lines.
        dplyr::mutate(
          `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
        )
      message('Stratified data made')
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
      
      # Make the plot, stratified by AHP role.
      p <-
        plot_data %>% 
        ggplot(
          aes(
            x = !!( sym( var_of_interest ) )
            ,y = year
          )
        ) +
        geom_boxplot( size = 0.5 ) +
        geom_point(
          position = position_jitter( heigh = 0.2 )
          ,alpha = 0.5
          ,size = 0.5
          ,colour = 'grey'
          ) +
        labs(
          title =
            paste0(
              "Distribution of "
              ,janitor::make_clean_names( var_of_interest, case = "title" )
              ," across Trusts, stratified by year and AHP role."
            )
          ,subtitle =
            paste0(
              "\u2022 Excludes values calculated from counts too small to disclose.\n"
              ,"\u2022 Showing values >", round( min_val, 2 )," and <", round( max_val, 2 ), "."
            )
          ,x = janitor::make_clean_names( var_of_interest, case = "title" )
        ) +
        facet_grid(
          cols = vars( `Care setting` )
          ,rows = vars( year )
          ,labeller =
            labeller(
              `Care setting` = label_wrap_gen( 10 )
              ,year = label_wrap_gen( 10 )
            )
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank()
          ,axis.title.y = element_blank()
          ,axis.text.x = element_text( size = 4 )
          ,legend.position = "None"
          ,strip.text.y = element_text( angle = 0 )
          ,strip.text.x = element_text( size = 5 )
        )
      message("Stratified plot made")
      
      # Save plot, stratified by AHP role.
      ggsave(
        plot = last_plot()
        ,filename = paste0( "Plots/plot__distribution_of_", var_of_interest, "_by_AHP_role.png" )
        ,dpi = 300
        ,width = 20
        ,height = 10
        ,units = "cm"
      )
      message("Stratified plot saved")
      
      
      
      
      
      ########################################
      ## Stratify by candidate factor role. ##
      ########################################
      lapply(
        ls_churn
        ,function(x)
        {
          # Get name of plot from the stratification.
          stratification_name <- colnames( x )[11]
          stratification_name <- sub( pattern = " ", replacement = "_", x = stratification_name )
          colnames( x )[11] <- stratification_name
          
          # Make plot data, stratified by candidate factor.
          plot_data <-
            x %>%
            # Filter for the overall value for each organisation.
            dplyr::group_by( `Org code`, Period ) %>%
            dplyr::filter(
              # # Filter for the counts irrespective of the AHP role.
              `Care setting` == "All care settings"
              # # Filter out the unstratified counts.
              ,!stringr::str_detect( string = !!(sym(stratification_name)), pattern = "All " )
            ) %>% 
            dplyr::ungroup() %>%
            # Filter out the values that are calculated from counts too small to disclose.
            # This has the benefit of removing sites whose overall value is the same as the
            # stratified values. This stratified values will not have been removed with the
            # previous function, unfortunately.
            dplyr::filter( !too_small_to_disclose )
          message('Factor data made')
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
          
          # Make plot, stratified by candidate factor.
          if ( nrow( plot_data ) > 0 )
          {
            p <- 
              plot_data %>%
              ggplot(
                aes(
                  x = !!( sym( var_of_interest ) )
                  ,y = year
                  ,fill = !!( sym( stratification_name ) )
                )
              ) +
              geom_boxplot( size = 0.5 ) +
              geom_point(
                aes( colour = !!( sym( stratification_name ) ) )
                ,position = position_jitter( height = 0.15 )
                ,alpha = 0.5
                ,size = 0.5
              ) +
              labs(
                title =
                  paste0(
                    "Distribution of "
                    ,janitor::make_clean_names( var_of_interest, case = "title" )
                    ," across Trusts, stratified by year\n"
                    ,"and ", stratification_name, "." 
                  )
                ,subtitle =
                  paste0(
                    "\u2022 Excludes values calculated from counts too small to disclose.\n"
                    ,"\u2022 Showing values >", round( min_val, 2 )," and <", round( max_val, 2 ), "."
                  )
                ,x = janitor::make_clean_names( var_of_interest, case = "title" )
              ) +
              facet_grid( rows = vars( year ) ) +
              theme_minimal() +
              theme(
                axis.text.y = element_blank()
                ,axis.title.y = element_blank()
                ,strip.text.y = element_text( angle = 0 )
              )
            message("Factor plot made")
            
            # Save plot, stratified by candidate factor.
            ggsave(
              plot = p
              ,filename =
                paste0(
                  "Plots/plot__distribution_of_"
                  ,var_of_interest
                  ,"_stratified_by_"
                  ,stratification_name
                  ,".png"
                )
              ,dpi = 300
              ,width = 20
              ,height = 20
              ,units = "cm"
            )
            message("Factor plot saved")
          } else { 
            message(
              paste0(
                '\tInsufficient data to plot stratification by '
                ,var_of_interest, '.'
              )
            )
          }
        }
      )
      
      
      
      #####################################################
      ## Stratify by AHP role and candidate factor role. ##
      #####################################################
      lapply(
        ls_churn
        ,function(x)
        {
          # Get name of plot from the stratification.
          stratification_name <- colnames( x )[11]
          stratification_name <- sub( pattern = " ", replacement = "_", x = stratification_name )
          colnames( x )[11] <- stratification_name
          
          # Make plot data, stratified by candidate factor.
          plot_data <-
            x %>%
            # Filter for the overall value for each organisation.
            dplyr::group_by( `Org code`, Period ) %>%
            dplyr::filter(
              # # Filter out the counts irrespective of the AHP role.
              `Care setting` != "All care settings"
              # # Filter out the unstratified counts.
              ,!stringr::str_detect( string = !!(sym(stratification_name)), pattern = "All " )
            ) %>% 
            dplyr::ungroup() %>%
            # Filter out the values that are calculated from counts too small to disclose.
            # This has the benefit of removing sites whose overall value is the same as the
            # stratified values. This stratified values will not have been removed with the
            # previous function, unfortunately.
            dplyr::filter( !too_small_to_disclose ) %>%
            # Edit the `Care setting` strings so that they can run over multiple lines.
            dplyr::mutate(
              `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
            )
          message('AHP-Factor data made')
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
          
          # Make plot, stratified by candidate factor.
          if ( nrow( plot_data ) > 0 )
          {
            p <- 
              plot_data %>%
              ggplot(
                aes(
                  x = !!( sym( var_of_interest ) )
                  ,y = year
                  ,fill = !!( sym( stratification_name ) )
                )
              ) +
              geom_boxplot( size = 0.5 ) +
              labs(
                title =
                  paste0(
                    "Distribution of "
                    ,janitor::make_clean_names( var_of_interest, case = "title" )
                    ," across Trusts, stratified by year and AHP role\n"
                    ,"and ", stratification_name, "." 
                  )
                ,subtitle =
                  paste0(
                    "\u2022 Excludes values calculated from counts too small to disclose.\n"
                    ,"\u2022 Showing values >", round( min_val, 2 )," and <", round( max_val, 2 ), "."
                  )
                ,x = janitor::make_clean_names( var_of_interest, case = "title" )
              ) +
              facet_grid(
                cols = vars( `Care setting` )
                ,rows = vars( year )
                ,labeller =
                  labeller(
                    `Care setting` = label_wrap_gen( 10 )
                    ,year = label_wrap_gen( 10 )
                  )
              ) +
              theme_minimal() +
              theme(
                axis.text.y = element_blank()
                ,axis.title.y = element_blank()
                ,strip.text.y = element_text( angle = 0 )
              )
            message("AHP-Factor plot made")
            
            # Save plot, stratified by candidate factor.
            ggsave(
              plot = p
              ,filename =
                paste0(
                  "Plots/plot__distribution_of_"
                  ,var_of_interest
                  ,"_stratified_by_"
                  ,stratification_name
                  ,"_and_AHP_role.png"
                )
              ,dpi = 300
              ,width = 35
              ,height = 20
              ,units = "cm"
            )
            message("AHP-Factor plot saved")
          } else { 
            message(
              paste0(
                '\tInsufficient data to plot stratification by '
                ,var_of_interest, '.'
              )
            )
          }
        }
      )
      
  }

# Run the function for the three variates of interest.
fnc__analyseDistributions( data = ls_churn, var_of_interest = 'joiner_rate' )
fnc__analyseDistributions( data = ls_churn, var_of_interest = 'leaver_rate' )
fnc__analyseDistributions( data = ls_churn, var_of_interest = 'remainer_rate' )
# ----










########################################################################
## Prepare data for unstratified investigation and for AHP-stratified ##
## investigation.                                                     ##
########################################################################
# Findings from these plots are:
# 1.
#
# ----
df <-
  ls_churn[[1]] %>%
  dplyr::mutate(
    `Leaver Rate (categorised)` =
      dplyr::if_else( leaver_rate < 0.1, 'Smaller', 'Larger' )
    ,`Leaver Rate (categorised)` =
      dplyr::if_else( too_small_to_disclose, 'Undisclosed', `Leaver Rate (categorised)` )
    ,`Joiner Rate (categorised)` =
      dplyr::if_else( joiner_rate < 0.14, 'Smaller', 'Larger' )
    ,`Joiner Rate (categorised)` =
      dplyr::if_else( too_small_to_disclose, 'Undisclosed', `Joiner Rate (categorised)` )
    ,`Remainer Rate (categorised)` =
      dplyr::if_else( remainer_rate < 0.9, 'Smaller', 'Larger' )
    ,`Remainer Rate (categorised)` =
      dplyr::if_else( too_small_to_disclose, 'Undisclosed', `Remainer Rate (categorised)` )
    ) %>%
  dplyr::select(
    year
    ,`Org code`
    ,`Care setting`
    ,`IMD Score`
    ,`RUC21 settlement class`
    ,contains( "satisfaction" )
    ,`Leaver Rate (categorised)`
    ,`Joiner Rate (categorised)`
    ,`Remainer Rate (categorised)`
  ) %>%
  dplyr::mutate(
    `RUC21 settlement class` =
      factor(
        `RUC21 settlement class`
        ,levels = c( "Rural", "Intermediate rural"
                     ,"Intermediate urban", "Urban")
      )
    ) %>%
  tidyr::pivot_longer(
    cols = contains( "categorised" )
    ,names_to = "statistic"
    ,values_to = "category"
  )

stratified_df <-
  df %>%
  dplyr::filter( `Care setting` != "All care settings" ) %>%
  dplyr::mutate( AHP_role = `Care setting` ) %>%
  tidyr::nest(
    .by = `Care setting`
  ) %>%
  dplyr::mutate(
    `Care setting` =
      stringr::str_replace_all(
        string = `Care setting`
        ,pattern = "/"
        ,replacement = " or "
      )
  )
# ----

####################
## Plots for IMD. ##
####################
# ----
# Unstratified.
p <-
  df %>%
  dplyr::filter( `Care setting` == "All care settings" ) %>%
  ggplot() +
    geom_boxplot( 
      aes(
        x = category
        ,y = `IMD Score`
        ) 
      ) +
    labs(
      title =
        paste0(
          'Distributions of the Index of Multiple Deprivation Score\n'
          ,'for each rate of interest, across categories of that rate.'
        )
      ,subtitle =
        paste0(
          "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
          ,"\u2022 Missing categories indicate counts were too small to disclose.\n"
          ,"\u2022 Missing boxes indicate missing data for the variable of interest."
        )
      ,y = 'Index of Multiple Deprivation Score'
    ) +
    facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
    theme(
      axis.title.x = element_blank()
      ,axis.text = element_text( size = 10 )
      ,plot.title = element_text( size = 20 )
      ,plot.subtitle = element_text( size = 15 )
    )
ggsave(
  plot = p
  ,filename = "Plots/plot__rates_stratified_by_IMD.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# Stratified by AHP role.
# # Make plots.
p <-
  stratified_df %>%
  dplyr::mutate(
    plots =
      lapply(
        data
        ,function(x)
        {
          x %>%  
            ggplot() +
            geom_boxplot( 
              aes(
                x = category
                ,y = `IMD Score`
              ) 
            ) +
            labs(
              title =
                paste0(
                   x$AHP_role
                  ,':\nDistributions of the Index of Multiple Deprivation Score\n'
                  ,'for each rate of interest, across categories of that rate.'
                )
              ,subtitle =
                paste0(
                  "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
                  ,"\u2022 Missing categories indicate counts were too small to disclose.an"
                  ,"\u2022 Missing boxes indicate missing data for the variable of interest."
                )
              ,y = 'Index of Multiple Deprivation Score'
            ) +
            facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
            theme(
              axis.title.x = element_blank()
              ,axis.text = element_text( size = 10 )
              ,plot.title = element_text( size = 20 )
              ,plot.subtitle = element_text( size = 15 )
            )
        }
      )
  )
# # Save plots.
for( i in 1:nrow( stratified_df ) )
{
  ggsave(
    plot = p$plots[[ i ]]
    ,filename =
      paste0(
        "Plots/plot__"
        ,p$`Care setting`[ i ]
        ,"_rates_stratified_by_IMD.png"
      )
    ,dpi = 300
    ,width = 20
    ,height = 21
    ,units = "cm"
  )
}
  
  # ----

########################
## Plots for rurality ##
########################
# ----
# Unstratified.
p <-
  df %>%
  dplyr::filter( `Care setting` == "All care settings" ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, statistic, category, `RUC21 settlement class` )
  ) %>%
  ggplot() +
  geom_point( 
    aes(
      x = category
      ,y = `RUC21 settlement class`
      ,size = n
    ) 
  ) +
  labs(
    title =
      paste0(
        'Distributions of Rural Settlement Class\n'
        ,'for each rate of interest, across categories of that rate.'
      )
    ,subtitle =
      paste0(
        "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
        ,"\u2022 Missing categories indicate counts were too small to disclose.\n"
        ,"\u2022 Missing circles indicate missing data for the variable of interest."
      )
  ) +
  facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
  theme(
    axis.title = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15 )
  )
ggsave(
  plot = p
  ,filename = "Plots/plot__rates_stratified_by_rurality.png"
  ,dpi = 300
  ,width = 25
  ,height = 15
  ,units = "cm"
)

# Stratified by AHP role.
# # Make plots.
p <-
  stratified_df %>%
  dplyr::mutate(
    plots =
      lapply(
        data
        ,function(x)
        {
          x %>%
          dplyr::reframe(
            n = n()
            ,.by = c( year, statistic, category, `RUC21 settlement class` )
          ) %>%
            ggplot() +
            geom_point( 
              aes(
                x = category
                ,y = `RUC21 settlement class`
                ,size = n
              ) 
            ) +
            labs(
              title =
                paste0(
                  x$AHP_role
                  ,':\nDistributions of the Rural Settlement Class\n'
                  ,'for each rate of interest, across categories of that rate.'
                )
              ,subtitle =
                paste0(
                  "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
                  ,"\u2022 Missing categories indicate counts were too small to disclose.\n"
                  ,"\u2022 Missing circles indicate missing data for the variable of interest."
                )
              ,y = 'Rural Settlement Class'
            ) +
            facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
            theme(
              axis.title.x = element_blank()
              ,axis.text = element_text( size = 10 )
              ,plot.title = element_text( size = 20 )
              ,plot.subtitle = element_text( size = 15 )
            )
        }
      )
  )
# # Save plots.
for( i in 1:nrow( stratified_df ) )
{
  ggsave(
    plot = p$plots[[ i ]]
    ,filename =
      paste0(
        "Plots/plot__"
        ,p$`Care setting`[ i ]
        ,"_rates_stratified_by_rurality.png"
      )
    ,dpi = 300
    ,width = 25
    ,height = 15
    ,units = "cm"
  )
}

# ----

#####################################
## Plots for patient satisfaction. ##
#####################################
# ----
# Unstratified.
# # Make data set.
ps_data <-
  df %>%
  dplyr::select(
    `Org code`, `Care setting`, category, statistic 
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
  dplyr::distinct()
ps_data <-
  df %>%
    dplyr::mutate(
      Year = dplyr::case_when(
        stringr::str_detect( year, "25" ) ~ 2024
        ,stringr::str_detect( year, "22" ) ~ 2022
        ,.default = 2023
      )
    ) %>%
    dplyr::distinct(
      year, `Org code`, `Care setting`, Year, category, statistic 
      ) %>% 
    dplyr::left_join(
      ps_data
      ,by = join_by( Year, `Org code`, `Care setting`, category, statistic   )
    ) 
# # Plot satisfaction scores for all care settings.
p <-
  ps_data %>%
  dplyr::filter( `Care setting` == "All care settings" ) %>%
  ggplot() +
    geom_boxplot( 
      aes(
        x = category
        ,y = Score
        ,fill = Q
      ) 
    )+
    labs(
      title =
        paste0(
          'Distributions of Patient Satisfaction Scores\n'
          ,'for each rate of interest, across categories of that rate.'
        )
      ,subtitle =
        paste0(
          "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
          ,"\u2022 Missing categories indicate counts were too small to disclose.\n"
          ,"\u2022 Missing boxes indicate missing data for the variable of interest."
        )
    ) +
    facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
    theme(
      axis.title.x = element_blank()
      ,axis.text = element_text( size = 10 )
      ,plot.title = element_text( size = 20 )
      ,plot.subtitle = element_text( size = 15 )
    )
ggsave(
  plot = p
  ,filename = "Plots/plot__rates_stratified_by_PatientSatisfaction.png"
  ,dpi = 300
  ,width = 30
  ,height = 20
  ,units = "cm"
)

# Stratified by AHP role.
# # Make plots.
p <-
  stratified_df %>%
  dplyr::mutate(
    plots =
      lapply(
        data
        ,function(x)
        {
            # # Make data set.
            ps_data <-
              x %>%
              dplyr::select(
                `Org code`, category, statistic 
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
              dplyr::distinct()
            ps_data <-
              df %>%
              dplyr::mutate(
                Year = dplyr::case_when(
                  stringr::str_detect( year, "25" ) ~ 2024
                  ,stringr::str_detect( year, "22" ) ~ 2022
                  ,.default = 2023
                )
              ) %>%
              dplyr::distinct(
                year, `Org code`, Year, category, statistic 
              ) %>% 
              dplyr::left_join(
                ps_data
                ,by = join_by( Year, `Org code`, category, statistic   )
              ) 

            # # Make the plot
            ps_data %>%
            ggplot() +
            geom_boxplot( 
              aes(
                x = category
                ,y = Score
                ,fill = Q
              ) 
            ) +
            labs(
              title =
                paste0(
                  x$AHP_role
                  ,':\nDistributions of the Patient Satisfaction Scores\n'
                  ,'for each rate of interest, across categories of that rate.'
                )
              ,subtitle =
                paste0(
                  "\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
                  ,"\u2022 Missing categories indicate counts were too small to disclose.\n"
                  ,"\u2022 Missing boxes indicate missing data for the variable of interest."
                )
              ,y = 'Score'
            ) +
            facet_grid( cols = vars( statistic ), rows = vars( year ) ) +
            theme(
              axis.title.x = element_blank()
              ,axis.text = element_text( size = 10 )
              ,plot.title = element_text( size = 20 )
              ,plot.subtitle = element_text( size = 15 )
            )
        }
      )
  )
# # Save plots.
for( i in 1:nrow( stratified_df ) )
{
  ggsave(
    plot = p$plots[[ i ]]
    ,filename =
      paste0(
        "Plots/plot__"
        ,p$`Care setting`[ i ]
        ,"_rates_stratified_by_PatientSatisfaction.png"
      )
    ,dpi = 300
    ,width = 20
    ,height = 21
    ,units = "cm"
  )
}
# ----