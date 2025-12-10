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
)
# ----

########################################################
## Unstratified distributions of the stability index. ##
########################################################
# The take-home messages are:
# 1. Excluding hospitals with counts that are too small to disclose results in
#    excluding 'Orthoptics/Optics', 'Call Handling', 'Chiropody/Podiatry', and
#    'Art/Music/Dramatherapy' roles.
# 2. The stability index is uni-modally distributed with a small left skew, in
#    all years.
# 3. Most AHP roles show very little change in stability index over the years.
# 4. Only 'Dietetics' show any non-trivial Stability Index less than 0.8.
# 5. None of the counts stratified by gender, ethnicity, etc are large enough to
#    be disclosed. 
# ----

# Unstratified distribution.
plot__distribution_of_overall_stability_index <-
  ls_churn[[1]] %>%
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
  dplyr::filter( !too_small_to_disclose ) %>%
  # Plot the distributions as density plots.
  ggplot(
    aes(
      x = `Stability index`
      ,group = year
    )
  ) +
  geom_density() +
  labs(
    title = "Distribution of stability-index scores across Trusts, stratified by year."
    ,subtitle =
      paste0(
        "\u2022 Excludes values calculated from counts too small to disclose.\n"
        ,"\u2022 Showing values >0.8, only."
      )
  ) +
  facet_grid( rows = vars( year ) ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank()
    ,axis.title.y = element_blank()
    ,strip.text.y = element_text( angle = 0 )
  )
ggsave(
  plot = last_plot()
  ,filename = "plot__distribution_of_overall_stability_index.png"
  ,dpi = 300
  ,width = 15
  ,height = 20
  ,units = "cm"
)
 
# Stratified by staff role.
plot_data <-
  ls_churn[[1]] %>%
  # Filter for the overall value for each organisation.
  dplyr::group_by( `Org code`, Period ) %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
    ,!`Care setting` %in% c(
     'Orthoptics/Optics', 'Call Handling', 'Chiropody/Podiatry'
     ,'Art/Music/Dramatherapy'
     )
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
min_stability_index <- min( plot_data$`Stability index`, na.rm = TRUE ) %>% round( 1 )
plot__distribution_of_stability_index_by_AHP_role <-
  plot_data %>% 
  ggplot(
    aes(
      x = `Stability index`
      ,fill = `Care setting`
    )
  ) +
  geom_density( ) +
  labs(
    title =
      paste0(
        "Distribution of Stability Index across Trusts, stratified by year"
        ," and AHP role."
      )
    ,subtitle =
      paste0(
        "\u2022 Excludes hospitals with counts too small to disclose.\n"
        ,"\u2022 Excludes 'Orthoptics/Optics', 'Call Handling', 'Chiropody/Podiatry'"
        ,", and 'Art/Music/Dramatherapy'\n  because of small samplpe size.\n"
        ,"\u2022 Showing values >", min_stability_index, " only."
      )
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
    ,axis.text.x = element_text( size = 6 )
    ,legend.position = "None"
    ,strip.text.y = element_text( angle = 0 )
    ,strip.text.x = element_text( size = 5 )
  )
ggsave(
  plot = last_plot()
  ,filename = "plot__distribution_of_stability_index_by_AHP_role.png"
  ,dpi = 300
  ,width = 20
  ,height = 10
  ,units = "cm"
)

# Generates the density plots for each data set in `ls_churn`, if the counts
# are large enough to be disclosed.
lapply(
  ls_churn
  ,function(x)
  {
    x<-ls_churn[[2]]
    # Get name of plot from the stratification.
    stratification_name <- colnames( x )[11]
    stratification_name <- sub( pattern = " ", replacement = "_", x = stratification_name )
    colnames( x )[11] <- stratification_name
    
    # Make plot data.
    plot_data <-
      x %>%
      # Filter for the overall value for each organisation.
      dplyr::group_by( `Org code`, Period ) %>%
      dplyr::filter(
         `Care setting` == "All care settings" &
           `Denominator at start of period` == max( `Denominator at start of period` )
        ,!stringr::str_detect( string = !!(sym(stratification_name)), pattern = "All " )
      ) %>% 
      dplyr::ungroup() %>% 
      # Filter out the values that are calculated from counts too small to disclose.
      # This has the benefit of removing sites whose overall value is the same as the
      # stratified values. This stratified values will not have been removed with the
      # previous function, unfortunately.
      dplyr::filter( !too_small_to_disclose )
      
    # Plot the distributions as density plots.
    if ( nrow( plot_data ) > 0 )
    {
      plot <- 
        plot_data %>%
        ggplot( aes( x = `Stability index` ) ) +
        geom_density( aes_string( fill = stratification_name ), alpha = 0.5 ) +
        labs(
          title =
            paste0(
              "Distribution of Stability Index across Trusts, stratified by year\n."
              ,"and ", stratification_name, "." 
              )
          ,subtitle =
            paste0(
              "\u2022 Excludes values calculated from counts too small to disclose."
              ," This might exlcude\nentire categories.\n"
              ,"\u2022 Showing values >0.8, only."
            )
        ) +
        facet_wrap( ~ year, ncol = 1 ) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank()
          ,axis.title.y = element_blank()
        )
      ggsave(
        plot = last_plot()
        ,filename =
          paste0(
            "plot__distribution_of_stability_index_by_"
            , stratification_name
            , ".png"
            )
        ,dpi = 300
        ,width = 15
        ,height = 20
        ,units = "cm"
      )
    }
  }
)



# ----


################################################
## Explore associations with other variables. ##
################################################
# Given that only one AHP showed any non-trivial Stability Index less than 0.8,
# I suggest we categorise Stability Index into:
# - Too small to disclose
# - Large enough to disclose but <0.8
# - Large enough to disclose and >=0.8
#
# I justify this categorisation on:
# 1. Only one AHP showed any non-trivial Stability Index less than 0.8
# 2. All values for Stability Index >=0.8 are uni-modal.
# 3. Most values for Stability Index that were too small to disclose.
#
# 
# Findings:
# 1. No association between IMD Score and Stability Index.
# 2. No association between the size of the Trust and Stability Index.
# 3. No association between rural-urban divide and Stability Index.
# 4. No association between patient satisfaction and Stability Index
# ----
df <-
  ls_churn[[1]] %>%
  dplyr::mutate(
    ST_cate = dplyr::if_else( `Stability index` < 0.8, '1', '2' )
    ,ST_cate = dplyr::if_else( too_small_to_disclose, '0', ST_cate )
    )

# IMD looks like nothing
plot__distribution_of_stability_index_by_IMD <-
  df %>%
  dplyr::select( ST_cate, `IMD Score` ) %>%
  ggplot() +
  geom_boxplot( aes( x = ST_cate, y = `IMD Score` ) ) +
  labs(
    title =
      paste0(
        'Distributions of the Index of Multiple Deprivation Score\n'
        ,'across categories of Stability Index.'
      )
    ,subtitle = 'There are no differences across categories of Stability Index.'
    ,x = 'Stability-Index category'
    ,y = 'Index of Multiple Deprivation Score'
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text( size = 15 )
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15 )
  )
ggsave(
  plot = plot__distribution_of_stability_index_by_IMD
  ,filename = "plot__IMD_and_StabilityIndex_are_not_associated.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# Rurality looks like nothing. 'Urban' overwhelms.
df %>% dplyr::select(ST_cate, `Rural Urban flag`) %>% table() -> a
round( a / rowSums(a), 2 )
df %>% dplyr::select(ST_cate, `Rural Urban flag`) %>% infotheo::mutinformation() %>% `[`(2) %>% max(.,0)
df %>% dplyr::select(ST_cate, `RUC21 settlement class`) %>% infotheo::mutinformation()%>% `[`(2) %>% max(.,0)
df %>% dplyr::select(ST_cate, `RUC21 relative access`) %>% infotheo::mutinformation()%>% `[`(2) %>% max(.,0)
plot__distribution_of_stability_index_by_pctOfRural <-
  df %>%
  dplyr::select( ST_cate, `Proportion of population in rural OAs (%)` ) %>%
  ggplot() +
  geom_boxplot( aes( x = ST_cate, y = `Proportion of population in rural OAs (%)` ) ) +
  labs(
    title =
      paste0(
        'Distributions of the percent of population in rural OAs (%)\n'
        ,'across categories of Stability Index.'
      )
    ,subtitle = 'There are no differences across categories of Stability Index.'
    ,x = 'Stability-Index category'
    ,y = 'Percentage of population in rural OAs (%)'
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text( size = 15 )
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15 )
  )
ggsave(
  plot = plot__distribution_of_stability_index_by_pctOfRural
  ,filename = "plot__pctOfRural_and_StabilityIndex_are_not_associated.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# Trust size looks like nothing
plot__distribution_of_stability_index_by_Trust_size <-
  df %>%
  dplyr::select(ST_cate, `Trust size 2021 03`, `Trust size 2022 03`, `Trust size 2023 03`) %>%
  tidyr::pivot_longer(
    cols = contains('Trust')
    ,names_to = 'Year'
    ,values_to = 'Size'
  ) %>%
  ggplot() +
  geom_boxplot( aes( x = ST_cate, y = Size ) ) +
  labs(
    title =
      paste0(
        'Distributions of the sizes of Trusts across\n'
        ,'categories of Stability Index.'
      )
    ,subtitle = 'There are no differences across categories of Stability Index.'
    ,x = 'Stability-Index category'
    ,y = 'Count of employees\nin the Trust'
  ) +
  facet_wrap( ~ Year ) +
  theme_bw() +
  theme(
    axis.title = element_text( size = 15 )
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15 )
  )
ggsave(
  plot = plot__distribution_of_stability_index_by_Trust_size
  ,filename = "plot__Trust_size_and_StabilityIndex_are_not_associated.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# Patient satisfaction
plot__distribution_of_stability_index_by_patientSatisfaction_score <-
  df %>%
  dplyr::select(ST_cate, contains( "satisfaction" ) ) %>%
  tidyr::pivot_longer(
    cols = contains('satisfaction')
    ,names_to = 'Question'
    ,values_to = 'Score'
  ) %>%
  ggplot() +
  geom_boxplot( aes( x = ST_cate, y = Score ) ) +
  labs(
    title =
      paste0(
        'Distributions of patient satisfaction of Trusts across '
        ,'categories of Stability Index.'
      )
    ,subtitle = 'There are no differences across categories of Stability Index.'
    ,x = 'Stability-Index category'
    ,y = 'Satisfaction'
  ) +
  facet_wrap( ~ Question, nrow = 5 ) +
  theme_bw() +
  theme(
    axis.title = element_text( size = 15 )
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15 )
  )
ggsave(
  plot = plot__distribution_of_stability_index_by_patientSatisfaction_score
  ,filename = "plot__satisfaction_and_StabilityIndex_are_not_associated.png"
  ,dpi = 300
  ,width = 30
  ,height = 20
  ,units = "cm"
)
# ----
