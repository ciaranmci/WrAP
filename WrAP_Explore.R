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
dir.create( file.path( getwd(), 'Plots' ) ) %>% suppressWarnings()
# ----


###########################################################
## Sensitivity analysis of dropping sites with counts of ##
## professions that have x or fewer.                     ##
###########################################################
# ----

# Add a column marking the remainer-rate values that are within the 90th
# percentile for that period and profession. Do the same for the 10th
# percentile.
x <-
  ls_churn_within_NHS[[1]] %>% 
  dplyr::group_by( `Org code`, Period ) %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
  ) %>%
  dplyr::ungroup() %>%
  # Remove data with counts that are too small to disclose.
  dplyr::filter( !too_small_to_disclose ) %>% 
  # Format the text of the profession values.
  dplyr::mutate(
    `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) 


# How many sites are represented by only 1, 2, 3 etc professions?
x %>%
  dplyr::group_by( Period, `Org code` ) %>%
  dplyr::summarise( n_roles = length( unique( `Care setting` ) ) ) %>%
  dplyr::ungroup() %>%
  dplyr::reframe( n_sites = n(), .by = c( Period, n_roles ) ) %>%
  dplyr::arrange( Period, -n_roles ) %>%
  write.csv( file = "tallies of sites with X-many professions, by period.csv" )
# ----


############################
## Apply the 5-step plan. ##
############################
# 1. Firstly, we need to exclude Call Handler and Emergency Department
#    (paramedics) Roles from any analysis. 
#       - This is undertaken in `WrAP_Load data.R` and `WrAP_Prepare data.R`.
# 2. We can exclude Trusts that have 4 Professions or less. This is a random 
#    choice selection so not sure if 3 or less makes more sense in terms of
#    numbers but it could be either 3 or 4. 
#       - Michaela is back-tracking on this in an email sent 6th Jan 2026:
#         "I have just thought that if we are selecting 2-3 Trusts per
#          profession, it doesn’t matter if they have 4 professions or more, as
#          long as they have THAT profession. Unless there’s some other reason
#          behind that. I’ll ask."
# 3. We then Plot Remainers Rate by Trust, Profession and only Band 5 (Band 5
#    because Remainers rate shows differently than for the rest). We know that 
#    age may be similar but for the purpose of this selection, we focus on Band
#    5s only. 
#       - I  does not see evidence that the Band 5s are consistently
#         different from the rest, across all professions. Also, Band 5 staff
#         are only in Occupational Therapy, Operating Theatres, Physiotherapy,
#         and Radiography (diagnostics). This can been seen in `plot__distribution_of_remainer_rate_stratified_by_AfC_band_and_AHP_role_from_WITHIN_NHS_dataset.PNG` 
#         I will produce the plot for Michaela but I will use everyone's data
#         going forward.
# 4. Because the focus is on rural, coastal and deprived  Trusts, and because we
#    know that London is its own 'universe', we plot the distribution of Remain 
#    Rate excluding London.
#       - These are excluded by excluding `NHSE region name` == "London".
# 5. We then select  2 Trusts for each profession (for all Professions that it
#    is possible to do so)  with the lowest and highest Remainer Rate using low,
#    middle and high grouping,  in our Yorkshire and Humber Patch if possible. I
#    will get the list of 'Trusts in our Patch'.  If not, we can go outside our 
#    patch.
#       - I will select Trusts just within Yorkshire and Humber, and separately
#         select without that constraint.
# ----


# Select Trusts.
trust_selection_data <-
  ls_churn_within_NHS[[1]] %>% 
  dplyr::filter( `Care setting` != "All care settings" ) %>%
  # Remove data with counts that are too small to disclose.
  dplyr::filter( !too_small_to_disclose ) %>% 
  # Format the text of the profession values.
  dplyr::mutate(
    `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>%
  # Mark the remainer rates in the 90th and 10th percentiles.
  dplyr::group_by( Period, `Care setting` ) %>%
  dplyr::mutate(
    tenth_pctl_threshold = quantile( remainer_rate, probs = 0.1 )
    ,ninetyth_pctl_threshold = quantile( remainer_rate, probs = 0.9 )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    lowest = ( remainer_rate <= tenth_pctl_threshold )
    ,highest = ( remainer_rate >= ninetyth_pctl_threshold )
  ) 
# Select 2 Trusts for each profession, in each of the highest and lowest quintiles.
trust_selection_data %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`, remainer_rate ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_min( remainer_rate, n = 2) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  write.csv( file = "smallest_remainer_rates_per_profession.csv")
trust_selection_data %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_max( remainer_rate, n = 2) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  write.csv( file = "largest_remainer_rates_per_profession.csv")
# Limit the selection to the Yorkshire area.
trust_selection_data %>%
  dplyr::filter( `NHSE region name` == "North East and Yorkshire" ) %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_min( remainer_rate, n = 2) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  write.csv( file = "Yorkshire__smallest_remainer_rates_per_profession.csv")
trust_selection_data %>%
  dplyr::filter( `NHSE region name` == "North East and Yorkshire" ) %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_max( remainer_rate, n = 2) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,remainer_rate ) %>%
  write.csv( file = "Yorkshire__largest_remainer_rates_per_profession.csv")

# Plot Remainers Rate by Trust, Profession and only Band 5
p <-
  ls_churn_within_NHS$df_churn_from_NHS_Grade %>% View()

  dplyr::group_by( `Org code`, Period ) %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "Band 5" )
  ) %>%
  dplyr::ungroup() %>%
  # Remove data with counts that are too small to disclose.
  dplyr::filter( !too_small_to_disclose ) %>% 
  # Format the text of the profession values.
  dplyr::mutate(
    `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>%
  ggplot(
    aes(
      x = year
      ,y = remainer_rate
    ) 
  ) +
  geom_line( aes( group = `Org code` ) ) +
  geom_point() +
  facet_wrap(~`Care setting`
             ,labeller =
               labeller(
                 `Care setting` = label_wrap_gen( 10 )
               )
  ) +
  labs(
    title = "Remainer rate over time, per profession, per site."
    ,subtitle = 
      paste0(
        "\u2022 Excludes values calculated from counts too small to disclose.\n"
        ,"\u2022 Include Band 5 staff, only."
      )
    ,x = "Year"
    ,y = "Remainer rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot.
ggsave(
  plot = p
  ,filename = "Band_5_only__Remainer_rate_over_time_by_profession_by_site.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----





################################################################
## Plot of remainer rate over time, per profession, per site. ##
################################################################
# ----
p <-
  x %>%
  ggplot(
    aes(
      x = year
      ,y = remainer_rate
    ) 
  ) +
  geom_line( aes( group = `Org code` ) ) +
  geom_point() +
  facet_wrap(~`Care setting`
             ,labeller =
               labeller(
                 `Care setting` = label_wrap_gen( 10 )
               )
             ) +
  labs(
    title = "Remainer rate over time, per profession, per site."
    ,subtitle = "\u2022 Excludes values calculated from counts too small to disclose."
    ,x = "Year"
    ,y = "Remainer rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save plot.
ggsave(
  plot = p
  ,filename = "Remainer_rate_over_time_by_profession_by_site.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

######################################################  
## Plot trajectories through remainer-joiner space. ##
######################################################
# ----
sample_of_hospitals <- 
  x %>%
  dplyr::distinct( `Org code` ) %>%
  dplyr::sample_n( size = 4 )
  
p <-
  ls_churn_within_NHS[[1]] %>% 
  dplyr::group_by( `Org code`, Period ) %>%
  dplyr::filter(
    `Care setting` == "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
  ) %>%
  dplyr::ungroup() %>%
  # Remove data with counts that are too small to disclose.
  dplyr::filter( !too_small_to_disclose ) %>% 
  # Format the text of the profession values.
  dplyr::mutate(
    `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>%
  dplyr::inner_join(
    sample_of_hospitals
    ,by = join_by( `Org code` )
  ) %>% 
  dplyr::select( remainer_rate, joiner_rate
                 , `Org code`, `Denominator at start of period`
                 ,Period) %>%
  #dplyr::filter( Period == max( Period ) ) %>%
  dplyr::arrange( Period ) %>%
  ggplot(
    aes(
      x = remainer_rate
      ,y = joiner_rate
      )
    ) +
  geom_path(
    aes(
      group = `Org code`
    )
    ,arrow = arrow()
  ) +
  geom_point(
    aes(
      size = `Denominator at start of period`
      )
    ,alpha = 0.2
    ) +
  geom_text(
    data = . %>% dplyr::filter( Period == max( Period ) )
    ,aes( label = `Org code` )
    ,check_overlap = TRUE
    ,hjust = 0
    , nudge_x = 0.001
  ) +
  labs(
    title = "A sample (n = 4) of Remainer rates and Joiner rates, over time."
    ,subtitle = 
      paste0(
        "\u2022 Excludes values calculated from counts too small to disclose.\n"
        ,"\u2022 Time is indicated by the direction of the arrow.\n"
        ,"\u2022 Note the narrow range of values for both rates.\n"
        ,"\u2022 Trajectories end with the `Org code` of the site."
      )
    ,x = "Remainer rate"
    ,y = "Joiner rate"
    ) +
  theme_minimal() +
  guides(
    size = guide_legend( title = "Staff count\nat year start" )
    )


# Save plot.
ggsave(
  plot = p
  ,filename = "Trajectories_through_remainer-joiner_space.png"
  ,dpi = 300
  ,width = 15
  ,height = 15
  ,units = "cm"
)
# ----

##########################################################
## Plot of remainer rates and vacancy rates, over time. ##
##########################################################
# PROBLEM:
# It is unjustifiably difficult to plot a secondary axis on an independent
# scale, in ggplot. My current issue is that the secondary axis is scaled to the
# GRAND max and min rather than the facet specific max and min. I could just make
# plots in a loop for all professions but I'd rater it be sorted in a facet wrap.
# ----

# Prepare a data.frame of remainer rate per period per site.
remainer_rate_data <-
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` == "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
  ) %>%
  dplyr::ungroup() %>%
  # Remove data with counts that are too small to disclose.
  dplyr::filter( !too_small_to_disclose ) %>% 
  # Format the text of the profession values.
  dplyr::mutate(
    `Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>%
  dplyr::select(
    Period
    ,`Org code`
    ,remainer_rate
  ) %>%
  dplyr::mutate(
    year = stringr::str_sub( Period, start = -6, end = -3 ) %>% as.integer()
    ,month = 03
  ) %>%
  dplyr::select( -Period ) %>%
  dplyr::rename( `Trust code` = `Org code`)


# Make function for scaling a secondary variable to a secondary axis.
transformer_dual_y_axis <-
  function(
    data
    ,primary_column
    ,secondary_column
    ,include_y_zero = FALSE
  )
  {
    params_tbl <-
      data %>%
      summarise(
        max_primary = max(!!enquo(primary_column), na.rm = T )
        ,min_primary = min(!!enquo(primary_column), na.rm = T )
        ,max_secondary = max(!!enquo(secondary_column), na.rm = T )
        ,min_secondary = min(!!enquo(secondary_column), na.rm = T )
      )
    
    if( include_y_zero)
    {
      params_tbl$min_primary <- 0
      params_tbl$min_secondary <- 0
    }
    
    params_tbl <-
      params_tbl %>%
      mutate(
        scale = ( max_secondary - min_secondary ) / ( max_primary - min_primary )
        ,shift = min_primary - min_secondary
      )
    
    scale_func <-
      function(x)
      {
        x * params_tbl$scale - params_tbl$shift
      }
    inv_func <-
      function(x)
      {
        ( x + params_tbl$shift ) / params_tbl$scale
      }
    
    ret <-
      list(scale_func = scale_func
           ,inv_func = inv_func
           ,params_tbl = params_tbl)
    
    return( ret )
  }


# Bind all vacancy data by rows and pivot the vacancy data long.
vacancy_data <- 
  ls_vacancy %>%
  dplyr::bind_rows() %>%
  dplyr::select(
    `Care setting`
    ,`Trust code`
    ,contains("vacancy_rate")
  ) %>%
  tidyr::pivot_longer(
    cols = contains("vacancy_rate")
    ,names_to = "mydate"
    ,values_to = "vacancy_rate"
  ) %>%
  dplyr::mutate(
    mydate = gsub(pattern = "vacancy_rate_", replacement = "", x = mydate )
    ,year = lubridate::year( mydate )
    ,month = lubridate::month( mydate )
  ) %>%
 dplyr::select( -mydate ) %>%
 dplyr::filter( !is.na( vacancy_rate ) ) %>%
  # # Join the remainer rates.
  dplyr::left_join(
    remainer_rate_data
    ,by = join_by( year, month, `Trust code`)
  ) %>%
  dplyr::arrange( year, month, `Trust code`) %>%
  # # Make better x-axis variable.
  dplyr::mutate(
    time = paste( year, month, sep = ".")
  ) 
    
# Scale the data for plotting on the secondary axis. 
transformed_vacancy_data <-
  vacancy_data %>%
  transformer_dual_y_axis(
    primary_column = vacancy_rate
    ,secondary_column = remainer_rate
  )
    
  # Make the plot.
  vacancy_data2 <- vacancy_data %>% filter(`Trust code` == "TAD")
  
  vacancy_data2 %>%
    ggplot( aes( x = time ) ) +
    geom_line(
      aes(
        y = vacancy_rate
        ,group = `Trust code`
      )
    ) +
    geom_point(
      aes(
        y = transformed_vacancy_data$inv_func( remainer_rate )
        ,group = `Trust code`
      )
      ,size = 5
      ,colour  = "red"
    ) +
    scale_y_continuous(
      name = "Vacancy rate"
      ,sec.axis =
        sec_axis(
          transform = ~transformed_vacancy_data$scale_func(.)
          ,name = "Remainer rate"
          #,labels = scales::percent_format()
        )
    ) +
    facet_wrap(
      ~`Care setting`
      ,labeller = labeller( `Care setting` = label_wrap_gen( 10 ) )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text( angle = 45 )
    )
  
  


# Example from https://www.business-science.io/code-tools/2022/12/01/how-to-replicate-excel-s-secondary-y-axis-plot-in-r.html
#
# mpg_summarised_tbl %>%
#   ggplot( aes( x = class ) ) +
#   geom_col(
#     aes( y = prop, fill = "Vehicle Proportion (%)" ), alpha = 0.9
#   ) +
#   geom_label(
#     aes( y = prop, label = str_glue("{scales::percent(prop)}")
#          ,colour =  "Vehicle Proportion (%)")
#   ) +
#   geom_line(
#     aes(
#       y = ( hwy_median +  min(prop) - min(hwy_median) ) / (( max(hwy_median) - min(hwy_median) ) / ( max(prop) - min(prop) ))
#       ,group = all_groups
#       ,colour = "Highway MPG"
#     )
#     ,linewidth = 1
#   ) +
#   geom_point(
#     aes(
#       y = ( hwy_median +  min(prop) - min(hwy_median) ) / (( max(hwy_median) - min(hwy_median) ) / ( max(prop) - min(prop) ))
#       ,group = all_groups
#       ,colour = "Highway MPG"
#     )
#     ,size = 1
#   ) +
#   geom_label(
#     aes(
#       y = ( hwy_median +  min(prop) - min(hwy_median) ) / (( max(hwy_median) - min(hwy_median) ) / ( max(prop) - min(prop) ))
#       ,label = str_glue("{hwy_median} mpg")
#       ,colour = "Highway MPG"
#     )
#     ,size = 3
#     ,nudge_y = 0.008
#   ) +
#   scale_y_continuous(
#     labels = scales::percent_format()
#     ,name = "Vehicle Proportion (%)"
#     ,sec.axis =
#       sec_axis(
#         #transform = ~transformer$scale_func(.)
#         transform = ~ . * (( max(mpg_summarised_tbl$hwy_median) - min(mpg_summarised_tbl$hwy_median) ) / ( max(mpg_summarised_tbl$prop) - min(mpg_summarised_tbl$prop) )) - ( min(mpg_summarised_tbl$prop) - min(mpg_summarised_tbl$hwy_median) )
#         ,name = "Highway MPG"
#       )
#   ) +
#   expand_limits( y = c( 0, 0.3 ) )

# ----

#############################################################
## Make plots for qualitative assessment of distributions. ##
#############################################################
# The following function creates plots that show the distribution of a variable
# of interest unstratified, stratified by AHP role,  stratified by the
# candidate factors provided in the data set, and stratified by AHP role and by
# candidate factors.
#
# ****************** THESE FINDINGS WERE WRITTEN BEFORE SEPARATE PLOTS WERE MADE FOR THE WITHIN-NHS AND FROM-NHS TURNOVER *****************
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
    ,dataset_id = NULL # A character string of the dataset ID.
    )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( var_of_interest ) ){ stop( "The 'var_of_interest' argument was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
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
              "\u2022 Using ", dataset_id," dataset.\n"
              ,"\u2022 Excludes values calculated from counts too small to disclose.\n"
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
      
      # Save the overall plot.
      ggsave(
        plot = p
        ,filename =
          paste0(
            "Plots/plot__distribution_of_"
            ,var_of_interest
            ,"_from_"
            ,dataset_id
            ,"_dataset.png"
            )
        ,dpi = 300
        ,width = 15
        ,height = 10
        ,units = "cm"
      )
      
      
      ###########################
      ## Stratify by AHP role. ##
      ###########################
      # Create the plot data, stratified by staff role.
      plot_data <-
        data[[1]] %>%
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
              "\u2022 Using ", dataset_id," dataset.\n"
              ,"\u2022 Excludes values calculated from counts too small to disclose.\n"
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
      
      
      # Save plot, stratified by AHP role.
      ggsave(
        plot = last_plot()
        ,filename =
          paste0(
            "Plots/plot__distribution_of_"
            ,var_of_interest
            ,"_by_AHP_role"
            ,"_from_"
            ,dataset_id
            ,"_dataset.png"
            )
        ,dpi = 300
        ,width = 20
        ,height = 10
        ,units = "cm"
      )
      
      
      
      
      
      
      ########################################
      ## Stratify by candidate factor role. ##
      ########################################
      lapply(
        data
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
                    "\u2022 Using ", dataset_id," dataset.\n"
                    ,"\u2022 Excludes values calculated from counts too small to disclose.\n"
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
            
            
            # Save plot, stratified by candidate factor.
            ggsave(
              plot = p
              ,filename =
                paste0(
                  "Plots/plot__distribution_of_"
                  ,var_of_interest
                  ,"_stratified_by_"
                  ,stratification_name
                  ,"_from_"
                  ,dataset_id
                  ,"_dataset.png"
                )
              ,dpi = 300
              ,width = 20
              ,height = 20
              ,units = "cm"
            )
            
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
        data
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
                    "\u2022 Using ", dataset_id," dataset.\n"
                    ,"\u2022 Excludes values calculated from counts too small to disclose.\n"
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
            
            
            # Save plot, stratified by candidate factor.
            ggsave(
              plot = p
              ,filename =
                paste0(
                  "Plots/plot__distribution_of_"
                  ,var_of_interest
                  ,"_stratified_by_"
                  ,stratification_name
                  ,"_and_AHP_role"
                  ,"_from_"
                  ,dataset_id
                  ,"_dataset.png"
                )
              ,dpi = 300
              ,width = 35
              ,height = 20
              ,units = "cm"
            )
            
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
# # Using the `ls_churn_from_NHS` dataset.
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'joiner_rate', dataset_id = "FROM_NHS" )
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'leaver_rate', dataset_id = "FROM_NHS" )
fnc__analyseDistributions(
  data = ls_churn_from_NHS, var_of_interest = 'remainer_rate', dataset_id = "FROM_NHS" )
# # Using the `ls_churn_within_NHS` dataset.
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'joiner_rate', dataset_id = "WITHIN_NHS" )
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'leaver_rate', dataset_id = "WITHIN_NHS" )
fnc__analyseDistributions(
  data = ls_churn_within_NHS, var_of_interest = 'remainer_rate', dataset_id = "WITHIN_NHS" )
# ----










########################################################################
## Prepare data for unstratified investigation and for AHP-stratified ##
## investigation.                                                     ##
########################################################################
# ----
# Write function.
fnc__prepareDataForAHPstratification <-
  function(
    data = NULL # One of the `ls_churn_*` datasets.
    )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    
    output <- 
      data[[1]] %>%
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
    
    stratified_output <-
      output %>%
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
    
    combined_output <- list( output, stratified_output )
    
    return( combined_output )
  }

# Apply the function.
df_within_NHS <-
  fnc__prepareDataForAHPstratification( ls_churn_from_NHS )
df_from_NHS <-
  fnc__prepareDataForAHPstratification( ls_churn_from_NHS )
# ----

####################
## Plots for IMD. ##
####################
# ----
# Write the functions.
fnc__IMDplots <-
  function(
    data = NULL # One of the `df_within_NHS` or `df_from_NHS` datasets.
    ,dataset_id = NULL # A character string of the dataset ID.
    )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    # Unstratified.
    p <-
      data[[1]] %>%
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
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
      ,filename =
        paste0(
          "Plots/plot__rates_stratified_by_IMD"
          ,"_from_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 20
      ,height = 20
      ,units = "cm"
    )
    
    # Stratified by AHP role.
    # # Make plots.
    p <-
      data[[2]] %>%
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
                      "\u2022 Using ", dataset_id," dataset.\n"
                      ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
            }
          )
      )
    # # Save plots.
    for( i in 1:nrow( data[[2]] ) )
    {
      ggsave(
        plot = p$plots[[ i ]]
        ,filename =
          paste0(
            "Plots/plot__"
            ,p$`Care setting`[ i ]
            ,"_rates_stratified_by_IMD"
            ,"_from_"
            ,dataset_id
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 21
        ,units = "cm"
      )
    }
  }

# Apply the functions.
fnc__IMDplots( data = df_within_NHS, dataset_id = 'WITHIN_NHS' )
fnc__IMDplots( data = df_from_NHS, dataset_id = 'FROM_NHS' )

# ----

########################
## Plots for rurality ##
########################
# ----
# Write the function.
fnc__ruralityPlots <-
  function(
    data = NULL # One of the `df_within_NHS` or `df_from_NHS` datasets.
    ,dataset_id = NULL # A character string of the dataset ID.
    )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    # Unstratified.
    p <-
      data[[1]] %>%
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
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
      ,filename =
        paste0(
          "Plots/plot__rates_stratified_by_rurality"
          ,"_from_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 25
      ,height = 15
      ,units = "cm"
    )
    
    # Stratified by AHP role.
    p <-
      data[[2]] %>%
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
                      "\u2022 Using ", dataset_id," dataset.\n"
                      ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
    for( i in 1:nrow( data[[2]] ) )
    {
      ggsave(
        plot = p$plots[[ i ]]
        ,filename =
          paste0(
            "Plots/plot__"
            ,p$`Care setting`[ i ]
            ,"_rates_stratified_by_rurality"
            ,"_from_"
            ,dataset_id
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 25
        ,height = 15
        ,units = "cm"
      )
}
  }

# Apply the functions.
fnc__ruralityPlots( data = df_within_NHS, dataset_id = 'WITHIN_NHS' )
fnc__ruralityPlots( data = df_from_NHS, dataset_id = 'FROM_NHS' )

# ----

#####################################
## Plots for patient satisfaction. ##
#####################################
# ----

# Create the function.
fnc__PSplots <-
  function(
    data = NULL # One of the `df_within_NHS` or `df_from_NHS` datasets.
    ,dataset_id = NULL # A character string of the dataset ID.
  )
  {
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' arguement was not supplied." ) }
    if( is.null( dataset_id ) ){ stop( "The 'dataset_id' arguement was not supplied." ) }
    
    # Unstratified.
    # # Prepare data
    ps_data <-
      data[[1]] %>%
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
      data[[1]] %>%
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
          ,by = join_by( Year, `Org code`, `Care setting`, category, statistic )
        )

    # # Plot satisfaction scores for all care settings, unstratfied.
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
            "\u2022 Using ", dataset_id," dataset.\n"
            ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
      ,filename =
        paste0(
          "Plots/plot__rates_stratified_by_PatientSatisfaction_from_"
          ,dataset_id
          ,"_dataset.png"
        )
      ,dpi = 300
      ,width = 30
      ,height = 20
      ,units = "cm"
    )
    
    
    # Stratified by AHP role.
    p <-
      data[[2]] %>%
      dplyr::mutate(
        plots =
          lapply(
            data
            ,function(x)
            {
              # # Prepare data.
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
                x %>%
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
                      "\u2022 Using ", dataset_id," dataset.\n"
                      ,"\u2022 'Undisclosed' means staff counts were too small to disclose.\n"
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
    for( i in 1:nrow( data[[2]] ) )
    {
      ggsave(
        plot = p$plots[[ i ]]
        ,filename =
          paste0(
            "Plots/plot__"
            ,p$`Care setting`[ i ]
            ,"_rates_stratified_by_PatientSatisfaction_from_"
            ,dataset_id
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 21
        ,units = "cm"
      )
    }
    
  }

# Apply the function.
fnc__PSplots( data = df_within_NHS, dataset_id = 'WITHIN_NHS' )
fnc__PSplots( data = df_from_NHS, dataset_id = 'FROM_NHS' )

# ----


