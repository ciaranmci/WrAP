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
dir.create( file.path( getwd(), 'Plots/Stability index/WITHIN NHS' ), recursive = TRUE ) %>% suppressWarnings()
dir.create( file.path( getwd(), 'Plots/Stability index/FROM NHS' ), recursive = TRUE ) %>% suppressWarnings()
dir.create( file.path( getwd(), 'Tables/Stability index/WITHIN NHS' ), recursive = TRUE ) %>% suppressWarnings()
dir.create( file.path( getwd(), 'Tables/Stability index/FROM NHS' ), recursive = TRUE ) %>% suppressWarnings()
# ----

###########################################################
## Sensitivity analysis of dropping sites with counts of ##
## professions that have x or fewer.                     ##
###########################################################
# This section produces "tallies of sites with X-many professions, by period.csv"
# ----
ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
  ) %>%
  dplyr::group_by( Period, `Org code` ) %>%
  dplyr::summarise( n_roles = length( unique( `Care setting` ) ) ) %>%
  dplyr::ungroup() %>%
  dplyr::reframe( n_sites = n(), .by = c( Period, n_roles ) ) %>%
  dplyr::arrange( Period, -n_roles ) %>%
  write.csv( file = "Tables/tallies of sites with X-many professions, by period.csv" )
# ----

#################################################
## Count of trusts with SI = 0% and SI = 100%. ##
#################################################
# This section produces "count_of_0_or_1_SI_per_year.csv"
# ----
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  filter( `Stability index` %in% c(0,1) ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Stability index` )
  ) %>%
  write.csv( file = "Tables/count_of_0_or_1_SI_per_year_.csv")

df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  filter( `Stability index` %in% c(0,1) ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Care setting`, `Stability index` )
  ) %>% 
  write.csv( file = "Tables/count_of_0_or_1_SI_per_year_per_profession.csv")
  
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::mutate(
    SI_val_category = 
      dplyr::case_when(
        `Stability index` == 0 ~ "0%"
        ,`Stability index` == 1 ~ "100%"
        ,.default = "0% < SI < 100%"
        )
    ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Care setting`, SI_val_category )
  ) %>% 
  dplyr::arrange( year,  `Care setting`, SI_val_category ) %>% View()#dplyr::filter( year == "March '24 to March '25") %>% View()
  write.csv( file = "Tables/count_of_0_1_or_in_betweeen_SI_per_year_per_profession.csv")
# ----

##################################################################
## Select highest and lowest-scoring Trusts by stability index. ##
##################################################################
# This section produces:
# 1. "smallest_stability_index_per_profession.csv"
# 2. "largest_stability_index_per_profession.csv"
# 3. "Yorkshire__smallest_stability_index_per_profession.csv"
# 4. "Yorkshire__largest_stability_index_per_profession.csv"
#
# Michaela requested:
#   "We then select  2 Trusts for each profession (for all Professions that it
#    is possible to do so)  with the lowest and highest Remainer Rate using low,
#    middle and high grouping,  in our Yorkshire and Humber Patch if possible. I
#    will get the list of 'Trusts in our Patch'.  If not, we can go outside our 
#    patch."
# I will select Trusts just within Yorkshire and Humber, and separately select
# without that constraint.
# ----
# Select Trusts.
trust_selection_data <-
  ls_churn_within_NHS[[1]] %>% 
  dplyr::filter(
    `Care setting` != "All care settings"
    # Remove SI = 0 and SI = 1
    ,!`Stability index` %in% c(0,1)
    )
# ## Select 2 Trusts for each profession.
trust_selection_data %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`, `Stability index` ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_min( `Stability index`, n = 2, na_rm = TRUE ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  write.csv( file = "Tables/smallest_stability_index_per_profession.csv")
trust_selection_data %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_max( `Stability index`, n = 2, na_rm = TRUE ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  write.csv( file = "Tables/largest_stability_index_per_profession.csv")
# ## Limit the selection to the Yorkshire area.
trust_selection_data %>%
  dplyr::filter( `NHSE region name` == "North East and Yorkshire" ) %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_min( `Stability index`, n = 2, na_rm = TRUE ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>% 
  write.csv( file = "Tables/Yorkshire__smallest_stability_index_per_profession.csv")
trust_selection_data %>%
  dplyr::filter( `NHSE region name` == "North East and Yorkshire" ) %>%
  dplyr::select( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  dplyr::group_by( `Care setting` ) %>%
  dplyr::slice_max( `Stability index`, n = 2, na_rm = TRUE ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange( `Care setting`, Period, `Org code`, `Organisation name`,`Stability index` ) %>%
  write.csv( file = "Tables/Yorkshire__largest_stability_index_per_profession.csv")
# ----

#############################################################
## Make plots for qualitative assessment of distributions. ##
#############################################################
source('WrAP_Explore__plot_outcomes_of_interest.R')

####################
## Plots for IMD. ##
####################
source('WrAP_Explore__plot_IMD.R')

#########################
## Plots for Rurality. ##
#########################
source('WrAP_Explore__plot_rurality.R')

#####################################
## Plots for patient satisfaction. ##
#####################################
source('WrAP_Explore__plot_patient_satisfaction.R')

#############################
## Plots for staff survey. ##
#############################
source('WrAP_Explore__plot_staff_survey.R')

#########################################
## Correlation between SI and factors. ##
#########################################
source('WrAP_Explore__correlation_between_SI_and_factors.R')

#############################
## Repeated-measures test. ##
#############################
# The purpose of this section of script is to assess whether the stability-index
# values are similar year-on-year.
# The Friedman rank sum test assesses whether Trusts' year-on-year stability-
# index values have no consistent ordered over time.
# The pairwise Wilcoxon test assesses whether each pairwise set of the differences
# in stability-index values are symmetrical around 0.
# ----
# Create the dataset.
df <-
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` == "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
    ,!is.na( `Stability index` )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
  ) %>%
  dplyr::select(
    year_end
    ,`Org code`
    ,`Stability index`
  ) 
df %>%
  tidyr::pivot_wider(
    id_cols = `Org code`
    ,values_from = `Stability index`
    ,names_from = year_end
  ) %>%
  tidyr::drop_na() %>%
  dplyr::select( -`Org code`) %>%
  as.matrix() %>%
  stats::friedman.test()
# Post-hoc test.
pairwise.wilcox.test(
  df$`Stability index`
  ,df$year_end
  ,p.adj = "bonf"
)

# Create the dataset for Band 5s only.
df <-
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` == "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "5" )
    ,!is.na( `Stability index` )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
  ) %>%
  dplyr::select(
    year_end
    ,`Org code`
    ,`Stability index`
  ) 
df %>%
  tidyr::pivot_wider(
    id_cols = `Org code`
    ,values_from = `Stability index`
    ,names_from = year_end
  ) %>%
  tidyr::drop_na() %>%
  dplyr::select( -`Org code`) %>%
  as.matrix() %>%
  stats::friedman.test()
# Post-hoc test.
pairwise.wilcox.test(
  df$`Stability index`
  ,df$year_end
  ,p.adj = "bonf"
)
# ----

##########################################################
## SI for the professions with the largest head counts. ##
##########################################################
# This section produces "median_SI_of_profession_with_largest_headcount.csv"
# ----
df_highest_head_counts <- 
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
    ,!is.na( `Stability index` )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
  ) %>%
  dplyr::reframe(
    combined_head_count = sum( `Denominator at start of period`)
    ,.by = c( `Care setting`, year )
  ) %>%
  dplyr::group_by( year  ) %>%
  dplyr::slice_max( combined_head_count, n = 5, na_rm = TRUE ) %>%
  dplyr::ungroup()


ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` != "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
    ,!is.na( `Stability index` )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
  ) %>% 
  dplyr::inner_join(
    df_highest_head_counts
    ,by = c( "Care setting", "year" )
  ) %>%
  dplyr::reframe(
    median_SI = median( `Stability index`, na.rm = T )
    ,.by = c( `Care setting`, year )
  ) %>%
  dplyr::select( year, `Care setting`, median_SI ) %>%
  dplyr::arrange( year, `Care setting` ) %>%
  write.csv( "Tables/median_SI_of_profession_with_largest_headcount.csv")

# ----

#################################
## Study of staff-survey data. ##
#################################
# This study uses the staff-survey data, only. The motivation is that this data
# set is self-contained, has been processed less than the other datasets, and 
# it represents self-reported information rather than organisationally-reported
# information. This final point is important because we are interested in staff's
# experiences.
source('WrAP_Explore__staff_suvery_study.R')

#######################
## Julie's mega plot.##
#######################
# Julie wanted a plot of depriviation score across rurality categories, with point
# size indicating the size of the Trust, and the point colour indicating the
# stability index.
# ----

# Collate the required data.
plot_data <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == 'All AfC bands'
    # Don't distinguish profession.
    ,`Care setting` == "All care settings"
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::select( `Org code`, `Stability index` ) %>%
  dplyr::left_join(
    df_deprivation %>% dplyr::distinct( `Trust Code`, `IMD Score` )
    ,by = join_by( `Org code` == `Trust Code` )
  ) %>%
  dplyr::left_join(
    df_ons_rurality %>% dplyr::distinct( `Trust code`, `RUC21 settlement class` )
    ,by = join_by( `Org code` == `Trust code` )
  ) %>%
  dplyr::left_join(
    df_Trust_size_2023_03 %>% dplyr::distinct( `Trust code 2023 03`, `Trust size 2023 03` )
    ,by = join_by( `Org code` == `Trust code 2023 03` )
  ) %>%
  dplyr::arrange( `Stability index` )

# Make plot
p <-
plot_data %>%
  ggplot() +
  geom_point(
    aes(
      x = `IMD Score`
      ,y = `RUC21 settlement class`
      ,size = `Trust size 2023 03`
      ,colour = `Stability index`
      )
    ,position = position_jitter( height = 0.2 )
    ) +
  scale_x_continuous( limits = c( 0, 50 ) ) +
  labs(
      title =
        paste0(
        "Depriviation score across rurality categories in non-specialist acute"
        ,"\nTrusts in NHS England."
        )
      ,subtitle =
        paste0(
          "Explanation of variables:"
          ,"\n\u2022 Larger deprivation score indicates greater deprivation."
          ,"\n\u2022 Lighter-coloured stability index indicates more staff retention."
          ,"\n\u2022 Bigger circle indicates larger Trust size (by staff head count)."
          )
      ,caption =
        paste0(
          "Stability index from ", year_of_interest,"."
          ,"\nRurality category from 2021."
          ,"\nTrust size is staff head count from March 2023."
        )
      ,x = "Deprivation score"
      ,y = "Rurality category"
      ,size = "Trust size"
    ) +
  theme_minimal()

ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/plot__Julie's mega plot.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 11
  ,units = "cm"
)

# ----