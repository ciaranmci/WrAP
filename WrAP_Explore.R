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
ls_churn_within_NHS[[1]] %>% 
  dplyr::filter(
    `Care setting` == "All care settings"
    ,`AfC band` == "All AfC bands"
    ) %>%
  filter( `Stability index` %in% c(0,1) ) %>% 
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Stability index` )
  ) %>%
  write.csv( file = "Tables/count_of_0_or_1_SI_per_year.csv")
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
