# WrAP_Prepare data.R
#
# The purpose of this script is to prepare the data, e.g. ensure correct column
# types and format.
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

######################################
## Process publicly-available data. ##
######################################
# ----
# Churn data.

# Create function to process churn data.
fnc__processChurnData <-
  function( list_element )
{
  processed_list_element <-
    list_element %>%
    dplyr::rename(
      joiner_rate = `Joiner rate`
      ,leaver_rate = `Leaver rate`
    ) %>%

    dplyr::mutate(
      # Calculate the stability index using the rounded counts and call it
      # the `reaminer_rate`.
      remainer_rate = 
        ( `Denominator at start of period` - Leaver ) /
        `Denominator at start of period`
      # Define a variable that indicates when a number too small to be disclosed
      # was used in a calculation.
        ,too_small_to_disclose =
        dplyr::if_else(
          Joiner <= 5 | Leaver <= 5
          ,TRUE, FALSE
        )
      # Make a new variable called `year` that explains the `Period` variable
      # a bit better.
      ,year = dplyr::case_when(
        Period == '202203 to 202303' ~ "March '22 to March '23"
        ,Period == '202303 to 202403' ~ "March '23 to March '24"
        ,Period == '202403 to 202503' ~ "March '24 to March '25"
        ,.default = NULL
      )
      # Process the stability index so that it behaves like a number.
      ,`Stability index` = dplyr::if_else( `Stability index` == ".", NA, `Stability index` )
      ,`Stability index` = as.numeric( `Stability index` ) 
      # Standardise the case sensitivity of `Organisation name` so that it joins
      # with the deprivation data.
      ,`Organisation name` = tolower( `Organisation name` )
    ) %>%
    dplyr::filter(
      # Exclude rows referring to Integrated Care Boards and Clinical Commissioning Groups
      !`Benchmark group` %in% c( "Integrated Care Board", "Clinical Commissioning Group")
      )
}

# Apply function.
# ls_churn_from_NHS <-
#   lapply(
#     X = ls_churn_from_NHS
#     ,FUN = fnc__processChurnData
#   )
ls_churn_within_NHS <-
  lapply(
    X = ls_churn_within_NHS
    ,FUN = fnc__processChurnData
  )

# Deprivation data.
df_deprivation <- 
  df_deprivation %>%
  dplyr::mutate( `Trust Name` = tolower( `Trust Name` ) )

# Rurality data.
# ## The rurality data is by local authority district (LAD) in 2021 (LAD21CD). I
# ## need to find out the LAD21CD for all the Trusts. The best I could do was to
# ## get the LAD22CD for 2022 and map it to postcodes from 2021. I then need to
# ## match that with postcodes of the Trusts.
# ## Note that the following LADCDs contain Trusts with two post codes@
# ## - E06000058
# ## - E06000023
# ## - E08000032
# ## - E06000049
# # Get postcode-LAD mapping.
df_postcodeToLADCD <- dplyr::select( df_postcodeToLADCD, pcds, ladcd, ladnm )
# # Join Trusts to LAD codes.
df_TrustToLACDC <-
  dplyr::left_join(
    df_postcodeToTrust
    ,df_postcodeToLADCD
    ,by = join_by( pcds )
  )
# # Join Trust to rurality.
df_ons_rurality <-
  df_ons_rurality %>%
  dplyr::left_join(
    df_TrustToLACDC
    ,by = join_by( LAD21CD == ladcd )
    ,relationship = "many-to-many"
  )
# # Set factor orders.
df_ons_rurality$`RUC21 settlement class` <-
  factor(
    df_ons_rurality$`RUC21 settlement class`
    ,levels = c( 'Urban', 'Intermediate urban', 'Intermediate rural', 'Rural')
    )
# Trust-size data.
# None

# ----

#########################
## Process local data. ##
#########################
# ----

# Vacancy rates
for ( i_element in 1:length( ls_vacancy ) )
{
  # Extract the AHP role.
  ahp_role <- unique( ls_vacancy[[ i_element ]]$`Care setting` )
    
  # Extract the columns of interest.
  new_cols <-
    ls_vacancy[[ i_element ]] %>%
      dplyr::select(
        `Trust code`
        ,`vacancy_count_2022-04-01`
        ,`vacancy_count_2023-03-01`
        ,`vacancy_count_2024-03-01`
      ) %>%
    `colnames<-`(
      c( 'trust_code'
         ,paste0( "2022_vacancy_count_", ahp_role)
         ,paste0( "2023_vacancy_count_", ahp_role)
         ,paste0( "2024_vacancy_count_", ahp_role)
         )
    ) %>%
    dplyr::mutate_at(
      .vars = vars( contains( "vacancy" ) )
      ,.funs = abs
    )
    if( i_element == 1 )
    {
      df_vacancy <- new_cols
    } else {
      df_vacancy <-
        dplyr::full_join(
          df_vacancy
          ,new_cols 
          ,by = join_by( trust_code )
        )
      }
}


# Patient satisfaction.
df_patientSatisfaction <- 
  df_patientSatisfaction %>%
  dplyr::select(
    trust_code, trust_name
    ,q18_mean_2024 = q18_mean, q21_mean_2024 = q21_mean
    ,q23_mean_2024 = q23_mean, q30_mean_2024 = q30_mean, q48_mean_2024 = q48_mean
  )
df_patientSatisfaction_historic <- 
  df_patientSatisfaction_historic %>%
  dplyr::select(
    trust_code, trust_name, survey_year
    ,q18_mean = q18_mean_h, q21_mean = q21_mean_h
    ,q23_mean = q23_mean_h, q30_mean = q30_mean_h, q48_mean = q48_mean_h
  ) %>%
  tidyr::pivot_wider(
    id_cols = c( trust_code, trust_name )
    ,names_from = survey_year
    ,values_from = c( q18_mean, q21_mean, q23_mean, q30_mean, q48_mean )
  )
df_patientSatisfaction <-
  dplyr::left_join(
    df_patientSatisfaction
    ,df_patientSatisfaction_historic
    ,by = join_by( trust_code, trust_name )
  ) %>%
  dplyr::rename_with(
    .fn = ~ sub( pattern = "q", replacement = "satisfaction_q", .x )
    ,.cols = starts_with( 'q' )
  )
rm( df_patientSatisfaction_historic )


# Staff survey.
df_staff_survey <-
  df_staff_survey_main %>%
  # Define a column that will match with the `Period` column in the turnover data, later.
  dplyr::mutate(
    Period = dplyr::case_when(
      year_date == "01/12/2023" ~ "March '22 to March '23"
      ,year_date == "01/12/2024" ~ "March '23 to March '24"
      ,.default = NULL
    )
  ) %>%
  tidyr::drop_na( Period ) %>%
  # Select only the columns of interest.
  dplyr::select(
    c(
      Period
      ,org_id
      ,org_name
      ,region_code
      ,region_name
      ,job_role
      ,area_of_work
      ,q2a    
      ,q3i
      ,q4d
      ,q5a
      ,q4c
      ,q9a
      ,q9i
      ,q11c
      ,q21
      ,q24d
      ,q25d
      ,q25f
      ,q26c
      ,q26a
    )
  ) %>%
  # Rename columns to match turnover data.
  dplyr::rename(
    `Org code` = org_id
    ,`Organisation name` = org_name
    ,`NHSE code` = region_code
    ,`NHSE region name`= region_name
    ,`Care setting` = job_role
  ) %>%
  # Rename question columns to be more explicit
  dplyr::rename_with(
    .fn = ~ sub( pattern = "q", replacement = "staffSurvey_q", .x )
    ,.cols = starts_with( 'q' )
  )
  # Tidy up
  dplyr::distinct() %>%
  dplyr::arrange( `Org code`, `Care setting` )

# ----

#####################
## Join data sets. ##
#####################
# ----

# Create function for joining.
fnc__joinToChurnData <-
  function( list_element )
{
  processed_list_element <-
    # Join with deprivation dataset.
    list_element %>%
    dplyr::left_join(
      df_deprivation %>% dplyr::select( `Trust Code`, `Trust Name`, `IMD Score` )
      ,by = join_by(
        `Organisation name` == `Trust Name`
      )
    ) %>%
    # Join with vacancy-rates dataset.
    dplyr::left_join(
      df_vacancy
      ,by = join_by(
        `Org code` == trust_code
      )
    ) %>%
    # Join with rurality dataset.
    dplyr::left_join(
      df_ons_rurality %>%
        dplyr::select(
          c(
            `Trust code`
            ,`Rural Urban flag`
            ,`RUC21 settlement class`
            ,`RUC21 relative access`
            ,`Proportion of population in rural OAs (%)`
            ,`Proportion of population in OAs further from a major town or city (%)`
          )
        )
      ,by = join_by(
        `Org code` == `Trust code`
      )
    ) %>%
    # Join with Trust-size datasets.
    dplyr::left_join(
      df_Trust_size_2021_03 %>% dplyr::select( - `Trust name 2021 03` )
      ,by = join_by(
        `Org code` == `Trust code 2021 03`
      )
    ) %>%
    dplyr::left_join(
      df_Trust_size_2022_03 %>% dplyr::select( - `Trust name 2022 03` )
      ,by = join_by(
        `Org code` == `Trust code 2022 03`
      )
    ) %>%
    dplyr::left_join(
      df_Trust_size_2023_03 %>% dplyr::select( - `Trust name 2023 03` )
      ,by = join_by(
        `Org code` == `Trust code 2023 03`
      )
    ) %>%
    # Join with patient satisfaction dataset.
    dplyr::left_join(
      df_patientSatisfaction %>% dplyr::select( -trust_name )
      ,by = join_by(
        `Org code` == trust_code
      )
    ) %>%
    # Join with staff survey.
    dplyr::left_join(
      df_staff_survey
      ,by = join_by(
        `Org code` == `ord_id`
      )
    )
  processed_list_element <-
    processed_list_element %>%
    dplyr::select( -`Trust Code` )
}

# Applying function.
ls_churn_within_NHS <-
  lapply(
    X = ls_churn_within_NHS
    ,FUN = fnc__joinToChurnData
  )
# ls_churn_from_NHS <-
#   lapply(
#     X = ls_churn_from_NHS
#     ,FUN = fnc__joinToChurnData
#   )
# ----