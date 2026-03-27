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
    # Rename columns.
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
      ,year_end = dplyr::case_when(
        stringr::str_sub( year, start = -2, end = -1) == "25" ~ 2025
        ,stringr::str_sub( year, start = -2, end = -1) == "24" ~ 2024
        ,stringr::str_sub( year, start = -2, end = -1) == "23" ~ 2023
        ,stringr::str_sub( year, start = -2, end = -1) == "22" ~ 2022
        ,.default = NULL
      )
      # Process the stability index so that it behaves like a number.
      ,`Stability index` = dplyr::if_else( `Stability index` == ".", NA, `Stability index` )
      ,`Stability index` = as.numeric( `Stability index` ) 
      # Standardise the case sensitivity of `Organisation name` so that it joins
      # with the deprivation data.
      ,`Organisation name` = tolower( `Organisation name` )
      # Format the text of the profession values.
      ,`Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
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
# Vacancy rates
# ----
for ( i_element in 1:length( ls_vacancy ) )
{
  # Extract the AHP role.
  ahp_role <- unique( ls_vacancy[[ i_element ]]$`Care setting` )
    
  # Extract the columns of interest.
  new_cols <-
    ls_vacancy[[ i_element ]] %>%
    # ## I filter for the third month so that it aligns with the stability-index
    # ## values that are all for the year ending March. It is important note that
    # ## that these vacancy statistics therefore refer to the most-recent month
    # ## rather than summarising the previous year up to that month.
    dplyr::filter( vacancy_month == 3 ) %>%
    dplyr::select( -c( `Trust name`, vacancy_month ) ) %>%
    # ## I calculate the absolute values of the counts because the minus sign
    # ## is an indicator used by NHS England to indicate over-staffing. We don't
    # ## need this 'directional' information.
    dplyr::mutate_at(
      .vars = vars(4:6)
      ,.funs = abs
    )
    if( i_element == 1 )
    {
      df_vacancy <- new_cols
    } else {
      df_vacancy <-
        dplyr::bind_rows(
          df_vacancy
          ,new_cols 
        )
      }
}
# ----

# Patient satisfaction.
# ----
# Select columns of interest.
df_patientSatisfaction <- 
  df_patientSatisfaction %>%
  tibble::add_column(
    ps_survey_year = 2024
    ,.after = "trust_name"
  ) %>%
  dplyr::select(
    trust_code, ps_survey_year
    ,ps_q18_mean = q18_mean, ps_q21_mean = q21_mean
    ,ps_q23_mean = q23_mean, ps_q30_mean = q30_mean, ps_q48_mean = q48_mean
  )
# Append historic data.
df_patientSatisfaction <- 
  df_patientSatisfaction_historic %>%
  dplyr::select(
    trust_code, ps_survey_year = survey_year
    ,ps_q18_mean = q18_mean_h, ps_q21_mean = q21_mean_h
    ,ps_q23_mean = q23_mean_h, ps_q30_mean = q30_mean_h, ps_q48_mean = q48_mean_h
  ) %>%
  dplyr::bind_rows( df_patientSatisfaction )
rm( df_patientSatisfaction_historic )
# ----

# Staff survey.
# ----
# We might choose to compare the staff survey data between the 'main' and 'bank'
# datasets. But, for now, I only process the 'main' dataset. Take note that the
# technical specification for the survey says "Any comparisons between results
# for bank only and substantive staff should be made with caution due to
# differences in the survey methodology/questions asked and differences in the
# profile of bank workers and staff with a substantive contract. Please see the
# NSSB Technical Guide for further information about the version of the survey
# for bank only workers."

df_staff_survey_main <-
  df_staff_survey_main %>%
  # Re-code the `job_role` column to match the `Care setting` options.
  dplyr::left_join(
    df_jobroleToCaresetting 
    ,by = join_by( job_role )
  ) %>%
  # Re-code the year.
  dplyr::rename( ss_year = year_date ) %>%
  dplyr::mutate(
    ss_year = dplyr::case_when(
      stringr::str_sub( ss_year, start = -2, end = -1) == "24" ~ 2024
      ,stringr::str_sub( ss_year, start = -2, end = -1) == "23" ~ 2023
      ,stringr::str_sub( ss_year, start = -2, end = -1) == "22" ~ 2022
      ,.default = NULL
    )
  ) %>%
  tidyr::drop_na( ss_year ) %>%
  # Select only the columns of interest.
  dplyr::select(
    c(
      ss_year
      ,`Care setting`
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
      ,q26a
      ,q26c
    )
  ) %>%
  # Convert Likert scaling to scores as per Table 2 in the technical specification
  # for the survey.
  # (https://www.nhsstaffsurveys.com/static/ea079b722ad235a21b0356670766a33b/NHS-Staff-Survey-2025-Technical-Guide-V1.pdf)
  # The Likert scale 0-5 is scored as { 1 = 0, 2 = 2.5, 3 = 5, 4 = 7.5, 5 = 10 }
  # for every question except for the following:
  # - q11c : 1 = 0, 2 = 10
  # - q26a : 1 = 10, 2 = 7.5, 3 = 5, 4 = 2.5 , 5 = 0
  # - q26c : 1 = 10, 2 = 7.5, 3 = 5, 4 = 2.5 , 5 = 0
  dplyr::mutate(
    across(
      c( q2a, q3i, q4d, q5a, q4c, q9a, q9i, q21, q24d, q25d, q25f )
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 0
          ,x == 2 ~ 2.5
          ,x == 3 ~ 5
          ,x == 4 ~ 7.5
          ,x == 5 ~ 10
          ,.default = NULL
          )
        }
      )
  ) %>%
  dplyr::mutate(
    across(
      q11c
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 0
          ,x == 2 ~ 10
          ,.default = NULL
        )
      }
    )
  ) %>%
  dplyr::mutate(
    across(
      c( q26a, q26c )
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 10
          ,x == 2 ~ 7.5
          ,x == 3 ~ 5
          ,x == 4 ~ 2.5
          ,x == 5 ~ 0
          ,.default = NULL
        )
      }
    )
  ) %>%
  # Rename columns to match turnover data.
  dplyr::rename( `Org code` = org_id ) %>%
  # Rename question columns.
  dplyr::rename_with(
    .fn = ~ sub( pattern = "q", replacement = "ss_q", .x )
    ,.cols = starts_with( 'q' )
  ) %>% 
  # Collapse rows for professions within an organisation.
  dplyr::select( -c( area_of_work, job_role, org_name, region_code, region_name ) ) %>%
  dplyr::reframe(
    across( contains( "ss_q"), median )
    ,.by = !contains( "ss_q")
  ) %>%
  # Tidy up.
  dplyr::distinct() %>%
  dplyr::arrange( `Org code`, `Care setting` )

# Unlike with the patient satisfaction scores, there is no staff survey 
# score that applies to the "All care settings" `Care setting`. Instead, to 
# get a summary for all care settings, I compute the median value for every
# staff survey question separately across all the professionals within each
# Trust.
df_staff_survey_main <-
  df_staff_survey_main %>%
    tidyr::pivot_longer(
      cols = c( contains('ss_'), -one_of( "ss_year" ) )
      ,names_to = 'Question'
      ,values_to = 'Score'
    ) %>%
    dplyr::reframe(
      Score = median( Score, na.rm = T )
      ,.by = c( ss_year, `Org code`, Question )
    ) %>%
    tibble::add_column(
      `Care setting` = "All care settings"
      ,.after = "ss_year"
    ) %>% 
    tidyr::pivot_wider(
      id_cols = everything()
      ,names_from = Question
      ,values_from = Score
    ) %>%
    dplyr::bind_rows( df_staff_survey_main )



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
        `Org code` == `Trust code`
        ,`Care setting` == `Care setting`
        ,year_end == vacancy_year
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
        ) %>%
        dplyr::distinct() %>%
        tidyr::drop_na( `Trust code`)
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
      df_patientSatisfaction
      ,by = join_by(
        `Org code` == trust_code
        ,year_end == ps_survey_year
      )
    ) %>%
    # Join with staff survey.
    dplyr::left_join(
      df_staff_survey_main
      ,by = join_by(
        `Org code` == `Org code`
        ,`Care setting` == `Care setting`
        ,year_end == ss_year
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