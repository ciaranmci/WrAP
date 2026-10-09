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

#################
## Requisites. ##
#################
# ----
if( !exists( "df_churn_within_NHS_Grade" ) )
{
  source( "WrAP_Load data.R" ) 
  }
# ----

######################################
## Process publicly-available data. ##
######################################
# ----
# Churn data.
# # Create function to process churn data.
fnc__processChurnData <-
  function( df )
{
  processed_df <-
    df %>%
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
      )%>%
    dplyr::mutate(
        `Care setting` = 
          dplyr::case_when(
            `Care setting` == "Operating Theatres" ~ "Operating Department Practitioners"
            # ,`Care setting` == "Orthoptics / Optics" ~ "Orthoptics"
            ,.default = `Care setting`
          )
      )
}
# # Apply function.
if( !"year_end" %in% colnames( df_churn_within_NHS_Grade ) )
  { df_churn_within_NHS_Grade <- fnc__processChurnData( df_churn_within_NHS_Grade ) }
if( !"year_end" %in% colnames( df_churn_within_NHS_Gender ) )
  { df_churn_within_NHS_Gender <- fnc__processChurnData( df_churn_within_NHS_Gender ) }
if( !"year_end" %in% colnames( df_churn_within_NHS_AgeBand ) )
  { df_churn_within_NHS_AgeBand <- fnc__processChurnData( df_churn_within_NHS_AgeBand ) }
if( !"year_end" %in% colnames( df_churn_within_NHS_EthnicGroup ) )
  { df_churn_within_NHS_EthnicGroup <- fnc__processChurnData( df_churn_within_NHS_EthnicGroup ) }

# Deprivation data.
df_deprivation <- 
  df_deprivation %>%
  dplyr::mutate( `Trust Name` = tolower( `Trust Name` ) )


# Rurality data.
# ## The rurality data is by local authority district (LAD) in 2021 (LAD21CD). I
# ## need to find out the LAD21CD for all the Trusts. The best I could do was to
# ## get the LAD22CD for 2022 and map it to postcodes from 2021. I then need to
# ## match that with postcodes of the Trusts.
# ## Note that the following LADCDs contain Trusts with two post codes:
# ## - E06000058
# ## - E06000023
# ## - E08000032
# ## - E06000049
# # Get postcode-LAD mapping.
if( !exists( "df_TrustToLACDC" ) )
{
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
      ) %>%
    ordered()
  # Trust-size data.
  # None
  
  # Save.
  saveRDS( df_ons_rurality, "Processed datasets/Paper 1/df_ons_rurality.RDS" )
}

# ----

#########################
## Process local data. ##
#########################
# ----
# Vacancy rates
# ## It is worth explaining how the vacancy rate is calculated because it leads
# ## to all kinds of mess. The formula for vacancy rate is:
# ##
# ##          Vacancy rate = vacancies / (vacancies + staff in post)
# ## 
# ## The staff in post is sometimes a negative number. The minus sign indicates
# ## over-staffing relative to the number of staff that the Trust is funded to
# ## employ. This leads to issues when, for example a Trust is employing one 
# ## person for whom they don't have funding. This leads to a 0 denominator,
# ## gives a divide-by-zero error that manifests as an NA.
# ## The whole thing is made more complicated because fractional posts are 
# ## possible, e.g. a half-time post is recorded as 0.5 staff. Tiny differences
# ## explode the vacancy rate. For example, operating department practitioners
# ## at Lewisham and Greenwich NHS Trust had a vacancy rate of 554654.7% in March
# ## 2025 despite having about 18 staff in post. The huge vacancy rate was caused
# ## by the fractional staff in post of 18.466-recurring but a vacancy count of
# ## -18.47. This makes for a denominator of -0.0033-recurring that, when dividing
# ## almost any expected numerator will produce a huge vacancy rate.
# ##
# ## So, to summarise:
# ## - A 0% vacancy rate means someone is in post and there are no vacancies.
# ## - A 100% vacancy rate means either:
# ##    1. no staff are in post and there are vacancies to fill.
# ##    2. no staff are in post yet the records say the Trust is over-staffed.
# ##       This makes no sense. It is unclear if the staff-in-post data are wrong
# ##       or the vacancy data are wrong. These vacancy rates should be excluded.
# ## - A >100% vacancy rate means someone is in post and the records say the 
# ##   Trust is over-staffed by an amount greater than the number of staff in
# ##   post. I treat this as an administrative error and set this to 0% vacancy.
# ## - A <0% vacancy rate means someone is in post and the Trust are over-staffed
# ##   by an amount less than the number of staff in post. For example, 5 staff
# ##   in post when there should only be 3.
# ## - A vacancy rate between 0% and 100% means someone is in post and there are
# ##   vacancies to fill.
# ## - An NA vacancy rate means either:
# ##    1. no staff in post and no vacancies. This is appropriately Not Applicable.
# ##    2. all staff in post were beyond what was expected. I set this to 0% vacancy.
# ##
# ## 
# # Convert the vacancy-data list into a vacancy-data dataframe.
if( !exists("df_vacancy" ) )
{
    for ( i_element in 1:length( ls_vacancy ) )
    {
      # Extract the AHP role.
      ahp_role <- unique( ls_vacancy[[ i_element ]]$`Care setting` )
      
      # Extract the columns of interest.
      new_cols <-
        ls_vacancy[[ i_element ]] %>%
        # ## Re-calculate the vacancy rate because it is a mess. See notes at
        # ## the start of this section of script.
        dplyr::mutate(
          vacancy_rate = 
            dplyr::case_when(
              # No staff in post and no vacancies. This is appropriately Not Applicable.
              ( staff_count == 0 ) & ( vacancy_count == 0 ) ~ NA
              # All staff in post were beyond what was expected. I set this to 0% vacancy.
              ,( staff_count != 0 ) & ( -staff_count == vacancy_count ) ~ 0
              # Someone is in post and the records say the Trust is over-staffed by an
              # amount greater than the number of staff in post. I set this to 0% vacancy.
              ,( staff_count > 0 ) & ( abs( vacancy_count ) > staff_count ) ~ 0
              # No staff are in post and there are vacancies to fill.
              ,( staff_count == 0 ) & ( abs( vacancy_count ) > 0 ) ~ 1
              ,.default = vacancy_count / ( vacancy_count + staff_count )
            )
        ) %>%
        # No staff are in post yet the records say the Trust is over-staffed. These
        # vacancy rates should be excluded because they indicate an administrative
        # error.
        dplyr::filter(
          !( ( staff_count == 0 ) & ( vacancy_count < 0 ) )
        ) %>%
        # ## Calculate the arithmetic mean vacancy rate over the preceding 12 months.
        dplyr::group_by( `Trust code` ) %>%
        dplyr::arrange( `Trust code`, vacancy_year, vacancy_month ) %>%
        dplyr::mutate(
          past_year_mean_vacancy_rate =
            slider::slide_dbl( vacancy_rate, mean, .before = 1, .after = 0 )
        ) %>%
        dplyr::ungroup() %>%
        # ## I filter for the third month so that it aligns with the stability-index
        # ## values that are all for the year ending March. It is important note that
        # ## that these vacancy statistics therefore refer to the most-recent month
        # ## rather than summarising the previous year up to that month.
        dplyr::filter( vacancy_month == 3 ) %>% 
        dplyr::select( -c( `Trust name`, vacancy_month ) )
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
    # # Rename the professions to match the churn data frames.
    df_vacancy <-
      df_vacancy %>%
      dplyr::mutate(
        `Care setting` = 
          dplyr::case_when(
            `Care setting` == "Occupational therapy" ~ "Occupational Therapy"
            ,`Care setting` == "Radiography (Diagnostic)" ~ "Radiography (diagnostic)"
            ,`Care setting` == "Radiography (Therapeutic)" ~ "Radiography (therapeutic)"
            ,`Care setting` == "Operating department practitioners" ~ "Operating Department Practitioners"
            ,`Care setting` == "Speech and Language Therapy" ~ "Speech & Language Therapy"
            ,`Care setting` == "Podiatry" ~ "Chiropody / Podiatry"
            ,.default = `Care setting`
          )
      )
    # # Save.
    saveRDS( df_vacancy, "Processed datasets/Paper 1/df_vacancy.RDS" )
  }


# # Patient satisfaction.
# # Select columns of interest.
if( !"ps_survey_year" %in% colnames( df_patientSatisfaction ) )
{
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
  # # Append historic data.
  df_patientSatisfaction <-
    df_patientSatisfaction_historic %>%
    dplyr::select(
      trust_code, ps_survey_year = survey_year
      ,ps_q18_mean = q18_mean_h, ps_q21_mean = q21_mean_h
      ,ps_q23_mean = q23_mean_h, ps_q30_mean = q30_mean_h, ps_q48_mean = q48_mean_h
    ) %>%
    dplyr::bind_rows( df_patientSatisfaction )
  rm( df_patientSatisfaction_historic )

  # Save.
  saveRDS( df_patientSatisfaction, "Processed datasets/Paper 1/df_patientSatisfaction.RDS" )
}

# Staff survey.
# We might choose to compare the staff survey data between the 'main' and 'bank'
# datasets. But, for now, I only process the 'main' dataset. Take note that the
# technical specification for the survey says "Any comparisons between results
# for bank only and substantive staff should be made with caution due to
# differences in the survey methodology/questions asked and differences in the
# profile of bank workers and staff with a substantive contract. Please see the
# NSSB Technical Guide for further information about the version of the survey
# for bank only workers."
if( !"ss_year" %in% colnames( df_staff_survey_main ) )
{
  df_staff_survey_main <-
    df_staff_survey_main %>%
    # Match `job_role` column to the `Care setting` options found in the main
    # churn data.
    dplyr::left_join(
      df_jobroleToCaresetting
      ,by = join_by( job_role )
    ) %>%
    # Re-code the year.
    dplyr::rename( ss_year = year_date ) %>%
    dplyr::mutate(        
      ss_year = as.integer( stringr::str_sub( ss_year, start = -4L ) )
    ) %>%
    tidyr::drop_na( ss_year, `Care setting` ) %>%
    # Select only the columns of interest.
    dplyr::select(
      c(
        ss_year
        ,`Care setting`
        ,org_id
        ,org_name
        ,job_role
        ,area_of_work
        ,q2a
        ,q3i
        ,q4d
        ,q4c
        ,q5a
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
        .cols = starts_with( "q" )
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
        ,.names = '{.col}_ResponseScore'
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
        ,.names = '{.col}_ResponseScore'
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
        ,.names = '{.col}_ResponseScore'
      )
    ) %>%
    # Convert the Likert scale columns to factor data type. Factor labels are from
    # the survey guide:
    # https://www.nhsstaffsurveys.com/static/958853e094733756687a78aa3d8cb36c/NSS2025-Questionnaire.zip
    dplyr::mutate(
      across(
        starts_with( "q" ) & !ends_with( "Score" )
        ,as.factor
      )
    ) %>%
    dplyr::mutate(
      across(
        c( q2a, q5a )
        ,~forcats::fct_recode(
          .x
          ,"Never" = "1"
          ,"Rarely"= "2"
          ,"Sometimes" = "3"
          ,"Often" = "4"
          ,"Always" = "5"
        )
      )
    ) %>%
    dplyr::mutate(
      across(
        c( q3i, starts_with( "q9" ), starts_with( "q2" ), -q2a, -ends_with( "Score" ) )
        ,~forcats::fct_recode(
          .x
          ,"Strongly disagree" = "1"
          ,"Disagree"= "2"
          ,"Neither agree nor disagree" = "3"
          ,"Agree" = "4"
          ,"Strongly agree" = "5"
        )
      )
    ) %>%
    dplyr::mutate(
      across(
        starts_with( "q4" ) & !ends_with( "Score" )
        ,~forcats::fct_recode(
          .x
          ,"Very dissatisfied" = "1"
          ,"Dissatisfied"= "2"
          ,"Neither satisfied nor dissatisfied" = "3"
          ,"Satisfied" = "4"
          ,"Very satisfied" = "5"
        )
      )
    ) %>%
    dplyr::mutate(
      q11c =
        forcats::fct_recode(
          q11c
          ,"Yes" = "1"
          ,"No"= "2"
        )
    ) %>%
    dplyr::rename_with(
      .cols = starts_with( "q" ) & !ends_with( "Score" )
      ,.f = ~paste0( .x, "_LikertScore")
    ) %>%
    # Rename columns to match turnover data.
    dplyr::rename( `Org code` = org_id ) %>%
    # Rename question columns.
    dplyr::rename_with(
      .fn = ~ sub( pattern = "q", replacement = "ss_q", .x )
      ,.cols = starts_with( 'q' )
    ) %>%
    # Create a dichotomous version of Likert questions, where appropriate.
    dplyr::mutate(
      across(
        contains( "Likert" )
        ,function(x){
          dplyr::case_when(
            x == "Strongly disagree" ~ "Disagree"
            ,x == "Disagree"~ "Disagree"
            ,x == "Neither agree nor disagree" ~ NA
            ,x == "Agree" ~ "Agree"
            ,x == "Strongly agree" ~ "Agree"
            ,x == "Very dissatisfied" ~ "Dissatisfied"
            ,x == "Dissatisfied" ~ "Dissatisfied"
            ,x == "Neither satisfied nor dissatisfied" ~ NA
            ,x == "Satisfied" ~ "Satisfied"
            ,x == "Very satisfied" ~ "Satisfied"
            ,x == "No" ~ "No"
            ,x == "Yes" ~ "Yes"
            ,.default = NULL
          )
        }
        ,.names = '{.col}_binary'
      )
    ) %>%
    dplyr::select( -( ( contains( "2a" ) | contains( "q5" ) ) & contains( "binary" ) ) ) %>%
    # Rename some professions to be consistent with other datasets.
    dplyr::mutate(
      `Care setting` = dplyr::if_else(
        `Care setting` == "Operating Theatres" 
        ,"Operating Department Practitioners"
        ,`Care setting`
        )
      ) %>%
    # ~ This cannot be done if we are maintaining the factor-level Likert values. ~ #
    # # Collapse rows for professions within an organisation.
    # dplyr::select( -c( area_of_work, job_role, org_name ) ) %>%
    # dplyr::reframe(
    #   across( contains( "Response"), ~median(as.integer(.x)) )
    #   ,.by = !contains( "ss_q")
    # ) %>%
    # Tidy up.
    dplyr::distinct() %>%
    dplyr::arrange( `Org code`, `Care setting` )

  # Save.
  saveRDS( df_staff_survey_main, "Processed datasets/Sept 2026 meeting/df_staff_survey_main.RDS" )
}


# ----

#####################
## Join data sets. ##
#####################
# ----

# Create function for joining.
fnc__joinToChurnData <-
  function( df )
{
  processed_df <-
    # Join with deprivation dataset.
    df %>%
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
    ) #%>%
    # # Join with patient satisfaction dataset.
    # dplyr::left_join(
    #   df_patientSatisfaction
    #   ,by = join_by(
    #     `Org code` == trust_code
    #     ,year_end == ps_survey_year
    #   )
    # )
   
  processed_df <-
    processed_df %>%
    dplyr::select( -`Trust Code` )
}


# Applying function.
if( !"IMD Score" %in% colnames( df_churn_within_NHS_Grade ) )
{
  df_churn_within_NHS_Grade <- fnc__joinToChurnData( df_churn_within_NHS_Grade )
  saveRDS( df_churn_within_NHS_Grade, "Processed datasets/Paper 1/df_churn_within_NHS_Grade.RDS" )
}
if( !"IMD Score" %in% colnames( df_churn_within_NHS_Gender ) )
{
  df_churn_within_NHS_Gender <- fnc__joinToChurnData( df_churn_within_NHS_Gender )
  saveRDS( df_churn_within_NHS_Gender, "Processed datasets/Paper 1/df_churn_within_NHS_Gender.RDS" )
}
if( !"IMD Score" %in% colnames( df_churn_within_NHS_AgeBand ) )
{
  df_churn_within_NHS_AgeBand <- fnc__joinToChurnData( df_churn_within_NHS_AgeBand )
  saveRDS( df_churn_within_NHS_AgeBand, "Processed datasets/Paper 1/df_churn_within_NHS_AgeBand.RDS" )
}
if( !"IMD Score" %in% colnames( df_churn_within_NHS_EthnicGroup ) )
{
  df_churn_within_NHS_EthnicGroup <- fnc__joinToChurnData( df_churn_within_NHS_EthnicGroup )
  saveRDS( df_churn_within_NHS_EthnicGroup, "Processed datasets/Paper 1/df_churn_within_NHS_EthnicGroup.RDS" )
}

# Handle special case of joining staff-survey data.
if( !exists( "df_churn_within_NHS_Grade_plus_staff_survey" ) )
{
  df_churn_within_NHS_Grade_plus_staff_survey <-
    df_churn_within_NHS_Grade %>%
    dplyr::filter(
      `AfC band` == "All AfC bands"
      ,`Care setting` != "All care settings"
      ,year_end == year_of_interest
      ) %>%
    dplyr::left_join(
      df_staff_survey_main %>%
        dplyr::select( -c( job_role, area_of_work ) )
      ,by = join_by(
        year_end == ss_year
        ,`Org code`
        ,`Care setting`
      )
      ,relationship = "one-to-many"
    ) %>%
    dplyr::select( -c( org_name ) )
  
    saveRDS(
      df_churn_within_NHS_Grade_plus_staff_survey
      ,"Processed datasets/Staff survey handover/df_churn_within_NHS_Grade_plus_staff_survey.RDS"
      )
}
if( !exists( "df_churn_within_NHS_AgeBand_plus_staff_survey" ) )
{
  df_churn_within_NHS_AgeBand_plus_staff_survey <-
    df_churn_within_NHS_AgeBand %>%
    dplyr::filter(
      `Age band` == "All Age bands"
      ,`Care setting` != "All care settings"
      ,year_end == year_of_interest
    ) %>%
    dplyr::left_join(
      df_staff_survey_main %>%
        dplyr::select( -c( job_role, area_of_work ) )
      ,by = join_by(
        year_end == ss_year
        ,`Org code`
        ,`Care setting`
      )
      ,relationship = "one-to-many"
    ) %>%
    dplyr::select( -c( org_name ) )
  # Save file.
  saveRDS(
    df_churn_within_NHS_AgeBand_plus_staff_survey
    ,"Processed datasets/Staff survey handover/df_churn_within_NHS_AgeBand_plus_staff_survey.RDS"
  )
} 
if( !exists( "df_churn_within_NHS_Gender_plus_staff_survey" ) )
{
  df_churn_within_NHS_Gender_plus_staff_survey <-
    df_churn_within_NHS_Gender %>%
    dplyr::filter(
      `Gender` == "All Genders"
      ,`Care setting` != "All care settings"
      ,year_end == year_of_interest
    ) %>%
    dplyr::left_join(
      df_staff_survey_main %>%
        dplyr::select( -c( job_role, area_of_work ) )
      ,by = join_by(
        year_end == ss_year
        ,`Org code`
        ,`Care setting`
      )
      ,relationship = "one-to-many"
    ) %>%
    dplyr::select( -c( org_name ) )
  # Save file.
  saveRDS(
    df_churn_within_NHS_Gender_plus_staff_survey
    ,"Processed datasets/Staff survey handover/df_churn_within_NHS_Gender_plus_staff_survey.RDS"
  )
}  
if( !exists( "df_churn_within_NHS_EthnicGroup_plus_staff_survey" ) )
{
  df_churn_within_NHS_EthnicGroup_plus_staff_survey <-
    df_churn_within_NHS_EthnicGroup %>%
    dplyr::filter(
      `Ethnic group` == "All ethnic groups"
      ,`Care setting` != "All care settings"
      ,year_end == year_of_interest
    ) %>%
    dplyr::left_join(
      df_staff_survey_main %>%
        dplyr::select( -c( job_role, area_of_work ) )
      ,by = join_by(
        year_end == ss_year
        ,`Org code`
        ,`Care setting`
      )
      ,relationship = "one-to-many"
    ) %>%
    dplyr::select( -c( org_name ) )
  # Save file.
  saveRDS(
    df_churn_within_NHS_EthnicGroup_plus_staff_survey
    ,"Processed datasets/Staff survey handover/df_churn_within_NHS_EthnicGroup_plus_staff_survey.RDS"
  )
}
# ----