# WrAP_make_datasets_for_Bayesian_modelling.r
#
# The purpose of this script is to make the datasets for the Bayesian modelling.
# A separate model and dataset are needed to estimate the association between 
# our candidate factors and stability index because the data were supplied to us
# pre-stratified by each factor separately.
# The factors and their datasets are:
# - Agenda for Change pay band, `payband_model_data`
# - Age band, `ageband_model_data`
# - Sex, `sex_model_data`
# - Ethnicity, `ethnicity_model_data`
# - Deprivation, `deprivation_model_data`
# - Rurality, `rurality_model_data`
# - Vacancy rate, `vacancy_model_data`
#
# The deprivation and rurality datasets are Trust-specific rather than
# profession-specific.
#

# Set save location of datasets.
dir_model_data <- "Processed datasets/Paper 1/"

# Agenda for Change pay band.
tryCatch(
  expr = {
    model_data_payband <- readRDS( paste0( dir_model_data, "model_data_payband.RDS" ) )
    
    message( "Model data for payband are already available in storage." )
  },
  warning = function(w){ 
    message( "Model data for payband are not available in storage so it is being created." )
    
    model_data_payband <-
      df_churn_within_NHS_Grade %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        !`AfC band` %in% c( 'All AfC bands', 'Non AfC band', 'Band 4' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,payband = `AfC band`
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) %>%
      dplyr::mutate(
        Profession = dplyr::if_else(
          Profession == 'Operating Theatres'
          ,'Operating Department Practitioners'
          ,Profession
        )
        ,payband_binary =
          dplyr::if_else(
            payband == "Band 5"
            ,payband
            ,">Band 5"
          )
        ,payband_binary = 
          forcats::fct_relevel( payband_binary, ">Band 5", after = Inf )
      )
    model_data_payband <-
      model_data_payband %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_payband
      )
    saveRDS( model_data_payband, paste0( dir_model_data, "model_data_payband.RDS" ) )
    
    message( "DONE. Model data for payband are now available in storage." )
  }
)

# Age band.
tryCatch(
  expr = {
    model_data_ageband <- readRDS( paste0( dir_model_data, "model_data_ageband.RDS" ) )
    
    message( "Model data for age band are already available in storage." )
  },
  warning = function(w){ 
    message( "Model data for age band are not available in storage so it is being created." )
    model_data_ageband <-
      df_churn_within_NHS_AgeBand %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        !`Age band` %in% c( 'All age bands' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,ageband = `Age band`
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) %>%
      dplyr::mutate(
        Profession = dplyr::if_else(
          Profession == 'Operating Theatres'
          ,'Operating Department Practitioners'
          ,Profession
        )
      )
    model_data_ageband <-
      model_data_ageband %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_ageband
      )
    saveRDS( model_data_ageband, paste0( dir_model_data, "model_data_ageband.RDS" ) )
    
    message( "DONE. Model data for age band are now available in storage." )
  }
)

# Sex.
tryCatch(
  expr = {
    model_data_sex <- readRDS( paste0( dir_model_data, "model_data_sex.RDS" ) )
    
    message( "Model data for sex are already available in storage." )
  },
  warning = function(w){
    message( "Model data for sex are not available in storage so it is being created." )
    model_data_sex <-
      df_churn_within_NHS_Gender %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        !Gender %in% c( 'All gender' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,sex = Gender
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) %>%
      dplyr::mutate(
        Profession = dplyr::if_else(
          Profession == 'Operating Theatres'
          ,'Operating Department Practitioners'
          ,Profession
        )
      )
    model_data_sex <-
      model_data_sex %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_sex
      )
    saveRDS( model_data_sex, paste0( dir_model_data, "model_data_sex.RDS" ) )
    
    message( "DONE. Model data for sex are now available in storage." )
    }
)

# Ethnicity.
tryCatch(
  expr = {
    model_data_ethnicity <- readRDS( paste0( dir_model_data, "model_data_ethnicity.RDS" ) )
    
    message( "Model data for ethnicity are already available in storage." )
  },
  warning = function(w){
    message( "Model data for ethnicity are not available in storage so it is being created." )
    
    model_data_ethnicity <-
      df_churn_within_NHS_EthnicGroup %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        !`Ethnic group` %in% c( 'All ethnic groups' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,ethnicity = `Ethnic group`
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) %>%
      dplyr::mutate(
        Profession = dplyr::if_else(
          Profession == 'Operating Theatres'
          ,'Operating Department Practitioners'
          ,Profession
        )
        ,ethnicity_binary =
          dplyr::if_else(
            ethnicity == "White"
            ,ethnicity
            ,"Non-white"
          )
        ,ethnicity_binary = 
          forcats::fct_relevel( ethnicity_binary, "Non-white", after = Inf )
      )
    model_data_ethnicity <-
      model_data_ethnicity %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_ethnicity
      )
    saveRDS( model_data_ethnicity, paste0( dir_model_data, "model_data_ethnicity.RDS" ) )
    
    message( "DONE. Model data for ethnicity are now available in storage." )
    }
)


# Vacancy rates.
tryCatch(
  expr = {
    model_data_vacancy <- readRDS( paste0( dir_model_data, "model_data_vacancy.RDS" ) )
    
    message( "Model data for vacancies are already available in storage." )
  },
  warning = function(w){
    message( "Model data for vacancies are not available in storage so it is being created." )
    
    model_data_vacancy <-
      df_churn_within_NHS_Grade %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        `AfC band` %in% c( 'All AfC bands' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>% 
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,vacancy = past_year_mean_vacancy_rate
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) %>%
      dplyr::mutate(
        Profession = dplyr::if_else(
          Profession == 'Operating Theatres'
          ,'Operating Department Practitioners'
          ,Profession
        )
      )
    model_data_vacancy <-
      model_data_vacancy %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_vacancy
      )
    saveRDS( model_data_vacancy, paste0( dir_model_data, "model_data_vacancy.RDS" ) )
    
    message( "DONE. Model data for vacancies are now available in storage." )
    }
)

# Deprivation.
tryCatch(
  expr = {
    model_data_deprivation <- readRDS( paste0( dir_model_data, "model_data_deprivation.RDS" ) )
    
    message( "Model data for deprivation are already available in storage." )
  },
  warning = function(w){
    message( "Model data for deprivation are not available in storage so it is being created." )
    
    model_data_deprivation <-
      df_churn_within_NHS_Grade %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        `AfC band` %in% c( 'All AfC bands' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,deprivation = `IMD Score`
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      )
    model_data_deprivation <-
      model_data_deprivation %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_deprivation
      )
    saveRDS( model_data_deprivation, paste0( dir_model_data, "model_data_deprivation.RDS" ) )
    
    message( "DONE. Model data for deprivation are now available in storage." )
    }
)

# Rurality.
tryCatch(
  expr = {
    model_data_rurality <- readRDS( paste0( dir_model_data, "model_data_rurality.RDS" ) )
    
    message( "Model data for rurality are already available in storage." )
  },
  warning = function(w){
    message( "Model data for rurality are not available in storage so it is being created." )
    
    
    model_data_rurality <-
      df_churn_within_NHS_Grade %>% 
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        `AfC band` %in% c( 'All AfC bands' )
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
        # Select year of interest
        ,year_end %in% year_of_interest
        # Select professions of interest
        ,!`Care setting` %in% c( 'All care settings' )
      ) %>%
      dplyr::distinct(
        orgcode = `Org code`
        ,Profession = `Care setting`
        ,rurality = `RUC21 settlement class`
        ,stability_index = `Stability index`
        ,n_staff_at_start = `Denominator at start of period`
      ) 
    model_data_rurality <-
      model_data_rurality %>%
      dplyr::reframe(
        .by = orgcode
        ,org_n_contribution = sum( n_staff_at_start )
      ) %>%
      dplyr::mutate(
        model_weighting =
          org_n_contribution / sum( org_n_contribution )
      ) %>%
      dplyr::right_join(
        by = join_by( orgcode )
        ,model_data_rurality
      )
    saveRDS( model_data_rurality, paste0( dir_model_data, "model_data_rurality.RDS" ) )
    
    message( "DONE. Model data for rurality are now available in storage." )
    }
)
