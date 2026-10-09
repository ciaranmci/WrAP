# WrAP_manuscript_figures.R
# 
# The purpose of this script is to produce the figures for a manuscript
# that describes the dataset.
# 

#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
if( !"cmdstanr" %in% installed.packages() )
{
  install.packages(
    "cmdstanr"
    ,repos = c('https://stan-dev.r-universe.dev'
               ,getOption("repos")
    )
  )
  cmdstanr::install_cmdstan()
  }
pacman::p_load(
  brms
  ,cmdstanr
  ,coin
  ,curl
  ,haven
  ,sf
  ,tidyverse
)

# ----

#################
## Requisites. ##
#################
# ----

# Set the year of interest.
year_of_interest <- 2025

# Set storage locations.
dir.create( "./Tests/Paper 1", recursive = TRUE )
dir.create( "./Tables/Paper 1", recursive = TRUE )
dir.create( "./Plots/Paper 1", recursive = TRUE )
dir.create( "./Models/Paper 1", recursive = TRUE )
dir.create( "./Processed datasets/Paper 1", recursive = TRUE )

# Set the list of questions of interest from the staff survey.
q_lookup <-
  data.frame(
    q_num = 
      c( "q2a", "q3i", "q4c", "q4d", "q5a", "q9a", "q9i", "q11c", "q21"
         ,"q24d", "q25d", "q25f"#, "q26a"
         , "q26c" )
    ,q_word =
      c(
        "I look forward to going to work."
        ,"There are enough staff at this organisation for me to do my job properly."
        ,"My level of pay."
        ,"The opportunities for flexible working patterns."
        ,"I have unrealistic time pressures."
        ,"My immediate manager encourages me at work."
        ,"My immediate manager takes effective action to help me with any problems I face."
        ,"During the last 12 months have you felt unwell as a result of work related stress?"
        ,"I think that my organisation respects individual differences."
        ,"I feel supported to develop my potential."
        ,"If a friend or relative needed treatment I would be happy with the standard of care provided by this organisation."
        ,"If I spoke up about something that concerned me I am confident my organisation would address my concern."
        #,"I often think about leaving this organisation"
        ,"As soon as I can find another job, I will leave this organisation."
      )
    ,q_positive_statement =
      c(
        "Job satisfaction"
        ,"Sufficient staff"
        ,"Level of pay"
        ,"Flexible work opportunties"
        ,"Unrealistic time pressures"
        ,"Manager encouragement"
        ,"Manager support (problems)"
        ,"Work related stress (last 12 months)"
        ,"Respect for individual differences"
        ,"Support for personal development"
        ,"Quality of care provided"
        ,"Actions taken when concerns raised"
        #,"Job dissatisfaction"
        ,"Intention to leave"
      )
  ) %>%
  dplyr::mutate(
    combined = paste0( q_num, " - \"", q_word, "\"")
  )
# Separate outcome questions from the others.
q_lookup_outcomes <-
  q_lookup %>%
  dplyr::filter( q_num %in% c( "q26a", "q26c" ) ) %>%
  dplyr::pull( q_num )
q_lookup_other <-
  q_lookup %>%
  dplyr::filter( !q_num %in% q_lookup_outcomes ) %>%
  dplyr::pull( q_num )


# ----

################
## Load data. ##
################
# ----
source( "WrAP_Load data.R" )
# ----

###################
## Prepare data. ##
###################
# ----
source( "WrAP_Prepare data.R" )
# ----

##################################################
# Determine the largest professions, nationally. #
##################################################
# -----
professions_of_interest <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    `AfC band` %in% c( 'All AfC bands' )
    ,!`Care setting` %in% c( 'All care settings' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Select year of interest
    ,year_end %in% year_of_interest
  ) %>% 
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = `Care setting`
  ) %>%
  dplyr::arrange( -N ) %>%
  head( 6 ) %>%
  dplyr::arrange( `Care setting` ) %>%
  dplyr::pull( `Care setting` )
# -----

####################################################
# Make factor-specific datasets for the modelling. #
####################################################
# ----
source( "WrAP_make_datasets_for_Bayesian_modelling.r" )
# ----


# ~~~~~~~~~~~~
# ~~ Tables ~~ 
# ~~~~~~~~~~~~

########################################################
# Count of non-specialist acute NHS Trusts in dataset. #
########################################################
# n_trusts = 122.
# ----
# Arbitrarily choose one of the churn worksheets.
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::distinct( `Org code` ) %>%
  dplyr::reframe( n_trusts = n() )
# ----

#############################
# Count of AHPs in dataset. #
#############################
# n_ahps_2025 = 158,985
# ----
# Arbitrarily choose one of the churn worksheets.
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::reframe( .by = Period, n_ahps = sum( `Denominator at start of period` ) )
# ----

#####################################
# Summary statistics of headcounts. #
#####################################
# ----

# Arbitrarily choose one of the churn worksheets.
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::reframe(
    N = sum( `Denominator at end of period`, na.rm = TRUE )
    ,Min = min( `Denominator at end of period`, na.rm = TRUE )
    ,qtr1 = quantile( `Denominator at end of period`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Denominator at end of period`, na.rm = TRUE )
    ,qtr3 = quantile( `Denominator at end of period`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Denominator at end of period`, na.rm = TRUE )
    ,.by = c( year_end, `Care setting` )
    ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__headcounts.csv" )
# ----

###########################
# Headcounts per payband. #
###########################
# ----
N_profession_payband <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Select professions of interest
    ,`Care setting` %in% c( "All care settings", professions_of_interest )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>% 
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = c( year_end, `Care setting`, `AfC band` )
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `AfC band` )

  # Save to file.
  write.csv( N_profession_payband, "Tables/Paper 1/table__headcounts_per_payband.csv" )
# ----

###########################
# Headcounts per ageband. #
###########################
# ----
N_profession_ageband <-
  df_churn_within_NHS_AgeBand %>% 
  dplyr::filter(
    # Select professions of interest
    ,`Care setting` %in% c( "All care settings", professions_of_interest )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>% 
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = c( year_end, `Care setting`, `Age band` )
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `Age band` )

# Save to file.
write.csv( N_profession_ageband, "Tables/Paper 1/table__headcounts_per_ageband.csv" )
# ----

#######################
# Headcounts per sex. #
#######################
# ----
N_profession_sex <-
  df_churn_within_NHS_Gender %>% 
  dplyr::filter(
    # Select professions of interest
    ,`Care setting` %in% c( "All care settings", professions_of_interest )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>% 
  dplyr::rename( Sex = Gender ) %>%
  dplyr::mutate( Sex = factor( Sex, levels = c( "Female", "Male" ) ) ) %>%
  tidyr::drop_na() %>%
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = c( year_end, `Care setting`, Sex )
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, Sex )

# Save to file.
write.csv( N_profession_sex, "Tables/Paper 1/table__headcounts_per_sex.csv" )
# ----

#############################
# Headcounts per ethnicity. #
#############################
# ----
N_profession_ethnicity <-
  df_churn_within_NHS_EthnicGroup %>% 
  dplyr::filter(
    !`Ethnic group` %in% c( 'All ethnic groups' )
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>%
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = c( year_end, `Care setting`, `Ethnic group` )
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `Ethnic group` )
  
  # Save to file.
  write.csv( N_profession_ethnicity, "Tables/Paper 1/table__headcounts_per_ethnicity.csv" )
  
# ----
  
############################
# Headcounts per rurality. #
############################
# ----
N_profession_rurality <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    !`AfC band` %in% c( 'All pay bands' )
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>%
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = c( year_end, `Care setting`, `RUC21 settlement class` )
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `RUC21 settlement class` )

# Save to file.
write.csv( N_profession_rurality, "Tables/Paper 1/table__headcounts_per_rurality.csv" )

# ----

#################################################
## Count of trusts with SI = 0% and SI = 100%. ##
#################################################
# ----
df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  ) %>%
  filter( `Stability index` %in% c(0,1) ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Stability index` )
  ) %>%
  dplyr::arrange( year ) %>%
  write.csv( file = "Tables/count_of_0_or_1_SI_per_year_.csv")

df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  ) %>%
  filter( `Stability index` %in% c(0,1) ) %>%
  dplyr::reframe(
    n = n()
    ,.by = c( year, `Care setting`, `Stability index` )
  ) %>% 
  dplyr::arrange( year,  `Care setting` ) %>%
  write.csv( file = "Tables/count_of_0_or_1_SI_per_year_per_profession.csv")

df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
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
  dplyr::arrange( year,  `Care setting`, SI_val_category ) %>%
  write.csv( file = "Tables/count_of_0_1_or_in_betweeen_SI_per_year_per_profession.csv")
# ----

##################################
# Stability index per ethnicity. #
##################################
# ----
df_churn_within_NHS_EthnicGroup %>% 
  dplyr::filter(
    !`Ethnic group` %in% c( 'All ethnic groups' )
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>% 
  dplyr::reframe(
    .by = c( year_end, `Care setting`, `Ethnic group` )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `Ethnic group` ) %>% 
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_per_ethnicity.csv" )
# ----

#################################
# Stability index per rurality. #
#################################
# ----
df_churn_within_NHS_EthnicGroup %>% 
  dplyr::filter(
    !`Ethnic group` %in% c( 'All ethnic groups' )
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  )  %>% 
  dplyr::mutate(
    Profession = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>% 
  dplyr::reframe(
    .by = c( year_end, `Care setting`, `RUC21 settlement class` )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
  ) %>%
  dplyr::arrange( -year_end, `Care setting`, `RUC21 settlement class` ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_per_ruraity.csv" )
# ----

#############################
# Stability index, payband. #
#############################
# ----

# Make dataset. This will be reused.
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
    ,`Care setting` == 'All care settings'
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  ) %>%
  dplyr::select( `Org code`, `Care setting`, `AfC band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  ) %>% 
  dplyr::reframe(
    .by =  c( Profession, `AfC band` )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
  ) %>% 
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_per_payband.csv" )
# ----

################################################
# Stability index, per profession and payband. #
################################################
# ----

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
    ,`Care setting` %in% professions_of_interest
    # Remove uncalculable stability-index values
    ,!is.na( `Stability index` )
  ) %>%
  dplyr::select( `Org code`, `Care setting`, `AfC band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  ) %>%
  dplyr::reframe(
    .by =  c( Profession, `AfC band` )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Mean = mean( `Stability index`, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
  ) %>%
  dplyr::arrange( Profession, `AfC band` ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_per_top6_profession_per_payband.csv" )
# ----

#######################################################
# Joiners:Leavers ratio, per profession and pay band. #
#######################################################
# ----
# Make dataset. This will be used again later.
data_JLrateRatio <- 
  # Arbitrarily choose one of the churn worksheets.
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
    ,`Care setting` != 'All care settings'
  ) %>% 
  # Calculate the rate ratio.
  dplyr::mutate(
    JL_rate_ratio = joiner_rate / leaver_rate
    ,JL_rate_ratio = 
      dplyr::if_else(
        is.infinite( JL_rate_ratio ) | is.nan( JL_rate_ratio )
        ,NA
        ,JL_rate_ratio
        )
    )

# Make table.
data_JLrateRatio %>% 
  # Select columns of interest.
  dplyr::select(
    `Organisation name`
    ,`Org code`
    ,`Care setting`
    ,`AfC band`
    ,JL_rate_ratio
    ,joiner_rate
    ,leaver_rate
  ) %>%
  # Get national summary statistics.
  dplyr::reframe(
    .by = c( `Care setting`, `AfC band` )
    ,Min = min( JL_rate_ratio, na.rm = TRUE )
    ,qtr1 = quantile( JL_rate_ratio, probs = 0.25, na.rm = TRUE )
    ,Median = median( JL_rate_ratio, na.rm = TRUE )
    ,Mean = mean( JL_rate_ratio, na.rm = TRUE )
    ,qtr3 = quantile( JL_rate_ratio, probs = 0.75, na.rm = TRUE )
    ,Max = max( JL_rate_ratio, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
    ) %>%
  dplyr::arrange( `Care setting`, `AfC band` ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__joinerLeaver_rate_ratio.csv" )
  
# ----

#################################
# Vacancy rates per profession. #
#################################
# ----
# Make dataset. This will be used again, later.
data_vacancy_variables <-
  model_data_vacancy %>%
  # Join in the vacancy data.
  dplyr::left_join(
    df_vacancy %>%
      dplyr::filter( vacancy_year == year_of_interest )
    ,by =
      dplyr::join_by(
        orgcode == `Trust code`
        ,Profession == `Care setting` 
      )
  )

# Make and save the summary table.
data_vacancy_variables %>%
  dplyr::reframe(
    .by = Profession
    ,Min = min( vacancy, na.rm = TRUE )
    ,qtr1 = quantile( vacancy, probs = 0.25, na.rm = TRUE )
    ,Median = median( vacancy, na.rm = TRUE )
    ,qtr3 = quantile( vacancy, probs = 0.75, na.rm = TRUE )
    ,Max = max( vacancy, na.rm = TRUE )
    ,IQR = qtr3 - qtr1
  ) %>%
  dplyr::arrange( Profession ) %>% 
  # Save to file.
  write.csv( "Tables/Paper 1/table__vacancy_per_profession.csv" )
# ----

##########################
# Extreme vacancy rates. #
##########################
# ----

 

# Make table.
dplyr::bind_rows(
  data_vacancy_variables %>%
    dplyr::arrange( vacancy_rate ) %>%
    dplyr::slice_head( n = 5 ) %>%
    dplyr::mutate( `High or low` = "Lowest" )
  
  ,data_vacancy_variables %>%
    dplyr::arrange( -vacancy_rate ) %>%
    dplyr::slice_head( n = 5 ) %>%
    dplyr::mutate( `High or low` = "Highest" )
) %>%
  dplyr::left_join(
    df_Trust_size_2023_03
    ,by = dplyr::join_by( orgcode == `Trust code 2023 03` )
    ) %>%
  dplyr::select(
    `High or low`, orgcode, `Trust size 2023 03`
    ,`Profession`, vacancy_rate, vacancy_count
  ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__extreme_vacancy_rates.csv" )
# ----



# ~~~~~~~~~~~~~~~
# ~~ Modelling ~~
# ~~~~~~~~~~~~~~~

#####################
## Fit the models. ##
#####################
# ----
source( "WrAP_fit_models.r" )
# ----

# Check model's convergence and re-fit if necessary
# ...manual...

###########################################################
## Compute the contrasts of the posterior distributions. ##
###########################################################
# ----
source( "compute_contrasts.r" )
# ----



# ~~~~~~~~~~~
# ~~ Tests ~~
# ~~~~~~~~~~~

######################################################
# Test of changes in stability over the three years. #
######################################################
# ----
# The purpose of this section of script is to assess whether the stability-index
# values are similar year-on-year.
# The Friedman rank sum test assesses whether Trusts' year-on-year stability-
# index values have no consistent ordered over time.
# The pairwise Wilcoxon test assesses whether each pairwise set of the differences
# in stability-index values are symmetrical around 0.

# Create dataset.
df <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == 'All AfC bands'
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select professions of interest
    ,`Care setting` == 'All care settings'
  ) %>%
  dplyr::select( year_end, `Org code`, `Stability index` )
# Friendman test.
df %>%
  tidyr::pivot_wider(
    id_cols = `Org code`
    ,values_from = `Stability index`
    ,names_from = year_end
  ) %>%
  tidyr::drop_na() %>%
  dplyr::select( -`Org code`) %>%
  as.matrix() %>%
  stats::friedman.test() %>% 
  # Save to file. ################### NEED TO FIND WAY TO EXTRACT THE INFO
  write.csv( "Tests/Paper 1/test__friedman_test_result.csv" )
# Post-hoc test.
pairwise.wilcox.test(
  df$`Stability index`
  ,df$year_end
  ,p.adj = "bonf"
) %>% 
  # Save to file.# Save to file. ################### NEED TO FIND WAY TO EXTRACT THE INFO
  write.csv( "Tests/Paper 1/test__Wilcoxon_pairwise_test_results.csv" )

# ----



# ~~~~~~~~~~~
# ~~ Plots ~~ 
# ~~~~~~~~~~~

###########################
## Reused plot elements. ##
###########################
# ----

# Background data points and the box plot.
p_grey_pts_and_boxplot <-
  list(
    geom_point(
      position = position_jitter( height = 0.1 )
      ,alpha = 0.2
      ,colour = "grey"
    )
    ,geom_boxplot(
      fill = "grey"
      ,alpha = 0.3
      ,width = 0.2
      ,outlier.colour = "grey"
    )
  )

# Theme preferences.
p_theme_preferences <-
  list(
    theme_minimal()
    ,theme(
      axis.title.y = element_blank()
      ,axis.text.x = element_text( size = 10 )
      ,axis.text.y = element_text( size = 14, face = "bold" )
      ,plot.title = element_text( size = 20 )
      ,plot.caption = element_text( hjust = 0, face = "italic" )
      ,strip.text.x = element_text( size = 15 )
      ,legend.position = "bottom"
    )
  )

# Scale and facet options.
p_scale_and_facet_options <-
  list(
    scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1)
      ,labels = c( "0", "0.25", "0.5", "0.75", "1" )
      )
    ,facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) )
  )

# Statistical significance.
func__make_significance_plot_layer <- 
  function( signif_plot_data = NULL )
  {
    p_signif <-
      list(
        # Significance long bar.
        geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_median_diff )
          ,aes( x = x_median, xend = x_median, y = y, yend = yend )
          ,inherit.aes = FALSE
        )
        # Significance bar leg 1.
        ,geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_median_diff )
          ,aes( x = x_median, xend = xend_median, y = y, yend = y )
          ,inherit.aes = FALSE
        )
        # Significance bar leg 2.
        ,geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_median_diff )
          ,aes( x = x_median, xend = xend_median, y = yend, yend = yend )
          ,inherit.aes = FALSE
        )
        # Significance symbol.
        ,geom_text(
          data = signif_plot_data %>% dplyr::filter( is_median_diff )
          ,aes( x = x_median + 0.03, y = ( y + yend ) / 2, label = signif_median )
          ,inherit.aes = FALSE
          ,hjust = 0
        )
        # Significance long bar.
        ,geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_iqr_diff )
          ,aes( x = x_iqr, xend = x_iqr, y = y, yend = yend )
          ,inherit.aes = FALSE
        )
        # Significance bar leg 1.
        ,geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_iqr_diff )
          ,aes( x = x_iqr, xend = xend_iqr, y = y, yend = y )
          ,inherit.aes = FALSE
        )
        # Significance bar leg 2.
        ,geom_segment(
          data = signif_plot_data %>% dplyr::filter( is_iqr_diff )
          ,aes( x = x_iqr, xend = xend_iqr, y = yend, yend = yend )
          ,inherit.aes = FALSE
        )
        # Significance symbol.
        ,geom_text(
          data = signif_plot_data %>% dplyr::filter( is_iqr_diff )
          ,aes( x = x_iqr + 0.03, y = ( y + yend ) / 2, label = signif_iqr )
          ,inherit.aes = FALSE
          ,hjust = 0
        )
      )
    
    return( p_signif )
  }


# Load the Trust-catchment shape file.
## https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/folder/170908955104
tc_shp <- sf::st_read( "../../Data/FPTP_AllAd22_Full.shp" )
# Append the names of the Trusts because the SHP file does not contain any
# meta-data.
## I need to colour one catchment area at a time and compare it with the
## PowerBI dashboard to figure out which Trust each area refers to because
## these areas are not labelled.
## The dashboard is at https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9
## Note that this dashboard was made in 2022. A more-up-to-date dashboard is
## available at https://app.powerbi.com/view?r=eyJrIjoiYzg2ODkzODgtMDA3OS00MGVhLTgyNWQtZjg1ZmQ2YWNlY2ZhIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9
## but the interface is not as helpful for what I'm trying to do.
tc_shp <-
  dplyr::bind_cols(
    tc_shp 
    ,orgname =
      c(
        'Manchester University NHS Foundation Trust'
        ,'South Tyneside And Sunderland NHS Foundation Trust'
        ,'University Hospitals Dorset NHS Foundation Trust'
        ,'Isle of Wight NHS Trust'
        ,'Barts Health NHS Trust'
        ,'London North West University Healthcare NHS Trust'
        ,'Royal Surrey County Hospital NHS Foundation Trust'
        # # Yeovil merged with Somerset NHS FT in April 2023.
        ,'Yeovil District Hospital NHS Foundation Trust' 
        ,'University Hospitals Bristol and Weston NHS Foundation Trust'
        ,'Torbay and South Devon NHS Foundation Trust'
        ,'Bradford Teaching Hospitals NHS Foundation Trust'
        ,'Mid and South Essex NHS Foundation Trust'
        ,'Royal Free London NHS Foundation Trust'
        ,'North Middlesex University Hospital NHS Trust'
        ,'Hillingdon Hospitals NHS Foundation Trust'
        ,'Kingston and Richmond NHS Foundation Trust'
        ,'Dorset County Hospital NHS Foundation Trust'
        ,'Walsall Healthcare NHS Trust'
        ,'Wirral University Teaching Hospital NHS Foundation Trust'
        ,'Mersey and West Lancashire Teaching Hospitals NHS Trust'
        ,'Mid Cheshire Hospitals NHS Foundation Trust'
        # # Northern Devon Healthcare existed in early 2022 but not after April.
        # # In April 2022, it merged with Royal Devon University Healthcare NHS
        # # Foundation Trust.
        ,'Northern Devon Healthcare NHS Trust'
        ,'Bedfordshire Hospitals NHS Foundation Trust'
        ,'York and Scarborough Teaching Hospitals NHS Foundation Trust'
        ,'Harrogate and District NHS Foundation Trust'
        ,'Airedale NHS Foundation Trust'
        ,'Queen Elizabeth Hospital King\'s Lynn NHS Foundation Trust'
        ,'Royal United Hospitals Bath NHS Foundation Trust'
        ,'Milton Keynes University Hospital NHS Foundation Trust'
        ,'East Suffolk and North Essex NHS Foundation Trust'
        ,'Frimley Health NHS Foundation Trust'
        ,'Royal Cornwall Hospitals NHS Trust'
        ,'Liverpool University Hospitals NHS Foundation Trust'
        ,'Barking, Havering and Redbridge University Hospitals NHS Trust'
        ,'Barnsley Hospital NHS Foundation Trust'
        ,'Rotherham NHS Foundation Trust'
        ,'Chesterfield Royal Hospital NHS Foundation Trust'
        ,'North West Anglia NHS Foundation Trust'
        ,'James Paget University Hospitals NHS Foundation Trust'
        ,'West Suffolk NHS Foundation Trust'
        ,'Cambridge University Hospitals NHS Foundation Trust'
        ,'Somerset NHS Foundation Trust'
        ,'Royal Devon University Healthcare NHS Foundation Trust'
        ,'University Hospital Southampton NHS Foundation Trust'
        ,'Sheffield Teaching Hospitals NHS Foundation Trust'
        ,'Portsmouth Hospitals University NHS Trust'
        ,'Royal Berkshire NHS Foundation Trust'
        ,'Guy\'s and St Thomas\' NHS Foundation Trust'
        ,'Lewisham and Greenwich NHS Trust'
        ,'Croydon Health Services NHS Trust'
        ,'St George\'s University Hospitals NHS Foundation Trust'
        ,'South Warwickshire University NHS Foundation Trust'
        ,'University Hospitals of North Midlands NHS Trust'
        ,'Northern Lincolnshire and Goole NHS Foundation Trust'
        ,'East Cheshire NHS Trust'
        ,'Countess of Chester Hospital NHS Foundation Trust'
        ,'King\'s College Hospital NHS Foundation Trust'
        ,'Sherwood Forest Hospitals NHS Foundation Trust'
        ,'University Hospitals Plymouth NHS Trust'
        ,'University Hospitals Coventry and Warwickshire NHS Trust'
        ,'Whittington Health NHS Trust'
        ,'Royal Wolverhampton NHS Trust'
        ,'Wye Valley NHS Trust'
        ,'George Eliot Hospital NHS Trust'
        ,'Norfolk and Norwich University Hospitals NHS Foundation Trust'
        ,'Northern Care Alliance NHS Foundation Trust'
        ,'Bolton NHS Foundation Trust'
        ,'Tameside and Glossop Integrated Care NHS Foundation Trust'
        ,'Great Western Hospitals NHS Foundation Trust'
        ,'Hampshire Hospitals NHS Foundation Trust'
        ,'Dartford and Gravesham NHS Trust'
        ,'Dudley Group NHS Foundation Trust'
        ,'North Cumbria Integrated Care NHS Foundation Trust'
        ,'Kettering General Hospital NHS Foundation Trust'
        ,'Northampton General Hospital NHS Trust'
        ,'Salisbury NHS Foundation Trust'
        ,'Doncaster and Bassetlaw Teaching Hospitals NHS Foundation Trust'
        ,'Medway NHS Foundation Trust'
        ,'Chelsea and Westminster Hospital NHS Foundation Trust'
        ,'Princess Alexandra Hospital NHS Trust'
        ,'Homerton Healthcare NHS Foundation Trust'
        ,'Gateshead Health NHS Foundation Trust'
        ,'Leeds Teaching Hospitals NHS Trust'
        ,'Wrightington, Wigan and Leigh NHS Foundation Trust'
        ,'University Hospitals Birmingham NHS Foundation Trust'
        ,'University College London Hospitals NHS Foundation Trust'
        ,'Newcastle Upon Tyne Hospitals NHS Foundation Trust'
        ,'Gloucestershire Hospitals NHS Foundation Trust'
        ,'Northumbria Healthcare NHS Foundation Trust'
        ,'University Hospitals of Derby and Burton NHS Foundation Trust'
        ,'Oxford University Hospitals NHS Foundation Trust'
        ,'Ashford and St. Peter\'s Hospitals NHS Foundation Trust'
        ,'Surrey and Sussex Healthcare NHS Trust'
        ,'South Tees Hospitals NHS Foundation Trust'
        ,'University Hospitals of Morecambe Bay NHS Foundation Trust'
        ,'North Bristol NHS Trust'
        ,'Epsom and St Helier University Hospitals NHS Trust'
        ,'East Kent Hospitals University NHS Foundation Trust'
        ,'North Tees and Hartlepool NHS Foundation Trust'
        # # Southport and Ormskirk existed in in 2022 but not after 2023. On 1
        # # July 2023, the Trust merged with St Helens and Knowsley Teaching
        # # Hospitals NHS Trust to form Mersey and West Lancashire Teaching
        # # Hospitals NHS Trust.
        ,'Southport and Ormskirk Hospital NHS Trust' 
        ,'Hull University Teaching Hospitals NHS Trust'
        ,'United Lincolnshire Teaching Hospitals NHS Trust'
        ,'University Hospitals of Leicester NHS Trust'
        ,'Maidstone and Tunbridge Wells NHS Trust'
        ,'West Hertfordshire Teaching Hospitals NHS Trust'
        ,'East and North Hertfordshire NHS Trust'
        ,'Stockport NHS Foundation Trust'
        ,'Worcestershire Acute Hospitals NHS Trust'
        ,'Warrington and Halton Teaching Hospitals NHS Foundation Trust'
        ,'Calderdale and Huddersfield NHS Foundation Trust'
        ,'Nottingham University Hospitals NHS Trust'
        ,'East Sussex Healthcare NHS Trust'
        ,'Mid Yorkshire Teaching NHS Trust'
        ,'Sandwell and West Birmingham Hospitals NHS Trust'
        ,'Blackpool Teaching Hospitals NHS Foundation Trust'
        ,'Lancashire Teaching Hospitals NHS Foundation Trust'
        ,'County Durham and Darlington NHS Foundation Trust'
        ,'Buckinghamshire Healthcare NHS Trust'
        ,'East Lancashire Hospitals NHS Trust'
        ,'Shrewsbury and Telford Hospital NHS Trust'
        ,'Imperial College Healthcare NHS Trust'
        ,'University Hospitals Sussex NHS Foundation Trust'
      ) %>% tolower()
  )
# ----


#################################################################################
## Distribution of stability index showing requirement for ordered beta model. ##
#################################################################################
# ----

# ----

###########################################################
## Plot of stability index over the years. No breakdown. ##
###########################################################
# ----
# Make plot data.
plot_data <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::select( year, Profession = `Care setting`, stability_index = `Stability index` ) %>%
  dplyr::mutate( grp = "All professions" )

plot_data <-
  dplyr::bind_rows(
    plot_data %>%
      dplyr::filter(
        ( year == "March '24 to March '25" ) & ( Profession == "Prosthetics and Orthotics" )
        ) %>%
      dplyr::mutate( grp = "Prosthetics and Orthotics, 2025" )

    ,plot_data %>%
      dplyr::filter(
        ( year == "March '22 to March '23" ) & ( Profession == "Physiotherapy" )
        ) %>%
      dplyr::mutate( grp = "Physiotherapy, 2023" )
    
    ,plot_data %>%
      dplyr::filter(
        ( year == "March '24 to March '25" ) & ( Profession == "Chiropody / Podiatry" )
      ) %>%
      dplyr::mutate( grp = "Chiropody / Podiatry, 2023" )
  ) %>%
  dplyr::mutate(
    grp = factor(
      grp
      ,levels = c( "Physiotherapy, 2023", "Prosthetics and Orthotics, 2025"
                   , "Chiropody / Podiatry, 2023" )
      )
    )
      
p <-
  plot_data %>%
  ggplot() +
  geom_histogram( aes( x = stability_index ) ) +
  facet_wrap( ~grp, nrow = 3 ) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1)
    ,labels = c( "0", "0.25", "0.5", "0.75", "1" )
  ) +
  labs(
    x = "Stability index"
    ,y = "Count of Trusts"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by Agenda-for-Change (AfC) pay band."
    #     )
    #     ,55
    #   )
    # ,subtitle =
    #   paste0(
    #     "Median stability index shown as a coral dot."
    #     # ,"\nStars indicate statistically-significant difference in medians."
    #   )
    # ,caption =
      # stringr::str_wrap(
      #   paste0(
      #     "Physiotherapy could use beta regression to model most Trusts with stability-index values"
      #     ," between 0% and 100%, but would exclude Trusts that lost all their"
      #     ," staff (i.e. stability = 0%). Prosthetics and Orthotics could use"
      #     ," logistic regression to model most Trusts with 0% or 100% stability,"
      #     ," but would exclude Trusts with stability between 0% and 100%."
      #     ," Chiropody / Podiatry could use ordered beta regression to model all Trusts in"
      #     ," the full range of stability index, including the 'spikes' at 0% and"
      #     ," 100%, excluding no Trusts."
      # 
      #   )
    #     ,100
    #   )
    
  ) +
  theme_minimal() +
  theme(
    ,axis.text.x = element_text( size = 10 )
    ,axis.text.y = element_text( size = 14, face = "bold" )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
    ,legend.position = "bottom"
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
      "Plots/Paper 1/plot__different_distributions_of_si_.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

######################################################################
## Plot of stability index for the largest professions by pay band. ##
######################################################################
# A Tukey-style boxplot that shows stability-index values across pay bands,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_payband %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::mutate( payband = droplevels( .$payband ) )

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, payband )
    ,class_median = median( stability_index, na.rm = TRUE )
    ,class_min = min( stability_index, na.rm = TRUE )
    ,class_max = max( stability_index, na.rm = TRUE )
  ) %>%
  dplyr::left_join(
    N_profession_payband %>%
      dplyr::filter(
        year_end == year_of_interest
        ,`AfC band` %in% levels(plot_data$payband )
        ) %>%
      dplyr::mutate( `AfC band` = droplevels( .$`AfC band` ) )
    ,by = join_by( Profession == `Care setting`, payband == `AfC band` )
  )

# Data for statistical significance indicators.
signif_plot_data <- 
  posterior_summaries_payband %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::select( Profession, payband1 = payband, is_median_diff, is_iqr_diff ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate( payband2 = lead( payband1 ) ) %>%
  dplyr::ungroup() %>%
  tidyr::replace_na( list( payband2 = tail( levels( plot_data$payband ), 2)[1] ) ) %>%
  dplyr::relocate( payband2, .after = payband1 ) %>%
  dplyr::mutate(
    signif_median = dplyr::if_else( is_median_diff, "*", "" )
    ,signif_iqr = dplyr::if_else( is_iqr_diff, "†", "" )
    ,y = match( payband1, levels( plot_data$payband ) )
    ,yend = match( payband2, levels( plot_data$payband ) )
    ,x_median = 1.08
    ,xend_median = x_median - 0.05
    ,x_iqr = xend_median + 0.18
    ,xend_iqr = x_iqr - 0.05
  )
p_signif <- func__make_significance_plot_layer( signif_plot_data )

# Make the plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = payband
    ) ) +
  p_grey_pts_and_boxplot +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = payband, size = N )
    ,colour = "coral"
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = payband )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  p_signif +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by Agenda-for-Change (AfC) pay band."
    #     )
    #     ,55
    #   )
    # ,subtitle =
    #   paste0(
    #     "Median stability index shown as a coral dot."
    #     # ,"\nStars indicate statistically-significant difference in medians."
    #   )
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(  
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
    
  ) +
  p_theme_preferences

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_pay__topProfessions.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----  

######################################################################
## Plot of stability index for the largest professions by age band. ##
######################################################################
# A Tukey-style boxplot that shows stability-index values across age bands,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_ageband %>%
  dplyr::filter( Profession %in% professions_of_interest )

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, ageband )
    ,class_median = median( stability_index, na.rm = TRUE )
    ,class_min = min( stability_index, na.rm = TRUE )
    ,class_max = max( stability_index, na.rm = TRUE )
  ) %>%
  dplyr::left_join(
    N_profession_ageband %>%
      dplyr::filter(
        year_end == year_of_interest
        ,`Age band` %in% levels( plot_data$ageband )
      ) %>%
      dplyr::mutate( `Age band` = droplevels( .$`Age band` ) )
    ,by = join_by( Profession == `Care setting`, ageband == `Age band` )
  )

# Data for statistical significance indicators.
signif_plot_data <- 
  posterior_summaries_ageband %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::select( Profession, ageband1 = ageband, is_median_diff, is_iqr_diff ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate( ageband2 = lead( ageband1 ) ) %>%
  dplyr::ungroup() %>%
  tidyr::replace_na( list( ageband2 = tail( levels( plot_data$ageband ), 2)[1] ) ) %>%
  dplyr::relocate( ageband2, .after = ageband1 ) %>%
  dplyr::mutate(
    signif_median = dplyr::if_else( is_median_diff, "*", "" )
    ,signif_iqr = dplyr::if_else( is_iqr_diff, "†", "" )
    ,y = match( ageband1, levels( plot_data$ageband ) )
    ,yend = match( ageband2, levels( plot_data$ageband ) )
    ,x_median = 1.08
    ,xend_median = x_median - 0.05
    ,x_iqr = xend_median + 0.18
    ,xend_iqr = x_iqr - 0.05
  )
p_signif <- func__make_significance_plot_layer( signif_plot_data )

# Make plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = ageband
    ) ) +
  p_grey_pts_and_boxplot +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = ageband, size = N )
    ,colour = "coral"
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = ageband )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  p_signif +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by age band."
    #     )
    #     ,55
    #   )
    # ,subtitle = "Median stability index shown as a coral dot."
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(  
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
  ) +
  p_theme_preferences
  
# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_age__topProfessions.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----  

#################################################################
## Plot of stability index for the largest professions by sex. ##
#################################################################
# A Tukey-style boxplot that shows stability-index values across sex,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_sex %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::mutate( sex = factor( sex, levels = c( "Female", "Male" ) ) ) %>%
  dplyr::mutate( ageband = droplevels( .$sex ) )

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, sex )
    ,class_median = median( stability_index, na.rm = TRUE )
    ,class_min = min( stability_index, na.rm = TRUE )
    ,class_max = max( stability_index, na.rm = TRUE )
  ) %>%
  dplyr::left_join(
    N_profession_sex %>%
      dplyr::filter(
        year_end == year_of_interest
        ,Sex %in% levels( plot_data$ageband )
      ) %>%
      dplyr::mutate( Sex = droplevels( .$Sex ) )
    ,by = join_by( Profession == `Care setting`, sex == Sex )
  )

# Data for statistical significance indicators.
signif_plot_data <- 
  posterior_summaries_sex %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::select( Profession, sex1 = sex, is_median_diff, is_iqr_diff ) %>%
  dplyr::mutate( sex2 = "Male" ) %>%
  dplyr::relocate( sex2, .after = sex1 ) %>%
  dplyr::mutate(
    signif_median = dplyr::if_else( is_median_diff, "*", "" )
    ,signif_iqr = dplyr::if_else( is_iqr_diff, "†", "" )
    ,y = match( sex1, levels( plot_data$sex ) )
    ,yend = match( sex2, levels( plot_data$sex ) )
    ,yend =
      dplyr::if_else( is.na( yend ), ( length( levels( plot_data$sex ) ) -1 ) /2, yend )
    ,x_median = 1.08
    ,xend_median = x_median - 0.05
    ,x_iqr = xend_median + 0.18
    ,xend_iqr = x_iqr - 0.05
  )
p_signif <- func__make_significance_plot_layer( signif_plot_data )

# Make plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = sex
    ) ) +
  p_grey_pts_and_boxplot +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = sex, size = N )
    ,colour = "coral"
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = sex )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1.5
  ) +
  p_signif +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by sex."
    #     )
    #     ,55
    #   )
    # ,subtitle = "Median stability index shown as a coral dot."
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(  
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
  ) +
  p_theme_preferences

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_sex__topProfessions.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----  

###########################################
## Plot of stability index by ethnicity. ##
###########################################
# A Tukey-style boxplot that shows stability-index values across ethnic groups,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_ethnicity %>% 
  dplyr::filter( Profession %in% professions_of_interest ) 

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, ethnicity )
    ,class_median = median( stability_index, na.rm = TRUE )
    ,class_min = min( stability_index, na.rm = TRUE )
    ,class_max = max( stability_index, na.rm = TRUE )
  ) %>%
  dplyr::left_join(
    N_profession_ethnicity %>%
      dplyr::filter(
        year_end == year_of_interest
        ,`Ethnic group` %in% plot_data$ethnicity
      )
    ,by = join_by( Profession == `Care setting`, ethnicity == `Ethnic group` )
  )

# Data for statistical significance indicators.
signif_plot_data <- 
  posterior_summaries_ethnicity_binary %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::select( Profession, ethnicity_binary2 = ethnicity_binary, is_median_diff, is_iqr_diff ) %>%
  dplyr::mutate( ethnicity_binary1 = factor( "White", levels = c( "White", "Non-white" ) ) ) %>%
  dplyr::relocate( ethnicity_binary1, .before = ethnicity_binary2 ) %>%
  dplyr::mutate(
    signif_median = dplyr::if_else( is_median_diff, "*", "" )
    ,signif_iqr = dplyr::if_else( is_iqr_diff, "†", "" )
    ,y = match( ethnicity_binary1, levels( plot_data$ethnicity_binary ) )
    ,yend = ( length( unique( plot_data$ethnicity ) ) + 1 ) / 2
    ,x_median = 1.08
    ,xend_median = x_median - 0.05
    ,x_iqr = xend_median + 0.18
    ,xend_iqr = x_iqr - 0.05
  )
p_signif <- func__make_significance_plot_layer( signif_plot_data )

# Make plot.
p <-
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = ethnicity
    ) ) +
  p_grey_pts_and_boxplot +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( ethnicity ), size = N )
    ,colour = "coral"
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( ethnicity ) )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  p_signif +
  geom_segment(
    data = signif_plot_data %>% dplyr::filter( is_median_diff )
    ,aes( x = xend_median, xend = xend_median, y = 2, yend = 8 )
    ,inherit.aes = FALSE
    ,linewidth = 1
  ) +
  geom_segment(
    data = signif_plot_data %>% dplyr::filter( is_iqr_diff )
    ,aes( x = xend_iqr, xend = xend_iqr, y = 2, yend = 8 )
    ,inherit.aes = FALSE
    ,linewidth = 1
  ) +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by Ethnic Group."
    #     )
    #     ,50
    #   )
    # ,subtitle = "Median stability index shown as a coral dot."
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
  ) +
  p_theme_preferences


# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_ethnicity__topProfessions.png"
    )
  ,dpi = 300
  ,width = 22
  ,height = 22
  ,units = "cm"
)
# ----

#############################################
## Plot of stability index by deprivation. ##
#############################################
# A scatter plot of stability-index values and deprivation score, using data
# from the largest professions, only. Delimit to the year ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_deprivation %>%
  dplyr::filter( Profession %in% professions_of_interest ) 

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession )
    ,class_median = median( stability_index, na.rm = TRUE )
  )

# Make plot.
p <-
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = deprivation
    ) ) +
  geom_vline(
    data = sumstat_plot_data
    ,aes( xintercept = class_median )
    ,colour = "coral"
    ,linewidth = 2
  ) +
  geom_point() +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    ,y = "Index of Multiple Deprivation (IMD) Score"
    # ,title =0
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index and deprivation score for the"
    #       ," largest* professions in 2025."
    #     )
    #     ,55
    #   )
    # ,subtitle = "Median stability index shown as a coral bar."
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text( size = 10 )
    ,axis.text.y = element_text( size = 12, face = "bold" )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )


# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_deprivation__topProfessions.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

##########################################
## Plot of stability index by rurality. ##
##########################################
# A Tukey-style boxplot that shows stability-index values across rurality
# category using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_rurality %>%
  dplyr::filter( Profession %in% professions_of_interest ) 


# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, rurality )
    ,class_median = median( stability_index, na.rm = TRUE )
    ,class_min = min( stability_index, na.rm = TRUE )
    ,class_max = max( stability_index, na.rm = TRUE )
  ) %>%
  dplyr::left_join(
    N_profession_rurality %>%
      dplyr::filter(
        year_end == year_of_interest
        ,`RUC21 settlement class` %in% levels( plot_data$rurality )
      ) %>%
      dplyr::mutate( `RUC21 settlement class` = droplevels( .$`RUC21 settlement class` ) )
    ,by = join_by( Profession == `Care setting`, rurality == `RUC21 settlement class` )
  )

# Make plot.
p <-
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = rurality
    ) ) +
  p_grey_pts_and_boxplot +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( rurality ), size = N )
    ,colour = "coral"
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( rurality ) )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  p_scale_and_facet_options +
  labs(
    x = "Stability index"
    ,y = "Rural-Urban Classification"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index for the largest* professions"
    #       ," in 2025, stratified by rurality category."
    #     )
    #     ,55
    #   )
    # ,subtitle = "Median stability index shown as a coral dot."
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #     )
    #     ,100
    #   )
  ) +
  p_theme_preferences

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_rurality__topProfessions.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

#########################################
## Plot of stability index by vacancy. ##
#########################################
# A scatter plot of stability-index values and vacancy, using data from the
# largest professions, only. Delimit to the year ending 2025.
# ----

# Make plot data.
plot_data <-
  model_data_vacancy %>%
  dplyr::filter( Profession %in% professions_of_interest )

# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession )
    ,class_median = median( stability_index, na.rm = TRUE )
  )

# Set plot range parameters.
y_axis_range <- c( -1, 1 )

# Make plot.
p <-
  plot_data %>%
  ggplot(
    aes(
      x = stability_index
      ,y = vacancy
    ) ) +
  geom_vline(
    data = sumstat_plot_data
    ,aes( xintercept = class_median )
    ,colour = "coral"
    ,linewidth = 2
  ) +
  geom_point() +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1)
    ,labels = c( "0", "0.25", "0.5", "0.75", "1" )
  ) +
  ylim( y_axis_range ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  labs(
    x = "Stability index"
    ,y = "Mean monthly vacancy rate in preceding 12 months"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Distribution of stability index and vacancy rate for the"
    #       ," largest* professions in 2025."
    #     )
    #     ,55
    #   )
    # ,subtitle = "Median stability index shown as a coral bar."
    # ,caption =
    #     paste0(
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #       ,"\n\u2022 Vacancy rate is the arithmetic mean of monthly rates over the preceding 12 months."
    #       ,"\n\u2022 Vacancy-rate axis is truncated ", y_axis_range[1], " to "
    #       ,y_axis_range[2], "."
    #     )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text( size = 10 )
    ,axis.text.y = element_text( size = 12, face = "bold" )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )


# Save plot.
ggsave(
  plot = p
  ,filename =
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_vacancy__topProfessions.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

##############################################################
## Cross-hairs plot of stability index versus vacancy rate. ##
##############################################################
# ----
# Make dataset.
plot_data <-
  data_vacancy_variables %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  # Summarise.
  dplyr::reframe(
    .by = c( Profession )
    ,SI_median = median( stability_index, na.rm = T )
    ,SI_qtr1 = quantile( stability_index, probs = 0.25,na.rm = T )
    ,SI_qtr3 = quantile( stability_index, probs = 0.75, na.rm = T )
    ,vacancy_median = median( vacancy_rate, na.rm = T )
    ,vacancy_qtr1 = quantile( vacancy_rate, probs = 0.25,na.rm = T )
    ,vacancy_qtr3 = quantile( vacancy_rate, probs = 0.75, na.rm = T )
  )

# Save to file.
write.csv( plot_data, "Tables/Paper 1/table__SIvsVacancy_Crosshairs_topProfessions.csv" )

# Set plot range parameters.
x_axis_range <- c( 0.75, 1 )
y_axis_range <- c( -0.02, 0.16 )

# Make plot
p <-
  plot_data %>%
  ggplot(
    aes( x = SI_median, y = vacancy_median, colour = Profession )
  ) +
  geom_point() +
  geom_errorbar(
    aes( xmin = SI_qtr1, xmax = SI_qtr3 )
  ) +
  geom_errorbar(
    aes( ymin = vacancy_qtr1, ymax = vacancy_qtr3 )
  ) +
  xlim( x_axis_range ) +
  ylim( y_axis_range ) +
  labs(
    x = "Stability index"
    ,y = "Mean monthly vacancy rate in preceding 12 months"
    ,title =
      stringr::str_wrap(
        paste0(
          "Stability index in 2025 and vacancy rate over the previous 12 months."
        )
        ,43
      )
    ,subtitle =
      stringr::str_wrap(
        paste0( "Showing medians and inter-quartile ranges." )
        ,55
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
          ,"\n(Stability-index axis is truncated ", x_axis_range[1], "-", x_axis_range[2]
          ," and vacancy-rate axis is truncated ", y_axis_range[1], "-", y_axis_range[2], ".)"
        )
        ,100
      )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__SIvsVacancy_Crosshairs_topProfessions.png"
    )
  ,dpi = 300
  ,width = 15
  ,height = 15
  ,units = "cm"
)
# ----

###############################################################################
## Cross-hairs plot of stability index versus vacancy rate. All professions. ##
###############################################################################
# ----
# Make dataset.
plot_data <-
  data_vacancy_variables %>%
  dplyr::filter( !stringr::str_detect( Profession, pattern = "Transport|Educ" ) ) %>%
  # Summarise.
  dplyr::reframe(
    .by = c( Profession )
    ,SI_median = median( stability_index, na.rm = T )
    ,SI_qtr1 = quantile( stability_index, probs = 0.25,na.rm = T )
    ,SI_qtr3 = quantile( stability_index, probs = 0.75, na.rm = T )
    ,vacancy_median = median( vacancy_rate, na.rm = T )
    ,vacancy_qtr1 = quantile( vacancy_rate, probs = 0.25,na.rm = T )
    ,vacancy_qtr3 = quantile( vacancy_rate, probs = 0.75, na.rm = T )
  )

# Save to file.
write.csv( plot_data, "Tables/Paper 1/table__SIvsVacancy_Crosshairs_AllProfessions.csv" )

# Make plot
p <-
  plot_data %>% 
  ggplot(
    aes( x = SI_median, y = vacancy_median, colour = Profession )
  ) +
  geom_point() +
  geom_errorbar(
    aes( xmin = SI_qtr1, xmax = SI_qtr3 )
  ) +
  geom_errorbar(
    aes( ymin = vacancy_qtr1, ymax = vacancy_qtr3 )
  ) +
  labs(
    x = "Stability index"
    ,y = "Mean monthly vacancy rate in preceding 12 months"
    ,title =
      stringr::str_wrap(
        paste0(
          "Stability index in 2025 and vacancy rate over the previous 12 months."
        )
        ,43
      )
    ,subtitle =
      stringr::str_wrap(
        paste0( "Showing medians and inter-quartile ranges." )
        ,55
      )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__SIvsVacancy_Crosshairs_AllProfessions.png"
    )
  ,dpi = 300
  ,width = 15
  ,height = 15
  ,units = "cm"
)
# ----

#################################
## Choropleth: Stability Index ##
#################################
# ----

# Join to 'df_churn_within_NHS_Grade'.
choropleth_churn_data <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == "All AfC bands"
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for all professions combined.
    ,`Care setting` == "All care settings"
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>%
  dplyr::select(
    orgname = `Organisation name`
    ,stability_index = `Stability index`
  )

choropleth_churn_data_band5only <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for all professions combined.
    ,`Care setting` == "All care settings"
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Filter for pay band 5, only.
    ,`AfC band` == "Band 5"
  ) %>% 
  dplyr::select(
    orgname = `Organisation name`
    ,stability_index_band5only = `Stability index`
  )

chloro_data_SI <-
  tc_shp %>%
  dplyr::left_join(
    choropleth_churn_data
    ,by = join_by( orgname )
  ) %>%
  dplyr::left_join(
    choropleth_churn_data_band5only
    ,by = join_by( orgname )
  ) 

chloro_data_SI <-
  data.frame(
    old =
      c(
        'Yeovil District Hospital NHS Foundation Trust' 
        ,'Northern Devon Healthcare NHS Trust'
        ,'Southport and Ormskirk Hospital NHS Trust' 
      ) 
    ,new = 
      c(
        'Somerset NHS Foundation Trust'
        ,'Royal Devon University Healthcare NHS Foundation Trust'
        ,'Mersey and West Lancashire Teaching Hospitals NHS Trust'
      ) 
  ) %>%
  dplyr::mutate( across( everything(), tolower ) ) %>%
  dplyr::left_join(
    choropleth_churn_data
    ,by = join_by( new == orgname )
  ) %>%
  dplyr::left_join(
    choropleth_churn_data_band5only
    ,by = join_by( new == orgname )
  ) %>%
  dplyr::select( -new ) %>% 
  dplyr::right_join(
    chloro_data_SI
    ,by = join_by( old == orgname )
    ,suffix = c( "", ".y" )
  ) %>%
  dplyr::mutate(
    stability_index = 
      dplyr::if_else(
        is.na( stability_index)
        ,stability_index.y
        ,stability_index
      )
    ,stability_index_band5only = 
      dplyr::if_else(
        is.na( stability_index_band5only )
        ,stability_index_band5only.y
        ,stability_index_band5only
      )
  ) %>%
  dplyr::select( -ends_with( ".y" ) ) %>%
  dplyr::rename( `Trust name` = old ) %>%
  tidyr::pivot_longer(
    cols = c( stability_index, stability_index_band5only )
    ,values_to = "stability_index" 
    ,names_to = "payband"
  ) %>%
  dplyr::mutate(
    payband = 
      dplyr::if_else(
        payband == "stability_index"
        ,"All pay bands"
        ,"Band-5, only"
      )
  )

# Plot map: Stability index for all pay bands.
p <-
  chloro_data_SI %>%
  dplyr::filter(
    stability_index > 0.5
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = stability_index
    )
  ) +
  # scale_fill_continuous( limits = c( 0.5, 1 ) ) +
  facet_wrap( ~payband ) +
  labs(
    fill = "Stability index"
    # title = 
    #   stringr::str_wrap(
    #     paste0(
    #       "Stability index of non-specialist acute Trusts' catchment areas in "
    #       ,"England for the year ending "
    #       ,year_of_interest, "."
    #     )
    #     ,65
    #   )
    # ,caption = "Range of stability index limited to 0.5-1."
  ) +
  theme_minimal() +
    theme(
      axis.title.y = element_blank()
      ,axis.text = element_blank()
      ,plot.title = element_text( size = 20 )
      ,strip.text.x = element_text( size = 15 )
      ,legend.position = "bottom"
    )
  

# Save the plot.
ggsave(
  plot = p
  ,filename = "Plots/Paper 1/plot__si_choropleth_with_Band5_comparison.png"
  ,dpi = 300
  ,width = 20
  ,height = 15
  ,units = "cm"
)

# ----

###########################################
## Choropleth: Vacancy. Top professions. ##
###########################################
# ----

# Make choropleth data.frame.
choropleth_vacancy_data <-
  model_data_vacancy %>%
  dplyr::filter( Profession %in% professions_of_interest ) %>%
  dplyr::select( orgcode, Profession, vacancy ) %>%
  dplyr::left_join(
    df_churn_within_NHS_Grade %>% dplyr::distinct( `Org code`, `Organisation name` )
    ,by = join_by( orgcode == `Org code` )
    ,relationship = "many-to-one"
    ) %>%
  dplyr::select( orgname = `Organisation name`, Profession, vacancy ) %>%
  dplyr::right_join(
    tc_shp
    ,by = join_by( orgname )
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate( vacancy = vacancy * 100 )

# Set plot range parameters.
y_axis_range <- c( -100, 100 )

# Plot map: Stability index for all pay bands.
p <-
  choropleth_vacancy_data %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = vacancy
    )
  ) +
  scale_fill_gradient2( limits = y_axis_range ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 15 )) +
  labs(
    fill = "Mean monthly vacancy rate in preceding 12 months"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Vacancy rate of non-specialist acute Trusts' catchment areas in "
    #       ,"England for the year ending "
    #       ,year_of_interest, "."
    #     )
    #     ,65
    #   )
    # ,caption =
    #   paste0(
    #     "Vacancy-rate axis is truncated ", y_axis_range[1]
    #     ," to ", y_axis_range[2], "."
    #     )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_blank()
    ,plot.title = element_text( size = 20 )
    ,strip.text.x = element_text( size = 15 )
    ,legend.position = "bottom"
    ,legend.title.position = "top"
  )


# Save the plot.
ggsave(
  plot = p
  ,filename = "Plots/Paper 1/plot__vacancy_choropleth_topProfessions.png"
  ,dpi = 300
  ,width = 20
  ,height = 15
  ,units = "cm"
)

# ----

###########################################
## Choropleth: Vacancy. All professions. ##
###########################################
# ----

# Make choropleth data.frame.
choropleth_vacancy_data <-
  model_data_vacancy %>%
  dplyr::select( orgcode, Profession, vacancy ) %>%
  dplyr::left_join(
    df_churn_within_NHS_Grade %>% dplyr::distinct( `Org code`, `Organisation name` )
    ,by = join_by( orgcode == `Org code` )
    ,relationship = "many-to-one"
  ) %>%
  dplyr::select( orgname = `Organisation name`, Profession, vacancy ) %>%
  dplyr::right_join(
    tc_shp
    ,by = join_by( orgname )
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate( vacancy = vacancy * 100 )

# Set plot range parameters.
y_axis_range <- c( -100, 100 )

# Plot map: Stability index for all pay bands.
p <-
  choropleth_vacancy_data %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = vacancy
    )
  ) +
  scale_fill_gradient2( limits = y_axis_range ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 15 )) +
  labs(
    fill = "Mean monthly vacancy rate in preceding 12 months"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Vacancy rate of non-specialist acute Trusts' catchment areas in "
    #       ,"England for the year ending "
    #       ,year_of_interest, "."
    #     )
    #     ,65
    #   )
    # ,caption =
    #   paste0(
    #     "Vacancy-rate axis is truncated ", y_axis_range[1]
    #     ," to ", y_axis_range[2], "."
    #     )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_blank()
    ,plot.title = element_text( size = 20 )
    ,strip.text.x = element_text( size = 10 )
    ,legend.position = "bottom"
    ,legend.title.position = "top"
    ,panel.spacing.x = unit( 30, "pt" )
  )


# Save the plot.
ggsave(
  plot = p
  ,filename = "Plots/Paper 1/plot__vacancy_choropleth_allProfessions.png"
  ,dpi = 300
  ,width = 20
  ,height = 15
  ,units = "cm"
)

# ----

####################################################
## Plot of Rurality vs Deprivation vs Trust size. ##
####################################################
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
  dplyr::select( orgcode = `Org code`, stability_index = `Stability index` ) %>%
  dplyr::left_join(
    df_deprivation %>% dplyr::distinct( trustcode = `Trust Code`, imdscore = `IMD Score` )
    ,by = join_by( orgcode == trustcode )
  ) %>%
  dplyr::left_join(
    df_ons_rurality %>% dplyr::distinct( trustcode = `Trust code`, rurality = `RUC21 settlement class` )
    ,by = join_by( orgcode == trustcode )
  ) %>%
  dplyr::left_join(
    df_Trust_size_2023_03 %>% dplyr::distinct( trustcode = `Trust code 2023 03`, trustsize =`Trust size 2023 03` )
    ,by = join_by( orgcode == trustcode )
  ) %>%
  dplyr::arrange( stability_index )

# Make plot
p <-
  plot_data %>%
  ggplot() +
  geom_point(
    aes(
      x = imdscore
      ,y = rurality
      ,size = trustsize
      ,colour = stability_index
    )
    ,position = position_jitter( height = 0.2 )
  ) +
  scale_x_continuous( limits = c( 0, 50 ) ) +
  scale_color_continuous( limits = c( 0, 1 ) ) +
  labs(
    x = "Deprivation score"
    ,y = "Rurality category"
    ,size = "Trust size"
    ,colour = "Stability index"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       "Depriviation score across rurality categories and Trust size "
    #       ," in non-specialist acute Trusts in NHS England for the year ending "
    #       , year_of_interest, "."
    #     )
    #     ,45
    #   )
    # ,subtitle =
    #   paste0(
    #     "\u2022 Larger deprivation scores indicate greater deprivation."
    #     ,"\n\u2022 Lighter-coloured stability index values indicate more staff retention."
    #     ,"\n\u2022 Bigger circles indicate larger Trust size (by staff head count)."
    #   )
    # ,caption =
    #   paste0(
    #     "Stability index from ", year_of_interest,"."
    #     ,"\nRurality category from 2021."
    #     ,"\nTrust size is staff head count from March 2023."
    #   )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text.x = element_text( size = 12, face = "bold" )
    ,axis.text.y = element_text( size = 12, face = "bold" )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )

ggsave(
  plot = p
  ,filename =
    "Plots/Paper 1/plot__ruralityVdeprivationVsize.png"
  ,dpi = 300
  ,width = 20
  ,height = 10
  ,units = "cm"
)

# ----

##########################################################
## Plot of stability index versus Joiners:Leavers rate. ##
##########################################################
# ----
# Make dataset.
plot_data <-
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
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select(
    orgcode = `Org code`, Profession = `Care setting`, payband = `AfC band`
    ,stability_index = `Stability index`
    ,staff_at_start = `Denominator at start of period` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  ) %>%
  # Join in the vacancy data.
  dplyr::left_join(
    data_JLrateRatio %>%
      dplyr::select(
        orgcode = `Org code`, Profession = `Care setting`
        ,payband = `AfC band`, JL_rate_ratio
      )
    ,by = dplyr::join_by( orgcode, Profession, payband )
  ) %>%
  # Exclude band 9 because the rate ratios are almost entirely NA.
  dplyr::filter( !payband %in% c( "Band 8d", "Band 9" ) )

# Set plot range parameters.
x_axis_range <- c( 0, 2 )

# Make plot
p <-
  plot_data %>% 
  ggplot(
    aes( x = stability_index, y = JL_rate_ratio )
  ) +
  annotate("rect", ymin = -Inf, ymax = 1, xmin = -Inf, xmax = Inf , fill = "red", alpha = 0.2 ) +
  geom_point( aes( size = staff_at_start ), alpha = 0.2 ) +
  facet_grid(
    rows = vars( forcats::fct_rev( payband ) )
    ,cols = vars( Profession )
    ,labeller = label_wrap_gen( 15 )
  ) +
  scale_y_continuous(
    labels = c( "", "1", "" )
    ,breaks = c( 0, 1, 2 )
    ,limits = x_axis_range
  ) +
  scale_x_continuous(
    labels = c( "0", "0.5", "1" )
    ,breaks = c( 0, 0.5, 1 )
  ) +
  labs(
    x = "Stability index"
    ,y = "Joiner : Leaver  ratio"
    ,size = "Staff head-count at start of year"
    # ,title =
    #   stringr::str_wrap(
    #     paste0(
    #       " Stability index and Joiner:Leaver ratio for the largest* professions"
    #       ," in 2025."
    #     )
    #     ,55
    #   )
    # ,subtitle =
    #   paste0(
    #     "\u2022 Ratio axis is right-censored at 2.0 to focus on the 1.0 pivot point."
    #     ,"\n\u2022 The red regions indicate more leavers than joiners."
    #     ,"\n\u2022 Bands 9 and 8d are excluded because of a lack of joiners and leavers."
    #   )
    # ,caption =
    #   stringr::str_wrap(
    #     paste0(  
    #       "*Size of profession was determined as the count of that staff role"
    #       ," at the start of the year."
    #       ,"\nJoiner:Leaver rate ratio = # people joined during the year / # people left during the year."
    #     )
    #     ,100
    #   )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 10 )
    ,strip.text.y = element_text( size = 10, angle = 0, hjust = -1 )
    ,legend.position = "bottom"
  )

# Save plot.
ggsave(
  plot = p
  ,filename = "Plots/Paper 1/plot__SIvsJLrateRatio__topProfessions.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



###############################################
## Plot of absolute versus relative vacancy. ##
###############################################
# ----
# Make dataset.
plot_data <- data_vacancy_variables

# Make plot
p <-
  plot_data %>%
  ggplot(
    aes( x = vacancy_count, y = vacancy_rate )
  ) +
  geom_point( aes( size = staff_count ), alpha = 0.2 ) +
  # ylim( y_axis_range ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  labs(
    x = "Vacancy count (n)"
    ,y = "Vacancy rate (%)"
    ,title =
      stringr::str_wrap(
        paste0(
          "Vacancy count versus vacancy rates for the largest* professions"
          ," in 2025."
        )
        ,55
      )
    ,subtitle =
      stringr::str_wrap(
        paste0(
          "Negative values indicate over-staffing."
        )
        ,100
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
        )
        ,100
      )
    ,size = stringr::str_wrap( "Staff in post", 10 )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__absoluteVSrelative_vacancy.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# Make zoomed-in plot.
# # Set plot range parameters.
x_axis_range <- c( -20, 20 )
y_axis_range <- c( -0.5, 0.5 )

p <-
  plot_data %>%
  dplyr::filter(
    dplyr::between( vacancy_count, x_axis_range[1], x_axis_range[2] )
    ,dplyr::between( vacancy_rate, y_axis_range[1], y_axis_range[2] )
    ) %>%
  ggplot(
    aes( x = vacancy_count, y = vacancy_rate )
  ) +
  geom_point( aes( size = staff_count ), alpha = 0.2 ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  xlim( x_axis_range ) +
  ylim( y_axis_range ) +
  labs(
    x = "Vacancy count (n)"
    ,y = "Vacancy rate (%)"
    ,title =
      stringr::str_wrap(
        paste0(
          "Vacancy count versus vacancy rates for the largest* professions"
          ," in 2025."
        )
        ,55
      )
    ,subtitle =
        paste0(
          "Negative values indicate over-staffing."
          ,"\nVacancy-count axis is truncated ", x_axis_range[1], " to ", x_axis_range[2], "."
          ," Vacancy-rate axis is truncated ", y_axis_range[1], "% to ", y_axis_range[2], "%."
        )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
          
        )
        ,100
      )
    ,size = stringr::str_wrap( "Staff in post", 10 )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__absoluteVSrelative_vacancy_zoomed.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# ----


##########################################
## Plot of joiner rate and leaver rate. ##
##########################################
# ----

# Set plot range parameters.
rate_max_val <- max( data_JLrateRatio$joiner_rate, data_JLrateRatio$leaver_rate )
x_axis_range <- c( 0, rate_max_val )
y_axis_range <- c( 0, rate_max_val )

# Make plot
p <-
  data_JLrateRatio %>%
  dplyr::filter(
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  ggplot(
    aes( x = joiner_rate, y = leaver_rate )
  ) +
  geom_point() +
  facet_grid(
    rows = vars( forcats::fct_rev( `AfC band` ) )
    ,cols = vars( `Care setting` )
    ,labeller = label_wrap_gen( 15 )
  ) +
  xlim( x_axis_range ) +
  ylim( y_axis_range ) +
  labs(
    x = "Joiner rate"
    ,y = "Leaver rate"
    ,title =
      stringr::str_wrap(
        paste0(
          "Joiner rate versus Leaver rate for the largest* professions in 2025."
        )
        ,100
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
        )
        ,100
      )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
    ,strip.text.y = element_text( size = 15, angle = 0, hjust = -1 )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__Joiner_v_Leaver_rate__topProfessions.png"
    )
  ,dpi = 300
  ,width = 30
  ,height = 30
  ,units = "cm"
)

# ----
