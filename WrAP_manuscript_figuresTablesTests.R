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
pacman::p_load(
  coin
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

# Set the year of interest.
year_of_interest <- 2025

# Set storage locations.
dir.create( "./Tests/paper 1" )
dir.create( "./Tables/paper 1" )
dir.create( "./Plots/paper 1" )
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


# ~~~~~~~~~~~~
# ~~ Tables ~~ 
# ~~~~~~~~~~~~

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
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
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
df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    !`AfC band` %in% c( 'All AfC bands' )
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
    ,.by = c( year_end, `Care setting`, `AfC band` )
  ) %>%
  dplyr::arrange( -year_end ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__headcounts_per_payband.csv" )
# ----

######################################################################
# Summary statistics of stability index, per profession and payband. #
######################################################################
# ----

# Make dataset. This will be reused.
df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands', 'Non AfC band', 'Band 4' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` == 'All care settings'
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
    ,N = sum( `Stability index`, na.rm = TRUE )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
  ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_summary_per_payband.csv" )
# ----

######################################################################
# Summary statistics of stability index, per profession and payband. #
######################################################################
# ----

# Make dataset. This will be reused.
payband_data <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands', 'Non AfC band', 'Band 4' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Org code`, `Care setting`, `AfC band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  )

payband_data %>%
  dplyr::reframe(
    .by =  c( Profession, `AfC band` )
    ,N = sum( `Stability index`, na.rm = TRUE )
    ,Min = min( `Stability index`, na.rm = TRUE )
    ,qtr1 = quantile( `Stability index`, probs = 0.25, na.rm = TRUE )
    ,Median = median( `Stability index`, na.rm = TRUE )
    ,qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = TRUE )
    ,Max = max( `Stability index`, na.rm = TRUE )
  ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__SI_summary_per_profession_per_payband.csv" )
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
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
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
    ) %>%
  # Select columns of interest.
  dplyr::select(
    `Organisation name`
    ,`Org code`
    ,`Care setting`
    ,`AfC band`
    ,JL_rate_ratio
    ,joiner_rate
    ,leaver_rate
  ) 

# Make table.
data_JLrateRatio %>% 
  # Get national summary statistics.
  dplyr::reframe(
    .by = c( `Care setting`, `AfC band` )
    ,Min = min( JL_rate_ratio, na.rm = TRUE )
    ,qtr1 = quantile( JL_rate_ratio, probs = 0.25, na.rm = TRUE )
    ,Median = median( JL_rate_ratio, na.rm = TRUE )
    ,Mean = mean( JL_rate_ratio, na.rm = TRUE )
    ,qtr3 = quantile( JL_rate_ratio, probs = 0.75, na.rm = TRUE )
    ,Max = max( JL_rate_ratio, na.rm = TRUE )
    ) %>%
  dplyr::arrange( `Care setting`, `AfC band` ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__joinerLeaver_rate_ratio.csv" )
  
# ----

##########################
# Extreme vacancy rates. #
##########################
# ----

# Make dataset. This will be used again, later.
data_vacancy_variables <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == 'All AfC bands'
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Organisation name`, `Org code`, `Care setting`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  ) %>%
  # Join in the vacancy data.
  dplyr::left_join(
    df_vacancy %>%
      dplyr::filter( vacancy_year == year_of_interest )
    ,by =
      join_by(
        `Org code` == `Trust code`
        ,Profession == `Care setting` 
      )
  ) 

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
    ,by = join_by( `Org code` == `Trust code 2023 03` )
    ) %>%
  dplyr::select(
    `High or low`, `Organisation name`, `Trust size 2023 03`
    ,`Profession`, vacancy_rate, vacancy_count
  ) %>%
  # Save to file.
  write.csv( "Tables/Paper 1/table__extreme_vacancy_rates.csv" )
# ----

# ~~~~~~~~~~~
# ~~ Tests ~~
# ~~~~~~~~~~~

#############################################
# Test of stability index across pay bands. #
#############################################
# ----
# I've decided to fit Linear Quantile Mixed Models using Bayesian regression.
# I will specify the pay band  as a fixed-effect covariate and look at the
# estimated coefficient. I will judge statistical significance at the 0.05 level
# if the credible intervals do not cross 0.
# This model specification acknowledges the fact that observations at different
# pay bands within a Trust are likely to be more correlated than those between
# Trusts.

# Edit the column names because the function doing the test is quirky.
payband_test_data <-
  payband_data %>%
  dplyr::rename(
    stability_index = `Stability index`
    ,payband = `AfC band`
    ,orgcode = `Org code`
  ) %>%
  dplyr::mutate(
    payband_binary =
      dplyr::if_else(
        payband == "Band 5"
        ,payband
        ,">Band 5"
      )
    ,payband_binary = 
      forcats::fct_relevel( payband_binary, ">Band 5", after = Inf )
  )

# Fit Linear Quantile Mixed Models to the data for each profession.
df_model_signif <- data.frame( name = character(), signif = logic() )
# Model 1: Band 5 -vs- all other bands combined.
for ( i_profession in 1:length( professions_of_interest ) )
{
  # Select dataset of interest.
  df_of_interest <-
    payband_test_data %>% 
    dplyr::filter( Profession == professions_of_interest[ i_profession ] )
  
  # Fit model.
  if(
    df_of_interest %>%
    dplyr::reframe( .by = payband_binary, n = n() ) %>%
    dplyr::pull( n ) %>%
    min() > 10
    )
  {
    # Set model name.
    mod_name <- paste0( "lmm_5vAll_", professions_of_interest[ i_profession ] )
    assign(
      mod_name
      ,brms::brm(
        brms::brmsformula(
          stability_index ~ payband_binary + ( payband | orgcode )
          ,quantile = 0.5
        )
        ,family = asym_laplace()
        ,data =df_of_interest
      )
    )
    
    # Extract indicator of statistical significance from the model object.
    mod_value <-
      summary( get( mod_name ) )$fixed[ 2, c( "Estimate", "l-95% CI",  "u-95% CI" ) ] %>%
        dplyr::transmute(
          signif = 
            dplyr::if_else(
              Estimate > 0
              ,( `l-95% CI` >0 ) & ( `u-95% CI` >0 )
              ,( `l-95% CI` <0 ) & ( `u-95% CI` <0 )
            )
        )
    
    # Save result.
    df_model_signif[ nrow( df_model_signif )+1, ]  <- c( name = mod_name, mod_value )
  }
}

# Model 2: Band 5 -vs- Band 6
for ( i_profession in 1:length( professions_of_interest ) )
{
  # Select dataset of interest.
  df_of_interest <-
    payband_test_data %>% 
    dplyr::filter( Profession == professions_of_interest[ i_profession ] )
  
  # Fit model.
  if(
    df_of_interest %>%
    dplyr::reframe( .by = payband_binary, n = n() ) %>%
    dplyr::pull( n ) %>%
    min() > 10
  )
  {
    # Set model name.
    mod_name <- paste0( "lmm_5v6_", professions_of_interest[ i_profession ] )
    assign(
      mod_name
      ,brms::brm(
        brms::brmsformula(
          stability_index ~ payband + ( 1 | orgcode )
          ,quantile = 0.5
        )
        ,family = asym_laplace()
        ,data = payband_test_data %>% dplyr::filter( payband %in% c( "Band 5", "Band 6" ) )
      )
    )
    
    # Extract indicator of statistical significance from the model object.
    mod_value <-
      summary( get( mod_name ) )$fixed[ 2, c( "Estimate", "l-95% CI",  "u-95% CI" ) ] %>%
      dplyr::transmute(
        signif = 
          dplyr::if_else(
            Estimate > 0
            ,( `l-95% CI` >0 ) & ( `u-95% CI` >0 )
            ,( `l-95% CI` <0 ) & ( `u-95% CI` <0 )
          )
      )
    
    # Save result.
    df_model_signif[ nrow( df_model_signif )+1, ]  <- c( name = mod_name, mod_value )
  }
}
    
# Model 3:  Band 6 -vs- Band 7
for ( i_profession in 1:length( professions_of_interest ) )
{
  # Select dataset of interest.
  df_of_interest <-
    payband_test_data %>% 
    dplyr::filter( Profession == professions_of_interest[ i_profession ] )
  
  # Fit model.
  if(
    df_of_interest %>%
    dplyr::reframe( .by = payband_binary, n = n() ) %>%
    dplyr::pull( n ) %>%
    min() > 10
  )
  {
    # Set model name.
    mod_name <- paste0( "lmm_6v7_", professions_of_interest[ i_profession ] )
    assign(
      mod_name
      ,brms::brm(
        brms::brmsformula(
          stability_index ~ payband + ( 1 | orgcode )
          ,quantile = 0.5
        )
        ,family = asym_laplace()
        ,data = payband_test_data %>% dplyr::filter( payband %in% c( "Band 6", "Band 7" ) )
      )
    )
    
    # Extract indicator of statistical significance from the model object.
    mod_value <-
      summary( get( mod_name ) )$fixed[ 2, c( "Estimate", "l-95% CI",  "u-95% CI" ) ] %>%
      dplyr::transmute(
        signif = 
          dplyr::if_else(
            Estimate > 0
            ,( `l-95% CI` >0 ) & ( `u-95% CI` >0 )
            ,( `l-95% CI` <0 ) & ( `u-95% CI` <0 )
          )
      )
    
    # Save result.
    df_model_signif[ nrow( df_model_signif )+1, ]  <- c( name = mod_name, mod_value )
  }
}

# Model 4:  Band 7 -vs- Band 8a
for ( i_profession in 1:length( professions_of_interest ) )
{
  # Select dataset of interest.
  df_of_interest <-
    payband_test_data %>% 
    dplyr::filter( Profession == professions_of_interest[ i_profession ] )
  
  # Fit model.
  if(
    df_of_interest %>%
    dplyr::reframe( .by = payband_binary, n = n() ) %>%
    dplyr::pull( n ) %>%
    min() > 10
  )
  {
    # Set model name.
    mod_name <- paste0( "lmm_7v8a_", professions_of_interest[ i_profession ] )
    assign(
      mod_name
      ,brms::brm(
        brms::brmsformula(
          stability_index ~ payband + ( 1 | orgcode )
          ,quantile = 0.5
        )
        ,family = asym_laplace()
        ,data = payband_test_data %>% dplyr::filter( payband %in% c( "Band 7", "Band 8a" ) )
      )
    )
    
    # Extract indicator of statistical significance from the model object.
    mod_value <-
      summary( get( mod_name ) )$fixed[ 2, c( "Estimate", "l-95% CI",  "u-95% CI" ) ] %>%
      dplyr::transmute(
        signif = 
          dplyr::if_else(
            Estimate > 0
            ,( `l-95% CI` >0 ) & ( `u-95% CI` >0 )
            ,( `l-95% CI` <0 ) & ( `u-95% CI` <0 )
          )
      )
    
    # Save result.
    df_model_signif[ nrow( df_model_signif )+1, ]  <- c( name = mod_name, mod_value )
  }
}

# Model 5:  Band 8d -vs- Band 9
for ( i_profession in 1:length( professions_of_interest ) )
{
  # Select dataset of interest.
  df_of_interest <-
    payband_test_data %>% 
    dplyr::filter( Profession == professions_of_interest[ i_profession ] )
  
  # Fit model.
  if(
    df_of_interest %>%
    dplyr::reframe( .by = payband_binary, n = n() ) %>%
    dplyr::pull( n ) %>%
    min() > 10
  )
  {
    # Set model name.
    mod_name <- paste0( "lmm_8dv9_", professions_of_interest[ i_profession ] )
    assign(
      mod_name
      ,brms::brm(
        brms::brmsformula(
          stability_index ~ payband + ( 1 | orgcode )
          ,quantile = 0.5
        )
        ,family = asym_laplace()
        ,data = payband_test_data %>% dplyr::filter( payband %in% c( "Band 8d", "Band 9" ) )
      )
    )
    
    # Extract indicator of statistical significance from the model object.
    mod_value <-
      summary( get( mod_name ) )$fixed[ 2, c( "Estimate", "l-95% CI",  "u-95% CI" ) ] %>%
      dplyr::transmute(
        signif = 
          dplyr::if_else(
            Estimate > 0
            ,( `l-95% CI` >0 ) & ( `u-95% CI` >0 )
            ,( `l-95% CI` <0 ) & ( `u-95% CI` <0 )
          )
      )
    
    # Save result.
    df_model_signif[ nrow( df_model_signif )+1, ]  <- c( name = mod_name, mod_value )
  }
}

# Save overall data.frame.
write.csv( df_model_signif, "Tests/Paper 1/test__si_across_paybands.csv" )

# ----

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

#############################################################
# Test if deprivation and rurality explain stability index. #
#############################################################
# My approach accounts for the profession type rather than stratifies by it.
# ----

# Make dataset. This will be used again later.
data_deprivation_rurality_si <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == 'All AfC bands'
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  )

# Fit null model that does not account for deprivation and rurality.
model_null <- 
  lme4::lmer(
    formula = `Stability index` ~ ( 1 | `Care setting` )
    ,data =
      data_deprivation_rurality_si %>%
      dplyr::select(
        `Care setting`, `Stability index`
        ,`IMD Score`, `RUC21 settlement class`
        ) %>%
      tidyr::drop_na()
  )
# Fit alternative model that accounts for deprivation and rurality.
model_with_covariates <- 
  lme4::lmer(
    formula = `Stability index` ~ `IMD Score` + ( 1 | `Care setting` ) + `RUC21 settlement class`
    ,data = 
      data_deprivation_rurality_si %>%
      dplyr::select(
        `Care setting`, `Stability index`
        ,`IMD Score`, `RUC21 settlement class`
      ) %>%
      tidyr::drop_na()
  )
# Compare the models using a log likelihood test.
lmtest::lrtest( model_null, model_with_covariates ) %>%
  # Save to file.
  write.csv( "Tests/Paper 1/test__LRtest_IMD_and_Rurality.csv" )
# ----

##############################################################
# Test association between stability index and vacancy rate. #
##############################################################
# My approach stratifies by the profession type rather than accounts for it.
# ----
# I use the p-value from the ANOVA of a OLS regression of vacancy rate on
# stability index.

data_deprivation_rurality_si %>%
  dplyr::select(
    `Org code`, `Care setting`, `Stability index`, past_year_mean_vacancy_rate
  ) %>%
  dplyr::nest_by( `Care setting` ) %>%
  dplyr::mutate(
    anova =
      list(
        lm(
          formula = `Stability index` ~ past_year_mean_vacancy_rate
          ,data = data
        ) %>%
          anova()
      )
  ) %>%
  dplyr::summarise( anova_p.value_for_meanVacancyRate = anova$`Pr(>F)`[1] ) %>%
  # Save to file.
  write.csv( "Tests/Paper 1/test__association_si_vs_meanVacancyRate.csv" )

# ----
  
#############################################################
# Test association between stability index and deprivation. #
#############################################################
# My approach stratifies by the profession type rather than accounts for it.
# ----
# I use the p-value from the ANOVA of a OLS regression of deprivation on
# stability index.

data_deprivation_rurality_si %>%
  dplyr::select(
    `Org code`, `Care setting`, `Stability index`
    ,`IMD Score`
  ) %>%
  dplyr::nest_by( `Care setting` ) %>%
  dplyr::mutate(
    anova =
      list(
        lm(
          formula = `Stability index` ~ `IMD Score`
          ,data = data
        ) %>%
          anova()
      )
  ) %>%
  dplyr::summarise( anova_p.value_for_IMD = anova$`Pr(>F)`[1] ) %>%
  # Save to file.
  write.csv( "Tests/Paper 1/test__association_si_vs_IMD.csv" )

# ----

##########################################################
# Test association between stability index and rurality. #
##########################################################
# My approach stratifies by the profession type rather than accounts for it.
# ----
# I use the p-value from the ANOVA of a OLS regression of rurality on stability
# index.

data_deprivation_rurality_si %>%
  dplyr::select(
    `Org code`, `Care setting`, `Stability index`
    ,`RUC21 settlement class`
    ) %>%
  dplyr::nest_by( `Care setting` ) %>%
  dplyr::mutate(
      anova =
        list(
          lm(
            formula = `Stability index` ~ `RUC21 settlement class`
            ,data = data
            ) %>%
            anova()
        )
    ) %>%
  dplyr::summarise( anova_p.value_for_rurality = anova$`Pr(>F)`[1] ) %>%
  # Save to file.
  write.csv( "Tests/Paper 1/test__association_si_vs_rurality.csv" )

# ----

############################################################
#  Test association between stability index and ethnicity. #
############################################################
# ----

df_churn_within_NHS_EthnicGroup %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `Ethnic group` != 'All Ethnic groups'
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` == "All care settings"
  ) %>%
  dplyr::filter( `Ethnic group` != "Not stated" ) %>%
  dplyr::mutate(
    ethnic_group_binary = 
      dplyr::if_else(
        `Ethnic group` == "White"
        ,`Ethnic group`
        ,"Non-White"
        )
    ) %>% 
  dplyr::select( `Stability index`, ethnic_group_binary ) %>%
  lm(
    formula = `Stability index` ~ ethnic_group_binary
    ,data = .
  ) %>%
  anova() %>%
  # Save to file.
  write.csv( "Tests/Paper 1/test__association_si_vs_binaryEthnicity.csv" )
# ----


# ~~~~~~~~~~~
# ~~ Plots ~~ 
# ~~~~~~~~~~~

######################################################################
## Plot of stability index for the largest professions by age band. ##
######################################################################
# A Tukey-style boxplot that shows stability-index values across age bands,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  df_churn_within_NHS_AgeBand %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`Age band` %in% c( 'All age bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Care setting`, `Age band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
      )
    )


sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    class_median = median( `Stability index`, na.rm = TRUE )
    ,class_min = min( `Stability index`, na.rm = TRUE )
    ,class_max = max( `Stability index`, na.rm = TRUE )
    ,.by = c( Profession, `Age band` )
  )

p <- 
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = `Age band`
    ) ) +
  geom_point(
    position = position_jitter( height = 0.1 )
    ,alpha = 0.2
    ,colour = "grey"
  ) +
  geom_boxplot(
    fill = "grey"
    ,alpha = 0.3
    ,width = 0.2
    ,outlier.colour = "grey"
  ) +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = `Age band` )
    ,colour = "coral"
    ,size = 3
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = `Age band` )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  xlim( 0, 1 ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  labs(
    title =
      stringr::str_wrap(
        paste0(
          "Distribution of stability index for the largest* professions"
          ," in 2025, stratified by age band."
        )
        ,55
      )
    ,subtitle = "Median stability index shown as a coral dot."
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
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )
  
# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_age__top5.png"
    )
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
plot_data <- payband_data

sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    class_median = median( `Stability index`, na.rm = TRUE )
    ,class_min = min( `Stability index`, na.rm = TRUE )
    ,class_max = max( `Stability index`, na.rm = TRUE )
    ,.by = c( Profession, `AfC band` )
  )

# Make the plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = `AfC band`
    ) ) +
  geom_point(
    position = position_jitter( height = 0.1 )
    ,alpha = 0.2
    ,colour = "grey"
  ) +
  geom_boxplot(
    fill = "grey"
    ,alpha = 0.3
    ,width = 0.2
    ,outlier.colour = "grey"
    ) +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = `AfC band` )
    ,colour = "coral"
    ,size = 3
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = `AfC band` )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  xlim( 0, 1 ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  labs(
    title =
      stringr::str_wrap(
        paste0(
          "Distribution of stability index for the largest* professions"
          ," in 2025, stratified by Agenda-for-Change (AfC) pay band."
        )
        ,55
      )
    ,subtitle = "Median stability index shown as a coral dot."
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
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_pay__top5.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----  

#############################################################################
## Plot of stability index by ethnicity. Separate plot for each profession ##
#############################################################################
# A Tukey-style boxplot that shows stability-index values across ethnic groups,
# using data from the largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  df_churn_within_NHS_EthnicGroup %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`Ethnic group` %in% c( 'All ethnic groups' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Care setting`, `Ethnic group`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
  ) 


# Extract summary statistics for plotting.
sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    .by = c( Profession, `Ethnic group` )
    ,class_median = median( `Stability index`, na.rm = TRUE )
    ,class_min = min( `Stability index`, na.rm = TRUE )
    ,class_max = max( `Stability index`, na.rm = TRUE )
  )

min_val <-
  min(
    plot_data[ , "Stability index" ]
    ,na.rm = TRUE
  )
max_val <-
  max(
    plot_data[ , "Stability index" ]
    ,na.rm = TRUE
  )


# Make plot.
p <-
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = `Ethnic group`
    ) ) +
  geom_point(
    position = position_jitter( height = 0.1 )
    ,alpha = 0.2
    ,colour = "grey"
  ) +
  geom_boxplot( fill = "grey", alpha = 0.3, width = 0.2 ) +
  geom_point(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( `Ethnic group` ) )
    ,colour = "coral"
    ,size = 3
  ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = class_median, y = forcats::fct_rev( `Ethnic group` ) )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,colour = "coral"
    ,size = 3
    ,vjust = -1
  ) +
  xlim( 0, 1 ) +
  facet_wrap( ~Profession, labeller = label_wrap_gen( 20 ) ) +
  labs(
    title =
      stringr::str_wrap(
        paste0(
          "Distribution of stability index for the largest* professions"
          ," in 2025, stratified by Ethnic Group."
        )
        ,55
      )
    ,subtitle = "Median stability index shown as a coral dot."
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
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 15 )
  )


# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_ethnicity__top5.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)
# ----

##################################################
## Plot of stability index versus vacancy rate. ##
##################################################
# ----
# Make dataset.
plot_data <-
  data_vacancy_variables %>%
  # Summarise.
  dplyr::reframe(
    .by = c( Profession )
    ,SI_median = median( `Stability index`, na.rm = T )
    ,SI_qtr1 = quantile( `Stability index`, probs = 0.25,na.rm = T )
    ,SI_qtr3 = quantile( `Stability index`, probs = 0.75, na.rm = T )
    ,vacancy_median = median( vacancy_rate, na.rm = T )
    ,vacancy_qtr1 = quantile( vacancy_rate, probs = 0.25,na.rm = T )
    ,vacancy_qtr3 = quantile( vacancy_rate, probs = 0.75, na.rm = T )
  )

# Set plot range parameters.
x_axis_range <- c( -0.02, 0.25 )
y_axis_range <- c( 0.75, 1 )

# Make plot
p <-
  plot_data %>%
  ggplot(
    aes( x = vacancy_median, y = SI_median, colour = Profession )
  ) +
  geom_point() +
  geom_errorbar(
    aes( ymin = SI_qtr1, ymax = SI_qtr3 )
  ) +
  geom_errorbar(
    aes( xmin = vacancy_qtr1, xmax = vacancy_qtr3 )
  ) +
  xlim( x_axis_range ) +
  ylim( y_axis_range ) +
  labs(
    x = "Vacancy rate"
    ,y = "Stability index"
    ,title =
      stringr::str_wrap(
        paste0(
          " Stability index and vacancy rates for the largest* professions"
          ," in 2025."
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
          ,"\nStability-index axis is truncated ", y_axis_range[1], "-", y_axis_range[2]
          ," and vacancy-rate axis is truncated ", x_axis_range[1], "-", x_axis_range[2], "."
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
      "Plots/Paper 1/plot__SIvsVacancy__top5.png"
    )
  ,dpi = 300
  ,width = 15
  ,height = 15
  ,units = "cm"
)
# ----

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
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Org code`, `Care setting`, `AfC band`, `Stability index` ) %>%
  dplyr::mutate(
    `Care setting` = dplyr::if_else(
      `Care setting` == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,`Care setting`
    )
  ) %>%
  # Join in the vacancy data.
  dplyr::left_join(
    data_JLrateRatio %>%
      dplyr::select( `Org code`, `Care setting`, `AfC band`, JL_rate_ratio )
    ,by = join_by( `Org code`, `Care setting`, `AfC band` )
  ) %>%
  # Exclude band 9 because the rate ratios are almost entirely NA.
  dplyr::filter( !`AfC band` %in% c( "Band 8d", "Band 9" ) )

# Set plot range parameters.
x_axis_range <- c( 0, 1 )
y_axis_range <- c( 0, 1 )

# Make plot
p <-
  plot_data %>%
  ggplot(
    aes( x = JL_rate_ratio, y = `Stability index` )
  ) +
  geom_point() +
  facet_grid(
    rows = vars( forcats::fct_rev( `AfC band` ) )
    ,cols = vars( `Care setting` )
    ,labeller = label_wrap_gen( 15 )
    ) +
  #xlim( x_axis_range ) +
  ylim( y_axis_range ) +
  labs(
    x = "Joiner : Leaver rate ratio"
    ,y = "Stability index"
    ,title =
      stringr::str_wrap(
        paste0(
          " Stability index and Joiner:Leaver rate ratio for the largest* professions"
          ," in 2025."
        )
        ,55
      )
    ,subtitle =
      stringr::str_wrap(
        paste0(
          "Bands 9 and 8d are excluded because of a lack of joiners and leavers."
        )
        ,80
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
          ,"\nJoiner:Leaver rate ratio = # people joined during the year / # people left during the year."
        )
        ,100
      )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,strip.text.x = element_text( size = 10 )
    ,strip.text.y = element_text( size = 10, angle = 0, hjust = -1 )
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__SIvsJLrateRatio__top5.png"
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
      "Plots/Paper 1/plot__Joiner_v_Leaver_rate__top5.png"
    )
  ,dpi = 300
  ,width = 30
  ,height = 30
  ,units = "cm"
)

# ----
