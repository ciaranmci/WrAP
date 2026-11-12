# WrAP_Compute_contrasts.r
#
# The purpose of this script is to compute the desired contrasts from the
# posterior distribution of the models.
#



#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  brms
  ,tidyverse
)

# ----

##########################
## Quantile calculator. ##
##########################
# ----
# Define function that calculate the distributions `quantile`-th value based on
# distributional parameters.
# # The function is applicable to zero-one-inflated beta models fitted in `brms`
# # and to ordered beta models fitted in `ordbetareg`.
func__quantile_value_from_cdf <-
  function(
    quantile = NULL # A vector of quantiles or single quantile for which the 
    ,mu      = NULL # The `mu`, i.e. mean, parameter from a beta regression model.
    # It is expected to be given on the scale of the linear
    # predictor for an ordered beta regression model, i.e. not as
    # a probability, but expected to be on the response scale
    # for a standard `brms` model. 
    ,phi     = NULL # The `phi`, i.e. precision, parameter from a beta regression model.
    # It is expected to be given on the scale of distributional
    # parameters. In other words, it is note expected to have 
    # been modelled by a linear predictor, in which case it will
    # have been outputted by the `brms` on the log scale.
    ,zoi     = NULL # The `zoi` parameter from a beta regression model. It is
    # expected to be given on the scale of the response scale
    # i.e. a probability.
    ,coi     = NULL # The `coi` parameter from a beta regression model. It is
    # expected to be given on the scale of the response scale
    # i.e. a probability.
    ,cutzero = NULL # The `cutzero` parameter from an ordered beta regression model.
    # It is expected to be given on the scale of the linear
    # predictor for an ordered beta regression model.
    ,cutone  = NULL # The `cutone` parameter from an ordered beta regression model.
    # It is expected to be given on the scale of the linear
    # predictor for an ordered beta regression model.
  )
  {
    
    # Check arguments.
    if( is.null( quantile ) ) stop( "A `quantile` value must be provided." )
    if( is.null( mu ) ) stop( "A `mu` value must be provided, and it is assumed to be on the scale of the linear predictor." )
    if( is.null( phi ) ) stop( "A `phi` value must be provided, and it is assumed to be on the distributional parameter scale." )
    if( is.null( zoi ) & is.null( cutzero ) ) stop( "Either `zoi` or `cutzero` must be provided." )
    if( is.null( coi ) & is.null( cutone ) ) stop( "Either `coi` or `cutone` must be provided. " )
    if( !is.null( zoi ) & !is.null( cutzero ) ) stop( "Only one of either `zoi` or `cutzero` can be provided." )
    if( !is.null( coi ) & !is.null( cutone ) ) stop( "Only one of either `coi` or `cutone` can be provided." )
    
    # Create output storage.
    output <- numeric()
    
    # Calculate the median.
    if( is.null( zoi ) & is.null( coi ) ) 
    {
      # Assume `ordbetareg` model, and assume `mu`, `cutzero` and `cutone` are
      # on the scale of the linear predictor, i.e. logit.
      # The probabilities assigned to three components of the model are:
      # - The probability that the Stability Index is 0% =
      #     p0 = 1 − plogis( mu - cutzero )
      # - The probability that the Stability Index is 100% =
      #     p1 = plogis( mu - cutzero + exp( cutone ) )
      # - The probability that the Stability Index is between 0% and 100% =
      #     p_between = plogis( mu - cutzero ) - p1
      #
      # The probabilities sum to 1. Note that these probabilities are not the 
      # values of the Stability Index itself.
      p0 = 1 - plogis( mu - cutzero )
      p1 = plogis( mu - ( cutzero + exp( cutone ) ) )
      p_beta_portion =  1 - p0 - p1
      
      # If the requested quantile falls within the portion of the cumulative
      # distribution function that is allocated to 0 (i.e. the `p0` =
      # P(Y = 0) component ), then the value at the requested quantile must
      # Y = 0.
      idx_zero_wins <- p0 >= quantile
      output[ idx_zero_wins ] <- 0
      
      # If the requested quantile falls within the portion of the cumulative
      # distribution function that is allocated to 1 (i.e. the `p1` =
      # P(Y = 1) component ), then the value at the requested quantile must
      # Y = 1.
      idx_one_wins <- ( p0 + p_beta_portion ) < quantile
      output[ idx_one_wins ] <- 1
      
      # If the requested quantile falls within the beta-distributed 
      # portion of the cumulative distribution function (i.e. the
      # `p_beta_portion` = P(0 < Y < 1) component ), then the value at
      # the requested quantile can be calculated from the quantile 
      # function of the beta distribution, given its shape parameters.
      idx_beta_wins <- !(idx_zero_wins | idx_one_wins)
      # Note that the requested probability is shifted by the amount
      # of probability already taken by the zero-component of the 
      # ordered beta model, `p0`, and then scaled to be a proportion
      # of the probability spectrum that is allocated to the beta
      # distribution, i.e. `p_beta_portion`.
      # Note also that the `ordbetareg::ordbetareg()` function outputs
      # `mu` on the scale of the linear predictor (i.e. logit scale)
      # (The `cutzero` and `cutone` parameters are also outputted on
      # the logit scale, which is why we had to transform them when
      # calculating `p0` and `p1`.)
      vals_beta_win<-
        qbeta(    
          p = ( quantile - p0 ) / p_beta_portion
          ,shape1 = plogis( mu ) * phi
          ,shape2 = ( 1 - plogis( mu ) ) * phi
        )
      output[ idx_beta_wins ] <- vals_beta_win[ idx_beta_wins ]
      
    } else {
      # Assume `brms` model, and assume `mu`, `zoi` and `coi` are on the
      # response scale rather than the scale of the linear predictor, i.e. logit.
      # `zoi` is the probability of an observation being either 0 or 1, and
      # `coi` is the conditional probability of a 1 given that the observation
      # is one of the inflated values (i.e. 0 or 1). So, we use the chain rule
      # to calculate the probability of 0, `p0`, and of 1, `p1`.
      p0 <- zoi * ( 1 - coi )
      p1 <- zoi * coi
      p_beta_portion <- 1 - zoi
      
      # If the requested quantile falls within the portion of the cumulative
      # distribution function that is allocated to 0 (i.e. the `p0` =
      # P(Y = 0) component ), then the value at the requested quantile must
      # Y = 0.
      idx_zero_wins <- p0 >= quantile
      output[ idx_zero_wins ] <- 0
      
      # If the requested quantile falls within the portion of the cumulative
      # probability distribution that is allocated to 1 (i.e. the `p1` =
      # P(Y = 1) component ), then the value at the requested quantile must
      # Y = 1.
      idx_one_wins <- ( p0 + p_beta_portion ) < quantile
      output[ idx_one_wins ] <- 1
      
      # If the requested quantile falls within the beta-distributed 
      # portion of the cumulative distribution function (i.e. the
      # `p_beta_portion` = P(0 < Y < 1) component ), then the value at
      # the requested quantile can be calculated from the quantile 
      # function of the beta distribution, given its shape parameters.
      idx_beta_wins <- !(idx_zero_wins | idx_one_wins)
      # Note that the requested probability is shifted by the amount
      # of probability already taken by the zero-component of the 
      # model, `p0`, and then scaled to be a proportion
      # of the probability spectrum that is allocated to the beta
      # distribution, i.e. `p_beta_portion`.
      vals_beta_win<-
        qbeta(
          p = ( quantile - p0 ) / p_beta_portion
          ,shape1 = mu * phi
          ,shape2 = ( 1 - mu ) * phi
        )
      output[ idx_beta_wins ] <- vals_beta_win[ idx_beta_wins ]
      
    }
    
    
    return( output )
  }
# ----

###########################
## Contrasts calculator. ##
###########################
# ----
# Define function that calculates the contrasts between levels of the covariate.
# # The hard-coded contrasts are the difference in medians and the difference
# # in interquartile ranges.
# # Future versions of this function might incorporated weighted medians and
# # weighted interquartile ranges.
fnc__compute_contrasts <-
  function(
    model = NULL
    ,weighting_data = NULL
    ,over
    )
  {
    # Check `model` argument.
    if( is.null( model ) ) stop( "Model object was not provided to `model` argument." )
    
    # Set the data object by extracting it from the model object.
    df <- tibble::as_tibble( model$data )
    
    # Check `weighting_data` argument.
    if( is.null( weighting_data ) )
      {
      message( "No weighting data were provided so equals weights are assumed." )
      weighting_data <-
        df %>%
        dplyr::distinct( orgcode ) %>%
        dplyr::mutate( model_weighting = 1 )
    }
    
    # Get the name of the covariate.
    covar_name <-
      df %>%
      dplyr::select(
        -c(
          orgcode, Profession, stability_index
          ,contains("_")
          )
        ,contains("binary")
        ) %>%
      names()
    message( paste0( "\nProcessing `", covar_name, "`...\n" ) )
    
    # Drop levels from any factors.
    df <- dplyr::mutate( df, across( where( is.factor ), droplevels ) )
    
    # Set the scenarios for which we want draws.
    my_newdata <-
      tidyr::expand_grid(
        orgcode = unique( df$orgcode )
        ,Profession = unique( df$Profession )
        ,covariate = unique( df[ , sym( covar_name ) ] )[[1]]
      )
    colnames( my_newdata ) <- c( "orgcode", "Profession",  covar_name )
    
    # Draw posterior distributional parameters.
    t1 <- Sys.time()
    # # Setting `re_formula = NULL` retains organisation-specific random effects.
    # tryCatch(
    #   expr = {
        name_of_covar_posterior_draws_var <- paste0( "posterior_draws_", covar_name )
      #   assign(
      #     "posterior_draws"
      #     ,readRDS(
      #       paste0(
      #         "Processed datasets/Paper 1/"
      #         ,name_of_covar_posterior_draws_var
      #         ,".RDS"
      #         )
      #       )
      #     )
      #   message(
      #     paste0(
      #       "`"
      #       ,name_of_covar_posterior_draws_var
      #       ,"` data are already available in storage."
      #     )
      #   )
      # }
      # ,warning = function(w) {
      #   
      #   if( overwrite )
      #   {
      #     message(
      #       paste0(
      #         "`"
      #         ,name_of_covar_posterior_draws_var
      #         ,"` data are not available in storage so it is being created."
      #         )
      #     )
          posterior_draws <-
            tidybayes::linpred_draws(
              object = model
              ,newdata = my_newdata
              ,dpar = c("mu", "phi", "cutzero", "cutone")
              ,re_formula = NULL
              ,transform = TRUE
              ,ndraws = 1000
            ) %>%
            dplyr::select(
              .draw
              ,.row
              ,orgcode
              ,Profession
              ,all_of( covar_name )
              ,mu
              ,phi
              ,cutzero
              ,cutone
            ) %>%
            dplyr::left_join( weighting_data, by = "orgcode" )
          # # Save posterior draws because it takes a long time to get them.
          saveRDS(
            posterior_draws
            ,paste0( "Processed datasets/Paper 1/posterior_draws_", covar_name, ".RDS" )
            )
      #   } else {
      #     warning(
      #       paste0(
      #         "`"
      #         ,name_of_covar_posterior_draws_var
      #         ,"` data are not available in storage, and `overwrite` has been set to FALSE."
      #       )
      #     )
      #     }
      # }
    #   
    # )
    
    
    # Calculate the marginal posterior medians and IQRs using our user-defined 
    # function.
    posterior_draws <-
      posterior_draws %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        
        q25 =
          func__quantile_value_from_cdf(
            quantile = 0.25
            ,mu = mu
            ,phi = phi
            ,cutzero = cutzero
            ,cutone = cutone
          )

        ,median =
          func__quantile_value_from_cdf(
            quantile = 0.50
            ,mu = mu
            ,phi = phi
            ,cutzero = cutzero
            ,cutone = cutone
          )
        
        ,q75 =
          func__quantile_value_from_cdf(
            quantile = 0.75
            ,mu = mu
            ,phi = phi
            ,cutzero = cutzero
            ,cutone = cutone
          )
        
        ,iqr = q75 - q25
      )
    message(
      "Posterior draws took: "
      ,round( difftime( Sys.time(),  t1, units = "mins" ), 2 )
      ," minutes"
    )
    # # Update-save posterior_draws. 
    saveRDS(
      posterior_draws
      ,paste0( "Processed datasets/Paper 1/posterior_draws_", covar_name, ".RDS" )
      )
    
    
    # Calculate the posterior contrasts.
    t1 <- Sys.time()
    if( !!sym( covar_name ) != "ethnicity" )
    {
      posterior_contrasts <-
        posterior_draws %>%
        tidyr::drop_na() %>%
        dplyr::arrange( .draw, orgcode, Profession, .row, !!sym( covar_name ) ) %>% 
        dplyr::group_by( .draw, orgcode, Profession ) %>% 
        dplyr::mutate(
          difference_of_medians = lead( median ) - median
          ,difference_of_iqrs = lead( iqr ) - iqr
        ) %>% 
        dplyr::ungroup() %>%
        tidyr::drop_na()
    } else {
      
      # Specify the contrasts of interest.
      ethnicity_contrasts_of_interest <-
        tidyr::expand_grid(
          ethnicity1 = unique( df$ethnicity )
          ,ethnicity2 = unique( df$ethnicity )
        ) %>%
        dplyr::filter( ethnicity1 != ethnicity2 ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          sorted_concat =
            paste( sort( c( ethnicity1, ethnicity2 ) ), collapse = ", " )
          ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct( sorted_concat ) %>%
        tidyr::separate_wider_delim(
          cols = sorted_concat
          ,delim = ", "
          ,names = c( "ethnicity1", "ethnicity2" )
          ) %>%
        dplyr::arrange( ethnicity1, ethnicity2 )
      
      for( i_profession in unique( posterior_draws$Profession ) )
      {
        # Extract the data subset of interest.
        data_subset <-
          posterior_draws %>%
          dplyr::filter( Profession == i_profession )
        # Explicate the contrasts of interest and calculate them.
        assign(
          paste0(
            "posterior_contrasts_ethnicity_"
            ,i_profession %>%
              stringr::str_replace_all( pattern= "/", replacement = "_" ) %>%
              stringr::str_replace_all( pattern= " ", replacement = "." )
            )
          ,ethnicity_contrasts_of_interest %>%
            dplyr::left_join(
              data_subset
              ,by = join_by( ethnicity1 == ethnicity )
              ,relationship = "many-to-many"
            ) %>%
            dplyr::left_join(
              data_subset
              ,by = join_by( .draw, orgcode, Profession, ethnicity2 == ethnicity )
              ,suffix = c( ".ethnicity1", ".ethnicity2" )
            ) %>%
            dplyr::group_by( .draw, orgcode, Profession ) %>% 
            dplyr::mutate(
              difference_of_medians = median.ethnicity1 - median.ethnicity2
              ,difference_of_iqrs = iqr.ethnicity1 - iqr.ethnicity2
            ) %>%
            dplyr::ungroup() %>%
            tidyr::drop_na() %>%
            dplyr::select(
              -c( "mu.ethnicity2", "phi.ethnicity2", "cutzero.ethnicity2", "cutone.ethnicity2"
                  ,"model_weighting.ethnicity2", ".row.ethnicity2" )
            ) %>%
            dplyr::rename(
              .row = .row.ethnicity1
              ,mu = mu.ethnicity1
              ,phi = phi.ethnicity1
              ,cutzero = cutzero.ethnicity1
              ,cutone = cutone.ethnicity1
              ,model_weighting = model_weighting.ethnicity1
              )
          ) %>%
        # Save contrasts for this profession. 
        saveRDS(
          paste0(
            "Processed datasets/Paper 1/posterior_contrasts_ethnicity_"
            ,i_profession %>%
              stringr::str_replace_all( pattern= "/", replacement = "_" ) %>%
              stringr::str_replace_all( pattern= " ", replacement = "." )
            , ".RDS"
            )
        )
      } # End of FOR LOOP of professions for contrasts.
      
    } # End of IF statement for contrasts.
    message(
      "Calculating posterior contrasts took: "
      ,round( difftime( Sys.time(),  t1, units = "mins" ), 2 )
      ," minutes"
    )
    
    # Provide summaries of the contrasts for inference.
    # # The 95% highest-density interval of differences is used. If it contains zero
    # # then we infer that there is no difference between the observed levels of the
    # # covariate.
    t1 <- Sys.time()
    if( !!sym( covar_name ) != "ethnicity" )
    {
      posterior_summaries <-
        posterior_contrasts %>%
        dplyr::reframe(
          .by = c( Profession, !!sym( covar_name ) )
          ,median_diff_in_medians = median( difference_of_medians, na.rm = TRUE )
          ,hdi_low_diff_in_medians = bayestestR::hdi( difference_of_medians )$CI_low
          ,hdi_high_diff_in_medians = bayestestR::hdi( difference_of_medians )$CI_high
          ,median_diff_in_iqr = median( difference_of_iqrs, na.rm = TRUE )
          ,hdi_low_diff_in_iqr = bayestestR::hdi( difference_of_iqrs )$CI_low
          ,hdi_high_diff_in_iqr = bayestestR::hdi( difference_of_iqrs )$CI_high
        ) %>%
        dplyr::mutate(
          is_median_diff = !( hdi_low_diff_in_medians <= 0 & hdi_high_diff_in_medians >= 0 )
          ,is_iqr_diff =  !( hdi_low_diff_in_iqr <= 0 & hdi_high_diff_in_iqr >= 0 )
        ) %>%
        dplyr::arrange( Profession, !!sym( covar_name ) ) %>%
        dplyr::relocate( is_median_diff, .after = !!sym( covar_name ) ) %>%
        dplyr::relocate( is_iqr_diff, .after = is_median_diff )
      
      # # Save posterior summaries 
      saveRDS(
        posterior_summaries
        ,paste0( "Processed datasets/Paper 1/posterior_summaries_", covar_name, ".RDS" )
      )
      
    } else {
      
      # Define storage.
      posterior_summaries_ethnicity <-
        data.frame(
          Profession = character()
          ,ethnicity1 = character()
          ,ethnicity2 = character()
          ,is_median_diff = logical()
          ,is_iqr_diff = logcal()
          ,median_diff_in_medians = numeric()
          ,hdi_low_diff_in_medians = numeric()
          ,hdi_high_diff_in_medians = numeric()
          ,median_diff_in_iqr = numeric()
          ,hdi_low_diff_in_iqr = numeric()
          ,hdi_high_diff_in_iqr = numeric()
          )
      
      # Calculate summaries.
      for( i_profession in unique( posterior_draws$Profession ) )
      {
        posterior_summaries_ethnicity <-
          get(
            paste0(
              "posterior_contrasts_ethnicity_"
              ,i_profession %>%
                stringr::str_replace_all( pattern= "/", replacement = "_" ) %>%
                stringr::str_replace_all( pattern= " ", replacement = "." )
              )
            ) %>%
            dplyr::reframe(
              .by = c( Profession, ethnicity1, ethnicity2 )
              ,median_diff_in_medians = median( difference_of_medians, na.rm = TRUE )
              ,hdi_low_diff_in_medians = bayestestR::hdi( difference_of_medians )$CI_low
              ,hdi_high_diff_in_medians = bayestestR::hdi( difference_of_medians )$CI_high
              ,median_diff_in_iqr = median( difference_of_iqrs, na.rm = TRUE )
              ,hdi_low_diff_in_iqr = bayestestR::hdi( difference_of_iqrs )$CI_low
              ,hdi_high_diff_in_iqr = bayestestR::hdi( difference_of_iqrs )$CI_high
            ) %>% 
            dplyr::mutate(
              is_median_diff = !( hdi_low_diff_in_medians <= 0 & hdi_high_diff_in_medians >= 0 )
              ,is_iqr_diff =  !( hdi_low_diff_in_iqr <= 0 & hdi_high_diff_in_iqr >= 0 )
            ) %>%
            dplyr::arrange( Profession , ethnicity1, ethnicity2 ) %>%
            dplyr::relocate( is_median_diff, .after = ethnicity2 ) %>%
            dplyr::relocate( is_iqr_diff, .after = is_median_diff ) %>%
          dplyr::bind_rows( posterior_summaries_ethnicity )
      } # End of FOR LOOP of professions for summaries.
      
      # Save summaries.
      saveRDS(
        posterior_summaries_ethnicity
        ,"Processed datasets/Paper 1/posterior_summaries_ethnicity.RDS"
        )
    } # End of IF statement for summaries.
    message(
      "Calculating posterior summaries took: "
      ,round( difftime( Sys.time(),  t1, units = "mins" ), 2 )
      ," minutes"
    )
    
    } # End of function definition.
# ----

##################
## Load models. ##
##################
# ----
if( !exists( "model_payband_nonMon" ) )
{ model_payband_nonMon <- readRDS( "Models/Paper 1/model_payband_nonMon.RDS" ) }
if( !exists( "model_ageband_nonMon" ) )
{ model_ageband_nonMon <- readRDS( "Models/Paper 1/model_ageband_nonMon.RDS" ) }
if( !exists( "model_sex" ) )
{ model_sex <- readRDS( "Models/Paper 1/model_sex.RDS" ) }
if( !exists( "model_ethnicity" ) )
{ model_ethnicity <- readRDS( "Models/Paper 1/model_ethnicity.RDS" ) }
if( !exists( "model_ethnicity_all_ethnicities_nonMon" ) )
{ model_ethnicity_all_ethnicities_nonMon <- readRDS( "Models/Paper 1/model_ethnicity_all_ethnicities_nonMon.RDS" ) }
# ----


#############################################
## Run the function for each model fitted. ##
#############################################
# ----
fnc__compute_contrasts( model = model_payband_nonMon )
fnc__compute_contrasts( model = model_ageband_nonMon )
fnc__compute_contrasts( model = model_sex )
fnc__compute_contrasts( model = model_ethnicity )
fnc__compute_contrasts( model = model_ethnicity_all_ethnicities_nonMon )
# ----