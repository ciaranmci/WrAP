
# Make functions that calculates the quantile.
# ----
ordbeta_quantile <- function(mu, phi, cutpoints, tau = 0.5) {
  
  # stopifnot(
  #   all(mu > 0 & mu < 1),
  #   all(phi > 0),
  #   length(cutpoints) == 2,
  #   cutpoints[1] < cutpoints[2]
  # )
  
  eta <- qlogis(mu)
  
  p0 <- 1 - plogis(eta - cutpoints[1])
  pc <- plogis(eta - cutpoints[1]) -
    plogis(eta - cutpoints[2])
  p1 <- plogis(eta - cutpoints[2])
  
  q <- numeric(length(mu))
  
  ## Point mass at zero
  i0 <- tau <= p0
  q[i0] <- 0
  
  ## Continuous beta component
  ic <- !i0 & tau <= p0 + pc
  
  beta_prob <- (tau - p0[ic]) / pc[ic]
  
  q[ic] <- qbeta(
    beta_prob,
    shape1 = mu[ic] * phi[ic],
    shape2 = (1 - mu[ic]) * phi[ic]
  )
  
  ## Point mass at one
  i1 <- tau > p0 + pc
  q[i1] <- 1
  
  return( q )
}
ordbeta_predict_quantile <- function( fit, newdata = fit$frame, tau = 0.5, re.form = ~0 ) {
  
  mu <- predict(
    fit,
    newdata = newdata,
    type = "response",
    re.form = re.form
  )
  
  phi <- predict(
    fit,
    newdata = newdata,
    type = "disp",
    re.form = re.form
  )
  
  cuts <- glmmTMB::family_params(fit)
  
  quantile_val <-
    ordbeta_quantile(
      mu = mu,
      phi = phi,
      cutpoints = cuts,
      tau = tau
    )
  
 output <-
    newdata[,attr( fit$modelInfo$terms$cond$fixed, "term.labels" )[1:2]]%>%
    as.data.frame() %>%
    dplyr::bind_cols( quantile = tau, quantile_val = quantile_val ) %>%
    dplyr::distinct()
  
  return( output )
}
# ----

# Fit models. #
# ----
# # Pay band
t1 <- Sys.time()
fit_payband <- glmmTMB::glmmTMB(
  stability_index ~ payband*Profession + (1 | orgcode)
  ,data = model_data_payband
  ,family = ordbeta()
)
Sys.time()-t1
# # Age band
t1 <- Sys.time()
fit_ageband <- glmmTMB::glmmTMB(
  stability_index ~ ageband*Profession + (1 | orgcode)
  ,data = model_data_ageband
  ,family = ordbeta()
)
Sys.time()-t1
# # Sex
t1 <- Sys.time()
fit_sex <- glmmTMB::glmmTMB(
  stability_index ~ sex*Profession + (1 | orgcode)
  ,data = model_data_sex
  ,family = ordbeta()
)
Sys.time()-t1
# # Ethnicity_binary
t1 <- Sys.time()
fit_ethnicity_binary <- glmmTMB::glmmTMB(
  stability_index ~ ethnicity_binary*Profession + (1 | orgcode)
  ,data = model_data_ethnicity
  ,family = ordbeta()
)
Sys.time()-t1
# ----

# Calculate the medians. #
# ----
medians_payband <- ordbeta_predict_quantile( fit_payband, tau = 0.5 )
medians_ageband <- ordbeta_predict_quantile( fit_ageband, tau = 0.5 )
medians_sex <- ordbeta_predict_quantile( fit = fit_sex, tau = 0.5 )
medians_ethnicity_binary <- ordbeta_predict_quantile( fit_ethnicity_binary, tau = 0.5 )
# ----

# Calculate the inter-quartile range. #
# ----
iqr_payband <-
  ordbeta_predict_quantile( fit_payband, tau = 0.75 ) %>%
    dplyr::left_join(
      ordbeta_predict_quantile( fit_payband, tau = 0.25 )
      ,by = join_by( Profession, payband )
      ,suffix = c( ".1stQTR", ".3rdQTR" )
    ) %>%
    dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
    dplyr::select( Profession, payband, iqr )
iqr_ageband <-
  ordbeta_predict_quantile( fit_ageband, tau = 0.75 ) %>%
  dplyr::left_join(
    ordbeta_predict_quantile( fit_ageband, tau = 0.25 )
    ,by = join_by( Profession, ageband )
    ,suffix = c( ".1stQTR", ".3rdQTR" )
  ) %>%
  dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
  dplyr::select( Profession, ageband, iqr )
iqr_sex <-
  ordbeta_predict_quantile( fit_sex, tau = 0.75 ) %>%
  dplyr::left_join(
    ordbeta_predict_quantile( fit_sex, tau = 0.25 )
    ,by = join_by( Profession, sex )
    ,suffix = c( ".1stQTR", ".3rdQTR" )
  ) %>%
  dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
  dplyr::select( Profession, sex, iqr )
iqr_ethnicity_binary <-
  ordbeta_predict_quantile( fit_ethnicity_binary, tau = 0.75 ) %>%
  dplyr::left_join(
    ordbeta_predict_quantile( fit_ethnicity_binary, tau = 0.25 )
    ,by = join_by( Profession, ethnicity_binary )
    ,suffix = c( ".1stQTR", ".3rdQTR" )
  ) %>%
  dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
  dplyr::select( Profession, ethnicity_binary, iqr )
# ----

# Calculate the differences. #
# ----
deltas_payband <-
  medians_payband %>%
  dplyr::left_join( iqr_payband ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate(
    comparator_val = lead( quantile_val )
    ,diff_median = comparator_val - quantile_val
    ,comparator_iqr = lead( iqr )
    ,diff_iqr = comparator_iqr - iqr
  )
deltas_ageband <-
  medians_ageband %>%
  dplyr::left_join( iqr_ageband ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate(
    comparator_val = lead( quantile_val )
    ,diff_median = comparator_val - quantile_val
    ,comparator_iqr = lead( iqr )
    ,diff_iqr = comparator_iqr - iqr
  )
deltas_sex <-
  medians_sex %>%
    dplyr::left_join( iqr_sex ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate(
    comparator_val = lead( quantile_val )
    ,diff_median = comparator_val - quantile_val
    ,comparator_iqr = lead( iqr )
    ,diff_iqr = comparator_iqr - iqr
  )
deltas_ethnicity_binary <-
  medians_ethnicity_binary %>%
  dplyr::left_join( iqr_ethnicity_binary ) %>%
  dplyr::group_by( Profession ) %>%
  dplyr::mutate(
    comparator_val = lead( quantile_val )
    ,diff_median = comparator_val - quantile_val
    ,comparator_iqr = lead( iqr )
    ,diff_iqr = comparator_iqr - iqr
  )
# ----


# Bootstrapped confidence intervals for difference in conditional medians.
# ----
bootstrap_delta_median <- function(
    fit
    ,newdata = fit$frame
    ,B = 2000
    ,seed = NULL
) {
  
  if( !is.null( seed ) ) { set.seed( seed ) }
  
  # Original data.
  dat <- model.frame( fit )
  response_name <- names( dat )[1]
  covariate_of_interest_name <- names( dat )[2]
  
  # Difference in quantile values.
  delta_q50 <- 
    ordbeta_predict_quantile( fit, tau = 0.5 ) %>%
    dplyr::group_by( Profession ) %>%
    dplyr::mutate(
      comparator_val = lead( quantile_val )
      ,diff_median = comparator_val - quantile_val
      )
  
  # Storage for bootstrap estimates.
  boot_delta <- list( )
  
  for( b in seq_len( B ) ) {
    
    # Simulate response from fitted model.
    y_boot <- simulate( fit )[[ 1 ]]
    
    # Create bootstrap dataset.
    dat_boot <- dat
    dat_boot[[ response_name ]] <- y_boot
    
    # Refit model.
    fit_boot <- try( update( fit, data = dat_boot ), silent = TRUE )
    
    # Handle failed fits.
    if( inherits( fit_boot, "try-error" ) ) {
      boot_delta[ b ] <- NA_real_
      next
    }
    
    # Calculate the quantile value.
    delta_q50_boot <- 
      ordbeta_predict_quantile( fit_boot, tau = 0.5 ) %>%
      dplyr::group_by( Profession ) %>%
      dplyr::mutate(
        comparator_val = lead( quantile_val )
        ,diff_median = comparator_val - quantile_val
      )
    
    # ~~~~~~~~
    # THE PROBLEM IS THAT I HAVE MANY CONTRASTS TO DO DO MY VALUE FOR `delta_q50_boot`
    # IS AN ENTIRE DATA.FRAME OBJECT (THOUGH I COULD PULL OUT THE `delta_q50_boot`
    # VALUES INTO A VECTOR). I THINK THE BEST OPTION IS TO DEFINE THE LENGTH OF 
    # `boot_delta` TO BE AS LONG AS `B*nrow(newdata)` INSTEAD OF `B`. THEN I
    # NEED TO DROP EACH BOOTSTRAPPED DATA.FRAME OBJECT INTO `boot_delta` IN THE
    # CORRECT ROWS. I WILL THEN COLUMN-BIND THE OTHER DATA FROM `newdata` TO
    # TRACK WHAT EACH VALUE OF `delta_iqr_boot` REFERS TO. ONLY THEN, I WILL
    # REMOVE THE `NA` ROWS THAT ARE THE FINAL REFERENCE LEVEL.
    # ~~~~~~~~
    # Drop values into the `boot_delta` object.
    boot_delta[ length( boot_delta ) + 1 ] <- delta_q50_boot
  }
  
  # Remove failed bootstrap replications.
  boot_delta <- boot_delta[ is.finite( boot_delta ) ]
  
  # 95% percentile bootstrap CI.
  ci <- quantile( boot_delta, probs = c( 0.025, 0.975 ) )
  
  # Return results.
  list(
    estimate = delta_q50
    ,conf_low = unname( ci[ 1 ] )
    ,conf_high = unname( ci[ 2 ] )
    ,bootstrap = boot_delta
    ,n_success = length( boot_delta )
    ,B = B
  )
}
median_diffs_payband <- bootstrap_delta_median( fit = fit_payband )
median_diffs_ageband <- bootstrap_delta_median( fit = fit_ageband )
median_diffs_sex <- bootstrap_delta_median( fit = fit_sex )
median_diffs_ethnicity_binary <- bootstrap_delta_median( fit = fit_ethnicity_binary )
# ----

#########################################################################
# Calculate the bootstrapped confidence intervals for the difference in #
# interquartile ranges.                                                 #
#########################################################################
# ----
bootstrap_delta_iqr <- function(
    fit
    ,newdata = fit$frame
    ,B = 2000
    ,seed = NULL
) {
  
  if( !is.null( seed ) ) { set.seed( seed ) }
  
  # Original data.
  dat <- model.frame( fit )
  response_name <- names( dat )[1]
  covariate_of_interest_name <- names( dat )[2]
  
  # Difference in IQRs.
  delta_iqr <-
    ordbeta_predict_quantile( fit, tau = 0.25 ) %>%
    dplyr::left_join(
      ordbeta_predict_quantile( fit, tau = 0.75 )
      ,by = join_by( Profession, !!sym( covariate_of_interest_name ) )
      ,suffix = c( ".1stQTR", ".3rdQTR" )
    ) %>%
    dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
    dplyr::select( Profession, !!sym( covariate_of_interest_name ), iqr )
  
  # Storage for bootstrap estimates.
  boot_delta <- numeric( B*nrow( newdata ) )
  
  for( b in seq_len( B ) ) {
    
    # Simulate response from fitted model.
    y_boot <- simulate( fit )[[ 1 ]]
    
    # Create bootstrap dataset by replacing the observed variate values
    # with values simulated using the observed covariate values.
    dat_boot <- dat
    dat_boot[[ response_name ]] <- y_boot
    
    # Refit model.
    fit_boot <- try( update( fit, data = dat_boot ), silent = TRUE )
    
    # Handle failed fits.
    if( inherits( fit_boot, "try-error" ) ) {
      boot_delta[ b ] <- NA_real_
      next
    }
    
    delta_iqr_boot <-
      ordbeta_predict_quantile( fit_boot, newdata = newdata, tau = 0.25 ) %>%
      dplyr::left_join(
        ordbeta_predict_quantile( fit_boot, newdata = newdata, tau = 0.75 )
        ,by = join_by( Profession, !!sym( covariate_of_interest_name ) )
        ,suffix = c( ".1stQTR", ".3rdQTR" )
      ) %>%
      dplyr::mutate( iqr = quantile_val.3rdQTR - quantile_val.1stQTR ) %>%
        dplyr::group_by( Profession ) %>%
        dplyr::mutate( delta_iqr = lead( iqr ) - iqr ) %>%
      dplyr::select( Profession, !!sym( covariate_of_interest_name ), delta_iqr )
    
    # ~~~~~~~~
    # THE PROBLEM IS THAT I HAVE MANY CONTRASTS TO DO DO MY VALUE FOR `delta_iqr_boot`
    # IS AN ENTIRE DATA.FRAME OBJECT (THOUGH I COULD PULL OUT THE `delta_iqr_boot`
    # VALUES INTO A VECTOR). I THINK THE BEST OPTION IS TO DEFINE THE LENGTH OF 
    # `boot_delta` TO BE AS LONG AS `B*nrow(newdata)` INSTEAD OF `B`. THEN I
    # NEED TO DROP EACH BOOTSTRAPPED DATA.FRAME OBJECT INTO `boot_delta` IN THE
    # CORRECT ROWS. I WILL THEN COLUMN-BIND THE OTHER DATA FROM `newdata` TO
    # TRACK WHAT EACH VALUE OF `delta_iqr_boot` REFERS TO. ONLY THEN, I WILL
    # REMOVE THE `NA` ROWS THAT ARE THE FINAL REFERENCE LEVEL.
    # ~~~~~~~~
    # Drop values into the `boot_delta` object.
    # boot_delta[b] <- delta_iqr_boot
  }
  
  # Remove failed bootstrap replications.
  boot_delta <- boot_delta[ is.finite( boot_delta ) ]
  
  # 95% percentile bootstrap CI.
  ci <- quantile( boot_delta, probs = c( 0.025, 0.975 ) )
  
  # Return results.
  list(
    estimate = delta_iqr
    ,iqr_1 = iqr[ 1 ]
    ,iqr_2 = iqr[ 2 ]
    ,conf_low = unname( ci[ 1 ] )
    ,conf_high = unname( ci[ 2 ] )
    ,bootstrap = boot_delta
    ,n_success = length( boot_delta )
    ,B = B
  )
}
t1 <- Sys.time()
iqr_diffs_payband <- bootstrap_delta_iqr( fit = fit_payband )
Sys.time() - t1
t1 <- Sys.time()
iqr_diffs_ageband <- bootstrap_delta_iqr( fit = fit_ageband )
Sys.time() - t1
t1 <- Sys.time()
iqr_diffs_sex <- bootstrap_delta_iqr( fit = fit_sex )
Sys.time() - t1
t1 <- Sys.time()
iqr_diffs_ethnicity_binary <- bootstrap_delta_iqr( fit = fit_ethnicity_binary )
Sys.time() - t1
# ----