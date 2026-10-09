# fit_models.r
#
# The purpose of this script is to fit the Bayesian multi-level zero-one-inflated
# beta models.
#

# # Set storage.
# ls_models <- list()

# Set reused arguments.
# # A note on parallelisation. The `brms` package uses Stan under the hood,
# # which allocates one chain to a core. You could simply parallelise the 
# # chains across cores. But you can also divide the data across threads, within
# # cores. This way, the per-row likelihood calculations for a given chain can be
# # parallelised across threads. The minimal `grainsize` is 100, which means that
# # no fewer than 100 rows can be allocated to a given thread. This is important
# # if you have a small dataset because the overheads of parallelising might
# # outweigh the benefit if you don't end up using many threads.
# # In terms of how many cores and threads to use, I'd go for the rule that the 
# # maximum number of cores to allocate should be two fewer cores that your
# # machine has. `brms` will only ever allocate one chain to a core so the 
# # number of cores you actually use will be the smaller of the number of chains
# # you specified and the number of cores you have (minus 2 for wiggle room).
# # Finally, `my_chains`*`my_threads` should not exceed `n_cores_available`.
# # This is how my arguments below are set up, relative to  automatically
# # determining how many cores you have.
my_trueBounds <- c( 0, 1 )
my_priors <-
  brms::set_prior( "student_t( 3, 0, 2.5 )", class = "Intercept" ) +
  brms::set_prior( "normal( 0, 5 )", class = "b" ) +
  brms::set_prior( "normal( 0, 5 )", class = "b", dpar = "cutone" ) +
  brms::set_prior( "normal( 0, 5 )", class = "b", dpar = "cutzero" )
my_controls <-
  list(
    adapt_delta = 0.97
    ,max_treedepth = 12
  )
my_chains <- 4 # Default is 4, which is fine.
n_cores_available <- parallel::detectCores() - 2 
my_cores <- min( my_chains, n_cores_available)
my_threads <- floor( n_cores_available / my_chains ) 
my_iter <- 5000
my_init <- "random"

########################
## The list approach. ## Can't get it to work.
########################
# ----
# Set formulae to loop through.
# ls_model_formulae <-
#   list(
    # payband_formula = 
    #   brms::bf(
    #     stability_index ~ mo( payband ) + Profession + ( 1 | orgcode )
    #     ,cutzero ~ mo( payband )
    #     ,cutone ~ mo( payband )
    #   )
    
    # ageband_formula =as.formula(
    #   brms::bf(
    #     stability_index ~ mo( ageband ) + Profession + ( 1 | orgcode )
    #     ,cutzero ~ mo( ageband )
    #     ,cutone ~ mo( ageband )
    #   )
    # )
    # Plots clearly show no relationship so no point in modelling it.
    # ,sex_formula = as.formula(
    #   brms::bf(
    #     stability_index ~ mo( ageband ) + Profession + ( 1 | orgcode )
    #     ,cutzero ~ mo( ageband )
    #     ,cutone ~ mo( ageband )
    #   )
    # )
    # ,ethnicity_formula = as.formula(
    #   brms::bf(
    #     stability_index ~ ethnicity_binary + Profession + ( 1 | orgcode )
    #     ,cutzero ~ ethnicity_binary
    #     ,cutone ~ ethnicity_binary
    #   )
    # )
    # Plots clearly show no relationship so no point in modelling it.
    # ,deprivation_formula = as.formula(
    #   brms::bf(
    #     stability_index ~ deprivation + Profession + ( 1 | orgcode )
    #     ,cutzero ~ deprivation
    #     ,cutone ~ deprivation
    #   )
    # )
    # Plots clearly show no relationship so no point in modelling it.
    # ,rurality_formula = as.formula(
    #   brms::bf(
    #     stability_index ~ mo( rurality ) + Profession + ( 1 | orgcode )
    #     ,cutzero ~ mo( rurality )
    #     ,cutone ~ mo( rurality )
    #   )
    # )
    # Plots clearly show no relationship so no point in modelling it.
    # ,vacancy_formula = as.formula(
    #   brms::bf(
    #     stability_index ~ vacancy + Profession + ( 1 | orgcode )
    #     ,cutzero ~ vacancy
    #     ,cutone ~ vacancy
    #   )
    # )
  # )

# Set list of data.frames to loop through.
# ls_model_datasets <-
#   list(
    # model_data_payband
    # model_data_ageband
    #,model_data_sex # Plots clearly show no relationship so no point in modelling it.
    # ,model_data_ethnicity
    #,model_data_deprivation # Plots clearly show no relationship so no point in modelling it.
    #,model_data_rurality # Plots clearly show no relationship so no point in modelling it.
    #,model_data_vacancy # Plots clearly show no relationship so no point in modelling it.
  # )

# # Check that there are as many formulae as data sets.
# if( length( ls_model_formulae ) != length( ls_model_datasets ) )
# {
#   message("Length of `model_formulae` does not equal the length of `model_datasets`.")
# }
# 
# # Fit models in a loop.
# for( i_model in 1:length( ls_model_formulae ) )
# {
#   
#   # Set model name.
#   mod_name <-
#     paste0(
#       "model_"
#       ,unlist( strsplit( names( ls_model_formulae )[[ i_model ]], "_" ) )[1]
#     )
#   # Send message to modeller.
#   message( paste0( "\nStarting to fit `", mod_name, "`.") )
#   
#   # Fit and assign model.
#   assign(
#     mod_name
#     ,ordbetareg::ordbetareg(
#       formula = ls_model_formulae[ i_model ]
#       ,data = ls_model_datasets[[ i_model ]]
#       ,true_bounds = my_trueBounds
#       ,cores = my_cores
#       ,threads = my_threads
#       ,backend = "cmdstanr"
#       ,manual_prior = my_priors
#       ,iter = my_iter
#       ,init = my_init
#       ,control = my_controls
#     )
#   )
#   
#   # Add model-fit statistics to the model object.
#   assign(
#     mod_name
#     ,add_criterion( get(mod_name), c("loo", "waic", "bayes_R2") )
#   )
#   
#   # # Save model because it take so long to fit.
#   # saveRDS( get( mod_name ), paste0( "Models/Paper 1/", mod_name, ".RDS" ) )
#   # 
#   # Save result.
#   if( i_model == 1)
#   {
#     ls_models <- get( mod_name ) 
#   } else {
#     ls_models[ nrow( ls_models ) + 1 ] <- get( mod_name )
#   }
#   
#   # Garbage collect before the next model.
#   gc()
#   
# }
# ----

#############################
## Unfurling the FOR loop. ##
#############################
# ----

# ordB models. 
model_payband <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ mo( payband )*Profession + ( 1 | orgcode )
      ,cutzero ~ mo( payband )*Profession
      ,cutone ~ mo( payband )*Profession
    )
    ,data = model_data_payband
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_payband, "Models/Paper 1/model_payband.RDS" )

model_ageband <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ mo( ageband )*Profession + ( 1 | orgcode )
      ,cutzero ~ mo( ageband )*Profession
      ,cutone ~ mo( ageband )*Profession
    )
    ,data = model_data_ageband
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_ageband, "Models/Paper 1/model_ageband.RDS" )

model_sex <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ sex*Profession + ( 1 | orgcode )
      ,cutzero ~ sex*Profession
      ,cutone ~ sex*Profession
    )
    ,data = model_data_sex
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_sex, "Models/Paper 1/model_sex.RDS" )

model_ethnicity <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ ethnicity_binary*Profession + ( 1 | orgcode )
      ,cutzero ~ ethnicity_binary*Profession
      ,cutone ~ ethnicity_binary*Profession
    )
    ,data = model_data_ethnicity
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_ethnicity, "Models/Paper 1/model_ethnicity.RDS" )

# nonMon models.
# # In previous models, I assume monotonic trends in the candidate factors. In
# # hindsight, it is clear from the plots of the data that the factors exhibit
# # non-monotonic trends in stability index, which I had confused with the
# # factors being monotonic for the phenomena that they represent. And so, below 
# # are specifications for "nonMon" model specification, i.e. the candidate
# # factors are not treated as if "their effects to be monotonic" (see 
# # doi: 10.1111/bmsp.12195).
model_payband_nonMon <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ payband*Profession + ( 1 | orgcode )
      ,cutzero ~ payband*Profession
      ,cutone ~ payband*Profession
    )
    ,data = model_data_payband
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  brms::add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_payband_nonMon, "Models/Paper 1/model_payband_nonMon.RDS" )

model_ageband_nonMon <-
  ordbetareg::ordbetareg(
    formula =  brms::bf(
      stability_index ~ ageband*Profession + ( 1 | orgcode )
      ,cutzero ~ ageband*Profession
      ,cutone ~ ageband*Profession
    )
    ,data = model_data_ageband
    ,true_bounds = my_trueBounds
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,manual_prior = my_priors
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
  ) %>%
  # Fit criteria are accessible via, for example, `<model name>$criteria$loo`.
  brms::add_criterion( c("loo", "waic", "bayes_R2") )
saveRDS( model_ageband_nonMon, "Models/Paper 1/model_ageband_nonMon.RDS" )



# ----




# When you plot the cut points for the zero and the one point masses,
# the ZOIB model (shown in blue) looks to do better than the ordB model
# (shown in red).
# The WAIC for the ZOIB also looks ever-so-slightly better but not
# meaningfully so.
payband_model_data %>%
  ggplot() +
  geom_histogram( aes( x = stability_index ) ) +
  # ZOIB model in red.
  geom_vline( aes( xintercept = plogis(-2.54) ), colour = "red" ) +
  geom_vline( aes( xintercept = plogis(1.67) ), colour = "red" ) +
  # ordB model in blue.
  geom_vline( aes( xintercept = plogis(-1.29) ), colour = "blue" ) +
  geom_vline( aes( xintercept = plogis(1.78) ), colour = "blue" ) +
  # ZOIB null in green.
  geom_vline( aes( xintercept = plogis(0.45) ), colour = "green" ) +
  geom_vline( aes( xintercept = plogis(0.92) ), colour = "green" ) 
waic(model_payband, model_payband_ZOIB, model_payband_ZOIB_null )
# I'm tempted to fit a null model for both ZOIB and ordB to see how they
# converge and how well they fit.
model_payband_ZOIB_NULL <-
  brms::brm(
    formula =
      brms::bf(
        stability_index ~ 1 + (1 | orgcode)
      )
    ,data = ls_model_datasets[[ i_model ]]
    ,cores = my_cores
    ,threads = my_threads
    ,backend = "cmdstanr"
    ,prior = set_prior( "student_t( 3, 0, 2.5 )", class = "Intercept" ) 
    ,iter = my_iter
    ,init = my_init
    ,control = my_controls
    ,family = brms::zero_one_inflated_beta()
  )