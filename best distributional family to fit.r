
# best distributional family to fit.r
#
# I will follow the process of Andrew Wheiss to fit my models:
# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
#
# Andrew has another blog about fitting zero-one inflated beta models:
# https://talks.andrewheiss.com/2024-11-13_udem-beyond-ols/regression-zoib.html
# and one about zero inflated beta:
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/
# and one on ordered beta, which is a single model version of a  zero-one 
# inflated model:
# https://stats.andrewheiss.com/compassionate-clam/notebook/ordbeta.html
# The ordered beta seems much more appropriate because it combines the "hurdles"
# into a single model that is parameterised with the same parameters.
# A key bit about interpreting the distributional parameters of an ordered beta
# model is that `cutzero` and `cutone` are the estimates of the two thresholds
# near 0 and 1 that bound the beta distribution between them. These are on the 
# logit scale. and , where cutzero = and cutone is on a transformed scale such that .
# 
# I need to have run some sections of WrAP_manuscript_figuresTablesTests.R so
# that I get `payband_test_data`.
#
# Something to note about the beta distribution is how it responded to its
# parameters:
# - equal values make a symmetrical distribution about 0.5.
# - if both parameters are equal, then:
#     - - values of 1 give a flat distribution.
#     - - values < 1 give a centre denser than the tails.
#     - - values > 1 give tails denser than the centre.
# - a larger shape parameter (first parameter) shifts the density higher,
#   e.g. curve( dbeta( x, shape1 = 6, shape2 = 2 ), from = 0, to = 1 ) gives
#   a left-skewed unimodal.
# - a larger scale parameter (second parameter) shifts the density lower,
#   e.g. curve( dbeta( x, shape1 = 2, shape2 = 6 ), from = 0, to = 1 ) gives
#   a left-skewed unimodal.
# - the larger the values of the parameters, the narrower the peaks, e.g.
#   compare curve( dbeta( x, shape1 = 5, shape2 = 5 ), from = 0, to = 1 ) to
#   curve( dbeta( x, shape1 = 50, shape2 = 50 ), from = 0, to = 1 ) and compare
#   curve( dbeta( x, shape1 = 0.50, shape2 = 0.50 ), from = 0, to = 1 ) to
#   curve( dbeta( x, shape1 = 0.05, shape2 = 0.05 ), from = 0, to = 1 )
#
# Interestingly, the `brms` package parameterises the beta distribution with
# phi rather than alpha and beta. The mu-phi parameterisation is a mean-precision
# parameterisation. The mean, mu, is (alpha / (alpha+beta)) and precision (or
# dispersion), phi, is (alpha + beta). This suits my needs quite well, actually,
# because I want to test the location and scale of the `Stability index`. Larger
# values of phi indicate less variance, so we would expect to see this for the 
# white ethnicity. I would need to include a formula for phi to have a covariate
# for ethnicity. Would I just fit an intercept model?

# More libraries.
pacman::p_load(
  scales
  ,patchwork
  ,ggh4x
  ,ggtext
  ,MetBrewer
  )  

# Dataset.
df <-
  payband_data %>%
  dplyr::mutate( stability_index2 = 1 - `Stability index` ) %>%
  dplyr::rename( orgcode = `Org code`, payband = `AfC band` )



# Plot the distribution.
plot_dist_unlogged <- gapminder %>% 
  mutate(gdpPercap = ifelse(is_zero, -0.1, gdpPercap)) %>% 
  ggplot(aes(x = gdpPercap)) +
  geom_histogram(aes(fill = is_zero), binwidth = 5000, 
                 boundary = 0, color = "white") +
  geom_vline(xintercept = 0) + 
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c(clrs[4], clrs[1]), 
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "GDP per capita", y = "Count", fill = "Is zero?",
       subtitle = "Nice and exponentially shaped, with a bunch of zeros") +
  theme_nice() +
  theme(legend.position = "bottom")

plot_dist_logged <- gapminder %>% 
  mutate(log_gdpPercap = ifelse(is_zero, -0.1, log_gdpPercap)) %>% 
  ggplot(aes(x = log_gdpPercap)) +
  geom_histogram(aes(fill = is_zero), binwidth = 0.5, 
                 boundary = 0, color = "white") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = label_math(e^.x)) +
  scale_fill_manual(values = c(clrs[4], clrs[1]), 
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "GDP per capita", y = "Count", fill = "Is zero?",
       subtitle = "Nice and normally shaped, with a bunch of zeros;\nit's hard to interpret intuitively though") +
  theme_nice() +
  theme(legend.position = "bottom")

(plot_dist_unlogged | plot_dist_logged) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "GDP per capita, original vs. logged",
                  theme = theme(plot.title = element_text(family = "Jost", face = "bold"),
                                legend.position = "bottom"))

# Model 1:
# Ignore
brms::brm(
  formula = 
    brms::bf(
      
      # Formula for the location parameter.
      # # Set `stability_index2` as the variate, and truncate its
      # # distribution.
      stability_index2  ~
        # # Set the population-level covariate.
        payband +
        # # Set the group-level covariate, correlated with the
        # # scale group-level intercept for the scale parameter.
        ( 1 | ID1 | orgcode )
      
     
    )
  
  ,family = brms::asym_laplace()
  
  # Specify the dataset.
  ,data = df
)







zinb <- read.csv("https://paul-buerkner.github.io/data/fish.csv")
fit_zinb1 <- brm(count ~ persons + child + camper,
                 data = zinb, family = zero_inflated_poisson())
fit_beta1 <- brm(Proportion ~ Grade
                 ,data = Data, family = Beta())
fit_beta2 <- brm(bf(Proportion ~ Grade, phi ~ Grade)
                 ,data = Data, family = Beta())
fit_beta3 <- brm(bf(Proportion ~ 1, phi ~ Grade)
                 ,data = Data, family = Beta())
fit_beta4 <- brm(bf(Proportion ~ 1, phi ~ Grade)
                 ,data = Data, family = zero_one_inflated_beta())



# SYNTAX AFTER INTERACTING WITH CHATGPT.
# It

# Packages --------------------------------------------------------------------

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)


# Simulate fake data.
org_size <- floor( abs( rnorm( n = 10, 0, 1 ) ) * 100 )
org_size_weighting <- org_size / sum( org_size )
my_data <-
  data.frame(
    variate = 
      c(
        rbeta(n = 50, shape1 = 6, shape2 = 3)
        ,rep(0,10)
        ,rep(1,40)
      )
    ,covariate =  c( rep(0, 30), rep(1, 30), rep(2, 40) )
    ,grp = factor( rep( 1:10, 10 ) )
    ,org_size_weighting = rep( org_size_weighting, 10 )
  )

# Fit zero-one-inflated beta model.
t1 <- Sys.time()
fit <- 
  brms::brm(
    formula =
      brms::bf(
        variate ~ mo( covariate ) + ( 1 | grp )
        ,phi ~  mo( covariate )
        ,zoi ~  mo( covariate )
        ,coi ~  mo( covariate )
      )
    ,data = my_data
    ,threads = 5
    ,backend = "cmdstanr"
    ,family = brms::zero_one_inflated_beta()
    )
Sys.time() - t1

# Define a function for calculating the quantile of the cumulative distribution
# function given the parameters of the zero-one-inflated beta model.
zoib_quantile <-
  function( prob, mu, phi, zoi, coi )
    {
      # Calculate the probability for the 0 and 1 point masses.
      # # `zoi` is the probability of an observation being either 0 or 1, and
      # # `coi` is the conditional probability of a 1 given that the observation
      # # is one of the inflated values (i.e. 0 or 1). So, we use the chain rule
      # # to calculate the probability of 0, `p0`, and of 1, `p1`.
      p0 <- zoi * ( 1 - coi )
      p1 <- zoi * coi
      
      # Calculate the quantile of the given probability. 
      # # Note that the two shape parameters are intended to be alpha and beta,
      # # i.e. location and scale, but the `brms` regression is parameterised 
      # # with `mu`, the conditional mean, and `phi`, the concentration.
      output <-
        stats::qbeta(
          p = ( prob - p0 ) / ( 1 - p0 - p1 )
          ,shape1 = mu * phi,
          ,shape2 = ( 1 - mu ) * phi
        ) %>% 
        suppressWarnings()
      
      # Make amends for probabilities that are covered by the 0 and 1 point
      # masses.
      # # If the desired probability is less than or equal to the probability of 
      # # a 0, then the the value from the cumulative distribution function will 
      # # be zero because the point mass at 0 has swallowed up all probabilities
      # # up to its probability, `p0`. Until `prob` is greater than `p0`, we 
      # # don't enter the beta-distributed part of the model.
      # # If the desired probability is greater than or equal to the inverse 
      # # probability of a 1, then the the value from the cumulative distribution
      # # function will be zero because the point mass at 1 has swallowed up all
      # # probabilities beyond its probability, `p1`. Unless `prob` is less than
      # # `1 - p1`, we don't enter the beta-distributed part of the model.
      output[ prob <= p0 ] <- 0
      output[ prob >= ( 1 - p1 ) ] <- 1
    
      return( output )
  }

# Set the data for the contrasts that I want to undertake.
my_newdata <-
  tibble::tibble(
    covariate = c(0, 1)
  )

# Draw posteriors of each distributional parameter.
# # Note that by setting `re_formula = NA`, we are asking to marginalise over
# # the "random effect" term (i.e. the grouping variable).
posterior_draws <-
  tidybayes::linpred_draws(
    fit
    ,newdata = my_newdata
    ,dpar = c( "mu", "phi", "zoi", "coi" )
    ,re_formula = NA
    ,transform = TRUE
  ) %>%
  dplyr::select( -c( .chain, .iteration, .linpred ) )

# Compute posterior medians and IQRs.
summaries <-
  posterior_draws %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    qtr1 = zoib_quantile( 0.25, mu, phi, zoi, coi )
    ,median = zoib_quantile( 0.5, mu, phi, zoi, coi )
    ,qtr3 = zoib_quantile( 0.75, mu, phi, zoi, coi )
    ,iqr = qtr3 - qtr1
  ) %>%
  dplyr::ungroup()

# Compute posterior contrasts.
contrasts <-
  summaries %>%
  tidyr::pivot_wider(
    id_cols =  .draw
    ,names_from = covariate
    ,values_from = c( median, iqr )
  ) %>% 
   dplyr::mutate(
     delta_median = median_1 - median_0
     ,delta_iqr = iqr_1 - iqr_0
  )

# Calculate the posterior summaries.
posterior_summaries <-
  contrasts %>%
  dplyr::summarise(

    median_est = mean( delta_median )
    ,median_l95  = stats::quantile( delta_median, 0.025 )
    ,median_u95  = stats::quantile( delta_median, 0.975 )

    ,iqr_est = mean( delta_iqr ),
    ,iqr_l95  = stats::quantile( delta_iqr, 0.025 )
    ,iqr_u95  = stats::quantile( delta_iqr, 0.975 )

  )

print( posterior_summaries )



#################
#################
#################
# TRIAL 2

# Step 0: Simulate appropriate model data and `newdata`.
org_size <- floor( abs( rnorm( n = 10, 0, 1 ) ) * 100 )
org_size_weighting <- org_size / sum( org_size )
my_data <-
  data.frame(
    variate = 
      c(
        rbeta(n = 50, shape1 = 6, shape2 = 3)
        ,rep(0,10)
        ,rep(1,40)
      )
    ,covariate =  c( rep(0, 30), rep(1, 30), rep(2, 40) )
    ,grp = factor( rep( 1:10, 10 ) )
    ,org_size_weighting = rep( org_size_weighting, 10 )
  )
my_newdata <-
  tibble::tibble(
    covariate = rep( c(0, 1), length( my_data$grp ) )
    ,grp = rep( my_data$grp, 2 )
  )

# Step 1: obtain mu, phi, zoi and coi for every observed organisation using re_formula = NULL.
posterior_draws <-
  tidybayes::linpred_draws(
    fit
    ,newdata = my_newdata
    ,dpar = c( "mu", "phi", "zoi", "coi" )
    ,re_formula = NULL
    ,transform = TRUE
  ) %>%
  dplyr::select( -c( .chain, .iteration, .linpred ) )

# Step 2: join the organisation sizes to compute weights.
posterior_draws <-
  posterior_draws %>%
  dplyr::left_join(
    my_data %>% dplyr::distinct( grp, org_size_weighting )
    ,by = join_by( grp )
    ,relationship = "many-to-one"
    )

# Step 3: define the weighted CDF.
# ?

# Step 4: invert F with stats::uniroot() (handling the endpoint masses explicitly);
zoib_quantile <-
  function( prob, mu, phi, zoi, coi )
  {
    # Calculate the probability for the 0 and 1 point masses.
    # # `zoi` is the probability of an observation being either 0 or 1, and
    # # `coi` is the conditional probability of a 1 given that the observation
    # # is one of the inflated values (i.e. 0 or 1). So, we use the chain rule
    # # to calculate the probability of 0, `p0`, and of 1, `p1`.
    p0 <- zoi * ( 1 - coi )
    p1 <- zoi * coi
    
    # Calculate the quantile of the given probability.
    # ?
    output <-
      stats::uniroot( ... )
    
    # Handle the endpoint masses explicitly.
    # # If the desired probability is less than or equal to the probability of 
    # # a 0, then the the value from the cumulative distribution function will 
    # # be zero because the point mass at 0 has swallowed up all probabilities
    # # up to its probability, `p0`. Until `prob` is greater than `p0`, we 
    # # don't enter the beta-distributed part of the model.
    # # If the desired probability is greater than or equal to the inverse 
    # # probability of a 1, then the the value from the cumulative distribution
    # # function will be zero because the point mass at 1 has swallowed up all
    # # probabilities beyond its probability, `p1`. Unless `prob` is less than
    # # `1 - p1`, we don't enter the beta-distributed part of the model.
    output[ prob <= p0 ] <- 0
    output[ prob >= ( 1 - p1 ) ] <- 1
    
    return( output )
  }

# Step 5: repeat for the required quantiles (0.25, 0.5 and 0.75)
summaries <-
  posterior_draws %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    qtr1 = zoib_quantile( 0.25, mu, phi, zoi, coi )
    ,median = zoib_quantile( 0.5, mu, phi, zoi, coi )
    ,qtr3 = zoib_quantile( 0.75, mu, phi, zoi, coi )
    ,iqr = qtr3 - qtr1
  ) %>%
  dplyr::ungroup()

# Step 6: compute posterior contrasts.
contrasts <-
  summaries %>%
  tidyr::pivot_wider(
    id_cols =  .draw
    ,names_from = covariate
    ,values_from = c( median, iqr )
  ) %>% 
  dplyr::mutate(
    delta_median = median_1 - median_0
    ,delta_iqr = iqr_1 - iqr_0
  )
posterior_summaries <-
  contrasts %>%
  dplyr::summarise(
    
    median_est = mean( delta_median )
    ,median_l95  = stats::quantile( delta_median, 0.025 )
    ,median_u95  = stats::quantile( delta_median, 0.975 )
    
    ,iqr_est = mean( delta_iqr ),
    ,iqr_l95  = stats::quantile( delta_iqr, 0.025 )
    ,iqr_u95  = stats::quantile( delta_iqr, 0.975 )
    
  )


###############
###############
###############
# TRIAL 3 - chatgpt

# Step 0: Simulate appropriate model data and `newdata`.

org_size <-
  floor(
    abs(
      rnorm(
        n = 10,
        mean = 0,
        sd = 1
      )
    ) * 100
  )

org_size_weighting <-
  org_size / sum(org_size)

my_data <-
  data.frame(
    variate =
      c(
        rbeta(
          n = 50,
          shape1 = 6,
          shape2 = 3
        ),
        rep(0, 10),
        rep(1, 40)
      ),
    covariate =
      c(
        rep(0, 30),
        rep(1, 30),
        rep(2, 40)
      ),
    grp =
      factor(
        rep(1:10, 10)
      ),
    org_size_weighting =
      rep(
        org_size_weighting,
        10
      )
  )


# Fit model -------------------------------------------------------------------
t1<-Sys.time()
fit <-
  brms::brm(
    formula =
      brms::bf(
        variate ~ mo(covariate) + (1 | grp),
        phi ~ mo(covariate),
        zoi ~ mo(covariate),
        coi ~ mo(covariate)
      ),
    data = my_data,
    threads = 5,
    backend = "cmdstanr",
    family = brms::zero_one_inflated_beta()
  )
(model_fit_duration <- Sys.time()-t1)

# Step 1: Define the prediction data.
# One row per observed organisation and covariate value.

my_newdata <-
  tidyr::expand_grid(
    payband = c("Band 8a", "Band 8b")
    ,orgcode = df_of_interest$orgcode
  )


# Step 2: Draw posterior distributional parameters.
# `re_formula = NULL` retains organisation-specific random effects.

# I need to add the weighting based on how many staff members contributed to 
# each pay band in each Trust.
df_of_interest <-
  df_of_interest %>%
  dplyr::reframe(
    .by = orgcode
    ,org_n_contribution = sum(`Denominator at start of period`)
  ) %>%
  dplyr::mutate(
    model_weighting =
      org_n_contribution / sum(org_n_contribution)
  ) %>%
  dplyr::right_join(
    by = join_by( orgcode )
    ,df_of_interest
  ) 

posterior_draws <-
  tidybayes::linpred_draws(
    fit,
    newdata = my_newdata,
    dpar = c("mu", "phi", "zoi", "coi"),
    re_formula = NULL,
    transform = TRUE
  ) |>
  dplyr::select(
    .draw
    ,payband
    ,orgcode
    ,mu
    ,phi
    ,zoi
    ,coi
  ) |>
  dplyr::left_join(
    df_of_interest |>
      dplyr::distinct(
        orgcode,
        model_weighting
      ),
    by = "orgcode"
  )


# Step 3: Define the ZOIB CDF for a single distribution.

zoib_cdf <-
  function(
    y,
    mu,
    phi,
    zoi,
    coi
  ){
    
    p0 <-
      zoi * (1 - coi)
    
    p1 <-
      zoi * coi
    
    output <-
      numeric(length(y))
    
    output[y < 0] <-
      0
    
    output[y == 0] <-
      p0
    
    inside <-
      y > 0 & y < 1
    
    output[inside] <-
      p0[inside] +
      (1 - p0[inside] - p1[inside]) *
      stats::pbeta(
        y[inside],
        shape1 = mu[inside] * phi[inside],
        shape2 = (1 - mu[inside]) * phi[inside]
      )
    
    output[y >= 1] <-
      1
    
    output
    
  }


# Step 4: Define the quantile of a weighted mixture of ZOIB distributions.

weighted_zoib_quantile <-
  function(
    prob,
    mu,
    phi,
    zoi,
    coi,
    weights
  ){
    
    weighted_cdf <-
      function(y){
        
        sum(
          weights *
            zoib_cdf(
              y,
              mu,
              phi,
              zoi,
              coi
            )
        )
        
      }
    
    
    # Check the point masses at zero and one.
    
    p0 <-
      sum(
        weights *
          zoi *
          (1 - coi)
      )
    
    p1 <-
      sum(
        weights *
          zoi *
          coi
      )
    
    
    if(prob <= p0){
      
      return(0)
      
    }
    
    
    if(prob >= (1 - p1)){
      
      return(1)
      
    }
    
    
    stats::uniroot(
      f =
        function(y){
          
          weighted_cdf(y) - prob
          
        },
      interval = c(0, 1)
    )$root
    
  }


# Step 5: Compute marginal posterior medians and IQRs.
# The quantile is calculated after weighting organisations.

summaries <-
  posterior_draws |>
  dplyr::group_by(
    .draw,
    payband
  ) |>
  dplyr::group_modify(
    ~{
      
      tibble::tibble(
        
        q25 =
          weighted_zoib_quantile(
            prob = 0.25,
            mu = .x$mu,
            phi = .x$phi,
            zoi = .x$zoi,
            coi = .x$coi,
            weights = .x$model_weighting
          ),
        
        median =
          weighted_zoib_quantile(
            prob = 0.50,
            mu = .x$mu,
            phi = .x$phi,
            zoi = .x$zoi,
            coi = .x$coi,
            weights = .x$model_weighting
          ),
        
        q75 =
          weighted_zoib_quantile(
            prob = 0.75,
            mu = .x$mu,
            phi = .x$phi,
            zoi = .x$zoi,
            coi = .x$coi,
            weights = .x$model_weighting
          )
        
      ) |>
        dplyr::mutate(
          iqr = q75 - q25
        )
      
    }
  ) |>
  dplyr::ungroup()


# Step 6: Compute posterior contrasts.

contrasts <-
  summaries %>%
  tidyr::pivot_wider(
    id_cols = .draw,
    names_from = payband,
    values_from = c( median, iqr )
  ) %>%
  dplyr::mutate(
    delta_median = .[[3]] - .[[2]]
    ,delta_iqr = .[[4]] - .[[3]]
  )


# Step 7: Posterior summaries.
posterior_summaries <-
  contrasts |>
  dplyr::summarise(
    
    median_est =
      mean(delta_median),
    
    median_l95 =
      stats::quantile(
        delta_median,
        0.025
      ),
    
    median_u95 =
      stats::quantile(
        delta_median,
        0.975
      ),
    
    iqr_est =
      mean(delta_iqr),
    
    iqr_l95 =
      stats::quantile(
        delta_iqr,
        0.025
      ),
    
    iqr_u95 =
      stats::quantile(
        delta_iqr,
        0.975
      )
    
  )

print(posterior_summaries)


# Fit an actual model overnight.
df_of_interest$payband <- ordered( df_of_interest$payband )

t1<-Sys.time()
trial_fit <-
  brms::brm(
    formula =
      brms::bf(
        stability_index ~ mo( payband ) + ( 1 | orgcode )
        ,phi ~ mo( payband )
        ,zoi ~ mo( payband )
        ,coi ~ mo( payband )
      )
    ,data = df_of_interest
    ,threads = 5
    ,backend = "cmdstanr"
    ,family = brms::zero_one_inflated_beta()
    ,prior = c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
               set_prior("normal(0, 1)", class = "b"))
    ,iter = 4000
    ,init = 0
    ,control =
      list(
        adapt_delta = 0.97
        ,max_treedepth = 12
        )
  )
(model_fit_duration <- Sys.time()-t1)

t1<-Sys.time()
trial_fit2 <-
  brms::brm(
    formula =
      brms::bf(
        stability_index ~ mo( payband ) + ( 1 | orgcode )
        ,phi ~ 1
        ,zoi ~ 1
        ,coi ~ 1
      )
    ,data = df_of_interest
    ,threads = 5
    ,backend = "cmdstanr"
    ,family = brms::zero_one_inflated_beta()
    ,prior = c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
               set_prior("normal(0, 1)", class = "b"))
    ,iter = 4000
    ,init = 0
    ,control =
      list(
        adapt_delta = 0.97
        ,max_treedepth = 12
      )
  )
(model_fit_duration <- Sys.time()-t1)

t1<-Sys.time()
trial_fit3 <-
  ordbetareg::ordbetareg(
    formula =
      brms::bf(
        stability_index ~ mo( payband ) + ( 1 | orgcode )
        ,cutzero ~ mo( payband )
        ,cutone ~ mo( payband )
      )
    ,data = df_of_interest
    ,threads = 5
    ,backend = "cmdstanr"
    ,manual_prior =
      set_prior( "student_t(3, 0, 2.5)", class = "Intercept" ) +
      set_prior( "normal(0, 1)", class = "b") +
      set_prior( "normal(0,5)", class = "b", dpar = "cutone" ) +
      set_prior( "normal(0,5)", class = "b", dpar = "cutzero" )
    ,iter = 5000
    ,init = 0
    ,control =
      list(
        adapt_delta = 0.97
        ,max_treedepth = 12
      )
  )
(model_fit_duration <- Sys.time()-t1)


t1<-Sys.time()
trial_fit4 <-
  ordbetareg::ordbetareg(
    formula =
      brms::bf(
        stability_index ~ mo( payband ) + ( 1 | orgcode )
        ,cutzero ~ 1
        ,cutone ~ 1
      )
    ,data = df_of_interest
    ,threads = 5
    ,backend = "cmdstanr"
    ,manual_prior =
      set_prior( "student_t(3, 0, 2.5)", class = "Intercept" ) +
      set_prior( "normal(0, 1)", class = "b")# +
      # set_prior( "normal(0,5)", class = "b", dpar = "cutone" ) +
      # set_prior( "normal(0,5)", class = "b", dpar = "cutzero" )
    ,iter = 5000
    ,init = 0
    ,control =
      list(
        adapt_delta = 0.97
        ,max_treedepth = 12
      )
  )
(model_fit_duration <- Sys.time()-t1)


# The expected different in medians is about 0.2.
df_of_interest %>% dplyr::reframe(.by = payband, med = median( stability_index ) )


