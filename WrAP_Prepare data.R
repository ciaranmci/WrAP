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
ls_churn <-
  lapply(
    X = ls_churn
    ,FUN = 
      function( list_element )
      {
        processed_list_element <-
          list_element %>%
          dplyr::mutate(
            `Stability index` = 
              dplyr::if_else( `Stability index` == ".", NA ,`Stability index` )
            ,`Stability index` = as.numeric( `Stability index` )
          )
        
      }
    
  )

# Deprivation data.
# None

# Rurality data.
# ## The rurality data is by local authority district (LAD) in 2021 (LAD21CD). I
# ## need to find out the LAD21CD for all the Trusts. The best I could do was to
# ## get the LAD22CD for 2022 and map it to postcodes from 2021. I then need to
# ## match that with postcodes of the Trusts.
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
  )

# Trust-size data.
# None

# ----

####################
## Join datasets. ##
####################
# ----
ls_churn <-
  lapply(
    X = ls_churn
    ,FUN = 
      function( list_element )
      {
        processed_list_element <-
          # Join with deprivation dataset.
          list_element %>%
          dplyr::left_join(
            df_deprivation
            ,by = join_by(
              `Organisation name` == `Trust Name`
            )
          ) %>%
          # Join with rurality dataset.
          dplyr::left_join(
            df_ons_rurality %>% dplyr::select( -ladnm )
            ,by = join_by(
              `Org code` == `Trust code`
            )
          ) %>%
          # Join with Trust-size dataset.
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
          )
      }
  )
# ----