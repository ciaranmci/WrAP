#########################################
## Correlation between SI and factors. ##
#########################################
# The purpose of this script is to calculate various correlation coefficients
# between the stability index and suggested factors.
#
# This script is called within `WrAP_Explore.R`.


# Extract dataset.
# ----
correlation_dataset <-
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    `Care setting` == "All care settings" 
    ,stringr::str_detect( string = `AfC band`, pattern = "All " )
    ,!is.na( `Stability index` )
    ,!is.na( `RUC21 settlement class` )
    ,!is.na( `IMD Score` )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
  ) %>%
  dplyr::select(
    year
    ,`Org code`
    ,`Stability index`
    ,`RUC21 settlement class`
    ,`IMD Score`
  )
# ----

# Correlation | IMD Score.
# ----
correlation_dataset %>%
  tidyr::nest( .by = year ) %>%
  dplyr::mutate(
    reg_p_value = lapply(
      data
      ,function(df)
        lm(
          `Stability index` ~ `IMD Score`
          ,data = df )  %>%
        summary() %>%
        broom::glance() %>%
        dplyr::pull( p.value )
    )
    ,reg_estimate = lapply(
      data
      ,function(df)
        lm(
          `Stability index` ~ `IMD Score`
          ,data = df )  %>%
        summary() %>%
        coef() %>%
        `[[`(2,1)
    )
    ,spearman_p_value = lapply(
      data
      ,function(df)
        cor.test(
          ~ `Stability index` + `IMD Score`
          ,method = "spearman"
          ,data = df
        )$p.value %>%
        unname() %>%
        suppressWarnings()
    )
    ,spearman_estimate = lapply(
      data
      ,function(df)
        cor.test(
          ~ `Stability index` + `IMD Score`
          ,method = "spearman"
          ,data = df
        )$estimate %>%
        unname() %>%
        suppressWarnings()
    )
  ) %>%
  unnest( c( reg_p_value, reg_estimate, spearman_p_value, spearman_estimate ) ) %>%
  dplyr::select( -data ) %>%
  write.csv( "Tables/correlations_IMD_by_year.csv" )
# ----

# Correlation | Rurality.
# ----
# ## ## Test models with an without factoring in rurality.
correlation_dataset %>%
  tidyr::nest( .by = year ) %>%
  dplyr::mutate(
    reg_p_value = lapply(
      data
      ,function(df)
        lm(
          `Stability index` ~ `RUC21 settlement class`
          ,data = df )  %>%
        summary() %>%
        broom::glance() %>%
        dplyr::pull( p.value )
    )
  ) %>%
  unnest( c( reg_p_value ) ) %>%
  dplyr::select( -data ) %>%
  write.csv( "Tables/correlations_rurality_by_year.csv" )
# ## ## Test model that simplifies to Rural -vs- Not rural.
correlation_dataset %>%
  dplyr::mutate(
    `RUC21 settlement class` = 
      dplyr::if_else(
        `RUC21 settlement class` == "Rural"
        ,`RUC21 settlement class`
        ,"Other"
      )
  ) %>%
  tidyr::nest( .by = year ) %>%
  dplyr::mutate(
    reg_p_value = lapply(
      data
      ,function(df)
        lm(
          `Stability index` ~ `RUC21 settlement class`
          ,data = df )  %>%
        summary() %>%
        broom::glance() %>%
        dplyr::pull( p.value )
    )
  ) %>%
  unnest( c( reg_p_value ) ) %>%
  dplyr::select( -data ) %>%
  write.csv( "Tables/correlations_rurality_simplified_by_year.csv" )
# ----