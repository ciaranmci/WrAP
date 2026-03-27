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
    year_end
    ,`Org code`
    ,`Stability index`
    ,`RUC21 settlement class`
    ,`IMD Score`
    ,starts_with( "ps_")
    ,starts_with( "ss_" )
  )
# ----

# Make vector of years that will define each data set.
# ----
years <-
  dplyr::distinct( correlation_dataset, year_end ) %>%
  as.matrix()
# ----

# Write a function to extract what is needed.
# ----
fnc__extract_correlations <-
  function(
    data = NULL # The `correlation_dataset` object.
    ,q_names = NULL # The `q_names` object.
    ,col_str = NULL # A character string specifying the substring to look for.
    ,check_cor = TRUE # A logical indicating whether or not to also calculate
                      # the Spearman's Rho.
  )
  {
    
    # Check arguments.
    if( is.null( data ) ){ stop( "The 'data' argument was not supplied." ) }
    if( !is.data.frame( data ) ){ stop( "The 'data' object is not a data.frame." ) }
    if( is.null( q_names ) ){ stop( "The 'q_names' argument was not supplied." ) }
    if( is.null( col_str ) ){ stop( "The 'col_str' argument was not supplied." ) }
    
    # Do the work.
    out <- 
      dplyr::bind_cols(
        q_names
        
        ,data %>%
          dplyr::select( contains( col_str ) ) %>%
          colnames() %>%
          rlang::syms() %>%
          map(
            ~rlang::expr(
              lm(
                `Stability index` ~ !!.x
                ,data = data
              )
            )
          ) %>%
          map( eval.parent ) %>%
          map( broom::tidy ) %>%
          bind_rows() %>%
          filter( term != "(Intercept)" ) %>%
          dplyr::select( term, estimate, p.value ) %>%
          dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
        
        ,if( check_cor )
        {
          data %>%
            dplyr::select( contains( col_str ) ) %>%
            colnames() %>%
            rlang::syms() %>%
            map(
              ~rlang::expr(
                cor.test(
                  ~ `Stability index` + !!.x
                  ,method = "spearman"
                  ,data = data
                )
              )
            ) %>%
            map( eval.parent ) %>%
            map( broom::tidy ) %>%
            bind_rows() %>%
            dplyr::select( estimate, p.value ) %>%
            dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
            suppressWarnings()
        } else {
            NULL
          }
      )
    
    return( out )
  }
# ----
  
# Correlation | IMD Score.
# ----
q_names <- "IMD score"
col_str <- "IMD score"

# Make data sets.
data1 <- correlation_dataset %>% dplyr::filter( year_end == years[1] )
data2 <- correlation_dataset %>% dplyr::filter( year_end == years[2] )
data3 <- correlation_dataset %>% dplyr::filter( year_end == years[3] )

# Build the output tables.
dplyr::bind_cols(
  
  years %>% as.data.frame() %>% `colnames<-`( "Year" )
  
  ,dplyr::bind_rows(
    dplyr::bind_cols(
      data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
    
    ,dplyr::bind_cols(
      data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
    
    ,dplyr::bind_cols(
      data3 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data3
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
      
      ,data3 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data3
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
  )
) %>%
  write.csv( "Tables/correlations_IMD_by_year.csv" )
# ----

# ----

# Correlation | Rurality.
# ----
q_names <- "RUC21 settlement class"
col_str <- "RUC21 settlement class"

# Make data sets.
data1 <- correlation_dataset %>% dplyr::filter( year_end == years[1] )
data2 <- correlation_dataset %>% dplyr::filter( year_end == years[2] )
data3 <- correlation_dataset %>% dplyr::filter( year_end == years[3] )
correlation_dataset_ruralitySimplified <-
  correlation_dataset %>%
  dplyr::mutate(
    `RUC21 settlement class` = 
      dplyr::if_else(
        `RUC21 settlement class` == "Rural"
        ,`RUC21 settlement class`
        ,"Other"
      )
  )
data1_ruralitySimplified <-
  correlation_dataset_ruralitySimplified %>% dplyr::filter( year_end == years[1] )
data2_ruralitySimplified <-
  correlation_dataset_ruralitySimplified %>% dplyr::filter( year_end == years[2] )
data3_ruralitySimplified <-
  correlation_dataset_ruralitySimplified %>% dplyr::filter( year_end == years[3] )

# Build the output tables.
dplyr::bind_cols(
  
  rep( years, each = 3 ) %>%
    as.data.frame() %>%
    `colnames<-`( "Year" )
  
  ,dplyr::bind_rows(
    data1 %>%
    dplyr::select( contains( col_str ) ) %>%
    colnames() %>%
    rlang::syms() %>%
    map(
      ~rlang::expr(
        lm(
          `Stability index` ~ !!.x
          ,data = data1
        )
      )
    ) %>%
    map( eval.parent ) %>%
    map( broom::tidy ) %>%
    bind_rows() %>%
    filter( term != "(Intercept)" ) %>%
    dplyr::select( term, estimate, p.value ) %>%
    dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
    
    ,data2 %>%
      dplyr::select( contains( col_str ) ) %>%
      colnames() %>%
      rlang::syms() %>%
      map(
        ~rlang::expr(
          lm(
            `Stability index` ~ !!.x
            ,data = data2
          )
        )
      ) %>%
      map( eval.parent ) %>%
      map( broom::tidy ) %>%
      bind_rows() %>%
      filter( term != "(Intercept)" ) %>%
      dplyr::select( term, estimate, p.value ) %>%
      dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
    
    ,data3 %>%
      dplyr::select( contains( col_str ) ) %>%
      colnames() %>%
      rlang::syms() %>%
      map(
        ~rlang::expr(
          lm(
            `Stability index` ~ !!.x
            ,data = data3
          )
        )
      ) %>%
      map( eval.parent ) %>%
      map( broom::tidy ) %>%
      bind_rows() %>%
      filter( term != "(Intercept)" ) %>%
      dplyr::select( term, estimate, p.value ) %>%
      dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
  )
) %>%
  write.csv( "Tables/correlations_rurality_by_year.csv" )

dplyr::bind_cols(
  
  years %>% as.data.frame() %>% `colnames<-`( "Year" )
  
  ,dplyr::bind_rows(
    data1_ruralitySimplified %>%
      dplyr::select( contains( col_str ) ) %>%
      colnames() %>%
      rlang::syms() %>%
      map(
        ~rlang::expr(
          lm(
            `Stability index` ~ !!.x
            ,data = data1_ruralitySimplified
          )
        )
      ) %>%
      map( eval.parent ) %>%
      map( broom::tidy ) %>%
      bind_rows() %>%
      filter( term != "(Intercept)" ) %>%
      dplyr::select( term, estimate, p.value ) %>%
      dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
    
    ,data2_ruralitySimplified %>%
      dplyr::select( contains( col_str ) ) %>%
      colnames() %>%
      rlang::syms() %>%
      map(
        ~rlang::expr(
          lm(
            `Stability index` ~ !!.x
            ,data = data2_ruralitySimplified
          )
        )
      ) %>%
      map( eval.parent ) %>%
      map( broom::tidy ) %>%
      bind_rows() %>%
      filter( term != "(Intercept)" ) %>%
      dplyr::select( term, estimate, p.value ) %>%
      dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
    
    ,data3_ruralitySimplified %>%
      dplyr::select( contains( col_str ) ) %>%
      colnames() %>%
      rlang::syms() %>%
      map(
        ~rlang::expr(
          lm(
            `Stability index` ~ !!.x
            ,data = data3_ruralitySimplified
          )
        )
      ) %>%
      map( eval.parent ) %>%
      map( broom::tidy ) %>%
      bind_rows() %>%
      filter( term != "(Intercept)" ) %>%
      dplyr::select( term, estimate, p.value ) %>%
      dplyr::rename( reg_estimate = estimate, reg_p_value = p.value ) 
  )
) %>%
  write.csv( "Tables/correlations_rurality_simplified_by_year.csv" )
# ----

# Correlation | Staff survey.
# ----
# Help to select the columns.
col_str <- "ss_"
# Make column of questions names.
q_names <-
  correlation_dataset %>%
    dplyr::select( starts_with( col_str ) ) %>%
    colnames() %>%
    stringr::str_sub( start = -3, end = -1) %>%
    as.data.frame() %>%
    `colnames<-`( "Question" ) 

# Make data sets.
# ## I would prefer to define these in-line, but the scoping of R variables means
# ## that the `data` argument is not seen within the `purrr::map()` function
# ## that I use within my user-defined function.
data1 <- correlation_dataset %>% dplyr::filter( year_end == years[1] )
data2 <- correlation_dataset %>% dplyr::filter( year_end == years[2] )

# Build the output table.
dplyr::bind_cols(
  
  rep( years[1:2], each = nrow( q_names ) ) %>%
    as.data.frame() %>%
    `colnames<-`( "Year" )
  
  ,dplyr::bind_rows(
    dplyr::bind_cols(
      q_names
      
      ,data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
    ,dplyr::bind_cols(
      q_names
      
      ,data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
  ) %>%
    dplyr::select( -1 )
) #%>%
write.csv( "Tables/correlations_staff_survey_by_year.csv" )
# ----

# Correlation | Patient satisfaction.
# ----
# Help to select the columns.
col_str <- "ps_"
# Make column of questions names.
q_names <-
  correlation_dataset %>%
  dplyr::select( starts_with( col_str ) ) %>%
  colnames() %>%
  stringr::word( start = 2, sep = "_") %>%
  as.data.frame() %>%
  `colnames<-`( "Question" ) 

# Apply the function to all datasets and then combine.
# Build the output table.
dplyr::bind_cols(
  
  rep( years[1:2], each = nrow( q_names ) ) %>%
    as.data.frame() %>%
    `colnames<-`( "Year" )
  
  ,dplyr::bind_rows(
    dplyr::bind_cols(
      q_names
      
      ,data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data1 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data1
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
    ,dplyr::bind_cols(
      q_names
      
      ,data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            lm(
              `Stability index` ~ !!.x
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        filter( term != "(Intercept)" ) %>%
        dplyr::select( term, estimate, p.value ) %>%
        dplyr::rename( reg_estimate = estimate, reg_p_value = p.value )
      
      ,data2 %>%
        dplyr::select( contains( col_str ) ) %>%
        colnames() %>%
        rlang::syms() %>%
        map(
          ~rlang::expr(
            cor.test(
              ~ `Stability index` + !!.x
              ,method = "spearman"
              ,data = data2
            )
          )
        ) %>%
        map( eval.parent ) %>%
        map( broom::tidy ) %>%
        bind_rows() %>%
        dplyr::select( estimate, p.value ) %>%
        dplyr::rename( spearman_estimate = estimate, spearman_p_value = p.value ) %>%
        suppressWarnings()
    )
  ) %>%
    dplyr::select( -1 )
) %>%
write.csv( "Tables/correlations_patient_satisfaction_by_year.csv" )
# ----