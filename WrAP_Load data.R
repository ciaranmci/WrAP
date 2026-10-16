# WrAP_Load data.R
#
# The purpose of this script is to load the data
# 
# The publicly-available data are:
#
# The files to be loaded are:
# 1. Turnover of Allied Health Professionals within the NHS as a whole, from 
#    March 2022 to March 2025.
#   - publicly available.
#   - URL = "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-nhs-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
# 2. Turnover of Allied Health Professionals within the NHS organisations, from 
#    March 2022 to March 2025.
#   - publicly available.
#   - URL = "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-organisation-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
#
# The outputs are 8 data.frame objects: 4 data.frames of data about churn from
# the NHS, and 4 data.frames of data about churn within NHS. The four data.frames
# refer to the breakdowns of churn by staff paygrade, sex, age-band, and ethnicity.
#

#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  haven
  ,tidyverse
)
# ----

####################
## Load web data. ##
####################
# ----
# NOTE ON TRUST SIZE:
# We can get the count of staff per Trust for every month. The churn dataset
# refers to March 2021 to March 2023 (inclusive?). Ideally, we should use all
# counts from all months of all years rather than choosing a particular month
# to represent the Trust. But this would require us to incorporate 24 counts
# per Trust. How we incorporate these data are a matter for the particular 
# analysis.
# In the meantime (2025 11 13), I will only incorporate the March counts from
# the three years.
#
tryCatch(
  expr = {
    df_churn_within_NHS_Grade <- readRDS( "Processed datasets/Paper 1/df_churn_within_NHS_Grade.RDS" )
    df_churn_within_NHS_Gender <- readRDS( "Processed datasets/Paper 1/df_churn_within_NHS_Gender.RDS" )
    df_churn_within_NHS_AgeBand <- readRDS( "Processed datasets/Paper 1/df_churn_within_NHS_AgeBand.RDS" )
    df_churn_within_NHS_EthnicGroup <- readRDS( "Processed datasets/Paper 1/df_churn_within_NHS_EthnicGroup.RDS" )
    
    message( "Turnover data are already available in storage." )
  }
  ,warning = function(w) {
    message( "Turnover data are not available in storage so it is being created." )
  
    # Set URLs.
    url_churn_from_NHS <- "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-nhs-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
    url_churn_within_NHS <- "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-organisation-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
    url_ons_rurality <- "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/ruralurbanclassifications/2021ruralurbanclassification/rucallsupplementarytables.xlsx"
    url_postcodeToLADCD <- "https://www.arcgis.com/sharing/rest/content/items/bcfc75627db44bb4b0261f8578361954/data"
    url_Trust_size_2021_03 <- "https://files.digital.nhs.uk/C4/453C24/NHS%20Workforce%20Statistics%2C%20March%202021%20England%20and%20Organisation.xlsx"
    url_Trust_size_2022_03 <- "https://files.digital.nhs.uk/C3/488527/NHS%20Workforce%20Statistics%2C%20March%202022%20England%20and%20Organisation.xlsx"
    url_Trust_size_2023_03 <- "https://files.digital.nhs.uk/07/6F3BE5/NHS%20Workforce%20Statistics%2C%20March%202023%20England%20and%20Organisation.xlsx"
    # # Download files from URL.
    # curl::curl_download( url_churn_from_NHS, "xls_churn_from_NHS.xlsx" )
    # curl::curl_download( url_churn_within_NHS, "xls_churn_within_NHS.xlsx" )
    # curl::curl_download( url_ons_rurality, "xls_ons_rurality.xlsx" )
    # curl::curl_download( url = url_postcodeToLADCD, destfile = "csv_postcodeToLADCD.zip" )
    # curl::curl_download( url_Trust_size_2021_03, "xls_Trust_size_2021_03.xlsx" )
    # curl::curl_download( url_Trust_size_2022_03, "xls_Trust_size_2022_03.xlsx" )
    # curl::curl_download( url_Trust_size_2023_03, "xls_Trust_size_2023_03.xlsx" )
    # # Load files. Focus on head-count values ("HC") and exclude call handlers
    # # and paramedics.
    # # ## Churn from NHS.
    # df_churn_from_NHS_Grade <-
    #   readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Grade" ) %>%
    #   dplyr::filter( Type == 'HC' ) %>%
    #   dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    # df_churn_from_NHS_Gender <-
    #   readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Gender" ) %>%
    #   dplyr::filter( Type == 'HC' ) %>%
    #   dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    # df_churn_from_NHS_AgeBand <-
    #   readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Age band" ) %>%
    #   dplyr::filter( Type == 'HC' ) %>%
    #   dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    # df_churn_from_NHS_EthnicGroup <-
    #   readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Ethnic group" ) %>%
    #   dplyr::filter( Type == 'HC' ) %>%
    #   dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    # ## Churn within NHS.
    df_churn_within_NHS_Grade <-
      readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Grade" ) %>%
      dplyr::filter( Type == 'HC' ) %>%
      dplyr::select( -Type ) %>%
      dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    df_churn_within_NHS_Grade$`AfC band` <-
      factor(
        df_churn_within_NHS_Grade$`AfC band`
        ,levels = c( "Non AfC band", "Band 4", "Band 5", "Band 6", "Band 7"       
          ,"Band 8a", "Band 8b", "Band 8c", "Band 8d", "Band 9", "All AfC bands" )
      ) %>%
      ordered()
    df_churn_within_NHS_Gender <-
      readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Gender" ) %>%
      dplyr::filter( Type == 'HC' ) %>%
      dplyr::select( -Type ) %>%
      dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    df_churn_within_NHS_AgeBand <-
      readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Age band" ) %>%
      dplyr::filter( Type == 'HC' ) %>%
      dplyr::select( -Type ) %>%
      dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    df_churn_within_NHS_AgeBand$`Age band` <-
      factor(
        df_churn_within_NHS_AgeBand$`Age band`
        ,levels = c( "Under 25", "25 to 34", "35 to 44", "45 to 54", "55 to 64"
                     , "65 and over", "All age bands" )
      ) %>%
      ordered()
    df_churn_within_NHS_EthnicGroup <-
      readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Ethnic group" ) %>%
      dplyr::filter( Type == 'HC' ) %>%
      dplyr::select( -Type ) %>%
      dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
    
    # Save.
    saveRDS( df_churn_within_NHS_Grade, "Processed datasets/Paper 1/df_churn_within_NHS_Grade.RDS" )
    saveRDS( df_churn_within_NHS_Gender, "Processed datasets/Paper 1/df_churn_within_NHS_Gender.RDS" )
    saveRDS( df_churn_within_NHS_AgeBand, "Processed datasets/Paper 1/df_churn_within_NHS_AgeBand.RDS" )
    saveRDS( df_churn_within_NHS_EthnicGroup, "Processed datasets/Paper 1/df_churn_within_NHS_EthnicGroup.RDS" )
    
    message( "DONE. Turnover data are now available in storage." )
  }
)

# ## ONS rurality.
tryCatch(
  expr = {
    df_ons_rurality <- readRDS( "Processed datasets/Paper 1/df_ons_rurality.RDS" )
    df_postcodeToLADCD <- readRDS( "Processed datasets/Paper 1/df_postcodeToLADCD.RDS" )
    
    message( "Rurality data are already available in storage." )
  }
  ,warning = function(w) {
    message( "Rurality data are not available in storage so it is being created." )
  
    df_ons_rurality <-
      readxl::read_xlsx( path = "xls_ons_rurality.xlsx", sheet = "Table 1D", range = "A3:I334" )
    # ## Postcode-to-LADCD mapping.
    df_postcodeToLADCD <- readr::read_csv( unzip("csv_postcodeToLADCD.zip") )
    
    # Save.
    saveRDS( df_ons_rurality, "Processed datasets/Paper 1/df_ons_rurality.RDS" )
    saveRDS( df_postcodeToLADCD, "Processed datasets/Paper 1/df_postcodeToLADCD.RDS" )
    
    message( "DONE. Rurality data are now available in storage." )
  }
)

# ## Trust size.
tryCatch(
  expr = {
    df_Trust_size_2021_03 <- readRDS( "Processed datasets/Paper 1/df_Trust_size_2021_03.RDS" )
    df_Trust_size_2022_03 <- readRDS( "Processed datasets/Paper 1/df_Trust_size_2022_03.RDS" )
    df_Trust_size_2023_03 <- readRDS( "Processed datasets/Paper 1/df_Trust_size_2023_03.RDS" )
    
    message( "Trust-size data are already available in storage." )
  }
  ,warning = function(w) {
    message( "Trust-size data are not available in storage so it is being created." )
  
    df_Trust_size_2021_03 <-
      readxl::read_xlsx(
        path = "xls_Trust_size_2021_03.xlsx"
        ,sheet = "2. NHSE, Org & SG - HC"
        ,range = "C11:E351"
      ) %>%
      `colnames<-`( c( 'Trust name 2021 03', 'Trust code 2021 03', 'Trust size 2021 03' ) )
    df_Trust_size_2022_03 <-
      readxl::read_xlsx(
        path = "xls_Trust_size_2022_03.xlsx"
        ,sheet = "2. NHSE, Org & SG - HC"
        ,range = "C11:E328"
      ) %>%
      `colnames<-`( c( 'Trust name 2022 03', 'Trust code 2022 03', 'Trust size 2022 03' ) )
    df_Trust_size_2023_03 <-
      readxl::read_xlsx(
        path = "xls_Trust_size_2023_03.xlsx"
        ,sheet = "2. NHSE, Org & SG - HC"
        ,range = "E11:G311"
      ) %>%
      dplyr::filter( !is.na(...1) ) %>%
      dplyr::filter( !stringr::str_detect( ...1, pattern = "ICB" ) ) %>%
      `colnames<-`( c( 'Trust name 2023 03', 'Trust code 2023 03', 'Trust size 2023 03' ) )
    
    # Save.
    saveRDS( df_Trust_size_2021_03, "Processed datasets/Paper 1/df_Trust_size_2021_03.RDS" )
    saveRDS( df_Trust_size_2022_03, "Processed datasets/Paper 1/df_Trust_size_2022_03.RDS" )
    saveRDS( df_Trust_size_2023_03, "Processed datasets/Paper 1/df_Trust_size_2023_03.RDS" )
    
    message( "DONE. Trust-size data are now available in storage." )
  }
)
# ----

#######################
## Load local files. ##
#######################
# Load deprivation data.
# ----
# ## This data are given by Julie Nightingale in an email to Michaela on the 9th
# ## of October. The only provenance to speak of is that the data are based on
# ## their catchment area rather than the hospital postcode.
tryCatch(
  expr = {
    df_deprivation <- readRDS( "Processed datasets/Paper 1/df_deprivation.RDS" )
    
    message( "Deprivation data are already available in storage." )
  }
  ,warning = function(w) {
    message( "Deprivation data are not available in storage so it is being created." )
  
    df_deprivation <-
      readxl::read_xlsx(
        path = file.path("../../Data/Hospital trusts and deprivation.xlsx")
        ,sheet = "Sheet1"
        )
    
    # Save.
    saveRDS( df_deprivation, "Processed datasets/Paper 1/df_deprivation.RDS" )
    
    message( "DONE. Deprivation data are now available in storage." )
  }
)
# ----

# Load vacancy-rates data.
# ----
# ## This data came from a freedom-of-information request sent by Michaela 
# ## (Ref: FOI - 2507-2236881 NHSE:0796329).
tryCatch(
  expr = {
    ls_vacancy <- readRDS( "Processed datasets/Paper 1/ls_vacancy.RDS" )
    
    message( "Vacancy-rate data are already available in storage." )
  }
  ,warning = function(w) {
    message( "Vacancy-rate data are not available in storage so it is being created." )
 
    filename <- file.path("../../Data/FOI - 2507-2236881 FOI_AHP_Vacancy_22-25.xlsx")
    sheets <- readxl::excel_sheets( filename )
    sheets <- sheets[ !sheets %in% c( "Paramedic" ) ] # There are no call handlers to remove.
    ls_vacancy <-
      lapply(
      sheets
      ,function(x)
        readxl::read_xlsx( filename, sheet = x, range = "A1:DH214" ) %>%
        suppressMessages()
      )
    names( ls_vacancy ) <- sheets
    ls_vacancy <-
      lapply(
        ls_vacancy
        ,function(x)
        {
          # Make column names.
          within_x <- x[1]
          x <- tibble::add_column(
            .data = x
            ,col = rep( names( x )[1], by = nrow( within_x ) )
            ,.before = 1
            ,.name_repair = "minimal"
            )
          names( x ) <-
            c(
              'Care setting'
              ,'Trust code'
              ,'Trust name'
              ,paste0( "vacancy_rate_", as.character( seq( as.Date( "2022-04-01" ), as.Date( "2025-03-01" ), by = "month" ) ) )
              ,'Staff in post'
              ,paste0( "staff_count_", as.character( seq( as.Date( "2022-04-01" ), as.Date( "2025-03-01" ), by = "month" ) ) )
              ,'Vacancies'
              ,paste0( "vacancy_count_", as.character( seq( as.Date( "2022-04-01" ), as.Date( "2025-03-01" ), by = "month" ) ) )
            )
          
          # Structure the data.frame so that there is only one column with the
          # statistic value, with another showing what the statistic is.
          # Data for all months are left in even though we only have yearly 
          # data for other variables. Only the appropriate annual value will be
          # included in the main data set.
          x <-
            dplyr::bind_cols(
        
              x %>%
                dplyr::select(
                `Care setting`, `Trust code`, `Trust name`
                ,contains( "vacancy_rate" ) ) %>%
                tidyr::pivot_longer(
                  cols = contains( "vacancy_rate")
                  ,names_to = "stat_name"
                  ,values_to = "stat_value"
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_name
                  ,delim = "rate_"
                  ,names = c( "stat_name", "stat_date")
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_date
                  ,delim = "-"
                  ,names = c( "vacancy_year", "vacancy_month", "Day")
                ) %>%
                dplyr::select( -c( stat_name, Day ) ) %>%
                dplyr::rename( vacancy_rate = stat_value )
            
              ,x %>%
                dplyr::select(
                  `Care setting`, `Trust code`, `Trust name`
                  ,contains( "staff_count" ) ) %>%
                tidyr::pivot_longer(
                  cols = contains( "staff_count")
                  ,names_to = "stat_name"
                  ,values_to = "stat_value"
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_name
                  ,delim = "count_"
                  ,names = c( "stat_name", "stat_date")
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_date
                  ,delim = "-"
                  ,names = c( "vacancy_year", "vacancy_month", "Day")
                ) %>%
                dplyr::rename( staff_count = stat_value ) %>%
                dplyr::select( staff_count )
            
              ,x %>%
                dplyr::select(
                  `Care setting`, `Trust code`, `Trust name`
                  ,contains( "vacancy_count" ) ) %>%
                tidyr::pivot_longer(
                  cols = contains( "vacancy_count")
                  ,names_to = "stat_name"
                  ,values_to = "stat_value"
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_name
                  ,delim = "count_"
                  ,names = c( "stat_name", "stat_date")
                ) %>%
                tidyr::separate_wider_delim(
                  cols = stat_date
                  ,delim = "-"
                  ,names = c( "vacancy_year", "vacancy_month", "Day")
                ) %>%
                dplyr::rename( vacancy_count = stat_value ) %>%
                dplyr::select( vacancy_count )
            
              ) %>%
            dplyr::mutate(
              vacancy_year = as.integer( vacancy_year )
              ,vacancy_month = as.integer( vacancy_month )
            )
          
          return( x )
        }
      )
    #rm( sheets, filename )
    # Save.
    saveRDS( ls_vacancy, "Processed datasets/Paper 1/ls_vacancy.RDS" )
    
    message( "DONE. Vacancy-rate data are now available in storage." )
  }
)

# ----

# Load postcode-to-Trust mapping data.
# ----
# ## This was painstakingly acquired by appending all Trust codes to the URL
# ## https://uat.directory.spineservices.nhs.uk/STU3/Organization/ and extracting
# ## the postcode at the bottom. The URL is used as part of the API but I don't
# ## have the time, skill, or will to sign-up and get the API working. The file
# ## only contains the postcodes and LAD22CDs for the Trust codes in the
# ## deprivation file that was given to Michaela. I will have to assume that the
# ## LAD has not changed since 2021.

tryCatch(
  expr = {
    df_postcodeToTrust <- readRDS( "Processed datasets/Paper 1/df_postcodeToTrust.RDS" )
    
    message( "Postcode-to-Trust mapping data are already available in storage." )
  },
  warning = function(w) {
    message( "Postcode-to-Trust mapping data are not available in storage so it is being created." )

    df_postcodeToTrust <- readr::read_csv( "../../Data/postcode_and_TrustCode.csv" )
    # # ## Exclude London post codes.
    # # ## I've assumed the complete list of postcodes are those coloured at
    # # ## https://www.doogal.co.uk/london_postcodes.
    # # ## This is part of Michaela's 5-point plan. She wants to exclude London because
    # # ## she believes it is an outlier.
    # df_postcodeToTrust <-
    #   df_postcodeToTrust %>%
    #   dplyr::filter(
    #     !stringr::str_detect(
    #       string = substr(pcds, start = 1, stop = 3)
    #       ,pattern = 
    #           "NW[0-9]|N[0-9] |N[0-9][0-9]|E[0-9] |E[0-9][0-9]|SE[0-9]|W[0-9] |W[0-9][0-9]|SW[0-9]|WC[0-9]|EC[0-9]"
    #     )
    #   )
    
    # Save.
    saveRDS( df_postcodeToTrust, "Processed datasets/Paper 1/df_postcodeToTrust.RDS" )
    
    message( "DONE. Postcode-to-Trust mapping data are now available in storage." )
  }
)

# ----

# Load patient satisfaction data.
# ----
tryCatch(
  expr = {
    df_patientSatisfaction <- readRDS( "Processed datasets/Paper 1/df_patientSatisfaction.RDS" )
    df_patientSatisfaction_historic <- readRDS( "Processed datasets/Paper 1/df_patientSatisfaction_historic.RDS" )
    
    message( "Patient-satisfaction data are already available in storage." )
  },
  warning = function(w) {
    message( "Patient satisfaction data are not available in storage so it is being created." )
    
    df_patientSatisfaction <- 
      readxl::read_xlsx(
        path = "../../Data/20250909_aip24_Benchmark_TrustLevel.xlsx" 
        ,sheet = "IP24_trust_results"
        )
    df_patientSatisfaction_historic <- 
      readxl::read_xlsx(
        path = "../../Data/20250909_aip24_Benchmark_TrustLevel.xlsx" 
        ,sheet = "IP24_trust_historic_results"
      )
    
    # Save.
    saveRDS( df_patientSatisfaction, "Processed datasets/Paper 1/df_patientSatisfaction.RDS" )
    saveRDS( df_patientSatisfaction_historic, "Processed datasets/Paper 1/df_patientSatisfaction_historic.RDS" )
    
    message( "DONE. Patient satisfaction data are now available in storage." )
  }
)
# ----

# Load staff-survey data.
# ----
# See the survey technical guide for details:
# https://www.nhsstaffsurveys.com/static/ea079b722ad235a21b0356670766a33b/NHS-Staff-Survey-2025-Technical-Guide-V1.pdf
# I cannot find any explanation of what the `main_or_bank_indicator` column
# means.
tryCatch(
  expr = {
    df_staff_survey_main <- readRDS( "Processed datasets/Sept 2026 meeting/df_staff_survey_main.RDS" )
    
    message( "Staff-survey data are already available in storage." )
  },
  warning = function(w) {
    message( "Staff-survey data are not available in storage so it is being created." )
    
    df_staff_survey_main <-
      haven::read_sav( "../../Data/NSS_2025_respondents_data_v2.sav" )
    # df_staff_survey_bank <-
    #   haven::read_sav( "../../Data/NSS24_bank_AN001_data v1.0.sav" )
    
    # Save.
    saveRDS( df_staff_survey_main, "Processed datasets/Sept 2026 meeting/df_staff_survey_main.RDS" )
    # saveRDS( df_staff_survey_bank, "Processed datasets/Sept 2026 meeting/df_staff_survey_bank.RDS" )
    
    message( "DONE. Staff-survey data are now available in storage." )
  }
)
# ----

# Load job role-to-care setting mapping data.
# ----
# This was painstaking to create. There were small differences between the
# `job_role` column in the staff-survey data and the `Care setting` column in
# the churn data. I needed to map between the two. I figured the best way was to
# create a mapping table. I create maps for every `job_role` value except the
# managers, whom I don't want to keep.
tryCatch(
  expr = {
    df_jobroleToCaresetting <- readRDS( "Processed datasets/Sept 2026 meeting/df_jobroleToCaresetting.RDS" )
    
    message( "Role-to-care setting mapping data are already available in storage." )
  },
  warning = function(w) {
    message( "Role-to-care setting mapping data are not available in storage so it is being created." )
    
    df_jobroleToCaresetting <- readr::read_csv( "../../Data/job_role_to_care_setting_map.csv" )
    
    # Save.
    saveRDS( df_jobroleToCaresetting, "Processed datasets/Sept 2026 meeting/df_jobroleToCaresetting.RDS" )
    
    message( "DONE. Role-to-care setting mapping data are now available in storage." )
  }
)
# ----