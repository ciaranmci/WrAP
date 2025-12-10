# WrAP_Load data.R
#
# The purpose of this script is to load the data
# 
# The publicly-available data is:
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
  tidyverse
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
# per Trust. How we incorporate these data is a matter for the particular 
# analysis.
# In the meantime (2025 11 13), I will only incorporate the March counts from
# the three years.
#

# Set URLs.
url_churn_from_NHS <- "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-nhs-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
url_churn_within_NHS <- "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-organisation-of-ahps-march-2022-to-march-2025_ah5318.xlsx"
url_ons_rurality <- "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/ruralurbanclassifications/2021ruralurbanclassification/rucallsupplementarytables.xlsx"
url_postcodeToLADCD <- "https://www.arcgis.com/sharing/rest/content/items/bcfc75627db44bb4b0261f8578361954/data"
url_Trust_size_2021_03 <- "https://files.digital.nhs.uk/C4/453C24/NHS%20Workforce%20Statistics%2C%20March%202021%20England%20and%20Organisation.xlsx"
url_Trust_size_2022_03 <- "https://files.digital.nhs.uk/C3/488527/NHS%20Workforce%20Statistics%2C%20March%202022%20England%20and%20Organisation.xlsx"
url_Trust_size_2023_03 <- "https://files.digital.nhs.uk/07/6F3BE5/NHS%20Workforce%20Statistics%2C%20March%202023%20England%20and%20Organisation.xlsx"
# Download files from URL.
curl::curl_download( url_churn_from_NHS, "xls_churn_from_NHS.xlsx" )
curl::curl_download( url_churn_within_NHS, "xls_churn_within_NHS.xlsx" )
curl::curl_download( url_ons_rurality, "xls_ons_rurality.xlsx" )
curl::curl_download( url = url_postcodeToLADCD, destfile = "csv_postcodeToLADCD.zip" )
curl::curl_download( url_Trust_size_2021_03, "xls_Trust_size_2021_03.xlsx" )
curl::curl_download( url_Trust_size_2022_03, "xls_Trust_size_2022_03.xlsx" )
curl::curl_download( url_Trust_size_2023_03, "xls_Trust_size_2023_03.xlsx" )
# Load files.
# ## Churn from NHS.
df_churn_from_NHS_Grade <-
  readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Grade" )
df_churn_from_NHS_Gender <-
  readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Gender" )
df_churn_from_NHS_AgeBand <-
  readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Age band" )
df_churn_from_NHS_EthnicGroup <-
  readxl::read_xlsx( path = "xls_churn_from_NHS.xlsx", sheet = "Ethnic group" )
# ## Churn within NHS.
df_churn_within_NHS_Grade <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Grade" )
df_churn_within_NHS_Gender <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Gender" )
df_churn_within_NHS_AgeBand <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Age band" )
df_churn_within_NHS_EthnicGroup <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Ethnic group" )
# ## ONS rurality.
df_ons_rurality <-
  readxl::read_xlsx( path = "xls_ons_rurality.xlsx", sheet = "Table 1D", range = "A3:I334" )
# ## Postcode-to-LADCD mapping.
df_postcodeToLADCD <- readr::read_csv( unzip("csv_postcodeToLADCD.zip") )
# ## Trust size.
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
# Tidy up.
ls_churn <-
  list(
    df_churn_from_NHS_Grade = df_churn_from_NHS_Grade
    ,df_churn_from_NHS_Gender = df_churn_from_NHS_Gender
    ,df_churn_from_NHS_AgeBand = df_churn_from_NHS_AgeBand
    ,df_churn_from_NHS_EthnicGroup = df_churn_from_NHS_EthnicGroup
    ,df_churn_within_NHS_Grade = df_churn_within_NHS_Grade
    ,df_churn_within_NHS_Gender = df_churn_within_NHS_Gender
    ,df_churn_within_NHS_AgeBand = df_churn_within_NHS_AgeBand
    ,df_churn_within_NHS_EthnicGroup = df_churn_within_NHS_EthnicGroup
  )
rm(
  df_churn_from_NHS_Grade
  ,df_churn_from_NHS_Gender
  ,df_churn_from_NHS_AgeBand
  ,df_churn_from_NHS_EthnicGroup
  ,df_churn_within_NHS_Grade
  ,df_churn_within_NHS_Gender
  ,df_churn_within_NHS_AgeBand
  ,df_churn_within_NHS_EthnicGroup
)
# ----

#######################
## Load local files. ##
#######################
# ----
# Load deprivation data.
# ## This data was given by Julie Nightingale in an email to Michaela on the 9th
# ## of October. The only provenance to speak of is that the data are based on
# ## based on their catchment area rather than the hospital postcode.
df_deprivation <-
  readxl::read_xlsx(
    path = file.path("../../Data/Hospital trusts and deprivation.xlsx")
    ,sheet = "Sheet1"
    )

# Load vacancy-rates data.
# ## This data came from a freedom-of-information request sent by Michaela 
# ## (Ref: FOI - 2507-2236881 NHSE:0796329).
filename <- file.path("../../Data/FOI - 2507-2236881 FOI_AHP_Vacancy_22-25.xlsx")
sheets <- readxl::excel_sheets( filename )
ls_vacancy <-
  lapply(
  sheets
  ,function(x)
    readxl::read_xlsx( filename, sheet = x, range = "A1:DH20" ) %>%
    suppressMessages()
  )
names( ls_vacancy ) <- sheets
ls_vacancy <-
  lapply(
    ls_vacancy
    ,function(x)
    {
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
          ,paste0( "staff_count", as.character( seq( as.Date( "2022-04-01" ), as.Date( "2025-03-01" ), by = "month" ) ) )
          ,'Vacancies'
          ,paste0( "vacancy_count", as.character( seq( as.Date( "2022-04-01" ), as.Date( "2025-03-01" ), by = "month" ) ) )
        )
      
      return( x )
    }
  )
rm( sheets, filename )

# Load postcode-to-Trust mapping data.
# ## This was painstakingly acquired by appending all Trust codes to the URL
# ## https://uat.directory.spineservices.nhs.uk/STU3/Organization/ and extracting
# ## the postcode at the bottom. The URL is used as part of the API but I don't
# ## have the time, skill, or will to sign-up and get the API working. The file
# ## only contains the postcodes and LAD22CDs for the Trust codes in the
# ## deprivation file that was given to Michaela. I will have to assume that the
# ## LAD has not changed since 2021.
df_postcodeToTrust <- readr::read_csv( "../../Data/postcode_and_TrustCode.csv" )


# Load CQC data.
# df_CQC <- 


# Load patient satisfaction data.
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

# ----