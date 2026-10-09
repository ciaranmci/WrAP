# dropdown_plotter.r
#
# The purpose of this script is to make and save the data required to make a
# plotter in Excel
#


#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  openxlsx
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
dir.create( "./Tests/Paper 1", recursive = TRUE )
dir.create( "./Tables/Paper 1", recursive = TRUE )
dir.create( "./Plots/Paper 1", recursive = TRUE )
dir.create( "./Models/Paper 1", recursive = TRUE )
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

##############
# Make data. #
##############
# ----
base_data<-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,!`Care setting` %in% c( 'All care settings' )
  ) %>% 
  dplyr::distinct(
    orgname = `Organisation name`
    ,Profession = `Care setting`
    ,vacancy = past_year_mean_vacancy_rate
    ,stability_index = `Stability index`
  ) %>%
  dplyr::mutate(
    Profession = dplyr::if_else(
      Profession == 'Operating Theatres'
      ,'Operating Department Practitioners'
      ,Profession
    )
    ,orgname = stringr::str_to_title(orgname )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    orgname = gsub( pattern = "Nhs", replacement = "NHS", x = orgname)
  )
# ----

##############################
# Create the Excel workbook. #
##############################
# ----
# Make workbook.
wb <- openxlsx::createWorkbook( "dropdown_plotter_si_vs_vacancy" )

# Add worksheets.
openxlsx::addWorksheet( wb, "stability index data")
openxlsx::addWorksheet( wb, "vacancy data")

# Add data.
openxlsx::writeData(
  wb
  ,sheet ="stability index data"
  ,base_data %>%
    dplyr::select( -vacancy ) %>%
    tidyr::pivot_wider(
      id_cols = orgname
      ,names_from = Profession
      , values_from = stability_index
    )
  )
openxlsx::writeData(
  wb
  ,sheet ="vacancy data"
  ,base_data %>%
    dplyr::select( -stability_index ) %>%
    tidyr::pivot_wider(
      id_cols = orgname
      ,names_from = Profession
      ,values_from = vacancy
    ) 
  )

# Save workbook.
openxlsx::saveWorkbook( wb, "dropdown_plotter_si_vs_vacancy.xlsx", overwrite = TRUE )
# ----