# WrAP_manuscript_figures.R
# 
# The purpose of this script is to produce the figures for a manuscript
# that describes the dataset.
# 



#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  curl
  ,haven
  ,sf
  ,tidyverse
)
# ----

#################
## Requisites. ##
#################
# ----
# Set URLs.
url_churn_within_NHS <- "https://digital.nhs.uk/binaries/content/assets/website-assets/supplementary-information/supplementary-info-2025/turnover-from-organisation-of-ahps-march-2022-to-march-2025_ah5318.xlsx"

# Set the list of questions of interest.
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


# Determine the five largest professions, nationally.
professions_of_interest <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    `AfC band` %in% c( 'All AfC bands' )
    ,!`Care setting` %in% c( 'All care settings' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Select year of interest
    ,year_end %in% year_of_interest
  ) %>% 
  dplyr::reframe(
    N = sum( `Denominator at start of period` )
    ,.by = `Care setting`
  ) %>%
  dplyr::arrange( -N ) %>%
  head( 5 ) %>%
  dplyr::pull( `Care setting` )
# ----

################
## Load data. ##
################
# ----

# Load job role-to-care setting mapping data.
# This was painstaking to create. There were small differences between the
# `job_role` column in the staff-survey data and the `Care setting` column in
# the churn data. I needed to map between the two. I figured the best way was to
# create a mapping table. I create maps for every `job_role` value except the
# managers, whom I don't want to keep.
df_jobroleToCaresetting <- readr::read_csv( "../../Data/job_role_to_care_setting_map.csv" )

# Churn dataset.
# # Download files from URL.
# curl::curl_download( url_churn_within_NHS, "xls_churn_within_NHS.xlsx" )
# # Load files. Focus on head-count values ("HC") and exclude call handlers
# # and paramedics.

# Pay grade data.
df_churn_within_NHS_Grade <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Grade" ) %>%
  dplyr::filter( Type == 'HC' ) %>%
  dplyr::select( -Type ) %>%
  dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
# # Prepare the data.
df_churn_within_NHS_Grade <-
  df_churn_within_NHS_Grade %>%
  # Rename columns.
  dplyr::rename(
    joiner_rate = `Joiner rate`
    ,leaver_rate = `Leaver rate`
  ) %>%
  dplyr::mutate(
    # Calculate the stability index using the rounded counts and call it
    # the `reaminer_rate`.
    remainer_rate = 
      ( `Denominator at start of period` - Leaver ) /
      `Denominator at start of period`
    # Define a variable that indicates when a number too small to be disclosed
    # was used in a calculation.
    ,too_small_to_disclose =
      dplyr::if_else(
        Joiner <= 5 | Leaver <= 5
        ,TRUE, FALSE
      )
    # Make a new variable called `year` that explains the `Period` variable
    # a bit better.
    ,year = dplyr::case_when(
      Period == '202203 to 202303' ~ "March '22 to March '23"
      ,Period == '202303 to 202403' ~ "March '23 to March '24"
      ,Period == '202403 to 202503' ~ "March '24 to March '25"
      ,.default = NULL
    )
    ,year_end = dplyr::case_when(
      stringr::str_sub( year, start = -2, end = -1) == "25" ~ 2025
      ,stringr::str_sub( year, start = -2, end = -1) == "24" ~ 2024
      ,stringr::str_sub( year, start = -2, end = -1) == "23" ~ 2023
      ,stringr::str_sub( year, start = -2, end = -1) == "22" ~ 2022
      ,.default = NULL
    )
    # Process the stability index so that it behaves like a number.
    ,`Stability index` = dplyr::if_else( `Stability index` == ".", NA, `Stability index` )
    ,`Stability index` = as.numeric( `Stability index` ) 
    # Standardise the case sensitivity of `Organisation name` so that it joins
    # with the deprivation data.
    ,`Organisation name` = tolower( `Organisation name` )
    # Format the text of the profession values.
    ,`Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>% 
  dplyr::filter(
    # Exclude rows referring to Integrated Care Boards and Clinical Commissioning Groups
    !`Benchmark group` %in% c( "Integrated Care Board", "Clinical Commissioning Group")
  )

# Age data.
df_churn_within_NHS_Age <-
  readxl::read_xlsx( path = "xls_churn_within_NHS.xlsx", sheet = "Age band" ) %>%
  dplyr::filter( Type == 'HC' ) %>%
  dplyr::select( -Type ) %>%
  dplyr::filter( !`Care setting` %in% c( "Call Handling", "Emergency Care" ) )
# # Prepare the data.
df_churn_within_NHS_Age <-
  df_churn_within_NHS_Age %>%
  # Order the age bands.
  dplyr::mutate(
    `Age band` =
      factor(
        `Age band`
        ,levels =
          c( "All age bands", "Under 25", "25 to 34", "35 to 44"
             ,"45 to 54", "55 to 64", "65 and over"
            )
        )
    ) %>%
  # Rename columns.
  dplyr::rename(
    joiner_rate = `Joiner rate`
    ,leaver_rate = `Leaver rate`
  ) %>%
  dplyr::mutate(
    # Calculate the stability index using the rounded counts and call it
    # the `reaminer_rate`.
    remainer_rate = 
      ( `Denominator at start of period` - Leaver ) /
      `Denominator at start of period`
    # Define a variable that indicates when a number too small to be disclosed
    # was used in a calculation.
    ,too_small_to_disclose =
      dplyr::if_else(
        Joiner <= 5 | Leaver <= 5
        ,TRUE, FALSE
      )
    # Make a new variable called `year` that explains the `Period` variable
    # a bit better.
    ,year = dplyr::case_when(
      Period == '202203 to 202303' ~ "March '22 to March '23"
      ,Period == '202303 to 202403' ~ "March '23 to March '24"
      ,Period == '202403 to 202503' ~ "March '24 to March '25"
      ,.default = NULL
    )
    ,year_end = dplyr::case_when(
      stringr::str_sub( year, start = -2, end = -1) == "25" ~ 2025
      ,stringr::str_sub( year, start = -2, end = -1) == "24" ~ 2024
      ,stringr::str_sub( year, start = -2, end = -1) == "23" ~ 2023
      ,stringr::str_sub( year, start = -2, end = -1) == "22" ~ 2022
      ,.default = NULL
    )
    # Process the stability index so that it behaves like a number.
    ,`Stability index` = dplyr::if_else( `Stability index` == ".", NA, `Stability index` )
    ,`Stability index` = as.numeric( `Stability index` ) 
    # Standardise the case sensitivity of `Organisation name` so that it joins
    # with the deprivation data.
    ,`Organisation name` = tolower( `Organisation name` )
    # Format the text of the profession values.
    ,`Care setting` = gsub( pattern = "/", replacement = " / ", x = `Care setting` )
  ) %>% 
  dplyr::filter(
    # Exclude rows referring to Integrated Care Boards and Clinical Commissioning Groups
    !`Benchmark group` %in% c( "Integrated Care Board", "Clinical Commissioning Group")
  )

# Load staff-survey data.
# ## See the survey technical guide for details:
# ## https://www.nhsstaffsurveys.com/static/ea079b722ad235a21b0356670766a33b/NHS-Staff-Survey-2025-Technical-Guide-V1.pdf
# ## I cannot find any explanation of what the `main_or_bank_indicator` column
# ## means.
df_staff_survey_main <-
  haven::read_sav( "../../Data/NSS24_main_AN001_data v1.0.sav" )

# Process staff-survey data.
df_staff_survey_main <-
  df_staff_survey_main %>%
  # Match `job_role` column to the `Care setting` options found in the main
  # churn data.
  dplyr::left_join(
    df_jobroleToCaresetting 
    ,by = join_by( job_role )
  ) %>%
  # Re-code the year.
  dplyr::rename( ss_year = year_date ) %>%
  dplyr::mutate(
    ss_year = dplyr::case_when(
      stringr::str_sub( ss_year, start = -2, end = -1) == "24" ~ 2024
      ,stringr::str_sub( ss_year, start = -2, end = -1) == "23" ~ 2023
      ,stringr::str_sub( ss_year, start = -2, end = -1) == "22" ~ 2022
      ,.default = NULL
    )
  ) %>%
  tidyr::drop_na( ss_year ) %>%
  # Select only the columns of interest.
  dplyr::select(
    c(
      ss_year
      ,`Care setting`
      ,org_id
      ,org_name
      ,job_role
      ,area_of_work
      ,q2a    
      ,q3i
      ,q4d
      ,q4c
      ,q5a
      ,q9a
      ,q9i
      ,q11c
      ,q21
      ,q24d
      ,q25d
      ,q25f
      ,q26a
      ,q26c
    )
  ) %>% 
  # Convert Likert scaling to scores as per Table 2 in the technical specification
  # for the survey.
  # (https://www.nhsstaffsurveys.com/static/ea079b722ad235a21b0356670766a33b/NHS-Staff-Survey-2025-Technical-Guide-V1.pdf)
  # The Likert scale 0-5 is scored as { 1 = 0, 2 = 2.5, 3 = 5, 4 = 7.5, 5 = 10 }
  # for every question except for the following:
  # - q11c : 1 = 0, 2 = 10
  # - q26a : 1 = 10, 2 = 7.5, 3 = 5, 4 = 2.5 , 5 = 0
  # - q26c : 1 = 10, 2 = 7.5, 3 = 5, 4 = 2.5 , 5 = 0
  dplyr::mutate(
    across(
      .cols = starts_with( "q" )
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 0
          ,x == 2 ~ 2.5
          ,x == 3 ~ 5
          ,x == 4 ~ 7.5
          ,x == 5 ~ 10
          ,.default = NULL
        )
      }
      ,.names = '{.col}_ResponseScore'
    )
  ) %>%
  dplyr::mutate(
    across(
      q11c
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 0
          ,x == 2 ~ 10
          ,.default = NULL
        )
      }
      ,.names = '{.col}_ResponseScore'
    )
  ) %>%
  dplyr::mutate(
    across(
      c( q26a, q26c )
      ,function(x){
        dplyr::case_when(
          x == 1 ~ 10
          ,x == 2 ~ 7.5
          ,x == 3 ~ 5
          ,x == 4 ~ 2.5
          ,x == 5 ~ 0
          ,.default = NULL
        )
      }
      ,.names = '{.col}_ResponseScore'
    )
  ) %>%  
  # Convert the Likert scale columns to factor data type. Factor labels are from
  # the survey guide:
  # https://www.nhsstaffsurveys.com/static/958853e094733756687a78aa3d8cb36c/NSS2025-Questionnaire.zip
  dplyr::mutate(
    across(
      starts_with( "q" ) & !ends_with( "Score" )
      ,as.factor
    )
  ) %>% 
  dplyr::mutate(
    across(
      c( q2a, q5a )
      ,~forcats::fct_recode(
        .x
        ,"Never" = "1"
        ,"Rarely"= "2"
        ,"Sometimes" = "3"
        ,"Often" = "4"
        ,"Always" = "5"
      )
    )
  ) %>% 
  dplyr::mutate(
    across(
      c( q3i, starts_with( "q9" ), starts_with( "q2" ), -q2a, -ends_with( "Score" ) )
      ,~forcats::fct_recode(
        .x
        ,"Strongly disagree" = "1"
        ,"Disagree"= "2"
        ,"Neither agree nor disagree" = "3"
        ,"Agree" = "4"
        ,"Strongly agree" = "5"
      )
    )
  ) %>%
  dplyr::mutate(
    across(
      starts_with( "q4" ) & !ends_with( "Score" )
      ,~forcats::fct_recode(
        .x
        ,"Very dissatisfied" = "1"
        ,"Dissatisfied"= "2"
        ,"Neither satisfied nor dissatisfied" = "3"
        ,"Satisfied" = "4"
        ,"Very satisfied" = "5"
      )
    )
  ) %>% 
  dplyr::mutate(
    q11c = 
      forcats::fct_recode(
        q11c
        ,"Yes" = "2"
        ,"No"= "1"
      )
  ) %>%
  dplyr::rename_with(
    .cols = starts_with( "q" ) & !ends_with( "Score" )
    ,.f = ~paste0( .x, "_LikertScore")
  ) %>%
  # Rename columns to match turnover data.
  dplyr::rename( `Org code` = org_id ) %>%
  # Rename question columns.
  dplyr::rename_with(
    .fn = ~ sub( pattern = "q", replacement = "ss_q", .x )
    ,.cols = starts_with( 'q' )
  ) %>% 
  # Create a dichotomous version of Likert questions, where appropriate.
  dplyr::mutate(
    across(
      contains( "Likert" )
      ,function(x){
        dplyr::case_when(
          x == "Strongly disagree" ~ "Disagree"
          ,x == "Disagree"~ "Disagree"
          ,x == "Neither agree nor disagree" ~ NA
          ,x == "Agree" ~ "Agree"
          ,x == "Strongly agree" ~ "Agree"
          ,x == "Very dissatisfied" ~ "Dissatisfied"
          ,x == "Dissatisfied" ~ "Dissatisfied"
          ,x == "Neither satisfied nor dissatisfied" ~ NA
          ,x == "Satisfied" ~ "Satisfied"
          ,x == "Very satisfied" ~ "Satisfied"
          ,x == "No" ~ "No"
          ,x == "Yes" ~ "Yes"
          ,.default = NULL
        )
      }
      ,.names = '{.col}_binary'
    )
  ) %>%
  dplyr::select( -( ( contains( "2a" ) | contains( "q5" ) ) & contains( "binary" ) ) ) %>%
  dplyr::mutate(
    across(
      starts_with( "q4" ) & ends_with( "_binary" )
      ,~forcats::fct_relevel(
        .x
        ,c( "Dissatisfied", "Satisfied" )
      )
    )
  ) %>%
  dplyr::mutate(
    across(
      ( ends_with( "_binary" ) & ( contains( "q9" ) | contains( "q2" ) ) ) |
        ends_with( "_binary" ) & contains( "q3" )
      ,~forcats::fct_relevel(
        .x
        ,c( "Disagree", "Agree" = "2" )
      )
    )
  ) %>%
  # Tidy up.
  dplyr::distinct() %>%
  dplyr::arrange( `Org code`, `Care setting` )


# Remove rows from staff that we are not interested in.
df_staff_survey_main <-
  df_staff_survey_main %>%
  tidyr::drop_na( `Care setting`)

# Extract the set of professions in the data.
roles <-  c( "All care settings", unique( df_staff_survey_main$`Care setting` ) )
# ----

###############################################################
## Plot of stability index for the five largest professions. ##
###############################################################
# A Tukey-style boxplot that shows 5 largest professions distribution of
# stability-index values. Delimit to the year ending 2025.
# ----

# Make plot data.
plot_data <-
  df_churn_within_NHS_Grade %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`AfC band` %in% c( 'All AfC bands', 'Non AfC band', 'Band 4' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Care setting`, `AfC band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` )

sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    class_median = median( `Stability index`, na.rm = TRUE )
    ,class_min = min( `Stability index`, na.rm = TRUE )
    ,class_max = max( `Stability index`, na.rm = TRUE )
    ,test_position = 1.05
    ,.by = c( Profession, `AfC band` )
  )

# Make plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = `AfC band`
    ) ) +
 geom_boxplot(
    aes( fill = Profession )
    ,alpha = 0.3
    ,outlier.colour = "grey"
    ) +
  guides(
    fill = guide_legend( reverse = T )
    ) +
  scale_fill_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = test_position, y = `AfC band`, group = Profession )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,size = 3
    ,position = position_dodge( width = 0.9 )
    ,hjust = 0
  ) +
  annotate( "text", x = 1.1, y = 8.8, label = "Median", hjust = 1 ) +
  scale_x_continuous(
    limits = c( 0, 1.1 )
    ,breaks = c( 0, 0.25, 0.5, 0.75, 1.0 )
  ) +
  scale_y_discrete(
    expand = expansion(add = c(0, 1.3))
  ) +
  labs(
    title =
      stringr::str_wrap(
        paste0(
          "Distribution of stability index for the 5 largest* professions"
          ," in 2025, stratified by Agenda - for - Change (AfC) pay band."
        )
        ,55
      )
    ,subtitle =
      paste0(
        "\u2022 Median stablity index is shown in the right-side column."
        ,"\n\u2022 The 5 largest professions are:"
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
          )
        ,100
        )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15, face = "italic" )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,legend.position = "top"
    ,legend.title = element_blank()
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_payband__top5.png"
    )
  ,dpi = 300
  ,width = 17
  ,height = 22
  ,units = "cm"
)
# ----

###############################################################
## Plot of stability index for the five largest professions. ##
###############################################################
# A Tukey-style boxplot that shows stability-index values across age bands,
# using data from the five largest professions, only. Delimit to the year
# ending 2025.
# ----

# Make plot data.
plot_data <-
  df_churn_within_NHS_Age %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    !`Age band` %in% c( 'All age bands' )
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Select professions of interest
    ,`Care setting` %in% professions_of_interest
  ) %>%
  dplyr::select( `Care setting`, `Age band`, `Stability index` ) %>%
  dplyr::rename( Profession = `Care setting` )

sumstat_plot_data <-
  plot_data %>%
  dplyr::reframe(
    class_median = median( `Stability index`, na.rm = TRUE )
    ,class_min = min( `Stability index`, na.rm = TRUE )
    ,class_max = max( `Stability index`, na.rm = TRUE )
    ,test_position = 1.05
    ,.by = c( Profession, `Age band` )
  )

# Make plot.
p <- 
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = `Age band`
    ) ) +
  geom_boxplot(
    aes( fill = Profession )
    ,alpha = 0.3
    ,outlier.colour = "grey"
  ) +
  guides(
    fill = guide_legend( reverse = T )
  ) +
  scale_fill_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
  geom_text(
    data = sumstat_plot_data
    ,aes( x = test_position, y = `Age band`, group = Profession )
    ,label = round( sumstat_plot_data$class_median, 2 )
    ,size = 3
    ,position = position_dodge( width = 0.9 )
    ,hjust = 0
  ) +
  annotate( "text", x = 1.1, y = 6.7, label = "Median", hjust = 1 ) +
  scale_x_continuous(
    limits = c( 0, 1.1 )
    ,breaks = c( 0, 0.25, 0.5, 0.75, 1.0 )
  ) +
  scale_y_discrete(
    expand = expansion(add = c(0, 1.3))
  ) +
  labs(
    title =
      stringr::str_wrap(
        paste0(
          "Distribution of stability index for the 5 largest* professions"
          ," in 2025, stratified by age band."
        )
        ,55
      )
    ,subtitle =
      paste0(
        "\u2022 Median stablity index is shown in the right-side column."
        ,"\n\u2022 The 5 largest professions are:"
      )
    ,caption =
      stringr::str_wrap(
        paste0(  
          "*Size of profession was determined as the count of that staff role"
          ," at the start of the year."
        )
        ,100
      )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
    ,plot.title = element_text( size = 20 )
    ,plot.subtitle = element_text( size = 15, face = "italic" )
    ,plot.caption = element_text( hjust = 0, face = "italic" )
    ,legend.position = "top"
    ,legend.title = element_blank()
  )

# Save plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/Paper 1/plot__distribution_of_si_stratified_by_age__top5.png"
    )
  ,dpi = 300
  ,width = 17
  ,height = 22
  ,units = "cm"
)

# ----
  