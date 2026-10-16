# WrAP_HSRUK2026_poster.R
# 
# The purpose of this script is to produce the visualisations for a poster to 
# be shown as the HSR UK conference 2026.
# 

# The three plots that need to be made are:
# 1. Stability Index stratified by pay band, using all-AHPs data, latest year.
#   - The message is "Hey! Band 5s are different."
# 2. Stability Index choropleth, using all-AHPs, latest year, payband-5, only.
#   - The purpose is to exploring if there are geographical reasons for the
#     band-5 uniqueness.
# 3. Scatter plot of Stability Index (x-axis) vs the proportion of people in a
#    trust with a positive response to staff-survey question (y-axis). Use
#    all-AHPs data, latest year, and facet wrap by survey question. Use the 
#    question wording to inform the y-axis labels.
#   - The purpose is to explore possible forms of the relationship between SI
#     and survey responses.
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
# ----

################
## Load data. ##
################
# ----

# Churn dataset.
# # Download files from URL.
# curl::curl_download( url_churn_within_NHS, "xls_churn_within_NHS.xlsx" )
# # Load files. Focus on head-count values ("HC") and exclude call handlers
# # and paramedics.
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

#############################################
## Stability Index stratified by pay band. ##
#############################################
# ----
year_of_interest <- 2025

# Make plot data
#plot_data <-
  #df_churn_within_NHS_Grade %>%
  ls_churn_within_NHS[[1]] %>% 
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
  ) 

# Make plots for each role.
for( i_role in 1:length( roles ) )
{
  if( roles[ i_role ] == "All care settings" )
  {
    i_plot_data <-
      plot_data %>%
      dplyr::filter( `Care setting` == "All care settings" )
  } else {
    i_plot_data <-
      plot_data %>%
      dplyr::filter( `Care setting` == roles[ i_role ] )
  }
  
  if ( nrow( i_plot_data ) > 0 )
  {
    
    sumstat_plot_data <-
      i_plot_data %>%
      dplyr::reframe(
        class_median = median( `Stability index`, na.rm = TRUE )
        ,class_min = min( `Stability index`, na.rm = TRUE )
        ,class_max = max( `Stability index`, na.rm = TRUE )
        ,.by = c( `AfC band`, year_end )
      ) %>%
      dplyr::arrange( year_end, `AfC band` )
    
    min_val <-
      min(
        i_plot_data[ ,"Stability index" ]
        ,na.rm = TRUE
      )
    max_val <-
      max(
        i_plot_data[ , "Stability index" ]
        ,na.rm = TRUE
      )
    
    #p <- 
      i_plot_data %>%
      ggplot(
        aes(
          x = `Stability index`
          ,y = `AfC band`
        ) ) +
      geom_point(
        position = position_jitter( height = 0.1 )
        ,alpha = 0.2
      ) +
      geom_point(
        data = sumstat_plot_data
        ,aes( x = class_median, y = `AfC band` )
        ,colour = "red", shape = "|", size = 15
      ) +
      labs(
        title =
            'Distribution of stability index stratified by Agenda-for-Change (AfC) pay band.'
        ,subtitle =
          "Conclusion: Lower pay grades have worse staff retention."
        ,caption =
          paste0(
            "Year = ", year_of_interest,"."
            ,"\nShowing data for "
            , ifelse( roles[ i_role ] == "All professions", "All professions", roles[ i_role ]), "."
            ,"\nShowing values \u2265", round( min_val, 2 )
            ," and \u2264", round( max_val, 2 ), "."
            ,"\nRed line shows the median for a given pay band."
          )
        , y = "AfC band"
      ) +
      theme_minimal() +
      theme(
         axis.title = element_blank()
        # ,axis.text = element_text( size = 15 )
        # ,plot.title = element_text( size = 20 )
        # ,plot.subtitle = element_text( size = 15 )
        ,plot.caption = element_text( hjust = 0, face = "italic" )
      )
    # Save plot, stratified by candidate factor.
    ggsave(
      plot = p
      ,filename =
        paste0(
          "Plots/HSRUK20026_poster/plot__distribution_of_si_stratified_by_payband__"
          ,gsub(roles[ i_role ], pattern = "/", replacement = " & ")
          ,".png"
        )
      ,dpi = 300
      ,width = 13.5
      ,height = 16
      ,units = "cm"
    )
    
  } else { 
    message(
      paste0(
        '\tInsufficient data to plot for '
        ,roles[ i_role ]
        ,'.'
      )
    )
  }
}

# Michaela requested a null-hypothesis significance test for the stability-index
# values across pay bands. The Friedman test is an appropriate test because
# it is non-parametric (which handles the non-Gaussian distribution of values
# in each pay band) and it handles within-subject dependence (which is present 
# because each Trust has data for each pay-band group.
# The results of the test are p < 0.0001, which leads us to infer that Trusts'
# stability-index values have some consistent ranking across pay bands. Looking 
# at the plot suggests that the ranking is that higher pay bands have higher 
# stability-index values.
# I also ran pairwise Wilcoxon tests to assess whether each pairwise set of the
# differences in stability-index values are symmetrical around 0. If the
# differences are symmetrical, then there is no evidence of systematic differences
# The results suggest that, assuming a level of significance of 0.05, there are
# systematic differences between almost all pay bands except the Band 8b-Band 9
# comparison and the Band 8c-Band 9 comparison.
i_plot_data %>%
  dplyr::select( `Org code`, `Stability index`, `AfC band`) %>%
  tidyr::pivot_wider( names_from = `AfC band`, values_from = `Stability index`) %>%
  dplyr::arrange( `Org code` ) %>%
  as.matrix() %>%
  stats::friedman.test()
# Post-hoc test.
a <- i_plot_data %>% dplyr::select( `Org code`, `Stability index`, `AfC band`)
pairwise.wilcox.test(
  a$`Stability index`
  ,a$`AfC band`
  ,p.adj = "bonf"
)
rm( a )

# ----

#################################
## Stability Index choropleth. ##
#################################
# ----
# Load the Trust-catchment shape file.
## https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/folder/170908955104
tc_shp <- sf::st_read( "../../Data/FPTP_AllAd22_Full.shp" )

# Append the names of the Trusts because the SHP file does not contain any
# meta-data.
## I need to colour one catchment area at a time and compare it with the
## PowerBI dashboard to figure out which Trust each area refers to because
## these areas are not labelled.
## The dashboard is at https://app.powerbi.com/view?r=eyJrIjoiODZmNGQ0YzItZDAwZi00MzFiLWE4NzAtMzVmNTUwMThmMTVlIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9
## Note that this dashboard was made in 2022. A more-up-to-date dashboard is
## available at https://app.powerbi.com/view?r=eyJrIjoiYzg2ODkzODgtMDA3OS00MGVhLTgyNWQtZjg1ZmQ2YWNlY2ZhIiwidCI6ImVlNGUxNDk5LTRhMzUtNGIyZS1hZDQ3LTVmM2NmOWRlODY2NiIsImMiOjh9
## but the interface is not as helpful for what I'm trying to do.
tc_shp <-
  dplyr::bind_cols(
    tc_shp 
    ,`Organisation name` =
      c(
        'Manchester University NHS Foundation Trust'
        ,'South Tyneside And Sunderland NHS Foundation Trust'
        ,'University Hospitals Dorset NHS Foundation Trust'
        ,'Isle of Wight NHS Trust'
        ,'Barts Health NHS Trust'
        ,'London North West University Healthcare NHS Trust'
        ,'Royal Surrey County Hospital NHS Foundation Trust'
        # # Yeovil merged with Somerset NHS FT in April 2023.
        ,'Yeovil District Hospital NHS Foundation Trust' 
        ,'University Hospitals Bristol and Weston NHS Foundation Trust'
        ,'Torbay and South Devon NHS Foundation Trust'
        ,'Bradford Teaching Hospitals NHS Foundation Trust'
        ,'Mid and South Essex NHS Foundation Trust'
        ,'Royal Free London NHS Foundation Trust'
        ,'North Middlesex University Hospital NHS Trust'
        ,'Hillingdon Hospitals NHS Foundation Trust'
        ,'Kingston and Richmond NHS Foundation Trust'
        ,'Dorset County Hospital NHS Foundation Trust'
        ,'Walsall Healthcare NHS Trust'
        ,'Wirral University Teaching Hospital NHS Foundation Trust'
        ,'Mersey and West Lancashire Teaching Hospitals NHS Trust'
        ,'Mid Cheshire Hospitals NHS Foundation Trust'
        # # Northern Devon Healthcare existed in early 2022 but not after April.
        # # In April 2022, it merged with Royal Devon University Healthcare NHS
        # # Foundation Trust.
        ,'Northern Devon Healthcare NHS Trust'
        ,'Bedfordshire Hospitals NHS Foundation Trust'
        ,'York and Scarborough Teaching Hospitals NHS Foundation Trust'
        ,'Harrogate and District NHS Foundation Trust'
        ,'Airedale NHS Foundation Trust'
        ,'Queen Elizabeth Hospital King\'s Lynn NHS Foundation Trust'
        ,'Royal United Hospitals Bath NHS Foundation Trust'
        ,'Milton Keynes University Hospital NHS Foundation Trust'
        ,'East Suffolk and North Essex NHS Foundation Trust'
        ,'Frimley Health NHS Foundation Trust'
        ,'Royal Cornwall Hospitals NHS Trust'
        ,'Liverpool University Hospitals NHS Foundation Trust'
        ,'Barking, Havering and Redbridge University Hospitals NHS Trust'
        ,'Barnsley Hospital NHS Foundation Trust'
        ,'Rotherham NHS Foundation Trust'
        ,'Chesterfield Royal Hospital NHS Foundation Trust'
        ,'North West Anglia NHS Foundation Trust'
        ,'James Paget University Hospitals NHS Foundation Trust'
        ,'West Suffolk NHS Foundation Trust'
        ,'Cambridge University Hospitals NHS Foundation Trust'
        ,'Somerset NHS Foundation Trust'
        ,'Royal Devon University Healthcare NHS Foundation Trust'
        ,'University Hospital Southampton NHS Foundation Trust'
        ,'Sheffield Teaching Hospitals NHS Foundation Trust'
        ,'Portsmouth Hospitals University NHS Trust'
        ,'Royal Berkshire NHS Foundation Trust'
        ,'Guy\'s and St Thomas\' NHS Foundation Trust'
        ,'Lewisham and Greenwich NHS Trust'
        ,'Croydon Health Services NHS Trust'
        ,'St George\'s University Hospitals NHS Foundation Trust'
        ,'South Warwickshire University NHS Foundation Trust'
        ,'University Hospitals of North Midlands NHS Trust'
        ,'Northern Lincolnshire and Goole NHS Foundation Trust'
        ,'East Cheshire NHS Trust'
        ,'Countess of Chester Hospital NHS Foundation Trust'
        ,'King\'s College Hospital NHS Foundation Trust'
        ,'Sherwood Forest Hospitals NHS Foundation Trust'
        ,'University Hospitals Plymouth NHS Trust'
        ,'University Hospitals Coventry and Warwickshire NHS Trust'
        ,'Whittington Health NHS Trust'
        ,'Royal Wolverhampton NHS Trust'
        ,'Wye Valley NHS Trust'
        ,'George Eliot Hospital NHS Trust'
        ,'Norfolk and Norwich University Hospitals NHS Foundation Trust'
        ,'Northern Care Alliance NHS Foundation Trust'
        ,'Bolton NHS Foundation Trust'
        ,'Tameside and Glossop Integrated Care NHS Foundation Trust'
        ,'Great Western Hospitals NHS Foundation Trust'
        ,'Hampshire Hospitals NHS Foundation Trust'
        ,'Dartford and Gravesham NHS Trust'
        ,'Dudley Group NHS Foundation Trust'
        ,'North Cumbria Integrated Care NHS Foundation Trust'
        ,'Kettering General Hospital NHS Foundation Trust'
        ,'Northampton General Hospital NHS Trust'
        ,'Salisbury NHS Foundation Trust'
        ,'Doncaster and Bassetlaw Teaching Hospitals NHS Foundation Trust'
        ,'Medway NHS Foundation Trust'
        ,'Chelsea and Westminster Hospital NHS Foundation Trust'
        ,'Princess Alexandra Hospital NHS Trust'
        ,'Homerton Healthcare NHS Foundation Trust'
        ,'Gateshead Health NHS Foundation Trust'
        ,'Leeds Teaching Hospitals NHS Trust'
        ,'Wrightington, Wigan and Leigh NHS Foundation Trust'
        ,'University Hospitals Birmingham NHS Foundation Trust'
        ,'University College London Hospitals NHS Foundation Trust'
        ,'Newcastle Upon Tyne Hospitals NHS Foundation Trust'
        ,'Gloucestershire Hospitals NHS Foundation Trust'
        ,'Northumbria Healthcare NHS Foundation Trust'
        ,'University Hospitals of Derby and Burton NHS Foundation Trust'
        ,'Oxford University Hospitals NHS Foundation Trust'
        ,'Ashford and St. Peter\'s Hospitals NHS Foundation Trust'
        ,'Surrey and Sussex Healthcare NHS Trust'
        ,'South Tees Hospitals NHS Foundation Trust'
        ,'University Hospitals of Morecambe Bay NHS Foundation Trust'
        ,'North Bristol NHS Trust'
        ,'Epsom and St Helier University Hospitals NHS Trust'
        ,'East Kent Hospitals University NHS Foundation Trust'
        ,'North Tees and Hartlepool NHS Foundation Trust'
        # # Southport and Ormskirk existed in in 2022 but not after 2023. On 1
        # # July 2023, the Trust merged with St Helens and Knowsley Teaching
        # # Hospitals NHS Trust to form Mersey and West Lancashire Teaching
        # # Hospitals NHS Trust.
        ,'Southport and Ormskirk Hospital NHS Trust' 
        ,'Hull University Teaching Hospitals NHS Trust'
        ,'United Lincolnshire Teaching Hospitals NHS Trust'
        ,'University Hospitals of Leicester NHS Trust'
        ,'Maidstone and Tunbridge Wells NHS Trust'
        ,'West Hertfordshire Teaching Hospitals NHS Trust'
        ,'East and North Hertfordshire NHS Trust'
        ,'Stockport NHS Foundation Trust'
        ,'Worcestershire Acute Hospitals NHS Trust'
        ,'Warrington and Halton Teaching Hospitals NHS Foundation Trust'
        ,'Calderdale and Huddersfield NHS Foundation Trust'
        ,'Nottingham University Hospitals NHS Trust'
        ,'East Sussex Healthcare NHS Trust'
        ,'Mid Yorkshire Teaching NHS Trust'
        ,'Sandwell and West Birmingham Hospitals NHS Trust'
        ,'Blackpool Teaching Hospitals NHS Foundation Trust'
        ,'Lancashire Teaching Hospitals NHS Foundation Trust'
        ,'County Durham and Darlington NHS Foundation Trust'
        ,'Buckinghamshire Healthcare NHS Trust'
        ,'East Lancashire Hospitals NHS Trust'
        ,'Shrewsbury and Telford Hospital NHS Trust'
        ,'Imperial College Healthcare NHS Trust'
        ,'University Hospitals Sussex NHS Foundation Trust'
        ) %>% tolower()
    )

# Join to 'df_churn_within_NHS_Grade'.
choropleth_churn_data <-
  df_churn_within_NHS_Grade %>% #dplyr::mutate(year_end=substr(Period, nchar(Period)-3+1, nchar(Period)), year_end=dplyr::if_else(year_end == "503", "2025", "0000")) %>% 
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` == "All AfC bands"
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for all professions combined.
    ,`Care setting` == "All care settings"
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  ) %>% 
  dplyr::select(
    `Organisation name`
    ,`Stability index`
  )
write.csv( choropleth_churn_data, "./Tables/Trusts_in_SI_analysis.csv")

choropleth_churn_data_band5only <-
  df_churn_within_NHS_Grade %>%
  dplyr::filter(
    # Remove SI = 0%.
    !`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for all professions combined.
    ,`Care setting` == "All care settings"
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
    # Filter for pay band 5, only.
    ,`AfC band` == "Band 5"
  ) %>% 
  dplyr::select(
    `Organisation name`
    ,SI_band5only = `Stability index`
  )
tc_shp <-
  tc_shp %>%
  dplyr::left_join(
    choropleth_churn_data
    ,by = join_by( `Organisation name` )
  ) %>%
  dplyr::left_join(
    choropleth_churn_data_band5only
    ,by = join_by( `Organisation name` )
  ) 
tc_shp <-
  data.frame(
    old =
      c(
        'Yeovil District Hospital NHS Foundation Trust' 
        ,'Northern Devon Healthcare NHS Trust'
        ,'Southport and Ormskirk Hospital NHS Trust' 
      ) 
    ,new = 
      c(
        'Somerset NHS Foundation Trust'
        ,'Royal Devon University Healthcare NHS Foundation Trust'
        ,'Mersey and West Lancashire Teaching Hospitals NHS Trust'
      ) 
  ) %>%
  dplyr::mutate( across( everything(), tolower ) ) %>%
  dplyr::left_join(
    choropleth_churn_data
    ,by = join_by( new == `Organisation name` )
  ) %>%
  dplyr::left_join(
    choropleth_churn_data_band5only
    ,by = join_by( new == `Organisation name` )
  ) %>%
  dplyr::select( -new ) %>% 
  dplyr::right_join(
    tc_shp
    ,by = join_by( old == `Organisation name` )
    ,suffix = c( "", ".y" )
  ) %>%
    dplyr::mutate(
      `Stability index` = 
        dplyr::if_else(
          is.na( `Stability index` )
          ,`Stability index.y`
          ,`Stability index`
        )
      ,SI_band5only = 
        dplyr::if_else(
          is.na( SI_band5only )
          ,SI_band5only.y
          ,SI_band5only
        )
      ) %>%
  dplyr::select( -ends_with( ".y" ) ) %>%
  dplyr::rename( `Trust name` = old ) %>%
  tidyr::pivot_longer(
    cols = c( `Stability index`, SI_band5only )
    ,values_to = "StabilityIndex" 
    ,names_to = "payBand"
    ) %>%
    dplyr::mutate(
      payBand = 
        dplyr::if_else(
          payBand == "Stability index"
          ,"All pay bands"
          ,"Band-5, only"
          )
      )

# Plot map: Stability index for all pay bands.
p <-
  tc_shp %>%
  dplyr::filter(
    payBand == "All pay bands"
    ) %>%
    ggplot() +
    geom_sf(
      aes(
        geometry = geometry
        ,fill = StabilityIndex
        )
      ) +
    scale_fill_continuous( limits = c( 0.5, 1 ) ) +
    labs(
      title = 
        stringr::str_wrap(
          paste0(
            "Stability index of non-specialist acute Trusts' catchment areas in "
            ,"England"
            )
          ,70
        )
      ,subtitle = 
        stringr::str_wrap(
          paste0(
            "\u2022 Conclusion: Stability index is similarly high among"
            ,"\n  all non-specialist acute Trusts in England."
          )
          ,50
        )
      ,caption = paste0( "Year = ", year_of_interest, "." )
      ) +
    theme_bw() +
    theme(
      axis.text = element_blank()
    )

# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__si_map_allPayBands.png"
    )
  ,dpi = 300
  ,width = 13.5
  ,height = 16
  ,units = "cm"
)

# Plot map: Stability index for pay band 5, only.
p <-
  tc_shp %>%
  dplyr::filter(
    payBand == "Band-5, only"
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = StabilityIndex
    )
  ) +
  scale_fill_continuous( limits = c( 0.5, 1 ) ) +
  labs(
    title = 
      stringr::str_wrap(
        paste0(
          "Stability index of non-specialist acute Trusts' catchment areas in "
          ,"England:\nPay-band 5, only."
        )
        ,70
      )
    ,subtitle =
      stringr::str_wrap(
        paste0(
          "\u2022 Conclusion: Stability index ranges from moderate to high across "
          ,"\nall non-specialist acute Trusts in England."
        )
        ,60
      )
    ,caption = paste0( "Year = ", year_of_interest, "." )
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank()
  )

# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__si_map_band5only.png"
    )
  ,dpi = 300
  ,width = 13.5
  ,height = 16
  ,units = "cm"
)

# Plot map: Side-by-side comparison of all pay bands -vs- band 5s.
p <-
  tc_shp %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = StabilityIndex
    )
  ) +
  scale_fill_continuous( limits = c( 0.5, 1 ) ) +
  facet_wrap( ~payBand ) +
  labs(
    title = 
      stringr::str_wrap(
        paste0(
          "Stability index of non-specialist acute Trusts' catchment areas in "
          ,"England:\nComparison of all pay-bands versus band-5, only."
        )
        ,100
      )
    # ,subtitle = 
    #   stringr::str_wrap(
    #     paste0(
    #       "\u2022 Conclusion: Stability index is similarly high among"
    #       ,"\n  all non-specialist acute Trusts in England."
    #     )
    #     ,100
    #   )
    ,caption = paste0( "Year = ", year_of_interest, "." )
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank()
    ,strip.text.x = element_text( size = 15 )
  )

# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__si_map_SideBySide.png"
    )
  ,dpi = 300
  ,width = 20
  ,height = 16
  ,units = "cm"
)
# ----

##########################################################
# NHST of stability index between Band-5s and all bands. #
##########################################################
# ----
test_data <-
  ls_churn_within_NHS[[1]] %>%
  dplyr::filter(
    # Remove pay-bands that are not of interest.
    `AfC band` %in% c( 'All AfC bands', 'Band 5' )
    # Filter for all professions
    ,`Care setting` == "All care settings"
    # Remove SI = 0%.
    ,!`Stability index` %in% c(0)
    # Select year of interest
    ,year_end %in% year_of_interest
    # Only use data for the non-specialist acute Trusts.
    ,`Cluster group` == "Acute"
    ,!stringr::str_detect( `Benchmark group`, "Specialist" )
  )
pairwise.wilcox.test(
  test_data$`Stability index`
  ,test_data$`AfC band`
  ,p.adj = "bonf"
) 

wilcox.test(
  x = test_data %>% dplyr::filter( `AfC band` == "All AfC bands" ) %>% dplyr::pull( `Stability index` )
  ,y = test_data %>% dplyr::filter( `AfC band` == "Band 5" ) %>% dplyr::pull( `Stability index` )
  ,alternative = "two.sided"
  ,mu = 0
  ,paired = TRUE
  ,exact = TRUE
  ,correct = TRUE,
  conf.int = TRUE
  ,conf.level = 0.95
  )
# V = 6795, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   0.03951671 0.05425882
# sample estimates:
#   (pseudo)median 
# 0.04653201 
test_data %>%
  dplyr::select( `Stability index`, `AfC band` ) %>%
  dplyr::reframe(
    mdn = median( `Stability index` )
    ,iqr = IQR( `Stability index` )
    ,.by = `AfC band`
    )
# # A tibble: 2 × 3
# `AfC band`          mdn    iqr
# <chr>              <dbl>  <dbl>
#   1 All AfC bands 0.861 0.0394
#   2 Band 5        0.818 0.0774

# ----

#########################################################
## Stability index vs Positive staff-survey responses. ##
#########################################################
# ----
year_of_interest <- 2024 # Can be a vector of years

# Collate dataset.
# # The dataset only needs:
# # - Stability index.
# # - The binarised staff-survey responses.
# # - The organisation code to link everything.
# # The dataset should also only contain data for the year of interest, and only
# # contain data for the all-AHPs rows rather than specific professions. This
# # second requirement is an issue because the staff survey only provides
# # profession-specific responses.

part_a <-
  df_staff_survey_main %>% 
  dplyr::filter( ss_year == year_of_interest ) %>%
  dplyr::select( `Org code`, contains( "binary" ) ) %>%
  tidyr::pivot_longer(
    cols = contains('ss_')
    ,names_to = 'Question'
    ,values_to = 'Response'
  ) %>% 
  dplyr::filter( !is.na( Response ) ) %>%
  dplyr::mutate(
    Response =
      dplyr::if_else(
          Response %in% c( "Agree", "Yes", "Satisfied")
          ,"Agreement"
          ,"Disagreement"
      )
  )
plot_data <-
  dplyr::left_join(
    part_a %>%
      dplyr::reframe(
        n = n()
        ,.by = everything()
      ) 
    ,part_a %>%
      dplyr::reframe(
        N = n()
        ,.by = c( `Org code`, Question  )
      ) 
    ,by = join_by( `Org code`, Question )
  ) %>%
  dplyr::filter( Response == "Agreement" ) %>%
  dplyr::mutate( Propr = n/N ) %>%
  dplyr::select( `Org code`, Question, Propr ) %>%
  dplyr::arrange( `Org code`, Question ) %>%
  dplyr::inner_join(
    df_churn_within_NHS_Grade %>%
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        `AfC band` == "All AfC bands"
        # Remove SI = 0%.
        ,!`Stability index` %in% c(0)
        # Select year of interest
        ,year_end %in% year_of_interest
        # Only use data for all professions combined.
        ,`Care setting` == "All care settings"
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
      )  %>%
    dplyr::select( `Org code`, `Stability index` ) 
    ,by = join_by( `Org code` )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Question =
      stringr::str_match( Question, "_(.*?)_" )[2] %>%
      stringr::str_remove_all( pattern = "_")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( Question == q_num)
  ) %>%
  tidyr::drop_na()
rm( part_a )


p <-
  plot_data %>%
  ggplot(
    aes(
      x = `Stability index`
      ,y = Propr
    )) +
  geom_point() +
  labs(
    title =
      paste0(
        "Scatter plots of stability index (x-axis) and the proportion"
        ,"\nof responses agreeing with each panel's concept (y-axis)."
        )
    ,subtitle = paste0(
      "\u2022 Conclusion: There are no observable relationships between"
      ,"\n  stability index and responses to survey questions."
      )
    ,caption =
      paste0(
        "Data from NHS staff survey, ", year_of_interest, "."
      )
    ,x = "Stability Index\n(only showing values \u22650.5)"
    ,y = "Proportion of responses\nthat agree with the concept in the panel"
  ) +
  scale_x_continuous(
    limits = c( 0.5, 1 )
    ,breaks = c( 0.5, 0.75, 1 )
    ,labels = c( "0.5", "0.75", "1.0")
    ) +
  scale_y_continuous(
    limits = c( 0, 1 )
    ,breaks = c( 0, 0.5, 1.0 )
    ,labels = c( "0.0", "0.5", "1.0")
    ) +
  facet_wrap( ~q_positive_statement, labeller = label_wrap_gen( 19 ) ) +
  theme_bw() +
  theme(
    axis.text = element_text( size = 6 )
  )

# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__ss_vs_si.png"
    )
  ,dpi = 300
  ,width = 13.5
  ,height = 16
  ,units = "cm"
)
# ----

########################################################
## Mutual information between staff survey responses. ##
########################################################
# ----

# Calculate the MI score.
mi_scores <-
  df_main %>%
  dplyr::select(
    contains( 'ss_q' ) & contains( '_Likert' ) &
      !contains( "binary") & !contains( '26a' )
    ) %>%
  infotheo::mutinformation()

# Prepare the plot data.
plot_data <-
  ( mi_scores / diag( mi_scores ) ) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select( contains( q_lookup_outcomes ) ) %>%
  dplyr::filter_at( 1:ncol(.), all_vars(.!=1) ) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter( ss_q26c_LikertScore >0 ) %>%
  dplyr::rename(
    q2 = rowname
    ,scaled_MI = ss_q26c_LikertScore
    ) %>%
  dplyr::bind_cols(
    data.frame( q1 = rep( "q26c", nrow(.) ) )
    ,.
  ) %>%
  dplyr::arrange( q1, desc( scaled_MI ) ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    q2 = stringr::str_match( q2, "_(.*?)_" )[2]
  ) %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q1 == q_num )
  ) %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q2 == q_num )
  ) %>%
  dplyr::rename(
    q1_word = q_positive_statement.x
    ,q2_word = q_positive_statement.y
    ) %>%
  dplyr::select( -starts_with( "q_word" ) ) 

# Prepare the plot.
p <-
  plot_data %>%
  ggplot() +
  geom_bar(
    aes(
      x = scaled_MI
      ,y = forcats::fct_reorder( q2_word, scaled_MI  )
      )
    ,stat = "identity"
    ) +
  scale_x_continuous( limits = c( 0, 1 ) ) +
  # scale_y_discrete(
  #   labels = forcats::fct_reorder( plot_data$q2_word, -plot_data$scaled_MI )
  #   ) +
  labs(
    title =
      paste0(
        "Associations between staff survey"
        ,"\nresponses and staff's intention to leave."
      )
    ,subtitle =
      paste0(
      "Conclusion: There is low agreement between"
      ,"\nstaff's intention to leave and other factors"
      ,"\nof interest."
      )
    ,x = "Mutual information\n(as a proportion of theoretical maximum)"
    ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    )
  
# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__ss_mutual information.png"
    )
  ,dpi = 300
  ,width = 13.5
  ,height = 16
  ,units = "cm"
)

# ----

####################################################################
## Mutual information between staff survey responses - binarised. ##
####################################################################
# ----

# Calculate the MI score.
mi_scores <-
  df_main %>%
  dplyr::select(
    contains( 'ss_q' ) & contains( '_Likert' ) &
      contains( "binary") & !contains( '26a' )
  ) %>%
  infotheo::mutinformation()

# Prepare the plot data.
plot_data <-
  ( mi_scores / diag( mi_scores ) ) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select( contains( q_lookup_outcomes ) ) %>%
  dplyr::filter_at( 1:ncol(.), all_vars(.!=1) ) %>%
  tibble::rownames_to_column() %>%
  dplyr::filter( ss_q26c_LikertScore_binary >0 ) %>%
  dplyr::rename(
    q2 = rowname
    ,scaled_MI = ss_q26c_LikertScore_binary
  ) %>%
  dplyr::bind_cols(
    data.frame( q1 = rep( "q26c", nrow(.) ) )
    ,.
  ) %>%
  dplyr::arrange( q1, desc( scaled_MI ) ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    q2 = stringr::str_match( q2, "_(.*?)_" )[2]
  ) %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q1 == q_num )
  ) %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q2 == q_num )
  ) %>%
  dplyr::rename(
    q1_word = q_positive_statement.x
    ,q2_word = q_positive_statement.y
  ) %>%
  dplyr::select( -starts_with( "q_word" ) ) 

# Prepare the plot.
p <-
  plot_data %>%
  ggplot() +
  geom_bar(
    aes(
      x = scaled_MI
      ,y = forcats::fct_reorder( q2_word, scaled_MI  )
    )
    ,stat = "identity"
  ) +
  scale_x_continuous( limits = c( 0, 1 ) ) +
  scale_y_discrete( labels = function(x) stringr::str_wrap( x, width = 20 ) ) +
  labs(
    title =
      paste0(
        stringr::str_wrap(
          paste0(
            "Associations between staff survey responses and staff's intention "
            ,"to leave."
          )
          ,80
        )
        ,"\n(Binary transformation of data)"
      )
    ,subtitle =
      stringr::str_wrap(
        paste0(
          "Conclusion: There is low agreement between staff's intention to "
          ,"leave and other factors of interest."
        )
        ,80
      )
    ,x = "Mutual information\n(as a proportion of theoretical maximum)"
    ,caption =
      stringr::str_wrap(
        paste0(
          "NOTE: Original responses are on a 5-point scale. This plot refers to data"
          ,"\nwhere the middle responses are excluded, and lower and higher"
          ,"\nresponses are each amalagamated. The result is a binary variable."
          )
        ,90
      )
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_text( size = 10 )
  )

# Save the plot.
ggsave(
  plot = p
  ,filename =
    paste0(
      "Plots/HSRUK20026_poster/plot__ss_mutual information_binary.png"
    )
  ,dpi = 300
  ,width = 13.5
  ,height = 16
  ,units = "cm"
)

# ----