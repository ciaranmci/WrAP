# WrAP_Explore__staff_suvey_study.R
#
# The purpose of this script is to provide descriptive statistics and visual
# summaries of survery responses to a selection of questions in the NHS Staff
# Survey (https://www.nhsstaffsurveys.com/static/958853e094733756687a78aa3d8cb36c/NSS2025-Questionnaire.zip).
# The technical specification explaining how the questions are summarised is available at
# https://www.nhsstaffsurveys.com/static/ea079b722ad235a21b0356670766a33b/NHS-Staff-Survey-2025-Technical-Guide-V1.pdf).
#
# Our selected questions are:
#   - q2a I look forward to going to work.
#       Never-Rarely-Sometimes-Often-Always
#   - q3i There are enough staff at this organisation for me to do my job properly.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q4c My level of pay.
#       Very dissatisfied-Dissatisfied-Neither satisfied nor dissatisfied-Satisfied-Very satisfied
#   - q4d The opportunities for flexible working patterns.
#       Very dissatisfied-Dissatisfied-Neither satisfied nor dissatisfied-Satisfied-Very satisfied
#   - q5a I have unrealistic time pressures.
#       Never-Rarely-Sometimes-Often-Always
#   - q9a My immediate manager encourages me at work
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q9i My immediate manager takes effective action to help me with any problems I face
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q11c During the last 12 months have you felt unwell as a result of work related stress?
#       Yes-No
#   - q21 I think that my organisation respects individual differences (e.g. cultures, working styles, backgrounds, ideas, etc).
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q24d I feel supported to develop my potential.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q25d If a friend or relative needed treatment I would be happy with the standard of care provided by this organisation.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q25f If I spoke up about something that concerned me I am confident my organisation would address my concern.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q26a I often think about leaving this organisation.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#   - q26c As soon as I can find another job, I will leave this organisation.
#       Strongly disagree-Disagree-Neither agree nor disagree-Agree-Strongly agree
#


#####################
## Load libraries. ##
#####################
# ----
if( !"pacman" %in% installed.packages() ){ install.packages( "pacman" ) }
pacman::p_load(
  curl
  ,haven
  ,info
  ,sf
  ,tidyverse
)

# ----

#################
## Requisites. ##
#################
# ----

# Set the year of interest.
year_of_interest <- 2025

# Set storage locations.
dir.create( "./Tests/Sept 2026 meeting", recursive = TRUE )
dir.create( "./Tables/Sept 2026 meeting", recursive = TRUE )
dir.create( "./Plots/Sept 2026 meeting", recursive = TRUE )
dir.create( "./Models/Sept 2026 meeting", recursive = TRUE )
dir.create( "./Processed datasets/Sept 2026 meeting", recursive = TRUE )

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
df_staff_survey_main <- 
  df_staff_survey_main %>%
  dplyr::mutate( across( contains( 'org_name' ), tolower ) ) %>%
  dplyr::filter( ss_year == year_of_interest ) %>%
  dplyr::mutate(
    `Care setting` = 
      dplyr::if_else(
        `Care setting` == 'Operating Theatres'
        ,'Operating Department Practitioners'
        ,`Care setting`
        )
    )

# Create a dataset with one row per Trust, and columns showing the proportion
# of respondents that answered Agree or Strongly Agree.
df_staff_agree <-
  df_staff_survey_main %>%
  dplyr::select(
    -contains( "binary" ), -contains( "Response" )
    ,-c( job_role, area_of_work, `Org code`, ss_year )
    ) %>% 
  dplyr::mutate(
    across(
      contains("_LikertScore") 
      ,~dplyr::if_else( 
        stringr::str_detect( .x, pattern = "(Agree)|(Strongly agree)" )
        ,1
        ,0
      )
    )
  ) %>%
  tidyr::pivot_longer(
    cols = contains( "Likert" )
    ,names_to = "question"
    ) %>%
  dplyr::reframe(
    .by = c( org_name, `Care setting`, "question" )
    ,proportion_agree = sum( value, na.rm = TRUE ) / n()
    )
# ----


# ~~~~~~~~~~~~
# ~~ Tables ~~
# ~~~~~~~~~~~~

#########################
## Intention to leave. ##
#########################
# ----
tbl_agree <- 
  df_staff_agree %>%
  dplyr::filter( stringr::str_detect( question, pattern = "26c" ) )%>%
  # Join the count of responses.
  dplyr::left_join(
    df_staff_survey_main %>%
      dplyr::reframe( .by = c( org_name, `Care setting` ), n_response = n() )
    ,by = join_by( org_name, `Care setting` )
  ) %>% 
  # Join the count of staff. I need to make use of the count at the start and the
  # end of the year because some times there are no staff at the start and
  # sometimes there are no staff at the end. If both counts are non-zero, then I
  # will use the count at the start of the year. I also check the count of
  # responses so that I don't choose a staff count that is less than it. 
  dplyr::inner_join(
    df_churn_within_NHS_Grade %>%
      dplyr::mutate( `Organisation name` = tolower( `Organisation name` ) ) %>%
      dplyr::filter(
        `AfC band` == "All AfC bands"
        ,stringr::str_detect( Period, pattern = "2025" ) )  %>%
      dplyr::distinct(
        `Organisation name`, `Care setting`
        ,`Denominator at start of period`, `Denominator at end of period`
        )
    ,by = join_by( org_name == `Organisation name`, `Care setting` )
    ,relationship = "many-to-many"
    ) %>% 
  dplyr::mutate(
    n_staff = 
      dplyr::case_when(
        `Denominator at start of period` == 0 ~ `Denominator at end of period`
        ,`Denominator at end of period` == 0 ~ `Denominator at start of period`
        ,`Denominator at start of period` < n_response ~ `Denominator at end of period`
        ,`Denominator at end of period` < n_response ~ `Denominator at start of period`
        ,.default = `Denominator at start of period`
      )
    ,proportion_response = n_response / n_staff
  ) %>% 
  dplyr::rename( Trust = org_name, Profession = `Care setting` ) %>%
  dplyr::select( -contains( "Denominator" ) ) %>%
  dplyr::relocate( contains( "agree" ), .after = contains( "ion_res" ) )

write.csv( tbl_agree, "Tables/Sept 2026 meeting/table_proportion_who_agree.csv")
# ----

# ~~~~~~~~~~~
# ~~ Plots ~~
# ~~~~~~~~~~~

################################################################
## Choropleth of proportion agreeing with intention to leave. ##
################################################################
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
    ,orgname =
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

tc_shp <-
  tc_shp %>%
  # Filter for Trusts of interest.
  dplyr::inner_join(
    df_churn_within_NHS_Grade %>%
      dplyr::filter(
        # Remove pay-bands that are not of interest.
        `AfC band` == "All AfC bands"
        # Select year of interest
        ,year_end %in% year_of_interest
        # Only use data for all professions combined.
        ,`Care setting` == "All care settings"
        # Only use data for the non-specialist acute Trusts.
        ,`Cluster group` == "Acute"
        ,!stringr::str_detect( `Benchmark group`, "Specialist" )
      ) %>%
      dplyr::distinct( orgname = `Organisation name` )
    ,by = join_by( orgname )
  ) %>%
  # Join staff-survey data.
  dplyr::left_join(
    df_staff_agree %>% dplyr::filter( question == "ss_q26c_LikertScore" )
    ,by = join_by( orgname == org_name )
    ,relationship = "one-to-many"
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
    df_staff_agree
    ,by = join_by( new == org_name )
  ) %>%
  dplyr::select( -new ) %>%
  dplyr::bind_rows( tc_shp )

# Plot map: Stability index for all pay bands.
p <-
  tc_shp %>%
  dplyr::filter( `Care setting` %in% professions_of_interest ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry
      ,fill = proportion_agree
    )
  ) +
  facet_wrap( ~`Care setting`, labeller = label_wrap_gen( 20 )  ) +
  labs(
    fill = "Proportion"
    # title = 
    #   stringr::str_wrap(
    #     paste0(
    #       "Stability index of non-specialist acute Trusts' catchment areas in "
    #       ,"England for the year ending "
    #       ,year_of_interest, "."
    #     )
    #     ,65
    #   )
    # ,caption = "Range of stability index limited to 0.5-1."
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank()
    ,axis.text = element_blank()
    ,plot.title = element_text( size = 20 )
    ,strip.text.x = element_text( size = 15 )
    ,legend.position = "bottom"
  )


# Save the plot.
ggsave(
  plot = p
  ,filename = "Plots/Sept 2026 meeting/plot__map_of_intention_to_leave.png"
  ,dpi = 300
  ,width = 20
  ,height = 20
  ,units = "cm"
)

# ----

###############################################
## Proportion who agree, over the two years. ##
###############################################
# ----

# ----


###########################################################
## Plot distribution of Likert scores for each question. ##
###########################################################
# ----
# Make the plots.
for( i_role in 1:length( roles )+1 )
{
  if( i_role < 13 )
  {
    i_plot_data <-
      df_staff_survey_main %>%
      dplyr::filter( `Care setting` == roles[ i_role ] ) %>%
      tidyr::drop_na()
  } else {
    i_plot_data <- df_staff_survey_main %>% tidyr::drop_na()
  }
  
  for( i_q_name in 1:nrow( q_lookup ) )
  { 
    ij_plot_data <-
      i_plot_data %>%
      dplyr::select(
        contains( "year" )
        ,contains( q_lookup[ i_q_name, "q_num" ] ) & contains( "Likert" )
      ) %>%
      `colnames<-`( c("year", "value" ) )
  
    if ( nrow( ij_plot_data ) > 0 )
    {
      p <-
        ij_plot_data %>%
        ggplot( aes( x = value ) ) +
        geom_bar( aes( fill = as.factor( year ) ), position = "dodge" ) +
        labs(
          title =
            paste0(
              'Distributions of Staff Survey Scores\nfor '
              ,ifelse( i_role < 13, roles[ i_role ], "all professions" )
              ,", "
              ,q_lookup[ i_q_name, "q_num" ]
              ,"."
            )
          ,subtitle =
              paste0(
                stringr::str_wrap(
                  paste0(
                    "\u2022 Question wording = \""
                  ,q_lookup %>%
                    dplyr::filter( q_num == q_lookup[ i_q_name, "q_num" ]) %>%
                     dplyr::select( q_word )
                  ,"\""
                  )
                  ,width = 75
                )
                ,"\n\u2022 Using individuals' responses rather than Trust-level summary."
              )
          ,y = "Count"
          ,fill = "Year"
        ) +
        scale_fill_grey(start = 0.2, end = 0.8) +
        scale_x_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
        theme_bw() +
        theme(
          axis.text = element_text( size = 10 )
          ,axis.title.x = element_blank()
          ,plot.title = element_text( size = 20 )
          ,plot.subtitle = element_text( size = 15 )
        )
      ggsave(
        plot = p
        ,filename =
          paste0(
            "Plots/Staff survey/"
            ,"Column charts/"
            ,"plot__ss_columns_"
            ,ifelse(
              i_role < 13
              ,gsub(roles[ i_role ], pattern = "/", replacement = "&")
              ,"all professions"
              )
            ,"_"
            ,q_lookup[ i_q_name, "q_num" ]
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
    } # End of IF
  } # End of first questions FOR
  
  for( i_q_name in 1:nrow( q_lookup ) )
  { 
    ij_plot_data <-
      i_plot_data %>%
      dplyr::select(
        contains( "year" )
        ,contains( q_lookup[ i_q_name, "q_num" ] ) & contains( "Likert" ) & contains( "binary" )
      ) %>%
      `colnames<-`( c("year", "value" ) )
    
    if( ncol( ij_plot_data ) != 2 ){ next }
    
    if ( nrow( ij_plot_data ) > 0 )
    {
      p <-
        ij_plot_data %>%
        ggplot( aes( x = value ) ) +
        geom_bar( aes( fill = as.factor( year ) ), position = "dodge" ) +
        labs(
          title =
            paste0(
              'Distributions of Staff Survey Scores\nfor '
              ,ifelse( i_role < 13, roles[ i_role ], "all professions" )
              ,", "
              ,q_lookup[ i_q_name, "q_num" ]
              ,"."
            )
          ,subtitle =
            paste0(
              stringr::str_wrap(
                paste0(
                  "\u2022 Question wording = \""
                  ,q_lookup %>%
                    dplyr::filter( q_num == q_lookup[ i_q_name, "q_num" ]) %>%
                    dplyr::select( q_word )
                  ,"\""
                )
                ,width = 75
              )
              ,"\n\u2022 Using individuals' responses rather than Trust-level summary."
            )
          ,y = "Count"
          ,fill = "Year"
        ) +
        scale_fill_grey(start = 0.2, end = 0.8) +
        scale_x_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
        theme_bw() +
        theme(
          axis.text = element_text( size = 10 )
          ,axis.title.x = element_blank()
          ,plot.title = element_text( size = 20 )
          ,plot.subtitle = element_text( size = 15 )
        )
      ggsave(
        plot = p
        ,filename =
          paste0(
            "Plots/Staff survey/"
            ,"Column charts/"
            ,"plot__ss_columns_"
            ,ifelse(
              i_role < 13
              ,gsub(roles[ i_role ], pattern = "/", replacement = "&")
              ,"all professions"
            )
            ,"_"
            ,q_lookup[ i_q_name, "q_num" ]
            ,"_binary"
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
    } # End of IF
  } # End of second questions FOR
  
} # End of roles FOR
  
# ----

#######################################################
## Cross-plot the outcome questions with the others. ##
#######################################################
# ----
# Make and save the plots.
for( i_role in 1:length( roles )+1 )
{
  if( i_role < 13 )
  {
    i_plot_data <-
      df_staff_survey_main %>%
      dplyr::filter( `Care setting` == roles[ i_role ] ) %>%
      tidyr::drop_na()
    
  } else {
    i_plot_data <- df_staff_survey_main %>% tidyr::drop_na()
  }
  
  for( i_outcome_var in 1:length( q_lookup_outcomes ) )
  {
    # Select the name of the outcome variable.
    outcome_var_name <- q_lookup_outcomes[ i_outcome_var ]
    
    for( i_other_var in 1:length( q_lookup_other ) )
    {
      # Select the name of the other variable.
      other_var_name <- q_lookup_other[ i_other_var ]
      
      # Collate the data for plotting.
      ijk_plot_data <-
        i_plot_data %>%
        dplyr::select(
          contains( outcome_var_name ) & contains( "Likert") & !contains( "binary" )
          ,contains( other_var_name ) & contains( "Likert") & !contains( "binary" )
        ) %>%
        `colnames<-`( c( outcome_var_name, other_var_name) ) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise( n = n() )
      
      # Make the plot.
      p <-
        ijk_plot_data %>%
        ggplot() +
        geom_point(
          aes( x = !!( sym( other_var_name ) ) , y = !!( sym( outcome_var_name ) ), size = n )
        ) +
        labs(
          title =
            paste0(
              'Distributions of Staff Survey Scores\nfor '
              ,ifelse( i_role < 13, roles[ i_role ], "all professions" )
              ,", "
              ,outcome_var_name
              ," and "
              ,other_var_name
              ,"."
            )
          ,subtitle =
              "\u2022 Using individuals' responses rather than Trust-level summary."
           ,y =
            stringr::str_wrap(
              dplyr::pull( dplyr::filter( q_lookup, q_num == outcome_var_name ), combined )
              ,width = 75
            )
          ,x =
            stringr::str_wrap(
              dplyr::pull( dplyr::filter( q_lookup, q_num == other_var_name ), combined )
              ,width = 75
            )
          ,size = "Size"
        ) +
        scale_fill_grey(start = 0.2, end = 0.8) +
        scale_x_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
        scale_size_continuous( range = c( 1, 10 ) ) +
        theme_bw() +
        theme(
          axis.text = element_text( size = 10 )
          ,axis.title = element_text( size = 15 )
          ,plot.title = element_text( size = 20 )
          ,plot.subtitle = element_text( size = 15 )
        )
      # Save the plot.
      ggsave(
        plot = p
        ,filename =
          paste0(
            "Plots/Staff survey/"
            ,"Bubble charts/"
            ,outcome_var_name
            ,"/plot__ss_bubbles_"
            ,ifelse(
              i_role < 13
              ,gsub(roles[ i_role ], pattern = "/", replacement = "&")
              ,"all professions"
            )
            ,"_"
            ,outcome_var_name
            ,"_and_"
            ,other_var_name
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
      
    } # End of first inner FOR
    
    for( i_other_var in 1:length( q_lookup_other ) )
    {
      # Select the name of the other variable.
      other_var_name <- q_lookup_other[ i_other_var ]
      
      # Collate the data for plotting.
      ijk_plot_data <-
        i_plot_data %>%
        dplyr::select(
          contains( outcome_var_name ) & contains( "Likert") & contains( "binary" )
          ,contains( other_var_name ) & contains( "Likert") & contains( "binary" )
        ) %>%
        `colnames<-`( c( outcome_var_name, other_var_name) ) %>%
        dplyr::group_by_all() %>%
        dplyr::summarise( n = n() ) 
      
      # Check that both variables were binary.
      if( ncol( ijk_plot_data ) <3 ) { next }
      
      # Make the plot.
      p <-
        ijk_plot_data %>%
        ggplot() +
        geom_point(
          aes( x = !!( sym( other_var_name ) ) , y = !!( sym( outcome_var_name ) ), size = n )
        ) +
        labs(
          title =
            paste0(
              'Distributions of Staff Survey Scores\nfor '
              ,ifelse( i_role < 13, roles[ i_role ], "all professions" )
              ,", "
              ,outcome_var_name
              ," and "
              ,other_var_name
              ,"."
            )
          ,subtitle = "\u2022 Using individuals' responses rather than Trust-level summary."
          ,y =
            stringr::str_wrap(
              dplyr::pull( dplyr::filter( q_lookup, q_num == outcome_var_name ), combined )
              ,width = 75
            )
          ,x =
            stringr::str_wrap(
              dplyr::pull( dplyr::filter( q_lookup, q_num == other_var_name ), combined )
              ,width = 75
            )
          ,size = "Size"
        ) +
        scale_fill_grey(start = 0.2, end = 0.8) +
        scale_x_discrete( labels = function(x) str_wrap( x, width = 10 ) ) +
        scale_size_continuous( range = c( 1, 20 ) ) +
        theme_bw() +
        theme(
          axis.text = element_text( size = 10 )
          ,axis.title = element_text( size = 15 )
          ,plot.title = element_text( size = 20 )
          ,plot.subtitle = element_text( size = 15 )
        )
      # Save the plot.
      ggsave(
        plot = p
        ,filename =
          paste0(
            "Plots/Staff survey/"
            ,"Bubble charts/"
            ,outcome_var_name
            ,"/plot__ss_bubbles_"
            ,ifelse(
              i_role < 13
              ,gsub(roles[ i_role ], pattern = "/", replacement = "&")
              ,"all professions"
            )
            ,"_"
            ,outcome_var_name
            ,"_and_"
            ,other_var_name
            ,"_binary"
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
      
    } # End of second inner FOR
    
  } # End of middle FOR
} # End of outer FOR
# ----

#########################
## Mutual information. ##
#########################
# ----
# Make function.
fnc__saveMIscores <- function(
    data = NULL # A dataset as a dataframe object.
    ,... # A tidyverse selection, as a character string.
    ,save.suffix # An optional character string to append to the saved file.
                 # used for identification.
    )
{

  # Check arguments.
  if( is.null( data ) ) stop( "The `data` argument has not been specified.")
  if( !hasArg( save.suffix ) ) { save.suffix <- "" }
  
  # Calculate the MI score.
  mi_scores <-
    data %>%
    dplyr::select( ... ) %>%
    infotheo::mutinformation()
  
  # Prepare the output
  output <-
    ( mi_scores / diag( mi_scores ) ) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( contains( q_lookup_outcomes ) ) %>%
    dplyr::filter_at( 1:ncol(.), all_vars(.!=1) ) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(
      cols = 2:3
      ,names_to = "var_of_interest"
      ,values_to = "scaled_MI"
    ) %>%
    dplyr::filter( scaled_MI >0 ) %>%
    dplyr::select(
      q1 = var_of_interest
      ,q2 = rowname
      ,scaled_MI
    ) %>%
    dplyr::arrange( -scaled_MI ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      q1 = stringr::str_match( q1, "_(.*?)_" )[2]
      ,q2 = stringr::str_match( q2, "_(.*?)_" )[2]
    ) %>%
    dplyr::left_join(
      q_lookup %>% dplyr::select( -combined )
      ,by = join_by( q1 == q_num )
    ) %>%
    dplyr::left_join(
      q_lookup %>% dplyr::select( -combined )
      ,by = join_by( q2 == q_num )
    ) %>%
    dplyr::rename( q1_word = q_word.x, q2_word = q_word.y )
  
  # Maybe calculate the odds ratio to help indicate the direction of relationship
  for( i in 1:nrow( output ) )
  {
    v1 <- output$q1[ i ]
    v2 <- output$q2[ i ]
    
    
    i_table <-
      data %>%
      dplyr::select( ... ) %>%
      dplyr::select( contains( v1 ) | contains( v2 ) ) %>%
      table() 
  
    if( ( nrow( i_table ) >2 ) | ( ncol( i_table ) >2 ) )
    { next } else{
      i_val <-
        i_table %>%
        as.data.frame() %>%
        dplyr::select( Freq ) %>%
        dplyr::mutate( col_name = 1:4) %>%
        tidyr::pivot_wider(
          names_from = col_name
          ,values_from = Freq
        ) %>%
        dplyr::mutate(
          relationship = ( `1` * `4` ) / ( `2` * `3` )
          ,relationship = dplyr::if_else( relationship > 1, "Agree", "Disagree")
          ,.keep = "none"
        ) %>%
        dplyr::pull()
      
      if( !"relationship" %in% colnames( output ) )
      { output$relationship <- character( nrow(output) ) }
      
      output$relationship[ i ] <- i_val
        
    }
      
  }
  
    
  # Save.
  write.csv(
    output
    ,paste0(
      "Questions ranked by mutual information score"
      ,save.suffix
      ,".csv"
    )
  )
}

# Run function.
# ## Look at responses in which we maintain all points on the Likert scale.
fnc__saveMIscores(
  data = df_staff_survey_main
  ,contains( 'ss_q' ) & contains( '_Likert' ) & !contains( "binary")
  ,save.suffix = "_Likert"
)
# ## Look at responses in which we reduce the points on the Likert scale to
# ## positive and negative.
fnc__saveMIscores(
  data = df_staff_survey_main
  ,contains( 'binary' )
  ,save.suffix = "_BinaryLikert"
)

# ----

#####################################
## Covariate-adjusted odds ratios. ##
#####################################
# 1.
# Fit a GLMM with uncorrelated random intercepts for Trusts and profession. Use
# this to acquire coefficients for the covariates to convert to odds ratios.
# This model specification gives coefficients that represent the average
# difference in the variate for a 1-unit difference in the covariate, conditional
# on the reference value for all other covaraites, and on the average Trust value
# and average profession value.
# Unfortunately, this model crashes Rstudio. I think it is a RAM issue.
# 
# 2.
# Calculate a GEE on profession-specific subsets of the data. Specify blocks by 
# Trust. The exponentatied coefficients for the covariates represents the 
# population-averaged odds ratio, where the population is the population of 
# professions.
# 
# ----

# Prepare the data.
# ----


GEE_df <-
  df_staff_survey_main %>%
  dplyr::filter( ss_year == 2024 ) %>%
  dplyr::mutate(
    ss_q26a_LikertScore_binary =
      factor(
        ss_q26a_LikertScore_binary
        ,levels = c( "Disagree", "Agree" )
      ) %>%
      as.numeric() %>%
      `-`( 1 )
    ,ss_q26c_LikertScore_binary =
      factor(
        ss_q26c_LikertScore_binary
        ,levels = c( "Disagree", "Agree" )
      ) %>%
      as.numeric() %>%
      `-`( 1 )
    ,`Org code` = as.factor( `Org code` )
  )
GEE_df_Likert <-
  GEE_df %>%
  dplyr::select(
    ss_year
    ,`Org code`
    ,`Care setting`
    ,ss_q26a_LikertScore_binary
    ,ss_q26c_LikertScore_binary
    ,ss_q2a_LikertScore
    ,ss_q3i_LikertScore
    ,ss_q4c_LikertScore
    ,ss_q4d_LikertScore
    ,ss_q5a_LikertScore
    ,ss_q9a_LikertScore
    ,ss_q9i_LikertScore
    ,ss_q11c_LikertScore
    ,ss_q21_LikertScore
    ,ss_q24d_LikertScore
    ,ss_q25d_LikertScore
    ,ss_q25f_LikertScore
  ) %>%
  na.omit() %>%
  dplyr::mutate(
    across(
      where( is.factor )
      ,droplevels
    )
  )
GEE_df_BinaryLikert <-
  GEE_df %>%
  dplyr::select(
    ss_year
    ,`Org code`
    ,`Care setting`
    ,ss_q26a_LikertScore_binary
    ,ss_q26c_LikertScore_binary
    ,ss_q2a_LikertScore
    ,ss_q3i_LikertScore_binary
    ,ss_q4c_LikertScore_binary
    ,ss_q4d_LikertScore_binary
    ,ss_q5a_LikertScore
    ,ss_q9a_LikertScore_binary
    ,ss_q9i_LikertScore_binary
    ,ss_q11c_LikertScore_binary
    ,ss_q21_LikertScore_binary
    ,ss_q24d_LikertScore_binary
    ,ss_q25d_LikertScore_binary
    ,ss_q25f_LikertScore_binary
  ) %>%
  na.omit() %>%
  dplyr::mutate(
    across(
      where( is.factor )
      ,droplevels
    )
  )
# ----

# Fit the GLMM.
# ----
#glmm <-
  # lme4::glmer(
  #   formula =
  #     as.factor( ss_q26a_LikertScore_binary )~ 
  #     ss_q2a_LikertScore +
  #      # ss_q3i_LikertScore_binary +
  #      # ss_q4c_LikertScore_binary +
  #      # ss_q4d_LikertScore_binary +
  #      # ss_q5a_LikertScore +
  #      #ss_q9a_LikertScore_binary +
  #      # ss_q9i_LikertScore_binary +
  #      # ss_q11c_LikertScore_binary +
  #      #ss_q21_LikertScore_binary +
  #      # ss_q24d_LikertScore_binary +
  #      # ss_q25d_LikertScore_binary +
  #     # ss_q25f_LikertScore_binary +
  #      ( 1 | `Org code` ) +
  #      ( 1 | `Care setting` )
  #   ,data = glmdata#[1:200,]
  #     
  #   ,family = "binomial"
  # )
# ----



#########################
## Calculate the GEEs. ##
#########################
# The "1block" and "2block" refer to how the correlation structure is blocked
# to represent dependence. "1block" indicates that observations are grouped by
# Trust but there is no distinction between
# ----
GEE_1block_allAHP_BinaryLikert_q26a <-
  geepack::geeglm(
    formula =
      ss_q26a_LikertScore_binary ~ 
      ss_q2a_LikertScore + 
      ss_q3i_LikertScore_binary +
      ss_q4c_LikertScore_binary +
      ss_q4d_LikertScore_binary +
      ss_q5a_LikertScore +
      ss_q9a_LikertScore_binary +
      ss_q9i_LikertScore_binary +
      ss_q11c_LikertScore_binary +
      ss_q21_LikertScore_binary +
      ss_q24d_LikertScore_binary +
      ss_q25d_LikertScore_binary +
      ss_q25f_LikertScore_binary
    ,family = binomial( link = "logit" )
    ,data = GEE_df_BinaryLikert
    ,id =`Org code`
    ,corstr = "exchangeable"
  )
GEE_2block_allAHP_BinaryLikert_q26a <-
  geepack::geeglm(
    formula =
      ss_q26a_LikertScore_binary ~ 
      ss_q2a_LikertScore + 
      ss_q3i_LikertScore_binary +
      ss_q4c_LikertScore_binary +
      ss_q4d_LikertScore_binary +
      ss_q5a_LikertScore +
      ss_q9a_LikertScore_binary +
      ss_q9i_LikertScore_binary +
      ss_q11c_LikertScore_binary +
      ss_q21_LikertScore_binary +
      ss_q24d_LikertScore_binary +
      ss_q25d_LikertScore_binary +
      ss_q25f_LikertScore_binary
    ,family = binomial( link = "logit" )
    ,data = GEE_df_BinaryLikert
    ,id = interaction( `Org code`, `Care setting` )
    ,corstr = "exchangeable"
  )
GEE_1block_allAHP_BinaryLikert_q26c <-
  geepack::geeglm(
    formula =
      ss_q26c_LikertScore_binary ~ 
      ss_q2a_LikertScore + 
      ss_q3i_LikertScore_binary +
      ss_q4c_LikertScore_binary +
      ss_q4d_LikertScore_binary +
      ss_q5a_LikertScore +
      ss_q9a_LikertScore_binary +
      ss_q9i_LikertScore_binary +
      ss_q11c_LikertScore_binary +
      ss_q21_LikertScore_binary +
      ss_q24d_LikertScore_binary +
      ss_q25d_LikertScore_binary +
      ss_q25f_LikertScore_binary
    ,family = binomial( link = "logit" )
    ,data = GEE_df_BinaryLikert
    ,id =`Org code`
    ,corstr = "exchangeable"
  )
GEE_2block_allAHP_BinaryLikert_q26c <-
  geepack::geeglm(
    formula =
      ss_q26c_LikertScore_binary ~ 
      ss_q2a_LikertScore + 
      ss_q3i_LikertScore_binary +
      ss_q4c_LikertScore_binary +
      ss_q4d_LikertScore_binary +
      ss_q5a_LikertScore +
      ss_q9a_LikertScore_binary +
      ss_q9i_LikertScore_binary +
      ss_q11c_LikertScore_binary +
      ss_q21_LikertScore_binary +
      ss_q24d_LikertScore_binary +
      ss_q25d_LikertScore_binary +
      ss_q25f_LikertScore_binary
    ,family = binomial( link = "logit" )
    ,data = GEE_df_BinaryLikert
    ,id = interaction( `Org code`, `Care setting` )
    ,corstr = "exchangeable"
  )

# GEE AHP-specific Likert 
GEE_AHPspecific_Likert <-
  GEE_df_Likert %>%
  dplyr::nest_by( `Care setting`, .key = "nested_data" ) %>%
  # The models will not fit for the follow professions because there are
  # too few observations:
  # - Osteopathy (n = 12)
  # - Prosthetics and Orthotics (n = 83)
  dplyr::filter(
    !`Care setting` %in% c( "Osteopathy", "Prosthetics and Orthotics" )
  ) %>%
  # Calculate the GEEs. 
  dplyr::mutate(
    gee_Likert_q26a =
      list(
        geepack::geeglm(
          formula =
            ss_q26a_LikertScore_binary ~ 
            ss_q2a_LikertScore + 
            ss_q3i_LikertScore +
            ss_q4c_LikertScore +
            ss_q4d_LikertScore +
            ss_q5a_LikertScore +
            ss_q9a_LikertScore +
            ss_q9i_LikertScore +
            ss_q11c_LikertScore +
            ss_q21_LikertScore +
            ss_q24d_LikertScore +
            ss_q25d_LikertScore +
            ss_q25f_LikertScore
          ,family = binomial( link = "logit" )
          ,data = nested_data
          ,id = `Org code`
          ,corstr = "exchangeable"
        )
      )
    ,gee_Likert_q26c =
      list(
        geepack::geeglm(
          formula =
            ss_q26c_LikertScore_binary ~ 
            ss_q2a_LikertScore + 
            ss_q3i_LikertScore +
            ss_q4c_LikertScore +
            ss_q4d_LikertScore +
            ss_q5a_LikertScore +
            ss_q9a_LikertScore +
            ss_q9i_LikertScore +
            ss_q11c_LikertScore +
            ss_q21_LikertScore +
            ss_q24d_LikertScore +
            ss_q25d_LikertScore +
            ss_q25f_LikertScore
          ,family = binomial( link = "logit" )
          ,data = nested_data
          ,id = `Org code`
          ,corstr = "exchangeable"
        )
      )
    )



# GEE AHP-specific binary Likert

GEE_AHPspecific_BinaryLikert <-
  GEE_df_BinaryLikert %>%
  dplyr::nest_by( `Care setting`, .key = "nested_data" ) %>%
  # The models will not fit for the follow professions because there are
  # too few observations:
  # - Osteopathy (n = 4)
  # - Prosthetics and Orthotics (n = 22)
  dplyr::filter(
    !`Care setting` %in% c( "Osteopathy", "Prosthetics and Orthotics" )
  ) %>%
  # Calculate the GEEs. 
  dplyr::mutate(
    gee_BinaryLikert_q26a =
      list(
        geepack::geeglm(
          formula =
            ss_q26a_LikertScore_binary ~ 
            ss_q2a_LikertScore + 
            ss_q3i_LikertScore_binary +
            ss_q4c_LikertScore_binary +
            ss_q4d_LikertScore_binary +
            ss_q5a_LikertScore +
            ss_q9a_LikertScore_binary +
            ss_q9i_LikertScore_binary +
            ss_q11c_LikertScore_binary +
            ss_q21_LikertScore_binary +
            ss_q24d_LikertScore_binary +
            ss_q25d_LikertScore_binary +
            ss_q25f_LikertScore_binary
          ,family = binomial( link = "logit" )
          ,data = nested_data
          ,id = `Org code`
          ,corstr = "exchangeable"
        )
      )
    
    ,gee_BinaryLikert_q26c =
      list(
        geepack::geeglm(
          formula =
            ss_q26c_LikertScore_binary ~
            ss_q2a_LikertScore +
            ss_q3i_LikertScore_binary +
            ss_q4c_LikertScore_binary +
            ss_q4d_LikertScore_binary +
            ss_q5a_LikertScore +
            ss_q9a_LikertScore_binary +
            ss_q9i_LikertScore_binary +
            ss_q11c_LikertScore_binary +
            ss_q21_LikertScore_binary +
            ss_q24d_LikertScore_binary +
            ss_q25d_LikertScore_binary +
            ss_q25f_LikertScore_binary
          ,family = binomial( link = "logit" )
          ,data = nested_data
          ,id = `Org code`
          ,corstr = "exchangeable"
        )
      )
  )

# ----

##################
## Diagnostics. ##
##################
# Assess multi-co-linearity using `car::vif()`
# 
# ----

# ----

########################
## Extract estimates. ##
########################
# ----
# Make function to do the work.
fnc__geeExtractEstimates <-
  function(
    # Function returns the odds ratios and 95% confidence intervals for covariates
    # whose estimates were unequivocal, i.e. != 0.
    #
    gee = NULL # A GEE from which to extract the estimates.
    ,qlabel = NULL # A character string to indicate the survey question of interest.
    ,plabel = NULL # A character string to indicate the population under study.
    ,dlabel = NULL # A character string to indicate what blocking was used.
    )
  {
    # Check arguments
    if( is.null( gee ) ) { stop("GEE not supplied.") }
    if( is.null( qlabel ) ) { stop("Argument `qlabel` not supplied.") }
    if( is.null( plabel ) ) { stop("Argument `plabel` not supplied.") }
    if( is.null( dlabel ) ) { stop("Argument `dlabel` not supplied.") }
    
    
    # Extract.
    summary( gee )$coefficients %>%
      as.data.frame() %>%
      dplyr::select( Estimate, contains("std."), contains("Pr") ) %>%
      `colnames<-`( c( "O.R.", "S.E.", "Pr") ) %>%
      dplyr::mutate(
        O.R._lb = O.R. - ( S.E. * 1.945 )
        ,O.R._ub = O.R. + ( S.E. * 1.945 )
        # Transform to odds ratio scale.
        ,across(
          contains( "O.R.")
          ,exp
        )
        ,ignore_est = dplyr::if_else( Pr > 0.05, T, F)
      ) %>%
      # Filter for the estimates whose confidence intervals are unequivocal.
      dplyr::filter( ignore_est == FALSE ) %>%
      dplyr::select( contains("O.R.") ) %>%
      # Append explanatory columns.
      tibble::rownames_to_column() %>%
      dplyr::mutate( rowname, Question = qlabel, Pop. = plabel, Dependence = dlabel )
      
  }

# Extract estimates from the all-AHP data frame.
output__GEE_allAHPs <-
  dplyr::bind_rows(
    fnc__geeExtractEstimates(
      gee = GEE_1block_allAHP_BinaryLikert_q26a
      ,qlabel = "q26a"
      ,plabel = "All professions"
      ,dlabel = "Trust"
    )
    ,fnc__geeExtractEstimates(
      gee = GEE_2block_allAHP_BinaryLikert_q26a
      ,qlabel = "q26a"
      ,plabel = "All professions"
      ,dlabel = "Trust and profession"
    )
    ,fnc__geeExtractEstimates(
      gee = GEE_1block_allAHP_BinaryLikert_q26c
      ,qlabel = "q26c"
      ,plabel = "All professions"
      ,dlabel = "Trust"
    )
    ,fnc__geeExtractEstimates(
      gee = GEE_2block_allAHP_BinaryLikert_q26c
      ,qlabel = "q26c"
      ,plabel = "All professions"
      ,dlabel = "Trust and profession"
    )
  )

# Extract estimates from the AHP-specific data frame.
for( i_gee in 1:nrow( GEE_AHPspecific_BinaryLikert ) )
{
  ## For question q26a.
  gee <- GEE_AHPspecific_BinaryLikert$gee_BinaryLikert_q26a[[ i_gee ]]
  pop <- GEE_AHPspecific_BinaryLikert$`Care setting`[ i_gee ]
  # Extract.
  extract <-
    fnc__geeExtractEstimates(
      gee = gee
      ,qlabel = "q26a"
      ,plabel = pop
      ,dlabel = "Trust"
    )
  # Merge.
  if( i_gee == 1)
  { output__GEE_eachAHP_q26a <- extract } else  {
    output__GEE_eachAHP_q26a <-
      dplyr::bind_rows( output__GEE_eachAHP_q26a, extract )
  }
  
  ## For question q26c.
  gee <- GEE_AHPspecific_BinaryLikert$gee_BinaryLikert_q26c[[ i_gee ]]
  pop <- GEE_AHPspecific_BinaryLikert$`Care setting`[ i_gee ]
  # Extract.
  extract <-
    fnc__geeExtractEstimates(
      gee = gee
      ,qlabel = "q26c"
      ,plabel = pop
      ,dlabel = "Trust"
    )
  # Merge.
  if( i_gee == 1)
  { output__GEE_eachAHP_q26c <- extract }   else   {
    output__GEE_eachAHP_q26c <-
      dplyr::bind_rows( output__GEE_eachAHP_q26c, extract )
  }
  
  # Combine.
  output__GEE_eachAHP <-
    dplyr::bind_rows( output__GEE_eachAHP_q26a, output__GEE_eachAHP_q26c )
}

# Combine extracts.
output <-
  dplyr::bind_rows( output__GEE_allAHPs, output__GEE_eachAHP )
# ----


#####################
## Plot estimates. ##
#####################
# ----

# Amend the data for plotting.
output <-
  output %>% 
  dplyr::filter(
    # The two dependence structures are equivalent.
    Dependence == "Trust"
    # Remove the intercept because interpretation is fraught.
    ,rowname != "(Intercept)"
    # Exclude any 0 or infinite estimates, which are obviously nonsense.
    ,O.R. !=  0, !is.infinite( O.R. )
    ) %>%
  # Change the labels for the questions.
  dplyr::rowwise() %>%
  dplyr::mutate(
    q =
      stringr::str_match( rowname, "_(.*?)_" )[2] %>%
      stringr::str_remove_all( "_")
    ,q_level =
      unlist( stringr::str_split( rowname, "_LikertScore*" ) )[2] %>%
      stringr::str_remove_all( "_binary")
  ) %>%  
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q == q_num )
  ) %>%
  dplyr::mutate(
    q_label = paste( q_level, q_word, sep = " - ")
  ) %>%
  # Change the labels for the outcome questions.
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( Question == q_num )
  ) %>%
  dplyr::rename( Q_label = q_word.y, q_word = q_word.x )

# Create list of professions whose data will be plotted separately.
professions <- unique( output$Pop. )

# Set limit of x-axis in O.R. units.
x_axis_lim <- 5

# Make and save the plots.
for( i_profession in 1:length( professions ) )
{
  
  # Plot each `profession `Pop.` group.
  p <-
    output %>%
    dplyr::filter(
    Pop. == professions[ i_profession ]
    ) %>%
  ggplot(
    aes(
      x = q_label, y = O.R., ymin = O.R._lb, ymax = O.R._ub
      )
    ) +
    geom_pointrange() +
    geom_hline( yintercept = 1, lty = 5 ) +
    geom_text(
      aes(
        y = x_axis_lim 
        ,label = format( O.R., digits = 2 )
        ,hjust = 1
        )
      ,size = 3
      ,vjust = -1
      ) +
    facet_grid(
      cols = vars( Q_label )
      ,labeller = labeller( Q_label = label_wrap_gen( 35 ) )
      ) +
    coord_flip() +
    labs(
      title =
        paste0(
          "Odds ratios with 95% confidence intervals,\n"
          ,professions[ i_profession ]
          ," ( n = "
          ,ifelse(
            i_profession == 1
            ,format(
              GEE_df_Likert %>%
              dplyr::filter(
                !`Care setting` %in% c( "Osteopathy", "Prosthetics and Orthotics" )
                ) %>%
                nrow()
              ,big.mark = ","
              )
            ,format(
              nrow( tidyr::unnest( GEE_AHPspecific_BinaryLikert[ i_profession-1, ] , cols = nested_data ) )
              ,big.mark = ","
            )
          )
          ," )."
        )
      ,subtitle =
        paste0(
        "\u2022 Using individuals' responses rather than Trust-level\n"
        ,"\ \ summary with dependecies accounted for within\n\ \ the correlation matrix."
        )
      ,x = "Survey question"
      ,y = "Odd ratio with 95% confidence interval"
      ,caption =
        paste(
        "\u2022 \"Rarely\" is relative to \"Never\"; \"Sometimes\" is relative to "
        ,"\"Rarely\";\n\ \ \"Often\" is relative to \"Sometimes\"; \"Always\" is "
        ,"relative to \"Often\"."
        ,"\n\u2022 \"Yes\" is relative to \"No\"."
        ,"\n\u2022 \"Satisfied\" is relative to \"Unsatisfied\"."
        ,"\n\u2022 \"Disagree\" is relative to \"Agree\"."
        )
      ) +
    scale_x_discrete( labels = function(x) str_wrap( x, width = 50 ) ) +
    ylim( 0, x_axis_lim ) +
    theme_bw() +
    theme(
      plot.caption = element_text( hjust = 0 )
      ,axis.text.y = element_text( size = 6 )
      )
  # Save the plot.
  ggsave(
    plot = p
    ,filename =
      paste0(
        "Plots/Staff survey/"
        ,"Point-range charts/"
        ,"plot__ss_ptrng_"
        ,gsub( professions[ i_profession ], pattern = "/", replacement = "&" )
        ,".png"
      )
    ,dpi = 300
    ,width = 17
    ,height = 20
    ,units = "cm"
  )
}
# ----



# ----
#### Later, I can make a df that contains explanations of the estimate, and join
#### this df to the unioned df of all output tables. That daves me having to 
#### type the explanations for each output table.

# Prepare output.
Method = c( GEE, GLM )
Profession =
  c(
    "all professions"
    ,"Chiropody / Podiatry"
    ,"Occupational Therapy"      
    ,"Physiotherapy"                         
    ,"Dietetics"
    ,"Speech & Language Therapy" 
    ,"Art / Music / Dramatherapy"
    ,"Prosthetics and Orthotics" 
    ,"Radiography (therapeutic)"
    ,"Operating Theatres"        
    ,"Radiography (diagnostic)"
    ,"Orthoptics"                
    ,"Osteopathy"
    )
Interpretation =
  c(
    # GLMM
    ,"Difference in how likely a responder is to agree to the question of \\
    interest, given that they agreed to the other question, \\
    specifically based on an average model of Trusts and an independent \\
    average model of professions."
    # GEE
    ,"Population-averaged difference in how likely a responder is to agree to \\
    the question of interest, given that they agreed to the other question, \\
    accouting for dependencies between responses."
  )
Dependence =
  c(
    # GEE blocking for `Org code`.
    "Trust specific; Profession generic - Assumes no correlation between responses from different Trusts but does \\
    assume correlation between responses within a Trust. The within-Trust \\
    correlation is the same for all Trusts. The correlation between responses \\
    from different professions is the same as the correlation between responses\\
     from the same profession."
    # GEE blocking for `Org code` and `Care setting.
    ,"Trust specific; Profession specific - Assumes no correlation between responses from different Trusts nor between\\
     responses from different professions within a Trust. But, responses from a \\
    given profession wthin a Trust are correlated. This within-Trust-within-profession \\
    correlation is the same for all Trust-profession pairs."
  )

# ----