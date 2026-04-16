# WrAP_Explore__staff_suvery_study.R
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
  infotheo
  ,tidyverse
)
# ----

#################
## Requisites. ##
#################
# ----
# Set the list of questions of interets.
q_lookup <-
  data.frame(
    q_num = 
      c( "q2a", "q3i", "q4c", "q4d", "q5a", "q9a", "q9i", "q11c", "q21"
         ,"q24d", "q25d", "q25f", "q26a", "q26c" )
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
        ,"I think that my organisation respects individual differences (e.g. cultures, working styles, backgrounds, ideas, etc)."
        ,"I feel supported to develop my potential."
        ,"If a friend or relative needed treatment I would be happy with the standard of care provided by this organisation."
        ,"If I spoke up about something that concerned me I am confident my organisation would address my concern."
        ,"I often think about leaving this organisation."
        ,"As soon as I can find another job, I will leave this organisation."
      )
  ) %>%
  dplyr::mutate(
    combined = paste0( q_num, " - \"", q_word, "\"")
  )

# Remove rows from staff that we are not interetsed in.
df_staff_survey_main<-
  df_staff_survey_main %>%
  tidyr::drop_na( `Care setting`)

# Extract the set of professions in the data.
roles <- unique( df_staff_survey_main$`Care setting` )

# Set the dataset of interest.
dataset_id <- "WITHIN_NHS"
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
      dplyr::filter( `Care setting` == roles[ i_role ] )
  } else {
    i_plot_data <- df_staff_survey_main
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
                ,"\n\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Using individuals' responses rather than Trust-level summary."
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
            ,dataset_id
            ,"/Column charts/"
            ,"/plot__ss_columns_"
            ,ifelse(
              i_role < 13
              ,gsub(roles[ i_role ], pattern = "/", replacement = "&")
              ,"all professions"
              )
            ,"_"
            ,q_lookup[ i_q_name, "q_num" ]
            ,"_"
            ,dataset_id
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
    } # End of IF
  } # End of questions FOR
} # End of roles FOR
  
# ----

#######################################################
## Cross-plot the outcome questions with the others. ##
#######################################################
# ----
# Separate outcome questions from the others.
q_lookup_outcomes <-
  q_lookup %>%
  dplyr::filter( q_num %in% c( "q2a", "q26a", "q26c" ) ) %>%
  dplyr::pull( q_num )
q_lookup_other <-
  q_lookup %>%
  dplyr::filter( !q_num %in% c( "q2a", "q26a", "q26c" ) ) %>%
  dplyr::pull( q_num )

# Make and save the plots.
for( i_role in 1:length( roles )+1 )
{
  if( i_role < 13 )
  {
    i_plot_data <-
      df_staff_survey_main %>%
      dplyr::filter( `Care setting` == roles[ i_role ] )
  } else {
    i_plot_data <- df_staff_survey_main
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
          contains( outcome_var_name ) & contains( "Likert")
          ,contains( other_var_name ) & contains( "Likert")
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
              paste0(
                "\u2022 Using ", dataset_id," dataset.\n"
                ,"\u2022 Using individuals' responses rather than Trust-level summary."
              )
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
            ,dataset_id
            ,"/Bubble charts/"
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
            ,"_"
            ,dataset_id
            ,"_dataset.png"
          )
        ,dpi = 300
        ,width = 20
        ,height = 20
        ,units = "cm"
      )
      
    }
  }
}
# ----

#########################
## Mutual information. ##
#########################
# ----
mi_scores <-
  df_staff_survey_main %>%
  dplyr::select( contains( "ss_q" ) & contains( "Likert" ) ) %>%
  infotheo::mutinformation()
mi_scores <-
  cbind(
    t( combn( colnames( mi_scores ), m = 2 ) )
    ,( mi_scores / diag( mi_scores ) )[ upper.tri( mi_scores ) ]
  ) %>%
  as.data.frame() %>%
  `colnames<-`( c( "q1", "q2", "MI" ) ) %>%
  dplyr::mutate( MI = as.double( MI ) )
mi_scores <-
  dplyr::filter(
    mi_scores
    ,stringr::str_detect( q2, stringr::str_c( q_lookup_outcomes, collapse = "|") ) 
  ) %>%
  dplyr::rename( q2 = q1, q1 = q2 ) %>%
  dplyr::select( q1, q2, MI ) %>%
  # Filter for the questions of interest.
  dplyr::filter(
    !stringr::str_detect( q2, stringr::str_c( q_lookup_outcomes, collapse = "|") )
  ) %>%
  dplyr::bind_rows(
    dplyr::filter(
      mi_scores
      ,stringr::str_detect( q1, stringr::str_c( q_lookup_outcomes, collapse = "|") ) 
    ) %>%
      dplyr::filter(
        !stringr::str_detect( q2, stringr::str_c( q_lookup_outcomes, collapse = "|") )
      )
  ) %>%
  dplyr::rowwise() %>%
  # Add the wording of the questions.
  dplyr::mutate(
    q1 = stringr::str_match( q1, "_(.*?)_" )[2]
    ,q2 = stringr::str_match( q2, "_(.*?)_" )[2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q1 == q_num )
  ) %>%
  dplyr::left_join(
    q_lookup %>% dplyr::select( -combined )
    ,by = join_by( q2 == q_num )
  ) %>%
  # Tidy up.
  dplyr::rename( q1_word = q_word.x, q2_word = q_word.y ) %>%
  dplyr::arrange( -MI ) %>%
  # Save.
  write.csv( "Questions ranked by mutual information score.csv" )
# ----
