rm(list=ls())

find_replication_root <- function() {
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:12) {
    if (file.exists(file.path(d, "run_all.R")) && dir.exists(file.path(d, "code")) && dir.exists(file.path(d, "data"))) {
      return(d)
    }
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("Could not locate the replication package root (folder containing run_all.R, code/, and data/). ",
       "Please set your R working directory to the 'replication' folder before running this script.")
}
root <- find_replication_root()
setwd(root)
source(file.path(root, "code", "header.R"))

full_data_w <- read_rds('data/derived/regression_build_w.rds') %>%
  filter(age==1,
         !is.na(e_ch_health),
         !is.na(e_private_school),
         !is.na(e_cg_edu_engagement),
         !is.na(e_ch_motiv),
         !is.na(e_ch_edu_asp),
         !is.na(e_cg_emotional_engagement),
         !is.na(e_attend),
         !is.na(cg_mh_scale)) %>%
  mutate(e_attend=case_when(as.double(e_attend)>3~1,
                            T~0))

mech_reg_func<-function(category){

  fi<-'e_cfies_indicator+e_fies_indicator'

  health_input='e_ch_health'
  edu_input='e_attend+e_private_school+e_cg_edu_engagement'
  child_psyc_input='e_ch_motiv+e_ch_edu_asp'
  cg_psyc_input='e_cg_emotional_engagement+cg_mh_scale'

  reg_func <- function(category, model){
    e_category_str<-paste0("e_",category,"_per")
    m_category_str<-paste0("m_",category,"_per")

    for_reg<-full_data_w %>%
      rename(lagged_outcome=m_category_str)

    fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))
    reg <- feols(fm,
                 data=for_reg,
                 cluster=~careid)
    return(reg)
  }

  input<- expand.grid(category=c(category),
                               model=c(
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{health_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{edu_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{child_psyc_input}+'),
                                       glue('~ {fi}+female+region_north_east+region_northern+region_upper_east+region_upper_west+treatment+age_pct_rank+factor(month)+{cg_psyc_input}+')))

  ols_results<- pmap(input,
                          reg_func) %>%
    set_names(
              'Health Input',
              'Edu. Input',
              'Child Pysc. Input',
              'Caregiver Psyc. Input')

}

the_models<-map(c('num'),
         mech_reg_func) %>%
  set_names(c('Numeracy'))

modelsummary(the_models[['Numeracy']],
             fmt=f,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*health|.*attend|.*school|.*engagement|.*motiv|.*asp|.*scale|.*treatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported FI",
                           'e_fies_indicator'="Caregiver-Reported FI",
                           'female'='Child is Female',
                           'e_ch_health2'="Poor Health",
                           'e_ch_health3'="Average Health",
                           'e_ch_health4'="Good Health",
                           'e_ch_health5'="Very Good Health",
                           'e_attend'="Attended School",
                           'e_private_school'="Private School",
                           'e_cg_edu_engagement'="Caregiver Edu. Engagement",
                           'e_ch_motiv'='Child Motivation',
                           'e_ch_edu_asp'='Child Edu. Aspiration',
                           'e_cg_emotional_engagement'='Caregiver Emo. Engagement',
                           'cg_mh_scale'='Caregiver Mental Health',
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'='Treatment',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, region, and household randomized treatment. The child-reported health covariates are measured relative to children who reported very poor health. Attended School is a binary variable indicating whether the child attended school most of the time. Private school is a binary variable indicating whether the child attended private school. Caregiver Edu. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's education. Child Motivation is the sum of a child's degree of agreement with statements related to whether they are motivated. Child Edu. Aspiration is a binary variable of whether a child aspires to complete high school. Caregiver Emo. Engagement is the sum of caregivers' degree of agreement with statements related to whether they engage with their children's emotional well-being. Caregiver Mental Health is the sum of caregivers' degree of experience with poor mental health.",
             out="tables/appendix_m.tex",
             escape = FALSE)
