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

fi <- read_rds("data/derived/fi.rds")%>%
  mutate(across(contains('id'),~as.double(.)))
controls <- read_rds("data/derived/controls.rds") %>%
  clean_names()
e_child <- read_dta("data/raw/03_PNP_Endline_ChildSurvey.dta") %>%
  dplyr::select(-contains("gb")) %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  mutate(across(contains('fs'),~as.double(.))) %>%
  rename(careid=caseid) %>%
  filter(io2==1)
e_cg <- read_dta("data/raw/02_PNP_Endline_CaregiverSurvey.dta") %>%
  mutate(careid=as.double(careid),
         childid=as.double(childid)) %>%
  mutate(across(contains('fs'),~as.double(.)))
child_order <-read_dta("data/derived/poverty.dta") %>%
  mutate(poverty=ppi_poverty_prob/100,
         childid=as.double(childid)) %>%
  dplyr::select(childid,poverty)
m_child <- read_dta("data/raw/03_PNP_Midline_ChildSurvey.dta") %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  filter(io2==1) %>%
  select(careid,
         childid,
         m_ch_health=cw1,
         m_school_type=ed2,
         m_ch_edu_asp=ja3,
         )

m_cg <- read_dta("data/raw/02_PNP_Midline_CaregiverSurvey.dta") %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  mutate(cg_female=case_when(cb2==2~1,
                             T~0),
         edu_rowmax=pmax(cb3,cb4,na.rm=T),
         cg_primary=case_when(edu_rowmax>=2 & edu_rowmax<=5~1,
                              T~0))
outcome_checker<- read_rds("data/derived/outcome_zscore_checker.rds") %>%
  mutate(across(contains('id'),~as.double(.)))
outcome_raw<- read_rds("data/derived/outcome_raw.rds") %>%
  mutate(across(contains('id'),~as.double(.)))
e_household <- read_dta("data/raw/01_PNP_Endline_HouseholdSurvey.dta") %>%
  mutate(
    across(starts_with("cr0_"), as.double),
    across(starts_with("cr6_"), as.double),
    careid = as.double(careid)
  ) %>%
  select(careid, starts_with("cr0_"), starts_with("cr6_")) %>%
  pivot_longer(
    cols = c(starts_with("cr0_"), starts_with("cr6_")),
    names_to = c(".value", "child_num"),
    names_pattern = "cr(0|6)_(.*)"
  ) %>%
  rename(
    childid   = `0`,
    child_age = `6`
  ) %>%
  select(careid, childid, child_age) %>%
  arrange(careid,child_age) %>%
  group_by(careid) %>%
  mutate(
    age_pct_rank = percent_rank(child_age)
  ) %>%
  ungroup()

full_data_w <- e_child %>%
  dplyr::select(careid,
                childid,
                age=childage,
                e_school_type=ed2,
                region,
                e_ch_health=cw1,
                e_ch_health_rel=cw2,
                e_ch_edu_asp=ja3,
                contains('fs'),
                treatment_raw=treatment,
                startdate
  ) %>%
  left_join(m_child,by=c('childid','careid')) %>%
  rename_with(~ paste0(., "_child"), .cols = matches("^fs\\d+$")) %>%
  mutate(across(contains('fs'),~case_when(. == 1 ~ 2,
                                          . == 2 ~ 1,
                                          . == 3 ~ 0,
                                          TRUE ~ NA_real_)),
         year=year(startdate),
         month=month(startdate)) %>%
  left_join(outcome_checker,
            by=c('childid')) %>%
  left_join(outcome_raw %>%
              dplyr::select(childid,careid,contains('raw')),
            by=c('childid',
                 'careid'),
            suffix = c("",
                       '_checker')) %>%
  dplyr::left_join(e_cg %>% dplyr::select(careid,
                                          childid,
                                          e_enroll_cg=cr7,
                                          e_attend=cr8,
                                          female=cr3,
                                          contains('fs')
  )%>%
    rename_with(~ paste0(., "_cg"), .cols = matches("^fs\\d+$")),
  by=c("childid",'careid')) %>%
  dplyr::select(-matches("^fs\\d+$")) %>%
  dplyr::left_join(controls,
                   by=c("childid")) %>%
  dplyr::left_join(fi,
                   by=c("childid")
  ) %>%
  dplyr::left_join(m_cg %>%
                     select(cg_age=cb1,
                            cg_female,
                            cg_primary,
                            childid,
                            m_attend=cr8,),
                   by=c("childid")
  ) %>%
  dplyr::left_join(child_order,
                   by=c("childid")

  ) %>%
  dplyr::left_join(e_household %>% select(childid,age_pct_rank),
                   by=c("childid")) %>%
  mutate(across(contains('enroll_ch'),~case_when(.!=1~0,
                                                 T~1)),
         female=if_else(female==1,0,1),
         e_private_school=case_when(e_school_type==2~1,
                                    is.na(e_school_type)~0,
                                    T~0),
         m_private_school=case_when(m_school_type==2~1,
                                    is.na(m_school_type)~0,
                                    T~0),
         across(contains('school_type'),~if_else(.==1,0,1)),
         age_num=as.double(age),
         age=if_else((age>=5 & age <=9),0,1),
         treatment=case_when(treatment_raw>0 ~ 1,
                             T~0),
         region=case_when(region==""~"Northern",
                          T~region),
         across(contains('health'),~as.factor(case_when(as.double(.)<0~NA_real_,
                                                        T~as.double(.)))),
         across(contains('attend'),~as.factor(.)),
         e_ch_edu_asp=case_when(e_ch_edu_asp>=5~1,
                                is.na(e_ch_edu_asp) ~ NA_real_,
                                T~0),
         m_ch_edu_asp=case_when(m_ch_edu_asp>=5~1,
                                is.na(m_ch_edu_asp) ~ NA_real_,
                                T~0)
  ) %>%
  mutate(across(contains('health'),
                ~as.double(case_when(.==4~1,
                                     .==5~1,
                                     .==3~0,
                                     .==2~0,
                                     .==1~0,
                                     T~NA_real_)),
                .names = "{col}_dummy"
  )) %>%
  mutate(across(c(
    e_ch_motiv,
    m_ch_motiv,
    e_cg_emotional_engagement,
    m_cg_emotional_engagement),
    ~scale(.)[,1])) %>%
  fastDummies::dummy_cols(select_columns='region') %>%
  clean_names() %>%
  mutate(missing=case_when(
    !is.na(female) &
      !is.na(age) &
      !is.na(e_cfies_sum) &
      !is.na(e_fies_sum) &
      !is.na(treatment) &
      !is.na(m_lit_per) &
      !is.na(m_num_per) &
      !is.na(m_sel_per) &
      !is.na(m_ef_per) &
      !is.na(e_lit_per) &
      !is.na(e_num_per) &
      !is.na(e_sel_per) &
      !is.na(e_ef_per) &
      !is.na(age_pct_rank) ~ 0,
    T~1
  ))

summary_stat<-full_data_w %>%
  dplyr::select(
    e_cfies_indicator,
    e_fies_indicator,
    female,
    age,
    age_num,
    contains('region_'),
    contains('per')
  ) %>%
  select(-contains('raw'))

t_test_func<-function(var,
                      group){

  exp <- expr(!!ensym(var) ~ !!ensym(group))

  test <- t.test(formula = eval(exp),
                 data=full_data_w)

    out<-tribble(~var,~not_excluded,~excluded,~pval_excluded,
                 var,test$estimate[1],test$estimate[2],test$p.value)

  return(out)

}

input<-names(summary_stat) %>%
  as_tibble() %>%
  rename(var=value) %>%
  mutate(group='missing'
  )

missing_summary_stat<-pmap_dfr(input,
                              t_test_func)

out<-missing_summary_stat %>%
  mutate(across(c('excluded','not_excluded'),~case_when(.==0~NA,
                                                T~.))) %>%
  filter(var!='age',
         !str_detect(var,'^m_.*.per$')) %>%
  rename('Excluded'=excluded,
    'Not Excluded'=not_excluded,
    'P-Value: Exclusion'=pval_excluded) %>%
  mutate(var=case_when(var=='e_cfies_indicator'~'Child-Reported FI (%)',
                       var=='e_fies_indicator'~'Caregiver-Reported FI (%)',
                       var=='female'~'Child is Female (%)',
                       var=='age_num'~'Child Age (Years)',
                       var=='region_north_east'~"Region: North East",
                       var=='region_northern'~"Region: Northern",
                       var=='region_savannah'~"Region: Savannah",
                       var=='region_upper_east'~"Region: Upper East",
                       var=='region_upper_west'~"Region: Upper West",
                       var=='e_lit_per'~'Endline Literacy Z-Score',
                       var=='e_num_per'~'Endline Numeracy Z-Score',
                       var=='e_ef_per'~'Endline EF Z-Score',
                       var=='poverty'~'Poverty',
                       var=='cg_schooling'~'Caregiver Attended Primary School',
                       var=='cg_age'~'Caregiver age',
                       var=='cg_female'~'Caregiver is Female',
                       T~'Endline SEL Z-Score',
  )
  ) %>%
  rename('Statistic'=var)

latex=xtable(out,
             type = "latex",
             caption = "\\label{appendix:missing_sumstat}Summary Statistics, Excluded vs Not Excluded")

names(latex)=c("\\multicolumn{1}{p{2.5in}}{Statistic}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Not Excluded}",
               "\\multicolumn{1}{p{0.25in}}{\\centering Excluded}",
               "\\multicolumn{1}{p{0.5in}}{\\centering P-Value: Exclusion}"
)

print(latex, sanitize.colnames.function=function(x){x},
      caption.placement = "top",
      file="tables/appendix_a2.tex",
      include.rownames=FALSE)

cat(
  "\\par {\\footnotesize \\textit{Note:} This table presents descriptive statistics.",
  "Endline Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7",
  "and if the sum of FIES was larger than 4, respectively.",
  "Food insecurity experiences were not recorded for the excluded respondents.}\n",
  file = "tables/appendix_a2.tex", append = TRUE
)
