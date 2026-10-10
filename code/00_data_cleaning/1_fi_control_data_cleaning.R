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

e_child <- read_dta("data/raw/03_PNP_Endline_ChildSurvey.dta") %>%
  dplyr::select(-contains("gb")) %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  mutate(across(contains('fs'),~as.double(case_when(.<0~NA,
                                                    T~.)))) %>%
  rename(careid=caseid) %>%
  filter(io2==1) %>%
  mutate(childage=as.double(childage)) %>%
  mutate(across(matches('fs[0-9]'),~case_when(is.na(.)~1,
                                              T~0),
                .names='{col}_na')) %>%
  mutate(na=fs1_na+fs2_na+fs3_na+fs4_na+fs5_na+fs6_na+fs7_na+fs8_na+fs9_na+fs10_na) %>%
  filter(na==0) %>%
  select(-na,
         -matches('fs[0-9]_na'))

e_cg <- read_dta("data/raw/02_PNP_Endline_CaregiverSurvey.dta") %>%
  mutate(careid=as.double(careid),
         childid=as.double(childid)) %>%
  mutate(across(contains('fs'),~as.double(.))) %>%
  mutate(across(matches('fs[0-9]'),~case_when(is.na(.)~1,
                                              T~0),
                .names='{col}_na')) %>%
  mutate(na=fs1_na+fs2_na+fs3_na+fs4_na+fs5_na+fs6_na+fs7_na+fs8_na) %>%
  filter(na==0) %>%
  select(-na,
         -matches('fs[0-9]_na'))

m_child <- read_dta("data/raw/03_PNP_Midline_ChildSurvey.dta") %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  mutate(across(contains('fs'),~as.double(case_when(.<0~NA,
                                                    T~.)))) %>%
  filter(io2==1) %>%
  mutate(across(matches('fs[0-9]'),~case_when(is.na(.)~1,
                                              T~0),
                .names='{col}_na')) %>%
  mutate(na=fs1_na+fs2_na+fs3_na+fs4_na+fs5_na+fs6_na+fs7_na+fs8_na+fs9_na+fs10_na) %>%
  filter(na==0) %>%
  select(-na,
         -matches('fs[0-9]_na'))

m_cg <- read_dta("data/raw/02_PNP_Midline_CaregiverSurvey.dta") %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  mutate(across(matches('fs[0-9]'),~case_when(is.na(.)~1,
                                              T~0),
                .names='{col}_na')) %>%
  mutate(na=fs1_na+fs2_na+fs3_na+fs4_na+fs5_na+fs6_na+fs7_na+fs8_na) %>%
  filter(na==0) %>%
  select(-na,
         -matches('fs[0-9]_na'))

e_child_fs<-e_child %>%
  select(childid,
         matches('fs[0-9]')
  ) %>%
  mutate(across(contains('fs'),~as.double(case_when(.==1~2,
                                                    .==2~1,
                                                    .==3~0,
                                                    T~NA_real_)))) %>%
  mutate(e_cfies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>%
  mutate(e_cfies_indicator=case_when(e_cfies_sum>=7~1,
                                     T~0),
         e_cfies_scale=as.factor(case_when(e_cfies_sum==0~0,
                                           e_cfies_sum>=1 & e_cfies_sum<=6~1,
                                           e_cfies_sum>=7 & e_cfies_sum<=10~2,
                                           T~3))) %>%
  select(-matches('fs[0-9]'))

e_cg_fs<-e_cg %>%
  select(childid,
         matches('fs[0-9]')) %>%
  mutate(e_fies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>%
  mutate(e_fies_indicator=case_when(e_fies_sum>=4~1,
                                    T~0),
         e_fies_scale=as.factor(case_when(e_fies_sum<=3~0,
                                          e_fies_sum>=4 & e_fies_sum<=6~1,
                                          T~2))) %>%
  select(-matches('fs[0-9]'))

m_child_fs<-m_child %>%
  select(childid,
         matches('fs[0-9]'),
  ) %>%
  mutate(across(contains('fs'),~as.double(case_when(.==1~2,
                                                    .==2~1,
                                                    .==3~0,
                                                    T~NA_real_)))) %>%
  mutate(m_cfies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>%
  mutate(m_cfies_indicator=case_when(m_cfies_sum>=7~1,
                                     T~0),
         m_cfies_scale=as.factor(case_when(m_cfies_sum==0~0,
                                           m_cfies_sum>=1 & m_cfies_sum<=6~1,
                                           m_cfies_sum>=7 & m_cfies_sum<=10~2,
                                           T~3))) %>%
  select(-matches('fs[0-9]'))

m_cg_fs<-m_cg %>%
  select(childid,
         matches('fs[0-9]')) %>%
  mutate(m_fies_sum=rowSums(select(.,contains('fs')),na.rm=T)) %>%
  mutate(m_fies_indicator=case_when(m_fies_sum>=4~1,
                                    T~0),
         m_fies_scale=as.factor(case_when(m_fies_sum<=3~0,
                                          m_fies_sum>=4 & m_fies_sum<=6~1,
                                          T~2))) %>%
  select(-matches('fs[0-9]'))

fi <- e_cg_fs %>%
  left_join(e_child_fs,by=c("childid"))%>%
  left_join(m_cg_fs,by=c("childid"))%>%
  left_join(m_child_fs,by=c("childid"))

m_cg_pe <- m_cg %>%
  dplyr::select(careid,childid,matches('pe[0-9]a'),pe8,pe9,pe10a,pe10b,pe10c,pe10d,pe10e) %>%
  mutate_at(vars(pe8,pe9),funs(new=case_when(. == 4 ~ 3,
                                             . == 3 ~ 2,
                                             . == 2 ~ 1,
                                             . == 1 ~ 0,
                                             TRUE ~ NA_real_))) %>%
  dplyr::select(careid,childid,pe1a:pe6a,pe8=pe8_new,pe9=pe9_new,pe10a,pe10b,pe10c,pe10d,pe10e) %>%
  mutate(across(everything(),~as.double(.))) %>%
  mutate(across(contains('pe'),~case_when(is.na(.)~0,
                                          T~.))) %>%
  mutate(m_cg_edu_engagement=dplyr::select(., contains("pe")) %>% rowSums()) %>%
  select(childid,
         careid,
         m_cg_edu_engagement)

e_cg_pe <- e_cg %>%
  dplyr::select(careid,childid,matches('pe[0-9]a'),pe8,pe9,pe10a,pe10b,pe10c,pe10d,pe10e) %>%
  mutate_at(vars(pe8,pe9),funs(new=case_when(. == 4 ~ 3,
                                       . == 3 ~ 2,
                                       . == 2 ~ 1,
                                       . == 1 ~ 0,
                                       TRUE ~ NA_real_))) %>%
  dplyr::select(careid,childid,pe1a:pe6a,pe8=pe8_new,pe9=pe9_new,pe10a,pe10b,pe10c,pe10d,pe10e) %>%
  mutate(across(everything(),~as.double(.))) %>%
  mutate(across(contains('pe'),~case_when(is.na(.)~0,
                                          T~.))) %>%
  mutate(e_cg_edu_engagement=dplyr::select(., contains("pe")) %>% rowSums()) %>%
  select(childid,
         careid,
         e_cg_edu_engagement)

m_cg_emotional_engagement<-m_cg %>%
  dplyr::select(childid,
                careid,
                es1,
                es2,
                es3,
                es4,
                es5,
                es6) %>%
  mutate(es6=case_when(es6==4~1,
                       es6==3~2,
                       es6==2~3,
                       T~4)) %>%
  mutate(across(contains('es'),~case_when(is.na(.)~0,
                                          T~.)),
         m_cg_emotional_engagement=es1+es2+es3+es4+es5+es6) %>%
  dplyr::select(-contains('es'))

e_cg_emotional_engagement<-e_cg %>%
  dplyr::select(childid,
         careid,
         es1,
         es2,
         es3,
         es4,
         es5,
         es6) %>%
  mutate(es6=case_when(es6==4~1,
                       es6==3~2,
                       es6==2~3,
                       T~4)) %>%
  mutate(across(contains('es'),~case_when(is.na(.)~0,
                                          T~.)),
         e_cg_emotional_engagement=es1+es2+es3+es4+es5+es6) %>%
  dplyr::select(-contains('es'))

m_ch_motiv_esteem <- m_child %>%
  mutate(across(contains("mo"),~as.double(.)),
         across(contains("mo"),~case_when(.<0~0,T~.))) %>%
  mutate(across(matches("se[0-9]"),~as.double(.)),
         across(matches("se[0-9]"),~case_when(.<0~0,T~.))) %>%
  mutate(m_ch_motiv=dplyr::select(., contains("mo")) %>% rowSums()) %>%
  mutate(across(c(se2,se5,se8,se9),~case_when(. == 4 ~ 1,
                                              . == 3 ~ 2,
                                              . == 2 ~ 3,
                                              . == 1 ~ 4,
                                              TRUE ~ NA_real_))
  ) %>%
  mutate(m_ch_esteem=dplyr::select(., matches("se[0-9]")) %>% rowSums()) %>%
  dplyr::select(childid,careid,m_ch_motiv)

e_ch_motiv_esteem <- e_child %>%
  mutate(across(contains("mo"),~as.double(.)),
         across(contains("mo"),~case_when(.<0~0,T~.))) %>%
  mutate(across(matches("se[0-9]"),~as.double(.)),
         across(matches("se[0-9]"),~case_when(as.double(.)<0~0,T~as.double(.)))) %>%
  mutate(e_ch_motiv=dplyr::select(., contains("mo")) %>% rowSums()) %>%
  mutate(across(c(se2,se5,se6,se8,se9),~case_when(. == 4 ~ 1,
                                             . == 3 ~ 2,
                                             . == 2 ~ 3,
                                             . == 1 ~ 4,
                                             TRUE ~ NA_real_))
         ) %>%
  mutate(e_ch_esteem=dplyr::select(., matches("se[0-9]")) %>% rowSums()) %>%
  dplyr::select(childid,careid,e_ch_motiv
                )

cg_mh<-e_cg %>%
  dplyr::select(childid,
         careid,
         contains('mh')) %>%
  mutate(cg_mh_scale=rowSums(dplyr::select(.,contains('mh')),na.rm=T),
         childid=as.double(childid),
         careid=as.double(careid)) %>%
  select(-matches('mh[0-9]'))

controls<-e_ch_motiv_esteem %>%
  left_join(m_ch_motiv_esteem,by=c('childid','careid')) %>%
  left_join(e_cg_pe,by=c('childid','careid')) %>%
  left_join(e_cg_emotional_engagement,by=c('childid','careid')) %>%
  left_join(m_cg_pe,by=c('childid','careid')) %>%
  left_join(m_cg_emotional_engagement,by=c('childid','careid')) %>%
  mutate(across(c(childid,careid),~as.double(.))) %>%
  left_join(cg_mh,
            by=c('childid',
                 'careid')) %>%
  left_join(e_child %>%
              select(childid,
                     enum_id,
                     treatment),
            by=c('childid'))

std_ctrl<-function(var){

  df<-controls %>%
    select(childid,
           enum_id,
           treatment,
           var) %>%
    filter(!is.na(!!sym(var)),
           !is.na(enum_id))

  fm=as.formula(glue('{var}~factor(enum_id)'))
  model <- lm(fm, data = df)
  df[['res']] <- resid(model)

  mean_res <- df %>%
    filter(treatment==0) %>%
    summarise(mean=mean(res)) %>%
    pull()

  sd_res <- df %>%
    filter(treatment==0) %>%
    summarise(sd=sd(res)) %>%
    pull()

  out<-df %>%
    mutate(mean=mean_res,
           sd=sd_res) %>%
    mutate(z_score=(res-mean)/sd) %>%
    dplyr::select(
      childid,
      !!quo_name(var) := z_score)

  return(out)

}

input=c('e_ch_motiv',
        'e_cg_edu_engagement',
        'e_cg_emotional_engagement',
        'm_ch_motiv',
        'm_cg_edu_engagement',
        'm_cg_emotional_engagement',
        'cg_mh_scale')

ctrl_std=map(input,
             std_ctrl) %>%
  reduce(left_join, by = "childid")

saveRDS(fi, "data/derived/fi.rds")
saveRDS(ctrl_std, "data/derived/controls.rds")
