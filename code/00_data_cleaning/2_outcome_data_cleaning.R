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
  mutate(across(contains('fs'),~as.double(.))) %>%
  rename(careid=caseid) %>%
  filter(io2==1) %>%
  mutate(childage=as.double(childage))

m_child <- read_dta("data/raw/03_PNP_Midline_ChildSurvey.dta") %>%
  mutate(across(contains('id'),~as.double(.))) %>%
  filter(io2==1)

e_lit<-e_child %>%
  select(childid,
         childage,
         ov1,
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         matches("pa[0-9]"),
         any_of(c("lc1","lc2","lc3")),
         enum_id,
         treatment) %>%
  mutate(across(any_of(c("lc1","lc2","lc3")),~case_when(. %in% c(-999,-888)~0,
                                                        T~.))) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~150,
                           T~136)) %>%
  mutate(e_lit_per=sum_correct/total_q) %>%
  select(childid,e_lit_per,enum_id,treatment)

m_lit<-m_child %>%
  select(childid,
         childage=cr6,
         ov1,
         starts_with("nr"),
         starts_with("sp"),
         starts_with("or"),
         starts_with("oc"),
         matches("pa[0-9]"),
         enum_id,
         treatment) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~150,
                           T~136)) %>%
  mutate(m_lit_per=sum_correct/total_q) %>%
  select(childid,m_lit_per,enum_id,treatment)

e_num<-e_child %>%
  select(childid,
         childage,
         matches("co[0-9]"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         matches("su[0-9]"),
         starts_with("mu"),
         matches("di[0-9]"),
         enum_id,
         treatment) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~53,
                           T~47)) %>%
  mutate(e_num_per=sum_correct/total_q) %>%
  select(childid,e_num_per,enum_id,treatment)

m_num<-m_child %>%
  select(childid,
         childage=cr6,
         matches("co[0-9]"),
         starts_with("nd"),
         starts_with("mn"),
         starts_with("nu"),
         starts_with("wp"),
         starts_with("ad"),
         matches("su[0-9]"),
         starts_with("mu"),
         matches("di[0-9]"),
         enum_id,
         treatment) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~53,
                           T~47)) %>%
  mutate(m_num_per=sum_correct/total_q) %>%
  select(childid,m_num_per,enum_id,treatment)

e_ef<-e_child %>%
  select(childid,
         childage,
         starts_with("wm"),
         starts_with("sm"),
         enum_id,
         treatment) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~17,
                           T~11)) %>%
  mutate(e_ef_per=sum_correct/total_q) %>%
  select(childid,e_ef_per,enum_id,treatment)

m_ef<-m_child %>%
  select(childid,
         childage=cr6,
         starts_with("wm"),
         starts_with("sm"),
         enum_id,
         treatment) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('nr'),~as.double(.))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~17,
                           T~11)) %>%
  mutate(m_ef_per=sum_correct/total_q) %>%
  select(childid,m_ef_per,enum_id,treatment)

m_sel<-m_child %>%
  select(childid,
         childage=cr6,
         cr1:re11,
         enum_id,
         treatment) %>%
  dplyr::select(-re5,-re8) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~13,
                           T~10)) %>%
  mutate(m_sel_per=sum_correct/total_q) %>%
  mutate(m_sel_per=case_when(m_sel_per>1~m_sel_per*10/13,
                             T~m_sel_per)) %>%
  select(childid,m_sel_per,enum_id,treatment)

e_sel<-e_child %>%
  select(childid,
         childage,
         cr1:re11,
         enum_id,
         treatment) %>%
  dplyr::select(-re5,-re8) %>%
  mutate(across(everything(),~case_when(.<0~NA,
                                        T~.)),
         across(contains('cr'),~case_when(.==2~0,
                                          .==1~1,
                                          T~NA_real_))) %>%
  mutate(sum_correct=rowSums(across(-c(childid,childage, enum_id, treatment)),na.rm=T),
         total_q=case_when(childage>=10~13,
                           T~10)) %>%
  mutate(e_sel_per=sum_correct/total_q) %>%
  mutate(e_sel_per=case_when(e_sel_per>1~e_sel_per*10/13,
                             T~e_sel_per)) %>%
  select(childid,e_sel_per,enum_id,treatment)

std_outcome<-function(df_str){

  outcome<-paste0(df_str,"_per")
  df<-get(df_str) %>%
    filter(!is.na(!!sym(outcome)),
           !is.na(enum_id))

  fm=as.formula(glue('{outcome}~factor(enum_id)'))
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
           !!quo_name(outcome) := z_score)

  return(out)

}

m_lit_z=std_outcome('m_lit')
m_sel_z=std_outcome('m_sel')
m_num_z=std_outcome('m_num')
m_ef_z=std_outcome('m_ef')
e_lit_z=std_outcome('e_lit')
e_sel_z=std_outcome('e_sel')
e_num_z=std_outcome('e_num')
e_ef_z=std_outcome('e_ef')

outcome <- m_lit_z %>%
  left_join(m_sel_z,by=c("childid")) %>%
  left_join(m_num_z,by=c("childid")) %>%
  left_join(m_ef_z,by=c("childid")) %>%
  left_join(e_sel_z,by=c("childid")) %>%
  left_join(e_lit_z,by=c("childid")) %>%
  left_join(e_num_z,by=c("childid")) %>%
  left_join(e_ef_z,by=c("childid"))

saveRDS(outcome, "data/derived/outcome_zscore_checker.rds")

careid_lookup <- bind_rows(
  e_child %>% dplyr::select(childid, careid),
  m_child %>% dplyr::select(childid, careid)
) %>%
  distinct(childid, .keep_all = TRUE)

outcome_raw <- careid_lookup %>%
  left_join(e_lit %>% dplyr::select(childid, e_lit_per_raw = e_lit_per), by = "childid") %>%
  left_join(m_lit %>% dplyr::select(childid, m_lit_per_raw = m_lit_per), by = "childid") %>%
  left_join(e_num %>% dplyr::select(childid, e_num_per_raw = e_num_per), by = "childid") %>%
  left_join(m_num %>% dplyr::select(childid, m_num_per_raw = m_num_per), by = "childid") %>%
  left_join(e_ef  %>% dplyr::select(childid, e_ef_per_raw  = e_ef_per),  by = "childid") %>%
  left_join(m_ef  %>% dplyr::select(childid, m_ef_per_raw  = m_ef_per),  by = "childid") %>%
  left_join(e_sel %>% dplyr::select(childid, e_sel_per_raw = e_sel_per), by = "childid") %>%
  left_join(m_sel %>% dplyr::select(childid, m_sel_per_raw = m_sel_per), by = "childid")

saveRDS(outcome_raw, "data/derived/outcome_raw.rds")
