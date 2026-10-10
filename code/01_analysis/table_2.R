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

library(tidyverse)
library(dplyr)
library(stargazer)
library(glue)
library(ltm)
library(xtable)
library(writexl)
library(knitr)
library(kableExtra)
library(flextable)

full_data_w <- read_rds('data/derived/regression_build_w.rds')

overlap_perc_raw<-full_data_w %>%
  dplyr::select(childid,
         careid,
         matches("^fs\\d+"),
         female,
         age) %>%
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>%
  dplyr::select(childid,
                careid,
                female,
                age,
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
                )

perc_diff_input<-expand.grid(cat=c('overall','female','male','older','younger','older_female','older_male','younger_female','younger_male'),
                             outcome=c('worry','cut','skip','hungry'))

perc_diff_func<-function(cat,outcome){

  if(cat=='overall'){

    df=overlap_perc_raw

  } else if(cat=='female'){

    df=overlap_perc_raw %>% filter(female==1)

  } else if(cat=='male'){

    df=overlap_perc_raw %>% filter(female==0)

  } else if(cat=='older'){

    df=overlap_perc_raw %>% filter(age==1)

  } else if(cat=='younger'){

    df=overlap_perc_raw %>% filter(age==0)

  } else if(cat=='older_female'){

    df=overlap_perc_raw %>% filter(age==1, female==1)

  } else if(cat=='older_male'){

    df=overlap_perc_raw %>% filter(age==1,female==0)

  } else if(cat=='younger_female'){

    df=overlap_perc_raw %>% filter(age==0, female==1)

  } else {

    df=overlap_perc_raw %>% filter(age==0,female==0)

  }

  ch_outcome=paste0(outcome,'_child')
  cg_outcome=paste0(outcome,'_cg')

  tab <- table(
    Child  = overlap_perc_raw[[ch_outcome]],
    Parent = overlap_perc_raw[[cg_outcome]]
  )

  mc=mcnemar.test(tab,correct = F)

  out <- df %>%
    summarise(
      child_pct  = mean(!!ensym(ch_outcome), na.rm = TRUE) * 100,
      cg_pct = mean(!!ensym(cg_outcome),    na.rm = TRUE) * 100,
      N_pairs    = sum(!is.na(!!ensym(ch_outcome)) & !is.na(!!ensym(cg_outcome))),
      discordant_pct = mean(!!ensym(ch_outcome) != !!ensym(cg_outcome), na.rm = TRUE) * 100
    ) %>%
    mutate(
      diff_pp = child_pct - cg_pct,
      pval    = mc$p.value,
      cat=cat,
      outcome=outcome
    )

}

overlap_perc<-pmap_dfr(perc_diff_input,
              perc_diff_func)

latex_perc <- overlap_perc %>%
  mutate(
    group = factor(cat, levels = c(
      "overall",
      "female", "male",
      "older", "younger",
      "older_female", "older_male",
      "younger_female", "younger_male"
    ))
  ) %>%
  mutate(
    group_label = recode(
      group,
      "overall"        = "Overall",
      "female"         = "Child is Female",
      "male"           = "Child is Male",
      "younger"        = "Child is 5-9",
      "older"          = "Child is 10-17",
      "younger_female" = "Child is Female (5-9)",
      "younger_male"   = "Child is Male (5-9)",
      "older_female"   = "Child is Female (10-17)",
      "older_male"     = "Child is Male (10-17)"
    )
  ) %>%
  pivot_wider(
    names_from  = cat,
    values_from = c(child_pct, cg_pct, pval)
  ) %>%
  arrange(group) %>%
  mutate(
    child_pct = coalesce(!!!dplyr::select(., dplyr::starts_with("child_pct"))),
    cg_pct    = coalesce(!!!dplyr::select(., dplyr::starts_with("cg_pct"))),
    pval      = coalesce(!!!dplyr::select(., dplyr::starts_with("pval")))
  ) %>%
  dplyr::select(group_label, outcome, child_pct, cg_pct, pval) %>%
  pivot_wider(
    names_from  = outcome,
    values_from = c(child_pct, cg_pct, pval)
  ) %>%
  dplyr::select(
    group_label,
    contains('worry'), contains('cut'), contains('skip'), contains('hungry')
  ) %>%
  mutate(
    across(matches("child|cg"), ~ paste0(sprintf("%.2f", .), "\\%")),
    across(contains("pval"),    ~ sprintf("%.2f", .))
  )

body <- kable(
  latex_perc,
  format    = "latex",
  booktabs  = TRUE,
  col.names = c(
    "Group",
    rep(c("Child (\\%)", "Caregiver (\\%)", "P-value"), 4)
  ),
  align  = "lcccccccccccc",
  escape = FALSE
) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(
    c(" " = 1, "Worry" = 3, "Cut" = 3, "Skip" = 3, "Hungry" = 3),
    escape = FALSE
  )

body <- gsub("\\\\begin\\{tabular\\}\\[t\\]", "\\\\begin{tabular}", body)

writeLines(body,
           "tables/table_2.tex")
