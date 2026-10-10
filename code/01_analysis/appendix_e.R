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

full_data_w <- read_rds('data/derived/regression_build_w.rds')

overlap_cor_df<-full_data_w %>%
  dplyr::select(childid,
                careid,
                age,
                female,
                matches("^fs\\d+")) %>%
  mutate(across(matches('^fs\\d+_child'),
                ~case_when(.==2~1,
                           T~.))) %>%
  dplyr::select(childid,
                careid,
                age,
                female,
                'worry_child'=fs1_child,
                'worry_cg'=fs1_cg,
                'cut_child'=fs4_child,
                'cut_cg'=fs5_cg,
                'skip_child'=fs6_child,
                'skip_cg'=fs4_cg,
                'hungry_child'=fs5_child,
                'hungry_cg'=fs7_cg
  )

cor_func<-function(by_var=NULL){

  cor<-overlap_cor_df %>%
    group_by(across({{by_var}})) %>%
    summarise(worry=list(cor.test(worry_child, worry_cg)),
              cut=list(cor.test(cut_child, cut_cg)),
              skip=list(cor.test(skip_child, skip_cg)),
              hungry=list(cor.test(hungry_child, hungry_cg)))

  if(length(by_var)<=1){

    if(is.null(by_var)){

      out<-tribble(~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']]) %>%
        mutate(category='Overall')

    } else if(by_var=='female') {

      out<-tribble(~female, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$female[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                   cor$female[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']]) %>%
        mutate(category=case_when(female==1 ~ 'Child is Female',
                                  T~'Child is Male')) %>%
        dplyr::select(-female)

    } else{

      out<-tribble(~age, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                   cor$age[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                   cor$age[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']]) %>%
        mutate(category=case_when(age==1 ~ 'Child is 10-17',
                                  T~'Child is 5-9')) %>%
        dplyr::select(-age)

    }

  } else {

    out<-tribble(~age,~female, ~worry, ~worry_pval, ~cut, ~cut_pval, ~skip, ~skip_pval, ~hungry, ~hungry_pval,
                 cor$age[1],cor$female[1],cor$worry[[1]][['estimate']], cor$worry[[1]][['p.value']], cor$cut[[1]][['estimate']], cor$cut[[1]][['p.value']],cor$skip[[1]][['estimate']], cor$skip[[1]][['p.value']], cor$hungry[[1]][['estimate']], cor$hungry[[1]][['p.value']],
                 cor$age[2],cor$female[2],cor$worry[[2]][['estimate']], cor$worry[[2]][['p.value']], cor$cut[[2]][['estimate']], cor$cut[[2]][['p.value']],cor$skip[[2]][['estimate']], cor$skip[[2]][['p.value']], cor$hungry[[2]][['estimate']], cor$hungry[[2]][['p.value']],
                 cor$age[3],cor$female[3],cor$worry[[3]][['estimate']], cor$worry[[3]][['p.value']], cor$cut[[3]][['estimate']], cor$cut[[3]][['p.value']],cor$skip[[3]][['estimate']], cor$skip[[3]][['p.value']], cor$hungry[[3]][['estimate']], cor$hungry[[3]][['p.value']],
                 cor$age[4],cor$female[4],cor$worry[[4]][['estimate']], cor$worry[[4]][['p.value']], cor$cut[[4]][['estimate']], cor$cut[[4]][['p.value']],cor$skip[[4]][['estimate']], cor$skip[[4]][['p.value']], cor$hungry[[4]][['estimate']], cor$hungry[[4]][['p.value']])%>%
      mutate(category=case_when(age==1 & female==1 ~ 'Child is Female (10-17)',
                                age==1 & female==0 ~ 'Child is Male (10-17)',
                                age==0 & female==1 ~ 'Child is Female (5-9)',
                                T~'Child is Male (5-9)')) %>%
      dplyr::select(-age,
                    -female)
  }
}

overlap_cor=map_dfr(list(c(NULL),
                 c('age'),
                 c('female'),
                 c('age','female')
                 ),
            cor_func)

latex_cor<-xtable(overlap_cor %>%
         dplyr::select('Category'=category,
                'Worry'=worry,
                'Cut'=cut,
                'Skip'=skip,
                'Hungry'=hungry),
         caption = "\\label{appendix:overlap_cor}Correlation of Overlapping Items of CFIES and FIES")

print(latex_cor, type = "latex", "tables/appendix_e.tex",
      caption.placement = "top",
      include.rownames = FALSE)

cat(
  "\\par {\\footnotesize \\textit{Note:} Pearson correlations between the overlapping items of CFIES and FIES are shown.",
  "For results by group, Pearson correlations between food insecurity reports of children of each stated group and food insecurity reports of their caregivers are reported.",
  "CFIES was transformed to binary variables, where ``Many Times'' and ``1--2 Times'' represented 1 and ``Never'' represented 0.",
  "``Worry'' represents whether the respondent was worried about running out of food to eat (FI 1 of child and caregiver scales).",
  "``Cut'' represents whether the respondent cut the size of their meals (FI 4 of child scale and FI 5 of caregiver scale).",
  "``Skip'' represents whether the respondent skipped a meal (FI 6 of child scale and FI 4 of caregiver scale).",
  "``Hungry'' represents whether the respondent was hungry but did not eat (FI 5 of child scale and FI 7 of caregiver scale).}\n",
  file = "tables/appendix_e.tex", append = TRUE
)
