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
  mutate(`FI Report Mismatch`=case_when(e_cfies_indicator!=e_fies_indicator~1,
                        T~0),
         attend=case_when(as.double(e_attend)>3~1,
                          T~0),
         cg_no_primary=case_when(cg_primary==1 ~ 0,
                                 cg_primary==0 ~ 1,
                                 T~NA_real_))

reg<-glm(`FI Report Mismatch` ~ age + female + age_pct_rank+ attend+cg_female+cg_no_primary+cg_age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west +factor(month),
         data = full_data_w)
summary(reg)

modelsummary(dvnames(reg),
             fmt=f,
             cluster='careid',
             coef_map=c('e_cfies_indicator'="Child-Reported FI",
                        'e_fies_indicator'="Caregiver-Reported FI",
                        'lagged_outcome'="Lagged Outcome",
                        "female"="Child is Female",
                        'age'='Child is 10-17',
                        'cg_no_primary'="Caregiver No Primary",
                        'cg_age'='Caregiver Age',
                        'cg_female'='Caregiver is Female',
                        'poverty'='Poverty',
                        'treatment'='PNP Treatment',
                        'region_north_east'="Region: North East",
                        'region_northern'="Region: Northern",
                        'region_upper_east'="Region: Upper East",
                        'region_upper_west'="Region: Upper West",
                        'e_ch_health2'='Child Reported Poor Health',
                        'e_ch_health3'='Child Reported Average Health',
                        'e_ch_health4'="Child Reported Good Health",
                        'e_ch_health5'="Child Reported Very Good Health",
                        "e_private_school"="Private School",
                        'e_cg_edu_engagement'="Caregiver Edu. Engagement Scale",
                        'e_ch_motiv'='Child Motivation Scale',
                        'e_ch_edu_asp'='Child Aspires Complete High School',
                        'e_cg_emotional_engagement'='Caregiver Emo. Engagement Scale',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             modelnames = "FI Report Mismatch",
             notes = "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. FI Report Mismatch=1 if children and caregivers of a given household did not match in their food insecurity indicator reports. Child rank is constructed as a percentile rank of all children in a given household from oldest to youngest. Model includes month fixed effect. Robust standard errors clustered by caregiver are reported.",
             latex_options = c("booktabs", "scale_down"),
             out="tables/appendix_f.tex",
             escape = FALSE)

shrink_table_width("tables/appendix_f.tex")
