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
  mutate(fi_report_mismatch=case_when(e_cfies_indicator!=e_fies_indicator~1,
                                      T~0),
         attend=case_when(as.double(e_attend)>3~1,
                          T~0),
         poverty_high=case_when(poverty > median(poverty, na.rm=TRUE) ~ 1,
                                T~0),
         cg_no_primary=case_when(cg_primary==1 ~ 0,
                                 cg_primary==0 ~ 1,
                                 T~NA_real_))

reg1 <- glm(fi_report_mismatch ~ age + female*poverty_high + age_pct_rank + attend + cg_female + cg_no_primary + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg1)

reg2 <- glm(fi_report_mismatch ~ age + female*cg_no_primary + age_pct_rank + attend + cg_female + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg2)

reg3 <- glm(fi_report_mismatch ~ age*poverty_high + female + age_pct_rank + attend + cg_female + cg_no_primary + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg3)

reg4 <- glm(fi_report_mismatch ~ age*cg_no_primary + female + age_pct_rank + attend + cg_female + cg_age + treatment +
              region_north_east + region_northern + region_upper_east + region_upper_west + factor(month),
            data = full_data_w)
summary(reg4)

combined_coef_map <- c("female"="Child is Female",
                       'age'="Child is 10-17",
                       'poverty_high'='Poverty (Above Median)',
                       'cg_no_primary'='Caregiver No Primary',
                       'female:poverty_high'='Female x Poverty (Above Median)',
                       'female:cg_no_primary'='Female x Caregiver No Primary',
                       'age:poverty_high'='10-17 x Poverty',
                       'age:cg_no_primary'='10-17 x Cg. No Primary',
                       'age_pct_rank'='Child Rank',
                       'attend'='Attends School',
                       'cg_primary'="Caregiver Primary",
                       'cg_age'='Caregiver Age',
                       'cg_female'='Caregiver Female',
                       'treatment'='PNP Treatment',
                       'region_north_east'="Reg: Northeast",
                       'region_northern'="Reg: Northern",
                       'region_upper_east'="Reg: Upper East",
                       'region_upper_west'="Reg: Upper West",
                       '(Intercept)'='Intercept')

combined_notes <- "Note: Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. The outcome in each model is FI Report Mismatch, where FI Report Mismatch=1 if children and caregivers of a given household did not match in their food insecurity indicator reports. Female x Poverty (Model 1) includes an interaction term between child sex and poverty. Female x Education (Model 2) includes an interaction term between child sex and caregiver education. Age x Poverty (Model 3) includes an interaction term between child age group and poverty. Age x Education (Model 4) includes an interaction term between child age group and caregiver education. Child rank is constructed as a percentile rank of all children in a given household from oldest to youngest. Poverty is constructed as a binary flag indicating above median poverty probability index. Caregiver No Primary=1 if the caregiver did not complete primary education or more. All models includes month fixed effect. Robust standard errors clustered by caregiver are reported."

subgroup_stars <- c('*' = .05, '**' = .01, '***' = .001)

gm_n_only <- list(list("raw" = "nobs", "clean" = "N", "fmt" = format_nobs))

modelsummary(list("Female x Poverty"=reg1,
                  "Female x Education"=reg2,
                  "Age x Poverty"=reg3,
                  "Age x Eduation"=reg4),
             fmt=f,
             cluster='careid',
             coef_map=combined_coef_map,
             gof_omit = 'AIC|BIC|Std.Errors|RMSE',
             gof_map=gm_n_only,
             stars = subgroup_stars,
             notes = combined_notes,
             latex_options = c("booktabs", "scale_down", "HOLD_position"),
             out="tables/appendix_g.tex",
             escape = FALSE)

suppress_table_entry("tables/appendix_g.tex")
