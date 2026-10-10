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

full_data_w <- read_rds('data/derived/regression_build_w.rds')

va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ e_cfies_sum+e_fies_sum+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

va_ols_region_results<- pmap(va_ols_input_region,
                             reg_func) %>%
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

modelsummary(va_ols_region_results,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*sum|.*outcome|.*reatment)",
             coef_map=c('e_cfies_sum'="CFIES Raw Score",
                        'e_fies_sum'="FIES Raw Score",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Raw CFIES and FIES are reported. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, child rank in percentile by age, and month fixed effects.",
             out="tables/appendix_k.tex",
             escape = FALSE)

shrink_table_width("tables/appendix_k.tex")
