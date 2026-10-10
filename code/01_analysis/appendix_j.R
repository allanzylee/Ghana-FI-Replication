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

reg_func_raw <- function(category, model){
  e_category_str<-paste0("e_",category,"_per_raw")
  m_category_str<-paste0("m_",category,"_per_raw")

  for_reg<-full_data_w %>%
    rename(lagged_outcome=m_category_str)

  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome'))
  reg <- feols(fm,
               data=for_reg,
               cluster=~careid)
  return(reg)
}

va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                                 model=c('~ e_cfies_indicator+e_fies_indicator+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

va_ols_region_results<- pmap(va_ols_input_region,
                             reg_func_raw) %>%
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

modelsummary(va_ols_region_results,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*indicator|.*outcome|.*reatment)",
             coef_map=c('e_cfies_indicator'="Child-Reported FI",
                        'e_fies_indicator'="Caregiver-Reported FI",
                        'lagged_outcome'="Lagged Outcome",
                        'treatment'="Treatment",
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7 and if the sum of FIES was larger than 4, respectively. Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, child rank in percentile by age, region, child rank in percentile by age, and month fixed effects.",
             out="tables/appendix_j.tex",
             escape = FALSE)

shrink_table_width("tables/appendix_j.tex")
