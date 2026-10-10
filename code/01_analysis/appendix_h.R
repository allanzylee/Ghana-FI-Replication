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
  mutate(e_fies_scale=as.factor(e_fies_scale))

va_ols_input_region <- expand.grid(category=c('lit','num','ef','sel'),
                                   model=c('~ e_cfies_scale+e_fies_scale+female+age+treatment+region_north_east+region_northern+region_upper_east+region_upper_west+age_pct_rank+factor(month)+'))

va_ols_region_results<- pmap(va_ols_input_region,
                             reg_func) %>%
  set_names('Literacy',
            'Numeracy',
            'EF',
            'SEL')

modelsummary(va_ols_region_results,
             cluster='careid',
             coef_omit = "^(?!.*tercept|.*[0-9]|.*outcome|.*treatment)",
             coef_map=c(e_cfies_scale1="Child: Few Exp. (CFIES=1-6)",
                           e_cfies_scale2="Child: Several Exp. (CFIES=7-10)",
                           e_cfies_scale3="Child: Many Exp. (CFIES=11-20)",
                           e_fies_scale0="FIES: Mild",
                           e_fies_scale1="Caregiver: Moderately Food Insecure (FIES=4-6)",
                           e_fies_scale2="Caregiver: Severely Food Insecure (FIES=7-8)",
                           'lagged_outcome'="Lagged Outcome",
                        'treatment'='Treatment',
                        '(Intercept)'='(Intercept)'),
             gof_omit = 'AIC|BIC|Std.Errors',
             gof_map=gm,
             stars = c('*' = .05,
                       '**' = .01,
                       '***' = .001),
             notes = "Note: Robust standard errors clustered by caregiver are reported. Results reported come from a value-added model that controls for midline standardized outcomes and covariates. Covariates in the regression that are not shown include child sex, child age group, region, and household randomized treatment. CFIES coefficients are reported relative to the category of no experience of food insecurity. FIES coefficients are reported relative to the category of mild food insecurity.",
             out="tables/appendix_h.tex",
             latex_options = "scale_down",
             escape = FALSE)

shrink_table_width("tables/appendix_h.tex")
