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

summary_stat<-full_data_w %>%
  dplyr::select(
                contains('indicator'),
                contains('per')
                )

t_test_func<-function(var){

  df<-summary_stat %>%
    dplyr::select(contains(var)) %>%
    pivot_longer(everything(),
                 names_to='group',
                 values_to='var') %>%
    mutate(group=case_when(startsWith(group,'e_')~1,
                           T~0))

  test <- t.test(var~group,
         data=df)

    out<-tribble(~var,~midline,~endline,~pval,
                 var,test$estimate[1],test$estimate[2],test$p.value)

  return(out)

}

input<-c('cfies',
         '_fies',
         'lit',
         'num',
         'ef',
         'sel')

end_vs_mid_sum_stat<-map_dfr(input,
                             t_test_func)

out<-end_vs_mid_sum_stat %>%
  rename(
         'Midline Mean'=midline,
         'Endline Mean'=endline,
         'P-Value'=pval) %>%
  mutate(var=case_when(var=='cfies'~'Child-Reported FI',
                       var=='_fies'~'Caregiver-Reported FI',
                       var=='lit'~'Literacy Z-Score',
                       var=='num'~'Numeracy Z-Score',
                       var=='ef'~'EF Z-Score',
                       T~'SEL Z-Score',
                       )
         ) %>%
  rename('Statistic'=var)

latex=xtable(out,
             type = "latex",
             caption = "\\label{appendix:sumstat_midline_endline}Summary Statistics, Midline and Endline (N=3,771)")

print(latex, sanitize.colnames.function=function(x){x},
      caption.placement = "top",
      include.rownames=FALSE,
      file="tables/appendix_c.tex",)

cat(
  "\\par {\\footnotesize \\textit{Note:} This table presents descriptive statistics.",
  "Child- and Caregiver-Reported Food insecurity were defined as binary indicators if the sum of CFIES was larger than 7",
  "and if the sum of FIES was larger than 4, respectively.}\n",
  file = "tables/appendix_c.tex", append = TRUE
)
