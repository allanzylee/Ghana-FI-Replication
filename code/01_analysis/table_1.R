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
  fastDummies::dummy_columns(c('e_cfies_scale', 'e_fies_scale')) %>%
  dplyr::select(-e_cfies_scale, -e_fies_scale) %>%
  mutate(disagree = as.integer(e_cfies_indicator != e_fies_indicator))

sum_stat_func <- function(var) {
  tribble(
    ~var, ~mean,                                                ~sd,
    var,  mean(full_data_w %>% pull(!!ensym(var)), na.rm = T), sd(full_data_w %>% pull(!!ensym(var)), na.rm = T)
  )
}

summed <- map_dfr(
  c(
    'e_cfies_indicator',
    'e_fies_indicator',
    'disagree',
    'e_cfies_scale_0',
    'e_cfies_scale_1',
    'e_cfies_scale_2',
    'e_cfies_scale_3',
    'e_fies_scale_0',
    'e_fies_scale_1',
    'e_fies_scale_2',
    'female',
    'age_num',
    'e_lit_per_raw',
    'e_num_per_raw',
    'e_ef_per_raw',
    'e_sel_per_raw',
    'region_north_east',
    'region_northern',
    'region_savannah',
    'region_upper_east',
    'region_upper_west',
    'cg_female',
    'cg_age',
    'cg_primary'
  ),
  sum_stat_func
) %>%
  mutate(
    across(
      -var,
      ~ case_when(
        str_detect(var, 'age_num|cg_age') ~ formatC(.x, format = "f", big.mark = ",", digits = 1),
        TRUE                              ~ formatC(.x, format = "f", big.mark = ",", digits = 3)
      )
    )
  ) %>%
  mutate(var = recode(
    var,
    'e_cfies_indicator'   = 'Child-Reported Food Insecurity (\\%)',
    'e_fies_indicator'    = 'Caregiver-Reported Food Insecurity (\\%)',
    'disagree'            = 'Child-Caregiver Food Insecurity Disagreement (\\%)',
    'e_cfies_scale_0'     = 'Child: No Food Insecurity (CFIES=0)',
    'e_cfies_scale_1'     = 'Child: Few Exp. (CFIES=1--6)',
    'e_cfies_scale_2'     = 'Child: Several Exp. (CFIES=7--10)',
    'e_cfies_scale_3'     = 'Child: Many Exp. (CFIES=11--20)',
    'e_fies_scale_0'      = 'Caregiver: Food Secure (FIES=0--3)',
    'e_fies_scale_1'      = 'Caregiver: Moderately Food Insecure (FIES=4--6)',
    'e_fies_scale_2'      = 'Caregiver: Severely Food Insecure (FIES=7--8)',
    'female'              = 'Child is Female (\\%)',
    'age_num'             = 'Child Age (Years)',
    'e_lit_per_raw'       = 'Literacy Raw Score (\\% Correct)',
    'e_num_per_raw'       = 'Numeracy Raw Score (\\% Correct)',
    'e_ef_per_raw'        = 'Executive Function Raw Score (\\% Correct)',
    'e_sel_per_raw'       = 'Social-emotional Learning Raw Score (\\% Correct)',
    'region_north_east'   = 'North East',
    'region_northern'     = 'Northern',
    'region_savannah'     = 'Savannah',
    'region_upper_east'   = 'Upper East',
    'region_upper_west'   = 'Upper West',
    'cg_female'           = 'Caregiver is Female (\\%)',
    'cg_age'              = 'Caregiver Age (Years)',
    'cg_primary'          = 'Caregiver Completed Primary Education or Above (\\%)'
  ))

make_row <- function(df, i) {
  paste0(
    df$var[i],  " & ",
    df$mean[i], " & ",
    df$sd[i],   " \\\\\n"
  )
}

panel_a_rows <- paste0(
  paste0(sapply(1:3, function(i) make_row(summed, i)), collapse = ""),
  "\\addlinespace\n",
  paste0(sapply(4:10, function(i) make_row(summed, i)), collapse = "")
)

panel_b_rows <- paste0(sapply(11:16, function(i) make_row(summed, i)), collapse = "")
panel_c_rows <- paste0(sapply(17:21, function(i) make_row(summed, i)), collapse = "")
panel_d_rows <- paste0(sapply(22:24, function(i) make_row(summed, i)), collapse = "")

n_row <- paste0(
  "Total Observations & ",
  formatC(nrow(full_data_w), big.mark = ","),
  " & \\\\\n"
)

latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Summary Statistics}\n",
  "\\label{tab:sumstats}\\small\n",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcc@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} \\\\\n",
  " & Mean & Std. Dev. \\\\\n",
  "\\midrule \\addlinespace\n",

  "\\multicolumn{3}{@{}l}{\\emph{Panel A: Food Insecurity Experiences}} \\\\ \\addlinespace\n",
  panel_a_rows,
  "\\addlinespace \\midrule \\addlinespace\n",

  "\\multicolumn{3}{@{}l}{\\emph{Panel B: Child Characteristics and Outcomes}} \\\\ \\addlinespace\n",
  panel_b_rows,
  "\\addlinespace \\midrule \\addlinespace\n",

  "\\multicolumn{3}{@{}l}{\\emph{Panel C: Caregiver Characteristics}} \\\\ \\addlinespace\n",
  panel_d_rows,
  "\\addlinespace \\midrule \\addlinespace\n",

  "\\multicolumn{3}{@{}l}{\\emph{Panel D: Region}} \\\\ \\addlinespace\n",
  panel_c_rows,
  "\\addlinespace \\midrule \\addlinespace\n",

  n_row,

  "\\bottomrule\n",
  "\\end{tabular*}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Note:} This table presents unweighted summary statistics.",
  " Endline Child- and Caregiver-Reported FI were defined as binary indicators if the sum of CFIES was larger than 7",
  " and if the sum of FIES was larger than 4, respectively, based on scoring guidelines provided by the developers",
  " (Frongillo et al. 2022; Gallegos et al., 2021).\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table,
           "tables/table_1.tex")
