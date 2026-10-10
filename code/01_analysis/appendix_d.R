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

cell_mean_ttest <- function(data, var, filter_var, filter_val, group_var, group_val_a, group_val_b) {

  sub <- data %>% filter(.data[[filter_var]] == filter_val)

  mean_a <- sub %>% filter(.data[[group_var]] == group_val_a) %>% pull(.data[[var]]) %>% mean(na.rm = TRUE)
  mean_b <- sub %>% filter(.data[[group_var]] == group_val_b) %>% pull(.data[[var]]) %>% mean(na.rm = TRUE)

  x <- sub %>% filter(.data[[group_var]] == group_val_a) %>% pull(.data[[var]])
  y <- sub %>% filter(.data[[group_var]] == group_val_b) %>% pull(.data[[var]])
  pval <- tryCatch(t.test(x, y)$p.value, error = function(e) NA_real_)

  list(mean_a = mean_a, mean_b = mean_b, pval = pval)
}

format_pval <- function(p) {
  if (is.na(p)) return("")
  stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
  paste0(formatC(p, format = "f", digits = 3), stars)
}

fmt_mean <- function(x) formatC(x, format = "f", digits = 3)

make_row2 <- function(label, fi, nfi) {
  paste0(
    label, " & ",
    fmt_mean(fi$mean_a),  " & ", fmt_mean(fi$mean_b),  " & ", format_pval(fi$pval),  " & ",
    fmt_mean(nfi$mean_a), " & ", fmt_mean(nfi$mean_b), " & ", format_pval(nfi$pval),
    " \\\\\n"
  )
}

full_data_w <- full_data_w %>%
  fastDummies::dummy_columns('e_cfies_scale') %>%
  dplyr::select(-e_cfies_scale)

build_group <- function(filter_val, group_var, group_val_a, group_val_b) {
  list(
    cfies = cell_mean_ttest(full_data_w, "e_cfies_indicator", "e_fies_indicator", filter_val, group_var, group_val_a, group_val_b),
    s0    = cell_mean_ttest(full_data_w, "e_cfies_scale_0",   "e_fies_indicator", filter_val, group_var, group_val_a, group_val_b),
    s1    = cell_mean_ttest(full_data_w, "e_cfies_scale_1",   "e_fies_indicator", filter_val, group_var, group_val_a, group_val_b),
    s2    = cell_mean_ttest(full_data_w, "e_cfies_scale_2",   "e_fies_indicator", filter_val, group_var, group_val_a, group_val_b),
    s3    = cell_mean_ttest(full_data_w, "e_cfies_scale_3",   "e_fies_indicator", filter_val, group_var, group_val_a, group_val_b)
  )
}

gender_fi  <- build_group(filter_val = 1, group_var = "female", group_val_a = 1, group_val_b = 0)
gender_nfi <- build_group(filter_val = 0, group_var = "female", group_val_a = 1, group_val_b = 0)

age_fi  <- build_group(filter_val = 1, group_var = "age", group_val_a = 1, group_val_b = 0)
age_nfi <- build_group(filter_val = 0, group_var = "age", group_val_a = 1, group_val_b = 0)

n_fi_girl    <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$female == 1, na.rm = TRUE)
n_fi_boy     <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$female == 0, na.rm = TRUE)
n_fi_older   <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$age   == 1, na.rm = TRUE)
n_fi_younger <- sum(full_data_w$e_fies_indicator == 1 & full_data_w$age   == 0, na.rm = TRUE)

n_nfi_girl    <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$female == 1, na.rm = TRUE)
n_nfi_boy     <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$female == 0, na.rm = TRUE)
n_nfi_older   <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$age   == 1, na.rm = TRUE)
n_nfi_younger <- sum(full_data_w$e_fies_indicator == 0 & full_data_w$age   == 0, na.rm = TRUE)

build_panel_rows2 <- function(fi_panel, nfi_panel) {
  paste0(
    make_row2('Child-Reported FI (\\%)',           fi_panel$cfies, nfi_panel$cfies),
    "\\addlinespace\n",
    make_row2('Child: No FI (CFIES=0)',             fi_panel$s0,    nfi_panel$s0),
    make_row2('Child: Few Exp. (CFIES=1--6)',       fi_panel$s1,    nfi_panel$s1),
    make_row2('Child: Several Exp. (CFIES=7--10)',  fi_panel$s2,    nfi_panel$s2),
    make_row2('Child: Many Exp. (CFIES=11--20)',    fi_panel$s3,    nfi_panel$s3)
  )
}

n_row_gender <- paste0(
  "Total Observations & ",
  formatC(n_fi_girl, big.mark=","),  " & ",
  formatC(n_fi_boy, big.mark=","),   " & & ",
  formatC(n_nfi_girl, big.mark=","), " & ",
  formatC(n_nfi_boy, big.mark=","),  " & ",
  " \\\\\n"
)

n_row_age <- paste0(
  "Total Observations & ",
  formatC(n_fi_older, big.mark=","),    " & ",
  formatC(n_fi_younger, big.mark=","),  " & & ",
  formatC(n_nfi_older, big.mark=","),   " & ",
  formatC(n_nfi_younger, big.mark=","), " & ",
  " \\\\\n"
)

latex_table <- paste0(
  "\\begin{table}[H]\n",
  "\\caption{Child Food Insecurity Reporting by Gender and Age, By Caregiver-Reported FI Status}\n",
  "\\label{tab:child_fi_by_cg_fi}\\small\n",
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcccccc@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{3}{c}{Caregiver-Reported Food Insecure} & \\multicolumn{3}{c}{Caregiver-Reported Food Secure} \\\\\n",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n",

  "\\multicolumn{7}{@{}l}{\\emph{Panel A: By Child Gender}} \\\\ \\addlinespace\n",
  " & Girls & Boys & P-value & Girls & Boys & P-value \\\\\n",
  "\\midrule \\addlinespace\n",
  build_panel_rows2(gender_fi, gender_nfi),
  "\\addlinespace\n",
  n_row_gender,
  "\\addlinespace \\midrule \\addlinespace\n",

  "\\multicolumn{7}{@{}l}{\\emph{Panel B: By Child Age}} \\\\ \\addlinespace\n",
  " & Age 10--17 & Age 5--9 & P-value & Age 10--17 & Age 5--9 & P-value \\\\\n",
  "\\midrule \\addlinespace\n",
  build_panel_rows2(age_fi, age_nfi),
  "\\addlinespace\n",
  n_row_age,

  "\\bottomrule\n",
  "\\end{tabular*}\n",
  "\\begin{spacing}{1}\n",
  "\\begin{tablenotes}\n",
  "  \\item \\footnotesize \\textit{Notes:} This table reports unweighted means of child-reported food insecurity measures,",
  " stratified by caregiver-reported food insecurity status (FI = Food Insecure).",
  " Columns 1--3 restrict to households where the caregiver reported food insecurity (\\texttt{e\\_fies\\_indicator}=1).",
  " Columns 4--6 restrict to households where the caregiver did not report food insecurity (\\texttt{e\\_fies\\_indicator}=0).",
  " Panel A compares girls vs. boys; Panel B compares older (age 10--17) vs. younger (age 5--9) children.",
  " p-values come from two-sided t-tests of equality of means between the two groups shown in each panel.",
  " $^{*}$p$<$0.10, $^{**}$p$<$0.05, $^{***}$p$<$0.01.",
  " CFIES = Child Food Insecurity Experience Scale.\n",
  "\\end{tablenotes}\n",
  "\\end{spacing}\n",
  "\\end{table}\n"
)

writeLines(latex_table,
           "tables/appendix_d.tex")
