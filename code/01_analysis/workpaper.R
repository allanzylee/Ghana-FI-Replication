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
library(psych)

full_data_w <- read_rds('data/derived/regression_build_w.rds')

e_child <- read_dta('data/raw/03_PNP_Endline_ChildSurvey.dta') %>%
  rename(careid = caseid) %>%
  mutate(across(contains('id'), ~as.double(.))) %>%
  filter(childid %in% unique(full_data_w$childid))

tetrachoric_with_p <- function(df) {
  rho <- tetrachoric(df)$rho[1, 2]
  p   <- chisq.test(table(df[[1]], df[[2]]))$p.value
  tibble(statistic = rho, p_value = p)
}

tetra_ch_cg <- tetrachoric_with_p(
  full_data_w %>% dplyr::select(e_cfies_indicator, e_fies_indicator)
)

tetra_child_mid_end <- tetrachoric_with_p(
  full_data_w %>% dplyr::select(m_cfies_indicator, e_cfies_indicator)
)

tetra_cg_mid_end <- tetrachoric_with_p(
  full_data_w %>% dplyr::select(m_fies_indicator, e_fies_indicator)
)

clean_items <- function(df) {
  df %>%
    mutate(across(everything(), ~as.double(.))) %>%
    mutate(across(everything(), ~ifelse(. < 0, NA, .)))
}

safe_alpha <- function(df) {
  k <- ncol(df)
  n <- sum(complete.cases(df))
  tryCatch({
    if (k < 2) {
      warning("Fewer than 2 items - alpha cannot be computed. Returning NA.")
      return(list(alpha = NA_real_, n = n, k = k))
    }
    a <- psych::alpha(df, check.keys = TRUE)$total$raw_alpha
    list(alpha = a, n = n, k = k)
  }, error = function(e) {
    warning(paste("alpha() failed:", e$message, "- returning NA."))
    list(alpha = NA_real_, n = n, k = k)
  })
}

alpha_pvalue <- function(alpha_hat, n, k) {
  if (is.na(alpha_hat) || is.na(n) || is.na(k) || k < 2 || n < 2) return(NA_real_)
  f_stat <- 1 / (1 - alpha_hat)
  1 - pf(f_stat, df1 = n - 1, df2 = (n - 1) * (k - 1))
}

e_sel_all <- e_child %>%
  dplyr::select(childid, child_age = childage,
                starts_with("cr"),
                re1, re2, re3, re4, re6, re7, re9, re10, re11) %>%
  clean_items() %>%
  mutate(across(starts_with("cr"), ~case_when(. == 1 ~ 1,
                                              . == 2 ~ 0,
                                              TRUE   ~ NA_real_)))

alpha_sel_young <- e_sel_all %>%
  filter(child_age < 10) %>%
  dplyr::select(-childid, -child_age, -re9, -re10, -re11) %>%
  safe_alpha()

alpha_sel_old <- e_sel_all %>%
  filter(child_age >= 10) %>%
  dplyr::select(-childid, -child_age) %>%
  safe_alpha()

alpha_lit <- e_child %>%
  dplyr::select(starts_with("nr"),
                starts_with("sp"),
                starts_with("or"),
                starts_with("oc"),
                matches("pa[0-9]")) %>%
  clean_items() %>%
  safe_alpha()

alpha_num_young <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age < 10) %>%
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]")) %>%
  clean_items() %>%
  safe_alpha()

alpha_num_old <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age >= 10) %>%
  dplyr::select(matches("co[0-9]"),
                starts_with("nd"),
                starts_with("mn"),
                starts_with("nu"),
                starts_with("wp"),
                starts_with("ad"),
                matches("su[0-9]"),
                starts_with("mu"),
                matches("di[0-9]")) %>%
  clean_items() %>%
  safe_alpha()

alpha_ef_young <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age < 10) %>%
  dplyr::select(starts_with("wm"), starts_with("sm")) %>%
  clean_items() %>%
  safe_alpha()

alpha_ef_old <- e_child %>%
  mutate(child_age = as.double(childage)) %>%
  filter(child_age >= 10) %>%
  dplyr::select(starts_with("wm"), starts_with("sm")) %>%
  clean_items() %>%
  safe_alpha()

alpha_rows <- list(
  sel_young = alpha_sel_young, sel_old = alpha_sel_old,
  lit       = alpha_lit,
  num_young = alpha_num_young, num_old = alpha_num_old,
  ef_young  = alpha_ef_young,  ef_old  = alpha_ef_old
)

alpha_results <- tibble(
  category  = c("SEL",           "SEL",
                "Literacy",
                "Numeracy",      "Numeracy",
                "EF",            "EF"),
  age_group = c("age < 10",      "age >= 10",
                "all ages",
                "age < 10",      "age >= 10",
                "age < 10",      "age >= 10"),
  raw_alpha = vapply(alpha_rows, function(x) x$alpha, numeric(1)),
  raw_p     = vapply(alpha_rows, function(x) alpha_pvalue(x$alpha, x$n, x$k), numeric(1))
) %>%
  group_by(category) %>%
  summarise(alpha = mean(raw_alpha, na.rm = TRUE), p_value = mean(raw_p, na.rm = TRUE), .groups = "drop")

sib_pairs <- full_data_w %>%
  group_by(careid) %>%
  filter(n() == 2) %>%
  arrange(careid) %>%
  mutate(sib = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = careid,
    names_from = sib,
    values_from = e_cfies_indicator
  )

sib_test <- cor.test(sib_pairs$`1`, sib_pairs$`2`, use = "complete.obs")
sib_cor  <- unname(sib_test$estimate)
sib_p    <- sib_test$p.value

hh_disagree <- full_data_w %>%
  mutate(disagree = as.integer(e_cfies_indicator != e_fies_indicator)) %>%
  group_by(careid) %>%
  summarise(any_disagree = as.integer(any(disagree == 1, na.rm = TRUE)), .groups = "drop")

disagree_test   <- prop.test(x = sum(hh_disagree$any_disagree), n = nrow(hh_disagree), p = 0.5)
pct_hh_disagree <- mean(hh_disagree$any_disagree) * 100
pct_hh_disagree_p <- disagree_test$p.value

workpaper <- bind_rows(
  tibble(category  = "Tetrachoric correlation: Child-Reported FI vs. Caregiver-Reported FI (endline)",
         statistic = tetra_ch_cg$statistic, p_value = tetra_ch_cg$p_value),
  tibble(category  = "Tetrachoric correlation: Child-Reported FI, midline vs. endline",
         statistic = tetra_child_mid_end$statistic, p_value = tetra_child_mid_end$p_value),
  tibble(category  = "Tetrachoric correlation: Caregiver-Reported FI, midline vs. endline",
         statistic = tetra_cg_mid_end$statistic, p_value = tetra_cg_mid_end$p_value),
  alpha_results %>%
    transmute(category = paste0("Cronbach's alpha: ", category), statistic = alpha, p_value),
  tibble(category  = "Sibling correlation: Child-Reported FI (households with 2 children)",
         statistic = sib_cor, p_value = sib_p),
  tibble(category  = "% of households where at least one child disagrees with caregiver FI report",
         statistic = pct_hh_disagree, p_value = pct_hh_disagree_p)
)

print(workpaper)

write_csv(workpaper, "tables/workpaper.csv")
