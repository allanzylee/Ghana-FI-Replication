library(fixest)
library(stargazer)
library(AER)
library(broom)
library(xtable)
library(lfe)
library(purrr)
library(modelsummary)
library(writexl)
library(ltm)
library(glue)
library(kableExtra)
library(readr)
library(tidyverse)
library(dplyr)
library(officer)
library(janitor)
library(fastDummies)
library(haven)
library(ggpubr)

reg_func <- function(category, model){
  e_category_str<-paste0("e_",category,"_per")
  m_category_str<-paste0("m_",category,"_per")

  for_reg<-full_data_w %>%
    rename(lagged_outcome=m_category_str)

  fm <- as.formula(paste(e_category_str, model, 'lagged_outcome','| factor(month)+factor(year)'))
  reg <- feols(fm,
            data=for_reg,
            cluster=~careid)
  return(reg)
}

cluster_robust_func <- function(category, results_str){

  results<-get(results_str)
  reg_robust <- coeftest(results[[category]], vcovCL, cluster=full_data_w$careid)

  out<-list(se=reg_robust[,2],
       p=reg_robust[,4])

  return(out)
}

tidy_func <- function(category, results_str){
  results<-get(results_str)
  out<-tidy(results[[category]]) %>%
    mutate(category=category)
  return(out)
}

f <- function(x) formatC(x, digits = 3, big.mark = ",", format = "f")

format_nobs <- function(x) {
  if (is.numeric(x)) {
    return(format(x, big.mark = ",", scientific = F, digits=3))
  }

  return(x)
}

format_stat <- function(x) {
  if (is.numeric(x)) {
    return(format(x, big.mark = ",", scientific = F, digits=3))
  }

  return(x)
}

gm <- list(
  list("raw" = "nobs", "clean" = "Num.Obs.", "fmt" = format_nobs),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = format_stat),
  list("raw" = "rmse", "clean" = "RMSE", "fmt" = format_stat))

shrink_table_width <- function(path) {
  lines <- readLines(path)
  begin_idx <- grep("\\\\begin\\{talltblr\\}", lines)
  end_idx   <- grep("\\\\end\\{talltblr\\}", lines)
  if (length(begin_idx) == 0 || length(end_idx) == 0) return(invisible(NULL))
  begin_idx <- begin_idx[1]
  end_idx   <- end_idx[length(end_idx)]
  lines <- append(lines, "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{%", after = begin_idx - 1)
  end_idx <- end_idx + 1
  lines <- append(lines, "}", after = end_idx)
  writeLines(lines, path)
}

wrap_table_landscape <- function(path) {
  lines <- readLines(path)
  begin_idx <- grep("\\\\begin\\{table\\}", lines)
  end_idx   <- grep("\\\\end\\{table\\}", lines)
  if (length(begin_idx) == 0 || length(end_idx) == 0) return(invisible(NULL))
  begin_idx <- begin_idx[1]
  end_idx   <- end_idx[length(end_idx)]
  lines <- append(lines, c("\\begin{landscape}", "{\\footnotesize"), after = begin_idx - 1)
  end_idx <- end_idx + 2
  lines <- append(lines, c("}% end footnotesize", "\\end{landscape}"), after = end_idx)
  writeLines(lines, path)
}

suppress_table_entry <- function(path) {
  lines <- readLines(path)
  if (any(grepl("entry=none", lines))) return(invisible(NULL))
  begin_idx <- grep("\\\\begin\\{talltblr\\}\\[", lines)
  if (length(begin_idx) == 0) return(invisible(NULL))
  lines <- append(lines, "entry=none,label=none,", after = begin_idx[1])
  writeLines(lines, path)
}
