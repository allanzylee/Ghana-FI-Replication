###############################################################################
# run_all.R - master script. Run this top to bottom (`Rscript run_all.R`, or
# source it in RStudio) to reproduce all data-cleaning steps and every
# table/appendix .tex file in tables/. See README.md for details.
###############################################################################

# This file is the root marker: every other script looks for a folder
# containing run_all.R to find its way back here.
root <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg)))
  } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
             !is.null(rstudioapi::getSourceEditorContext())) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    getwd()
  }
}, error = function(e) getwd())

if (!file.exists(file.path(root, "run_all.R"))) {
  stop("Could not determine the replication package root automatically. ",
       "Please setwd() to the folder containing run_all.R and re-source this file.")
}

setwd(root)
cat("Replication package root:", root, "\n")

# Empty output folders can be dropped when the package is zipped or
# downloaded, so (re)create them before any script writes into them.
for (d in c("data/raw", "data/derived", "tables")) {
  dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
}

# ---- Package check ------------------------------------------------------------

# Ensure a CRAN mirror is set so install.packages() below works when this
# script is run non-interactively (Rscript run_all.R) - without this, a fresh
# R installation with no mirror configured fails with "trying to use CRAN
# without setting a mirror" on every install attempt.
if (is.null(getOption("repos")) || identical(getOption("repos")[["CRAN"]], "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

required_packages <- c(
  "fixest","stargazer","AER","broom","xtable","lfe","purrr",
  "modelsummary","writexl","ltm","glue","kableExtra","readr","tidyverse",
  "dplyr","officer","janitor","fastDummies","haven","ggpubr","MASS",
  "sandwich","lmtest","flextable","knitr","psych"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("The following required R packages are not usable (missing, or failing to\n",
      "load due to an out-of-date dependency such as rlang):\n  ",
      paste(missing_packages, collapse = ", "), "\n", sep = "")
  ans <- if (interactive()) {
    readline("(Re)install them now from CRAN, including any outdated dependencies? [y/N]: ")
  } else {
    "y"
  }
  if (tolower(trimws(ans)) == "y") {
    install.packages(missing_packages, dependencies = TRUE)
    still_missing <- missing_packages[!vapply(missing_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing) > 0) {
      stop("Still unable to load: ", paste(still_missing, collapse = ", "),
           ". Try restarting R (so newly-installed package versions are picked up) ",
           "and re-running run_all.R; if that doesn't help, install these packages ",
           "by hand and check for an old rlang/other dependency (packageVersion('rlang')).")
    }
  } else {
    stop("Missing required packages. Install them and re-run run_all.R.")
  }
}

# ---- Bookkeeping: run each step, keep going on failure, report at the end ----

run_log <- list()

# Each script runs as its own R subprocess (not source()'d) so it gets a
# fresh global environment, matching how header.R's shared reg_func() expects
# to resolve full_data_w via ordinary lexical scoping.
rscript_path <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

run_step <- function(label, path) {
  cat("\n==============================================================\n")
  cat("Running:", label, "(", path, ")\n")
  cat("==============================================================\n")
  result <- tryCatch({
    out <- system2(rscript_path, args = shQuote(file.path(root, path)),
                    stdout = TRUE, stderr = TRUE)
    status_code <- attr(out, "status")
    cat(paste(out, collapse = "\n"), "\n")
    if (!is.null(status_code) && status_code != 0) {
      paste("FAILED: Rscript exited with status", status_code)
    } else {
      "OK"
    }
  }, error = function(e) {
    paste("FAILED:", conditionMessage(e))
  })
  if (startsWith(result, "FAILED")) cat(result, "\n")
  run_log[[label]] <<- result
  invisible(result)
}

###############################################################################
# 1. Data cleaning
###############################################################################

run_step("1_fi_control_data_cleaning",  "code/00_data_cleaning/1_fi_control_data_cleaning.R")
run_step("2_outcome_data_cleaning",     "code/00_data_cleaning/2_outcome_data_cleaning.R")

# Step 3 is a Stata do-file (constructs the Poverty Probability Index used as
# a covariate). We try to shell out to Stata in batch mode; if no Stata
# executable can be found (on PATH, or in common install locations), we stop
# with instructions, since 4_regression_build.R (and therefore every table)
# depends on its output (data/derived/poverty.dta).
cat("\n==============================================================\n")
cat("Running: 3_ppi_ghana_dofile (code/00_data_cleaning/3_ppi_ghana_dofile.do)\n")
cat("==============================================================\n")

find_stata_exe <- function() {
  # 1. Check PATH first (works cross-platform if the user has already set this up)
  path_candidates <- c("StataMP-64", "StataSE-64", "StataBE-64", "StataIC-64",
                        "stata-mp", "stata-se", "stata-be", "stata-ic", "stata")
  on_path <- Filter(function(x) nzchar(Sys.which(x)), path_candidates)
  if (length(on_path) > 0) return(on_path[1])

  # 2. Windows: Stata is commonly installed under Program Files but not added
  #    to PATH. Search common install roots (including "StataNow", the
  #    current subscription-branded release) for a matching executable,
  #    preferring MP > SE > BE > IC editions, newest version first.
  if (.Platform$OS.type == "windows") {
    roots <- c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)"))
    roots <- roots[nzchar(roots)]
    install_dirs <- unlist(lapply(roots, function(r) {
      Sys.glob(file.path(r, c("Stata*", "StataNow*")))
    }))
    install_dirs <- install_dirs[order(install_dirs, decreasing = TRUE)]
    exe_patterns <- c("StataMP-64.exe", "StataSE-64.exe", "StataBE-64.exe", "StataIC-64.exe")
    for (dir in install_dirs) {
      for (pat in exe_patterns) {
        candidate <- file.path(dir, pat)
        if (file.exists(candidate)) return(candidate)
      }
    }
  }

  NA_character_
}

stata_exe <- find_stata_exe()

if (!is.na(stata_exe)) {
  do_path <- file.path(root, "code", "00_data_cleaning", "3_ppi_ghana_dofile.do")
  result <- tryCatch({
    system2(stata_exe, args = c("-b", "do", shQuote(do_path)), stdout = TRUE, stderr = TRUE)
  }, error = function(e) NA)
  cat(paste(result, collapse = "\n"), "\n")
  run_log[["3_ppi_ghana_dofile"]] <- if (file.exists(file.path(root, "data/derived/poverty.dta"))) "OK" else "FAILED: poverty.dta not produced"
  # Stata batch mode always writes <dofile>.log to the working directory.
  # Show its tail if the step failed (so the error is visible), then delete it.
  stata_log <- file.path(root, "3_ppi_ghana_dofile.log")
  if (file.exists(stata_log)) {
    if (startsWith(run_log[["3_ppi_ghana_dofile"]], "FAILED")) {
      cat(tail(readLines(stata_log, warn = FALSE), 40), sep = "\n")
    }
    invisible(file.remove(stata_log))
  }
} else {
  msg <- paste0(
    "No Stata executable found (checked PATH and common install locations). ",
    "Please run code/00_data_cleaning/3_ppi_ghana_dofile.do manually in Stata ",
    "(with the working directory set to this replication folder) so that ",
    "data/derived/poverty.dta is created, then re-run run_all.R."
  )
  cat(msg, "\n")
  run_log[["3_ppi_ghana_dofile"]] <- paste("SKIPPED:", msg)
}

if (!file.exists(file.path(root, "data/derived/poverty.dta"))) {
  stop("data/derived/poverty.dta was not created. All tables/appendices depend on it via ",
       "4_regression_build.R. Please run the Stata do-file (see message above) and re-run run_all.R.")
}

run_step("4_regression_build", "code/00_data_cleaning/4_regression_build.R")

###############################################################################
# 2. Tables
###############################################################################

for (i in 1:6) {
  run_step(paste0("table_", i), file.path("code/01_analysis", paste0("table_", i, ".R")))
}

###############################################################################
# 3. Appendices
###############################################################################

appendix_ids <- c("a1","a2","c","d","e","f","g","h","i","j","k","l","m")
for (id in appendix_ids) {
  run_step(paste0("appendix_", id), file.path("code/01_analysis", paste0("appendix_", id, ".R")))
}

###############################################################################
# 4. Workpaper (supplementary calculations, not a paper exhibit)
###############################################################################

run_step("workpaper", "code/01_analysis/workpaper.R")

###############################################################################
# 5. Summary
###############################################################################

cat("\n\n==============================================================\n")
cat("RUN SUMMARY\n")
cat("==============================================================\n")
for (nm in names(run_log)) {
  cat(sprintf("%-30s %s\n", nm, run_log[[nm]]))
}

failed <- names(run_log)[vapply(run_log, function(x) startsWith(x, "FAILED"), logical(1))]
if (length(failed) > 0) {
  cat("\nThe following steps FAILED:", paste(failed, collapse = ", "), "\n")
  cat("See messages above for details.\n")
} else {
  cat("\nAll steps completed. Outputs are in tables/.\n")
}
