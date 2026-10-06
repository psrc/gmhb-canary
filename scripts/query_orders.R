#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]))))
.libPaths(c(file.path(root, ".R-library"), .libPaths()))

main <- function() {
  for (package in c("httr2", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(paste("Missing dependency:", package, "(see README.md)."), call. = FALSE)
    }
  }
  source(file.path(root, "R", "cms.R"))
  source(file.path(root, "R", "email.R"))
  source(file.path(root, "R", "run.R"))
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--help")) {
    cat("Usage: Rscript scripts/query_orders.R [--since YYYY-MM-DD [--before YYYY-MM-DD]] [--output FILE.csv] [--email | --preview-email] [--env-file PATH]\n",
        "Default: previous complete Monday-Sunday week (based on today's Los Angeles date).\n",
        "--before is exclusive, using CMS UTC dates.\n",
        "The CMS request includes --before through 23:59:59Z; that date is excluded locally.\n",
        "With --since alone: all orders on or after that date.\n",
        "Default output: CSV only; no email is sent unless --email is specified.\n",
        "--email sends results (or a failure alert) using SMTP settings in .Renviron.\n",
        "--preview-email writes HTML/text previews without sending or requiring SMTP settings.\n", sep = "")
    return(invisible(NULL))
  }
  options <- list()
  mode <- "csv"
  while (length(args)) {
    if (args[1L] %in% c("--email", "--preview-email")) {
      if (mode != "csv") stop("Choose only one email mode.", call. = FALSE)
      mode <- if (args[1L] == "--email") "email" else "preview"
      args <- args[-1L]
      next
    }
    if (length(args) < 2L || !args[1L] %in% c("--since", "--before", "--output", "--env-file") ||
        args[1L] %in% names(options)) stop("Invalid arguments; use --help.", call. = FALSE)
    options[[args[1L]]] <- args[2L]
    args <- args[-c(1L, 2L)]
  }
  env_file <- options[["--env-file"]]
  if (!is.null(env_file)) {
    load_environment_file(env_file)
  } else if (file.exists(file.path(root, ".Renviron"))) {
    load_environment_file(file.path(root, ".Renviron"))
  }
  if (mode != "csv") {
    for (package in c("curl", "xml2")) {
      if (!requireNamespace(package, quietly = TRUE)) stop(paste("Missing dependency:", package), call. = FALSE)
    }
  }
  window <- previous_week()
  since <- if (is.null(options[["--since"]])) window$since else options[["--since"]]
  before <- options[["--before"]]
  if (is.null(options[["--since"]]) && is.null(before)) before <- window$before
  output <- options[["--output"]]
  if (is.null(output)) {
    suffix <- if (is.null(before)) "onward" else paste0("before-", before)
    output <- file.path(root, "results", paste0("gmhb-orders-", since, "-", suffix, ".csv"))
  }
  log_path <- Sys.getenv("GMHB_LOG_FILE", unset = file.path(root, "logs", "gmhb-canary.log"))
  orders <- run_order_job(since, before, output, mode, log = run_logger(log_path))
  status <- if (isTRUE(attr(orders, "manual_review_required"))) "REVIEW REQUIRED" else "SUCCESS"
  cat(sprintf("%s: retrieved %d orders; %d in selected window.\n",
              status, attr(orders, "retrieved_count"), nrow(orders)))
  cat("CSV:", normalizePath(output, winslash = "/"), "\n")
}

tryCatch(main(), error = function(e) {
  message("ERROR: ", conditionMessage(e))
  quit(save = "no", status = 1L)
})
