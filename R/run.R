load_environment_file <- function(path) {
  if (!file.exists(path)) cms_error("Environment file not found; check --env-file.")
  loaded <- withCallingHandlers(readRenviron(path), warning = function(w) {
    # R's warning may include the offending line, which could contain a secret.
    cms_error("Could not read environment file; check its syntax and access permissions.")
  })
  if (!isTRUE(loaded)) cms_error("Could not read environment file.")
  invisible(TRUE)
}

run_logger <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  function(event, detail = "") {
    line <- paste(format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
                  event, gsub("[\r\n]", " ", detail))
    cat(line, "\n", file = path, append = TRUE)
    message(line)
  }
}

run_order_job <- function(since, before, output, mode = c("csv", "email", "preview"),
                          config = NULL, fetch = fetch_orders, send = send_smtp,
                          log = function(event, detail = "") message(event, ": ", detail)) {
  mode <- match.arg(mode)
  select_window(data.frame(Document_Date__c = character()), since, before)
  log("START", paste(mode, window_label(since, before)))
  stage <- "configuration"
  tryCatch({
    if (mode == "email" && is.null(config)) config <- smtp_config()
    stage <- "CMS search"
    orders <- fetch(since, before)
    count <- attr(orders, "retrieved_count")
    review <- isTRUE(attr(orders, "manual_review_required"))
    log(if (review) "REVIEW_REQUIRED" else "SEARCH_OK",
        sprintf("retrieved=%d selected=%d", count, nrow(orders)))
    stage <- "CSV export"
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    write.csv(orders, output, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    if (mode != "csv") {
      stage <- "email formatting"
      email <- format_order_email(orders, since, before)
      if (is.null(email)) {
        log("NO_RESULTS", "No email needed.")
      } else if (mode == "preview") {
        stage <- "email preview export"
        prefix <- sub("\\.[Cc][Ss][Vv]$", "", output)
        writeLines(enc2utf8(email$html), paste0(prefix, ".html"), useBytes = TRUE)
        writeLines(enc2utf8(paste(email$subject, email$text, sep = "\n\n")),
                   paste0(prefix, ".txt"), useBytes = TRUE)
        log("PREVIEW_SAVED", "HTML and text previews saved beside the CSV; no email sent.")
      } else {
        stage <- "SMTP delivery"
        send(email, config)
        log("EMAIL_ACCEPTED", "SMTP relay accepted the message; inbox delivery is not confirmed.")
      }
    }
    log("COMPLETE", if (review) "Manual CMS review required." else "Run completed.")
    invisible(orders)
  }, error = function(e) {
    detail <- if (inherits(e, "gmhb_error")) conditionMessage(e) else "Check local file access and configuration."
    log("FAILED", paste("Stage:", stage, "-", detail))
    if (mode == "email" && !is.null(config) && stage != "SMTP delivery") {
      tryCatch({
        send(format_failure_email(stage, since, before), config)
        log("FAILURE_ALERT_ACCEPTED", "SMTP relay accepted the failure notification.")
      }, error = function(alert_error) {
        log("FAILURE_ALERT_FAILED", "Could not send failure notification; inspect the local log.")
      })
    }
    stop(e)
  })
}
