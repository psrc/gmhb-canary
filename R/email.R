email_addresses <- function(value, variable, multiple = FALSE) {
  if (!scalar_text(value) || grepl("[\r\n]", value)) {
    cms_error(paste(variable, "must contain a plain email address."))
  }
  addresses <- trimws(strsplit(value, "[,;]", perl = TRUE)[[1L]])
  pattern <- "^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9]+([.-][A-Za-z0-9]+)*\\.[A-Za-z]{2,}$"
  if ((!multiple && length(addresses) != 1L) || !length(addresses) ||
      any(!grepl(pattern, addresses)) || grepl("[,;]\\s*$", value)) {
    cms_error(paste(variable, "must contain plain email addresses without display names."))
  }
  unique(addresses)
}

smtp_config <- function(env = Sys.getenv) {
  required <- c("SMTP", "SMTPPORT", "MAILER_EMAIL", "MAILER_PW", "MYEMAIL")
  values <- setNames(lapply(required, env), required)
  missing <- required[!vapply(values, scalar_text, logical(1))]
  if (length(missing)) cms_error(paste("Missing SMTP configuration:", paste(missing, collapse = ", ")))
  host <- trimws(values$SMTP)
  port <- values$SMTPPORT
  if (!grepl("^[0-9]{1,5}$", port) || as.integer(port) < 1L || as.integer(port) > 65535L) {
    cms_error("SMTPPORT must be an integer from 1 to 65535.")
  }
  # Allow an explicit TLS scheme, but never userinfo, paths, or credentials in URLs.
  scheme <- if (grepl("^smtps?://", host)) sub("://.*$", "", host) else
    if (as.integer(port) == 465L) "smtps" else "smtp"
  host <- sub("^smtps?://", "", host)
  if (!grepl("^[A-Za-z0-9]+([.-][A-Za-z0-9]+)*$", host)) {
    cms_error("SMTP must be a hostname, optionally prefixed with smtp:// or smtps://; use SMTPPORT for the port.")
  }
  sender <- email_addresses(values$MAILER_EMAIL, "MAILER_EMAIL")
  list(server = paste0(scheme, "://", host, ":", as.integer(port)),
       sender = sender, recipients = email_addresses(values$MYEMAIL, "MYEMAIL", TRUE),
       username = sender, password = values$MAILER_PW)
}

html_escape <- function(text) {
  text[is.na(text)] <- ""
  for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"),
                    c('"', "&quot;"), c("'", "&#39;"))) {
    text <- gsub(pair[1L], pair[2L], text, fixed = TRUE)
  }
  text
}

case_details <- function(markup, case_id) {
  fallback <- list(title = paste("Case", case_id), url = NULL)
  if (!scalar_text(markup)) return(fallback)
  # Parse as bytes, never a URL/file. Copy text and a narrowly allowed link only.
  doc <- tryCatch(xml2::read_html(charToRaw(enc2utf8(markup)), encoding = "UTF-8",
                                 options = c("RECOVER", "NOERROR", "NOWARNING", "NONET")),
                  error = function(e) NULL)
  if (is.null(doc)) return(fallback)
  anchor <- xml2::xml_find_first(doc, ".//a")
  title <- trimws(xml2::xml_text(anchor))
  href <- xml2::xml_attr(anchor, "href")
  if (scalar_text(title)) fallback$title <- title
  if (scalar_text(href) && grepl("^/[A-Za-z0-9]{15}([A-Za-z0-9]{3})?$", href) &&
      substr(href, 2L, 16L) == substr(case_id, 1L, 15L)) {
    fallback$url <- paste0("https://eluho2022.my.site.com", href)
  }
  fallback
}

window_label <- function(since, before = NULL) {
  since <- valid_date(since)
  if (is.null(before)) return(paste("on or after", since))
  before <- valid_date(before)
  if (before <= since) cms_error("The exclusive end date must be after the start date.")
  paste(since, "to", as.Date(before) - 1L)
}

format_order_email <- function(orders, since, before = NULL) {
  review <- isTRUE(attr(orders, "manual_review_required"))
  if (!nrow(orders) && !review) return(NULL)
  period <- window_label(since, before)
  subject <- sprintf("GMHB orders: %d results (%s)", nrow(orders), period)
  if (review) subject <- paste("[REVIEW REQUIRED]", subject)
  intro <- sprintf("%d GMHB orders for %s (CMS dates in UTC).", nrow(orders), period)
  caution <- if (review) paste0("REVIEW REQUIRED: The CMS returned ",
    attr(orders, "retrieved_count"), " records before date filtering, reaching the 50-record ",
    "review threshold. This list may be incomplete. Search the CMS manually for additional orders.") else NULL
  text_items <- html_items <- character()
  for (i in seq_len(nrow(orders))) {
    order <- orders[i, , drop = FALSE]
    case <- case_details(order$Case_Link__c, order$Case__c)
    metadata <- paste0("Date: ", substr(order$Document_Date__c, 1L, 10L),
                       " | Type: ", ifelse(is.na(order$Document_Type__c), "Unspecified", order$Document_Type__c),
                       " | Docket entry: ", ifelse(is.na(order$Docket_Number__c), "Unspecified", order$Docket_Number__c))
    text_items <- c(text_items, paste(c(paste0(i, ". ", order$Name), case$title,
                                       metadata, case$url), collapse = "\n"))
    case_html <- html_escape(case$title)
    if (!is.null(case$url)) case_html <- paste0('<a href="', html_escape(case$url), '">', case_html, '</a>')
    html_items <- c(html_items, paste0("<li><p><strong>", html_escape(order$Name),
      "</strong><br>", case_html, "<br>", html_escape(metadata), "</p></li>"))
  }
  footer <- paste("CMS Order Search:", cms_page_url)
  text <- paste(c(intro, caution, text_items, footer), collapse = "\n\n")
  html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>',
    html_escape(subject), '</title></head><body><h1>GMHB orders</h1><p>', html_escape(intro),
    '</p>', if (!is.null(caution)) paste0('<p><strong>', html_escape(caution), '</strong></p>') else "",
    '<ol>', paste(html_items, collapse = "\n"), '</ol><p><a href="', cms_page_url,
    '">Open CMS Order Search</a></p></body></html>')
  list(subject = subject, text = text, html = html)
}

format_failure_email <- function(stage, since, before) {
  period <- window_label(since, before)
  text <- paste0("The GMHB check for ", period, " failed during ", stage,
                 ".\n\nCheck the local run log and search the CMS manually: ", cms_page_url,
                 "\n\nNo successful result notification was confirmed for this run.")
  list(subject = paste("[FAILED] GMHB check:", period), text = text,
       html = paste0('<!doctype html><html><head><meta charset="utf-8"></head><body><p>',
                     gsub("\n", "<br>", html_escape(text), fixed = TRUE), "</p></body></html>"))
}

mime_message <- function(email, config, now = Sys.time()) {
  sender <- email_addresses(config$sender, "MAILER_EMAIL")
  recipients <- email_addresses(paste(config$recipients, collapse = ","), "MYEMAIL", TRUE)
  if (!scalar_text(email$subject) || grepl("[\r\n]", email$subject)) cms_error("Invalid email subject.")
  encode <- function(value) {
    encoded <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(enc2utf8(value))))
    starts <- seq.int(1L, nchar(encoded), by = 76L)
    paste(substring(encoded, starts, starts + 75L), collapse = "\r\n")
  }
  unique <- paste(sample(c(0:9, letters[1:6]), 32L, replace = TRUE), collapse = "")
  boundary <- paste0("gmhb_", unique)
  # Build English RFC date components independently of the workstation's locale.
  utc <- as.POSIXlt(now, tz = "UTC")
  date <- sprintf("%s, %02d %s %04d %02d:%02d:%02d +0000",
                  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[utc$wday + 1L],
                  utc$mday, month.abb[utc$mon + 1L], utc$year + 1900L,
                  utc$hour, utc$min, as.integer(utc$sec))
  headers <- c(paste("Date:", date), paste("From:", sender), paste("To:", paste(recipients, collapse = ", ")),
               paste("Subject:", email$subject),
               paste0("Message-ID: <", unique, "@", sub(".*@", "", sender), ">"),
               "MIME-Version: 1.0", paste0('Content-Type: multipart/alternative; boundary="', boundary, '"'))
  part <- function(type, body) c(paste0("--", boundary), paste0("Content-Type: ", type, "; charset=UTF-8"),
                                "Content-Transfer-Encoding: base64", "", encode(body))
  paste(c(headers, "", part("text/plain", email$text), part("text/html", email$html),
          paste0("--", boundary, "--"), ""), collapse = "\r\n")
}

send_smtp <- function(email, config, transport = curl::send_mail) {
  message <- mime_message(email, config)
  result <- tryCatch(transport(
    mail_from = config$sender, mail_rcpt = config$recipients, message = message,
    smtp_server = config$server, username = config$username, password = config$password,
    use_ssl = "force", verbose = FALSE, ssl_verifypeer = TRUE, ssl_verifyhost = 2L,
    connecttimeout = 15L, timeout = 60L), error = function(e) {
      # Curl messages can repeat URLs or server replies. Keep only its safe class.
      classes <- grep("^curl_error_[a-z_]+$", class(e), value = TRUE)
      detail <- if (length(classes)) paste0(" (", classes[1L], ")") else ""
      cms_error(paste0("SMTP delivery failed", detail,
                      ". Check relay access, TLS settings, and credentials. Delivery may be uncertain; no automatic retry."))
    })
  if (!is.list(result) || length(result$status_code) != 1L || is.na(result$status_code) ||
      result$status_code < 200L || result$status_code >= 300L) {
    cms_error("SMTP relay did not confirm message acceptance; no automatic retry.")
  }
  invisible(TRUE)
}
